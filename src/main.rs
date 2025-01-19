#![recursion_limit = "1024"]
#![feature(rustc_private)]
#![warn(clippy::pedantic, clippy::unwrap_used)]
#![allow(clippy::cast_precision_loss)]

mod analysis;
mod ast_parser;
mod compiler;
mod graph;

extern crate rustc_ast;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_parse;
extern crate rustc_session;

use analysis::calls_to_chains;
use clap::Parser;
use compiler::{cargo_ast, get_compiler_args, get_manifest_info, run_compiler, AnalysisCallbacks};
use graph::CallGraph;
use std::{
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// Location of manifest file of project to analyze
    manifest: PathBuf,

    /// Location to write dotfile of graph to
    output_file: PathBuf,

    /// Provide full call graph instead of propagation graph
    #[arg(long)]
    call_graph: bool,
}

/// Entry point, first sets up the compiler, and then runs it using the provided arguments.
fn main() {
    // Create a wrapper around an DiagCtxt that is used for early error emissions.
    let early_dcx =
        rustc_session::EarlyDiagCtxt::new(rustc_session::config::ErrorOutputType::default());

    let args = Args::parse();
    let manifest_path = absolute_path(&args.manifest);

    let manifest_info = get_manifest_info(&manifest_path);

    // Extract the compiler arguments from running `cargo build`
    let compiler_commands = get_compiler_args(&manifest_path, &manifest_info);

    // Enable CTRL + C
    rustc_driver::install_ctrlc_handler();

    // Install a panic hook that will print the ICE message on unexpected panics.
    let using_internal_features =
        rustc_driver::install_ice_hook(rustc_driver::DEFAULT_BUG_REPORT_URL, |_| ());

    // This allows tools to enable rust logging without having to magically match rustc’s tracing crate version.
    rustc_driver::init_rustc_env_logger(&early_dcx);
    let mut callbacks = AnalysisCallbacks::new(Arc::new(Mutex::new(CallGraph::new(
        manifest_info.package_name,
    ))));
    // Run the compiler using the retrieved args.
    let cwd = std::env::current_dir().expect("cwd invalid");
    std::env::set_current_dir(manifest_info.root_path).expect("root path invalid");
    if let Some(compiler_command) = compiler_commands.lib_command {
        run_compiler(
            compiler_command,
            &mut callbacks,
            using_internal_features.clone(),
        );
    }
    for compiler_command in compiler_commands.bin_commands {
        run_compiler(
            compiler_command.clone(),
            &mut callbacks,
            using_internal_features.clone(),
        );
    }
    std::env::set_current_dir(cwd).expect("failed to reset cwd");

    let mut asts = vec![];
    if manifest_info.lib.is_some() {
        asts.push(cargo_ast(&manifest_path, compiler::LibOrBin::Lib));
    }
    for name in manifest_info.bins {
        asts.push(cargo_ast(
            &manifest_path,
            compiler::LibOrBin::Bin(&name.name),
        ));
    }

    println!("Parsing ASTs...");

    let parsed_asts: Vec<ast_parser::Crate> = asts
        .iter()
        .map(|ast| ast_parser::parse(ast).map(|(_, x)| x))
        .filter_map(|parse_result| match parse_result {
            Ok(out) => Some(out),
            Err(err) => {
                eprintln!(
                    "AST parsing failure: {}",
                    match err {
                        nom::Err::Incomplete(_) => "AST incomplete?",
                        nom::Err::Error(e) | nom::Err::Failure(e) => e.input,
                    }
                );
                None
            }
        })
        .collect();

    let mut call_graph = Arc::into_inner(callbacks.graph)
        .expect("arc still referenced")
        .into_inner()
        .expect("mutex poisoned");

    call_graph.attach_panic_info(&parsed_asts);

    // Parse graph to show chains
    let chain_graph = calls_to_chains::to_chains(&call_graph);
    let dot = if args.call_graph {
        call_graph.to_dot()
    } else {
        chain_graph.to_dot()
    };

    println!("Writing graph...");

    match std::fs::write(&args.output_file, dot.clone()) {
        Ok(()) => {
            println!("Done!");
            println!("Wrote to {}", &args.output_file.display());
        }
        Err(e) => {
            eprintln!("Could not write output!");
            eprintln!("{e}");
            eprintln!();
            println!("{dot}");
        }
    }
}

/// Turn relative path into absolute path
fn absolute_path(cargo_path: &Path) -> PathBuf {
    std::env::current_dir()
        .expect("current directory is invalid")
        .join(cargo_path)
}
