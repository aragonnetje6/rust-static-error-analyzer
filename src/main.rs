#![recursion_limit = "1024"]
#![feature(rustc_private)]
#![warn(clippy::pedantic, clippy::unwrap_used)]
#![allow(clippy::cast_precision_loss)]

mod analysis;
mod ast_parser;
mod compiler;
mod graphs;

extern crate rustc_ast;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_parse;
extern crate rustc_session;

use analysis::calls_to_chains;
use cargo::{
    core::{Shell, Workspace},
    util::homedir,
    GlobalContext,
};
use clap::{Parser, ValueEnum};
use compiler::{cargo_ast, get_compiler_args, run_compiler, AnalysisCallbacks};
use graphs::CallGraph;
use std::{
    fmt::Display,
    path::PathBuf,
    sync::{Arc, Mutex},
};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// Location of manifest file of project to analyze
    manifest: PathBuf,

    #[arg(value_enum)]
    graph_types: Vec<GraphType>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum GraphType {
    /// Output call graph
    Call,
    /// Output chain graph of result-type errors
    ErrorChain,
    /// Output chain graph of panic-type errors
    PanicChain,
}

impl Display for GraphType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Call => "call",
                Self::ErrorChain => "error-chain",
                Self::PanicChain => "panic-chain",
            },
        )
    }
}

/// Entry point, first sets up the compiler, and then runs it using the provided arguments.
fn main() {
    // Create a wrapper around an DiagCtxt that is used for early error emissions.
    let early_dcx =
        rustc_session::EarlyDiagCtxt::new(rustc_session::config::ErrorOutputType::default());

    let args = Args::parse();
    let manifest_path = std::path::absolute(&args.manifest)
        .expect("current directory invalid")
        .canonicalize()
        .expect("manifest path failed to resolve");

    let mut shell = Shell::new();
    shell.set_verbosity(cargo::core::Verbosity::Verbose);
    let gctx = GlobalContext::new(
        shell,
        manifest_path
            .parent()
            .expect("manifest in invalid folder")
            .to_path_buf(),
        homedir(&manifest_path).expect("couldn't get homedir"),
    );
    let workspace = Workspace::new(&manifest_path, &gctx).expect("workspace not created");

    // Extract the compiler arguments from running `cargo build`
    let process_builders = get_compiler_args(&workspace, &gctx);

    // Enable CTRL + C
    rustc_driver::install_ctrlc_handler();

    // Install a panic hook that will print the ICE message on unexpected panics.
    let using_internal_features =
        rustc_driver::install_ice_hook(rustc_driver::DEFAULT_BUG_REPORT_URL, |_| ());

    // This allows tools to enable rust logging without having to magically match rustc’s tracing crate version.
    rustc_driver::init_rustc_env_logger(&early_dcx);
    let package_name = workspace.current().expect("impossible").name().to_string();
    let mut callbacks = AnalysisCallbacks::new(Arc::new(Mutex::new(CallGraph::new(String::from(
        &package_name,
    )))));
    // Run the compiler using the retrieved args.
    let cwd = std::env::current_dir().expect("cwd invalid");
    std::env::set_current_dir(workspace.root()).expect("root path invalid");
    for process_builder in process_builders {
        run_compiler(
            &process_builder,
            &mut callbacks,
            using_internal_features.clone(),
        );
    }
    std::env::set_current_dir(cwd).expect("failed to reset cwd");

    let asts: Vec<String> = workspace
        .current()
        .expect("impossible")
        .targets()
        .iter()
        .filter(|target| !target.is_test())
        .map(|target| cargo_ast(&manifest_path, target))
        .collect();

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
    let error_chain_graph = calls_to_chains::to_error_chains(&call_graph);

    let panic_chain_graph = calls_to_chains::to_panic_chains(&call_graph);

    for graph_type in args.graph_types {
        write_out(
            &match graph_type {
                GraphType::Call => call_graph.to_dot(),
                GraphType::ErrorChain => error_chain_graph.to_dot(),
                GraphType::PanicChain => panic_chain_graph.to_dot(),
            },
            &package_name,
            graph_type,
        );
    }
}

fn write_out(graph: &str, name: &str, graph_type: GraphType) {
    println!("Writing graph...");

    let output_file = format!("{name}-{graph_type}.dot");
    match std::fs::write(&output_file, graph) {
        Ok(()) => {
            println!("Done!");
            println!("Wrote to {output_file}");
        }
        Err(e) => {
            eprintln!("Could not write output!");
            eprintln!("{e}");
        }
    }
}
