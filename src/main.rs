#![feature(rustc_private)]
#![warn(clippy::pedantic, clippy::unwrap_used)]
#![allow(clippy::cast_precision_loss)]

mod analysis;
mod graph;

extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_parse;
extern crate rustc_session;

use clap::Parser;
use rustc_driver::Compilation;
use rustc_interface::interface::Compiler;
use rustc_interface::Queries;
use std::path::{Path, PathBuf};
use std::process::Command;
use toml::Table;

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
    let output_path = absolute_path(&args.output_file);

    // Extract the compiler arguments from running `cargo build`
    let compiler_args = get_compiler_args(&args.manifest, &manifest_path)
        .expect("Could not get arguments from cargo build!");

    // Enable CTRL + C
    rustc_driver::install_ctrlc_handler();

    // Install a panic hook that will print the ICE message on unexpected panics.
    let using_internal_features =
        rustc_driver::install_ice_hook(rustc_driver::DEFAULT_BUG_REPORT_URL, |_| ());

    // This allows tools to enable rust logging without having to magically match rustc’s tracing crate version.
    rustc_driver::init_rustc_env_logger(&early_dcx);

    // Run the compiler using the retrieved args.
    let _exit_code = run_compiler(
        compiler_args,
        &mut AnalysisCallback {
            output_path,
            call_graph: args.call_graph,
        },
        using_internal_features,
    );
}

/// Turn relative path into absolute path
fn absolute_path(cargo_path: &Path) -> PathBuf {
    std::env::current_dir()
        .expect("current directory is invalid")
        .join(cargo_path)
}

/// Get the compiler arguments used to compile the package by first running `cargo clean` and then `cargo build -vv`.
fn get_compiler_args(relative_manifest_path: &Path, manifest_path: &Path) -> Option<Vec<String>> {
    println!("Using {}!", cargo_version().trim_end_matches('\n'));

    let (package_name, bin_name) = get_package_name(manifest_path);

    cargo_clean(manifest_path, &package_name);

    let build_output = cargo_build_verbose(manifest_path);

    let command = get_rustc_invocation(&build_output, &package_name, bin_name)?;

    Some(split_args(
        &relative_manifest_path.to_string_lossy(),
        &command,
    ))
}

/// Split up individual arguments from the command.
fn split_args(relative_manifest_path: &str, command: &str) -> Vec<String> {
    let mut res = vec![];
    let mut temp = String::new();

    // Split on ' '
    for arg in command.split(' ') {
        let mut arg = arg.to_owned();

        // If this is the path to main.rs, prepend the relative path to the manifest, stripping away Cargo.toml
        if arg.contains("main.rs") {
            let mut new_arg = String::from(relative_manifest_path.trim_end_matches("Cargo.toml"));
            new_arg.push_str(&arg);
            arg = new_arg;
        }

        // Leave ' ' when enclosed in '"', removing the enclosing '"'
        if arg.starts_with('"') && arg.ends_with('"') {
            temp.push_str(
                arg.strip_prefix('"')
                    .expect("Could not remove '\"' from start of string!")
                    .strip_suffix('"')
                    .expect("Could not remove '\"' from end of string!"),
            );
            res.push(temp);
            temp = String::new();
        } else if arg.ends_with('"') {
            temp.push_str(
                arg.strip_suffix('"')
                    .expect("Could not remove '\"' from end of string!"),
            );
            res.push(temp);
            temp = String::new();
        } else if arg.starts_with('"') {
            temp.push_str(
                arg.strip_prefix('"')
                    .expect("Could not remove '\"' from start of string!"),
            );
            temp.push(' ');
        } else if arg.starts_with('\'') && arg.ends_with('\'') {
            temp.push_str(
                arg.strip_prefix('\'')
                    .expect("Could not remove '\"' from start of string!")
                    .strip_suffix('\'')
                    .expect("Could not remove '\"' from end of string!"),
            );
            res.push(temp);
            temp = String::new();
        } else if arg.ends_with('\'') {
            temp.push_str(
                arg.strip_suffix('\'')
                    .expect("Could not remove '\"' from end of string!"),
            );
            res.push(temp);
            temp = String::new();
        } else if arg.starts_with('\'') {
            temp.push_str(
                arg.strip_prefix('\'')
                    .expect("Could not remove '\"' from start of string!"),
            );
            temp.push(' ');
        } else if !temp.is_empty() {
            temp.push_str(&arg);
            temp.push(' ');
        } else {
            res.push(arg);
        }
    }

    // Overwrite error format args
    for i in 0..res.len() {
        if i >= res.len() {
            break;
        }
        res[i] = res[i].replace("\\\"", "\"");
        if res[i].starts_with("--error-format=") {
            res[i] = String::from("--error-format=short");
        }
        if res[i].starts_with("--json=") {
            res.remove(i);
        }
    }

    res
}

/// Run `cargo clean -p PACKAGE`, where the package name is extracted from the given manifest.
fn cargo_clean(manifest_path: &Path, package_name: &str) -> String {
    println!("Cleaning package...");
    let output = Command::new("cargo")
        .arg("clean")
        .arg("-p")
        .arg(package_name)
        .current_dir(
            manifest_path
                .parent()
                .expect("Could not get manifest directory!"),
        )
        .output()
        .expect("Could not clean!");

    let stderr = String::from_utf8(output.stderr).expect("Invalid UTF8!");

    if output.status.code() != Some(0) {
        eprintln!("Could not clean package!");
        println!("{stderr:?}");
    }

    stderr
}

/// Extract the package name from the given manifest.
fn get_package_name(manifest_path: &Path) -> (String, Option<String>) {
    let file = std::fs::read(manifest_path).expect("Could not read manifest!");
    let content = String::from_utf8(file).expect("Invalid UTF8!");
    let table = content
        .parse::<Table>()
        .expect("Could not parse manifest as TOML!");
    let package_table = table["package"]
        .as_table()
        .expect("'package' is not a table!");
    let package_name = package_table["name"]
        .as_str()
        .expect("No name found in package information!")
        .to_owned();
    if table.contains_key("bin") {
        let binary_table = table["bin"]
            .as_array()
            .expect("'bin' is not an array!")
            .first()
            .expect("'bin' contains no values!")
            .as_table()
            .expect("'bin' is not a table!");
        let binary_name = binary_table["name"]
            .as_str()
            .expect("No name found in binary information!")
            .to_owned();
        return (package_name, Some(binary_name));
    }

    (package_name, None)
}

/// Run `cargo --version`.
fn cargo_version() -> String {
    let output = Command::new("cargo")
        .arg("--version")
        .output()
        .expect("Could not get cargo version!");

    String::from_utf8(output.stdout).expect("Invalid UTF8!")
}

/// Run `cargo build -v` on the given manifest.
fn cargo_build_verbose(manifest_path: &Path) -> String {
    // TODO: interrupt build as to not compile the program twice
    println!("Building package...");
    let output = Command::new("cargo")
        .arg("build")
        .arg("-v")
        .arg("--manifest-path")
        .arg(manifest_path.as_os_str())
        .output()
        .expect("Could not build!");

    let stderr = String::from_utf8(output.stderr).expect("Invalid UTF8!");

    if output.status.code() != Some(0) {
        eprintln!("Could not (fully) build package!");
        eprintln!();
        for line in stderr.split('\n') {
            if line.starts_with("error") {
                eprintln!("{line}");
            }
        }
        eprintln!();
        eprintln!("Trying to continue...");
    }

    stderr
}

/// Gets the rustc invocation command from the output of `cargo build -vv`.
fn get_rustc_invocation(
    build_output: &str,
    package_name: &str,
    bin_name: Option<String>,
) -> Option<String> {
    let name = bin_name
        .unwrap_or(package_name.to_owned())
        .replace('-', "_");
    for line in build_output.split('\n') {
        for part in line.split('`') {
            for command in part.split("&& ") {
                if command.contains("rustc")
                    && command.contains("--crate-type bin")
                    && !command.contains("build.rs")
                    && command.contains("main.rs")
                    && command.contains(&format!("--crate-name {name}"))
                {
                    return Some(String::from(command));
                }
            }
        }
    }

    None
}

/// Run a compiler with the provided arguments and callbacks.
/// Returns the exit code of the compiler.
fn run_compiler(
    args: Vec<String>,
    callbacks: &mut (dyn rustc_driver::Callbacks + Send),
    using_internal_features: std::sync::Arc<std::sync::atomic::AtomicBool>,
) -> i32 {
    println!("Running compiler...");

    // Invoke compiler, and return the exit code
    rustc_driver::catch_with_exit_code(move || {
        rustc_driver::RunCompiler::new(&args, callbacks)
            .set_using_internal_features(using_internal_features)
            .run()
    })
}

struct AnalysisCallback {
    output_path: PathBuf,
    call_graph: bool,
}

impl rustc_driver::Callbacks for AnalysisCallback {
    fn after_crate_root_parsing<'tcx>(
        &mut self,
        _compiler: &Compiler,
        queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        // Access type context
        queries
            .global_ctxt()
            .expect("global context unavailable")
            .enter(|context| {
                println!("Analyzing output...");
                // Analyze the program using the type context
                let (call_graph, chain_graph) = analysis::analyze(context);

                let dot = if self.call_graph {
                    chain_graph.to_dot()
                } else {
                    call_graph.to_dot()
                };

                println!("Writing graph...");

                match std::fs::write(&self.output_path, dot.clone()) {
                    Ok(()) => {
                        println!("Done!");
                        println!("Wrote to {}", &self.output_path.display());
                    }
                    Err(e) => {
                        eprintln!("Could not write output!");
                        eprintln!("{e}");
                        eprintln!();
                        println!("{dot}");
                    }
                }
            });

        // No need to compile further
        Compilation::Stop
    }
}
