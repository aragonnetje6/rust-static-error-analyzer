#![feature(rustc_private)]
#![warn(clippy::pedantic, clippy::unwrap_used)]
#![allow(clippy::cast_precision_loss)]

mod analysis;
mod graph;

extern crate rustc_ast;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_parse;
extern crate rustc_session;

use clap::Parser;
use rustc_driver::Compilation;
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
    let compiler_commands = get_compiler_args(&args.manifest, &manifest_path);

    // Enable CTRL + C
    rustc_driver::install_ctrlc_handler();

    // Install a panic hook that will print the ICE message on unexpected panics.
    let using_internal_features =
        rustc_driver::install_ice_hook(rustc_driver::DEFAULT_BUG_REPORT_URL, |_| ());

    // This allows tools to enable rust logging without having to magically match rustc’s tracing crate version.
    rustc_driver::init_rustc_env_logger(&early_dcx);

    // Run the compiler using the retrieved args.
    if let Some(compiler_command) = compiler_commands.lib_command {
        run_compiler(
            compiler_command,
            &mut AnalysisCallbacks {
                output_path: output_path.clone(),
                call_graph: args.call_graph,
            },
            using_internal_features.clone(),
        );
    }
    for compiler_command in compiler_commands.bin_commands {
        run_compiler(
            compiler_command,
            &mut AnalysisCallbacks {
                output_path: output_path.clone(),
                call_graph: args.call_graph,
            },
            using_internal_features.clone(),
        );
    }
}

/// Turn relative path into absolute path
fn absolute_path(cargo_path: &Path) -> PathBuf {
    std::env::current_dir()
        .expect("current directory is invalid")
        .join(cargo_path)
}

#[derive(Debug)]
struct CompilerCommands {
    bin_commands: Vec<Vec<String>>,
    lib_command: Option<Vec<String>>,
}

/// Get the compiler arguments used to compile the package by first running `cargo clean` and then `cargo build -vv`.
fn get_compiler_args(relative_manifest_path: &Path, manifest_path: &Path) -> CompilerCommands {
    println!("Using {}!", cargo_version().trim_end_matches('\n'));

    let manifest_info = get_manifest_info(manifest_path);

    cargo_clean(manifest_path, &manifest_info.package_name);

    let build_output = cargo_build_verbose(manifest_path);

    let invocations = get_rustc_invocations(&build_output, &manifest_info);

    let bin_commands = invocations
        .bin_invocations
        .iter()
        .map(|invocation| split_args(relative_manifest_path, invocation, &manifest_info))
        .collect();
    let lib_command = invocations
        .lib_invocation
        .map(|invocation| split_args(relative_manifest_path, &invocation, &manifest_info));

    CompilerCommands {
        bin_commands,
        lib_command,
    }
}

#[derive(Debug, PartialEq, Eq)]
enum StringArg {
    None,
    SingleQuoted(String),
    DoubleQouted(String),
}

/// Split up individual arguments from the command.
fn split_args(
    relative_manifest_path: &Path,
    command: &str,
    manifest_info: &ManifestInfo,
) -> Vec<String> {
    let mut res = vec![];
    let mut temp = StringArg::None;

    // Split on ' '
    for arg in command.split(' ') {
        let mut arg = arg.to_owned();

        // If this is the path to main.rs, prepend the relative path to the manifest, stripping away Cargo.toml
        for target in manifest_info.bins.iter().chain(&manifest_info.lib) {
            if arg.contains(&target.path) {
                arg = format!(
                    "{}/{arg}",
                    relative_manifest_path
                        .parent()
                        .expect("manifest folder invalid")
                        .to_str()
                        .expect("invalid characters in path")
                );
            }
        }
        temp = match temp {
            StringArg::None => {
                if arg.starts_with('"') && arg.ends_with('"') {
                    res.push(
                        arg.strip_prefix('"')
                            .and_then(|x| x.strip_suffix('"'))
                            .expect("unquoting failure")
                            .to_owned(),
                    );
                    StringArg::None
                } else if arg.starts_with('\'') && arg.ends_with('\'') {
                    res.push(
                        arg.strip_prefix('\'')
                            .and_then(|x| x.strip_suffix('\''))
                            .expect("unquoting failure")
                            .to_owned(),
                    );
                    StringArg::None
                } else if arg.starts_with('"') {
                    StringArg::DoubleQouted(
                        arg.strip_prefix('"').expect("unquoting failure").to_owned(),
                    )
                } else if arg.starts_with('\'') {
                    StringArg::SingleQuoted(
                        arg.strip_prefix('\'')
                            .expect("unquoting failure")
                            .to_owned(),
                    )
                } else {
                    res.push(arg);
                    StringArg::None
                }
            }
            StringArg::SingleQuoted(mut temp) => {
                temp.push(' ');
                if arg.ends_with('\'') {
                    temp.push_str(arg.strip_suffix('\'').expect("unquoting failure"));
                    res.push(temp);
                    StringArg::None
                } else {
                    temp.push_str(&arg);
                    temp.push(' ');
                    StringArg::SingleQuoted(temp)
                }
            }
            StringArg::DoubleQouted(mut temp) => {
                temp.push(' ');
                if arg.ends_with('"') {
                    temp.push_str(arg.strip_suffix('"').expect("unquoting failure"));
                    res.push(temp);
                    StringArg::None
                } else {
                    temp.push_str(&arg);
                    StringArg::DoubleQouted(temp)
                }
            }
        };
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

#[derive(Debug)]
struct ManifestInfo {
    package_name: String,
    lib: Option<Crate>,
    bins: Vec<Crate>,
}

#[derive(Debug)]
struct Crate {
    name: String,
    path: String,
}

/// Extract the package name from the given manifest.
fn get_manifest_info(manifest_path: &Path) -> ManifestInfo {
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
    let root_path = manifest_path
        .parent()
        .expect("crate directory does not exist");
    let bins = table
        .get("bin")
        .map(|bin| {
            bin.as_array()
                .expect("invalid bins section")
                .iter()
                .filter_map(|bin_entry| {
                    let bin_name = bin_entry
                        .as_table()
                        .and_then(|bin| bin.get("name"))
                        .and_then(|name| name.as_str())
                        .map(str::to_owned)
                        .unwrap_or_else(|| package_name.clone());
                    let bin_path = bin_entry
                        .as_table()
                        .and_then(|bin| bin.get("path"))
                        .and_then(|name| name.as_str())
                        .map(str::to_owned)
                        .unwrap_or_else(|| String::from("src/main.rs"));
                    root_path.join(&bin_path).exists().then(|| Crate {
                        name: bin_name,
                        path: bin_path,
                    })
                })
                .collect()
        })
        .unwrap_or_default();
    let lib_name = table
        .get("lib")
        .and_then(|lib| lib.as_table())
        .and_then(|lib| lib.get("name"))
        .and_then(|name| name.as_str())
        .map(str::to_owned)
        .unwrap_or_else(|| package_name.clone());
    let lib_path = table
        .get("lib")
        .and_then(|lib| lib.as_table())
        .and_then(|lib| lib.get("path"))
        .and_then(|name| name.as_str())
        .map(str::to_owned)
        .unwrap_or_else(|| String::from("src/lib.rs"));
    let lib = root_path.join(&lib_path).exists().then(|| Crate {
        name: lib_name,
        path: lib_path,
    });
    ManifestInfo {
        package_name,
        lib,
        bins,
    }
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

#[derive(Debug)]
struct CompilerInvocations {
    bin_invocations: Vec<String>,
    lib_invocation: Option<String>,
}

/// Gets the rustc invocation command from the output of `cargo build -vv`.
fn get_rustc_invocations(build_output: &str, manifest_info: &ManifestInfo) -> CompilerInvocations {
    let bin_names: Vec<String> = manifest_info
        .bins
        .iter()
        .map(|bin| bin.name.to_owned())
        .chain([manifest_info.package_name.to_owned()])
        .map(|name| name.replace("-", "_"))
        .collect();
    let lib_name = manifest_info
        .lib
        .as_ref()
        .map(|lib| &lib.name)
        .unwrap_or(&manifest_info.package_name)
        .replace("-", "_");
    let bin_invocations = build_output
        .split('\n')
        .flat_map(|line| {
            line.split('`').flat_map(|part| {
                part.split("&& ")
                    .filter(|command| {
                        command.contains("rustc")
                            && bin_names
                                .iter()
                                .any(|name| command.contains(&format!("--crate-name {name}")))
                            && command.contains("--crate-type bin")
                    })
                    .map(String::from)
            })
        })
        .collect();
    let lib_invocation = build_output
        .split('\n')
        .flat_map(|line| {
            line.split('`').flat_map(|part| {
                part.split("&& ")
                    .filter(|command| {
                        command.contains("rustc")
                            && command.contains(&format!("--crate-name {lib_name}"))
                            && command.contains("--crate-type lib")
                    })
                    .map(String::from)
            })
        })
        .next();
    CompilerInvocations {
        bin_invocations,
        lib_invocation,
    }
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
            .run();
        Ok(())
    })
}

struct AnalysisCallbacks {
    output_path: PathBuf,
    call_graph: bool,
}

impl rustc_driver::Callbacks for AnalysisCallbacks {
    fn after_analysis(
        &mut self,
        _compiler: &rustc_interface::interface::Compiler,
        tcx: rustc_middle::ty::TyCtxt<'_>,
    ) -> Compilation {
        // Access type context
        println!("Analyzing output...");
        // Analyze the program using the type context
        let (call_graph, chain_graph) = analysis::analyze(tcx);

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

        // No need to compile further
        Compilation::Stop
    }
}
