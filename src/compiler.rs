use std::{
    path::Path,
    process::Command,
    sync::{Arc, Mutex},
};

use rustc_driver::Compilation;
use toml::Table;

use crate::{analysis, graph::CallGraph};

#[derive(Debug)]
pub struct CompilerCommands {
    pub bin_commands: Vec<Vec<String>>,
    pub lib_command: Option<Vec<String>>,
}

/// Get the compiler arguments used to compile the package by first running `cargo clean` and then `cargo build -vv`.
pub fn get_compiler_args(
    relative_manifest_path: &Path,
    manifest_path: &Path,
    manifest_info: &ManifestInfo,
) -> CompilerCommands {
    println!("Using {}!", cargo_version().trim_end_matches('\n'));

    cargo_clean(manifest_path, &manifest_info.package_name);

    let build_output = cargo_build_verbose(manifest_path);

    let invocations = get_rustc_invocations(&build_output, manifest_info);

    let bin_commands = invocations
        .bin_invocations
        .iter()
        .map(|invocation| split_args(relative_manifest_path, invocation, manifest_info))
        .collect();
    let lib_command = invocations
        .lib_invocation
        .map(|invocation| split_args(relative_manifest_path, &invocation, manifest_info));

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
pub struct ManifestInfo {
    pub package_name: String,
    pub lib: Option<Crate>,
    pub bins: Vec<Crate>,
}

#[derive(Debug)]
pub struct Crate {
    pub name: String,
    pub path: String,
}

/// Extract the package name from the given manifest.
pub fn get_manifest_info(manifest_path: &Path) -> ManifestInfo {
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
                        .map_or_else(|| package_name.clone(), str::to_owned);
                    let bin_path = bin_entry
                        .as_table()
                        .and_then(|bin| bin.get("path"))
                        .and_then(|name| name.as_str())
                        .map_or_else(|| String::from("src/main.rs"), str::to_owned);
                    root_path.join(&bin_path).exists().then_some(Crate {
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
        .map_or_else(|| package_name.clone(), str::to_owned);
    let lib_path = table
        .get("lib")
        .and_then(|lib| lib.as_table())
        .and_then(|lib| lib.get("path"))
        .and_then(|name| name.as_str())
        .map_or_else(|| String::from("src/lib.rs"), str::to_owned);
    let lib = root_path.join(&lib_path).exists().then_some(Crate {
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
        .map(|bin| bin.name.clone())
        .chain([manifest_info.package_name.clone()])
        .map(|name| name.replace('-', "_"))
        .collect();
    let lib_name = manifest_info
        .lib
        .as_ref()
        .map_or(&manifest_info.package_name, |lib| &lib.name)
        .replace('-', "_");
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
pub fn run_compiler(
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

#[derive(Debug)]
pub struct AnalysisCallbacks {
    pub graph: Arc<Mutex<CallGraph>>,
}

impl AnalysisCallbacks {
    pub fn new(graph: Arc<Mutex<CallGraph>>) -> Self {
        Self { graph }
    }
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
        let mut call_graph = self.graph.lock().expect("locking failed");
        analysis::analyze(tcx, &mut call_graph);

        // No need to compile further
        Compilation::Stop
    }
}
