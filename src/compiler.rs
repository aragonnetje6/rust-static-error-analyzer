use std::{
    path::Path,
    process::Command,
    sync::{Arc, Mutex},
};

use cargo::{
    core::{Package, Target, Workspace},
    util::interning::InternedString,
    CargoResult, GlobalContext,
};
use rustc_driver::Compilation;

use crate::{analysis, graphs::CallGraph};

/// Get the compiler arguments used to compile the package by first running `cargo clean` and then `cargo build -vv`.
pub fn get_compiler_args(workspace: &Workspace, gctx: &GlobalContext) -> CompilerCommands {
    println!("Using {}!", cargo_version().trim_end_matches('\n'));
    cargo_clean(workspace, gctx).expect("cleaning failed");
    get_build_arguments(workspace.current().expect("no current package"))
}

/// Run `cargo clean -p PACKAGE`, where the package name is extracted from the given manifest.
fn cargo_clean(workspace: &Workspace, gctx: &GlobalContext) -> CargoResult<()> {
    eprintln!("Cleaning package...");
    cargo::ops::clean(
        workspace,
        &cargo::ops::CleanOptions {
            gctx,
            spec: Vec::new(),
            targets: Vec::new(),
            profile_specified: false,
            requested_profile: InternedString::new("dev"),
            doc: false,
            dry_run: false,
        },
    )
}

pub fn cargo_ast(manifest_path: &Path, target: &Target) -> String {
    eprintln!("Getting AST...");
    let mut command = Command::new("cargo");
    command.arg("+nightly-2025-01-12").arg("rustc");
    let output = if target.is_lib() {
        command.arg("--lib")
    } else {
        command.arg("--bin").arg(target.name())
    }
    .arg("--")
    .arg("-Zunpretty=ast-tree,expanded")
    .current_dir(
        manifest_path
            .parent()
            .expect("Could not get manifest directory!"),
    )
    .output()
    .expect("Could not get AST!");

    let stderr = String::from_utf8(output.stderr).expect("Invalid UTF8!");
    let stdout = String::from_utf8(output.stdout).expect("Invalid UTF8!");

    if output.status.code() != Some(0) {
        eprintln!("Could not get AST!");
        eprintln!("{stderr}");
    }

    stdout
}

/// Run `cargo --version`.
fn cargo_version() -> String {
    let output = Command::new("cargo")
        .arg("--version")
        .output()
        .expect("Could not get cargo version!");

    String::from_utf8(output.stdout).expect("Invalid UTF8!")
}

#[derive(Debug)]
pub struct CompilerCommands {
    pub bin_commands: Vec<Vec<String>>,
    pub lib_commands: Vec<Vec<String>>,
}

fn get_build_arguments(package: &Package) -> CompilerCommands {
    let build_output = cargo_build_verbose(package);
    let invocations = get_rustc_invocations(&build_output, package.targets());
    let bin_commands = invocations
        .bin_invocations
        .iter()
        .map(|invocation| split_args(invocation, package))
        .collect();
    let lib_commands = invocations
        .lib_invocations
        .iter()
        .map(|invocation| split_args(invocation, package))
        .collect();
    CompilerCommands {
        bin_commands,
        lib_commands,
    }
}

/// Run `cargo build -v` on the given manifest.
fn cargo_build_verbose(package: &Package) -> String {
    eprintln!("Building package...");
    let output = Command::new("cargo")
        .arg("+nightly-2025-01-12")
        .arg("build")
        .arg("-v")
        .arg("--manifest-path")
        .arg(package.manifest_path())
        .current_dir(package.root())
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
    lib_invocations: Vec<String>,
}

fn get_rustc_invocations(build_output: &str, targets: &[Target]) -> CompilerInvocations {
    let bin_names: Vec<&str> = targets
        .iter()
        .filter(|target| target.is_bin())
        .map(Target::name)
        .collect();
    let lib_names: Vec<&str> = targets
        .iter()
        .filter(|target| target.is_lib())
        .map(Target::name)
        .collect();
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
    let lib_invocations = build_output
        .split('\n')
        .flat_map(|line| {
            line.split('`').flat_map(|part| {
                part.split("&& ")
                    .filter(|command| {
                        command.contains("rustc")
                            && lib_names
                                .iter()
                                .any(|name| command.contains(&format!("--crate-name {name}")))
                            && command.contains("--crate-type lib")
                    })
                    .map(String::from)
            })
        })
        .collect();
    CompilerInvocations {
        bin_invocations,
        lib_invocations,
    }
}

#[derive(Debug, PartialEq, Eq)]
enum StringArg {
    None,
    SingleQuoted(String),
    DoubleQouted(String),
}

/// Split up individual arguments from the command.
fn split_args(command: &str, package: &Package) -> Vec<String> {
    let mut res = vec![];
    let mut temp = StringArg::None;
    // Split on ' '
    for arg in command.split(' ') {
        let mut arg = arg.to_owned();
        // If this is the path to main.rs, prepend the relative path to the manifest, stripping away Cargo.toml
        if arg.contains(&package.manifest_path().to_string_lossy().to_string()) {
            arg = format!("{}/{arg}", package.root().to_string_lossy());
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

pub fn run_compiler(
    process_builder: &[String],
    callbacks: &mut (dyn rustc_driver::Callbacks + Send),
    using_internal_features: std::sync::Arc<std::sync::atomic::AtomicBool>,
) -> i32 {
    eprintln!("Running compiler...");

    let mut args = process_builder
        .iter()
        .filter(|x| !x.contains("--json"))
        .map(|x| {
            if x.contains("--error-format") {
                String::from("--error-format=short")
            } else {
                x.clone()
            }
        })
        .collect::<Vec<String>>();
    args.push(String::from("--cap-lints"));
    args.push(String::from("allow"));
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
        eprintln!("Analyzing output...");
        // Analyze the program using the type context
        let mut call_graph = self.graph.lock().expect("locking failed");
        analysis::analyze(tcx, &mut call_graph);

        // No need to compile further
        Compilation::Stop
    }
}
