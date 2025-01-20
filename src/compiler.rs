use std::{
    error::Error,
    fmt::Display,
    path::{Path, PathBuf},
    process::Command,
    sync::{Arc, Mutex},
};

use cargo::{
    core::{compiler::Executor, Target, Workspace},
    util::interning::InternedString,
    CargoResult, GlobalContext,
};
use cargo_util::ProcessBuilder;
use rustc_driver::Compilation;
use toml::Table;

use crate::{analysis, graph::CallGraph};

/// Get the compiler arguments used to compile the package by first running `cargo clean` and then `cargo build -vv`.
pub fn get_compiler_args(workspace: &Workspace, gctx: &GlobalContext) -> Vec<ProcessBuilder> {
    println!("Using {}!", cargo_version().trim_end_matches('\n'));
    cargo_clean(&workspace, &gctx).expect("cleaning failed");
    cargo_build_verbose(&gctx, &workspace)
}

/// Run `cargo clean -p PACKAGE`, where the package name is extracted from the given manifest.
fn cargo_clean(workspace: &Workspace, gctx: &GlobalContext) -> CargoResult<()> {
    println!("Cleaning package...");
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

#[derive(Debug, Clone, Copy)]
pub(crate) enum LibOrBin<'a> {
    Lib,
    Bin(&'a str),
}

pub fn cargo_ast(manifest_path: &Path, target: &Target) -> String {
    println!("Getting AST...");
    let mut command = Command::new("cargo");
    command.arg("+nightly").arg("rustc");
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
        println!("{stderr:?}");
    }

    stdout
}

#[derive(Debug)]
pub struct ManifestInfo {
    pub package_name: String,
    pub lib: Option<Crate>,
    pub bins: Vec<Crate>,
    pub root_path: PathBuf,
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
    let crate_root_path = manifest_path
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
                    crate_root_path.join(&bin_path).exists().then_some(Crate {
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
    let lib = crate_root_path.join(&lib_path).exists().then_some(Crate {
        name: lib_name,
        path: lib_path,
    });
    let mut workspace_path =
        std::path::absolute(crate_root_path).expect("directory does not exist");
    workspace_path.pop();
    workspace_path.push("Cargo.toml");
    let root_path = if std::fs::read_to_string(&workspace_path)
        .ok()
        .and_then(|text| text.parse::<Table>().ok())
        .as_ref()
        .and_then(|table| table.get("workspace"))
        .and_then(|value| value.as_table())
        .and_then(|workspace| workspace.get("members"))
        .and_then(|value| value.as_array())
        .is_some_and(|members| {
            members
                .iter()
                .filter_map(|member| member.as_str())
                .any(|member| member == crate_root_path.file_name().expect("no name?"))
        }) {
        workspace_path.pop();
        workspace_path
    } else {
        let mut root_path = manifest_path.to_path_buf();
        root_path.pop();
        root_path
    };
    ManifestInfo {
        package_name,
        lib,
        bins,
        root_path,
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

#[derive(Debug, Clone, Copy)]
struct StopError;

impl Display for StopError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "compilation stopped")
    }
}

impl Error for StopError {}

#[derive(Debug, Default)]
struct GetArgumentExecutor {
    result: Mutex<Vec<ProcessBuilder>>,
}

impl Executor for GetArgumentExecutor {
    fn exec(
        &self,
        cmd: &ProcessBuilder,
        _id: cargo::core::PackageId,
        _target: &cargo::core::Target,
        _mode: cargo::core::compiler::CompileMode,
        _on_stdout_line: &mut dyn FnMut(&str) -> CargoResult<()>,
        _on_stderr_line: &mut dyn FnMut(&str) -> CargoResult<()>,
    ) -> CargoResult<()> {
        if cmd
            .get_env("CARGO_PRIMARY_PACKAGE")
            .is_some_and(|x| x == "1")
        {
            self.result.lock().unwrap().push(cmd.clone());
        }
        Ok(())
    }
}

/// Run `cargo build -v` on the given manifest.
fn cargo_build_verbose(gctx: &GlobalContext, workspace: &Workspace) -> Vec<ProcessBuilder> {
    println!("Building package...");
    let executor = Arc::new(GetArgumentExecutor::default());
    let options = cargo::ops::CompileOptions::new(&gctx, cargo::core::compiler::CompileMode::Build)
        .expect("could not create options????");
    cargo::ops::compile_with_exec(
        &workspace,
        &options,
        &(executor.clone() as Arc<dyn Executor>),
    );
    Arc::into_inner(executor)
        .unwrap()
        .result
        .into_inner()
        .unwrap()
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
    process_builder: ProcessBuilder,
    callbacks: &mut (dyn rustc_driver::Callbacks + Send),
    using_internal_features: std::sync::Arc<std::sync::atomic::AtomicBool>,
) -> i32 {
    println!("Running compiler...");

    let mut args = process_builder
        .get_args()
        .map(|x| x.to_str().unwrap().to_owned())
        .filter(|x| !x.contains("--json"))
        .map(|x| {
            if x.contains("--error-format") {
                String::from("--error-format=short")
            } else {
                x
            }
        })
        .collect::<Vec<String>>();
    args.insert(0, String::new());
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
