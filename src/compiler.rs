use std::{
    path::Path,
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

use crate::{analysis, graphs::CallGraph};

/// Get the compiler arguments used to compile the package by first running `cargo clean` and then `cargo build -vv`.
pub fn get_compiler_args(workspace: &Workspace, gctx: &GlobalContext) -> Vec<ProcessBuilder> {
    println!("Using {}!", cargo_version().trim_end_matches('\n'));
    // cargo_clean(workspace, gctx).expect("cleaning failed");
    get_build_arguments(gctx, workspace)
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

/// Run `cargo --version`.
fn cargo_version() -> String {
    let output = Command::new("cargo")
        .arg("--version")
        .output()
        .expect("Could not get cargo version!");

    String::from_utf8(output.stdout).expect("Invalid UTF8!")
}

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
            self.result
                .lock()
                .expect("mutex poisoned, something crashed")
                .push(cmd.clone());
        }
        Ok(())
    }

    fn force_rebuild(&self, unit: &cargo::core::compiler::Unit) -> bool {
        dbg!(unit.is_local())
    }
}

fn get_build_arguments(gctx: &GlobalContext, workspace: &Workspace) -> Vec<ProcessBuilder> {
    println!("Building package...");
    let executor = Arc::new(GetArgumentExecutor::default());
    let options = cargo::ops::CompileOptions::new(gctx, cargo::core::compiler::CompileMode::Build)
        .expect("could not create options????");
    cargo::ops::compile_with_exec(
        workspace,
        &options,
        &(executor.clone() as Arc<dyn Executor>),
    )
    .expect("Cargo failed to build package");
    Arc::into_inner(executor)
        .expect("should never fail")
        .result
        .into_inner()
        .expect("should never fail")
}

pub fn run_compiler(
    process_builder: &ProcessBuilder,
    callbacks: &mut (dyn rustc_driver::Callbacks + Send),
    using_internal_features: std::sync::Arc<std::sync::atomic::AtomicBool>,
) -> i32 {
    println!("Running compiler...");

    let mut args = process_builder
        .get_args()
        .map(|x| {
            x.to_str()
                .expect("non-UTF8 filenames are not supported")
                .to_owned()
        })
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
        println!("Analyzing output...");
        // Analyze the program using the type context
        let mut call_graph = self.graph.lock().expect("locking failed");
        analysis::analyze(tcx, &mut call_graph);

        // No need to compile further
        Compilation::Stop
    }
}
