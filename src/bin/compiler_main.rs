use std::path::PathBuf;
// Copyright (c) 2026 knix
// All rights reserved.
use std::process::ExitCode;

use clap::Parser;
use k1::compiler;
use k1::compiler::{Args, Command};
use log::info;
use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn main() -> anyhow::Result<ExitCode> {
    run()
    // let compiler_thread = std::thread::Builder::new()
    //     .stack_size(k1::STACK_SIZE)
    //     .name("compiler".to_string())
    //     .spawn(run)
    //     .unwrap();
    //
    // match compiler_thread.join() {
    //     Ok(result) => result,
    //     Err(_) => Ok(ExitCode::FAILURE),
    // }
}

fn run() -> anyhow::Result<ExitCode> {
    let l = Box::leak(Box::new(
        env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info"))
            .format_timestamp(None)
            .build(),
    ));
    let max_level = l.filter();
    log::set_logger(l).unwrap();
    log::set_max_level(max_level);
    let args = Args::parse();
    log::debug!("{:#?}", args);

    let Ok(mut program) = compiler::compile_program(&args) else {
        return Ok(ExitCode::FAILURE);
    };
    if !args.command.kind().codegens() || matches!(args.command, Command::Server { .. }) {
        compiler::report_trace(&args, &program);
    }
    if !args.command.kind().codegens() {
        // Setup runs as a module-load gate, so a successful compile means setup ran.
        // In release builds, just exit fast
        if cfg!(debug_assertions) {
            return Ok(ExitCode::SUCCESS);
        } else {
            std::process::exit(0)
        }
    };
    if matches!(args.command, Command::Server { .. }) {
        use std::sync::{Arc, Mutex};
        k1::server::serve(Arc::new(Mutex::new(Some(Box::new(program)))));
        return Ok(ExitCode::SUCCESS);
    }
    if matches!(args.command, Command::Run { .. } | Command::Test { .. })
        && !program.program_settings.executable
    {
        eprintln!(
            "{} is a library module; run/test require an executable module",
            program.program_name()
        );
        return Ok(ExitCode::FAILURE);
    }
    let llvm_ctx = inkwell::context::Context::create();
    let codegen_result = compiler::codegen_module(&args, &llvm_ctx, &mut program);
    compiler::report_trace(&args, &program);
    let success = match codegen_result {
        Ok(()) => match args.command {
            Command::Check { .. } => unreachable!(),
            Command::Build { .. } => true,
            Command::Run { ref program_args, .. } => {
                info!("run executable: {}", program.program_name());
                let exit_code = compiler::run_compiled_program(
                    &program.ast.idents,
                    program.config.target,
                    program.config.out_dir,
                    program.config.home_dir,
                    program.program_name(),
                    false,
                    program_args,
                );
                exit_code == Some(0)
            }
            Command::Test { .. } => {
                info!("test executable: {}", program.program_name());
                let exit_code = compiler::run_compiled_program(
                    &program.ast.idents,
                    program.config.target,
                    program.config.out_dir,
                    program.config.home_dir,
                    program.program_name(),
                    true,
                    &[],
                );
                exit_code == Some(0)
            }
            Command::Server { .. } => unreachable!("server runs before codegen"),
            Command::Setup { .. } => unreachable!("setup exits after compile"),
            Command::Clean { .. } => {
                // Clear the out dir
                let out_dir_path =
                    PathBuf::from(program.ast.idents.get_string(program.config.out_dir));
                if std::fs::exists(&out_dir_path).unwrap() {
                    std::fs::remove_dir_all(&out_dir_path)?;
                }
                true
            }
        },
        Err(err) => {
            eprintln!("Codegen error: {err}");
            false
        }
    };
    if !cfg!(debug_assertions) {
        std::process::exit(if success { 0 } else { 1 })
    }
    Ok(if success { ExitCode::SUCCESS } else { ExitCode::FAILURE })
}
