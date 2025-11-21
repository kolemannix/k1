// Copyright (c) 2025 knix
// All rights reserved.

use std::path::PathBuf;
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
        env_logger::Builder::new()
            .format_timestamp(None)
            .filter_level(log::LevelFilter::Debug)
            .build(),
    ));
    log::set_logger(l).unwrap();
    // For permanent debug:
    // log::set_max_level(log::LevelFilter::Debug);
    log::set_max_level(log::LevelFilter::Info);
    let args = Args::parse();
    log::debug!("{:#?}", args);

    info!("k1 Compiler v0.1.0");

    let out_dir: PathBuf = ".k1-out".into();
    std::fs::create_dir_all(&out_dir)?;

    let Ok(program) = compiler::compile_program(&args, &out_dir) else {
        return Ok(ExitCode::FAILURE);
    };
    let program_name = program.program_name();
    if matches!(args.command, Command::Check { .. }) {
        // Note: I wouldn't mind switching back to exit for the faster
        // exit, but this was hiding a memory bug that causes the lsp
        // and test suite to crash when we try to drop the TypedModule.
        return Ok(ExitCode::SUCCESS);
    };
    let llvm_ctx = inkwell::context::Context::create();
    return match compiler::codegen_module(&args, &llvm_ctx, &program, &out_dir, true) {
        Ok(_codegen) => {
            if matches!(args.command, Command::Build { .. }) {
                Ok(ExitCode::SUCCESS)
            } else {
                compiler::run_compiled_program(&out_dir, program_name);
                Ok(ExitCode::SUCCESS)
            }
        }
        Err(err) => {
            eprintln!("Codegen error: {err}");
            Ok(ExitCode::FAILURE)
        }
    };
}
