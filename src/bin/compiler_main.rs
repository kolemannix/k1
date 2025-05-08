use std::process::ExitCode;

use clap::Parser;
use k1::compiler;
use k1::compiler::{Args, Command};
use log::info;
use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn main() -> anyhow::Result<ExitCode> {
    let compiler_thread = std::thread::Builder::new()
        .stack_size(k1::STACK_SIZE)
        .name("compiler".to_string())
        .spawn(run)
        .unwrap();

    compiler_thread.join().unwrap()
}

fn run() -> anyhow::Result<ExitCode> {
    let l = Box::leak(Box::new(
        env_logger::Builder::new()
            .format_timestamp(None)
            .filter_level(log::LevelFilter::Debug)
            .build(),
    ));
    log::set_logger(l).unwrap();
    log::set_max_level(log::LevelFilter::Info);
    let args = Args::parse();
    info!("{:#?}", args);

    info!("k1 Compiler v0.1.0");

    let out_dir = ".k1-out";
    std::fs::create_dir_all(out_dir)?;

    // let mut i = 0;
    // while i < 1000 {
    //     compiler::compile_module(&args);
    //     i += 1;
    // }
    let Ok(module) = compiler::compile_module(&args) else { return Ok(ExitCode::FAILURE) };
    let module_name = module.name();
    info!("done waiting on compile thread");
    if matches!(args.command, Command::Check { .. }) {
        // nocommit: I wouldn't mind switching back to exit for the faster
        // exit, but this was hiding a memory bug that causes the lsp
        // and test suite to crash when we try to drop the TypedModule.
        return Ok(ExitCode::SUCCESS);
    };
    let llvm_ctx = inkwell::context::Context::create();
    return match compiler::codegen_module(&args, &llvm_ctx, &module, out_dir, true) {
        Ok(_codegen) => {
            if matches!(args.command, Command::Build { .. }) {
                Ok(ExitCode::SUCCESS)
            } else {
                compiler::run_compiled_program(out_dir, module_name);
                Ok(ExitCode::SUCCESS)
            }
        }
        Err(err) => {
            eprintln!("Codegen error: {err}");
            Ok(ExitCode::FAILURE)
        }
    };
}
