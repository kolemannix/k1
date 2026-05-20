// Copyright (c) 2025 knix
// All rights reserved.

use std::path::PathBuf;
use std::process::ExitCode;

use clap::Parser;
use k1::compiler;
use k1::compiler::{Args, Command};
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

    let out_dir: PathBuf = ".k1-out".into();
    std::fs::create_dir_all(&out_dir)?;

    let Ok(mut program) = compiler::compile_program(&args, &out_dir) else {
        return Ok(ExitCode::FAILURE);
    };
    if args.command.is_check() {
        // Note: I wouldn't mind switching back to exit for the faster
        // exit, but this was hiding a memory bug that causes the lsp
        // and test suite to crash when we try to drop the TypedModule.
        if cfg!(debug_assertions) {
            return Ok(ExitCode::SUCCESS);
        } else {
            std::process::exit(0)
        }
    };
    let llvm_ctx = inkwell::context::Context::create();
    return match compiler::codegen_module(&args, &llvm_ctx, &mut program, &out_dir, true) {
        Ok(cg) => match args.command {
            Command::Check { .. } => unreachable!(),
            Command::Build { .. } => Ok(ExitCode::SUCCESS),
            Command::Run { .. } => {
                compiler::run_compiled_program(&out_dir, cg.name());
                Ok(ExitCode::SUCCESS)
            }
            Command::Test { .. } => {
                eprintln!("k1 test is unimplemented. So you get a free pass.");
                Ok(ExitCode::SUCCESS)
            }
            Command::Repl { .. } => {
                let mut line = String::new();
                loop {
                    let _len = std::io::stdin().read_line(&mut line).unwrap();
                    if &line == "exit\n" {
                        return Ok(ExitCode::SUCCESS);
                    }

                    println!("you said: {}", line);
                    line.clear();
                }
            }
        },
        Err(err) => {
            eprintln!("Codegen error: {err}");
            Ok(ExitCode::FAILURE)
        }
    };
}
