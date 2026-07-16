use std::net::TcpListener;
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

    let Ok(mut program) = compiler::compile_program(&args) else {
        return Ok(ExitCode::FAILURE);
    };
    if args.command.is_check() {
        // In release builds, just exit fast
        if cfg!(debug_assertions) {
            return Ok(ExitCode::SUCCESS);
        } else {
            std::process::exit(0)
        }
    };
    let llvm_ctx = inkwell::context::Context::create();
    return match compiler::codegen_module(&args, &llvm_ctx, &mut program) {
        Ok(mut cg) => match args.command {
            Command::Check { .. } => unreachable!(),
            Command::Build { .. } => Ok(ExitCode::SUCCESS),
            Command::Run { .. } => {
                info!("run executable: {}", cg.name());
                let home_dir = &cg.k1.config.home_dir;
                let out_dir = &cg.k1.config.out_dir;
                compiler::run_compiled_program(out_dir, home_dir, cg.name(), false);
                Ok(ExitCode::SUCCESS)
            }
            Command::Test { .. } => {
                info!("test executable: {}", cg.name());
                let home_dir = &cg.k1.config.home_dir;
                let out_dir = &cg.k1.config.out_dir;
                let exit_code = compiler::run_compiled_program(out_dir, home_dir, cg.name(), true);
                if exit_code != Some(0) { Ok(ExitCode::FAILURE) } else { Ok(ExitCode::SUCCESS) }
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
            Command::Server { .. } => {
                let listener = TcpListener::bind("127.0.0.1:8080").expect("Could not bind to port");
                println!("k1 server running on http://127.0.0.1:8080");

                for stream in listener.incoming() {
                    match stream {
                        Ok(stream) => {
                            k1::server::handle_client(&mut cg, stream);
                        }
                        Err(e) => eprintln!("Connection failed: {}", e),
                    }
                }
                Ok(ExitCode::SUCCESS)
            }
        },
        Err(err) => {
            eprintln!("Codegen error: {err}");
            Ok(ExitCode::FAILURE)
        }
    };
}
