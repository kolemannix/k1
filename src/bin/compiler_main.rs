use clap::Parser;
use k1::compiler;
use k1::compiler::{Args, Command};
use log::info;

fn main() -> anyhow::Result<()> {
    let l = Box::leak(Box::new(
        env_logger::Builder::new().filter_level(log::LevelFilter::Debug).build(),
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
    let Ok(module) = compiler::compile_module(&args) else {
        std::process::exit(1);
    };
    let module_name = module.name();
    info!("done waiting on compile thread");
    if matches!(args.command, Command::Check { .. }) {
        std::process::exit(0)
    };
    let llvm_ctx = inkwell::context::Context::create();
    match compiler::codegen_module(&args, &llvm_ctx, &module, out_dir, true) {
        Ok(_codegen) => {
            if matches!(args.command, Command::Build { .. }) {
                std::process::exit(0)
            };
            compiler::run_compiled_program(out_dir, module_name);
            std::process::exit(0);
        }
        Err(err) => {
            eprintln!("Codegen error: {err}");
            std::process::exit(1);
        }
    };
}
