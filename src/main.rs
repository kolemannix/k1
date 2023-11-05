use std::rc::Rc;

mod codegen_llvm;
mod lex;
mod parse;
mod prelude;
#[cfg(test)]
mod test_suite;
mod typer;

use anyhow::Result;
use inkwell::context::Context;

use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};

use crate::codegen_llvm::Codegen;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// No Prelude
    #[arg(short, long, default_value_t = false)]
    no_prelude: bool,

    /// Print LLVM to stdout
    #[arg(long, default_value_t = false)]
    print_llvm: bool,

    /// No Optimize
    #[arg(long, default_value_t = false)]
    no_llvm_opt: bool,

    /// Print AST to stdout
    #[arg(long, default_value_t = false)]
    print_ast: bool,

    /// File
    file: PathBuf,
}

/// Type size assertion. The first argument is a type and the second argument is its expected size.
/// Cool trick from rustc.
#[macro_export]
macro_rules! static_assert_size {
    ($ty:ty, $size:expr) => {
        const _: [(); $size] = [(); ::std::mem::size_of::<$ty>()];
    };
}

pub fn compile_single_file_program<'ctx>(
    ctx: &'ctx Context,
    filename: &str,
    source: &str,
    no_prelude: bool,
    out_dir: &str,
    llvm_optimize: bool,
) -> Result<Codegen<'ctx>> {
    let use_prelude = !no_prelude;
    let ast = parse::parse_text(source, filename, use_prelude).unwrap_or_else(|e| {
        eprintln!("Encountered ParseError on file '{}': {:?}", filename, e);
        panic!("parse error");
    });
    let ast = Rc::new(ast);
    let mut irgen = typer::TypedModule::new(ast);
    irgen.run()?;
    let irgen = Rc::new(irgen);
    // println!("{irgen}");
    let mut codegen: Codegen<'ctx> = Codegen::create(ctx, irgen);
    codegen.codegen_module();
    codegen.optimize(llvm_optimize)?;

    let llvm_text = codegen.output_llvm_ir_text();
    let mut f = File::create(format!("{}/{}.ll", out_dir, filename))?;
    f.write_all(llvm_text.as_bytes()).unwrap();

    codegen.emit_object_file(out_dir)?;

    let mut build_cmd = std::process::Command::new("clang");
    build_cmd.args([
        "-g",
        &format!("{}/{}.o", out_dir, filename), //"-l", "nxlib/zig-out/lib/nxlib.a"
        "-o",
        &format!("{}/{}.out", out_dir, filename),
        "-L",
        "nxlib/zig-out/lib",
        "-l",
        "nxlib",
        "-O2",
    ]);
    log::debug!("Build Command: {:?}", build_cmd);
    let build_output = build_cmd.output().unwrap();
    if !build_output.status.success() {
        eprintln!("Build failed!");
        eprintln!("{}", String::from_utf8(build_output.stderr).unwrap());
        std::process::exit(1)
    }

    Ok(codegen)
}

fn main() -> Result<()> {
    env_logger::init();
    let args = Args::parse();
    println!("{:?}", args);

    static_assert_size!(parse::Definition, 16);
    static_assert_size!(parse::BlockStmt, 224); // Get down below 100
    static_assert_size!(parse::Expression, 104); // Get back down
    static_assert_size!(typer::TypedExpr, 56);
    static_assert_size!(typer::TypedStmt, 16);
    println!("NexLang Compiler v0.1.0");
    let src_path = &args.file;
    let no_prelude = args.no_prelude;
    let out_dir = "nx-out";

    let ctx = Context::create();
    let filename = Path::new(&src_path).file_name().unwrap().to_str().unwrap();
    let src = fs::read_to_string(src_path).expect("could not read source directory");
    let codegen =
        compile_single_file_program(&ctx, filename, &src, no_prelude, out_dir, !args.no_llvm_opt)?;
    if args.print_llvm {
        println!("{}", codegen.output_llvm_ir_text());
    }

    // println!("Build Output: {}", String::from_utf8(build_output.stdout).unwrap());

    let mut run_cmd = std::process::Command::new(format!("{}/{}.out", out_dir, filename));
    log::debug!("Run Command: {:?}", run_cmd);
    let run_output = run_cmd.output().unwrap();

    println!("{}", String::from_utf8(run_output.stdout).unwrap());
    println!("Exit Code: {}", run_output.status.code().unwrap());
    // println!("{}", llvm_text);
    Ok(())
}
