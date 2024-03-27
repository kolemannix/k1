use std::cmp::Ordering;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::os::unix::prelude::ExitStatusExt;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use anyhow::Result;
use clap::Parser;
use inkwell::context::Context;

use crate::codegen_llvm::Codegen;
use crate::parse::{lex_text, ParsedModule, Source};

mod codegen_llvm;
mod lex;
mod parse;
#[cfg(test)]
mod test_suite;
mod typer;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// No Prelude
    #[arg(short, long, default_value_t = false)]
    no_prelude: bool,

    /// Output an LLVM IR file at out_dir/{module_name}.ll
    #[arg(long, default_value_t = true)]
    write_llvm: bool,

    /// No Optimize
    #[arg(long, default_value_t = false)]
    no_llvm_opt: bool,

    /// Dump Module
    #[arg(long, default_value_t = false)]
    dump_module: bool,

    /// Debug Module
    #[arg(long)]
    debug: bool,

    /// Run after compiling
    #[arg(long, default_value_t = false)]
    run: bool,

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

/// If `args.file` points to a directory,
/// - compile all files in the directory.
/// - module name is the name of the directory.
///
/// If `args.file` points to a file,
/// - compile that file only.
/// - module name is the name of the file.
pub fn compile_module<'ctx>(
    ctx: &'ctx Context,
    args: &Args,
    out_dir: impl AsRef<str>,
) -> Result<Codegen<'ctx>> {
    let src_path = &args.file;
    let is_dir = src_path.is_dir();
    let (src_dir, module_name) = if is_dir {
        let src_dir = src_path.canonicalize().unwrap();
        let outname = src_dir.file_name().unwrap().to_str().unwrap().to_string();
        (src_dir, outname)
    } else {
        let src_dir = src_path.parent().unwrap().to_path_buf();
        (src_dir, src_path.file_stem().unwrap().to_str().unwrap().to_string())
    };
    let src_filter = if !is_dir { Some(|p: &Path| *p == *src_path) } else { None };

    let out_dir = out_dir.as_ref();
    let use_prelude = !args.no_prelude;

    let mut module = ParsedModule::make(module_name.to_string());

    let dir_entries = {
        let mut ents = fs::read_dir(&src_dir)?
            .filter_map(|item| item.ok())
            .filter(|item| src_filter.as_ref().map_or(true, |filter| filter(&item.path())))
            .collect::<Vec<_>>();
        ents.sort_by(|ent1, ent2| {
            if ent1.file_name() == "main" {
                Ordering::Less
            } else {
                ent1.file_name().cmp(&ent2.file_name())
            }
        });
        ents
    };
    let mut parse_errors = Vec::new();

    let mut parse_file = |p: &Path, file_id: u32| {
        let content = fs::read_to_string(p)?;
        let name = p.file_name().unwrap();
        println!("Parsing {}", name.to_string_lossy());
        let source = Rc::new(Source::make(
            file_id,
            module_name.to_string(),
            name.to_str().unwrap().to_string(),
            content,
        ));

        let token_vec = lex_text(&source.content, source.file_id)?;
        let mut parser = parse::Parser::make(&token_vec, source.clone(), &mut module);

        let result = parser.parse_module();
        if let Err(e) = result {
            parser.print_error(&e);
            parse_errors.push(e);
        }
        let res: Result<(), anyhow::Error> = Ok(());
        res
    };

    if use_prelude {
        let prelude_path: &Path = Path::new("builtins/prelude.bfl");
        parse_file(prelude_path, 0)?;
    }

    for (idx, f) in dir_entries.iter().enumerate() {
        let file_id = idx as u32 + 1;
        parse_file(&f.path(), file_id)?;
    }

    if !parse_errors.is_empty() {
        anyhow::bail!("Parsing failed")
    }

    let ast = Rc::new(module);

    let mut typed_module = typer::TypedModule::new(ast);
    typed_module.run()?;

    let irgen = Rc::new(typed_module);
    // println!("{irgen}");
    let llvm_optimize = !args.no_llvm_opt;

    let mut codegen: Codegen<'ctx> = Codegen::create(ctx, irgen, args.debug, llvm_optimize);
    codegen.codegen_module();
    codegen.optimize(llvm_optimize)?;

    // TODO: We could do this a lot more efficiently by just feeding the in-memory LLVM IR to clang

    if args.write_llvm {
        let llvm_text = codegen.output_llvm_ir_text();
        let mut f = File::create(format!("{}/{}.ll", out_dir, module_name))
            .expect("Failed to create .ll file");
        f.write_all(llvm_text.as_bytes()).unwrap();
        // println!("{}", codegen.output_llvm_ir_text());
    }
    if args.dump_module {
        println!("{}", codegen.module);
    }

    let mut build_cmd = std::process::Command::new("clang");
    build_cmd.args([
        // "-v",
        if args.debug { "-g" } else { "" },
        if args.debug { "-O0" } else { "-O3" },
        "-Woverride-module",
        "-mmacosx-version-min=14.4",
        &format!("{}/{}.ll", out_dir, module_name),
        "-o",
        &format!("{}/{}.out", out_dir, module_name),
        "-L",
        "bfllib/zig-out/lib",
        "-l",
        "bfllib",
    ]);
    log::info!("Build Command: {:?}", build_cmd);
    let build_status = build_cmd.status().unwrap();

    if !build_status.success() {
        eprintln!("Build failed!");
        std::process::exit(1)
    }

    Ok(codegen)
}

fn main() -> Result<()> {
    env_logger::init();
    let args = Args::parse();
    println!("{:#?}", args);

    static_assert_size!(parse::Definition, 16);
    static_assert_size!(parse::BlockStmt, 80); // Get down below 100
    static_assert_size!(parse::ParsedExpression, 96); // Get back down
    static_assert_size!(typer::TypedExpr, 56);
    static_assert_size!(typer::TypedStmt, 16);
    println!("bfl Compiler v0.1.0");

    let out_dir = "bfl-out";

    let ctx = Context::create();
    let codegen = compile_module(&ctx, &args, out_dir)?;
    let module_name = codegen.name();

    if args.run {
        let mut run_cmd = std::process::Command::new(format!("{}/{}.out", out_dir, module_name));
        log::debug!("Run Command: {:?}", run_cmd);
        let run_status = run_cmd.status().unwrap();

        match run_status.code() {
            Some(code) => {
                println!("Program exited with code: {}", code);
            }
            None => {
                println!("Program was terminated with signal: {:?}", run_status.signal());
            }
        }
    }
    Ok(())
}
