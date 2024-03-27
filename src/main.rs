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
mod prelude;
#[cfg(test)]
mod test_suite;
mod typer;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// No Prelude
    #[arg(short, long, default_value_t = false)]
    no_prelude: bool,

    /// Print LLVM to stdout
    #[arg(long, default_value_t = false)]
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

pub fn compile_directory_program<'ctx>(
    ctx: &'ctx Context,
    source_dir: impl AsRef<Path>,
    args: Args,
    out_dir: impl AsRef<str>,
    sources_filter: Option<impl Fn(&PathBuf) -> bool>,
) -> Result<Codegen<'ctx>> {
    let source_path = source_dir.as_ref().to_owned();
    let module_name = source_path.file_name().unwrap().to_str().unwrap();
    let main_path = source_path.join("main.bfl");
    println!("main_path: {}", main_path.to_string_lossy());
    let out_dir = out_dir.as_ref();
    let use_prelude = !args.no_prelude;

    let mut module = ParsedModule::make(module_name.to_string());

    let dir_entries = {
        let mut ents = fs::read_dir(&source_path)?
            .filter_map(|item| item.ok())
            .filter(|item| sources_filter.as_ref().map_or(true, |filter| filter(&item.path())))
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
        let mut parser = crate::parse::Parser::make(&token_vec, source.clone(), &mut module);

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
        println!("{}", codegen.output_llvm_ir_text());
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

pub fn compile_single_file_program<'ctx>(
    ctx: &'ctx Context,
    filename: impl AsRef<str>,
    source_dir: impl AsRef<str>,
    source_path: impl AsRef<Path>,
    no_prelude: bool,
    out_dir: impl AsRef<str>,
    llvm_optimize: bool,
    debug: bool,
) -> Result<Codegen<'ctx>> {
    let filename = filename.as_ref();
    let source_dir = source_dir.as_ref();
    let out_dir = out_dir.as_ref();
    let use_prelude = !no_prelude;
    let source = fs::read_to_string(source_path.as_ref()).expect("could not read source directory");
    let ast = parse::parse_text(Rc::new(Source::make(
        0,
        source_dir.to_string(),
        filename.to_string(),
        source,
    )))
    .unwrap_or_else(|e| {
        eprintln!("Encountered ParseError on file '{}/{}': {:?}", source_dir, filename, e);
        panic!("parse error");
    });
    let ast = Rc::new(ast);
    let mut irgen = typer::TypedModule::new(ast);
    irgen.run()?;
    let irgen = Rc::new(irgen);
    // println!("{irgen}");
    let mut codegen: Codegen<'ctx> = Codegen::create(ctx, irgen, debug, llvm_optimize);
    codegen.codegen_module();
    codegen.optimize(llvm_optimize)?;

    let llvm_text = codegen.output_llvm_ir_text();
    let mut f =
        File::create(format!("{}/{}.ll", out_dir, filename)).expect("Failed to create .ll file");
    f.write_all(llvm_text.as_bytes()).unwrap();

    // TODO: We could do this a lot more efficiently by just feeding the in-memory LLVM IR to clang

    // codegen.emit_object_file(out_dir)?;

    let mut build_cmd = std::process::Command::new("clang");
    build_cmd.args([
        // "-v",
        if debug { "-g" } else { "" },
        if debug { "-O0" } else { "-O3" },
        "-Woverride-module",
        &format!("{}/{}.ll", out_dir, filename),
        "-o",
        &format!("{}/{}.out", out_dir, filename),
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
    let src_path = &args.file;
    let no_prelude = args.no_prelude;
    let out_dir = "bfl-out";

    let ctx = Context::create();
    let src_path = src_path.canonicalize().unwrap();
    let filename = src_path.file_name().unwrap().to_str().unwrap();
    let codegen =
        compile_directory_program(&ctx, &src_path, args, out_dir, None::<fn(&PathBuf) -> bool>)?;
    // let src_dir = src_path.parent().unwrap().to_str().unwrap();
    // let codegen = compile_single_file_program(
    //     &ctx,
    //     filename,
    //     src_dir,
    //     src_path,
    //     no_prelude,
    //     out_dir,
    //     !args.no_llvm_opt,
    //     args.debug,
    // )?;

    if args.run {
        let mut run_cmd = std::process::Command::new(format!("{}/{}.out", out_dir, filename));
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
