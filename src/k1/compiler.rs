use std::fs;
use std::fs::File;
use std::io::Write;
use std::os::unix::prelude::ExitStatusExt;
use std::path::Path;
use std::time::Instant;

use crate::parse::print_error_location;
use crate::parse::{self, print_error};
use crate::typer::TypedModule;
use anyhow::{bail, Result};
use inkwell::context::Context;
use log::info;

use crate::codegen_llvm::Codegen;
use parse::{lex_text, ParsedModule, Source};

use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// No core
    #[arg(short, long, default_value_t = false)]
    pub no_std: bool,

    /// Output an LLVM IR file at out_dir/{module_name}.ll
    #[arg(long, default_value_t = true)]
    pub write_llvm: bool,

    /// No Optimize
    #[arg(long, default_value_t = false)]
    pub no_llvm_opt: bool,

    /// Dump Module
    #[arg(long, default_value_t = false)]
    pub dump_module: bool,

    /// Debug Module
    #[arg(long)]
    pub debug: bool,

    /// Run after compiling
    #[arg(long, default_value_t = false)]
    pub run: bool,

    /// GUI
    #[arg(long, default_value_t = false)]
    pub gui: bool,

    /// File
    pub file: PathBuf,
}

/// Type size assertion. The first argument is a type and the second argument is its expected size.
/// Cool trick from rustc.
#[macro_export]
macro_rules! static_assert_size {
    ($ty:ty, $size:expr) => {
        const _: [(); $size] = [(); ::std::mem::size_of::<$ty>()];
    };
}

pub struct CompileModuleError {
    pub parsed_module: Option<Box<ParsedModule>>,
    pub module: Option<Box<TypedModule>>,
}

/// If `args.file` points to a directory,
/// - compile all files in the directory.
/// - module name is the name of the directory.
///
/// If `args.file` points to a file,
/// - compile that file only.
/// - module name is the name of the file.
pub fn compile_module(args: &Args) -> std::result::Result<TypedModule, CompileModuleError> {
    let start_parse = std::time::Instant::now();
    let src_path = &args.file.canonicalize().unwrap();
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

    let use_std = !args.no_std;

    let mut parsed_module = ParsedModule::make(module_name.to_string());

    let dir_entries = {
        let mut ents = fs::read_dir(src_dir)
            .unwrap()
            .filter_map(|item| item.ok())
            .filter(|item| src_filter.as_ref().map_or(true, |filter| filter(&item.path())))
            .collect::<Vec<_>>();
        ents.sort_by_key(|ent1| ent1.file_name());
        ents
    };
    let mut parse_errors = Vec::new();

    let mut parse_file = |path: &Path| {
        let content = fs::read_to_string(path).unwrap();
        let name = path.file_name().unwrap();
        eprintln!("Parsing {}", name.to_string_lossy());
        let file_id = parsed_module.sources.next_file_id();
        let source = Source::make(
            file_id,
            path.canonicalize().unwrap().parent().unwrap().to_str().unwrap().to_string(),
            name.to_str().unwrap().to_string(),
            content,
        );
        let token_vec = match lex_text(&mut parsed_module, source) {
            Ok(token_vec) => token_vec,
            Err(e) => {
                print_error(&parsed_module, &e);
                parse_errors.push(e);
                return;
            }
        };
        let mut parser = parse::Parser::make(&token_vec, file_id, &mut parsed_module);

        let result = parser.parse_module();
        if let Err(e) = result {
            print_error(&parsed_module, &e);
            parse_errors.push(e);
        }
    };

    parse_file(Path::new("builtins/builtin.k1"));

    if use_std {
        parse_file(Path::new("builtins/core.k1"));
        parse_file(Path::new("builtins/buffer.k1"));
        parse_file(Path::new("builtins/opt.k1"));
        parse_file(Path::new("builtins/list.k1"));
        parse_file(Path::new("builtins/string.k1"));
        parse_file(Path::new("builtins/types.k1"));
        parse_file(Path::new("builtins/string_builder.k1"));
        parse_file(Path::new("builtins/bitwise.k1"));
        parse_file(Path::new("builtins/allocator.k1"));
    }

    for f in dir_entries.iter() {
        parse_file(&f.path());
    }

    if !parse_errors.is_empty() {
        return Err(CompileModuleError {
            parsed_module: Some(Box::new(parsed_module)),
            module: None,
        });
    }

    let type_start = Instant::now();
    let parsing_elapsed = type_start.duration_since(start_parse);
    info!("parsing took {}ms", parsing_elapsed.as_millis());

    let mut typed_module = TypedModule::new(parsed_module);
    if let Err(e) = typed_module.run() {
        if args.dump_module {
            println!("{}", typed_module);
        }
        eprintln!("{}", e);
        return Err(CompileModuleError {
            parsed_module: None,
            module: Some(Box::new(typed_module)),
        });
    };
    let typing_elapsed = type_start.elapsed();
    if args.dump_module {
        println!("{}", typed_module);
    }
    info!(
        "typing took {}ms (\n\t{} expressions,\n\t{} functions,\n\t{} types\n)",
        typing_elapsed.as_millis(),
        typed_module.ast.expressions.count(),
        typed_module.function_iter().count(),
        typed_module.types.type_count()
    );

    Ok(typed_module)
}

pub fn write_executable(debug: bool, out_dir: &str, module_name: &str) -> Result<()> {
    let clang_time = std::time::Instant::now();
    let mut build_cmd = std::process::Command::new("clang");

    // Note: Could we do this a lot more efficiently by just feeding the in-memory LLVM IR to libclang or whatever the library version is called.
    build_cmd.args([
        // "-v",
        if debug { "-g" } else { "" },
        if debug { "-fsanitize=address,undefined" } else { "" },
        if debug { "-O0" } else { "-O3" },
        "-Woverride-module",
        "-mmacosx-version-min=14.4",
        &format!("{}/{}.ll", out_dir, module_name),
        "-o",
        &format!("{}/{}.out", out_dir, module_name),
        // "-L",
        // "k1lib/zig-out/lib",
        "-L",
        "k1lib",
        "-l",
        "k1lib",
    ]);
    log::info!("Build Command: {:?}", build_cmd);
    let build_status = build_cmd.status()?;

    if !build_status.success() {
        eprintln!("Build failed!");
        bail!("build_executable with clang failed");
    }

    let elapsed = clang_time.elapsed();
    info!("codegen phase 'clang' took {}ms", elapsed.as_millis());
    Ok(())
}

pub fn codegen_module<'ctx, 'module>(
    args: &Args,
    ctx: &'ctx Context,
    typed_module: &'module TypedModule,
    out_dir: impl AsRef<str>,
    do_write_executable: bool,
) -> Result<Codegen<'ctx, 'module>> {
    let llvm_optimize = !args.no_llvm_opt;
    let out_dir = out_dir.as_ref();

    let mut codegen = Codegen::create(ctx, typed_module, args.debug, llvm_optimize);
    let module_name = codegen.name().to_string();
    if let Err(e) = codegen.codegen_module() {
        print_error_location(&codegen.module.ast.spans, &codegen.module.ast.sources, e.span);
        eprintln!("Codegen error: {}", e.message);
        anyhow::bail!(e)
    }
    if let Err(e) = codegen.optimize(llvm_optimize) {
        eprintln!("Codegen error: {}", e);
        anyhow::bail!(e)
    };

    if args.write_llvm {
        let llvm_text = codegen.output_llvm_ir_text();
        let mut f = File::create(format!("{}/{}.ll", out_dir, &module_name))
            .expect("Failed to create .ll file");
        f.write_all(llvm_text.as_bytes()).unwrap();
        // println!("{}", codegen.output_llvm_ir_text());
    }

    if do_write_executable {
        write_executable(args.debug, out_dir, &module_name)?;
    }

    Ok(codegen)
}

// Eventually, we want to return output and exit code to the application
pub fn run_compiled_program(out_dir: &str, module_name: &str) {
    let mut run_cmd = std::process::Command::new(format!("{}/{}.out", out_dir, module_name));
    log::debug!("Run Command: {:?}", run_cmd);
    let run_status = run_cmd.status().unwrap();

    match run_status.code() {
        Some(code) => {
            info!("Program exited with code: {}", code);
        }
        None => {
            info!("Program was terminated with signal: {:?}", run_status.signal());
        }
    }
}
