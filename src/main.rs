use std::ffi::CString;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::os::unix::prelude::ExitStatusExt;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};
use std::thread::JoinHandle;

use anyhow::Result;
use clap::Parser;
use inkwell::context::Context;
use typer::TypedModule;

use crate::codegen_llvm::Codegen;
use crate::parse::{lex_text, ParsedModule, Source};

mod codegen_llvm;
mod gui;
mod lex;
mod parse;
#[cfg(test)]
mod test_suite;
mod typer;

#[derive(Parser, Debug, Clone)]
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

    /// GUI
    #[arg(long, default_value_t = false)]
    gui: bool,

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
pub fn compile_module(args: &Args) -> Result<TypedModule> {
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

    let use_prelude = !args.no_prelude;

    let mut parsed_module = ParsedModule::make(module_name.to_string());

    let dir_entries = {
        let mut ents = fs::read_dir(src_dir)?
            .filter_map(|item| item.ok())
            .filter(|item| src_filter.as_ref().map_or(true, |filter| filter(&item.path())))
            .collect::<Vec<_>>();
        ents.sort_by_key(|ent1| ent1.file_name());
        ents
    };
    let mut parse_errors = Vec::new();

    let mut parse_file = |path: &Path, file_id: u32| {
        let content = fs::read_to_string(path)?;
        let name = path.file_name().unwrap();
        eprintln!("Parsing {}", name.to_string_lossy());
        let source = Source::make(
            file_id,
            path.canonicalize().unwrap().parent().unwrap().to_str().unwrap().to_string(),
            name.to_str().unwrap().to_string(),
            content,
        );

        let token_vec = lex_text(&mut parsed_module.spans, &source.content, source.file_id)?;
        let mut parser = parse::Parser::make(&token_vec, source, &mut parsed_module);

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

    let mut typed_module = typer::TypedModule::new(parsed_module);
    if let Err(e) = typed_module.run() {
        if args.dump_module {
            println!("{}", typed_module);
        }
        return Err(e);
    };

    Ok(typed_module)
}

fn codegen_module<'ctx, 'module>(
    args: &Args,
    ctx: &'ctx Context,
    typed_module: &'module TypedModule,
    out_dir: impl AsRef<str>,
) -> Result<Codegen<'ctx, 'module>> {
    let llvm_optimize = !args.no_llvm_opt;
    let out_dir = out_dir.as_ref();
    let module_name = typed_module.name().to_string();

    let mut codegen = Codegen::create(ctx, &typed_module, args.debug, llvm_optimize);
    if let Err(e) = codegen.codegen_module() {
        parse::print_error_location(&codegen.module.ast.spans, &codegen.module.ast.sources, e.span);
        eprintln!("Codegen error: {}", e.message);
    }
    codegen.optimize(llvm_optimize)?;

    // TODO: We could do this a lot more efficiently by just feeding the in-memory LLVM IR to libclang or whatever the library version is called.

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

    static_assert_size!(parse::BlockStmt, 40); // Get down below 100 // We did it!
    static_assert_size!(parse::ParsedExpression, 80); // Get back down ideally below 50
    static_assert_size!(typer::TypedExpr, 48);
    static_assert_size!(typer::TypedStmt, 16);
    println!("bfl Compiler v0.1.0");

    let out_dir = "bfl-out";

    // If gui mode:
    // - Create a new thread to compile the module
    // - Run gui loop from this thread
    // - On frame or channel push, get module snapshot and render it
    // - Put module inside a RwLock, just try to read it from the gui thread

    let shared_module: Arc<RwLock<Option<TypedModule>>> = Arc::new(RwLock::new(None));

    // OH instead of a RwLock, we could just use a channel to send the module to the gui thread
    // Or we could just spawn a thread whenever we need to compile a module, and then send the module back to the main thread
    let shared_module_clone = shared_module.clone();
    let args_clone = args.clone();
    let compile_thread: JoinHandle<()> = std::thread::spawn(move || {
        let Ok(module) = compile_module(&args_clone) else {
            return;
        };
        let llvm_ctx = Context::create();
        println!("2");
        shared_module_clone.write().unwrap().replace(module);
        println!("3");

        let module_read = shared_module_clone.read().unwrap();
        println!("4");
        let module = module_read.as_ref().unwrap();
        println!("5");
        let Ok(_codegen) = codegen_module(&args_clone, &llvm_ctx, module, out_dir) else { return };

        std::thread::sleep(std::time::Duration::from_secs(100));
    });

    if args.run {
        println!("waiting on compile thread");
        loop {
            let module = shared_module.try_read();
            if module.is_ok() && module.unwrap().is_some() {
                break;
            } else {
                std::thread::sleep(std::time::Duration::from_secs(1));
            }
        }
        let module = shared_module.read().unwrap();
        let module_name = module.as_ref().unwrap().name();
        println!("done waiting on compile thread");
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

    if args.gui {
        let mut gui = gui::Gui::init();
        use raylib::prelude::*;
        let thread = gui.rl_thread.clone();
        let font: Font = gui
            .rl
            .load_font(&thread, "/Users/knix/Library/Fonts/JetBrainsMono-Regular.ttf")
            .expect("font load");

        let mut scroll_index = 0;
        let mut active = 0;

        while !gui.rl.window_should_close() {
            let mut d = gui.rl.begin_drawing(&thread);
            // d.gui_set_font(&font);

            let lock = shared_module.try_read();
            let Ok(module_option_guard) = lock else {
                continue;
            };
            let module = module_option_guard.as_ref().expect("Expected module by now?");

            let name = module.name();

            d.clear_background(raylib::color::Color::WHITE);
            d.draw_text(name, 12, 12, 20, raylib::color::Color::BLACK);
            d.draw_fps(500, 10);
            let index = d.gui_list_view(
                Rectangle { x: 100.0, y: 100.0, width: 500.0, height: 500.0 },
                Some(rstr!("hello;goodbye;wat")),
                &mut scroll_index,
                &mut active,
            );
            let b = d.gui_label_button(
                Rectangle { x: 30.0, y: 30.0, width: 200.0, height: 100.0 },
                Some(rstr!("Run;Compile")),
            );
            if b {
                println!("Run again");
            }
            // d.gui_list_view_ex(bounds, text, focus, scroll_index, active)

            for (idx, (id, f)) in module.function_iter().enumerate() {
                let name = CString::new(module.ast.identifiers.get_name(f.name)).unwrap();
                d.gui_label(Rectangle::new(0.0, 0.0, 100.0, 50.0), Some(name.as_c_str()));
                // d.draw_text_ex(
                //     &font,
                //     module.ast.identifiers.get_name(f.name),
                //     Vector2 { x: 20.0, y: 20.0 + (20.0 * idx as f32) },
                //     12.0,
                //     1.0,
                //     Color::GRAY,
                // );
            }
        }
        Ok(())
    } else {
        compile_thread.join().unwrap();
        Ok(())
    }
}
