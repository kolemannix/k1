use std::fs;
use std::fs::File;
use std::io::Write;
use std::os::unix::prelude::ExitStatusExt;
use std::path::Path;

use crate::parse::{self};
use crate::parse::{write_source_location, NumericWidth};
use crate::typer::{ErrorLevel, TypedProgram};
use anyhow::{bail, Result};
use inkwell::context::Context;
use log::info;

use crate::codegen_llvm::Codegen;
use parse::ParsedProgram;

use std::path::PathBuf;

use clap::{Parser, Subcommand};

pub const MAC_SDK_VERSION: &str = "11.0.0";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetOs {
    Linux,
    MacOs,
    Wasm,
}

impl TargetOs {
    pub fn to_str(&self) -> &'static str {
        match self {
            TargetOs::Linux => "linux",
            TargetOs::MacOs => "macos",
            TargetOs::Wasm => "wasm",
        }
    }
}

pub fn detect_host_target() -> Option<Target> {
    let (arch, word_size) = match std::env::consts::ARCH {
        "x86" => (Arch::Intel, WordSize::W32),
        "x86_64" => (Arch::Intel, WordSize::W64),
        "arm" => (Arch::Arm, WordSize::W32),
        "aarch64" => (Arch::Arm, WordSize::W64),
        _ => return None,
    };
    let os = match std::env::consts::OS {
        "linux" => Some(TargetOs::Linux),
        "macos" => Some(TargetOs::MacOs),
        _ => None,
    };
    Target::from(arch, word_size, os)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WordSize {
    W32,
    W64,
}

impl WordSize {
    pub const fn width(&self) -> NumericWidth {
        match self {
            WordSize::W32 => NumericWidth::B32,
            WordSize::W64 => NumericWidth::B64,
        }
    }

    pub fn from_width(w: NumericWidth) -> WordSize {
        match w {
            NumericWidth::B32 => WordSize::W32,
            NumericWidth::B64 => WordSize::W64,
            w => panic!("Invalid word size: {w:?}"),
        }
    }

    pub const fn bits(&self) -> u32 {
        match self {
            WordSize::W32 => 32,
            WordSize::W64 => 64,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Arch {
    Intel,
    Arm,
    Wasm,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
/// For now, I just do a simple exhaustive enum of the triples I actually support
/// rather than a 'target triple' type of struct where very few values of that type
/// are actually valid
pub enum Target {
    LinuxIntel64,
    MacOsArm64,
    Wasm32,
}

impl Target {
    pub fn from(arch: Arch, word_size: WordSize, os: Option<TargetOs>) -> Option<Self> {
        match (arch, word_size, os) {
            (Arch::Intel, WordSize::W64, Some(TargetOs::Linux)) => Some(Target::LinuxIntel64),
            (Arch::Arm, WordSize::W64, Some(TargetOs::MacOs)) => Some(Target::MacOsArm64),
            (Arch::Wasm, WordSize::W32, Some(TargetOs::Wasm)) => Some(Target::Wasm32),
            _ => None,
        }
    }
    pub fn word_size(&self) -> WordSize {
        match self {
            Target::LinuxIntel64 => WordSize::W64,
            Target::MacOsArm64 => WordSize::W64,
            Target::Wasm32 => WordSize::W32,
        }
    }
    pub fn target_os(&self) -> TargetOs {
        match self {
            Target::LinuxIntel64 => TargetOs::Linux,
            Target::MacOsArm64 => TargetOs::MacOs,
            Target::Wasm32 => TargetOs::Wasm,
        }
    }
    pub fn arch(&self) -> Arch {
        match self {
            Target::LinuxIntel64 => Arch::Intel,
            Target::MacOsArm64 => Arch::Arm,
            Target::Wasm32 => Arch::Wasm,
        }
    }
}

#[derive(Debug, Clone, Subcommand)]
pub enum Command {
    Check {
        /// File
        file: PathBuf,
    },
    Build {
        /// File
        file: PathBuf,
    },
    Run {
        /// File
        file: PathBuf,
    },
    Test {
        /// File
        file: PathBuf,
    },
}

impl Command {
    pub fn file(&self) -> &PathBuf {
        match self {
            Command::Check { file } => file,
            Command::Build { file } => file,
            Command::Run { file } => file,
            Command::Test { file } => file,
        }
    }

    pub fn is_check(&self) -> bool {
        matches!(self, Command::Check { .. })
    }

    pub fn is_build(&self) -> bool {
        matches!(self, Command::Build { .. })
    }

    pub fn is_run(&self) -> bool {
        matches!(self, Command::Run { .. })
    }

    pub fn is_test(&self) -> bool {
        matches!(self, Command::Test { .. })
    }
}

#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// No core
    #[arg(short, long, default_value_t = false)]
    pub no_std: bool,

    /// Output an LLVM IR file at out_dir/{module_name}.ll
    #[arg(long, default_value_t = false)]
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

    /// Log LLVM Instruction Counts
    #[arg(long, default_value_t = false)]
    pub llvm_counts: bool,

    /// Target platform
    pub target: Option<Target>,

    #[arg(long = "clang-option", short = 'c', allow_hyphen_values = true)]
    pub clang_options: Vec<String>,

    #[command(subcommand)]
    pub command: Command,
}

impl Args {
    pub fn file(&self) -> &PathBuf {
        self.command.file()
    }
}

#[derive(Debug, Clone)]
pub struct CompilerConfig {
    pub is_test_build: bool,
    pub no_std: bool,
    pub target: Target,
    pub debug: bool,
}

/// Type size assertion. The first argument is a type and the second argument is its expected size.
/// Cool trick from rustc.
#[macro_export]
macro_rules! static_assert_size {
    ($ty:ty, $size:expr) => {
        const _: [(); $size] = [(); ::std::mem::size_of::<$ty>()];
    };
}

pub enum CompileModuleError {
    ParseFailure(Box<ParsedProgram>),
    TyperFailure(Box<TypedProgram>),
}

/// Requires a canonicalized src_path
/// Returned pair is (parent_dir, source_files)
pub fn discover_source_files(src_path: &Path) -> (PathBuf, Vec<PathBuf>) {
    let is_dir = src_path.is_dir();
    let src_dir = if is_dir {
        let src_dir = src_path;
        src_dir.to_path_buf()
    } else {
        let src_dir = src_path.parent().unwrap().to_path_buf();
        src_dir
    };

    let src_filter: &dyn Fn(&Path) -> bool = if !is_dir {
        &|p: &Path| *p == *src_path
    } else {
        &|p: &Path| p.extension().is_some_and(|ext| ext == "k1")
    };

    let dir_entries = {
        let mut ents = fs::read_dir(&src_dir)
            .unwrap()
            .filter_map(|item| item.ok())
            .map(|item| item.path())
            .filter(|item| src_filter(item))
            .collect::<Vec<_>>();
        ents.sort_by_key(|ent| ent.file_name().unwrap().to_os_string());
        ents
    };
    (src_dir, dir_entries)
}

/// If `args.file` points to a directory,
/// - compile all files in the directory.
/// - module name is the name of the directory.
///
/// If `args.file` points to a file,
/// - compile that file only.
/// - module name is the name of the file.
pub fn compile_module(args: &Args) -> std::result::Result<TypedProgram, CompileModuleError> {
    let src_path = &args
        .file()
        .canonicalize()
        .unwrap_or_else(|_| panic!("Failed to load source path: {:?}", args.file()));

    let use_std = !args.no_std;

    let target = args
        .target
        .or(detect_host_target())
        .unwrap_or_else(|| panic!("Unsupported host platform; provide your target explicitly"));
    let config = CompilerConfig {
        is_test_build: args.command.is_test(),
        no_std: args.no_std,
        target,
        debug: args.debug,
    };

    let module_name = if src_path.is_dir() {
        src_path.file_name().unwrap().to_str().unwrap().to_string()
    } else {
        src_path.file_stem().unwrap().to_str().unwrap().to_string()
    };

    let mut typed_program = TypedProgram::new(module_name, config);

    let lib_dir_string = std::env::var("K1_LIB_DIR").unwrap_or("k1lib".to_string());
    let lib_dir = Path::new(&lib_dir_string);
    let corelib_dir = lib_dir.join("core");
    let stdlib_dir = lib_dir.join("std");

    if let Err(e) = typed_program.add_module(&corelib_dir) {
        if args.dump_module {
            eprintln!("{}", typed_program);
        }
        eprintln!("{}", e);
        return Err(CompileModuleError::TyperFailure(Box::new(typed_program)));
    };

    if use_std {
        if let Err(e) = typed_program.add_module(&stdlib_dir) {
            if args.dump_module {
                eprintln!("{}", typed_program);
            }
            eprintln!("{}", e);
            return Err(CompileModuleError::TyperFailure(Box::new(typed_program)));
        }
    }

    if let Err(e) = typed_program.add_module(src_path) {
        if args.dump_module {
            eprintln!("{}", typed_program);
        }
        eprintln!("{}", e);
        return Err(CompileModuleError::TyperFailure(Box::new(typed_program)));
    };

    if args.dump_module {
        println!("{}", typed_program);
    }
    Ok(typed_program)
}

pub fn write_executable(
    debug: bool,
    k1_lib_dir: &Path,
    out_dir: &Path,
    module_name: &Path,
    extra_options: &[String],
) -> Result<()> {
    let clang_time = std::time::Instant::now();

    //opt/homebrew/opt/llvm@18/lib/libunwind.dylib
    let llvm_base = PathBuf::from(
        std::env::var("LLVM_SYS_180_PREFIX").expect("could not find llvm at $LLVM_SYS_180_PREFIX"),
    );
    let clang_path = llvm_base.join("bin").join("clang");
    let mut build_cmd = std::process::Command::new(clang_path);
    let llvm_lib_base = llvm_base.join("lib");
    let ll_name = out_dir.join(module_name.with_extension("ll"));
    let ll_file = ll_name.to_str().unwrap();
    let out_name = out_dir.join(module_name);
    let out_file = out_name.to_str().unwrap();
    let unwind_c_path = k1_lib_dir.join("k1_unwind.c");
    // Note: Could we do this a lot more efficiently by just feeding the in-memory LLVM IR to libclang or whatever the library version is called.
    build_cmd.args([
        // "-v",
        if debug { "-g" } else { "" },
        if debug { "-fsanitize=address,undefined" } else { "" },
        if debug { "-O0" } else { "-O3" },
        "-L",
        llvm_lib_base.to_str().unwrap(),
        "-I",
        llvm_base.join("include").to_str().unwrap(),
        &format!("-mmacosx-version-min={}", MAC_SDK_VERSION),
        "-lunwind",
        unwind_c_path.to_str().unwrap(),
        ll_file,
        "-o",
        out_file,
    ]);
    build_cmd.args(extra_options);
    log::info!("Build Command: {:?}", build_cmd);
    let build_status = build_cmd.status()?;

    if !build_status.success() {
        eprintln!("Build failed!");
        bail!("build_executable with clang failed");
    }

    let elapsed = clang_time.elapsed();
    info!("codegen phase 'link optimized executable' took {}ms", elapsed.as_millis());
    Ok(())
}

pub fn codegen_module<'ctx, 'module>(
    args: &Args,
    ctx: &'ctx Context,
    typed_module: &'module TypedProgram,
    out_dir: impl AsRef<str>,
    do_write_executable: bool,
) -> Result<Codegen<'ctx, 'module>> {
    let llvm_optimize = !args.no_llvm_opt;
    let out_dir = PathBuf::from(out_dir.as_ref());

    let mut codegen = Codegen::create(ctx, typed_module, args.debug, llvm_optimize);
    let module_name = codegen.name().to_string();
    let module_name_path = PathBuf::from(&module_name);
    if let Err(e) = codegen.codegen_module() {
        write_source_location(
            &mut std::io::stderr(),
            &codegen.k1.ast.spans,
            &codegen.k1.ast.sources,
            e.span,
            ErrorLevel::Error,
        )
        .unwrap();
        eprintln!("Codegen error: {}", e.message);
        anyhow::bail!(e)
    }
    if let Err(e) = codegen.optimize_verify(llvm_optimize) {
        eprintln!("Codegen error: {}", e);
        anyhow::bail!(e)
    };

    if args.write_llvm || do_write_executable {
        let llvm_text = codegen.output_llvm_ir_text();
        let mut f = File::create(out_dir.join(module_name_path.with_extension("ll")))
            .expect("Failed to create .ll file");
        f.write_all(llvm_text.as_bytes()).unwrap();
    }

    let k1_lib_dir_string = std::env::var("K1_LIB_DIR").unwrap_or("k1lib".to_string());
    let k1_lib_dir = PathBuf::from(k1_lib_dir_string);

    if do_write_executable {
        write_executable(
            args.debug,
            &k1_lib_dir,
            &out_dir,
            &module_name_path,
            &args.clang_options,
        )?;
    }

    if args.llvm_counts {
        let mut instruction_counts = codegen
            .llvm_functions
            .values()
            .map(|llvm_fn| {
                (llvm_fn.function_value.get_name().to_string_lossy(), llvm_fn.instruction_count)
            })
            .collect::<Vec<_>>();
        instruction_counts.sort_by_key(|f| f.1);
        for (name, count) in &instruction_counts {
            eprintln!("{name}: {count}")
        }
    }

    Ok(codegen)
}

// Eventually, we want to return output and exit code to the application
pub fn run_compiled_program(out_dir: &str, module_name: &str) {
    let mut run_cmd = std::process::Command::new(format!("{}/{}", out_dir, module_name));
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
