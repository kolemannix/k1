// Copyright (c) 2025 knix
// All rights reserved.

use std::fs;
use std::fs::File;
use std::io::Write;
use std::os::unix::prelude::ExitStatusExt;
use std::path::Path;

use crate::parse::write_source_location;
use crate::parse::{self};
use crate::typer::{LibRefLinkType, MessageLevel, TypedProgram};
use anyhow::{Result, bail};
use inkwell::context::Context;
use log::info;

use crate::codegen_llvm::Codegen;
use crate::codegen_llvm_new::Cg;
use parse::ParsedProgram;

use std::path::PathBuf;

use clap::{Parser, Subcommand};

pub const MAC_SDK_VERSION: &str = "15.0.0";
pub const MAC_SDK_SYSROOT: &str = "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk";

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
    let arch = match std::env::consts::ARCH {
        "x86" => return None,
        "x86_64" => Arch::Intel,
        "arm" => return None,
        "aarch64" => Arch::Arm,
        _ => return None,
    };
    let os = match std::env::consts::OS {
        "linux" => Some(TargetOs::Linux),
        "macos" => Some(TargetOs::MacOs),
        _ => None,
    };
    Target::from(arch, os)
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
    Wasm64,
}

impl Target {
    pub fn from(arch: Arch, os: Option<TargetOs>) -> Option<Self> {
        match (arch, os) {
            (Arch::Intel, Some(TargetOs::Linux)) => Some(Target::LinuxIntel64),
            (Arch::Arm, Some(TargetOs::MacOs)) => Some(Target::MacOsArm64),
            (Arch::Wasm, Some(TargetOs::Wasm)) => Some(Target::Wasm64),
            _ => None,
        }
    }
    pub fn target_os(&self) -> TargetOs {
        match self {
            Target::LinuxIntel64 => TargetOs::Linux,
            Target::MacOsArm64 => TargetOs::MacOs,
            Target::Wasm64 => TargetOs::Wasm,
        }
    }
    pub fn arch(&self) -> Arch {
        match self {
            Target::LinuxIntel64 => Arch::Intel,
            Target::MacOsArm64 => Arch::Arm,
            Target::Wasm64 => Arch::Wasm,
        }
    }
}

pub const LIBS_DIR_NAME: &str = "libs";

pub fn logical_name_to_dylib_filename(
    module_libs_dir: &Path,
    target_os: TargetOs,
    link_type: LibRefLinkType,
    logical_name: &str,
) -> PathBuf {
    match (target_os, link_type) {
        (TargetOs::Linux, LibRefLinkType::Static) => {
            module_libs_dir.join(Path::new(&format!("lib{}", logical_name)).with_extension("a"))
        }
        (TargetOs::Linux, LibRefLinkType::Dynamic) => {
            module_libs_dir.join(Path::new(&format!("lib{}", logical_name)).with_extension("so"))
        }
        (TargetOs::Linux, LibRefLinkType::Default) => PathBuf::from(logical_name),
        (TargetOs::MacOs, LibRefLinkType::Static) => {
            module_libs_dir.join(Path::new(&format!("lib{}", logical_name)).with_extension("a"))
        }
        (TargetOs::MacOs, LibRefLinkType::Dynamic) => {
            module_libs_dir.join(Path::new(&format!("lib{}", logical_name)).with_extension("dylib"))
        }
        (TargetOs::MacOs, LibRefLinkType::Default) => PathBuf::from(logical_name),
        // In Windows we'd skip the 'lib' prefix and add extension dll or lib
        (TargetOs::Wasm, _) => {
            panic!("Dynamic libraries are not supported on the wasm target")
        }
    }
}

#[derive(Debug, Clone, Subcommand)]
pub enum Command {
    #[clap(alias = "c")]
    Check {
        /// File
        file: PathBuf,
    },
    #[clap(alias = "b")]
    Build {
        /// File
        file: PathBuf,
    },
    #[clap(alias = "r")]
    Run {
        /// File
        file: PathBuf,
    },
    #[clap(alias = "t")]
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

    /// Optimize
    #[arg(long, default_value_t = false)]
    pub optimize: bool,

    /// Dump Module
    #[arg(long, default_value_t = false)]
    pub dump_module: bool,

    /// Debug Module
    #[arg(long)]
    pub debug: bool,

    #[arg(long)]
    pub profile: bool,

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
    pub src_path: PathBuf,
    pub is_test_build: bool,
    pub no_std: bool,
    pub target: Target,
    pub debug: bool,
    pub out_dir: PathBuf,
}

/// Type size assertion. The first argument is a type and the second argument is its expected size.
/// Cool trick from rustc.
#[macro_export]
macro_rules! static_assert_size {
    ($ty:ty, $size:expr) => {
        const _: [(); $size] = [(); ::std::mem::size_of::<$ty>()];
    };
}

#[macro_export]
macro_rules! static_assert_niched {
    ($ty:ty) => {
        static_assert_size!(Option<$ty>, ::std::mem::size_of::<$ty>());
    };
}

pub enum CompileProgramError {
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

    let src_filter: &dyn Fn(&Path) -> bool =
        &|p: &Path| p.extension().is_some_and(|ext| ext == "k1");
    let dir_entries = if is_dir {
        let mut ents = fs::read_dir(&src_dir)
            .unwrap()
            .filter_map(|item| item.ok())
            .map(|item| item.path())
            .filter(|item| src_filter(item))
            .collect::<Vec<_>>();
        ents.sort_by_key(|ent| ent.file_name().unwrap().to_os_string());
        ents
    } else {
        vec![src_path.to_owned()]
    };

    (src_dir, dir_entries)
}

fn write_program_dump(p: &TypedProgram) {
    let _ = std::fs::write(format!("{}_module_dump.txt", p.program_name()), format!("{}", p));
}

/// If `args.file` points to a directory,
/// - compile all files in the directory.
/// - program name is the name of the directory.
///
/// If `args.file` points to a file,
/// - compile that file only.
/// - program name is the name of the file.
pub fn compile_program(
    args: &Args,
    out_dir: &Path,
) -> std::result::Result<TypedProgram, CompileProgramError> {
    let profiler_guard = if args.profile {
        Some(
            pprof::ProfilerGuardBuilder::default()
                .frequency(50000)
                .blocklist(&["libc", "libgcc", "pthread", "vdso"])
                .build()
                .unwrap(),
        )
    } else {
        None
    };
    let start_time = std::time::Instant::now();

    let out_dir = out_dir.canonicalize().unwrap();

    let src_path = args
        .file()
        .canonicalize()
        .unwrap_or_else(|_| panic!("Failed to load source path: {:?}", args.file()));

    let use_std = !args.no_std;

    let target = args
        .target
        .or(detect_host_target())
        .unwrap_or_else(|| panic!("Unsupported host platform; provide your target explicitly"));

    let lib_dir_pathbuf =
        std::env::var("K1_LIB_DIR").map(PathBuf::from).unwrap_or(PathBuf::from("k1lib"));

    let corelib_dir = lib_dir_pathbuf.join("core");
    let stdlib_dir = lib_dir_pathbuf.join("std");

    let module_name = if src_path.is_dir() {
        src_path.file_name().unwrap().to_str().unwrap().to_string()
    } else {
        src_path.file_stem().unwrap().to_str().unwrap().to_string()
    };

    let config = CompilerConfig {
        src_path: src_path.clone(),
        is_test_build: args.command.is_test(),
        no_std: args.no_std,
        target,
        debug: args.debug,
        out_dir,
    };

    let mut p = TypedProgram::new(module_name.clone(), config);

    if let Err(e) = p.add_module(&corelib_dir, false) {
        write_program_dump(&p);
        eprintln!("{}", e);
        return Err(CompileProgramError::TyperFailure(Box::new(p)));
    };

    if use_std {
        if let Err(e) = p.add_module(&stdlib_dir, false) {
            write_program_dump(&p);
            eprintln!("{}", e);
            return Err(CompileProgramError::TyperFailure(Box::new(p)));
        }
    }

    if let Err(e) = p.add_module(&src_path, true) {
        write_program_dump(&p);
        eprintln!("{}", e);
        return Err(CompileProgramError::TyperFailure(Box::new(p)));
    };
    let total_elapsed_ms = start_time.elapsed().as_millis();
    let warning_count = p.errors.iter().filter(|e| e.level == MessageLevel::Warn).count();
    if warning_count > 0 {
        eprintln!("Completed with {} warnings", warning_count);
    }
    p.print_timing_info(
        &src_path.to_string_lossy(),
        total_elapsed_ms as u64,
        &mut std::io::stderr(),
    )
    .unwrap();

    if let Some(profiler_guard) = profiler_guard {
        if let Ok(report) = profiler_guard.report().build() {
            let mut options = pprof::flamegraph::Options::default();
            options.min_width = 0.02;
            options.image_width = Some(3200);
            options.text_truncate_direction = pprof::flamegraph::TextTruncateDirection::Left;
            options.frame_height = 20;
            options.font_size = 10;

            let fname = format!("{}.svg", module_name);
            eprintln!("Outputting profile flamegraph to {fname}");
            let file = File::create(fname).unwrap();
            options.reverse_stack_order = false;
            options.direction = pprof::flamegraph::Direction::Inverted;
            report.flamegraph_with_options(file, &mut options).unwrap();

            let fname_rev = format!("{}_reverse.svg", module_name);
            let file_rev = File::create(fname_rev).unwrap();
            options.reverse_stack_order = true;
            options.direction = pprof::flamegraph::Direction::Straight;
            report.flamegraph_with_options(file_rev, &mut options).unwrap();
        }
    }

    if args.dump_module {
        write_program_dump(&p);
    }

    Ok(p)
}

pub fn write_executable(
    k1: &TypedProgram,
    module_name: &Path,
    extra_options: &[String],
    optimize: bool,
) -> Result<()> {
    let target = k1.ast.config.target;
    let debug = k1.ast.config.debug;
    let out_dir = &k1.ast.config.out_dir;
    let clang_time = std::time::Instant::now();

    let llvm_base = PathBuf::from(
        std::env::var("LLVM_SYS_211_PREFIX").expect("could not find llvm at $LLVM_SYS_211_PREFIX"),
    );
    let clang_path = llvm_base.join("bin").join("clang");
    let mut build_cmd = std::process::Command::new(clang_path);
    let llvm_lib_base = llvm_base.join("lib");
    let bc_name = out_dir.join(module_name.with_extension("bc"));
    let out_name = out_dir.join(module_name);

    let llvm_include_path = llvm_base.join("include");
    let macos_version_flag = if target.target_os() == TargetOs::MacOs {
        Some(format!("-mmacosx-version-min={}", MAC_SDK_VERSION))
    } else {
        None
    };

    if debug {
        build_cmd.arg("-g");
        build_cmd.arg("-fsanitize=address,undefined");
        build_cmd.arg("-O0");
    } else {
        if optimize {
            build_cmd.arg("-O3");
        } else {
            build_cmd.arg("-gline-tables-only");
            build_cmd.arg("-fno-omit-frame-pointer");
        }
    };

    build_cmd.arg("-L");
    build_cmd.arg(llvm_lib_base.into_os_string());
    build_cmd.arg("-I");
    build_cmd.arg(llvm_include_path);

    match target.target_os() {
        TargetOs::MacOs => {
            build_cmd.arg(macos_version_flag.as_ref().unwrap());
            build_cmd.arg("--sysroot");
            build_cmd.arg(MAC_SDK_SYSROOT);
        }
        TargetOs::Linux => {}
        TargetOs::Wasm => {}
    }

    // Our actual compiled LLVM bitcode!
    build_cmd.arg(bc_name);

    build_cmd.arg("-o");
    build_cmd.arg(out_name);

    // Linking with libraries.
    // For each module, for each of its libraries, link with it as specified by the link_type
    for module in k1.modules.iter() {
        let module_libs_dir = module.home_dir.join(LIBS_DIR_NAME);
        if !module.manifest.libs.is_empty() {
            build_cmd.arg(format!("-L{}", module_libs_dir.display()));
        }
        for lib in &module.manifest.libs {
            let logical_name_str = k1.get_string(lib.name);
            let filename = logical_name_to_dylib_filename(
                &module_libs_dir,
                target.target_os(),
                lib.link_type,
                logical_name_str,
            );
            match lib.link_type {
                // Link via linker arg, since the name has no extension
                LibRefLinkType::Default => build_cmd.arg(format!("-l{}", filename.display())),
                // 'Link' via direct clang arg, since its an exact filepath
                _ => build_cmd.arg(filename),
            };
        }
    }

    build_cmd.args(extra_options);
    eprintln!("Build Command: {:?}", build_cmd);
    let build_status = build_cmd.status()?;

    if !build_status.success() {
        eprintln!("Build failed!");
        bail!("build_executable with clang failed");
    }

    let elapsed = clang_time.elapsed();
    info!("link executable took {}ms", elapsed.as_millis());
    Ok(())
}

pub fn codegen_module<'ctx, 'module>(
    args: &Args,
    ctx: &'ctx Context,
    typed_module: &'module mut TypedProgram,
    out_dir: &Path,
    do_write_executable: bool,
) -> Result<Codegen<'ctx, 'module>> {
    //let mut codegen2 = Cg::create(ctx, typed_module, args.debug, args.optimize);
    //let mut module_name = codegen2.name().to_string();
    //module_name.push_str("_new");
    //let module_name_path = PathBuf::from(&module_name);
    //codegen2.codegen_program()?;
    //codegen2.optimize_verify(true)?;
    //let llvm_text = codegen2.output_llvm_ir_text();
    //let mut f = File::create(out_dir.join(module_name_path.with_extension("ll")))
    //    .expect("Failed to create .ll file");
    //f.write_all(llvm_text.as_bytes()).unwrap();
    //drop(codegen2);

    let mut codegen = Codegen::create(ctx, typed_module, args.debug, args.optimize);
    // let mut codegen = Cg::create(ctx, typed_module, args.debug, args.optimize);
    let module_name = codegen.name().to_string();
    let module_name_path = PathBuf::from(&module_name);
    if let Err(e) = codegen.codegen_program() {
        write_source_location(
            &mut std::io::stderr(),
            &codegen.k1.ast.spans,
            &codegen.k1.ast.sources,
            e.span,
            MessageLevel::Error,
            6,
            Some(&e.message),
        )
        .unwrap();
        write_program_dump(codegen.k1);
        anyhow::bail!(e)
    }

    // Sometimes its really nice to inspect optimized IR instead of assembly!
    let optimize_ir = false;
    if let Err(e) = codegen.optimize_verify(optimize_ir) {
        eprintln!("Codegen error: {}", e);
        anyhow::bail!(e)
    };

    if args.write_llvm {
        let llvm_text = codegen.output_llvm_ir_text();
        let mut f = File::create(out_dir.join(module_name_path.with_extension("ll")))
            .expect("Failed to create .ll file");
        f.write_all(llvm_text.as_bytes()).unwrap();
    }
    if do_write_executable {
        let path = out_dir.join(module_name_path.with_extension("bc"));
        if !codegen.write_bitcode_to_path(&path) {
            bail!("Error writing bitcode to path: {}", path.display());
        }
    }

    if do_write_executable {
        write_executable(codegen.k1, &module_name_path, &args.clang_options, args.optimize)?;
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
pub fn run_compiled_program(out_dir: &Path, module_name: &str) {
    let mut run_cmd = std::process::Command::new(format!("{}/{}", out_dir.display(), module_name));
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
