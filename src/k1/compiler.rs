// Copyright (c) 2026 knix
// All rights reserved.

use std::fs;
use std::fs::File;
use std::io::{IsTerminal, Write};
use std::os::unix::prelude::ExitStatusExt;
use std::path::Path;

use crate::kpath;
use crate::parse::{StringId, write_source_location};
use crate::typer::{LibRefLinkType, MessageLevel, TypedProgram};
use anyhow::{Result, bail};
use inkwell::context::Context;
use log::error;

use crate::codegen_llvm::Cg;

use std::path::PathBuf;

use clap::{Parser, Subcommand};

pub const MAC_SDK_VERSION: &str = "15.0.0";
pub const MAC_SDK_SYSROOT: &str = "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum TargetOs {
    Linux = 0,
    MacOs = 1,
    Wasm = 2,
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

pub fn detect_simd_bytes(target: Target) -> u32 {
    if detect_host_target() != Some(target) {
        // Cross builds assume the target's baseline
        return 16;
    }
    match target.arch() {
        Arch::Intel => {
            #[cfg(target_arch = "x86_64")]
            {
                if std::arch::is_x86_feature_detected!("avx512f") {
                    64
                } else if std::arch::is_x86_feature_detected!("avx2") {
                    32
                } else {
                    16
                }
            }
            #[cfg(not(target_arch = "x86_64"))]
            {
                16
            }
        }
        // NEON is baseline; SVE detection can widen this later
        Arch::Arm => 16,
        Arch::Wasm => 16,
    }
}

pub const LIBS_DIR_NAME: &str = "libs";

pub fn logical_name_to_lib_filename(
    module_libs_dir: &str,
    target_os: TargetOs,
    link_type: LibRefLinkType,
    logical_name: &str,
) -> String {
    match (target_os, link_type) {
        (TargetOs::Linux, LibRefLinkType::Static) => {
            kpath::join(module_libs_dir, &format!("lib{logical_name}.a"))
        }
        (TargetOs::Linux, LibRefLinkType::Dynamic) => {
            kpath::join(module_libs_dir, &format!("lib{logical_name}.so"))
        }
        (TargetOs::Linux, LibRefLinkType::Default) => logical_name.to_string(),
        (TargetOs::MacOs, LibRefLinkType::Static) => {
            kpath::join(module_libs_dir, &format!("lib{logical_name}.a"))
        }
        (TargetOs::MacOs, LibRefLinkType::Dynamic) => {
            kpath::join(module_libs_dir, &format!("lib{logical_name}.dylib"))
        }
        (TargetOs::MacOs, LibRefLinkType::Default) => logical_name.to_string(),
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
    #[clap()]
    Repl {
        /// File
        file: PathBuf,
    },
    #[clap()]
    Server {
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
            Command::Repl { file } => file,
            Command::Server { file } => file,
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
    /// core only
    #[arg(short, long, default_value_t = false)]
    pub no_std: bool,

    /// Output an LLVM IR file at out_dir/{program_name}.ll
    #[arg(long, default_value_t = false)]
    pub emit_llvm: bool,

    /// Optimize
    #[arg(long, default_value_t = false)]
    pub optimize: bool,

    /// Write out a text representation of the typed program
    #[arg(long, default_value_t = false)]
    pub dump_module: bool,

    /// Write out every string in the identifier intern pool, with stats
    #[arg(long, default_value_t = false)]
    pub dump_idents: bool,

    /// Generate debug info
    #[arg(long)]
    pub debug: bool,

    /// Link AddressSanitizer and UndefinedBehaviorSanitizer
    #[arg(long)]
    pub sanitize: bool,

    /// Compile and link through a Fil-C toolchain (memory-safe C runtime).
    /// Requires target linux-intel64 and the K1_FILC env var pointing at a
    /// Fil-C installation
    #[arg(long, default_value_t = false)]
    pub filc: bool,

    #[arg(long)]
    pub profile: bool,

    /// Chatty mode, timing summaries, other info; for compiler developers
    #[arg(short, long, default_value_t = false, action = clap::ArgAction::Set)]
    pub chatty: bool,

    /// Toggles whether k1 optimizes its own ir during emission; for compiler developers
    #[arg(short, long, default_value_t = true, action = clap::ArgAction::Set)]
    pub optimize_ir: bool,

    /// Target platform
    #[arg(long)]
    pub target: Option<Target>,

    #[command(subcommand)]
    pub command: Command,
}

impl Args {
    pub fn file(&self) -> &PathBuf {
        self.command.file()
    }
}

/// All paths are canonical UTF-8 strings; see kpath
#[derive(Debug, Clone)]
pub struct CompilerConfig {
    pub src_path: String,
    pub home_dir: String,
    pub k1_home: String,
    pub is_test_build: bool,
    pub no_std: bool,
    pub target: Target,
    /// See detect_simd_bytes
    pub simd_bytes: u32,
    pub debug: bool,
    pub sanitize: bool,
    pub filc: bool,
    pub out_dir: String,
    pub out_dir_generated: String,
    pub optimize: bool,
    pub chatty: bool,
    pub optimize_ir: bool,
    pub lsp: LspCompileOptions,
}

#[derive(Debug, Clone, Default)]
pub struct LspCompileOptions {
    /// canonicalized path -> content to compile instead of the file on disk
    pub source_overrides: fxhash::FxHashMap<String, String>,
    /// Arms completion-marker recording and parse-error tolerance; see
    /// TypedProgram::completion
    pub completion: bool,
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
    TyperFailure(Box<TypedProgram>),
}

struct CwdGuard {
    prev: PathBuf,
}

impl CwdGuard {
    fn enter(dir: &str) -> CwdGuard {
        let prev = std::env::current_dir().unwrap();
        std::env::set_current_dir(Path::new(dir))
            .unwrap_or_else(|e| panic!("Failed to set cwd to {dir}: {e}"));
        CwdGuard { prev }
    }
}

impl Drop for CwdGuard {
    fn drop(&mut self) {
        let _ = std::env::set_current_dir(&self.prev);
    }
}

/// Requires a canonicalized src_path
pub fn module_home_from_src_path(src_path: &str) -> (bool, String) {
    let is_dir = Path::new(src_path).is_dir();
    if is_dir {
        (true, src_path.to_string())
    } else {
        (false, kpath::parent(src_path).to_string())
    }
}

pub struct SourceFile {
    /// canonical absolute
    pub path: String,
    pub content: String,
    pub content_hash: u64,
}

fn read_source_file(path: String, override_content: Option<String>) -> Result<SourceFile, String> {
    let content = match override_content {
        Some(content) => content,
        None => fs::read_to_string(Path::new(&path))
            .map_err(|e| format!("Failed to read source file {path}: {e}"))?,
    };
    let content_hash = content_hash64(content.as_bytes());
    Ok(SourceFile { path, content, content_hash })
}

pub struct ModuleRootHandle {
    /// canonicalized module path (dir or single file)
    pub src_path: String,
    pub module_dir: String,
    pub root_path: String,
    is_dir: bool,
    reader: std::thread::JoinHandle<Result<SourceFile, String>>,
}

impl ModuleRootHandle {
    /// core_module=true must be passed for (exactly) the corelib module, whose root
    /// file is builtin.k1
    pub fn spawn(
        src_path: &str,
        core_module: bool,
        source_overrides: &fxhash::FxHashMap<String, String>,
    ) -> anyhow::Result<ModuleRootHandle> {
        let src_path = kpath::canonicalize(src_path)
            .map_err(|e| anyhow::anyhow!("Error loading module '{src_path}': {e}"))?;
        let (is_dir, module_dir) = module_home_from_src_path(&src_path);
        let root_path = if !is_dir {
            src_path.clone()
        } else if core_module {
            let builtin = kpath::join(&module_dir, "builtin.k1");
            if !Path::new(&builtin).is_file() {
                bail!("corelib module must contain builtin.k1");
            }
            builtin
        } else {
            let module_name = kpath::file_name(&src_path);
            let module_root = kpath::join(&module_dir, "module.k1");
            let named_root = kpath::join(&module_dir, &format!("{module_name}.k1"));
            if Path::new(&module_root).is_file() {
                module_root
            } else if Path::new(&named_root).is_file() {
                named_root
            } else {
                bail!(
                    "Directory module '{module_name}' has no root file: create module.k1 or {module_name}.k1"
                );
            }
        };
        let override_content = source_overrides.get(&root_path).cloned();
        let read_path = root_path.clone();
        let reader = std::thread::spawn(move || read_source_file(read_path, override_content));
        Ok(ModuleRootHandle { src_path, module_dir, root_path, is_dir, reader })
    }

    pub fn join_root(self) -> anyhow::Result<(SourceFile, ModuleRemainingSources)> {
        let root = self
            .reader
            .join()
            .expect("module root reader thread panicked")
            .map_err(|e| anyhow::anyhow!(e))?;
        let remaining = ModuleRemainingSources {
            module_dir: self.module_dir,
            root_path: self.root_path,
            is_dir: self.is_dir,
        };
        Ok((root, remaining))
    }
}

/// The module's non-root sources; the glob is deferred to spawn_read, called only
/// once the module is loaded (manifest evaluated, eventually setup run), so
/// setup-generated sources are discovered by the same single glob as committed ones
pub struct ModuleRemainingSources {
    module_dir: String,
    root_path: String,
    is_dir: bool,
}

impl ModuleRemainingSources {
    pub fn spawn_read(
        self,
        source_overrides: &fxhash::FxHashMap<String, String>,
    ) -> ModuleRemainingSourcesHandle {
        let overrides = source_overrides.clone();
        let reader = std::thread::spawn(move || {
            if !self.is_dir {
                return Ok(vec![]);
            }
            let mut files = fs::read_dir(Path::new(&self.module_dir))
                .map_err(|e| format!("Failed to list module dir {}: {e}", self.module_dir))?
                .filter_map(|item| item.ok())
                .filter(|item| item.path().extension().is_some_and(|ext| ext == "k1"))
                .map(|item| {
                    item.path().into_os_string().into_string().map_err(|s| {
                        format!("Source file name is not valid UTF-8: {}", s.to_string_lossy())
                    })
                })
                .collect::<Result<Vec<String>, String>>()?;
            files.retain(|p| *p != self.root_path);
            files.sort();
            files
                .into_iter()
                .map(|path| {
                    let override_content = overrides.get(&path).cloned();
                    read_source_file(path, override_content)
                })
                .collect::<Result<Vec<_>, String>>()
        });
        ModuleRemainingSourcesHandle { reader }
    }
}

pub struct ModuleRemainingSourcesHandle {
    reader: std::thread::JoinHandle<Result<Vec<SourceFile>, String>>,
}

impl ModuleRemainingSourcesHandle {
    /// Err is the first failure
    pub fn join(self) -> anyhow::Result<Vec<SourceFile>> {
        self.reader
            .join()
            .expect("module sources reader thread panicked")
            .map_err(|e| anyhow::anyhow!(e))
    }
}

fn content_hash64(bytes: &[u8]) -> u64 {
    use std::hash::Hasher;
    let mut h = fxhash::FxHasher64::default();
    h.write(bytes);
    h.finish()
}

fn write_program_dump(p: &TypedProgram) {
    let _ = std::fs::write(format!("{}_module_dump.txt", p.program_name()), format!("{}", p));
}

fn write_idents_dump(p: &TypedProgram) {
    use std::fmt::Write;
    let idents = &p.ast.idents;
    let count = idents.len();
    let content_bytes = idents.content_bytes();
    let mut out = String::with_capacity(content_bytes + count * 16);
    let avg = if count == 0 { 0.0 } else { content_bytes as f64 / count as f64 };
    writeln!(
        out,
        "; ident pool: {} strings, {} content bytes, avg len {:.1}",
        count, content_bytes, avg
    )
    .unwrap();

    let mut by_len: Vec<(StringId, &str)> = idents.iter().collect();
    by_len.sort_by_key(|(_, s)| std::cmp::Reverse(s.len()));
    writeln!(out, "; 20 longest:").unwrap();
    for (id, s) in by_len.iter().take(20) {
        let mut end = s.len().min(120);
        while !s.is_char_boundary(end) {
            end -= 1;
        }
        writeln!(out, ";   [{}] len={} {:?}", id, s.len(), &s[..end]).unwrap();
    }

    for (id, s) in idents.iter() {
        writeln!(out, "[{}] {:?}", id, s).unwrap();
    }
    let path = format!("{}_idents_dump.txt", p.program_name());
    eprintln!("Wrote ident pool dump to {path}");
    let _ = std::fs::write(path, out);
}

/// If `args.file` points to a directory,
/// - compile all files in the directory.
/// - program name is the name of the directory.
///
/// If `args.file` points to a file,
/// - compile that file only.
/// - program name is the name of the file.
pub fn compile_program(args: &Args) -> std::result::Result<TypedProgram, CompileProgramError> {
    compile_program_ext(args, LspCompileOptions::default())
}

pub fn compile_program_ext(
    args: &Args,
    lsp: LspCompileOptions,
) -> std::result::Result<TypedProgram, CompileProgramError> {
    #[cfg(feature = "profile")]
    let profiler_guard = if args.profile {
        Some(
            pprof::ProfilerGuardBuilder::default()
                .frequency(9999)
                .blocklist(&["libc", "libgcc", "pthread", "vdso"])
                .build()
                .unwrap(),
        )
    } else {
        None
    };
    let start_time = std::time::Instant::now();

    let src_path = kpath::canonicalize(args.file())
        .unwrap_or_else(|e| panic!("Failed to load source path: {e}"));

    let (is_dir, home_dir) = module_home_from_src_path(&src_path);
    let out_dir = kpath::join(&home_dir, ".k1-out");
    let out_dir_generated = kpath::join(&out_dir, "generated");
    std::fs::create_dir_all(Path::new(&out_dir_generated)).unwrap();

    let use_std = !args.no_std;

    let target = args
        .target
        .or(detect_host_target())
        .unwrap_or_else(|| panic!("Unsupported host platform; provide your target explicitly"));

    if args.filc {
        assert!(
            target == Target::LinuxIntel64,
            "--filc requires target linux-intel64; Fil-C only supports Linux/x86_64"
        );
        assert!(!args.sanitize, "--filc and --sanitize are mutually exclusive");
    }

    // Find the installation. env var overrides, otherwise release mode says co-located with the
    // binary. dev mode says cwd
    let k1_home_raw = std::env::var("K1_HOME").map(PathBuf::from).unwrap_or_else(|_| {
        let current_exe = std::env::current_exe().unwrap();
        let exe_parent = current_exe.parent().unwrap();
        if exe_parent.ends_with("debug") {
            // its a cargo run
            std::env::current_dir().unwrap()
        } else {
            // its in k1/bin, most likely
            exe_parent.parent().unwrap().to_path_buf()
        }
    });
    let k1_home = kpath::canonicalize(&k1_home_raw)
        .unwrap_or_else(|e| panic!("K1 home {} is not usable: {e}", k1_home_raw.display()));
    if args.chatty {
        eprintln!("using k1 home: {k1_home}");
    }
    let modules_dir = kpath::join(&k1_home, "modules");

    let corelib_dir = kpath::join(&modules_dir, "core");
    let stdlib_dir = kpath::join(&modules_dir, "std");

    // All planned paths are absolute, so spawning before the CwdGuard chdir is safe;
    // reads overlap TypedProgram::new's ident/scope init and core's typecheck.
    let core_plan = ModuleRootHandle::spawn(&corelib_dir, true, &lsp.source_overrides);
    let std_plan = use_std.then(|| ModuleRootHandle::spawn(&stdlib_dir, false, &lsp.source_overrides));
    let main_plan = ModuleRootHandle::spawn(&src_path, false, &lsp.source_overrides);

    let module_name = if is_dir {
        kpath::file_name(&src_path).to_string()
    } else {
        kpath::file_stem(&src_path).to_string()
    };

    let config = CompilerConfig {
        src_path: src_path.clone(),
        home_dir,
        k1_home,
        is_test_build: args.command.is_test(),
        no_std: args.no_std,
        target,
        simd_bytes: detect_simd_bytes(target),
        debug: args.debug,
        sanitize: args.sanitize,
        filc: args.filc,
        out_dir,
        out_dir_generated,
        optimize: args.optimize,
        chatty: args.chatty,
        optimize_ir: args.optimize_ir,
        lsp,
    };

    let _cwd = CwdGuard::enter(&config.home_dir);

    let mut k1 = TypedProgram::new(module_name.clone(), config);

    let add_result = (|| {
        k1.add_module(core_plan?, false)?;
        if let Some(std_plan) = std_plan {
            k1.add_module(std_plan?, false)?;
        }
        k1.add_module(main_plan?, true)
    })();
    k1.write_emitted_sources();
    if let Err(e) = add_result {
        if args.dump_module {
            write_program_dump(&k1);
        }
        eprintln!("{}", e);
        return Err(CompileProgramError::TyperFailure(Box::new(k1)));
    };
    let total_elapsed_ns = start_time.elapsed().as_nanos();
    let warning_count =
        k1.messages.borrow().iter().filter(|e| e.level == MessageLevel::Warn).count();
    if warning_count > 0 {
        eprintln!("Completed with {} warnings", warning_count);
    }
    if args.chatty {
        k1.print_timing_info(&src_path, total_elapsed_ns as u64, &mut std::io::stderr()).unwrap();
    }

    #[cfg(feature = "profile")]
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
        write_program_dump(&k1);
    }
    if args.dump_idents {
        write_idents_dump(&k1);
    }

    Ok(k1)
}

pub fn write_executable(
    k1: &TypedProgram,
    module_name: &str,
    extra_options: &[String],
) -> Result<()> {
    let target = k1.config.target;
    let debug = k1.config.debug;
    let out_dir = &k1.config.out_dir;
    let optimize = k1.config.optimize;
    let sanitize = k1.config.sanitize;
    let filc = k1.config.filc;
    let clang_time = std::time::Instant::now();

    let mut build_cmd = if filc {
        let filc_home = std::env::var("K1_FILC").map(PathBuf::from).map_err(|_| {
            anyhow::anyhow!("--filc requires K1_FILC to point at a Fil-C installation")
        })?;
        std::process::Command::new(filc_home.join("build/bin/clang"))
    } else {
        std::process::Command::new("cc")
    };
    // Fil-C consumes llvm IR rather than the object file
    let object_name = if filc {
        kpath::join(out_dir, &format!("{module_name}.ll"))
    } else {
        kpath::join(out_dir, &format!("{module_name}.o"))
    };
    let out_name = kpath::join(out_dir, module_name);

    let _macos_version_flag = if target.target_os() == TargetOs::MacOs {
        Some(format!("-mmacosx-version-min={}", MAC_SDK_VERSION))
    } else {
        None
    };

    if filc {
        build_cmd.arg("-O2");
    } else if optimize {
        build_cmd.arg("-O3");
    } else if debug {
        build_cmd.arg("-O0");
    }
    if debug {
        build_cmd.arg("-g");
    } else {
        // For stack traces
        if target.target_os() == TargetOs::Linux {
            build_cmd.arg("-g");
        } else {
            build_cmd.arg("-gline-tables-only");
        }
        build_cmd.arg("-fno-omit-frame-pointer");
    };
    if sanitize {
        build_cmd.arg("-fsanitize=address,undefined");
    }

    match target.target_os() {
        TargetOs::MacOs => {
            //build_cmd.arg(macos_version_flag.as_ref().unwrap());
            //build_cmd.arg("--sysroot");
            //build_cmd.arg(MAC_SDK_SYSROOT);
        }
        TargetOs::Linux => {}
        TargetOs::Wasm => {}
    }

    // Our actual compiled k1 code!
    build_cmd.arg(object_name);

    // Linking with libraries.
    // For each module, for each of its libraries, link with it as specified by the link_type
    for module in k1.modules.iter() {
        let module_libs_dir = kpath::join(k1.ident_str(module.home_dir), LIBS_DIR_NAME);
        if !module.manifest.libs.is_empty() {
            build_cmd.arg(format!("-L{module_libs_dir}"));
        }
        for link_arg_string_id in &module.manifest.link_args {
            build_cmd.arg(k1.get_string(*link_arg_string_id));
        }
        for lib in &module.manifest.libs {
            let logical_name_str = k1.get_string(lib.name);
            let logical_name =
                if filc { format!("{logical_name_str}-filc") } else { logical_name_str.into() };
            let filename = logical_name_to_lib_filename(
                &module_libs_dir,
                target.target_os(),
                lib.link_type,
                &logical_name,
            );
            match lib.link_type {
                // Link via linker arg, since the name has no extension
                LibRefLinkType::Default => build_cmd.arg(format!("-l{filename}")),
                // 'Link' via direct clang arg, since its an exact filepath
                _ => build_cmd.arg(filename),
            };
        }
    }

    build_cmd.args(extra_options);

    build_cmd.arg("-o");
    build_cmd.arg(out_name);

    log::debug!("Build Command: {:?}", build_cmd);
    let build_status = build_cmd.status()?;

    if !build_status.success() {
        eprintln!("Build failed!");
        bail!("build_executable with clang failed");
    }

    let elapsed = clang_time.elapsed();
    if k1.config.chatty {
        eprintln!("link executable took {}ms", elapsed.as_millis());
    }
    Ok(())
}

pub fn codegen_module<'ctx, 'module>(
    args: &Args,
    ctx: &'ctx Context,
    k1: &'module mut TypedProgram,
) -> Result<Cg<'ctx, 'module>> {
    let codegen_start = std::time::Instant::now();
    let mut codegen = Cg::create(ctx, k1, args.debug, args.optimize);
    let mut module_name = codegen.name().to_string();
    if args.command.is_test() {
        module_name.push_str("_test");
    };
    let out_dir = codegen.k1.config.out_dir.clone();

    if let Err(e) = codegen.codegen_program() {
        let use_color = std::io::stderr().is_terminal();
        write_source_location(
            &mut std::io::stderr(),
            &codegen.k1.ast,
            e.span,
            MessageLevel::Error,
            6,
            Some(codegen.k1.ident_str(e.message)),
            use_color,
        )
        .unwrap();
        write_program_dump(codegen.k1);
        anyhow::bail!(codegen.k1.message_to_anyhow(e))
    }

    if args.chatty {
        eprintln!("codegen took {}ms", codegen_start.elapsed().as_millis());
        eprintln!("iropt: {}ms", codegen.k1.timing.total_iropt_nanos / 1_000_000);
    }

    // Under --filc, Fil-C's clang runs the whole optimization pipeline
    let optimize_ir = args.optimize && !codegen.k1.config.filc;
    if let Err(e) = codegen.optimize_verify(optimize_ir) {
        eprintln!("Codegen error: {}", e);
        anyhow::bail!(e)
    };

    if codegen.k1.config.filc {
        let llvm_text = codegen.emit_llvm_ir_text_filc();
        let ll_path = kpath::join(&out_dir, &format!("{module_name}.ll"));
        std::fs::write(Path::new(&ll_path), llvm_text)
            .map_err(|e| anyhow::anyhow!("Failed to write {ll_path}: {e}"))?;
    } else {
        if args.emit_llvm {
            let llvm_text = codegen.emit_llvm_ir_text();
            let mut f = File::create(kpath::join(&out_dir, &format!("{module_name}.ll")))
                .expect("Failed to create .ll file");
            f.write_all(llvm_text.as_bytes()).unwrap();
        }

        let path = kpath::join(&out_dir, &format!("{module_name}.o"));
        if codegen.emit_object_file(&path).is_err() {
            bail!("Error writing object file to path: {path}");
        }
    }

    write_executable(codegen.k1, &module_name, &[])?;

    Ok(codegen)
}

#[cfg(test)]
mod compiler_test {
    use super::*;

    #[test]
    fn dep_source_overrides_reach_dep_modules() {
        static SET_HOME: std::sync::Once = std::sync::Once::new();
        SET_HOME.call_once(|| unsafe {
            std::env::set_var("K1_HOME", env!("CARGO_MANIFEST_DIR"));
        });
        let root = env!("CARGO_MANIFEST_DIR");
        let dep_file =
            kpath::canonicalize(kpath::join(root, "test_src/dep_diamond_test/deps/shared/shared.k1"))
                .unwrap();
        let args = Args {
            no_std: false,
            emit_llvm: false,
            optimize: false,
            dump_module: false,
            dump_idents: false,
            debug: false,
            sanitize: false,
            filc: false,
            profile: false,
            chatty: false,
            optimize_ir: true,
            target: None,
            command: Command::Check { file: PathBuf::from(kpath::join(root, "test_src/dep_diamond_test")) },
        };
        let mut source_overrides = fxhash::FxHashMap::default();
        source_overrides.insert(dep_file, "fn renamed-base(): int { 21 }\n".to_string());
        let result =
            compile_program_ext(&args, LspCompileOptions { source_overrides, completion: false });
        assert!(
            result.is_err(),
            "an overridden dep source must be compiled in place of the on-disk file"
        );
    }
}

// Eventually, we want to return output and exit code to the application
pub fn run_compiled_program(
    out_dir: &str,
    program_home_dir: &str,
    module_name: &str,
    is_test: bool,
) -> Option<i32> {
    let exe_name =
        format!("{}{}", module_name, if is_test { "_test" } else { "" });
    let mut run_cmd = std::process::Command::new(kpath::join(out_dir, &exe_name));
    run_cmd.current_dir(program_home_dir);
    log::debug!("Run Command: {:?}", run_cmd);
    let run_status = run_cmd.status().unwrap();

    match run_status.code() {
        Some(code) => {
            if code != 0 {
                error!("{} exited with code: {}", module_name, code);
            }
            Some(code)
        }
        None => {
            error!("{} was terminated with signal: {:?}", module_name, run_status.signal());
            None
        }
    }
}
