// Copyright (c) 2026 knix
// All rights reserved.

use std::fs;
use std::fs::File;
use std::io::{IsTerminal, Read};
use std::os::unix::prelude::ExitStatusExt;
use std::path::Path;

use crate::kmem::{self, MStr, Mem};
use crate::lex::SpanId;
use crate::parse::{IdentPool, StringId, write_source_location};
use crate::typer::{
    K1Message, LibRefLinkType, Linkage, MemTmp, MessageLevel, NamespaceId, TypedProgram,
};
use crate::{SV8, kpath, typer};
use anyhow::{Result, bail};
use inkwell::context::Context;
use log::{error, info};

use crate::codegen_llvm::{
    self, Cg, CgError, CgKind, CodegenRoots, Pipeline, UnitOutput, UnitTiming,
};
use crate::typer::trace::{FrameId, TraceKind};

use std::path::PathBuf;

use clap::{Parser, Subcommand};

pub const MAC_SDK_VERSION: &str = "15.0.0";
pub const MAC_SDK_SYSROOT: &str = "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk";

/// Who provides `ns platform`: a posix flavor, WASI, or `bare` -- the consumer
/// of the emitted object provides the k1_platform_* symbols. Discriminants
/// match core's `type platform` either tags
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Platform {
    PosixLinux = 0,
    PosixMacos = 1,
    Wasi = 2,
    Bare = 3,
}

impl Platform {
    pub fn to_str(&self) -> &'static str {
        match self {
            Platform::PosixLinux => "posix-linux",
            Platform::PosixMacos => "posix-macos",
            Platform::Wasi => "wasi",
            Platform::Bare => "bare",
        }
    }

    pub fn dylib_ext(&self) -> &'static str {
        match self {
            Platform::PosixLinux => "so",
            Platform::PosixMacos => "dylib",
            Platform::Wasi | Platform::Bare => unreachable!("no dylibs on wasi or bare"),
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
    let platform = match std::env::consts::OS {
        "linux" => Some(Platform::PosixLinux),
        "macos" => Some(Platform::PosixMacos),
        _ => None,
    };
    Target::from(arch, platform)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Arch {
    Intel,
    Arm,
    Wasm,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
/// A target is an (arch, platform) pair; I just do a simple exhaustive enum of
/// the pairs that are real rather than a 'target triple' type of struct where
/// very few values of that type are actually valid. Bare targets emit objects
/// only: k1 never links them, and their ELF/wasm object format follows the arch
pub enum Target {
    Intel64Linux,
    Arm64Macos,
    Wasm64Wasi,
    Intel64Bare,
    Arm64Bare,
    Wasm64Bare,
}

impl Target {
    pub fn from(arch: Arch, platform: Option<Platform>) -> Option<Self> {
        match (arch, platform) {
            (Arch::Intel, Some(Platform::PosixLinux)) => Some(Target::Intel64Linux),
            (Arch::Arm, Some(Platform::PosixMacos)) => Some(Target::Arm64Macos),
            (Arch::Wasm, Some(Platform::Wasi)) => Some(Target::Wasm64Wasi),
            (Arch::Intel, Some(Platform::Bare)) => Some(Target::Intel64Bare),
            (Arch::Arm, Some(Platform::Bare)) => Some(Target::Arm64Bare),
            (Arch::Wasm, Some(Platform::Bare)) => Some(Target::Wasm64Bare),
            _ => None,
        }
    }
    pub fn platform(&self) -> Platform {
        match self {
            Target::Intel64Linux => Platform::PosixLinux,
            Target::Arm64Macos => Platform::PosixMacos,
            Target::Wasm64Wasi => Platform::Wasi,
            Target::Intel64Bare | Target::Arm64Bare | Target::Wasm64Bare => Platform::Bare,
        }
    }
    pub fn to_str(&self) -> &'static str {
        match self {
            Target::Intel64Linux => "intel64-linux",
            Target::Arm64Macos => "arm64-macos",
            Target::Wasm64Wasi => "wasm64-wasi",
            Target::Intel64Bare => "intel64-bare",
            Target::Arm64Bare => "arm64-bare",
            Target::Wasm64Bare => "wasm64-bare",
        }
    }
    pub fn arch(&self) -> Arch {
        match self {
            Target::Intel64Linux | Target::Intel64Bare => Arch::Intel,
            Target::Arm64Macos | Target::Arm64Bare => Arch::Arm,
            Target::Wasm64Wasi | Target::Wasm64Bare => Arch::Wasm,
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

fn logical_name_to_lib_filename(
    idents: &IdentPool,
    mem: &mut Mem<MemTmp>,
    module_libs_dir: &str,
    target: Target,
    link_type: LibRefLinkType,
    logical_name: &str,
) -> MStr<MemTmp> {
    // Static archives are arch artifacts: wasm objects get their own -wasm builds
    if target.arch() == Arch::Wasm {
        if link_type != LibRefLinkType::Static {
            panic!("Only static libraries are supported on wasm targets");
        }
        return kpath::join_tmp(
            mem,
            idents,
            module_libs_dir,
            format_args!("lib{logical_name}-wasm.a"),
        );
    }
    match (target.platform(), link_type) {
        (_, LibRefLinkType::Static) => {
            kpath::join_tmp(mem, idents, module_libs_dir, format_args!("lib{logical_name}.a"))
        }
        (Platform::PosixLinux, LibRefLinkType::Dynamic) => {
            kpath::join_tmp(mem, idents, module_libs_dir, format_args!("lib{logical_name}.so"))
        }
        // In Windows we'd skip the 'lib' prefix and add extension dll or lib
        (Platform::PosixMacos, LibRefLinkType::Dynamic) => {
            kpath::join_tmp(mem, idents, module_libs_dir, format_args!("lib{logical_name}.dylib"))
        }
        (Platform::PosixLinux | Platform::PosixMacos, LibRefLinkType::Default) => {
            mem.push_str(logical_name)
        }
        (Platform::Wasi | Platform::Bare, _) => {
            panic!("Only static libraries are supported on {} targets", target.to_str())
        }
    }
}

#[derive(Debug, Clone, Subcommand)]
pub enum Command {
    #[clap(alias = "c")]
    Check {
        /// File
        file: Option<PathBuf>,
    },
    #[clap(alias = "b")]
    Build {
        /// File
        file: Option<PathBuf>,
    },
    #[clap(alias = "r")]
    Run {
        /// File
        file: Option<PathBuf>,
        /// Arguments passed through to the program
        #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
        program_args: Vec<String>,
    },
    #[clap(alias = "t")]
    Test {
        /// File
        file: Option<PathBuf>,
    },
    #[clap()]
    Server {
        /// File
        file: Option<PathBuf>,
    },
    /// Run a module's setup step if stale
    #[clap()]
    Setup {
        /// Module directory
        file: Option<PathBuf>,
        /// Re-run the module's setup even if fresh
        #[arg(long, default_value_t = false)]
        force: bool,
    },
    #[clap()]
    Clean { file: Option<PathBuf> },
}

impl Command {
    pub fn file(&self) -> Option<&PathBuf> {
        match self {
            Command::Check { file }
            | Command::Build { file }
            | Command::Run { file, .. }
            | Command::Test { file }
            | Command::Server { file }
            | Command::Setup { file, .. }
            | Command::Clean { file } => file.as_ref(),
        }
    }

    pub fn kind(&self) -> CommandKind {
        match self {
            Command::Check { .. } => CommandKind::Check,
            Command::Build { .. } => CommandKind::Build,
            Command::Run { .. } => CommandKind::Run,
            Command::Test { .. } => CommandKind::Test,
            Command::Server { .. } => CommandKind::Server,
            Command::Setup { force, .. } => CommandKind::Setup { force: *force },
            Command::Clean { .. } => CommandKind::Clean,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommandKind {
    Check,
    Build,
    Run,
    Test,
    Server,
    Setup { force: bool },
    Clean,
}

impl CommandKind {
    pub fn is_test(&self) -> bool {
        matches!(self, CommandKind::Test)
    }

    pub fn codegens(&self) -> bool {
        !matches!(self, CommandKind::Check | CommandKind::Setup { .. })
    }

    pub fn inputs_hash_byte(&self) -> u8 {
        match self {
            CommandKind::Check => 0,
            CommandKind::Build => 1,
            CommandKind::Run => 2,
            CommandKind::Test => 3,
            CommandKind::Server => 4,
            CommandKind::Setup { force: false } => 5,
            CommandKind::Setup { force: true } => 6,
            CommandKind::Clean => 7,
        }
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

    /// Write the compile trace as folded stacks to out_dir/{program_name}_trace.folded
    #[arg(long, default_value_t = false)]
    pub dump_trace: bool,

    /// Generate debug info
    #[arg(long)]
    pub debug: bool,

    /// Link AddressSanitizer and UndefinedBehaviorSanitizer
    #[arg(long)]
    pub sanitize: bool,

    /// Compile and link through a Fil-C toolchain (memory-safe C runtime).
    /// Requires target intel64-linux and the K1_FILC env var pointing at a
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

    /// Disk-cached compiles
    #[arg(long, default_value_t = true, action = clap::ArgAction::Set)]
    pub cache: bool,

    /// Target platform
    #[arg(long)]
    pub target: Option<Target>,

    /// Internal: nested compiles inherit the outer compile's k1 home instead of
    /// re-deriving it from the environment
    #[arg(skip)]
    pub k1_home_override: Option<String>,

    #[command(subcommand)]
    pub command: Command,
}

impl Args {
    pub fn file(&self) -> Option<&PathBuf> {
        self.command.file()
    }
}

/// All paths are canonical UTF-8 strings interned in the ident pool; see kpath
#[derive(Debug, Clone, Copy)]
pub struct CompilerConfig {
    pub src_path: StringId,
    pub home_dir: StringId,
    pub k1_home: StringId,
    pub command: CommandKind,
    pub no_std: bool,
    pub target: Target,
    /// See detect_simd_bytes
    pub simd_bytes: u32,
    pub debug: bool,
    pub sanitize: bool,
    pub filc: bool,
    pub out_dir: StringId,
    pub out_dir_generated: StringId,
    pub cache_dir: StringId,
    pub optimize: bool,
    pub emit_llvm: bool,
    pub chatty: bool,
    /// Full frame trace with clocks: chatty or --dump-trace
    pub record_trace: bool,
    pub optimize_ir: bool,
    pub cache: bool,
}

impl CompilerConfig {
    pub fn inline_ir(&self) -> bool {
        !(self.command.codegens() && self.debug)
    }

    pub fn host_platform(&self) -> Platform {
        match detect_host_target() {
            Some(host) => host.platform(),
            None => self.target.platform(),
        }
    }
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
pub fn module_home_from_src_path(
    idents: &IdentPool,
    src_path: StringId,
) -> (bool, StringId, StringId) {
    let src_path_str = idents.get_string(src_path);
    if Path::new(src_path_str).is_dir() {
        let module_name = idents.intern(kpath::file_name(src_path_str));
        (true, src_path, module_name)
    } else {
        let module_name = idents.intern(kpath::file_stem(src_path_str));
        (false, idents.intern(kpath::parent(src_path_str)), module_name)
    }
}

// Deliberately dependency-free so the LSP can share the logic for bootstrap
pub fn detect_module_root_file(home_dir: &str) -> Option<PathBuf> {
    let mut path_buf = PathBuf::from(home_dir);
    path_buf.push("module.k1");
    if path_buf.is_file() {
        Some(path_buf)
    } else {
        path_buf.pop();
        let dir_name = kpath::file_name(home_dir);
        path_buf.push(dir_name);
        let module_is_file_stem_path = path_buf.with_extension("k1");
        if module_is_file_stem_path.is_file() { Some(module_is_file_stem_path) } else { None }
    }
}

/// Given a .k1 source file, the src_path to compile: its directory when that
/// directory is a module dir else the file itself
/// Used by lsp bootstrap where we always start out pointed at a file
pub fn find_check_target_for_file(file: &Path) -> PathBuf {
    let dir = file.parent().unwrap();
    match detect_module_root_file(&dir.to_string_lossy()) {
        None => file.to_owned(),
        Some(_root_file) => dir.to_owned(),
    }
}

pub struct SourceFile {
    /// canonical absolute, owned String because these are handed across the reader threads
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

pub struct ModuleLoadHandle {
    /// canonicalized module path (dir or single file)
    pub module_name: StringId,
    pub src_path: StringId,
    pub module_dir: StringId,
    pub root_source_file_path: StringId,
    is_dir: bool,
    reader: std::thread::JoinHandle<Result<SourceFile, String>>,
    remaining: ModuleRemainingSourcesHandle,
}

pub fn spawn_module_load<MemTag>(
    idents: &IdentPool,
    tmp: &mut Mem<MemTag>,
    // canonical, absolute source path for the module
    src_path: StringId,
    is_core: bool,
    source_overrides: &fxhash::FxHashMap<String, String>,
) -> anyhow::Result<ModuleLoadHandle> {
    let (is_dir, home_dir, module_name) = module_home_from_src_path(idents, src_path);
    let root_source_file_path = if !is_dir {
        src_path
    } else if is_core {
        kpath::join_id(idents, tmp, home_dir, "builtin.k1")
    } else {
        match detect_module_root_file(idents.get_string(home_dir)) {
            None => bail!(
                "module '{}' has no root file. This can be module.k1 or {}.k1",
                idents.get_string(module_name),
                idents.get_string(module_name)
            ),
            Some(root_file_path) => idents.intern(root_file_path.to_string_lossy()),
        }
    };
    let override_content = source_overrides.get(idents.get_string(root_source_file_path)).cloned();
    let read_path_for_thread = idents.get_string(root_source_file_path).to_owned();
    let reader =
        std::thread::spawn(move || read_source_file(read_path_for_thread, override_content));
    let remaining =
        spawn_sources_read(idents, home_dir, root_source_file_path, is_dir, source_overrides);
    Ok(ModuleLoadHandle {
        module_name,
        src_path,
        module_dir: home_dir,
        root_source_file_path,
        is_dir,
        reader,
        remaining,
    })
}

impl ModuleLoadHandle {
    pub fn await_read_remaining(self) -> anyhow::Result<(SourceFile, ModuleRemainingSources)> {
        let root = self
            .reader
            .join()
            .expect("module root reader thread panicked")
            .map_err(|e| anyhow::anyhow!(e))?;
        let remaining = ModuleRemainingSources {
            module_dir: self.module_dir,
            root_source_file_path: self.root_source_file_path,
            is_dir: self.is_dir,
            speculative_read_handle: self.remaining,
        };
        Ok((root, remaining))
    }
}

pub fn collect_directory_module_source_paths(
    module_dir: &str,
    root_path: &str,
) -> Result<Vec<String>, String> {
    let entries = fs::read_dir(Path::new(module_dir))
        .map_err(|e| format!("Failed to list module dir {module_dir}: {e}"))?;
    let mut files = vec![];
    for item in entries {
        let Ok(item) = item else {
            continue;
        };
        let path = item.path();
        if path.extension().is_some_and(|ext| ext == "k1") {
            let path_string = path.into_os_string().into_string().map_err(|s| {
                format!("Source file name is not valid UTF-8: {}", s.to_string_lossy())
            })?;

            // The root is parsed separately
            if path_string != root_path {
                files.push(path_string);
            }
        }
    }
    files.sort();
    Ok(files)
}

pub struct ModuleRemainingSources {
    module_dir: StringId,
    root_source_file_path: StringId,
    is_dir: bool,
    speculative_read_handle: ModuleRemainingSourcesHandle,
}

impl ModuleRemainingSources {
    pub fn is_dir(&self) -> bool {
        self.is_dir
    }

    pub fn into_read_sources_handle(
        self,
        idents: &IdentPool,
        setup_ran: bool,
        source_overrides: &fxhash::FxHashMap<String, String>,
    ) -> ModuleRemainingSourcesHandle {
        if setup_ran {
            spawn_sources_read(
                idents,
                self.module_dir,
                self.root_source_file_path,
                self.is_dir,
                source_overrides,
            )
        } else {
            self.speculative_read_handle
        }
    }
}

pub fn spawn_sources_read(
    idents: &IdentPool,
    module_dir: StringId,
    root_path: StringId,
    is_dir: bool,
    source_overrides: &fxhash::FxHashMap<String, String>,
) -> ModuleRemainingSourcesHandle {
    let overrides = source_overrides.clone();
    let module_dir_str = idents.get_string(module_dir);
    let root_path_str = idents.get_string(root_path);
    let reader = std::thread::spawn(move || {
        if !is_dir {
            return Ok(vec![]);
        }
        let paths = collect_directory_module_source_paths(module_dir_str, root_path_str)?;
        let mut sources = Vec::with_capacity(paths.len());
        for path in paths {
            let override_content = overrides.get(&path).cloned();
            sources.push(read_source_file(path, override_content)?);
        }
        Ok(sources)
    });
    ModuleRemainingSourcesHandle { reader }
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
    xxhash_rust::xxh3::xxh3_64(bytes)
}

fn hash_file_content64(path: &str, buf: &mut [u8]) -> std::io::Result<u64> {
    let mut file = fs::File::open(Path::new(path))?;
    let mut h = xxhash_rust::xxh3::Xxh3::new();
    loop {
        let n = file.read(buf)?;
        if n == 0 {
            break;
        }
        h.update(&buf[..n]);
    }
    Ok(h.digest())
}

const FILE_HASH_BUF_LEN: usize = 64 * 1024;

pub struct SetupRequest<'a> {
    pub idents: &'a IdentPool,
    pub module_dir: StringId,
    pub module_name: StringId,
    pub root_filename: StringId,
    pub outputs: &'a [StringId],
    pub inputs: &'a [StringId],
    pub target: Target,
    pub force: bool,
}

/// Live for the duration of one setup run: holds the cross-process lock and the
/// module dir as cwd, which `fn setup` runs in
pub struct StartedSetup {
    fingerprint: String,
    _lock: SetupLock,
    _cwd: CwdGuard,
}

/// `None` when the declared outputs are already fresh. Otherwise the stamp is
/// cleared and the declared outputs removed, so a failed run reads as stale
pub fn start_setup<Tag>(
    req: &SetupRequest,
    scratch: &mut Mem<Tag>,
) -> Result<Option<StartedSetup>> {
    let module_name = req.idents.get_string(req.module_name);
    let module_dir = req.idents.get_string(req.module_dir);
    let root_path = kpath::join_tmp(scratch, req.idents, req.module_dir, req.root_filename);
    let root_src = fs::read_to_string(Path::new(root_path.as_str())).map_err(|e| {
        anyhow::anyhow!(
            "module '{module_name}' declares setup but {root_path} could not be read: {e}"
        )
    })?;

    let fingerprint = setup_fingerprint(req, scratch, &root_src)?;
    let setup_out_dir = kpath::join_tmp(scratch, req.idents, req.module_dir, (".k1-out", "setup"));
    let stamp_path = kpath::join_tmp(scratch, req.idents, setup_out_dir.as_str(), "stamp");

    if !req.force && setup_is_fresh(req, scratch, stamp_path.as_str(), &fingerprint) {
        return Ok(None);
    }

    fs::create_dir_all(Path::new(setup_out_dir.as_str()))?;
    let lock_path = kpath::join_tmp(scratch, req.idents, setup_out_dir.as_str(), "lock");
    let lock = SetupLock::acquire(lock_path.as_str())?;
    // Another process may have completed this setup while we waited on the lock
    if !req.force && setup_is_fresh(req, scratch, stamp_path.as_str(), &fingerprint) {
        return Ok(None);
    }

    eprintln!("Setting up module '{module_name}' (running fn setup in {root_path})...");
    let _ = fs::remove_file(Path::new(stamp_path.as_str()));
    for output in req.outputs {
        let output_path = kpath::join_tmp(scratch, req.idents, req.module_dir, *output);
        let p = Path::new(output_path.as_str());
        if p.is_dir() {
            fs::remove_dir_all(p)?;
        } else if p.exists() {
            fs::remove_file(p)?;
        }
    }
    Ok(Some(StartedSetup { fingerprint, _lock: lock, _cwd: CwdGuard::enter(module_dir) }))
}

pub fn finish_setup<Tag>(
    req: &SetupRequest,
    started: StartedSetup,
    scratch: &mut Mem<Tag>,
) -> Result<()> {
    let module_name = req.idents.get_string(req.module_name);
    let outputs = setup_output_manifest(req, scratch)
        .map_err(|e| anyhow::anyhow!("fn setup for module '{module_name}' completed but {e}"))?;
    let stamp_path =
        kpath::join_tmp(scratch, req.idents, req.module_dir, (".k1-out", ("setup", "stamp")));
    fs::write(Path::new(stamp_path.as_str()), format!("{}{outputs}", started.fingerprint))?;
    Ok(())
}

fn setup_is_fresh<Tag>(
    req: &SetupRequest,
    scratch: &mut Mem<Tag>,
    stamp_path: &str,
    fingerprint: &str,
) -> bool {
    let Ok(existing) = fs::read_to_string(Path::new(stamp_path)) else {
        return false;
    };
    let Ok(outputs) = setup_output_manifest(req, scratch) else {
        return false;
    };
    existing.strip_prefix(fingerprint) == Some(outputs.as_str())
}

/// One stamp line: the file's module-relative path and content hash
fn writeln_file_hash(
    s: &mut String,
    label: &str,
    module_dir: &str,
    file: &str,
    buf: &mut [u8],
) -> Result<()> {
    use std::fmt::Write;
    let hash = hash_file_content64(file, buf)
        .map_err(|e| anyhow::anyhow!("failed to read setup {label} {file}: {e}"))?;
    let rel = file.strip_prefix(module_dir).unwrap_or(file).trim_start_matches('/');
    writeln!(s, "{label}-file: {} {:016x}", rel, hash).unwrap();
    Ok(())
}

/// The output half of the stamp: every file under every declared output, hashed
fn setup_output_manifest<Tag>(req: &SetupRequest, scratch: &mut Mem<Tag>) -> Result<String> {
    let module_dir = req.idents.get_string(req.module_dir);
    let mut s = String::new();
    let buf = scratch.push_slice_uninit::<u8>(FILE_HASH_BUF_LEN);
    let mut collected_filenames: Vec<String> = vec![];
    for output in req.outputs {
        let output_path = kpath::join_tmp(scratch, req.idents, req.module_dir, *output);
        if !Path::new(output_path.as_str()).exists() {
            bail!("did not produce declared output '{}'", req.idents.get_string(*output));
        }
        collected_filenames.clear();
        collect_files_within_dir(output_path.as_str(), &mut collected_filenames)?;
        collected_filenames.sort();
        for file in &collected_filenames {
            writeln_file_hash(&mut s, "output", module_dir, file, buf)?;
        }
    }
    Ok(s)
}

fn setup_fingerprint<Tag>(
    req: &SetupRequest,
    scratch: &mut Mem<Tag>,
    root_src: &str,
) -> Result<String> {
    use std::fmt::Write;
    let module_dir = req.idents.get_string(req.module_dir);
    let mut s = String::new();
    writeln!(s, "k1-setup-stamp v4").unwrap();
    writeln!(s, "target: {}", req.target.to_str()).unwrap();
    writeln!(
        s,
        "root: {} {:016x}",
        req.idents.get_string(req.root_filename),
        content_hash64(root_src.as_bytes())
    )
    .unwrap();
    for (label, ids) in [("outputs", req.outputs), ("inputs", req.inputs)] {
        write!(s, "{label}: ").unwrap();
        for (i, id) in ids.iter().enumerate() {
            if i > 0 {
                s.push('|');
            }
            s.push_str(req.idents.get_string(*id));
        }
        s.push('\n');
    }
    let mut output_paths: kmem::List<MStr<Tag>, Tag> = scratch.new_list(req.outputs.len() as u32);
    for o in req.outputs {
        let path = kpath::join_tmp(scratch, req.idents, req.module_dir, *o);
        output_paths.push(path);
    }
    let mut buf = vec![0u8; FILE_HASH_BUF_LEN];
    let mut files_named_by_path: Vec<String> = vec![];
    for input in req.inputs {
        let input_path = kpath::join_tmp(scratch, req.idents, req.module_dir, *input);
        files_named_by_path.clear();
        collect_files_within_dir(input_path.as_str(), &mut files_named_by_path)?;
        files_named_by_path.sort();
        for file in &files_named_by_path {
            if output_paths.iter().any(|o| o.as_str() == file) {
                continue;
            }
            writeln_file_hash(&mut s, "input", module_dir, file, &mut buf)?;
        }
    }
    Ok(s)
}

fn collect_files_within_dir(path: &str, out: &mut Vec<String>) -> Result<()> {
    let p = Path::new(path);
    if p.is_file() {
        out.push(path.to_string());
    } else if p.is_dir() {
        for entry in fs::read_dir(p)? {
            let child = entry?.path().into_os_string().into_string().map_err(|s| {
                anyhow::anyhow!("setup path is not valid UTF-8: {}", s.to_string_lossy())
            })?;
            collect_files_within_dir(&child, out)?;
        }
    } else {
        bail!("setup input '{path}' does not exist");
    }
    Ok(())
}

/// Advisory lock so concurrent compiles (LSP background + CLI) can't run a
/// module's setup twice; released when the file handle drops
struct SetupLock {
    _file: File,
}

impl SetupLock {
    fn acquire(path: &str) -> Result<SetupLock> {
        let file = File::create(Path::new(path))
            .map_err(|e| anyhow::anyhow!("failed to create setup lock {path}: {e}"))?;
        #[cfg(unix)]
        {
            use std::os::unix::io::AsRawFd;
            let rc = unsafe { libc::flock(file.as_raw_fd(), libc::LOCK_EX) };
            if rc != 0 {
                bail!("failed to lock {path}: {}", std::io::Error::last_os_error());
            }
        }
        Ok(SetupLock { _file: file })
    }
}

struct ListedModule {
    name: StringId,
    root_source_path: StringId,
    is_dir: bool,
    setup: Option<(SV8<StringId>, SV8<StringId>)>,
}

fn read_module_list(idents: &IdentPool, cache_dir: &Path) -> Option<Vec<ListedModule>> {
    let text = crate::snap::cache_load_text(cache_dir, "modules")?;
    let mut lines = text.lines();
    if lines.next()? != format!("k1-modules v{}", crate::BUILD_ID) {
        return None;
    }
    let mut modules: Vec<ListedModule> = vec![];
    for line in lines {
        if let Some(rest) = line.strip_prefix("module\t") {
            let mut parts = rest.split('\t');
            let name = idents.intern(parts.next()?);
            let is_dir = parts.next()? == "1";
            let root_source_path = idents.intern(parts.next()?);
            modules.push(ListedModule { name, root_source_path, is_dir, setup: None });
        } else if let Some(rest) = line.strip_prefix("setup\t") {
            let mut parts = rest.split('\t');
            let n_out: usize = parts.next()?.parse().ok()?;
            let mut outs: SV8<StringId> = SV8::new();
            let mut ins: SV8<StringId> = SV8::new();
            for (i, part) in parts.enumerate() {
                let id = idents.intern(part);
                if i < n_out { outs.push(id) } else { ins.push(id) }
            }
            if outs.len() < n_out {
                return None;
            }
            modules.last_mut()?.setup = Some((outs, ins));
        } else if !line.is_empty() {
            return None;
        }
    }
    Some(modules)
}

/// Record which modules and files made up this compile, in typing order, so
/// the next compile can compute inputs hashes without loading anything
fn write_module_list(k1: &TypedProgram) {
    use std::fmt::Write as _;
    let mut s = format!("k1-modules v{}\n", crate::BUILD_ID);
    for &module_id in &k1.modules_completed {
        let m = k1.modules.get(module_id);
        writeln!(
            s,
            "module\t{}\t{}\t{}",
            k1.get_string(m.name),
            m.is_dir as u8,
            k1.get_string(m.root_file_path),
        )
        .unwrap();
        if let Some(setup) = m.manifest.setup {
            let outs = k1.mem.getn(setup.outputs);
            let ins = k1.mem.getn(setup.inputs);
            write!(s, "setup\t{}", outs.len()).unwrap();
            for id in outs.iter().chain(ins.iter()) {
                write!(s, "\t{}", k1.get_string(*id)).unwrap();
            }
            writeln!(s).unwrap();
        }
    }
    crate::snap::cache_store_text(k1.cache_dir(), "modules", &s);
}

fn inputs_hashes_from_module_list<Tag>(
    idents: &IdentPool,
    config: &CompilerConfig,
    overrides: &fxhash::FxHashMap<String, String>,
    modules: &[ListedModule],
    scratch: &mut Mem<Tag>,
) -> Vec<crate::snap::InputsHash> {
    let mut buffer = vec![0u8; FILE_HASH_BUF_LEN];
    let hash_file = |path: &str, buf: &mut [u8]| -> Option<u64> {
        match overrides.get(path) {
            Some(content) => Some(content_hash64(content.as_bytes())),
            None => hash_file_content64(path, buf).ok(),
        }
    };
    let n = modules.len();
    let mut group_ends: SV8<usize> = SV8::new();
    group_ends.push(1.min(n));
    if !config.no_std {
        group_ends.push(2.min(n));
    }
    group_ends.push(n);

    let mut hash = typer::snapshot::inputs_hash_from_settings(idents, config);
    let mut hashes = Vec::with_capacity(n);
    let mut start = 0;
    for end in group_ends {
        let group = &modules[start..end];
        start = end;
        for m in group {
            let root_path = idents.get_string(m.root_source_path);
            let Some(root_hash) = hash_file(root_path, &mut buffer) else {
                // An unreadable root invalidates the whole group and everything after
                return hashes;
            };
            hash = hash.add_module_header(idents.get_string(m.name), root_path, root_hash);
        }
        for m in group {
            if m.setup.as_ref().is_some_and(|(outputs, inputs)| {
                !listed_setup_is_fresh(scratch, idents, config, m, outputs, inputs)
            }) {
                // If we're running setup, give up on getting a cache hit for this module this time
                // around
                return hashes;
            }
            let mut sources: kmem::List<(String, u64), Tag> = kmem::List::empty();
            if m.is_dir {
                let root_path = idents.get_string(m.root_source_path);
                let home_dir = kpath::parent(root_path);
                let Ok(files) = collect_directory_module_source_paths(home_dir, root_path) else {
                    return hashes;
                };
                sources = scratch.new_list(files.len() as u32);
                for path in files {
                    let Some(h) = hash_file(&path, &mut buffer) else { return hashes };
                    sources.push((path, h));
                }
            }
            hash = hash.add_module_sources(
                idents.get_string(m.name),
                sources.iter().map(|(p, h)| (p.as_str(), *h)),
            );
            hashes.push(hash);
        }
    }
    hashes
}

fn listed_setup_is_fresh<Tag>(
    scratch: &mut Mem<Tag>,
    idents: &IdentPool,
    config: &CompilerConfig,
    m: &ListedModule,
    outputs: &[StringId],
    inputs: &[StringId],
) -> bool {
    let root_source_path = idents.get_string(m.root_source_path);
    let Ok(root_src) = fs::read_to_string(Path::new(root_source_path)) else {
        return false;
    };
    let module_dir_str = kpath::parent(root_source_path);
    let module_dir = idents.intern(module_dir_str);
    let req = SetupRequest {
        idents,
        module_dir,
        module_name: idents.intern(kpath::file_name(module_dir_str)),
        root_filename: idents.intern(kpath::file_name(root_source_path)),
        outputs,
        inputs,
        target: config.target,
        force: false,
    };
    let Ok(fingerprint) = setup_fingerprint(&req, scratch, &root_src) else {
        return false;
    };
    let stamp_path = kpath::join_tmp(scratch, idents, module_dir, (".k1-out", ("setup", "stamp")));
    setup_is_fresh(&req, scratch, stamp_path.as_str(), &fingerprint)
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
    let clock_start = crate::clock::Clock::new().raw();

    let mut ast = crate::parse::ParsedProgram::make();
    let idents = &ast.idents;
    let src_path = (match args.file() {
        None => kpath::canonicalize_string_id(idents, "."),
        Some(path_buf) => kpath::canonicalize_string_id(idents, path_buf),
    })
    .unwrap_or_else(|e| panic!("Failed to load source path: {e}"));

    let (_is_dir, home_dir, module_name) = module_home_from_src_path(idents, src_path);
    ast.name_id = module_name;

    let out_dir = kpath::join_id(&ast.idents, &mut ast.mem, home_dir, ".k1-out");
    let out_dir_generated = kpath::join_id(&ast.idents, &mut ast.mem, out_dir, "generated");
    let cache_dir = kpath::join_id(&ast.idents, &mut ast.mem, out_dir, crate::snap::CACHE_DIR_NAME);
    std::fs::create_dir_all(Path::new(ast.idents.get_string(out_dir_generated))).unwrap();

    let use_std = !args.no_std;

    let target = args
        .target
        .or(detect_host_target())
        .unwrap_or_else(|| panic!("Unsupported host platform; provide your target explicitly"));

    if args.filc {
        assert!(
            target == Target::Intel64Linux,
            "--filc requires target intel64-linux; Fil-C only supports Linux/x86_64"
        );
        assert!(!args.sanitize, "--filc and --sanitize are mutually exclusive");
    }

    // Find the installation. Nested compiles inherit, env var overrides, otherwise
    // release mode says co-located with the binary. dev mode says cwd
    let k1_home_raw = args
        .k1_home_override
        .as_ref()
        .map(PathBuf::from)
        .or_else(|| std::env::var("K1_HOME").map(PathBuf::from).ok())
        .unwrap_or_else(|| {
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
    let k1_home = kpath::canonicalize_owned(&k1_home_raw)
        .unwrap_or_else(|e| panic!("K1 home {} is not usable: {e}", k1_home_raw.display()));
    if args.chatty {
        eprintln!("using k1 home: {k1_home}");
    }
    let k1_home_id = ast.idents.intern(&k1_home);
    let corelib_dir =
        kpath::join_id(&ast.idents, &mut ast.tmp, k1_home.as_str(), ("modules", "core"));
    let stdlib_dir =
        kpath::join_id(&ast.idents, &mut ast.tmp, k1_home.as_str(), ("modules", "std"));

    let core_plan =
        spawn_module_load(&ast.idents, &mut ast.tmp, corelib_dir, true, &lsp.source_overrides);
    let std_plan = use_std.then(|| {
        spawn_module_load(&ast.idents, &mut ast.tmp, stdlib_dir, false, &lsp.source_overrides)
    });
    let main_plan =
        spawn_module_load(&ast.idents, &mut ast.tmp, src_path, false, &lsp.source_overrides);

    let config = CompilerConfig {
        src_path,
        home_dir,
        k1_home: k1_home_id,
        command: args.command.kind(),
        no_std: args.no_std,
        target,
        simd_bytes: detect_simd_bytes(target),
        debug: args.debug,
        sanitize: args.sanitize,
        filc: args.filc,
        out_dir,
        out_dir_generated,
        cache_dir,
        optimize: args.optimize,
        emit_llvm: args.emit_llvm,
        chatty: args.chatty,
        record_trace: args.chatty || args.dump_trace,
        optimize_ir: args.optimize_ir,
        cache: args.cache,
    };

    let _cwd = CwdGuard::enter(idents.get_string(home_dir));

    let mut k1 = 'program: {
        let cache_dir = Path::new(ast.idents.get_string(cache_dir));
        if args.cache
            && let Some(modules) = read_module_list(&ast.idents, cache_dir)
        {
            let tmp_mark = ast.tmp.mark();
            let input_hashes_by_module = inputs_hashes_from_module_list(
                &ast.idents,
                &config,
                &lsp.source_overrides,
                &modules,
                &mut ast.tmp,
            );
            ast.tmp.reset_to(tmp_mark);
            if args.chatty {
                eprintln!(
                    "cache: {} of {} listed modules have valid inputs",
                    input_hashes_by_module.len(),
                    modules.len()
                );
            }

            // The snapshot with the most modules in it is at the end
            let clock = crate::clock::Clock::new();
            for (i, hash) in input_hashes_by_module.iter().enumerate().rev() {
                // If we have a hit, the file exists by this name, and we restore
                let load_start = clock.raw();
                let Some(bytes) = crate::snap::cache_load(cache_dir, *hash) else { continue };
                let load_end = clock.raw();
                let module_count = i as u32 + 1;
                let live = args.chatty && std::io::stderr().is_terminal();
                match TypedProgram::restore(
                    &bytes,
                    config,
                    lsp.clone(),
                    (load_start, load_end),
                    live,
                ) {
                    Ok(mut restored) => {
                        restored.restored_module_count = module_count;
                        let msg = format!(
                            "restored {module_count} modules from cache ({:.1}mb)",
                            bytes.len() as f64 / (1024.0 * 1024.0)
                        );
                        info!("{msg}");
                        if args.chatty {
                            eprintln!("{msg}");
                        }
                        break 'program restored;
                    }
                    Err(e) => {
                        if args.chatty {
                            eprintln!("ignoring cache entry: {e}");
                        }
                    }
                }
            }
        }
        TypedProgram::new(ast, config, lsp)
    };
    k1.trace.clock_start = clock_start;

    let add_result = (|| {
        k1.add_module(core_plan?, false)?;
        if let Some(std_plan) = std_plan {
            k1.add_module(std_plan?, false)?;
        }
        k1.add_module(main_plan?, true)
    })();
    k1.write_emitted_sources();
    if args.cache {
        write_module_list(&k1);
    }
    if let Err(e) = add_result {
        if args.dump_module {
            write_program_dump(&k1);
        }
        eprintln!("{}", e);
        if k1.error_count(&[MessageLevel::Error]) == 0 {
            let message = k1.make_error(format!("{e}"), SpanId::NONE);
            k1.messages.borrow_mut().push(message);
        }
        return Err(CompileProgramError::TyperFailure(Box::new(k1)));
    };
    let warning_count =
        k1.messages.borrow().iter().filter(|e| e.level == MessageLevel::Warn).count();
    if warning_count > 0 {
        k1.trace_live_clear();
        eprintln!("Completed with {} warnings", warning_count);
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

            let fname = format!("{}.svg", k1.program_name());
            eprintln!("Outputting profile flamegraph to {fname}");
            let file = File::create(fname).unwrap();
            options.reverse_stack_order = false;
            options.direction = pprof::flamegraph::Direction::Inverted;
            report.flamegraph_with_options(file, &mut options).unwrap();

            let fname_rev = format!("{}_reverse.svg", k1.program_name());
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LinkOutputKind {
    Executable,
    Dylib,
}

struct ModuleLibs {
    libs_dir: String,
    link_args: Vec<String>,
    libs: Vec<(LibRefLinkType, String)>,
}

fn collect_all_module_libs(k1: &TypedProgram) -> Vec<ModuleLibs> {
    let idents = &k1.ast.idents;
    let filc = k1.config.filc;
    let mut out: Vec<ModuleLibs> = vec![];
    for module in k1.modules.iter() {
        let module_libs_dir =
            kpath::join_tmp(k1.get_tmp_unsafe(), idents, module.home_dir, LIBS_DIR_NAME);
        let mut link_args: Vec<String> = vec![];
        for link_arg_string_id in k1.mem.getn(module.manifest.link_args) {
            link_args.push(k1.get_string(*link_arg_string_id).to_string());
        }
        let mut libs: Vec<(LibRefLinkType, String)> = vec![];
        for lib in k1.mem.getn(module.manifest.libs) {
            let logical_name_str = k1.get_string(lib.name);
            let logical_name =
                if filc { format!("{logical_name_str}-filc") } else { logical_name_str.into() };
            let filename = logical_name_to_lib_filename(
                idents,
                k1.get_tmp_unsafe(),
                module_libs_dir.as_str(),
                k1.config.target,
                lib.link_type,
                &logical_name,
            );
            libs.push((lib.link_type, filename.as_str().to_string()));
        }
        out.push(ModuleLibs { libs_dir: module_libs_dir.as_str().to_string(), link_args, libs });
    }
    out
}

fn push_module_lib_args(k1: &TypedProgram, args: &mut Vec<String>) {
    for module_libs in collect_all_module_libs(k1) {
        if !module_libs.libs.is_empty() {
            args.push(format!("-L{}", module_libs.libs_dir));
        }
        for link_arg in &module_libs.link_args {
            args.push(link_arg.clone());
        }
        for (link_type, filename) in &module_libs.libs {
            match link_type {
                // Link via linker arg, since the name has no extension
                LibRefLinkType::Default => args.push(format!("-l{filename}")),
                // 'Link' via direct arg, since its an exact filepath
                _ => args.push(filename.clone()),
            };
        }
    }
}

unsafe extern "C" {
    fn k1_lld_link(args: *const *const std::ffi::c_char, num_args: usize) -> i32;
}

// Call our linked-in lld, built with wasm support.
// `flavor` is lld's argv[0] ("wasm-ld", "ld.lld").
fn lld_link(flavor: &str, args: &[String]) -> Result<()> {
    let mut cstrings: Vec<std::ffi::CString> = Vec::with_capacity(args.len() + 1);
    cstrings.push(std::ffi::CString::new(flavor).unwrap());
    for arg in args {
        cstrings.push(std::ffi::CString::new(arg.as_str())?);
    }
    let mut ptrs: Vec<*const std::ffi::c_char> = Vec::with_capacity(cstrings.len());
    for c in &cstrings {
        ptrs.push(c.as_ptr());
    }
    log::debug!("{flavor} {}", args.join(" "));
    let code = unsafe { k1_lld_link(ptrs.as_ptr(), ptrs.len()) };
    if code != 0 {
        bail!("{flavor} failed with code {code}");
    }
    Ok(())
}

fn command_status(cmd: &mut std::process::Command) -> Result<std::process::ExitStatus> {
    let program = cmd.get_program().to_string_lossy().into_owned();
    cmd.status().map_err(|e| match e.kind() {
        std::io::ErrorKind::NotFound => {
            anyhow::anyhow!("`{program}` is not installed or not on PATH")
        }
        _ => anyhow::anyhow!("failed to run `{program}`: {e}"),
    })
}

pub fn write_linked_output(
    k1: &TypedProgram,
    module_name: &str,
    objects: &[String],
    extra_options: &[String],
    kind: LinkOutputKind,
) -> Result<()> {
    let target = k1.config.target;
    let debug = k1.config.debug;
    let idents = &k1.ast.idents;
    let out_dir = k1.config.out_dir;
    let optimize = k1.config.optimize;
    let sanitize = k1.config.sanitize;
    let filc = k1.config.filc;

    let out_name = match kind {
        LinkOutputKind::Executable => {
            kpath::join_tmp(k1.get_tmp_unsafe(), idents, out_dir, module_name)
        }
        LinkOutputKind::Dylib => kpath::join_tmp(
            k1.get_tmp_unsafe(),
            idents,
            out_dir,
            format_args!("lib{module_name}.{}", target.platform().dylib_ext()),
        ),
    };

    if target.arch() == Arch::Wasm {
        let mut ld_args: Vec<String> = vec!["-mwasm64".into()];
        ld_args.extend_from_slice(objects);
        push_module_lib_args(k1, &mut ld_args);
        // stack-first makes null (0) deref trap
        ld_args.push("--stack-first".into());
        ld_args.push("-z".into());
        ld_args.push("stack-size=8388608".into());
        ld_args.extend_from_slice(extra_options);
        ld_args.push("-o".into());
        ld_args.push(out_name.as_str().into());

        lld_link("wasm-ld", &ld_args).map_err(|e| anyhow::anyhow!("linking {out_name}: {e}"))?;
        return Ok(());
    }

    let mut build_cmd = if filc {
        let filc_home = std::env::var("K1_FILC").map(PathBuf::from).map_err(|_| {
            anyhow::anyhow!("--filc requires K1_FILC to point at a Fil-C installation")
        })?;
        std::process::Command::new(filc_home.join("build/bin/clang"))
    } else {
        std::process::Command::new("cc")
    };
    if kind == LinkOutputKind::Dylib {
        match target.platform() {
            Platform::PosixMacos => {
                build_cmd.arg("-dynamiclib");
                build_cmd.arg(format!(
                    "-Wl,-install_name,@rpath/lib{module_name}.{}",
                    target.platform().dylib_ext()
                ));
                build_cmd.arg(format!(
                    "-Wl,-exported_symbols_list,{}",
                    build_export_list_file_path(k1, module_name)
                ));
            }
            Platform::PosixLinux => {
                build_cmd.arg("-shared");
                build_cmd.arg(format!(
                    "-Wl,--version-script={}",
                    build_version_script_file_path(k1, module_name)
                ));
            }
            Platform::Wasi | Platform::Bare => {
                bail!("dylib output is not supported on {}", target.to_str())
            }
        }
    }

    if target.platform() == Platform::PosixMacos {
        build_cmd.arg(format!("-mmacosx-version-min={}", MAC_SDK_VERSION));
    }

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
        if target.platform() == Platform::PosixLinux {
            build_cmd.arg("-g");
        } else {
            build_cmd.arg("-gline-tables-only");
        }
        build_cmd.arg("-fno-omit-frame-pointer");
    };
    if sanitize {
        build_cmd.arg("-fsanitize=address,undefined");
    }

    // Our actual compiled k1 code!
    build_cmd.args(objects);

    // Linking with libraries.
    // For each module, for each of its libraries, link with it as specified by the link_type
    let mut lib_args: Vec<String> = vec![];
    push_module_lib_args(k1, &mut lib_args);

    // libm is part of libSystem on darwin but a separate library on linux, and
    // it has to come after the objects and archives that reference it
    if target.platform() == Platform::PosixLinux {
        lib_args.push("-lm".into());
    }

    build_cmd.args(lib_args);

    build_cmd.args(extra_options);

    build_cmd.arg("-o");
    build_cmd.arg(out_name.as_str());

    log::debug!("Build Command: {:?}", build_cmd);
    let build_status = command_status(&mut build_cmd)?;

    if !build_status.success() {
        eprintln!("Build failed!");
        bail!("linking {out_name} with clang failed");
    }
    Ok(())
}

fn build_export_list_file_path(k1: &TypedProgram, module_name: &str) -> String {
    let path = kpath::join_tmp(
        k1.get_tmp_unsafe(),
        &k1.ast.idents,
        k1.config.out_dir,
        format_args!("{module_name}.exports"),
    );
    path.as_str().to_string()
}

fn build_version_script_file_path(k1: &TypedProgram, module_name: &str) -> String {
    let path = kpath::join_tmp(
        k1.get_tmp_unsafe(),
        &k1.ast.idents,
        k1.config.out_dir,
        format_args!("{module_name}.version"),
    );
    path.as_str().to_string()
}

pub fn write_library_export_files(k1: &TypedProgram, module_name: &str) -> Result<()> {
    let mut symbols: Vec<String> = vec![];
    for (_, function) in k1.function_iter() {
        if let Linkage::Exported { fn_name } = function.linkage {
            symbols.push(k1.ident_str(fn_name.unwrap_or(function.name)).to_string());
        }
    }
    for global_id in k1.globals.iter_ids() {
        let global = k1.globals.get(global_id);
        if global.is_exported {
            symbols.push(k1.ident_str(k1.variables.get(global.variable_id).name).to_string());
        }
    }
    symbols.sort();

    match k1.config.target.platform() {
        Platform::PosixMacos => {
            let mut list = String::with_capacity(symbols.len() * 24);
            for s in &symbols {
                list.push('_');
                list.push_str(s);
                list.push('\n');
            }
            let list_path = build_export_list_file_path(k1, module_name);
            std::fs::write(&list_path, list)
                .map_err(|e| anyhow::anyhow!("Failed to write {list_path}: {e}"))?;
        }
        Platform::PosixLinux => {
            let mut list = String::with_capacity(symbols.len() * 24);
            let mut script = String::with_capacity(symbols.len() * 24 + 32);
            script.push_str("{ global:\n");
            for s in &symbols {
                list.push_str(s);
                list.push('\n');
                script.push_str(s);
                script.push_str(";\n");
            }
            script.push_str("local: *; };\n");
            let list_path = build_export_list_file_path(k1, module_name);
            std::fs::write(&list_path, list)
                .map_err(|e| anyhow::anyhow!("Failed to write {list_path}: {e}"))?;
            let script_path = build_version_script_file_path(k1, module_name);
            std::fs::write(&script_path, script)
                .map_err(|e| anyhow::anyhow!("Failed to write {script_path}: {e}"))?;
        }
        Platform::Wasi | Platform::Bare => {
            bail!("library output is not supported on {}", k1.config.target.to_str())
        }
    }
    Ok(())
}

pub fn write_library_archive(
    k1: &TypedProgram,
    module_name: &str,
    objects: &[String],
) -> Result<()> {
    let target = k1.config.target;
    let idents = &k1.ast.idents;
    let out_dir = k1.config.out_dir;

    let combined_name =
        kpath::join_tmp(k1.get_tmp_unsafe(), idents, out_dir, format_args!("lib{module_name}.o"));
    let archive_name =
        kpath::join_tmp(k1.get_tmp_unsafe(), idents, out_dir, format_args!("lib{module_name}.a"));

    let mut static_libs: Vec<String> = vec![];
    for module_libs in collect_all_module_libs(k1) {
        for (link_type, path) in module_libs.libs {
            if link_type == LibRefLinkType::Static && !static_libs.contains(&path) {
                static_libs.push(path);
            }
        }
    }

    let mut ld_cmd = std::process::Command::new("ld");
    ld_cmd.arg("-r");
    ld_cmd.args(objects);
    for lib in &static_libs {
        ld_cmd.arg(lib);
    }
    match target.platform() {
        Platform::PosixMacos => {
            ld_cmd.arg("-exported_symbols_list");
            ld_cmd.arg(build_export_list_file_path(k1, module_name));
        }
        Platform::PosixLinux => {}
        Platform::Wasi | Platform::Bare => {
            bail!("static library output is not supported on {}", target.to_str())
        }
    }
    ld_cmd.arg("-o");
    ld_cmd.arg(combined_name.as_str());
    log::debug!("Partial link Command: {:?}", ld_cmd);
    if !command_status(&mut ld_cmd)?.success() {
        bail!("partial link of {combined_name} failed");
    }

    if target.platform() == Platform::PosixLinux {
        let mut objcopy_cmd = std::process::Command::new("objcopy");
        objcopy_cmd
            .arg(format!("--keep-global-symbols={}", build_export_list_file_path(k1, module_name)));
        objcopy_cmd.arg(combined_name.as_str());
        log::debug!("Localize Command: {:?}", objcopy_cmd);
        if !command_status(&mut objcopy_cmd)?.success() {
            bail!("objcopy localize of {combined_name} failed");
        }
    }

    let _ = std::fs::remove_file(archive_name.as_str());
    let mut ar_cmd = std::process::Command::new("ar");
    ar_cmd.arg("rcs");
    ar_cmd.arg(archive_name.as_str());
    ar_cmd.arg(combined_name.as_str());
    log::debug!("Archive Command: {:?}", ar_cmd);
    if !command_status(&mut ar_cmd)?.success() {
        bail!("archiving {archive_name} failed");
    }
    Ok(())
}

fn report_codegen_error(k1: &TypedProgram, e: K1Message) -> anyhow::Error {
    let use_color = std::io::stderr().is_terminal();
    write_source_location(
        &mut std::io::stderr(),
        &k1.ast,
        e.span,
        MessageLevel::Error,
        6,
        Some(k1.ident_str(e.message)),
        use_color,
    )
    .unwrap();
    write_program_dump(k1);
    k1.message_to_anyhow(e)
}

fn cg_error_to_message(k1: &TypedProgram, e: CgError) -> K1Message {
    k1.make_error(e.message, e.span)
}

pub fn codegen_module(args: &Args, ctx: &Context, k1: &mut TypedProgram) -> Result<()> {
    // Ns-driven, not fn-driven: a reload ns holding only globals still gets a dylib
    let mut reload_nss: Vec<NamespaceId> = vec![];
    for ns_id in k1.namespaces.namespaces.iter_ids() {
        if k1.namespaces.get(ns_id).reload {
            reload_nss.push(ns_id);
        }
    }
    if !reload_nss.is_empty() && args.filc {
        bail!("ns(reload) is not supported under --filc");
    }
    if !k1.program_settings.executable {
        if !reload_nss.is_empty() {
            bail!("ns(reload) requires an executable host module");
        }
        if args.filc {
            bail!("library output is not supported under --filc");
        }
    }
    for ns_id in &reload_nss {
        let frame = k1.trace_push(TraceKind::ReloadDylib, ns_id.as_u32(), 0);
        let written = write_reload_dylib(args, ctx, k1, *ns_id);
        k1.trace_pop(frame);
        written?;
    }

    let mut module_name = k1.program_name().to_string();
    if args.command.kind().is_test() {
        module_name.push_str("_test");
    };
    let prepare_frame = k1.trace_push(TraceKind::CodegenPrepare, 0, 0);
    let prepared = Cg::prepare_host(k1);
    if let Ok(roots) = &prepared {
        k1.trace.set_top_count(roots.reachable.len() as u64);
    }
    k1.trace_pop(prepare_frame);
    let roots = match prepared {
        Ok(roots) => roots,
        Err(e) => match k1.error_count(&[MessageLevel::Error]) {
            0 => anyhow::bail!(report_codegen_error(k1, e)),
            n => {
                anyhow::bail!("Module {} failed typechecking with {} errors", k1.program_name(), n)
            }
        },
    };
    let is_host_native = detect_host_target() == Some(k1.config.target);
    let object_is_artifact = !k1.program_settings.executable && !is_host_native
        || k1.config.target.platform() == Platform::Bare;
    let objects = write_unit_artifacts(
        args,
        ctx,
        k1,
        &roots,
        CgKind::Host,
        &module_name,
        object_is_artifact,
    )?;

    if k1.program_settings.executable {
        if k1.config.target.platform() == Platform::Bare {
            bail!("bare targets emit objects only; there is no executable lane");
        }
        let mut link_options: Vec<String> = vec![];
        if !reload_nss.is_empty() {
            // Ensure host globals are visible to dlopen'd reload dylibs
            let export_flag = match k1.config.target.platform() {
                Platform::PosixMacos => "-Wl,-export_dynamic",
                Platform::PosixLinux => "-rdynamic",
                Platform::Wasi | Platform::Bare => {
                    bail!("ns(reload) is not supported on {}", k1.config.target.to_str())
                }
            };
            link_options.push(export_flag.to_string());
        }
        let frame = k1.trace_push(TraceKind::Link, 0, 0);
        let linked = write_linked_output(
            k1,
            &module_name,
            &objects,
            &link_options,
            LinkOutputKind::Executable,
        );
        k1.trace_pop(frame);
        linked?;
    } else if is_host_native {
        write_library_export_files(k1, &module_name)?;
        let frame = k1.trace_push(TraceKind::Link, 0, 0);
        let linked = write_linked_output(k1, &module_name, &objects, &[], LinkOutputKind::Dylib);
        k1.trace_pop(frame);
        linked?;
        let frame = k1.trace_push(TraceKind::Archive, 0, 0);
        let archived = write_library_archive(k1, &module_name, &objects);
        k1.trace_pop(frame);
        archived?;
    }

    Ok(())
}

pub fn report_trace(args: &Args, k1: &TypedProgram) {
    k1.trace_live_clear();
    if args.chatty {
        k1.print_trace_summary(&mut std::io::stderr()).unwrap();
    }
    if args.dump_trace {
        let out_dir = k1.ast.idents.get_string(k1.config.out_dir);
        let path = format!("{out_dir}/{}_trace.folded", k1.program_name());
        let written = std::fs::File::create(&path).and_then(|file| {
            let mut out = std::io::BufWriter::new(file);
            k1.write_trace_folded(&mut out)
        });
        match written {
            Ok(()) => eprintln!("wrote trace to {path}"),
            Err(e) => eprintln!("failed to write trace to {path}: {e}"),
        }
    }
}

fn report_cg(k1: &TypedProgram, e: CgError) -> anyhow::Error {
    report_codegen_error(k1, cg_error_to_message(k1, e))
}

fn record_unit_timings(k1: &mut TypedProgram, root: Option<FrameId>, timings: &[UnitTiming]) {
    for t in timings {
        let unit = k1.trace.record(
            TraceKind::Codegen,
            t.index as u32,
            root,
            t.clock_start,
            t.clock_end,
            t.fn_count as u64,
            0,
        );
        k1.trace.record(
            TraceKind::LlvmPasses,
            t.index as u32,
            Some(unit),
            t.clock_generated,
            t.clock_end,
            0,
            0,
        );
    }
}

fn write_unit_artifacts(
    args: &Args,
    ctx: &Context,
    k1: &mut TypedProgram,
    roots: &CodegenRoots,
    kind: CgKind,
    module_name: &str,
    object_is_artifact: bool,
) -> Result<Vec<String>> {
    const MAX_UNITS: usize = 32;
    let out_dir = k1.ast.idents.get_string(k1.config.out_dir).to_string();
    let plans = Cg::plan_units(k1, &roots.reachable, MAX_UNITS);
    let unit_count = plans.len();
    // Under --filc, Fil-C's clang runs the whole optimization pipeline
    let optimize_ir = args.optimize && !k1.config.filc;
    let pipeline = if optimize_ir {
        Pipeline::O3
    } else if args.debug {
        Pipeline::None
    } else {
        Pipeline::Dev
    };
    let single_file = k1.config.filc || args.emit_llvm || object_is_artifact;
    let object_path = |i: usize| format!("{out_dir}/{module_name}.{i}.o");
    let output = if single_file {
        UnitOutput::Bitcode(Pipeline::None)
    } else if optimize_ir {
        UnitOutput::Bitcode(Pipeline::ThinLtoPreLink)
    } else {
        UnitOutput::Object(pipeline)
    };

    let codegen_frame = k1.trace_push(TraceKind::Codegen, unit_count as u32, 0);
    let generated = {
        let k1: &TypedProgram = k1;
        Cg::codegen_units(k1, roots, plans, kind, args.debug, output, object_path)
            .map_err(|e| report_cg(k1, e))
    };
    if let Ok((_, timings)) = &generated {
        record_unit_timings(k1, codegen_frame, timings);
    }
    k1.trace_pop(codegen_frame);
    let (artifacts, _) = generated?;

    if single_file {
        let merged = Cg::merge_units(ctx, module_name, &artifacts).map_err(|e| report_cg(k1, e))?;
        let machine = Cg::make_target_machine(args.optimize, k1.config.target);
        let passes_frame = k1.trace_push(TraceKind::LlvmPasses, 0, 0);
        codegen_llvm::run_passes(&merged, &machine, pipeline);
        k1.trace_pop(passes_frame);
        if k1.config.filc {
            let ll_path = format!("{out_dir}/{module_name}.ll");
            std::fs::write(Path::new(&ll_path), codegen_llvm::llvm_ir_text_filc(&merged))
                .map_err(|e| anyhow::anyhow!("Failed to write {ll_path}: {e}"))?;
            return Ok(vec![ll_path]);
        }
        if args.emit_llvm {
            let ll_path = format!("{out_dir}/{module_name}.ll");
            std::fs::write(Path::new(&ll_path), merged.print_to_string().to_string())
                .map_err(|e| anyhow::anyhow!("Failed to write {ll_path}: {e}"))?;
        }
        let path = format!("{out_dir}/{module_name}.o");
        codegen_llvm::emit_object(&merged, &machine, &path).map_err(|e| report_cg(k1, e))?;
        return Ok(vec![path]);
    }

    let mut paths: Vec<String> = Vec::with_capacity(unit_count);
    for i in 0..unit_count {
        paths.push(object_path(i));
    }
    if optimize_ir {
        let frame = k1.trace_push(TraceKind::Thinlto, unit_count as u32, 0);
        let linked = Cg::thinlto_codegen(k1, &artifacts, &paths);
        k1.trace_pop(frame);
        linked.map_err(|e| report_cg(k1, e))?;
    }
    Ok(paths)
}

/// Codegens and links one reloadable ns's dylib:
/// `.k1-out/<program>.<ns>.<dylib|so>` beside the executable
fn write_reload_dylib(
    args: &Args,
    ctx: &Context,
    k1: &mut TypedProgram,
    ns_id: NamespaceId,
) -> Result<()> {
    let ns_name = k1.ident_str(k1.namespaces.get(ns_id).name).to_string();
    let module_name = k1.program_name().to_string();
    let platform = k1.config.target.platform();
    let unit_name = format!("{module_name}.{ns_name}");

    let prepare_frame = k1.trace_push(TraceKind::CodegenPrepare, ns_id.as_u32(), 0);
    let prepared = Cg::prepare_dylib(k1, ns_id);
    if let Ok(roots) = &prepared {
        k1.trace.set_top_count(roots.reachable.len() as u64);
    }
    k1.trace_pop(prepare_frame);
    let roots = match prepared {
        Ok(roots) => roots,
        Err(e) => anyhow::bail!(report_codegen_error(k1, e)),
    };
    let objects =
        write_unit_artifacts(args, ctx, k1, &roots, CgKind::ReloadDylib(ns_id), &unit_name, false)?;
    let k1: &TypedProgram = k1;

    let out_dir = k1.ast.idents.get_string(k1.config.out_dir);
    let dylib_ext = platform.dylib_ext();
    let dylib_path = format!("{out_dir}/{unit_name}.{dylib_ext}");
    let mut link_cmd = std::process::Command::new("cc");
    match platform {
        Platform::PosixMacos => {
            link_cmd.arg(format!("-mmacosx-version-min={}", MAC_SDK_VERSION));
            link_cmd.arg("-dynamiclib");
            link_cmd.arg("-undefined").arg("dynamic_lookup");
        }
        Platform::PosixLinux => {
            link_cmd.arg("-shared");
        }
        Platform::Wasi | Platform::Bare => unreachable!(),
    }
    // A running app watches this path; link to the side and rename into place so
    let staged_path = format!("{dylib_path}.staged");
    link_cmd.args(&objects);
    link_cmd.arg("-o");
    link_cmd.arg(&staged_path);
    log::debug!("Reload dylib link command: {:?}", link_cmd);
    if !command_status(&mut link_cmd)?.success() {
        let _ = std::fs::remove_file(&staged_path);
        bail!("linking reload dylib {dylib_path} failed");
    }
    std::fs::rename(&staged_path, &dylib_path)
        .map_err(|e| anyhow::anyhow!("Failed to publish reload dylib {dylib_path}: {e}"))?;
    Ok(())
}

// Eventually, we want to return output and exit code to the application
pub fn run_compiled_program(
    idents: &IdentPool,
    target: Target,
    out_dir: StringId,
    program_home_dir: StringId,
    module_name: &str,
    is_test: bool,
    program_args: &[String],
) -> Option<i32> {
    let exe_path = kpath::join_pathbuf(
        idents,
        out_dir,
        format_args!("{}{}", module_name, if is_test { "_test" } else { "" }),
    );
    let mut run_cmd = if target.platform() == Platform::Wasi {
        let mut cmd = std::process::Command::new("wasmtime");
        cmd.args(["run", "-W", "memory64"]);
        cmd.arg(exe_path);
        cmd
    } else {
        std::process::Command::new(exe_path)
    };
    run_cmd.args(program_args);
    run_cmd.current_dir(idents.get_string(program_home_dir));
    log::debug!("Run Command: {:?}", run_cmd);
    let run_status = match run_cmd.status() {
        Ok(status) => status,
        Err(e) => {
            error!("failed to run `{}`: {e}", run_cmd.get_program().to_string_lossy());
            return None;
        }
    };

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

#[cfg(test)]
mod compiler_test {
    use super::*;

    #[test]
    fn disk_cache_restores_longest_valid_prefix() {
        static SET_HOME: std::sync::Once = std::sync::Once::new();
        SET_HOME.call_once(|| unsafe {
            std::env::set_var("K1_HOME", env!("CARGO_MANIFEST_DIR"));
        });
        let dir = std::env::temp_dir().join(format!("k1_disk_cache_test_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        let app = dir.join("app.k1");
        fs::write(&app, "fn main(): i32 { 0 }\n").unwrap();
        let args = Args {
            no_std: true,
            emit_llvm: false,
            optimize: false,
            dump_module: false,
            dump_idents: false,
            dump_trace: false,
            debug: false,
            sanitize: false,
            filc: false,
            profile: false,
            chatty: false,
            optimize_ir: true,
            cache: true,
            target: None,
            k1_home_override: None,
            command: Command::Check { file: Some(app.clone()) },
        };

        let cold = compile_program(&args).ok().expect("cold compile must succeed");
        assert_eq!(cold.restored_module_count, 0, "first compile has nothing to restore");

        let warm = compile_program(&args).ok().expect("warm compile must succeed");
        assert_eq!(warm.restored_module_count, 2, "unchanged input restores core, app");

        fs::write(&app, "fn main(): i32 {\n  println(\"v2\")\n  0\n}\n").unwrap();
        let edited = compile_program(&args).ok().expect("edited compile must succeed");
        assert_eq!(edited.restored_module_count, 1, "an edited app restores only core");

        let _ = fs::remove_dir_all(&dir);
    }
}
