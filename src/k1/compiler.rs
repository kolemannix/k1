// Copyright (c) 2026 knix
// All rights reserved.

use std::fs;
use std::fs::File;
use std::io::{IsTerminal, Write};
use std::os::unix::prelude::ExitStatusExt;
use std::path::Path;

use crate::kmem::{self, MStr, Mem};
use crate::lex::SpanId;
use crate::parse::{IdentPool, StringId, write_source_location};
use crate::typer::{LibRefLinkType, Linkage, MemTmp, MessageLevel, NamespaceId, TypedProgram};
use crate::{kpath, typer};
use anyhow::{Result, bail};
use inkwell::context::Context;
use log::{error, info};

use crate::codegen_llvm::{Cg, CgUnit};

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

    pub fn dylib_ext(&self) -> &'static str {
        match self {
            TargetOs::Linux => "so",
            TargetOs::MacOs => "dylib",
            TargetOs::Wasm => unreachable!("no dylibs on wasm"),
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
    pub fn to_str(&self) -> &'static str {
        match self {
            Target::LinuxIntel64 => "linux-intel64",
            Target::MacOsArm64 => "macos-arm64",
            Target::Wasm64 => "wasm64",
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

fn logical_name_to_lib_filename(
    idents: &IdentPool,
    mem: &mut Mem<MemTmp>,
    module_libs_dir: &str,
    target_os: TargetOs,
    link_type: LibRefLinkType,
    logical_name: &str,
) -> MStr<MemTmp> {
    match (target_os, link_type) {
        (TargetOs::Linux, LibRefLinkType::Static) => {
            kpath::join_tmp(mem, idents, module_libs_dir, format_args!("lib{logical_name}.a"))
        }
        (TargetOs::Linux, LibRefLinkType::Dynamic) => {
            kpath::join_tmp(mem, idents, module_libs_dir, format_args!("lib{logical_name}.so"))
        }
        (TargetOs::Linux, LibRefLinkType::Default) => mem.push_str(logical_name),
        (TargetOs::MacOs, LibRefLinkType::Static) => {
            kpath::join_tmp(mem, idents, module_libs_dir, format_args!("lib{logical_name}.a"))
        }
        (TargetOs::MacOs, LibRefLinkType::Dynamic) => {
            kpath::join_tmp(mem, idents, module_libs_dir, format_args!("lib{logical_name}.dylib"))
        }
        (TargetOs::MacOs, LibRefLinkType::Default) => mem.push_str(logical_name),
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
    /// Run a module's setup step (setup.k1) if stale
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

    /// Disk-cached compiles
    #[arg(long, default_value_t = true, action = clap::ArgAction::Set)]
    pub cache: bool,

    /// Target platform
    #[arg(long)]
    pub target: Option<Target>,

    /// Internal: this compile is a module's setup.k1 program; see
    /// SetupMode::SetupProgram
    #[arg(skip)]
    pub is_setup_program: bool,

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SetupMode {
    /// Run stale setup steps at module load (default)
    Normal,
    /// This compile IS a module's setup.k1 program: setup steps never run
    /// inside one, so setup trees can't recurse
    SetupProgram,
    /// `k1 setup`: run setup function during load, then stop before the primary
    /// tree typechecks; force runs even if fresh
    SetupOnly { force: bool },
}

impl SetupMode {
    pub fn is_setup_only(&self) -> bool {
        matches!(self, SetupMode::SetupOnly { .. })
    }
}

/// All paths are canonical UTF-8 strings interned in the ident pool; see kpath
#[derive(Debug, Clone, Copy)]
pub struct CompilerConfig {
    pub src_path: StringId,
    pub home_dir: StringId,
    pub k1_home: StringId,
    pub is_test_build: bool,
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
    pub chatty: bool,
    pub optimize_ir: bool,
    pub cache: bool,
    pub setup_mode: SetupMode,
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
    if is_dir { (true, src_path.to_string()) } else { (false, kpath::parent(src_path).to_string()) }
}

/// Given a .k1 source file, the src_path to compile: its directory when that
/// directory is a module dir else the file itself
pub fn find_check_target_for_file(file: &Path) -> anyhow::Result<String> {
    let file = kpath::canonicalize(file)?;
    if kpath::file_name(&file) == "setup.k1" {
        return Ok(file);
    }
    let dir = kpath::parent(&file);
    let dir_name = kpath::file_name(dir);
    let has_root = Path::new(dir).join("module.k1").is_file()
        || Path::new(dir).join(format!("{dir_name}.k1")).is_file();
    if has_root { Ok(dir.to_string()) } else { Ok(file) }
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

pub(crate) fn pathbuf_into_string(p: PathBuf) -> String {
    p.into_os_string().into_string().unwrap()
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
        idents: &IdentPool,
        src_path: &Path,
        core_module: bool,
        source_overrides: &fxhash::FxHashMap<String, String>,
    ) -> anyhow::Result<ModuleRootHandle> {
        let src_path = kpath::canonicalize(src_path)
            .map_err(|e| anyhow::anyhow!("Error loading module '{}': {e}", src_path.display()))?;
        let (is_dir, module_dir) = module_home_from_src_path(&src_path);
        let root_path = if !is_dir {
            src_path.clone()
        } else if core_module {
            let builtin = kpath::join_buf(idents, module_dir.as_str(), "builtin.k1");
            if !builtin.is_file() {
                bail!("corelib module must contain builtin.k1");
            }
            pathbuf_into_string(builtin)
        } else {
            let module_name = kpath::file_name(&src_path);
            let module_root = kpath::join_buf(idents, module_dir.as_str(), "module.k1");
            let named_root =
                kpath::join_buf(idents, module_dir.as_str(), format_args!("{module_name}.k1"));
            if module_root.is_file() {
                pathbuf_into_string(module_root)
            } else if named_root.is_file() {
                pathbuf_into_string(named_root)
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

pub fn collect_module_source_paths(
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

            // The root is parsed separately; setup.k1 is a standalone program
            if path_string != root_path && kpath::file_name(&path_string) != "setup.k1" {
                files.push(path_string);
            }
        }
    }
    files.sort();
    Ok(files)
}

pub struct ModuleRemainingSources {
    module_dir: String,
    root_path: String,
    is_dir: bool,
}

impl ModuleRemainingSources {
    pub fn new(module_dir: String, root_path: String, is_dir: bool) -> ModuleRemainingSources {
        ModuleRemainingSources { module_dir, root_path, is_dir }
    }

    pub fn is_dir(&self) -> bool {
        self.is_dir
    }

    pub fn spawn_read(
        self,
        source_overrides: &fxhash::FxHashMap<String, String>,
    ) -> ModuleRemainingSourcesHandle {
        let overrides = source_overrides.clone();
        let reader = std::thread::spawn(move || {
            if !self.is_dir {
                return Ok(vec![]);
            }
            let paths = collect_module_source_paths(&self.module_dir, &self.root_path)?;
            let mut sources = Vec::with_capacity(paths.len());
            for path in paths {
                let override_content = overrides.get(&path).cloned();
                sources.push(read_source_file(path, override_content)?);
            }
            Ok(sources)
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

pub struct SetupRequest<'a> {
    pub idents: &'a IdentPool,
    pub module_dir: StringId,
    pub module_name: StringId,
    pub outputs: &'a [StringId],
    pub inputs: &'a [StringId],
    pub target: Target,
    pub k1_home: StringId,
    pub force: bool,
    pub chatty: bool,
}

pub fn run_setup_function(req: &SetupRequest) -> Result<()> {
    let mut scratch: Mem<()> = Mem::make();
    let module_name = req.idents.get_string(req.module_name);
    let setup_k1_path = kpath::join_tmp(&mut scratch, req.idents, req.module_dir, "setup.k1");
    let setup_src = fs::read_to_string(Path::new(setup_k1_path.as_str())).map_err(|e| {
        anyhow::anyhow!(
            "module '{module_name}' declares setup but {setup_k1_path} could not be read: {e}"
        )
    })?;

    let fingerprint = setup_fingerprint(req, &mut scratch, &setup_src)?;
    let setup_out_dir =
        kpath::join_tmp(&mut scratch, req.idents, req.module_dir, (".k1-out", "setup"));
    let stamp_path = kpath::join_tmp(&mut scratch, req.idents, setup_out_dir.as_str(), "stamp");

    if !req.force && setup_is_fresh(req, &mut scratch, stamp_path.as_str(), &fingerprint) {
        return Ok(());
    }

    fs::create_dir_all(Path::new(setup_out_dir.as_str()))?;
    let lock_path = kpath::join_tmp(&mut scratch, req.idents, setup_out_dir.as_str(), "lock");
    let _lock = SetupLock::acquire(lock_path.as_str())?;
    // Another process may have completed this setup while we waited on the lock
    if !req.force && setup_is_fresh(req, &mut scratch, stamp_path.as_str(), &fingerprint) {
        return Ok(());
    }

    eprintln!("Setting up module '{module_name}' (running {setup_k1_path})...");
    // Clean slate: outputs must be produced by this run, not inherited from a
    // previous one, and a failed or partial run must read as stale
    let _ = fs::remove_file(Path::new(stamp_path.as_str()));
    for output in req.outputs {
        let output_path = kpath::join_tmp(&mut scratch, req.idents, req.module_dir, *output);
        let p = Path::new(output_path.as_str());
        if p.is_dir() {
            fs::remove_dir_all(p)?;
        } else if p.exists() {
            fs::remove_file(p)?;
        }
    }
    run_setup_program(req, setup_k1_path.as_str())?;

    let outputs = output_manifest(req, &mut scratch)
        .map_err(|e| anyhow::anyhow!("setup.k1 for module '{module_name}' completed but {e}"))?;
    fs::write(Path::new(stamp_path.as_str()), format!("{fingerprint}{outputs}"))?;
    Ok(())
}

fn setup_is_fresh(
    req: &SetupRequest,
    scratch: &mut Mem<()>,
    stamp_path: &str,
    fingerprint: &str,
) -> bool {
    let Ok(existing) = fs::read_to_string(Path::new(stamp_path)) else {
        return false;
    };
    let Ok(outputs) = output_manifest(req, scratch) else {
        return false;
    };
    existing == format!("{fingerprint}{outputs}")
}

/// The output half of the stamp: every file under every declared output, hashed
fn output_manifest(req: &SetupRequest, scratch: &mut Mem<()>) -> Result<String> {
    use std::fmt::Write;
    let module_dir = req.idents.get_string(req.module_dir);
    let mut s = String::new();
    for output in req.outputs {
        let output_path = kpath::join_tmp(scratch, req.idents, req.module_dir, *output);
        if !Path::new(output_path.as_str()).exists() {
            bail!("did not produce declared output '{}'", req.idents.get_string(*output));
        }
        let mut matched: Vec<String> = vec![];
        collect_input_output_files(output_path.as_str(), &mut matched)?;
        matched.sort();
        for file in &matched {
            let content = fs::read(Path::new(file))
                .map_err(|e| anyhow::anyhow!("failed to read setup output {file}: {e}"))?;
            let rel = file.strip_prefix(module_dir).unwrap_or(file).trim_start_matches('/');
            writeln!(s, "output-file: {} {:016x}", rel, content_hash64(&content)).unwrap();
        }
    }
    Ok(s)
}

fn setup_fingerprint(req: &SetupRequest, scratch: &mut Mem<()>, setup_src: &str) -> Result<String> {
    use std::fmt::Write;
    let module_dir = req.idents.get_string(req.module_dir);
    let get_strings = |ids: &[StringId]| {
        let mut strings = Vec::with_capacity(ids.len());
        for id in ids {
            strings.push(req.idents.get_string(*id));
        }
        strings
    };
    let mut s = String::new();
    writeln!(s, "k1-setup-stamp v2").unwrap();
    writeln!(s, "target: {}", req.target.to_str()).unwrap();
    writeln!(s, "setup.k1: {:016x}", content_hash64(setup_src.as_bytes())).unwrap();
    writeln!(s, "outputs: {}", get_strings(req.outputs).join("|")).unwrap();
    writeln!(s, "inputs: {}", get_strings(req.inputs).join("|")).unwrap();
    let mut output_paths: Vec<MStr<()>> = Vec::with_capacity(req.outputs.len());
    for o in req.outputs {
        output_paths.push(kpath::join_tmp(scratch, req.idents, req.module_dir, *o));
    }
    for input in req.inputs {
        let input_path = kpath::join_tmp(scratch, req.idents, req.module_dir, *input);
        let mut matched: Vec<String> = vec![];
        collect_input_output_files(input_path.as_str(), &mut matched)?;
        matched.sort();
        for file in &matched {
            if output_paths.iter().any(|o| o.as_str() == file) {
                continue;
            }
            let content = fs::read(Path::new(file))
                .map_err(|e| anyhow::anyhow!("failed to read setup input {file}: {e}"))?;
            let rel = file.strip_prefix(module_dir).unwrap_or(file).trim_start_matches('/');
            writeln!(s, "input-file: {} {:016x}", rel, content_hash64(&content)).unwrap();
        }
    }
    Ok(s)
}

fn collect_input_output_files(path: &str, out: &mut Vec<String>) -> Result<()> {
    let p = Path::new(path);
    if p.is_file() {
        out.push(path.to_string());
    } else if p.is_dir() {
        for entry in fs::read_dir(p)? {
            let child = entry?.path().into_os_string().into_string().map_err(|s| {
                anyhow::anyhow!("setup path is not valid UTF-8: {}", s.to_string_lossy())
            })?;
            collect_input_output_files(&child, out)?;
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

fn run_setup_program(req: &SetupRequest, setup_k1_path: &str) -> Result<()> {
    let module_dir = req.idents.get_string(req.module_dir);
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
        chatty: req.chatty,
        optimize_ir: true,
        cache: true,
        target: Some(req.target),
        is_setup_program: true,
        k1_home_override: Some(req.idents.get_string(req.k1_home).to_string()),
        command: Command::Check { file: Some(PathBuf::from(setup_k1_path)) },
    };
    let _cwd = CwdGuard::enter(module_dir);
    let mut program = match compile_program(&args) {
        Ok(p) => p,
        Err(CompileProgramError::TyperFailure(_)) => {
            bail!("setup.k1 failed to compile (errors above)")
        }
    };
    program.run_setup_entry(module_dir)
}

fn module_list_filename(config: &CompilerConfig) -> &'static str {
    match config.setup_mode {
        SetupMode::SetupProgram => "modules-setup",
        _ => "modules",
    }
}

struct ListedModule {
    name: String,
    home_dir: String,
    root_filename: String,
    is_dir: bool,
    setup: Option<(Vec<String>, Vec<String>)>,
}

fn read_module_list(cache_dir: &Path, config: &CompilerConfig) -> Option<Vec<ListedModule>> {
    let text = crate::snap::cache_load_text(cache_dir, module_list_filename(config))?;
    let mut lines = text.lines();
    if lines.next()? != format!("k1-modules v{}", crate::BUILD_ID) {
        return None;
    }
    let mut modules: Vec<ListedModule> = vec![];
    for line in lines {
        if let Some(rest) = line.strip_prefix("module\t") {
            let mut parts = rest.split('\t');
            let name = parts.next()?.to_string();
            let is_dir = parts.next()? == "1";
            let home_dir = parts.next()?.to_string();
            let root_filename = parts.next()?.to_string();
            modules.push(ListedModule { name, home_dir, root_filename, is_dir, setup: None });
        } else if let Some(rest) = line.strip_prefix("setup\t") {
            let mut parts = rest.split('\t');
            let n_out: usize = parts.next()?.parse().ok()?;
            let mut outs: Vec<String> = vec![];
            for part in parts {
                outs.push(part.to_string());
            }
            if outs.len() < n_out {
                return None;
            }
            let ins = outs.split_off(n_out);
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
        let root_source = k1.ast.sources.get(m.root_file_id(&k1.mem));
        writeln!(
            s,
            "module\t{}\t{}\t{}\t{}",
            k1.get_string(m.name),
            m.is_dir as u8,
            k1.get_string(m.home_dir),
            k1.get_string(root_source.filename),
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
    crate::snap::cache_store_text(k1.cache_dir(), module_list_filename(&k1.config), &s);
}

fn inputs_hashes_from_module_list(
    idents: &IdentPool,
    config: &CompilerConfig,
    overrides: &fxhash::FxHashMap<String, String>,
    modules: &[ListedModule],
) -> Vec<crate::snap::InputsHash> {
    let hash_file = |path: &str| -> Option<u64> {
        match overrides.get(path) {
            Some(content) => Some(content_hash64(content.as_bytes())),
            None => fs::read(Path::new(path)).ok().map(|b| content_hash64(&b)),
        }
    };
    let mut scratch: Mem<()> = Mem::make();
    let n = modules.len();
    let mut group_ends = vec![1.min(n)];
    if !config.no_std {
        group_ends.push(2.min(n));
    }
    group_ends.push(n);

    let mut hash = typer::snapshot::inputs_hash_from_settings(idents, config);
    let mut hashes = vec![];
    let mut start = 0;
    for end in group_ends {
        let group = &modules[start..end];
        start = end;
        for m in group {
            let root_path = kpath::join_tmp(
                &mut scratch,
                idents,
                m.home_dir.as_str(),
                m.root_filename.as_str(),
            );
            let Some(root_hash) = hash_file(root_path.as_str()) else {
                // An unreadable root invalidates the whole group and everything after
                return hashes;
            };
            hash = hash.add_module_header(&m.name, &m.home_dir, &m.root_filename, root_hash);
        }
        for m in group {
            if config.setup_mode != SetupMode::SetupProgram
                && m.setup.as_ref().is_some_and(|(outputs, inputs)| {
                    !listed_setup_is_fresh(&mut scratch, idents, config, m, outputs, inputs)
                })
            {
                // If we're running setup, give up on getting a cache hit for this module this time
                // around
                return hashes;
            }
            let mut sources: kmem::List<(String, u64), _> = scratch.new_list(128);
            if m.is_dir {
                let root_path = kpath::join_tmp(
                    &mut scratch,
                    idents,
                    m.home_dir.as_str(),
                    m.root_filename.as_str(),
                );
                let Ok(files) = collect_module_source_paths(&m.home_dir, root_path.as_str()) else {
                    return hashes;
                };
                for path in files {
                    let Some(h) = hash_file(&path) else { return hashes };
                    sources.push((path, h));
                }
            }
            hash = hash.add_module_sources(&m.name, sources.iter().map(|(p, h)| (p.as_str(), *h)));
            hashes.push(hash);
        }
    }
    hashes
}

fn listed_setup_is_fresh(
    scratch: &mut Mem<()>,
    idents: &IdentPool,
    config: &CompilerConfig,
    m: &ListedModule,
    outputs: &[String],
    inputs: &[String],
) -> bool {
    let module_dir = idents.intern(&m.home_dir);
    let setup_k1_path = kpath::join_tmp(scratch, idents, module_dir, "setup.k1");
    let Ok(setup_src) = fs::read_to_string(Path::new(setup_k1_path.as_str())) else {
        return false;
    };
    let outputs: Vec<StringId> = {
        let mut ids = Vec::with_capacity(outputs.len());
        for s in outputs {
            ids.push(idents.intern(s));
        }
        ids
    };
    let inputs: Vec<StringId> = {
        let mut ids = Vec::with_capacity(inputs.len());
        for s in inputs {
            ids.push(idents.intern(s));
        }
        ids
    };
    let req = SetupRequest {
        idents,
        module_dir,
        module_name: idents.intern(&m.name),
        outputs: &outputs,
        inputs: &inputs,
        target: config.target,
        k1_home: config.k1_home,
        force: false,
        chatty: false,
    };
    let Ok(fingerprint) = setup_fingerprint(&req, scratch, &setup_src) else {
        return false;
    };
    let setup_out_dir = kpath::join_tmp(scratch, idents, module_dir, (".k1-out", "setup"));
    let stamp_path = kpath::join_tmp(scratch, idents, setup_out_dir.as_str(), "stamp");
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
    let start_time = std::time::Instant::now();

    let src_path = (match args.file() {
        None => kpath::canonicalize("."),
        Some(path_buf) => kpath::canonicalize(path_buf),
    })
    .unwrap_or_else(|e| panic!("Failed to load source path: {e}"));

    let (is_dir, home_dir) = module_home_from_src_path(&src_path);
    let module_name =
        if is_dir { kpath::file_name(&src_path) } else { kpath::file_stem(&src_path) };
    let mut ast = crate::parse::ParsedProgram::make(module_name.to_string());

    let src_path_id = ast.idents.intern(&src_path);
    let home_dir_id = ast.idents.intern(&home_dir);
    let out_dir = kpath::join_id(&ast.idents, &mut ast.mem, home_dir.as_str(), ".k1-out");
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
            target == Target::LinuxIntel64,
            "--filc requires target linux-intel64; Fil-C only supports Linux/x86_64"
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
    let k1_home = kpath::canonicalize(&k1_home_raw)
        .unwrap_or_else(|e| panic!("K1 home {} is not usable: {e}", k1_home_raw.display()));
    if args.chatty {
        eprintln!("using k1 home: {k1_home}");
    }
    let k1_home_id = ast.idents.intern(&k1_home);
    let corelib_dir = kpath::join_buf(&ast.idents, k1_home.as_str(), ("modules", "core"));
    let stdlib_dir = kpath::join_buf(&ast.idents, k1_home.as_str(), ("modules", "std"));

    // All planned paths are absolute, so spawning before the CwdGuard chdir is safe;
    // reads overlap the rest of TypedProgram::new (scope/VM init) and core's typecheck.
    let core_plan = ModuleRootHandle::spawn(&ast.idents, &corelib_dir, true, &lsp.source_overrides);
    let std_plan = use_std
        .then(|| ModuleRootHandle::spawn(&ast.idents, &stdlib_dir, false, &lsp.source_overrides));
    let main_plan =
        ModuleRootHandle::spawn(&ast.idents, Path::new(&src_path), false, &lsp.source_overrides);

    let config = CompilerConfig {
        src_path: src_path_id,
        home_dir: home_dir_id,
        k1_home: k1_home_id,
        is_test_build: args.command.is_test(),
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
        chatty: args.chatty,
        optimize_ir: args.optimize_ir,
        cache: args.cache,
        setup_mode: if args.is_setup_program {
            SetupMode::SetupProgram
        } else if let Command::Setup { force, .. } = args.command {
            SetupMode::SetupOnly { force }
        } else {
            SetupMode::Normal
        },
    };

    let _cwd = CwdGuard::enter(&home_dir);

    let mut k1 = 'program: {
        let cache_dir = Path::new(ast.idents.get_string(cache_dir));
        if args.cache
            && let Some(modules) = read_module_list(cache_dir, &config)
        {
            let input_hashes_by_module = inputs_hashes_from_module_list(
                &ast.idents,
                &config,
                &lsp.source_overrides,
                &modules,
            );

            // The snapshot with the most modules in it is at the end
            for (i, hash) in input_hashes_by_module.iter().enumerate().rev() {
                // If we have a hit, the file exists by this name, and we restore
                let Some(bytes) = crate::snap::cache_load(cache_dir, *hash) else { continue };
                let module_count = i as u32 + 1;
                match TypedProgram::restore(&bytes, config, lsp.clone()) {
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
    k1.join_cache_writes();
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
                k1.config.target.target_os(),
                lib.link_type,
                &logical_name,
            );
            libs.push((lib.link_type, filename.as_str().to_string()));
        }
        out.push(ModuleLibs { libs_dir: module_libs_dir.as_str().to_string(), link_args, libs });
    }
    out
}

pub fn write_linked_output(
    k1: &TypedProgram,
    module_name: &str,
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
        kpath::join_tmp(k1.get_tmp_unsafe(), idents, out_dir, format_args!("{module_name}.ll"))
    } else {
        kpath::join_tmp(k1.get_tmp_unsafe(), idents, out_dir, format_args!("{module_name}.o"))
    };
    let out_name = match kind {
        LinkOutputKind::Executable => {
            kpath::join_tmp(k1.get_tmp_unsafe(), idents, out_dir, module_name)
        }
        LinkOutputKind::Dylib => kpath::join_tmp(
            k1.get_tmp_unsafe(),
            idents,
            out_dir,
            format_args!("lib{module_name}.{}", target.target_os().dylib_ext()),
        ),
    };
    if kind == LinkOutputKind::Dylib {
        match target.target_os() {
            TargetOs::MacOs => {
                build_cmd.arg("-dynamiclib");
                build_cmd.arg(format!(
                    "-Wl,-install_name,@rpath/lib{module_name}.{}",
                    target.target_os().dylib_ext()
                ));
                build_cmd.arg(format!(
                    "-Wl,-exported_symbols_list,{}",
                    build_export_list_file_path(k1, module_name)
                ));
            }
            TargetOs::Linux => {
                build_cmd.arg("-shared");
                build_cmd.arg(format!(
                    "-Wl,--version-script={}",
                    build_version_script_file_path(k1, module_name)
                ));
            }
            TargetOs::Wasm => bail!("dylib output is not supported on wasm"),
        }
    }

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
    build_cmd.arg(object_name.as_str());

    // Linking with libraries.
    // For each module, for each of its libraries, link with it as specified by the link_type
    for module_libs in collect_all_module_libs(k1) {
        if !module_libs.libs.is_empty() {
            build_cmd.arg(format!("-L{}", module_libs.libs_dir));
        }
        for link_arg in &module_libs.link_args {
            build_cmd.arg(link_arg);
        }
        for (link_type, filename) in &module_libs.libs {
            match link_type {
                // Link via linker arg, since the name has no extension
                LibRefLinkType::Default => build_cmd.arg(format!("-l{filename}")),
                // 'Link' via direct clang arg, since its an exact filepath
                _ => build_cmd.arg(filename),
            };
        }
    }

    build_cmd.args(extra_options);

    build_cmd.arg("-o");
    build_cmd.arg(out_name.as_str());

    log::debug!("Build Command: {:?}", build_cmd);
    let build_status = build_cmd.status()?;

    if !build_status.success() {
        eprintln!("Build failed!");
        bail!("linking {out_name} with clang failed");
    }

    let elapsed = clang_time.elapsed();
    if k1.config.chatty {
        eprintln!("link {out_name} took {}ms", elapsed.as_millis());
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

    match k1.config.target.target_os() {
        TargetOs::MacOs => {
            let mut list = String::with_capacity(symbols.len() * 24);
            for s in &symbols {
                list.push('_');
                list.push_str(s);
                list.push('\n');
            }
            std::fs::write(build_export_list_file_path(k1, module_name), list)?;
        }
        TargetOs::Linux => {
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
            std::fs::write(build_export_list_file_path(k1, module_name), list)?;
            std::fs::write(build_version_script_file_path(k1, module_name), script)?;
        }
        TargetOs::Wasm => bail!("library output is not supported on wasm"),
    }
    Ok(())
}

pub fn write_library_archive(k1: &TypedProgram, module_name: &str) -> Result<()> {
    let target = k1.config.target;
    let idents = &k1.ast.idents;
    let out_dir = k1.config.out_dir;
    let ar_time = std::time::Instant::now();

    let object_name =
        kpath::join_tmp(k1.get_tmp_unsafe(), idents, out_dir, format_args!("{module_name}.o"));
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
    ld_cmd.arg(object_name.as_str());
    for lib in &static_libs {
        ld_cmd.arg(lib);
    }
    match target.target_os() {
        TargetOs::MacOs => {
            ld_cmd.arg("-exported_symbols_list");
            ld_cmd.arg(build_export_list_file_path(k1, module_name));
        }
        TargetOs::Linux => {}
        TargetOs::Wasm => bail!("static library output is not supported on wasm"),
    }
    ld_cmd.arg("-o");
    ld_cmd.arg(combined_name.as_str());
    log::debug!("Partial link Command: {:?}", ld_cmd);
    if !ld_cmd.status()?.success() {
        bail!("partial link of {combined_name} failed");
    }

    if target.target_os() == TargetOs::Linux {
        let mut objcopy_cmd = std::process::Command::new("objcopy");
        objcopy_cmd
            .arg(format!("--keep-global-symbols={}", build_export_list_file_path(k1, module_name)));
        objcopy_cmd.arg(combined_name.as_str());
        log::debug!("Localize Command: {:?}", objcopy_cmd);
        if !objcopy_cmd.status()?.success() {
            bail!("objcopy localize of {combined_name} failed");
        }
    }

    let _ = std::fs::remove_file(archive_name.as_str());
    let mut ar_cmd = std::process::Command::new("ar");
    ar_cmd.arg("rcs");
    ar_cmd.arg(archive_name.as_str());
    ar_cmd.arg(combined_name.as_str());
    log::debug!("Archive Command: {:?}", ar_cmd);
    if !ar_cmd.status()?.success() {
        bail!("archiving {archive_name} failed");
    }

    if k1.config.chatty {
        eprintln!("archive {archive_name} took {}ms", ar_time.elapsed().as_millis());
    }
    Ok(())
}

pub fn codegen_module<'ctx, 'module>(
    args: &Args,
    ctx: &'ctx Context,
    k1: &'module mut TypedProgram,
) -> Result<Cg<'ctx, 'module>> {
    let codegen_start = std::time::Instant::now();

    let mut reload_nss: Vec<NamespaceId> = vec![];
    for (_, function) in k1.function_iter() {
        if function.is_reloadable && !reload_nss.contains(&function.namespace_id) {
            reload_nss.push(function.namespace_id);
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
        write_reload_dylib(args, ctx, k1, *ns_id)?;
    }

    let mut codegen = Cg::create(ctx, k1, args.debug, args.optimize, CgUnit::Host);
    let mut module_name = codegen.name().to_string();
    if args.command.is_test() {
        module_name.push_str("_test");
    };
    let out_dir = codegen.k1.config.out_dir;

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
        let ll_path = kpath::join_tmp(
            codegen.k1.get_tmp_unsafe(),
            &codegen.k1.ast.idents,
            out_dir,
            format_args!("{module_name}.ll"),
        );
        std::fs::write(Path::new(ll_path.as_str()), llvm_text)
            .map_err(|e| anyhow::anyhow!("Failed to write {ll_path}: {e}"))?;
    } else {
        if args.emit_llvm {
            let llvm_text = codegen.emit_llvm_ir_text();
            let ll_path = kpath::join_tmp(
                codegen.k1.get_tmp_unsafe(),
                &codegen.k1.ast.idents,
                out_dir,
                format_args!("{module_name}.ll"),
            );
            let mut f = File::create(ll_path.as_str()).expect("Failed to create .ll file");
            f.write_all(llvm_text.as_bytes()).unwrap();
        }

        let path = kpath::join_tmp(
            codegen.k1.get_tmp_unsafe(),
            &codegen.k1.ast.idents,
            out_dir,
            format_args!("{module_name}.o"),
        );
        if codegen.emit_object_file(path.as_str()).is_err() {
            bail!("Error writing object file to path: {path}");
        }
    }

    if codegen.k1.program_settings.executable {
        let mut link_options: Vec<String> = vec![];
        if !reload_nss.is_empty() {
            // Ensure host globals are visible to dlopen'd reload dylibs
            let export_flag = match codegen.k1.config.target.target_os() {
                TargetOs::MacOs => "-Wl,-export_dynamic",
                TargetOs::Linux => "-rdynamic",
                TargetOs::Wasm => bail!("ns(reload) is not supported on wasm"),
            };
            link_options.push(export_flag.to_string());
        }
        write_linked_output(codegen.k1, &module_name, &link_options, LinkOutputKind::Executable)?;
    } else {
        write_library_export_files(codegen.k1, &module_name)?;
        write_linked_output(codegen.k1, &module_name, &[], LinkOutputKind::Dylib)?;
        write_library_archive(codegen.k1, &module_name)?;
    }

    Ok(codegen)
}

/// Codegens and links one reloadable ns's dylib:
/// `.k1-out/<program>.<ns>.<dylib|so>` beside the executable
fn write_reload_dylib(
    args: &Args,
    ctx: &Context,
    k1: &mut TypedProgram,
    ns_id: NamespaceId,
) -> Result<()> {
    let dylib_start = std::time::Instant::now();
    let ns_name = k1.ident_str(k1.namespaces.get(ns_id).name).to_string();
    let module_name = k1.ast.name.clone();
    let target_os = k1.config.target.target_os();
    let out_dir = k1.config.out_dir;

    let mut cg = Cg::create(ctx, k1, args.debug, args.optimize, CgUnit::ReloadDylib(ns_id));
    if let Err(e) = cg.codegen_reload_dylib() {
        let use_color = std::io::stderr().is_terminal();
        write_source_location(
            &mut std::io::stderr(),
            &cg.k1.ast,
            e.span,
            MessageLevel::Error,
            6,
            Some(cg.k1.ident_str(e.message)),
            use_color,
        )
        .unwrap();
        anyhow::bail!(cg.k1.message_to_anyhow(e))
    }
    let optimize_ir = args.optimize && !cg.k1.config.filc;
    cg.optimize_verify(optimize_ir)?;

    let idents = &cg.k1.ast.idents;
    let unit_name = format!("{module_name}.{ns_name}");
    if args.emit_llvm {
        let ll_path = kpath::join_tmp(
            cg.k1.get_tmp_unsafe(),
            idents,
            out_dir,
            format_args!("{unit_name}.ll"),
        );
        std::fs::write(Path::new(ll_path.as_str()), cg.emit_llvm_ir_text())
            .map_err(|e| anyhow::anyhow!("Failed to write {ll_path}: {e}"))?;
    }
    let obj_path =
        kpath::join_tmp(cg.k1.get_tmp_unsafe(), idents, out_dir, format_args!("{unit_name}.o"));
    if cg.emit_object_file(obj_path.as_str()).is_err() {
        bail!("Error writing dylib object file to path: {obj_path}");
    }

    let dylib_ext = target_os.dylib_ext();
    let dylib_path = kpath::join_tmp(
        cg.k1.get_tmp_unsafe(),
        idents,
        out_dir,
        format_args!("{unit_name}.{dylib_ext}"),
    );
    let mut link_cmd = std::process::Command::new("cc");
    match target_os {
        TargetOs::MacOs => {
            link_cmd.arg("-dynamiclib");
            link_cmd.arg("-undefined").arg("dynamic_lookup");
        }
        TargetOs::Linux => {
            link_cmd.arg("-shared");
        }
        TargetOs::Wasm => unreachable!(),
    }
    link_cmd.arg(obj_path.as_str());
    link_cmd.arg("-o");
    link_cmd.arg(dylib_path.as_str());
    log::debug!("Reload dylib link command: {:?}", link_cmd);
    if !link_cmd.status()?.success() {
        bail!("linking reload dylib {dylib_path} failed");
    }
    if args.chatty {
        eprintln!("reload dylib {unit_name} took {}ms", dylib_start.elapsed().as_millis());
    }
    Ok(())
}

// Eventually, we want to return output and exit code to the application
pub fn run_compiled_program(
    idents: &IdentPool,
    out_dir: StringId,
    program_home_dir: StringId,
    module_name: &str,
    is_test: bool,
    program_args: &[String],
) -> Option<i32> {
    let exe_path = kpath::join_buf(
        idents,
        out_dir,
        format_args!("{}{}", module_name, if is_test { "_test" } else { "" }),
    );
    let mut run_cmd = std::process::Command::new(exe_path);
    run_cmd.args(program_args);
    run_cmd.current_dir(idents.get_string(program_home_dir));
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
        let dep_file = kpath::canonicalize(
            Path::new(root).join("test_src/dep_diamond_test/deps/shared/shared.k1"),
        )
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
            cache: false,
            target: None,
            is_setup_program: false,
            k1_home_override: None,
            command: Command::Check {
                file: Some(PathBuf::from(root).join("test_src/dep_diamond_test")),
            },
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
        fs::write(&app, "fn main(): i32 {\n  println(\"v1\")\n  0\n}\n").unwrap();
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
            cache: true,
            target: None,
            is_setup_program: false,
            k1_home_override: None,
            command: Command::Check { file: Some(app.clone()) },
        };

        let cold = compile_program(&args).ok().expect("cold compile must succeed");
        assert_eq!(cold.restored_module_count, 0, "first compile has nothing to restore");

        let warm = compile_program(&args).ok().expect("warm compile must succeed");
        assert_eq!(warm.restored_module_count, 3, "unchanged input restores core, std, app");

        fs::write(&app, "fn main(): i32 {\n  println(\"v2\")\n  0\n}\n").unwrap();
        let edited = compile_program(&args).ok().expect("edited compile must succeed");
        assert_eq!(edited.restored_module_count, 2, "an edited app restores only core and std");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn setup_program_runs() {
        static SET_HOME: std::sync::Once = std::sync::Once::new();
        SET_HOME.call_once(|| unsafe {
            std::env::set_var("K1_HOME", env!("CARGO_MANIFEST_DIR"));
        });
        let root = env!("CARGO_MANIFEST_DIR");
        let dir = std::env::temp_dir().join(format!("k1_setup_gate_test_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        let genlib = dir.join("deps/genlib");
        fs::create_dir_all(&genlib).unwrap();
        fs::write(
            dir.join("app.k1"),
            "fn module(): k1/module {\n  let m = k1/module/new()\n  m.dep(\"genlib\")\n  m\n}\n\
             fn main(): i32 {\n  assert(genlib/gen-value() == 42)\n  0\n}\n",
        )
        .unwrap();
        fs::write(
            genlib.join("genlib.k1"),
            "fn module(): k1/module {\n  let m = k1/module/new()\n  m.setup([\"gen.k1\"], [])\n  m\n}\n",
        )
        .unwrap();
        let setup_src = r#"fn setup(ctx: k1/setup-ctx) {
  core/files/write-entire-file("${ctx.module-dir}/gen.k1", "fn gen-value(): int { 42 }\n")
  core/files/write-entire-file("${ctx.module-dir}/witness.txt", "ran\n")
}
"#;
        fs::write(genlib.join("setup.k1"), setup_src).unwrap();

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
            cache: false,
            target: None,
            is_setup_program: false,
            k1_home_override: Some(root.to_string()),
            command: Command::Check { file: Some(dir.join("app.k1")) },
        };

        assert!(compile_program(&args).is_ok(), "first compile (setup runs) must succeed");
        let gen_path = genlib.join("gen.k1");
        let witness_path = genlib.join("witness.txt");
        let generated = fs::read_to_string(&gen_path).unwrap();

        fs::write(&witness_path, "sentinel\n").unwrap();
        assert!(compile_program(&args).is_ok(), "second compile (fresh) must succeed");
        assert_eq!(
            fs::read_to_string(&witness_path).unwrap(),
            "sentinel\n",
            "a fresh setup must not rerun setup"
        );

        // A hand-modified declared output means dirty: setup reruns, regenerating it
        fs::write(&gen_path, format!("{generated}// tampered\n")).unwrap();
        assert!(compile_program(&args).is_ok(), "third compile (dirty output) must succeed");
        assert_eq!(
            fs::read_to_string(&gen_path).unwrap(),
            generated,
            "a modified output must rerun setup"
        );

        // A deleted declared output likewise
        fs::remove_file(&gen_path).unwrap();
        assert!(compile_program(&args).is_ok(), "fourth compile (missing output) must succeed");
        assert_eq!(fs::read_to_string(&gen_path).unwrap(), generated);

        // Changing setup.k1 content busts the input fingerprint
        fs::write(&witness_path, "sentinel\n").unwrap();
        fs::write(genlib.join("setup.k1"), format!("{setup_src}// changed\n")).unwrap();
        assert!(compile_program(&args).is_ok(), "fifth compile (stale inputs) must succeed");
        assert_eq!(
            fs::read_to_string(&witness_path).unwrap(),
            "ran\n",
            "a changed setup.k1 must rerun setup"
        );

        let _ = fs::remove_dir_all(&dir);
    }
}
