// Copyright (c) 2026 knix
// All rights reserved.

use std::fs;
use std::fs::File;
use std::io::{IsTerminal, Read};
use std::os::unix::prelude::ExitStatusExt;
use std::path::Path;

use crate::kmem::{self, MStr, Mem};
use crate::parse::{IdentPool, Interner, StringId, write_source_location};
use crate::lex::SpanId;
use crate::typer::{
    K1Message, K1Result, LibRefLinkType, Linkage, MemTmp, MessageLevel, NamespaceId, TypedProgram,
};
use crate::{SV8, ir, kbail, kpath, typer};
use anyhow::{Result, bail};
use inkwell::context::Context;
use log::{error, info};

use crate::codegen_llvm::{
    self, Cg, CgError, CgKind, CodegenRoots, Pipeline, UnitOutput, UnitTiming,
};
use crate::typer::trace::{FrameId, TraceKind};

use std::path::PathBuf;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
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
    pub const ALL: [Target; 6] = [
        Target::Intel64Linux,
        Target::Arm64Macos,
        Target::Wasm64Wasi,
        Target::Intel64Bare,
        Target::Arm64Bare,
        Target::Wasm64Bare,
    ];

    pub fn parse(name: &str) -> Option<Target> {
        Target::ALL.into_iter().find(|t| t.to_str() == name)
    }

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

#[derive(Debug, Clone, Copy)]
pub struct BuildConfig {
    pub target: Target,
    pub cpu: StringId,
    pub features: StringId,
    pub optimize: bool,
    pub debug: bool,
    pub no_std: bool,
    pub sanitize: bool,
    pub filc: bool,
}

impl BuildConfig {
    pub fn new(target: Target, strings: &mut Interner) -> BuildConfig {
        let empty = strings.intern("");
        BuildConfig {
            target,
            cpu: empty,
            features: empty,
            optimize: false,
            debug: false,
            no_std: false,
            sanitize: false,
            filc: false,
        }
    }

    pub fn resolve(self, strings: &mut Interner) -> Result<BuildConfig, &'static str> {
        let target = self.target;
        if self.filc && target != Target::Intel64Linux {
            return Err("filc requires target intel64-linux; Fil-C only supports Linux/x86_64");
        }
        if self.filc && self.sanitize {
            return Err("filc and sanitize are mutually exclusive");
        }
        let native = detect_host_target() == Some(target);
        let (baseline_cpu, baseline_features) = if native {
            ("native", "")
        } else {
            match target.arch() {
                Arch::Intel => ("x86-64", ""),
                Arch::Arm => ("generic", "+neon"),
                Arch::Wasm => (
                    "generic",
                    "+simd128,+bulk-memory,+sign-ext,+mutable-globals,+nontrapping-fptoint",
                ),
            }
        };
        let cpu = match strings.get(self.cpu) {
            "" => baseline_cpu,
            "native" if !native => return Err("cpu \"native\" requires the host target"),
            cpu => cpu,
        };
        let features = match (strings.get(self.cpu), strings.get(self.features)) {
            ("", "") => baseline_features,
            (_, features) => features,
        };
        Ok(BuildConfig { cpu: strings.intern(cpu), features: strings.intern(features), ..self })
    }

    pub fn simd_bytes(&self, strings: &Interner) -> u32 {
        let mut simd_bytes = 16;
        for feature in strings.get(self.features).split(',') {
            let width = match feature {
                "+avx512f" => 64,
                "+avx2" => 32,
                _ => 16,
            };
            simd_bytes = simd_bytes.max(width);
        }
        #[cfg(target_arch = "x86_64")]
        if strings.get(self.cpu) == "native" {
            if std::arch::is_x86_feature_detected!("avx512f") {
                simd_bytes = 64;
            } else if std::arch::is_x86_feature_detected!("avx2") {
                simd_bytes = 32;
            }
        }
        simd_bytes
    }

    pub fn add_to_hash(
        &self,
        strings: &Interner,
        hash: crate::snap::InputsHash,
    ) -> crate::snap::InputsHash {
        let BuildConfig { target, cpu, features, optimize, debug, no_std, sanitize, filc } = *self;
        hash.add(&[
            target.to_str().as_bytes(),
            strings.get(cpu).as_bytes(),
            strings.get(features).as_bytes(),
            &self.simd_bytes(strings).to_le_bytes(),
            &[optimize, debug, no_std, sanitize, filc].map(|b| b as u8),
        ])
    }
}

pub struct BuildRequest {
    pub default: BuildConfig,
    pub command: Command,
    pub options: Vec<String>,
    pub host: Option<Target>,
}

impl BuildRequest {
    pub fn hash(
        &self,
        strings: &Interner,
        idents: &IdentPool,
        config: &CompilerConfig,
    ) -> crate::snap::InputsHash {
        let mut hash = self.default.add_to_hash(
            strings,
            crate::snap::InputsHash(0).add(&[
                b"build-request",
                idents.get_string(config.src_path).as_bytes(),
                idents.get_string(config.k1_home).as_bytes(),
                self.command.request_name().as_bytes(),
                self.host.map_or("", |t| t.to_str()).as_bytes(),
            ]),
        );
        for option in &self.options {
            hash = hash.add(&[option.as_bytes()]);
        }
        hash
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Command {
    Check,
    Build,
    Run,
    Test,
    Server,
    Setup { force: bool },
    Clean,
}

impl Command {
    pub fn is_test(&self) -> bool {
        matches!(self, Command::Test)
    }

    pub fn codegens(&self) -> bool {
        !matches!(self, Command::Check | Command::Setup { .. })
    }

    pub fn request_name(&self) -> &'static str {
        match self {
            Command::Check | Command::Setup { .. } | Command::Clean => "check",
            Command::Build => "build",
            Command::Run => "run",
            Command::Test => "test",
            Command::Server => "server",
        }
    }

    pub fn inputs_hash_byte(&self) -> u8 {
        match self {
            Command::Check => 0,
            Command::Build => 1,
            Command::Run => 2,
            Command::Test => 3,
            Command::Server => 4,
            Command::Setup { force: false } => 5,
            Command::Setup { force: true } => 6,
            Command::Clean => 7,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ToolFlags {
    pub emit_llvm: bool,
    pub dump_module: bool,
    pub dump_ir: bool,
    pub dump_idents: bool,
    pub dump_trace: bool,
    pub profile: bool,
    pub chatty: bool,
    pub optimize_ir: bool,
    pub cache: bool,
}

impl Default for ToolFlags {
    fn default() -> ToolFlags {
        ToolFlags {
            emit_llvm: false,
            dump_module: false,
            dump_ir: false,
            dump_idents: false,
            dump_trace: false,
            profile: false,
            chatty: false,
            optimize_ir: true,
            cache: true,
        }
    }
}

impl ToolFlags {
    pub fn record_trace(&self) -> bool {
        self.chatty || self.dump_trace
    }
}

pub struct CompileRequest {
    pub path: PathBuf,
    pub strings: Interner,
    pub build: BuildRequest,
    pub tools: ToolFlags,
    pub k1_home: Option<String>,
    pub lsp: LspCompileOptions,
}

impl CompileRequest {
    pub fn new(
        path: PathBuf,
        command: Command,
        target: Option<Target>,
    ) -> std::result::Result<CompileRequest, &'static str> {
        let host = detect_host_target();
        let Some(target) = target.or(host) else {
            return Err("Unsupported host platform; provide your target explicitly");
        };
        let mut strings = Interner::make_small();
        let default = BuildConfig::new(target, &mut strings);
        Ok(CompileRequest {
            path,
            strings,
            build: BuildRequest { default, command, options: Vec::new(), host },
            tools: ToolFlags::default(),
            k1_home: None,
            lsp: LspCompileOptions::default(),
        })
    }
}

/// All paths are canonical UTF-8 strings interned in the ident pool; see kpath
#[derive(Debug, Clone, Copy)]
pub struct CompilerConfig {
    pub src_path: StringId,
    pub home_dir: StringId,
    pub k1_home: StringId,
    pub command: Command,
    pub out_dir: StringId,
    pub out_dir_generated: StringId,
    pub cache_dir: StringId,
    pub tools: ToolFlags,
}

impl CompilerConfig {
    pub fn reintern(self, from: &IdentPool, to: &IdentPool) -> CompilerConfig {
        let move_id = |id: StringId| to.intern(from.get_string(id));
        CompilerConfig {
            src_path: move_id(self.src_path),
            home_dir: move_id(self.home_dir),
            k1_home: move_id(self.k1_home),
            out_dir: move_id(self.out_dir),
            out_dir_generated: move_id(self.out_dir_generated),
            cache_dir: move_id(self.cache_dir),
            ..self
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
    pub progress_sink: Option<crate::typer::trace::LiveProgressSink>,
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
    Build(&'static str),
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

pub fn module_root_file(dir: &str) -> Option<PathBuf> {
    let mut path_buf = PathBuf::from(dir);
    path_buf.push("module.k1");
    if path_buf.is_file() {
        return Some(path_buf);
    }
    path_buf.pop();
    path_buf.push(kpath::file_name(dir));
    let stem_path = path_buf.with_extension("k1");
    if stem_path.is_file() { Some(stem_path) } else { None }
}

pub fn is_module_dir(dir: &str) -> bool {
    Path::new(dir).join(crate::plan::BUILD_FILE_NAME).is_file() || module_root_file(dir).is_some()
}

pub fn find_check_target_for_file(file: &Path) -> PathBuf {
    let dir = file.parent().unwrap();
    if is_module_dir(&dir.to_string_lossy()) { dir.to_owned() } else { file.to_owned() }
}

pub struct SourceFile {
    /// canonical absolute, owned String because these are handed across the reader threads
    pub path: String,
    pub content: String,
    pub content_hash: u64,
    pub lexed: crate::lex::Lexed,
}

pub fn read_source(
    path: &str,
    overrides: &fxhash::FxHashMap<String, String>,
) -> Result<String, String> {
    match overrides.get(path) {
        Some(content) => Ok(content.clone()),
        None => fs::read_to_string(Path::new(path))
            .map_err(|e| format!("Failed to read source file {path}: {e}")),
    }
}

fn read_and_lex_source_file(
    path: String,
    overrides: &fxhash::FxHashMap<String, String>,
) -> Result<SourceFile, String> {
    let content = read_source(&path, overrides)?;
    let content_hash = content_hash64(content.as_bytes());
    let lexed = crate::lex::lex(&content, crate::lex::Lexed::default());
    Ok(SourceFile { path, content, content_hash, lexed })
}

fn collect_module_source_paths(module_dir: &str, is_core: bool) -> Result<Vec<String>, String> {
    let entries = fs::read_dir(Path::new(module_dir))
        .map_err(|e| format!("Failed to list module dir {module_dir}: {e}"))?;
    let dir_name = kpath::file_name(module_dir);
    let mut files = vec![];
    let mut root: Option<(u8, usize)> = None;
    for item in entries {
        let Ok(item) = item else {
            continue;
        };
        let path = item.path();
        if !path.extension().is_some_and(|ext| ext == "k1") {
            continue;
        }
        let path_string = path
            .into_os_string()
            .into_string()
            .map_err(|s| format!("Source file name is not valid UTF-8: {}", s.to_string_lossy()))?;
        let name = kpath::file_name(&path_string);
        if name == crate::plan::BUILD_FILE_NAME {
            continue;
        }
        let rank = if is_core {
            (name == "builtin.k1").then_some(0)
        } else if name == "module.k1" {
            Some(0)
        } else if kpath::file_stem(name) == dir_name {
            Some(1)
        } else {
            None
        };
        if let Some(rank) = rank
            && root.is_none_or(|(best, _)| rank < best)
        {
            root = Some((rank, files.len()));
        }
        files.push(path_string);
    }
    let root_path = root.map(|(_, index)| files.swap_remove(index));
    files.sort();
    if let Some(root_path) = root_path {
        files.insert(0, root_path);
    }
    Ok(files)
}

pub fn spawn_module_sources_read(
    path: &str,
    is_dir: bool,
    is_core: bool,
    source_overrides: &fxhash::FxHashMap<String, String>,
) -> ModuleSources {
    let overrides = source_overrides.clone();
    let path = path.to_string();
    let reader = std::thread::spawn(move || {
        let paths = if is_dir { collect_module_source_paths(&path, is_core)? } else { vec![path] };
        let mut sources = Vec::with_capacity(paths.len());
        for path in paths {
            sources.push(read_and_lex_source_file(path, &overrides)?);
        }
        Ok(sources)
    });
    ModuleSources::Reading(reader)
}

pub enum ModuleSources {
    Reading(std::thread::JoinHandle<Result<Vec<SourceFile>, String>>),
    Read(Result<Vec<SourceFile>, String>),
}

impl ModuleSources {
    pub fn wait(&mut self) -> &Result<Vec<SourceFile>, String> {
        if let ModuleSources::Reading(_) = self {
            let ModuleSources::Reading(reader) =
                std::mem::replace(self, ModuleSources::Read(Ok(Vec::new())))
            else {
                unreachable!()
            };
            *self =
                ModuleSources::Read(reader.join().expect("module sources reader thread panicked"));
        }
        match self {
            ModuleSources::Read(result) => result,
            ModuleSources::Reading(_) => unreachable!(),
        }
    }

    pub fn join(self) -> Result<Vec<SourceFile>, String> {
        match self {
            ModuleSources::Reading(reader) => {
                reader.join().expect("module sources reader thread panicked")
            }
            ModuleSources::Read(result) => result,
        }
    }
}

pub fn content_hash64(bytes: &[u8]) -> u64 {
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

pub struct SetupRequest {
    pub module_dir: StringId,
    pub module_name: StringId,
    pub build_hash: u64,
    pub outputs: SV8<StringId>,
    pub inputs: SV8<StringId>,
    pub target: Target,
    pub force: bool,
}

/// Live for the duration of one setup run: holds the cross-process lock and the
/// module dir as cwd, which `fn setup` runs in
pub struct StartedSetup {
    header: String,
    inputs: Vec<StampFile>,
    _lock: SetupLock,
    _cwd: CwdGuard,
}

/// `None` when the declared outputs are already fresh. Otherwise the stamp is
/// cleared and the declared outputs removed, so a failed run reads as stale
pub fn start_setup<Tag>(
    idents: &IdentPool,
    req: &SetupRequest,
    scratch: &mut Mem<Tag>,
) -> anyhow::Result<Option<StartedSetup>> {
    let module_dir = idents.get_string(req.module_dir);
    if !req.force && check_setup(idents, req, scratch)?.0 {
        return Ok(None);
    }
    let setup_out_dir = kpath::join_tmp(scratch, idents, req.module_dir, (".k1-out", "setup"));
    fs::create_dir_all(Path::new(setup_out_dir.as_str()))?;
    let lock_path = kpath::join_tmp(scratch, idents, setup_out_dir.as_str(), "lock");
    let lock = SetupLock::acquire(lock_path.as_str())?;
    let (fresh, header, inputs) = check_setup(idents, req, scratch)?;
    if !req.force && fresh {
        return Ok(None);
    }

    let stamp_path = kpath::join_tmp(scratch, idents, setup_out_dir.as_str(), "stamp");
    let _ = fs::remove_file(Path::new(stamp_path.as_str()));
    for output in &req.outputs {
        let output_path = kpath::join_tmp(scratch, idents, req.module_dir, *output);
        let p = Path::new(output_path.as_str());
        if p.is_dir() {
            fs::remove_dir_all(p)?;
        } else if p.exists() {
            fs::remove_file(p)?;
        }
    }
    Ok(Some(StartedSetup { header, inputs, _lock: lock, _cwd: CwdGuard::enter(module_dir) }))
}

pub fn finish_setup<Tag>(
    idents: &IdentPool,
    req: &SetupRequest,
    started: StartedSetup,
    scratch: &mut Mem<Tag>,
) -> Result<()> {
    let module_name = idents.get_string(req.module_name);
    let outputs = collect_output_files_for_setup(idents, req, scratch, None)
        .map_err(|e| anyhow::anyhow!("fn setup for module '{module_name}' completed but {e}"))?;
    let stamp_path =
        kpath::join_tmp(scratch, idents, req.module_dir, (".k1-out", ("setup", "stamp")));
    let stamp_content = generate_stamp_text(&started.header, &started.inputs, &outputs);
    fs::write(Path::new(stamp_path.as_str()), stamp_content)?;
    Ok(())
}

fn setup_is_fresh<Tag>(
    idents: &IdentPool,
    req: &SetupRequest,
    scratch: &mut Mem<Tag>,
    stamp_path: &str,
    existing: Option<SetupStamp>,
    header: &str,
    inputs: &[StampFile],
) -> bool {
    let Some(existing) = existing else {
        return false;
    };
    let Ok(outputs) = collect_output_files_for_setup(idents, req, scratch, Some(&existing)) else {
        return false;
    };
    if existing.header != header
        || existing.files.len() != inputs.len() + outputs.len()
        || !existing.files.iter().zip(inputs.iter().chain(outputs.iter())).all(|(a, b)| {
            a.label == b.label && a.relative_path == b.relative_path && a.hash == b.hash
        })
    {
        return false;
    }
    let current = generate_stamp_text(header, inputs, &outputs);
    if existing.text != current {
        let _ = fs::write(Path::new(stamp_path), current);
    }
    true
}

struct StampFile {
    label: &'static str,
    relative_path: String,
    size: u64,
    mtime_ns: u128,
    hash: u64,
}

struct SetupStamp {
    text: String,
    header: String,
    files: Vec<StampFile>,
}

impl SetupStamp {
    fn read(path: &str) -> Option<SetupStamp> {
        let text = fs::read_to_string(Path::new(path)).ok()?;
        let mut header = String::new();
        let mut files: Vec<StampFile> = vec![];
        for line in text.lines() {
            let (label, rest) = if let Some(rest) = line.strip_prefix("input-file: ") {
                ("input", rest)
            } else if let Some(rest) = line.strip_prefix("output-file: ") {
                ("output", rest)
            } else {
                if !files.is_empty() {
                    return None;
                }
                header.push_str(line);
                header.push('\n');
                continue;
            };
            let mut fields = rest.rsplitn(4, ' ');
            let hash = u64::from_str_radix(fields.next()?, 16).ok()?;
            let mtime_ns: u128 = fields.next()?.parse().ok()?;
            let size: u64 = fields.next()?.parse().ok()?;
            let rel = fields.next()?.to_string();
            files.push(StampFile { label, relative_path: rel, size, mtime_ns, hash });
        }
        Some(SetupStamp { text, header, files })
    }

    fn known_hashes(&self) -> fxhash::FxHashMap<&str, &StampFile> {
        let mut known = fxhash::FxHashMap::default();
        for f in &self.files {
            known.insert(f.relative_path.as_str(), f);
        }
        known
    }
}

fn generate_stamp_text(header: &str, inputs: &[StampFile], outputs: &[StampFile]) -> String {
    use std::fmt::Write;
    let mut s = header.to_string();
    for f in inputs.iter().chain(outputs.iter()) {
        writeln!(
            s,
            "{}-file: {} {} {} {:016x}",
            f.label, f.relative_path, f.size, f.mtime_ns, f.hash
        )
        .unwrap();
    }
    s
}

/// A file whose size and mtime match its stamp entry keeps that entry's hash
fn stamp_file(
    label: &'static str,
    module_dir: &str,
    file: &str,
    known: &fxhash::FxHashMap<&str, &StampFile>,
    buf: &mut [u8],
) -> Result<StampFile> {
    let relative_path =
        file.strip_prefix(module_dir).unwrap_or(file).trim_start_matches('/').to_string();
    let meta = fs::metadata(Path::new(file))
        .map_err(|e| anyhow::anyhow!("failed to stat setup {label} {file}: {e}"))?;
    let size = meta.len();
    let mtime_ns = meta
        .modified()
        .ok()
        .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let hash = match known.get(relative_path.as_str()) {
        Some(k) if k.size == size && k.mtime_ns == mtime_ns => k.hash,
        _ => hash_file_content64(file, buf)
            .map_err(|e| anyhow::anyhow!("failed to read setup {label} {file}: {e}"))?,
    };
    Ok(StampFile { label, relative_path, size, mtime_ns, hash })
}

/// every file under every declared output
fn collect_output_files_for_setup<Tag>(
    idents: &IdentPool,
    req: &SetupRequest,
    scratch: &mut Mem<Tag>,
    existing: Option<&SetupStamp>,
) -> Result<Vec<StampFile>> {
    let module_dir = idents.get_string(req.module_dir);
    let known = existing.map(SetupStamp::known_hashes).unwrap_or_default();
    let buf = scratch.push_slice_uninit::<u8>(FILE_HASH_BUF_LEN);
    let mut files: Vec<StampFile> = vec![];
    let mut collected_filenames: Vec<String> = vec![];
    for output in &req.outputs {
        let output_path = kpath::join_tmp(scratch, idents, req.module_dir, *output);
        if !Path::new(output_path.as_str()).exists() {
            bail!("did not produce declared output '{}'", idents.get_string(*output));
        }
        collected_filenames.clear();
        collect_files_within_dir(output_path.as_str(), &mut collected_filenames)?;
        collected_filenames.sort();
        for file in &collected_filenames {
            files.push(stamp_file("output", module_dir, file, &known, buf)?);
        }
    }
    Ok(files)
}

fn compute_setup_fingerprint<Tag>(
    idents: &IdentPool,
    req: &SetupRequest,
    scratch: &mut Mem<Tag>,
    existing: Option<&SetupStamp>,
) -> Result<(String, Vec<StampFile>)> {
    use std::fmt::Write;
    let module_dir = idents.get_string(req.module_dir);
    let mut s = String::new();
    writeln!(s, "k1-setup-stamp v6").unwrap();
    writeln!(s, "target: {}", req.target.to_str()).unwrap();
    writeln!(s, "build: {:016x}", req.build_hash).unwrap();
    for (label, ids) in [("outputs", &req.outputs), ("inputs", &req.inputs)] {
        write!(s, "{label}: ").unwrap();
        for (i, id) in ids.iter().enumerate() {
            if i > 0 {
                s.push('|');
            }
            s.push_str(idents.get_string(*id));
        }
        s.push('\n');
    }
    let mut output_paths: kmem::List<MStr<Tag>, Tag> = scratch.new_list(req.outputs.len() as u32);
    for o in &req.outputs {
        let path = kpath::join_tmp(scratch, idents, req.module_dir, *o);
        output_paths.push(path);
    }
    let known = existing.map(SetupStamp::known_hashes).unwrap_or_default();
    let buf = scratch.push_slice_uninit::<u8>(FILE_HASH_BUF_LEN);
    let mut files: Vec<StampFile> = vec![];
    let mut files_named_by_path: Vec<String> = vec![];
    for input in &req.inputs {
        let input_path = kpath::join_tmp(scratch, idents, req.module_dir, *input);
        files_named_by_path.clear();
        collect_files_within_dir(input_path.as_str(), &mut files_named_by_path)?;
        files_named_by_path.sort();
        for file in &files_named_by_path {
            if output_paths.iter().any(|o| o.as_str() == file) {
                continue;
            }
            files.push(stamp_file("input", module_dir, file, &known, buf)?);
        }
    }
    Ok((s, files))
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

fn check_setup<Tag>(
    idents: &IdentPool,
    req: &SetupRequest,
    scratch: &mut Mem<Tag>,
) -> Result<(bool, String, Vec<StampFile>)> {
    let stamp_path =
        kpath::join_tmp(scratch, idents, req.module_dir, (".k1-out", ("setup", "stamp")));
    let existing = SetupStamp::read(stamp_path.as_str());
    let (header, inputs) = compute_setup_fingerprint(idents, req, scratch, existing.as_ref())?;
    let fresh =
        setup_is_fresh(idents, req, scratch, stamp_path.as_str(), existing, &header, &inputs);
    Ok((fresh, header, inputs))
}

pub fn setup_is_current<Tag>(
    idents: &IdentPool,
    req: &SetupRequest,
    scratch: &mut Mem<Tag>,
) -> bool {
    matches!(check_setup(idents, req, scratch), Ok((true, _, _)))
}

fn dump_checked_ir(k1: &mut TypedProgram) -> K1Result<()> {
    let roots = ir::get_program_roots(k1)?;
    let reachable = ir::compile_reachable(k1, &roots.functions)?;
    if let Err(e) = ir::dump_program_ir(k1, &reachable) {
        kbail!(k1, SpanId::NONE, "Failed to write the ir dump: {e}");
    }
    Ok(())
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

enum Planned {
    Plan(crate::plan::BuildPlan, Option<Box<TypedProgram>>),
    Setup(Box<TypedProgram>),
}

fn plan_program(
    ast: &mut crate::parse::ParsedProgram,
    config: CompilerConfig,
    mut strings: Interner,
    request: &BuildRequest,
    lsp: &LspCompileOptions,
    is_dir: bool,
    module_name: StringId,
) -> std::result::Result<Planned, CompileProgramError> {
    use crate::plan::BuildPlan;
    let idents = &ast.idents;
    let src_path = idents.get_string(config.src_path);
    let k1_home = idents.get_string(config.k1_home);
    let is_setup = matches!(config.command, Command::Setup { .. });
    let has_build_file = is_dir
        && crate::plan::build_file_path(idents, &mut ast.tmp, src_path, &lsp.source_overrides)
            .is_some();
    if !has_build_file && !is_setup {
        if is_dir && !is_module_dir(src_path) {
            return Err(CompileProgramError::Build(
                "a module dir needs a build.k1, module.k1 or <dir>.k1",
            ));
        }
        let resolved = request.default.resolve(&mut strings).map_err(CompileProgramError::Build)?;
        let primary = crate::plan::PlannedModule::primary(
            strings.intern(idents.get_string(module_name)),
            strings.intern(src_path),
            is_dir,
        );
        let plan =
            BuildPlan::trivial(strings, resolved, idents, &mut ast.tmp, k1_home, Some(primary));
        return Ok(Planned::Plan(plan, None));
    }
    let request_hash = request.hash(&strings, idents, &config);
    let plan_path =
        crate::plan::plan_cache_path(Path::new(idents.get_string(config.cache_dir)), request_hash);
    if !is_setup
        && config.tools.cache
        && let Some(plan) = BuildPlan::load(&plan_path, request_hash)
        && plan.is_fresh(idents, &mut ast.tmp, k1_home, &lsp.source_overrides)
    {
        return Ok(Planned::Plan(plan, None));
    }
    let (plan, host) =
        crate::typer::host::plan_in_host(config, idents, strings, request, lsp, module_name)?;
    if is_setup {
        return Ok(Planned::Setup(host));
    }
    if config.tools.cache && lsp.source_overrides.is_empty() {
        let _ = plan.store(&plan_path, request_hash);
    }
    Ok(Planned::Plan(plan, Some(host)))
}

pub(crate) fn open_program(
    ast: crate::parse::ParsedProgram,
    config: CompilerConfig,
    plan: crate::plan::BuildPlan,
    lsp: LspCompileOptions,
    snapshot_count: usize,
) -> (TypedProgram, Vec<ModuleSources>) {
    let mut sources: Vec<ModuleSources> = Vec::with_capacity(plan.modules().len());
    for (index, m) in plan.modules().iter().enumerate() {
        sources.push(spawn_module_sources_read(
            plan.get(m.path),
            m.is_dir,
            index == 0,
            &lsp.source_overrides,
        ));
    }
    let restored = if config.tools.cache {
        restore_longest_prefix(&ast, config, plan, &lsp, &mut sources, snapshot_count)
    } else {
        Err(plan)
    };
    let k1 = match restored {
        Ok(k1) => k1,
        Err(plan) => TypedProgram::new(ast, config, plan, lsp),
    };
    (k1, sources)
}

fn restore_longest_prefix(
    ast: &crate::parse::ParsedProgram,
    config: CompilerConfig,
    plan: crate::plan::BuildPlan,
    lsp: &LspCompileOptions,
    sources: &mut [ModuleSources],
    snapshot_count: usize,
) -> std::result::Result<TypedProgram, crate::plan::BuildPlan> {
    let settings = typer::snapshot::inputs_hash_from_settings(&ast.idents, &config, &plan);
    let mut hashes: SV8<crate::snap::InputsHash> = SV8::new();
    let mut hash = settings;
    for (index, module_sources) in sources.iter_mut().take(snapshot_count).enumerate() {
        let Ok(files) = module_sources.wait() else { break };
        hash = typer::snapshot::module_inputs_hash(hash, &plan, index, files);
        hashes.push(hash);
    }
    let cache_dir = Path::new(ast.idents.get_string(config.cache_dir));
    let clock = crate::clock::Clock::new();
    for (index, hash) in hashes.iter().enumerate().rev() {
        let load_start = clock.raw();
        let path = crate::snap::cache_entry_path(cache_dir, index, settings);
        let Some(bytes) = crate::snap::cache_load(&path) else { continue };
        let load_end = clock.raw();
        let reader = match crate::snap::SnapReader::new(&bytes, *hash) {
            Ok(reader) => reader,
            Err(e) => {
                if config.tools.chatty {
                    eprintln!("ignoring cache entry: {e}");
                }
                continue;
            }
        };
        let mut restored = TypedProgram::restore(
            reader,
            *hash,
            config,
            &ast.idents,
            plan,
            lsp.clone(),
            (load_start, load_end),
        );
        let module_count = index as u32 + 1;
        restored.restored_module_count = module_count;
        let msg = format!(
            "restored {module_count} modules from cache ({:.1}mb)",
            bytes.len() as f64 / (1024.0 * 1024.0)
        );
        info!("{msg}");
        if config.tools.chatty {
            eprintln!("{msg}");
        }
        return Ok(restored);
    }
    Err(plan)
}

/// If `path` is a directory,
/// - compile all files in the directory.
/// - program name is the name of the directory.
///
/// If `path` is a file,
/// - compile that file only.
/// - program name is the name of the file.
pub fn compile_program(
    request: CompileRequest,
) -> std::result::Result<TypedProgram, CompileProgramError> {
    let CompileRequest { path, strings, build, tools, k1_home, lsp } = request;
    #[cfg(feature = "profile")]
    let profiler_guard = if tools.profile {
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
    let src_path = kpath::canonicalize_string_id(idents, &path)
        .unwrap_or_else(|e| panic!("Failed to load source path: {e}"));

    let (is_dir, home_dir, module_name) = module_home_from_src_path(idents, src_path);
    ast.name_id = module_name;

    let out_dir = kpath::join_id(&ast.idents, &mut ast.mem, home_dir, ".k1-out");
    let out_dir_generated = kpath::join_id(&ast.idents, &mut ast.mem, out_dir, "generated");
    let cache_dir = kpath::join_id(&ast.idents, &mut ast.mem, out_dir, crate::snap::CACHE_DIR_NAME);
    std::fs::create_dir_all(Path::new(ast.idents.get_string(out_dir_generated))).unwrap();

    // Find the installation. The request's wins, env var overrides, otherwise
    // release mode says co-located with the binary. dev mode says cwd
    let k1_home_raw = k1_home
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
    if tools.chatty {
        eprintln!("using k1 home: {k1_home}");
    }
    let k1_home_id = ast.idents.intern(&k1_home);
    let config = CompilerConfig {
        src_path,
        home_dir,
        k1_home: k1_home_id,
        command: build.command,
        out_dir,
        out_dir_generated,
        cache_dir,
        tools,
    };

    let _cwd = CwdGuard::enter(ast.idents.get_string(home_dir));

    let plan_start = crate::clock::Clock::new().raw();
    let (plan, host) =
        match plan_program(&mut ast, config, strings, &build, &lsp, is_dir, module_name)? {
            Planned::Plan(plan, host) => (plan, host),
            Planned::Setup(host) => return Ok(*host),
        };
    let plan_end = crate::clock::Clock::new().raw();

    let snapshot_count = plan.modules().len() - 1;
    let (mut k1, sources) = open_program(ast, config, plan, lsp, snapshot_count);
    k1.trace.clock_start = clock_start;
    if k1.trace.profiling_mode {
        let kind = if host.is_some() { TraceKind::HostPlan } else { TraceKind::PlanCheck };
        k1.trace.record(kind, 0, None, plan_start, plan_end, 0, 0);
    }

    let loaded = k1.load_plan_modules(sources, snapshot_count);
    if let Some(host) = host {
        k1.adopt_messages(&host);
    }
    match loaded {
        Err(e) => k1.report(e),
        Ok(()) => {
            #[cfg(debug_assertions)]
            k1.debug_snapshot_roundtrip();
        }
    }

    k1.write_emitted_sources();
    if tools.dump_module {
        write_program_dump(&k1);
    }
    if tools.dump_idents {
        write_idents_dump(&k1);
    }

    let is_ok = k1.error_count() == 0;
    if !is_ok {
        return Err(CompileProgramError::TyperFailure(Box::new(k1)));
    };
    if tools.dump_ir && !config.command.codegens() {
        if let Err(e) = dump_checked_ir(&mut k1) {
            k1.report(e);
            return Err(CompileProgramError::TyperFailure(Box::new(k1)));
        }
    }

    let warning_count =
        k1.messages.borrow().iter().filter(|e| e.level == MessageLevel::Warn).count();
    if warning_count > 0 {
        k1.trace_clear();
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
    let plan = &k1.plan;
    let filc = plan.config.filc;
    let mut out: Vec<ModuleLibs> = vec![];
    for module in plan.modules() {
        let module_libs_dir =
            kpath::join_tmp(k1.get_tmp_unsafe(), idents, plan.module_dir(module), LIBS_DIR_NAME);
        let mut link_args: Vec<String> = vec![];
        for link_arg in plan.mem.getn(module.link_args) {
            link_args.push(plan.get(*link_arg).to_string());
        }
        let mut libs: Vec<(LibRefLinkType, String)> = vec![];
        for lib in plan.mem.getn(module.libs) {
            let logical_name_str = plan.get(lib.name);
            let logical_name =
                if filc { format!("{logical_name_str}-filc") } else { logical_name_str.into() };
            let filename = logical_name_to_lib_filename(
                idents,
                k1.get_tmp_unsafe(),
                module_libs_dir.as_str(),
                plan.config.target,
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
    let target = k1.plan.config.target;
    let debug = k1.plan.config.debug;
    let idents = &k1.ast.idents;
    let out_dir = k1.config.out_dir;
    let optimize = k1.plan.config.optimize;
    let sanitize = k1.plan.config.sanitize;
    let filc = k1.plan.config.filc;

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

    match k1.plan.config.target.platform() {
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
            bail!("library output is not supported on {}", k1.plan.config.target.to_str())
        }
    }
    Ok(())
}

pub fn write_library_archive(
    k1: &TypedProgram,
    module_name: &str,
    objects: &[String],
) -> Result<()> {
    let target = k1.plan.config.target;
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
    k1.anyhow_from_message(e)
}

fn cg_error_to_message(k1: &TypedProgram, e: CgError) -> K1Message {
    let message_string = k1.ast.idents.intern(e.message);
    k1.make_error(message_string, e.span)
}

pub fn codegen_module(ctx: &Context, k1: &mut TypedProgram) -> Result<()> {
    // Ns-driven, not fn-driven: a reload ns holding only globals still gets a dylib
    let mut reload_nss: Vec<NamespaceId> = vec![];
    for ns_id in k1.namespaces.namespaces.iter_ids() {
        if k1.namespaces.get(ns_id).reload {
            reload_nss.push(ns_id);
        }
    }
    if !reload_nss.is_empty() && k1.plan.config.filc {
        bail!("ns(reload) is not supported under --filc");
    }
    if !k1.plan.is_executable() {
        if !reload_nss.is_empty() {
            bail!("ns(reload) requires an executable host module");
        }
        if k1.plan.config.filc {
            bail!("library output is not supported under --filc");
        }
    }
    for ns_id in &reload_nss {
        let frame = k1.trace_push(TraceKind::ReloadDylib, ns_id.as_u32(), 0);
        let written = write_reload_dylib(ctx, k1, *ns_id);
        k1.trace_pop(frame);
        written?;
    }

    let mut module_name = k1.program_name().to_string();
    if k1.config.command.is_test() {
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
        Err(e) => match k1.error_count() {
            0 => anyhow::bail!(report_codegen_error(k1, e)),
            _ => anyhow::bail!(k1.failure_summary()),
        },
    };
    if k1.config.tools.dump_ir {
        ir::dump_program_ir(k1, &roots.reachable)?;
    }
    let is_host_native = detect_host_target() == Some(k1.plan.config.target);
    let object_is_artifact = !k1.plan.is_executable() && !is_host_native
        || k1.plan.config.target.platform() == Platform::Bare;
    let objects =
        write_unit_artifacts(ctx, k1, &roots, CgKind::Host, &module_name, object_is_artifact)?;

    if k1.plan.is_executable() {
        if k1.plan.config.target.platform() == Platform::Bare {
            bail!("bare targets emit objects only; there is no executable lane");
        }
        let mut link_options: Vec<String> = vec![];
        if !reload_nss.is_empty() {
            // Ensure host globals are visible to dlopen'd reload dylibs
            let export_flag = match k1.plan.config.target.platform() {
                Platform::PosixMacos => "-Wl,-export_dynamic",
                Platform::PosixLinux => "-rdynamic",
                Platform::Wasi | Platform::Bare => {
                    bail!("ns(reload) is not supported on {}", k1.plan.config.target.to_str())
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

pub fn report_trace(k1: &TypedProgram) {
    k1.trace_clear();
    if k1.config.tools.chatty {
        k1.print_trace_summary(&mut std::io::stderr()).unwrap();
    }
    if k1.config.tools.dump_trace {
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
            t.clock_passed,
            0,
            0,
        );
        k1.trace.record(
            TraceKind::LlvmEmit,
            t.index as u32,
            Some(unit),
            t.clock_passed,
            t.clock_end,
            0,
            0,
        );
    }
}

fn write_unit_artifacts(
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
    let optimize_ir = k1.plan.config.optimize && !k1.plan.config.filc;
    let pipeline = if optimize_ir {
        Pipeline::O3
    } else if k1.plan.config.debug {
        Pipeline::None
    } else {
        Pipeline::Dev
    };
    let single_file = k1.plan.config.filc || k1.config.tools.emit_llvm || object_is_artifact;
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
        Cg::codegen_units(k1, roots, plans, kind, output, object_path).map_err(|e| report_cg(k1, e))
    };
    if let Ok((_, timings)) = &generated {
        record_unit_timings(k1, codegen_frame, timings);
    }
    k1.trace_pop(codegen_frame);
    let (artifacts, _) = generated?;

    if single_file {
        let merged = Cg::merge_units(ctx, module_name, &artifacts).map_err(|e| report_cg(k1, e))?;
        let machine = Cg::make_target_machine(k1);
        let passes_frame = k1.trace_push(TraceKind::LlvmPasses, 0, 0);
        codegen_llvm::run_passes(&merged, &machine, pipeline);
        k1.trace_pop(passes_frame);
        if k1.plan.config.filc {
            let ll_path = format!("{out_dir}/{module_name}.ll");
            std::fs::write(Path::new(&ll_path), codegen_llvm::llvm_ir_text_filc(&merged))
                .map_err(|e| anyhow::anyhow!("Failed to write {ll_path}: {e}"))?;
            return Ok(vec![ll_path]);
        }
        if k1.config.tools.emit_llvm {
            let ll_path = format!("{out_dir}/{module_name}.ll");
            std::fs::write(Path::new(&ll_path), merged.print_to_string().to_string())
                .map_err(|e| anyhow::anyhow!("Failed to write {ll_path}: {e}"))?;
        }
        let path = format!("{out_dir}/{module_name}.o");
        let emit_frame = k1.trace_push(TraceKind::LlvmEmit, 0, 0);
        let emitted = codegen_llvm::emit_object(&merged, &machine, &path);
        k1.trace_pop(emit_frame);
        emitted.map_err(|e| report_cg(k1, e))?;
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
fn write_reload_dylib(ctx: &Context, k1: &mut TypedProgram, ns_id: NamespaceId) -> Result<()> {
    let ns_name = k1.ident_str(k1.namespaces.get(ns_id).name).to_string();
    let module_name = k1.program_name().to_string();
    let platform = k1.plan.config.target.platform();
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
        write_unit_artifacts(ctx, k1, &roots, CgKind::ReloadDylib(ns_id), &unit_name, false)?;
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
pub(crate) mod test_support {
    use super::*;

    pub(crate) fn set_home() {
        static SET_HOME: std::sync::Once = std::sync::Once::new();
        SET_HOME.call_once(|| unsafe {
            std::env::set_var("K1_HOME", env!("CARGO_MANIFEST_DIR"));
        });
    }

    pub(crate) fn compile_source(name: &str, source: &str) -> TypedProgram {
        set_home();
        let dir = std::env::temp_dir().join(format!("k1_{name}_{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        let app = dir.join("app.k1");
        fs::write(&app, source).unwrap();
        let mut request = CompileRequest::new(app, Command::Check, None).unwrap();
        request.build.default.no_std = true;
        request.tools.cache = false;
        compile_program(request).ok().expect("compile must succeed")
    }

    pub(crate) fn function_named(k1: &TypedProgram, name: &str) -> crate::typer::FunctionId {
        for (function_id, function) in k1.function_iter() {
            if k1.ident_str(function.name) == name {
                return function_id;
            }
        }
        panic!("no function named {name}")
    }
}

#[cfg(test)]
mod compiler_test {
    use super::test_support::set_home;
    use super::*;

    fn check(file: &Path) -> CompileRequest {
        let mut request = CompileRequest::new(file.to_path_buf(), Command::Check, None).unwrap();
        request.build.default.no_std = true;
        request
    }

    fn program_of(result: std::result::Result<TypedProgram, CompileProgramError>) -> TypedProgram {
        match result {
            Ok(program) => program,
            Err(CompileProgramError::TyperFailure(program)) => *program,
            Err(CompileProgramError::Build(message)) => panic!("{message}"),
        }
    }

    fn snapshot_count(cache_dir: &Path) -> usize {
        let mut n = 0;
        for entry in fs::read_dir(cache_dir).unwrap() {
            if entry.unwrap().path().extension().is_some_and(|e| e == "snap") {
                n += 1;
            }
        }
        n
    }

    #[test]
    fn disk_cache_restores_deps_when_primary_fails() {
        set_home();
        let dir =
            std::env::temp_dir().join(format!("k1_disk_cache_deps_test_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        let app = dir.join("app");
        let lib = app.join("deps").join("lib");
        fs::create_dir_all(&lib).unwrap();
        fs::write(
            app.join("build.k1"),
            "fn module(_b: k1/build-config): k1/module { let m = k1/module/new(); m.executable(); m.dep(\"lib\"); m }\n",
        )
        .unwrap();
        fs::write(
            lib.join("build.k1"),
            "fn module(_b: k1/build-config): k1/module { let m = k1/module/new(); m.library(); m }\n",
        )
        .unwrap();
        fs::write(lib.join("lib.k1"), "fn one(): i32 { 1 }\n").unwrap();
        let main = app.join("main.k1");
        fs::write(&main, "fn main(): i32 { \"not an int\" }\n").unwrap();

        let cold = compile_program(check(&app));
        assert!(cold.is_err(), "the broken app must fail typechecking");
        let cold = program_of(cold);
        assert_eq!(cold.restored_module_count, 0, "first compile has nothing to restore");
        let cache_dir = cold.cache_dir().to_path_buf();
        let stored = snapshot_count(&cache_dir);

        let warm = program_of(compile_program(check(&app)));
        assert_eq!(warm.restored_module_count, 2, "a still-broken app restores core, lib");
        assert_eq!(snapshot_count(&cache_dir), stored, "a restored run stores nothing new");

        fs::write(&main, "fn main(): i32 { lib/one() }\n").unwrap();
        let fixed = compile_program(check(&app)).ok().expect("fixed app must succeed");
        assert_eq!(fixed.restored_module_count, 2, "the fixed app restores core, lib");

        let warm = compile_program(check(&app)).ok().expect("warm compile must succeed");
        assert_eq!(
            warm.restored_module_count, 2,
            "unchanged input restores core, lib; the app is never snapshotted"
        );

        for body in ["lib/one() + 1", "lib/one() + 2"] {
            fs::write(&main, format!("fn main(): i32 {{ {body} }}\n")).unwrap();
            let edited = compile_program(check(&app)).ok().expect("edited compile must succeed");
            assert_eq!(edited.restored_module_count, 2, "an edited app restores core, lib");
            assert_eq!(snapshot_count(&cache_dir), stored, "editing the app stores nothing");
        }

        fs::write(lib.join("lib.k1"), "fn one(): i32 { 2 }\n").unwrap();
        let dep_edited =
            compile_program(check(&app)).ok().expect("dep-edited compile must succeed");
        assert_eq!(dep_edited.restored_module_count, 1, "an edited dep restores only core");
        assert_eq!(snapshot_count(&cache_dir), stored, "the dep's entry is replaced, not added");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn plan_cache_follows_build_file() {
        set_home();
        let dir = std::env::temp_dir().join(format!("k1_plan_cache_test_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        let app = dir.join("app");
        let lib = app.join("deps").join("lib");
        fs::create_dir_all(&lib).unwrap();
        fs::write(lib.join("lib.k1"), "fn one(): i32 { 1 }\n").unwrap();
        fs::write(app.join("main.k1"), "fn main(): i32 { 0 }\n").unwrap();
        let no_deps = "fn module(_b: k1/build-config): k1/module { k1/module/new() }\n";
        fs::write(app.join("build.k1"), no_deps).unwrap();

        let cold = compile_program(check(&app)).ok().expect("cold compile must succeed");
        assert_eq!(cold.plan.modules().len(), 2, "core and app");
        let warm = compile_program(check(&app)).ok().expect("warm compile must succeed");
        assert_eq!(warm.plan.modules().len(), 2, "the cached plan is reused");

        fs::write(
            app.join("build.k1"),
            "fn module(_b: k1/build-config): k1/module { let m = k1/module/new(); m.dep(\"lib\"); m }\n",
        )
        .unwrap();
        let with_dep = compile_program(check(&app)).ok().expect("edited build.k1 must replan");
        assert_eq!(with_dep.plan.modules().len(), 3, "core, lib and app");

        fs::remove_dir_all(&lib).unwrap();
        let missing = compile_program(check(&app));
        assert!(missing.is_err(), "a removed dep dir invalidates the plan");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn disk_cache_restores_longest_valid_prefix() {
        set_home();
        let dir = std::env::temp_dir().join(format!("k1_disk_cache_test_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        let app = dir.join("app.k1");
        fs::write(&app, "fn main(): i32 { 0 }\n").unwrap();

        let cold = compile_program(check(&app)).ok().expect("cold compile must succeed");
        assert_eq!(cold.restored_module_count, 0, "first compile has nothing to restore");

        let warm = compile_program(check(&app)).ok().expect("warm compile must succeed");
        assert_eq!(warm.restored_module_count, 1, "unchanged input restores only core");

        fs::write(&app, "fn main(): i32 {\n  println(\"v2\")\n  0\n}\n").unwrap();
        let edited = compile_program(check(&app)).ok().expect("edited compile must succeed");
        assert_eq!(edited.restored_module_count, 1, "an edited app restores only core");

        let _ = fs::remove_dir_all(&dir);
    }
}
