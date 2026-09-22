// Copyright (c) 2026 knix
// All rights reserved.

use std::path::{Path, PathBuf};

use crate::compiler::{BuildConfig, SetupRequest};
use crate::kmem::{MSlice, MStr, Mem};
use crate::kpath;
use crate::parse::{IdentPool, Interner, StringId};
use crate::snap::{InputsHash, SnapReader, SnapWriter};
use crate::typer::{LibRefLinkType, ModuleKind};
use crate::{SV4, SV8};

pub struct PlanMem;

pub const BUILD_FILE_NAME: &str = "build.k1";

#[derive(Clone, Copy)]
pub struct PlannedLib {
    pub name: StringId,
    pub link_type: LibRefLinkType,
}

#[derive(Clone, Copy)]
pub struct ParamsProvider {
    pub from: u32,
    pub offset: u32,
    pub len: u32,
}

#[derive(Clone, Copy)]
pub struct PlannedSetup {
    pub outputs: MSlice<StringId, PlanMem>,
    pub inputs: MSlice<StringId, PlanMem>,
}

#[derive(Clone, Copy)]
pub struct PlannedModule {
    pub name: StringId,
    pub path: StringId,
    pub is_dir: bool,
    pub kind: ModuleKind,
    pub deps: MSlice<u32, PlanMem>,
    pub libs: MSlice<PlannedLib, PlanMem>,
    pub link_args: MSlice<StringId, PlanMem>,
    pub providers: MSlice<ParamsProvider, PlanMem>,
    pub setup: Option<PlannedSetup>,
    pub build_file_hash: Option<u64>,
}

impl PlannedModule {
    pub fn primary(name: StringId, path: StringId, is_dir: bool) -> PlannedModule {
        PlannedModule { is_dir, kind: ModuleKind::Executable, ..PlannedModule::library(name, path) }
    }

    pub fn library(name: StringId, path: StringId) -> PlannedModule {
        PlannedModule {
            name,
            path,
            is_dir: true,
            kind: ModuleKind::Library,
            deps: MSlice::empty(),
            libs: MSlice::empty(),
            link_args: MSlice::empty(),
            providers: MSlice::empty(),
            setup: None,
            build_file_hash: None,
        }
    }
}

pub struct BuildPlan {
    pub strings: Interner,
    pub mem: Mem<PlanMem>,
    pub config: BuildConfig,
    pub modules: MSlice<PlannedModule, PlanMem>,
}

impl BuildPlan {
    pub fn new(strings: Interner, config: BuildConfig) -> BuildPlan {
        BuildPlan { strings, mem: Mem::make(), config, modules: MSlice::empty() }
    }

    pub fn trivial<Tag>(
        strings: Interner,
        config: BuildConfig,
        idents: &IdentPool,
        tmp: &mut Mem<Tag>,
        k1_home: &str,
        primary: Option<PlannedModule>,
    ) -> BuildPlan {
        let mut plan = BuildPlan::new(strings, config);
        let mut modules: SV8<PlannedModule> = SV8::new();
        plan.push_fixed_modules(&mut modules, idents, tmp, k1_home);
        modules.extend(primary);
        plan.modules = plan.mem.pushn(&modules);
        plan
    }

    pub fn get(&self, id: StringId) -> &'static str {
        self.strings.get(id)
    }

    pub fn modules(&self) -> &[PlannedModule] {
        self.mem.getn(self.modules)
    }

    pub fn module(&self, index: usize) -> &PlannedModule {
        &self.modules()[index]
    }

    pub fn primary(&self) -> &PlannedModule {
        self.modules().last().unwrap()
    }

    pub fn is_executable(&self) -> bool {
        self.primary().kind == ModuleKind::Executable
    }

    pub fn module_dir(&self, m: &PlannedModule) -> &'static str {
        let path = self.get(m.path);
        if m.is_dir { path } else { kpath::parent(path) }
    }

    pub fn push_fixed_modules<Tag>(
        &mut self,
        modules: &mut SV8<PlannedModule>,
        idents: &IdentPool,
        tmp: &mut Mem<Tag>,
        k1_home: &str,
    ) {
        let mark = tmp.mark();
        let core_path = kpath::join_tmp(tmp, idents, k1_home, ("modules", "core"));
        let mut core = PlannedModule::library(
            self.strings.intern("core"),
            self.strings.intern(core_path.as_str()),
        );
        core.libs = self.mem.pushn(&[PlannedLib {
            name: self.strings.intern("k1rt"),
            link_type: LibRefLinkType::Static,
        }]);
        modules.push(core);
        if !self.config.no_std {
            let std_path = kpath::join_tmp(tmp, idents, k1_home, ("modules", "std"));
            modules.push(PlannedModule::library(
                self.strings.intern("std"),
                self.strings.intern(std_path.as_str()),
            ));
        }
        tmp.reset_to(mark);
    }

    pub fn setup_request(
        &self,
        idents: &IdentPool,
        m: &PlannedModule,
        force: bool,
    ) -> Option<SetupRequest> {
        let setup = m.setup?;
        let mut outputs = SV8::new();
        for id in self.mem.getn(setup.outputs) {
            outputs.push(idents.intern(self.get(*id)));
        }
        let mut inputs = SV8::new();
        for id in self.mem.getn(setup.inputs) {
            inputs.push(idents.intern(self.get(*id)));
        }
        Some(SetupRequest {
            module_dir: idents.intern(self.module_dir(m)),
            module_name: idents.intern(self.get(m.name)),
            build_hash: m.build_file_hash.unwrap(),
            outputs,
            inputs,
            target: self.config.target,
            force,
        })
    }

    pub fn store(&self, path: &Path, request: InputsHash) -> std::io::Result<()> {
        let mut w = crate::snap::cache_store_begin(path, request)?;
        self.snap_into(&mut w);
        crate::snap::cache_store_finish(path, w)
    }

    fn snap_into(&self, w: &mut SnapWriter) {
        w.write_section("plan");
        self.strings.snap(w);
        self.mem.snap(w);
        w.write_t(&self.config);
        w.write_t(&self.modules);
        w.write_section("end");
    }

    pub fn load(path: &Path, request: InputsHash) -> Option<BuildPlan> {
        let bytes = crate::snap::cache_load(path)?;
        let mut r = SnapReader::new(&bytes, request).ok()?;
        r.section("plan");
        let mut strings = Interner::make_small();
        strings.restore(&mut r);
        let mut mem = Mem::make();
        mem.restore(&mut r);
        let config = r.read_t();
        let modules = r.read_t();
        r.section("end");
        Some(BuildPlan { strings, mem, config, modules })
    }

    pub fn is_fresh<Tag>(
        &self,
        idents: &IdentPool,
        tmp: &mut Mem<Tag>,
        k1_home: &str,
        overrides: &fxhash::FxHashMap<String, String>,
    ) -> bool {
        let primary_dir = self.module_dir(self.primary());
        for m in self.modules() {
            let dir = self.module_dir(m);
            if m.is_dir && read_build_file_hash(idents, tmp, dir, overrides) != m.build_file_hash {
                return false;
            }
            for dep in self.mem.getn(m.deps) {
                let target = self.module(*dep as usize);
                let mark = tmp.mark();
                let resolved =
                    resolve_dep(idents, tmp, primary_dir, dir, k1_home, self.get(target.name));
                tmp.reset_to(mark);
                match resolved {
                    Ok(path) if idents.get_string(path) == self.get(target.path) => {}
                    _ => return false,
                }
            }
            if let Some(req) = self.setup_request(idents, m, false) {
                let mark = tmp.mark();
                let current = crate::compiler::setup_is_current(idents, &req, tmp);
                tmp.reset_to(mark);
                if !current {
                    return false;
                }
            }
        }
        true
    }
}

pub fn plan_cache_path(cache_dir: &Path, request: InputsHash) -> PathBuf {
    cache_dir.join(format!("plan-{:016x}", request.0 as u64))
}

pub fn build_file_path<Tag>(
    idents: &IdentPool,
    tmp: &mut Mem<Tag>,
    dir: &str,
    overrides: &fxhash::FxHashMap<String, String>,
) -> Option<StringId> {
    let mark = tmp.mark();
    let path = kpath::join_tmp(tmp, idents, dir, BUILD_FILE_NAME);
    let exists = overrides.contains_key(path.as_str()) || Path::new(path.as_str()).is_file();
    let id = exists.then(|| idents.intern(path.as_str()));
    tmp.reset_to(mark);
    id
}

pub fn read_build_file_hash<Tag>(
    idents: &IdentPool,
    tmp: &mut Mem<Tag>,
    dir: &str,
    overrides: &fxhash::FxHashMap<String, String>,
) -> Option<u64> {
    let path = build_file_path(idents, tmp, dir, overrides)?;
    let content = crate::compiler::read_source(idents.get_string(path), overrides).ok()?;
    Some(crate::compiler::content_hash64(content.as_bytes()))
}

pub fn resolve_dep<Tag>(
    idents: &IdentPool,
    tmp: &mut Mem<Tag>,
    primary_dir: &str,
    dependent_dir: &str,
    k1_home: &str,
    name: &str,
) -> Result<StringId, String> {
    let mut candidates: SV4<MStr<Tag>> = SV4::new();
    candidates.push(kpath::join_tmp(tmp, idents, primary_dir, ("deps", name)));
    if dependent_dir != primary_dir {
        candidates.push(kpath::join_tmp(tmp, idents, dependent_dir, ("deps", name)));
    }
    candidates.push(kpath::join_tmp(tmp, idents, k1_home, ("modules", name)));
    for candidate in &candidates {
        if Path::new(candidate.as_str()).is_dir() {
            return kpath::canonicalize_string_id(idents, candidate.as_str())
                .map_err(|e| format!("dependency '{name}' at {candidate}: {e}"));
        }
    }
    let tried = candidates.iter().map(|c| c.as_str()).collect::<Vec<_>>().join(", ");
    Err(format!("dependency '{name}' not found; tried {tried}"))
}
