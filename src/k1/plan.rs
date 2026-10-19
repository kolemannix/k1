// Copyright (c) 2026 knix
// All rights reserved.

use std::path::{Path, PathBuf};

use crate::SV8;
use crate::compiler::{BuildConfig, SetupRequest};
use crate::kmem::{MSlice, Mem};
use crate::kpath;
use crate::parse::{IdentPool, Interner, StringId};
use crate::snap::{InputsHash, SnapReader, SnapWriter};
use crate::typer::{LibRefLinkType, ModuleId, ModuleKind};

pub struct PlanMem;

pub const BUILD_FILE_NAME: &str = "build.k1";

#[derive(Clone, Copy)]
pub struct PlannedLib {
    pub name: StringId,
    pub link_type: LibRefLinkType,
}

#[derive(Clone, Copy)]
pub struct TextOrigin {
    pub start: u32,
    pub end: u32,
    pub build_file: StringId,
    pub offset: u32,
    pub len: u32,
}

#[derive(Clone, Copy)]
pub struct PlannedParams {
    pub text: StringId,
    pub origins: MSlice<TextOrigin, PlanMem>,
    pub value_hash: u64,
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
    pub submodule_owner: Option<ModuleId>,
    pub deps: MSlice<ModuleId, PlanMem>,
    pub libs: MSlice<PlannedLib, PlanMem>,
    pub link_args: MSlice<StringId, PlanMem>,
    pub params: Option<PlannedParams>,
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
            submodule_owner: None,
            deps: MSlice::empty(),
            libs: MSlice::empty(),
            link_args: MSlice::empty(),
            params: None,
            setup: None,
            build_file_hash: None,
        }
    }
}

pub struct BuildPlan {
    pub strings: Interner,
    pub mem: Mem<PlanMem>,
    pub config: BuildConfig,
    pub editor: EditorSettings,
    pub modules: MSlice<PlannedModule, PlanMem>,
    pub excludes: MSlice<StringId, PlanMem>,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct EditorSettings {
    pub test_on_save: bool,
}

impl BuildPlan {
    pub fn new(strings: Interner, config: BuildConfig) -> BuildPlan {
        BuildPlan {
            strings,
            mem: Mem::make(),
            config,
            editor: EditorSettings::default(),
            modules: MSlice::empty(),
            excludes: MSlice::empty(),
        }
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

    pub fn get_module(&self, id: ModuleId) -> Option<&PlannedModule> {
        self.modules().get(id.as_u32() as usize - 1)
    }

    pub fn primary(&self) -> &PlannedModule {
        self.modules().last().unwrap()
    }

    pub fn primary_id(&self) -> ModuleId {
        ModuleId::from_u32(self.modules().len() as u32).unwrap()
    }

    pub fn excludes(&self) -> &[StringId] {
        self.mem.getn(self.excludes)
    }

    pub fn find_owning_module(
        &self,
        path: &str,
        overlay: &fxhash::FxHashMap<String, String>,
    ) -> Option<ModuleId> {
        let mut excludes: Vec<&str> = Vec::with_capacity(self.excludes().len());
        for exclude in self.excludes() {
            excludes.push(self.get(*exclude));
        }
        let mut dir = kpath::parent(path);
        loop {
            for (index, m) in self.modules().iter().enumerate() {
                let module_path = self.get(m.path);
                if module_path == path || module_path == dir {
                    return ModuleId::from_u32(index as u32 + 1);
                }
            }
            let parent = kpath::parent(dir);
            if parent == dir || crate::compiler::is_skipped_module_dir(dir, &excludes, overlay) {
                return None;
            }
            dir = parent;
        }
    }

    pub fn get_inputs_hash(&self) -> InputsHash {
        let mut hash = InputsHash(0);
        for m in self.modules() {
            hash = hash
                .add(&[self.get(m.path).as_bytes(), &m.build_file_hash.unwrap_or(0).to_le_bytes()]);
        }
        hash
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
        let params_hash = match m.params {
            None => 0,
            Some(p) => p.value_hash,
        };
        Some(SetupRequest {
            module_dir: idents.intern(self.module_dir(m)),
            module_name: idents.intern(self.get(m.name)),
            build_hash: m.build_file_hash.unwrap(),
            params_hash,
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
        w.write_t(&self.editor);
        w.write_t(&self.modules);
        w.write_t(&self.excludes);
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
        let editor = r.read_t();
        let modules = r.read_t();
        let excludes = r.read_t();
        r.section("end");
        Some(BuildPlan { strings, mem, config, editor, modules, excludes })
    }

    pub fn is_fresh<Tag>(
        &self,
        idents: &IdentPool,
        tmp: &mut Mem<Tag>,
        overlay: &fxhash::FxHashMap<String, String>,
    ) -> bool {
        for m in self.modules() {
            let dir = self.module_dir(m);
            if !Path::new(dir).is_dir() {
                return false;
            }
            if m.is_dir && read_build_file_hash(idents, tmp, dir, overlay) != m.build_file_hash {
                return false;
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
    overlay: &fxhash::FxHashMap<String, String>,
) -> Option<StringId> {
    let mark = tmp.mark();
    let path = kpath::join_tmp(tmp, idents, dir, BUILD_FILE_NAME);
    let id = if crate::compiler::source_exists(path.as_str(), overlay) {
        Some(idents.intern(path.as_str()))
    } else {
        None
    };
    tmp.reset_to(mark);
    id
}

pub fn read_build_file_hash<Tag>(
    idents: &IdentPool,
    tmp: &mut Mem<Tag>,
    dir: &str,
    overlay: &fxhash::FxHashMap<String, String>,
) -> Option<u64> {
    let path = build_file_path(idents, tmp, dir, overlay)?;
    let content = crate::compiler::read_source(idents.get_string(path), overlay).ok()?;
    Some(crate::compiler::content_hash64(content.as_bytes()))
}
