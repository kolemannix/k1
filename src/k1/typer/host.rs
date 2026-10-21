// Copyright (c) 2026 knix
// All rights reserved.

use super::*;
use crate::compiler::{
    BuildConfig, BuildRequest, Command, CompileProgramError, CompilerConfig, LspCompileOptions,
    ModuleSources, Target,
};
use crate::parse::Interner;
use crate::plan::{
    BUILD_FILE_NAME, BuildPlan, PlanMem, PlannedLib, PlannedModule, PlannedParams, PlannedSetup,
    TextOrigin,
};
use crate::vm::k1_types::K1CodeSpan;
use std::path::PathBuf;

const PRIMARY_MODULE_INDEX: usize = 0;

#[derive(Clone)]
struct PlanEntry {
    module: PlannedModule,
    host: Option<ModuleId>,
    manifest_span: Span,
    resolved_by: usize,
    submodule_owner: Option<usize>,
    deps: SV8<usize>,
    ignore: SV8<StringId>,
    configure_span: Option<Span>,
}

struct Planner {
    plan: BuildPlan,
    entries: SV8<PlanEntry>,
    configure_requests: SV8<(StringId, Span, Span)>,
}

impl Planner {
    fn find_library(&self, name: &str) -> Option<usize> {
        self.entries
            .iter()
            .position(|e| e.submodule_owner.is_none() && self.plan.get(e.module.name) == name)
    }
}

type Pending = SV8<(usize, Option<ModuleSources>)>;

#[derive(Clone, Copy)]
enum Visit {
    Unvisited,
    InProgress,
    Done,
}

pub fn plan_in_host(
    config: CompilerConfig,
    config_idents: &IdentPool,
    strings: Interner,
    request: &BuildRequest,
    lsp: &LspCompileOptions,
    primary_name: StringId,
) -> Result<(BuildPlan, Box<TypedProgram>), CompileProgramError> {
    let mut ast = ParsedProgram::make();
    let host_config = CompilerConfig {
        command: Command::Check,
        test: false,
        ..config.reintern(config_idents, &ast.idents)
    };
    ast.name_id = ast.idents.intern(config_idents.get_string(primary_name));
    let k1_home = ast.idents.get_string(host_config.k1_home);
    let mut host_strings = Interner::make_small();
    let Some(host_target) = crate::compiler::detect_host_target() else {
        return Err(CompileProgramError::Build(
            "Unsupported host platform; fn build runs on the host".to_string(),
        ));
    };
    let host_build = BuildConfig::new(host_target, &mut host_strings)
        .resolve(&mut host_strings)
        .map_err(|e| CompileProgramError::Build(e.to_string()))?;
    let host_plan =
        BuildPlan::trivial(host_strings, host_build, &ast.idents, &mut ast.tmp, k1_home, None);
    let host_lsp = LspCompileOptions {
        source_overlay: lsp.source_overlay.clone(),
        completion: lsp.completion,
        progress_sink: None,
        host_mode: crate::compiler::HostProgramMode::Discard,
    };
    let snapshot_count = host_plan.modules().len();
    let (mut h, sources) =
        crate::compiler::open_program(ast, host_config, host_plan, host_lsp, snapshot_count);
    h.module_params_phase = ModuleParamsPhase::HostDiscovery;
    let result = h
        .load_plan_modules(sources, snapshot_count)
        .and_then(|()| h.plan_build(BuildPlan::new(strings, request.default), request));
    match result {
        Ok(plan) if h.error_count() == 0 => Ok((plan, Box::new(h))),
        Ok(_) => Err(CompileProgramError::TyperFailure(Box::new(h))),
        Err(e) => {
            h.report(e);
            Err(CompileProgramError::TyperFailure(Box::new(h)))
        }
    }
}

impl TypedProgram {
    fn plan_build(&mut self, plan: BuildPlan, request: &BuildRequest) -> K1Result<BuildPlan> {
        let mut planner = Planner { plan, entries: SV8::new(), configure_requests: SV8::new() };
        let src_path = self.ast.idents.get_string(self.config.src_path);
        let primary_name = self.ast.idents.get_string(self.ast.name_id);
        let mut pending: Pending = SV8::new();
        let is_dir = Path::new(src_path).is_dir();
        self.queue_planned_module(
            &mut planner,
            primary_name,
            src_path,
            is_dir,
            PRIMARY_MODULE_INDEX,
            &mut pending,
        );
        planner.entries[PRIMARY_MODULE_INDEX].module.kind = ModuleKind::Executable;

        let mut build_value = None;
        let mut wave = 0;
        while !pending.is_empty() {
            let frame = self.trace_push(TraceKind::HostWave, wave, 0);
            let result = self.run_wave(&mut planner, pending, &mut build_value, request);
            self.trace_pop(frame);
            pending = result?;
            wave += 1;
        }

        // Every manifest has run, so every library the primary configures is known:
        // evaluate each module's deferred k1/module-params global against them
        for (name, name_span, value_span) in std::mem::take(&mut planner.configure_requests) {
            let name = self.ast.idents.get_string(name);
            let Some(index) =
                planner.entries.iter().position(|e| planner.plan.get(e.module.name) == name)
            else {
                kbail!(
                    self,
                    name_span,
                    "configure names '{}', which is not a library in this build",
                    name
                )
            };
            planner.entries[index].configure_span = Some(value_span);
        }
        self.module_params_phase = ModuleParamsPhase::HostSetup;
        for global_id in std::mem::take(&mut self.deferred_module_params) {
            let global = self.globals.get(global_id);
            let global_scope = self.variables.get(global.variable_id).owner_scope;
            let Some(index) = planner.entries.iter().position(|e| {
                e.host.is_some_and(|h| self.modules.get(h).namespace_scope_id == global_scope)
            }) else {
                kbail!(
                    self,
                    global.span,
                    "k1/module-params must initialize a top-level global of build.k1"
                )
            };
            let params = self.evaluate_module_params(&mut planner, index, global_id)?;
            planner.entries[index].module.params = Some(params);
        }
        for entry in &planner.entries {
            if let Some(span) = entry.configure_span
                && entry.module.params.is_none()
            {
                kbail!(
                    self,
                    span,
                    "Module '{}' accepts no parameters",
                    planner.plan.get(entry.module.name)
                );
            }
        }

        if request.command.is_test() {
            planner.entries[PRIMARY_MODULE_INDEX].module.kind = ModuleKind::Executable;
        }
        let order = self.get_dependency_order(&planner)?;
        let force_primary = matches!(request.command, Command::Setup { force: true });
        self.run_setups(&planner, &order, build_value.unwrap(), force_primary)?;
        Ok(self.order_plan_modules(planner, &order))
    }

    fn queue_planned_module(
        &mut self,
        planner: &mut Planner,
        name: &str,
        path: &str,
        is_dir: bool,
        resolved_by: usize,
        pending: &mut Pending,
    ) -> usize {
        let index = planner.entries.len();
        let mut module = PlannedModule::library(
            planner.plan.strings.intern(name),
            planner.plan.strings.intern(path),
        );
        module.is_dir = is_dir;
        planner.entries.push(PlanEntry {
            module,
            host: None,
            manifest_span: Span::NONE,
            resolved_by,
            submodule_owner: None,
            deps: SV8::new(),
            ignore: SV8::new(),
            configure_span: None,
        });
        let overlay = &self.lsp.source_overlay;
        let build_path = if is_dir {
            let tmp = self.get_tmp_unsafe();
            crate::plan::build_file_path(&self.ast.idents, tmp, path, overlay)
        } else {
            None
        };
        let read = build_path.map(|build_path| {
            let build_path = self.ast.idents.get_string(build_path);
            crate::compiler::spawn_module_sources_read(
                build_path,
                false,
                false,
                Vec::new(),
                overlay,
            )
        });
        pending.push((index, read));
        index
    }

    fn run_wave(
        &mut self,
        planner: &mut Planner,
        pending: Pending,
        build_value: &mut Option<StaticValueId>,
        request: &BuildRequest,
    ) -> K1Result<Pending> {
        let mut indices: SV8<usize> = SV8::new();
        for (index, read) in pending {
            if let Some(read) = read {
                let module = planner.entries[index].module;
                let name = self.ast.idents.intern(planner.plan.get(module.name));
                let files = self.read_module_sources(name, read)?;
                let hash = files[0].content_hash;
                let dir = self.ast.idents.intern(planner.plan.get(module.path));
                planner.entries[index].host = Some(self.compile_module(name, dir, files)?);
                planner.entries[index].module.build_file_hash = Some(hash);
            }
            indices.push(index);
        }
        let build_value = match *build_value {
            Some(value) => value,
            None => *build_value.insert(self.configure(planner, request)?),
        };

        let mut next: Pending = SV8::new();
        for index in indices {
            self.evaluate_manifest(planner, index, build_value, &mut next)?;
        }
        Ok(next)
    }

    fn find_host_fn(
        &self,
        module: ModuleId,
        name: StringId,
        param_type: TypeId,
        min_params: usize,
        return_type: Option<TypeId>,
        signature: &str,
    ) -> K1Result<Option<(FunctionId, Span, usize)>> {
        let scope = self.modules.get(module).namespace_scope_id;
        let Some(fn_id) = self.scopes.find_function_local(scope, name) else { return Ok(None) };
        let function = self.get_function(fn_id);
        let span = self.ast.get_span_for_id(function.parsed_id);
        let fn_type = self.get_function_type(fn_id);
        let params = self.mem.getn(fn_type.logical_params());
        if !function.type_params.is_empty()
            || params.len() < min_params
            || params.len() > 1
            || params.first().is_some_and(|p| p.type_id != param_type)
            || return_type.is_some_and(|t| fn_type.return_type != t)
        {
            kbail!(self, span, "expected {}", signature);
        }
        Ok(Some((fn_id, span, params.len())))
    }

    fn configure(
        &mut self,
        planner: &mut Planner,
        request: &BuildRequest,
    ) -> K1Result<StaticValueId> {
        let config_type = self.builtin_types.k1_build_config.unwrap();
        let request_type = self.builtin_types.k1_build_request.unwrap();
        let build_fn = match planner.entries[PRIMARY_MODULE_INDEX].host {
            Some(host) => self.find_host_fn(
                host,
                self.ast.idents.b.build,
                request_type,
                1,
                Some(config_type),
                "fn build(req: k1/build-request): k1/build-config",
            )?,
            None => None,
        };
        let (unresolved, span) = match build_fn {
            None => (request.default, Span::NONE),
            Some((fn_id, span, _)) => {
                let request_value = self.encode_build_request(&planner.plan, request);
                let result = self.execute_static_function(fn_id, &[request_value], span)?;
                (self.decode_build_config(result, &mut planner.plan, span)?, span)
            }
        };
        let config = match unresolved.resolve(&mut planner.plan.strings) {
            Ok(config) => config,
            Err(message) => kbail!(self, span, "{}", message),
        };
        planner.plan.config = config;
        if request.editor
            && let Some(host) = planner.entries[PRIMARY_MODULE_INDEX].host
            && let Some((fn_id, span, _)) = self.find_host_fn(
                host,
                self.ast.idents.b.editor,
                request_type,
                1,
                self.builtin_types.k1_editor_settings,
                "fn editor(req: k1/build-request): k1/editor-settings",
            )?
        {
            let request_value = self.encode_build_request(&planner.plan, request);
            let settings = self.execute_static_function(fn_id, &[request_value], span)?;
            planner.plan.editor.test_on_save = self.field_bool(settings, "test-on-save");
        }
        Ok(self.encode_build_config(&planner.plan, &config))
    }

    fn resolve_planned_dep(
        &mut self,
        planner: &mut Planner,
        dependent: usize,
        spec: &str,
        span: Span,
        pending: &mut Pending,
    ) -> K1Result<Option<usize>> {
        match spec {
            "core" => return Ok(None),
            "std" if planner.plan.config.no_std => {
                kbail!(self, span, "module 'std' is excluded by no-std")
            }
            "std" => return Ok(None),
            _ => {}
        }
        let is_path = spec.starts_with('/') || spec.starts_with("./") || spec.starts_with("../");
        if !is_path
            && let Some(index) = planner.find_library(spec)
            && index != PRIMARY_MODULE_INDEX
            && planner.entries[index].resolved_by == PRIMARY_MODULE_INDEX
        {
            return Ok(Some(index));
        }
        let dependent_dir = planner.plan.module_dir(&planner.entries[dependent].module);
        let joined = if spec.starts_with('/') {
            PathBuf::from(spec)
        } else if is_path {
            kpath::join_pathbuf(&self.ast.idents, dependent_dir, spec)
        } else {
            kpath::join_pathbuf(&self.ast.idents, self.config.k1_home, ("modules", spec))
        };
        if !joined.is_dir() {
            kbail!(self, span, "dependency '{}' not found: no folder {}", spec, joined.display());
        }
        let dir = match kpath::canonicalize_string_id(&self.ast.idents, &joined) {
            Ok(dir) => self.ast.idents.get_string(dir),
            Err(e) => kbail!(self, span, "dependency '{}': {}", spec, e),
        };
        let name = kpath::file_name(dir);
        let Some(index) = planner.find_library(name) else {
            return Ok(Some(
                self.queue_planned_module(planner, name, dir, true, dependent, pending),
            ));
        };
        let existing = &planner.entries[index];
        let existing_dir = planner.plan.get(existing.module.path);
        let primary_chose = index != PRIMARY_MODULE_INDEX
            && existing.resolved_by == PRIMARY_MODULE_INDEX
            && dependent != PRIMARY_MODULE_INDEX;
        if existing_dir == dir || primary_chose {
            return Ok(Some(index));
        }
        kbail!(
            self,
            span,
            "library '{}' resolves to two folders: {} (for '{}') and {} (for '{}')",
            name,
            existing_dir,
            planner.plan.get(planner.entries[existing.resolved_by].module.name),
            dir,
            planner.plan.get(planner.entries[dependent].module.name)
        )
    }

    fn evaluate_manifest(
        &mut self,
        planner: &mut Planner,
        entry_index: usize,
        build_value: StaticValueId,
        pending: &mut Pending,
    ) -> K1Result<()> {
        let module = planner.entries[entry_index].module;
        let Some(host) = planner.entries[entry_index].host else { return Ok(()) };
        let host_scope = self.modules.get(host).namespace_scope_id;
        let has_setup =
            self.scopes.find_function_local(host_scope, self.ast.idents.b.setup).is_some();
        let module_type = self.builtin_types.k1_module.unwrap();
        let config_type = self.builtin_types.k1_build_config.unwrap();
        let Some((fn_id, span, param_count)) = self.find_host_fn(
            host,
            self.ast.idents.b.module,
            config_type,
            0,
            Some(module_type),
            "fn module(): k1/module or fn module(b: k1/build-config): k1/module",
        )?
        else {
            if has_setup {
                planner.entries[entry_index].module.setup =
                    Some(PlannedSetup { outputs: MSlice::empty(), inputs: MSlice::empty() });
            }
            return Ok(());
        };
        let manifest = self.execute_static_function(fn_id, &[build_value][..param_count], span)?;
        let module_name = planner.plan.get(module.name);
        let module_dir = planner.plan.module_dir(&module);

        let mut deps: SV8<usize> = SV8::new();
        for spec in self.field_list(manifest, "depend") {
            let spec = self.static_str_of(*spec);
            let Some(dep) = self.resolve_planned_dep(planner, entry_index, spec, span, pending)?
            else {
                continue;
            };
            if deps.contains(&dep) {
                kbail!(self, span, "duplicate dependency '{}'", spec);
            }
            deps.push(dep);
        }

        let submodules = self.field_list(manifest, "submodules");
        let first_submodule = planner.entries.len();
        for submodule in submodules {
            let dir_spec = self.field_str(*submodule, "dir");
            let joined = kpath::join_pathbuf(&self.ast.idents, module_dir, dir_spec);
            if !joined.is_dir() {
                kbail!(
                    self,
                    span,
                    "submodule '{}' not found: no folder {}",
                    dir_spec,
                    joined.display()
                );
            }
            let dir = match kpath::canonicalize_string_id(&self.ast.idents, &joined) {
                Ok(dir) => self.ast.idents.get_string(dir),
                Err(e) => kbail!(self, span, "submodule '{}': {}", dir_spec, e),
            };
            let tmp = self.get_tmp_unsafe();
            if crate::plan::build_file_path(&self.ast.idents, tmp, dir, &self.lsp.source_overlay)
                .is_some()
            {
                kbail!(self, span, "submodule folder {} has its own {}", dir, BUILD_FILE_NAME);
            }
            let short_name = kpath::file_name(dir);
            for sibling in &planner.entries[first_submodule..] {
                if kpath::file_name(planner.plan.get(sibling.module.path)) == short_name {
                    kbail!(self, span, "submodule '{}' declared twice", short_name);
                }
            }
            let name = planner.plan.strings.intern(&format!("{module_name}/{short_name}"));
            planner.entries.push(PlanEntry {
                module: PlannedModule::library(name, planner.plan.strings.intern(dir)),
                host: None,
                manifest_span: span,
                resolved_by: entry_index,
                submodule_owner: Some(entry_index),
                deps: SV8::new(),
                ignore: SV8::new(),
                configure_span: None,
            });
        }
        let siblings = first_submodule..first_submodule + submodules.len();
        for (submodule_index, submodule) in siblings.clone().zip(submodules) {
            let mut submodule_deps: SV8<usize> = SV8::new();
            for spec in self.field_list(*submodule, "depend") {
                let spec = self.static_str_of(*spec);
                let sibling = siblings.clone().find(|s| {
                    kpath::file_name(planner.plan.get(planner.entries[*s].module.path)) == spec
                });
                let dep = match sibling {
                    Some(sibling) => sibling,
                    None => {
                        let Some(dep) =
                            self.resolve_planned_dep(planner, entry_index, spec, span, pending)?
                        else {
                            continue;
                        };
                        dep
                    }
                };
                if submodule_deps.contains(&dep) {
                    kbail!(self, span, "duplicate dependency '{}'", spec);
                }
                submodule_deps.push(dep);
            }
            planner.entries[submodule_index].deps = submodule_deps;
            deps.push(submodule_index);
        }

        let mut ignore: SV8<StringId> = SV8::new();
        for path in self.field_list(manifest, "ignore") {
            let path = self.static_str_of(*path);
            let joined = kpath::join_pathbuf(&self.ast.idents, module_dir, path);
            match kpath::canonicalize_string_id(&self.ast.idents, &joined) {
                Ok(dir) => {
                    ignore.push(planner.plan.strings.intern(self.ast.idents.get_string(dir)))
                }
                Err(e) => kbail!(self, span, "ignore '{}': {}", path, e),
            }
        }

        let configure = self.static_field(manifest, "configure");
        let text = self.field_str(configure, "text");
        if !text.is_empty() {
            if entry_index != PRIMARY_MODULE_INDEX {
                kbail!(
                    self,
                    span,
                    "Only the program configures libraries; '{}' sets configure",
                    module_name
                );
            }
            let mut code_spans: SV8<K1CodeSpan> = SV8::new();
            for code_span in self.field_list(configure, "spans") {
                code_spans.push(K1CodeSpan {
                    start: self.field_int(*code_span, "start") as u32,
                    len: self.field_int(*code_span, "len") as u32,
                    source: self.field_int(*code_span, "source"),
                });
            }
            let Some((content, table)) =
                self.build_emitted_source(span, true, text, &code_spans)?
            else {
                self.ice_span(span, "configure text was empty")
            };
            let file_id = self.add_emitted_source(&content, self.tmp.getn(table), span);
            let ParseMetaprogramResult::Expr(parsed) =
                self.parse_metaprogram_source(host, file_id, ParseAdHocKind::Expr)?
            else {
                self.ice_span(span, "configure parsed as definitions");
            };
            let ParsedExpr::Struct(_) = self.ast.exprs.get(parsed) else {
                kbail!(
                    self,
                    self.ast.exprs.get_span(parsed),
                    "configure must be a struct literal keyed by library name"
                );
            };
            let configured = self.get_struct_literal(parsed)?;
            for field in self.ast.mem.getn(configured.fields) {
                let StructValueFieldKind::Expr(value) = field.value else {
                    kbail!(self, field.span, "configure field '{}' needs a value", field.name);
                };
                let value_span = self.ast.exprs.get_span(value);
                planner.configure_requests.push((field.name, field.span, value_span));
            }
        }

        let mut libs: SV8<PlannedLib> = SV8::new();
        for lib in self.field_list(manifest, "link") {
            let link_type = match self.enum_name(self.static_field(*lib, "link-type")) {
                "static" => LibRefLinkType::Static,
                "dynamic" => LibRefLinkType::Dynamic,
                _ => LibRefLinkType::Default,
            };
            libs.push(PlannedLib {
                name: planner.plan.strings.intern(self.field_str(*lib, "name")),
                link_type,
            });
        }
        let libs = planner.plan.mem.pushn(&libs);
        let link_args =
            self.plan_strings(&mut planner.plan, self.field_list(manifest, "link-args"));
        let outputs =
            self.plan_strings(&mut planner.plan, self.field_list(manifest, "setup-outputs"));
        let inputs =
            self.plan_strings(&mut planner.plan, self.field_list(manifest, "setup-inputs"));
        if !has_setup && !(outputs.is_empty() && inputs.is_empty()) {
            kbail!(
                self,
                span,
                "setup-inputs and setup-outputs need a fn setup in this {}",
                BUILD_FILE_NAME
            );
        }
        let is_library = self.field_bool(manifest, "library");

        let entry = &mut planner.entries[entry_index];
        entry.manifest_span = span;
        entry.deps = deps;
        entry.ignore = ignore;
        if is_library {
            entry.module.kind = ModuleKind::Library;
        }
        entry.module.libs = libs;
        entry.module.link_args = link_args;
        if has_setup {
            entry.module.setup = Some(PlannedSetup { outputs, inputs });
        }
        Ok(())
    }

    fn plan_strings(
        &self,
        plan: &mut BuildPlan,
        values: &[StaticValueId],
    ) -> MSlice<StringId, PlanMem> {
        let mut out: SV8<StringId> = SV8::new();
        for value in values {
            out.push(plan.strings.intern(self.static_str_of(*value)));
        }
        plan.mem.pushn(&out)
    }

    fn static_field(&self, value: StaticValueId, name: &str) -> StaticValueId {
        let s = self.static_values.get(value).as_struct().unwrap();
        let name_id = self.ast.idents.intern(name);
        let Some((index, _)) = self.get_struct_field_by_name(s.type_id, name_id) else {
            self.ice_span(Span::NONE, format!("static struct has no field '{name}'"))
        };
        self.static_values.get_slice(s.fields)[index]
    }

    fn field_str(&self, value: StaticValueId, name: &str) -> &'static str {
        self.static_str_of(self.static_field(value, name))
    }

    fn static_str_of(&self, value: StaticValueId) -> &'static str {
        self.ast.idents.get_string(self.static_values.get(value).as_string().unwrap())
    }

    fn field_int(&self, value: StaticValueId, name: &str) -> u64 {
        let StaticValue::Int(int) = *self.static_values.get(self.static_field(value, name)) else {
            self.ice_span(Span::NONE, format!("static field '{name}' is not an integer"))
        };
        int.to_u64_bits()
    }

    fn field_bool(&self, value: StaticValueId, name: &str) -> bool {
        self.static_values.get(self.static_field(value, name)).as_boolean().unwrap()
    }

    fn field_list(&self, value: StaticValueId, name: &str) -> &'static [StaticValueId] {
        let list = *self.static_values.get(self.static_field(value, name)).as_container().unwrap();
        self.static_values.mem.getn(list.elements)
    }

    fn field_type(&self, struct_type: TypeId, name: &str) -> TypeId {
        let name_id = self.ast.idents.intern(name);
        self.get_struct_field_by_name(struct_type, name_id).unwrap().1.type_id
    }

    fn enum_name(&self, value: StaticValueId) -> &'static str {
        let (type_id, int_value) = self.static_values.get(value).as_enum().unwrap();
        let members = self.mem.getn(self.types.get(type_id).expect_enum().member_values);
        let Some(member) = members.iter().find(|m| m.int_value == int_value) else {
            self.ice_span(Span::NONE, "enum value matches no member")
        };
        self.ast.idents.get_string(member.name)
    }

    fn enum_named(&mut self, enum_type: TypeId, name: &str) -> StaticValueId {
        let name_id = self.ast.idents.intern(name);
        let members = self.mem.getn(self.types.get(enum_type).expect_enum().member_values);
        let Some(member) = members.iter().find(|m| m.name == name_id) else {
            self.ice_span(Span::NONE, format!("enum has no member '{name}'"))
        };
        self.static_values.add(StaticValue::Enum(enum_type, member.int_value))
    }

    fn static_str(&mut self, s: &str) -> StaticValueId {
        let id = self.ast.idents.intern(s);
        self.static_values.add_string(id)
    }

    fn encode_struct(
        &mut self,
        type_id: TypeId,
        fields: &[(&str, StaticValueId)],
    ) -> StaticValueId {
        let struct_type = *self.types.get(type_id).as_struct().unwrap();
        let mut values: SV8<StaticValueId> = SV8::new();
        for field in self.mem.getn(struct_type.fields) {
            let name = self.ast.idents.get_string(field.name);
            let Some((_, value)) = fields.iter().find(|(n, _)| *n == name) else {
                self.ice_span(Span::NONE, format!("no value for field '{name}'"))
            };
            values.push(*value);
        }
        self.static_values.add_struct_from_slice(type_id, &values)
    }

    fn encode_build_config(&mut self, plan: &BuildPlan, config: &BuildConfig) -> StaticValueId {
        let type_id = self.builtin_types.k1_build_config.unwrap();
        let target_type = self.field_type(type_id, "target");
        let fields = [
            ("target", self.enum_named(target_type, config.target.to_str())),
            ("cpu", self.static_str(plan.get(config.cpu))),
            ("features", self.static_str(plan.get(config.features))),
            ("optimize", StaticValuePool::get_bool_id(config.optimize)),
            ("debug", StaticValuePool::get_bool_id(config.debug)),
            ("no-std", StaticValuePool::get_bool_id(config.no_std)),
            ("sanitize", StaticValuePool::get_bool_id(config.sanitize)),
            ("filc", StaticValuePool::get_bool_id(config.filc)),
        ];
        self.encode_struct(type_id, &fields)
    }

    fn decode_build_config(
        &mut self,
        value: StaticValueId,
        plan: &mut BuildPlan,
        span: Span,
    ) -> K1Result<BuildConfig> {
        let target_name = self.enum_name(self.static_field(value, "target"));
        let Some(target) = Target::parse(target_name) else {
            kbail!(self, span, "fn build returned an unknown target {}", target_name);
        };
        Ok(BuildConfig {
            target,
            cpu: plan.strings.intern(self.field_str(value, "cpu")),
            features: plan.strings.intern(self.field_str(value, "features")),
            optimize: self.field_bool(value, "optimize"),
            debug: self.field_bool(value, "debug"),
            no_std: self.field_bool(value, "no-std"),
            sanitize: self.field_bool(value, "sanitize"),
            filc: self.field_bool(value, "filc"),
        })
    }

    fn encode_build_request(&mut self, plan: &BuildPlan, request: &BuildRequest) -> StaticValueId {
        let type_id = self.builtin_types.k1_build_request.unwrap();
        let default = self.encode_build_config(plan, &request.default);
        let command_type = self.field_type(type_id, "command");
        let command = self.enum_named(command_type, request.command.request_name());
        let options_type = self.field_type(type_id, "options");
        let option_type = self.get_linear_container_element(options_type).unwrap();
        let mut options: SV8<StaticValueId> = SV8::new();
        for option in &request.options {
            let (name, value) = option.split_once('=').unwrap_or((option, ""));
            let fields = [("name", self.static_str(name)), ("value", self.static_str(value))];
            options.push(self.encode_struct(option_type, &fields));
        }
        let options =
            self.add_static_container_from_ids(StaticContainerKind::List, options_type, &options);
        let host_type = self.field_type(type_id, "host");
        let host_sum = *self.types.get(host_type).as_sum().unwrap();
        let target_type = self.mem.getn(host_sum.variants)[OPT_SOME_VARIANT_INDEX].payload.unwrap();
        let host_target = request.host.map(|host| self.enum_named(target_type, host.to_str()));
        let host = synth::synth_static_option(&mut self.static_values, host_type, host_target);
        let editor = StaticValuePool::get_bool_id(request.editor);
        let fields = [
            ("default", default),
            ("command", command),
            ("options", options),
            ("host", host),
            ("editor", editor),
        ];
        self.encode_struct(type_id, &fields)
    }

    fn get_dependency_order(&self, planner: &Planner) -> K1Result<SV8<usize>> {
        let mut state: SV8<Visit> = smallvec![Visit::Unvisited; planner.entries.len()];
        let mut stack: SV8<usize> = smallvec![];
        let mut order: SV8<usize> = smallvec![];
        self.visit_module(planner, PRIMARY_MODULE_INDEX, &mut state, &mut stack, &mut order)?;
        Ok(order)
    }

    fn visit_module(
        &self,
        planner: &Planner,
        index: usize,
        state: &mut SV8<Visit>,
        stack: &mut SV8<usize>,
        order: &mut SV8<usize>,
    ) -> K1Result<()> {
        match state[index] {
            Visit::Done => return Ok(()),
            Visit::InProgress => {
                let mut cycle: Vec<&str> = vec![];
                for open in stack.iter().skip_while(|open| **open != index) {
                    cycle.push(planner.plan.get(planner.entries[*open].module.name));
                }
                cycle.push(planner.plan.get(planner.entries[index].module.name));
                let span = planner.entries[*stack.last().unwrap()].manifest_span;
                kbail!(self, span, "Module dependency cycle: {}", cycle.join(" -> "));
            }
            Visit::Unvisited => {}
        }
        state[index] = Visit::InProgress;
        stack.push(index);
        for dep in &planner.entries[index].deps {
            self.visit_module(planner, *dep, state, stack, order)?;
        }
        stack.pop();
        state[index] = Visit::Done;
        order.push(index);
        Ok(())
    }

    fn run_setups(
        &mut self,
        planner: &Planner,
        order: &[usize],
        build_value: StaticValueId,
        force_primary: bool,
    ) -> K1Result<()> {
        for &index in order {
            let entry = &planner.entries[index];
            let force = force_primary && index == PRIMARY_MODULE_INDEX;
            let Some(req) = planner.plan.setup_request(&self.ast.idents, &entry.module, force)
            else {
                continue;
            };
            let setup_ctx_type = self.builtin_types.k1_setup_ctx.unwrap();
            let Some((setup_fn, span, param_count)) = self.find_host_fn(
                entry.host.unwrap(),
                self.ast.idents.b.setup,
                setup_ctx_type,
                0,
                None,
                "fn setup() or fn setup(ctx: k1/setup-ctx)",
            )?
            else {
                self.ice_span(entry.manifest_span, "setup planned without a fn setup")
            };
            let frame = self.trace_push(TraceKind::SetupStamp, req.module_name.as_u32(), 0);
            let tmp = self.get_tmp_unsafe();
            let mark = tmp.mark();
            let started = crate::compiler::start_setup(&self.ast.idents, &req, tmp)
                .map_err(|e| self.error_from_anyhow(e, span));
            tmp.reset_to(mark);
            self.trace_pop(frame);
            let Some(started) = started? else { continue };
            self.trace_clear();
            eprintln!(
                "Setting up module '{}' (running fn setup in {}/{})...",
                planner.plan.get(entry.module.name),
                planner.plan.module_dir(&entry.module),
                BUILD_FILE_NAME
            );
            let ctx_value = self.encode_struct(setup_ctx_type, &[("build", build_value)]);
            let frame = self.trace_push(TraceKind::SetupFn, req.module_name.as_u32(), 0);
            let ran = self.execute_static_function(setup_fn, &[ctx_value][..param_count], span);
            self.trace_pop(frame);
            ran?;
            let tmp = self.get_tmp_unsafe();
            let mark = tmp.mark();
            let finished = crate::compiler::finish_setup(&self.ast.idents, &req, started, tmp)
                .map_err(|e| self.error_from_anyhow(e, span));
            tmp.reset_to(mark);
            finished?;
        }
        Ok(())
    }

    fn evaluate_module_params(
        &mut self,
        planner: &mut Planner,
        index: usize,
        global_id: TypedGlobalId,
    ) -> K1Result<PlannedParams> {
        let global = *self.globals.get(global_id);
        let host = planner.entries[index].host.unwrap();
        let host_scope_id = self.modules.get(host).namespace_scope_id;
        let module_name = planner.plan.get(planner.entries[index].module.name);
        let schema_type = global.type_id;
        let value_expr = global.parsed_expr.unwrap();
        let call_span = self.ast.exprs.get_span(value_expr);
        let args = self.ast.mem.getn(self.find_module_params_call_args(value_expr).unwrap());
        let configure_span = planner.entries[index].configure_span;
        let defaults = match args {
            [] if configure_span.is_none() => {
                kbail!(self, call_span, "Module '{}' requires parameters", module_name)
            }
            [] => None,
            [defaults] => Some(self.ast.exprs.get_span(defaults.value)),
            _ => {
                kbail!(self, call_span, "k1/module-params takes at most one argument: the defaults")
            }
        };

        let mut expr = String::new();
        let mut expr_origins: SV8<TextOrigin> = SV8::new();
        let mut emitted_chunks: SV4<CodeChunkPos> = SV4::new();
        for span in [defaults, configure_span].into_iter().flatten() {
            if !expr.is_empty() {
                expr.push_str(" | ");
            }
            let start = expr.len() as u32;
            self.append_source_text(planner, span, &mut expr, &mut expr_origins);
            emitted_chunks.push(CodeChunkPos { start, end: expr.len() as u32, source: span });
        }
        let source_file_id = self.add_emitted_source(&expr, &emitted_chunks, call_span);
        let ParseMetaprogramResult::Expr(parsed) =
            self.parse_metaprogram_source(host, source_file_id, ParseAdHocKind::Expr)?
        else {
            self.ice_span(call_span, "module params parsed as definitions");
        };
        let ctx = EvalExprContext::make(host_scope_id)
            .with_expected_type(Some(schema_type))
            .with_static_ctx(Some(StaticExecContext { expected_return_type: Some(schema_type) }));
        let value_id = self.execute_static_expr(parsed, ctx)?;
        let value_type = self.get_static_value_type(value_id);
        if let Err(msg) = self.check_types(schema_type, value_type, host_scope_id) {
            kbail!(
                self,
                call_span,
                "Type mismatch in params for module '{}': {}",
                module_name,
                msg
            );
        }
        self.globals.get_mut(global_id).initial_value = GlobalInitialValue::Value(value_id);
        let value_hash =
            crate::compiler::content_hash64(self.static_value_to_string(value_id).as_bytes());

        // The value above serves fn setup in this host program; the program being built
        // shares nothing with it but the plan, so it gets the same `let` as source text
        let parsed_global = self.ast.globals.get(global.ast_id);
        let name_start = parsed_global.name_span.global_start;
        let schema_end = self.ast.get_type_expr_span(parsed_global.type_expr).end();
        let mut text = String::from("let ");
        let mut let_origins: SV8<TextOrigin> = SV8::new();
        self.append_source_text(
            planner,
            Span { global_start: name_start, len: schema_end - name_start },
            &mut text,
            &mut let_origins,
        );
        text.push_str(" = ");
        let shift = text.len() as u32;
        for o in &expr_origins {
            let_origins.push(TextOrigin { start: o.start + shift, end: o.end + shift, ..*o });
        }
        text.push_str(&expr);
        text.push('\n');
        let origins = planner.plan.mem.pushn(&let_origins);
        Ok(PlannedParams { text: planner.plan.strings.intern(&text), origins, value_hash })
    }

    fn append_source_text(
        &self,
        planner: &mut Planner,
        span: Span,
        text: &mut String,
        origins: &mut SV8<TextOrigin>,
    ) {
        let file_id = self.ast.sources.file_of(span.global_start);
        let source = self.ast.sources.get(file_id);
        let offset = source.local_from_global_offset(span.global_start);
        let start = text.len() as u32;
        text.push_str(
            &source.content(&self.ast.mem)[offset as usize..(offset + span.len) as usize],
        );
        let mut pieces: SV4<(u32, u32, Span)> = smallvec![(start, text.len() as u32, span)];
        if let Some(emitted) = self.emitted_source_for_file(file_id) {
            pieces.clear();
            for entry in self.mem.getn(self.emitted_sources[emitted].entries) {
                let from = entry.start.max(offset);
                let to = entry.end.min(offset + span.len);
                let skip = from.saturating_sub(entry.start);
                if from >= to || skip >= entry.source.len {
                    continue;
                }
                let len = (to - from).min(entry.source.len - skip);
                let piece = Span { global_start: entry.source.global_start + skip, len };
                pieces.push((start + from - offset, start + to - offset, piece));
            }
        }
        for (piece_start, piece_end, piece) in pieces {
            let piece_file = self.ast.sources.file_of(piece.global_start);
            let in_build_file = planner.entries.iter().any(|e| {
                e.host.is_some_and(|h| self.modules.get(h).contains_file(&self.mem, piece_file))
            });
            if !in_build_file {
                continue;
            }
            let source = self.ast.sources.get(piece_file);
            let build_file =
                planner.plan.strings.intern(self.ast.idents.get_string(source.file_path));
            origins.push(TextOrigin {
                start: piece_start,
                end: piece_end,
                build_file,
                offset: source.local_from_global_offset(piece.global_start),
                len: piece.len,
            });
        }
    }

    fn order_plan_modules(&mut self, mut planner: Planner, order: &[usize]) -> BuildPlan {
        let mut modules: SV8<PlannedModule> = SV8::new();
        let k1_home = self.ast.idents.get_string(self.config.k1_home);
        let tmp = self.get_tmp_unsafe();
        planner.plan.push_fixed_modules(&mut modules, &self.ast.idents, tmp, k1_home);
        let mut ids: SV8<ModuleId> = smallvec![ModuleId::PENDING; planner.entries.len()];
        for (offset, &index) in order.iter().enumerate() {
            ids[index] = ModuleId::from_u32((modules.len() + offset + 1) as u32).unwrap();
        }
        let mut excludes: SV8<StringId> = SV8::new();
        for m in &modules {
            excludes.push(m.path);
        }
        for &index in order {
            let entry = &planner.entries[index];
            let mut m = entry.module;
            let mut deps: SV8<ModuleId> = SV8::new();
            for dep in &entry.deps {
                deps.push(ids[*dep]);
            }
            m.deps = planner.plan.mem.pushn(&deps);
            m.submodule_owner = entry.submodule_owner.map(|submodule_owner| ids[submodule_owner]);
            if m.is_dir {
                excludes.push(m.path);
            }
            excludes.extend_from_slice(&entry.ignore);
            modules.push(m);
        }
        planner.plan.modules = planner.plan.mem.pushn(&modules);
        planner.plan.excludes = planner.plan.mem.pushn(&excludes);
        planner.plan
    }
}
