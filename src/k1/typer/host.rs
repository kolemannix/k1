// Copyright (c) 2026 knix
// All rights reserved.

use super::*;
use crate::compiler::{
    BuildConfig, BuildRequest, Command, CompileProgramError, CompilerConfig, LspCompileOptions,
    ModuleSources, Target,
};
use crate::parse::Interner;
use crate::plan::{
    BUILD_FILE_NAME, BuildPlan, PlanChunk, PlanMem, PlannedLib, PlannedModule, PlannedSetup,
    PlannedSource,
};

const PRIMARY_MODULE_INDEX: usize = 0;

#[derive(Clone)]
struct Entry {
    module: PlannedModule,
    host: Option<ModuleId>,
    manifest_span: Span,
    resolved_by: usize,
    deps: SV8<u32>,
    params_expr_span: Option<Span>,
}

struct Planner {
    plan: BuildPlan,
    entries: SV8<Entry>,
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
    let host_config =
        CompilerConfig { command: Command::Check, ..config.reintern(config_idents, &ast.idents) };
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
        source_overrides: lsp.source_overrides.clone(),
        completion: false,
        progress_sink: None,
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
        let mut planner = Planner { plan, entries: SV8::new() };
        let src_path = self.ast.idents.get_string(self.config.src_path);
        let primary_name = self.ast.idents.get_string(self.ast.name_id);
        let mut pending: Pending = SV8::new();
        let is_dir = Path::new(src_path).is_dir();
        self.reach(
            &mut planner,
            primary_name,
            src_path,
            is_dir,
            PRIMARY_MODULE_INDEX,
            &mut pending,
        );

        let mut build_value = None;
        let mut wave = 0;
        while !pending.is_empty() {
            let frame = self.trace_push(TraceKind::HostWave, wave, 0);
            let result = self.run_wave(&mut planner, pending, &mut build_value, request);
            self.trace_pop(frame);
            pending = result?;
            wave += 1;
        }

        // Every manifest has run, so the primary's dep-params are known: evaluate
        // each module's deferred k1/module-params global against them
        self.module_params_phase = ModuleParamsPhase::HostSetup;
        for global_id in std::mem::take(&mut self.deferred_module_params) {
            let span = self.globals.get(global_id).span;
            let module_id = self.module_of_span(span);
            let Some(index) = planner.entries.iter().position(|e| e.host == Some(module_id)) else {
                self.ice_span(span, "module params global outside any planned module")
            };
            let params = self.evaluate_module_params(&mut planner, index, global_id)?;
            planner.entries[index].module.params = Some(params);
        }
        for entry in &planner.entries {
            if let Some(span) = entry.params_expr_span
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

        let order = self.get_dependency_order(&planner)?;
        let force_primary = matches!(request.command, Command::Setup { force: true });
        self.run_setups(&planner, &order, build_value.unwrap(), force_primary)?;
        Ok(self.finish_plan(planner, &order))
    }

    fn reach(
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
        planner.entries.push(Entry {
            module,
            host: None,
            manifest_span: Span::NONE,
            resolved_by,
            deps: SV8::new(),
            params_expr_span: None,
        });
        let overrides = &self.lsp.source_overrides;
        let build_path = if is_dir {
            let tmp = self.get_tmp_unsafe();
            crate::plan::build_file_path(&self.ast.idents, tmp, path, overrides)
        } else {
            None
        };
        let read = build_path.map(|build_path| {
            let build_path = self.ast.idents.get_string(build_path);
            crate::compiler::spawn_module_sources_read(build_path, false, false, overrides)
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
            let deps = self.evaluate_manifest(planner, index, build_value)?;
            for (dep_name, params_span) in deps {
                let span = params_span.unwrap_or(planner.entries[index].manifest_span);
                let Some(dep) =
                    self.resolve_planned_dep(planner, index, dep_name, span, &mut next)?
                else {
                    if params_span.is_some() {
                        kbail!(self, span, "Module '{}' accepts no parameters", dep_name);
                    }
                    continue;
                };
                if let Some(s) = params_span {
                    if index != PRIMARY_MODULE_INDEX {
                        kbail!(
                            self,
                            s,
                            "Only the primary configures module params; '{}' cannot set params for '{}'",
                            planner.plan.get(planner.entries[index].module.name),
                            dep_name
                        );
                    }
                    planner.entries[dep].params_expr_span = Some(s);
                }
                planner.entries[index].deps.push(dep as u32);
            }
        }
        Ok(next)
    }

    fn host_fn(
        &self,
        module: ModuleId,
        name: StringId,
        param_type: TypeId,
        return_type: Option<TypeId>,
        signature: &str,
    ) -> K1Result<Option<(FunctionId, Span)>> {
        let scope = self.modules.get(module).namespace_scope_id;
        let Some(fn_id) = self.scopes.find_function_local(scope, name) else { return Ok(None) };
        let function = self.get_function(fn_id);
        let span = self.ast.get_span_for_id(function.parsed_id);
        let fn_type = self.get_function_type(fn_id);
        let params = self.mem.getn(fn_type.logical_params());
        if !function.type_params.is_empty()
            || params.len() != 1
            || params[0].type_id != param_type
            || return_type.is_some_and(|t| fn_type.return_type != t)
        {
            kbail!(self, span, "expected {}", signature);
        }
        Ok(Some((fn_id, span)))
    }

    fn configure(
        &mut self,
        planner: &mut Planner,
        request: &BuildRequest,
    ) -> K1Result<StaticValueId> {
        let config_type = self.builtin_types.k1_build_config.unwrap();
        let request_type = self.builtin_types.k1_build_request.unwrap();
        let build_fn = match planner.entries[PRIMARY_MODULE_INDEX].host {
            Some(host) => self.host_fn(
                host,
                self.ast.idents.b.build,
                request_type,
                Some(config_type),
                "fn build(req: k1/build-request): k1/build-config",
            )?,
            None => None,
        };
        let (unresolved, span) = match build_fn {
            None => (request.default, Span::NONE),
            Some((fn_id, span)) => {
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
        Ok(self.encode_build_config(&planner.plan, &config))
    }

    fn resolve_planned_dep(
        &mut self,
        planner: &mut Planner,
        dependent: usize,
        dep_name: StringId,
        span: Span,
        pending: &mut Pending,
    ) -> K1Result<Option<usize>> {
        let name = self.ast.idents.get_string(dep_name);
        match name {
            "core" => return Ok(None),
            "std" if planner.plan.config.no_std => {
                kbail!(self, span, "module 'std' is excluded by no-std")
            }
            "std" => return Ok(None),
            _ => {}
        }
        if planner.plan.get(planner.entries[PRIMARY_MODULE_INDEX].module.name) == name {
            return Ok(Some(PRIMARY_MODULE_INDEX));
        }
        let primary_dir = planner.plan.module_dir(&planner.entries[PRIMARY_MODULE_INDEX].module);
        let dependent_dir = planner.plan.module_dir(&planner.entries[dependent].module);
        let k1_home = self.ast.idents.get_string(self.config.k1_home);
        let tmp = self.get_tmp_unsafe();
        let mark = tmp.mark();
        let resolved = crate::plan::resolve_dep(
            &self.ast.idents,
            tmp,
            primary_dir,
            dependent_dir,
            k1_home,
            name,
        );
        tmp.reset_to(mark);
        let dir = match resolved {
            Ok(dir) => self.ast.idents.get_string(dir),
            Err(message) => kbail!(self, span, "{}", message),
        };
        for (index, existing) in planner.entries.iter().enumerate() {
            if planner.plan.get(existing.module.name) != name {
                continue;
            }
            let existing_dir = planner.plan.get(existing.module.path);
            if existing_dir != dir {
                kbail!(
                    self,
                    span,
                    "module '{}' resolves to two dirs: {} (for '{}') and {} (for '{}'); add {}/deps/{} to pick one",
                    name,
                    existing_dir,
                    planner.plan.get(planner.entries[existing.resolved_by].module.name),
                    dir,
                    planner.plan.get(planner.entries[dependent].module.name),
                    primary_dir,
                    name
                );
            }
            return Ok(Some(index));
        }
        Ok(Some(self.reach(planner, name, dir, true, dependent, pending)))
    }

    fn evaluate_manifest(
        &mut self,
        planner: &mut Planner,
        index: usize,
        build_value: StaticValueId,
    ) -> K1Result<SV8<(StringId, Option<Span>)>> {
        let mut deps: SV8<(StringId, Option<Span>)> = SV8::new();
        planner.entries[index].module.kind = if index == PRIMARY_MODULE_INDEX {
            ModuleKind::Executable
        } else {
            ModuleKind::Library
        };
        let Some(host) = planner.entries[index].host else { return Ok(deps) };
        let module_type = self.builtin_types.k1_module.unwrap();
        let config_type = self.builtin_types.k1_build_config.unwrap();
        let Some((fn_id, span)) = self.host_fn(
            host,
            self.ast.idents.b.module,
            config_type,
            Some(module_type),
            "fn module(b: k1/build-config): k1/module",
        )?
        else {
            return Ok(deps);
        };
        planner.entries[index].manifest_span = span;
        let manifest = self.execute_static_function(fn_id, &[build_value], span)?;

        if let Some(kind) = self.field_opt(manifest, "kind") {
            planner.entries[index].module.kind = match self.enum_name(kind) {
                "executable" => ModuleKind::Executable,
                _ => ModuleKind::Library,
            };
        }
        if index != PRIMARY_MODULE_INDEX
            && planner.entries[index].module.kind == ModuleKind::Executable
        {
            kbail!(
                self,
                span,
                "Cannot compile a program with 2 executable modules. {}",
                planner.plan.get(planner.entries[index].module.name)
            );
        }

        let build_file_id = self.modules.get(host).files.as_slice(&self.mem)[0];
        for dep in self.field_list(manifest, "deps") {
            let name = self.ast.idents.intern(self.field_str(*dep, "name"));
            let StaticValue::Int(TypedIntValue::U64(raw)) =
                *self.static_values.get(self.static_field(*dep, "params-span"))
            else {
                self.ice_span(span, "dep-entry params-span was not a u64");
            };
            let params_span = match Span::from_encoded_k1_u64(raw) {
                None => None,
                Some(s) if !self.ast.sources.contains_span(s) => {
                    kbail!(self, span, "invalid params span for dependency {}", name)
                }
                Some(s) if self.ast.sources.file_of(s.global_start) != build_file_id => kbail!(
                    self,
                    s,
                    "dep params for '{}' must be written in the module's build.k1",
                    name
                ),
                Some(s) => Some(s),
            };
            deps.push((name, params_span));
        }

        let mut libs: SV8<PlannedLib> = SV8::new();
        for lib in self.field_list(manifest, "libs") {
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
        let setup = match self.field_opt(manifest, "setup") {
            None => None,
            Some(setup) => Some(PlannedSetup {
                outputs: self.plan_strings(&mut planner.plan, self.field_list(setup, "outputs")),
                inputs: self.plan_strings(&mut planner.plan, self.field_list(setup, "inputs")),
            }),
        };
        let module = &mut planner.entries[index].module;
        module.libs = libs;
        module.link_args = link_args;
        module.setup = setup;
        Ok(deps)
    }

    fn plan_strings(
        &self,
        plan: &mut BuildPlan,
        values: &[StaticValueId],
    ) -> MSlice<StringId, PlanMem> {
        let mut out: SV8<StringId> = SV8::new();
        for value in values {
            let s = self.static_values.get(*value).as_string().unwrap();
            out.push(plan.strings.intern(self.ast.idents.get_string(s)));
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
        let s = self.static_values.get(self.static_field(value, name)).as_string().unwrap();
        self.ast.idents.get_string(s)
    }

    fn field_bool(&self, value: StaticValueId, name: &str) -> bool {
        self.static_values.get(self.static_field(value, name)).as_boolean().unwrap()
    }

    fn field_list(&self, value: StaticValueId, name: &str) -> &'static [StaticValueId] {
        let list = *self.static_values.get(self.static_field(value, name)).as_container().unwrap();
        self.static_values.mem.getn(list.elements)
    }

    fn field_opt(&self, value: StaticValueId, name: &str) -> Option<StaticValueId> {
        self.static_values.get(self.static_field(value, name)).as_sum().unwrap().payload
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

    fn static_bool(&self, b: bool) -> StaticValueId {
        if b { self.static_values.true_id() } else { self.static_values.false_id() }
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
            ("optimize", self.static_bool(config.optimize)),
            ("debug", self.static_bool(config.debug)),
            ("no-std", self.static_bool(config.no_std)),
            ("sanitize", self.static_bool(config.sanitize)),
            ("filc", self.static_bool(config.filc)),
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
        let fields =
            [("default", default), ("command", command), ("options", options), ("host", host)];
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
            self.visit_module(planner, *dep as usize, state, stack, order)?;
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
            let span = entry.manifest_span;
            let frame = self.trace_push(TraceKind::SetupStamp, req.module_name.as_u32(), 0);
            let tmp = self.get_tmp_unsafe();
            let mark = tmp.mark();
            let started = crate::compiler::start_setup(&self.ast.idents, &req, tmp)
                .map_err(|e| self.error_from_anyhow(e, span));
            tmp.reset_to(mark);
            self.trace_pop(frame);
            let Some(started) = started? else { continue };
            let setup_ctx_type = self.builtin_types.k1_setup_ctx.unwrap();
            let Some((setup_fn, fn_span)) = self.host_fn(
                entry.host.unwrap(),
                self.ast.idents.b.setup,
                setup_ctx_type,
                None,
                "fn setup(ctx: k1/setup-ctx)",
            )?
            else {
                kbail!(
                    self,
                    span,
                    "a module that declares setup needs a `fn setup` in its build.k1"
                );
            };
            self.trace_clear();
            eprintln!(
                "Setting up module '{}' (running fn setup in {}/{})...",
                planner.plan.get(entry.module.name),
                planner.plan.module_dir(&entry.module),
                BUILD_FILE_NAME
            );
            let ctx_value = self.encode_struct(setup_ctx_type, &[("build", build_value)]);
            let frame = self.trace_push(TraceKind::SetupFn, req.module_name.as_u32(), 0);
            let ran = self.execute_static_function(setup_fn, &[ctx_value], fn_span);
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
    ) -> K1Result<PlannedSource> {
        let global = *self.globals.get(global_id);
        let host = planner.entries[index].host.unwrap();
        let module_name = planner.plan.get(planner.entries[index].module.name);
        if global.parent_scope != self.modules.get(host).namespace_scope_id {
            kbail!(
                self,
                global.span,
                "k1/module-params must initialize a top-level global of build.k1"
            );
        }
        let schema_type = global.type_id;
        let value_expr = global.parsed_expr.unwrap();
        let call_span = self.ast.exprs.get_span(value_expr);
        let args = self.ast.mem.getn(self.find_module_params_call_args(value_expr).unwrap());
        let deps_provider_expr_span = planner.entries[index].params_expr_span;
        let defaults = match args {
            [] if deps_provider_expr_span.is_none() => {
                kbail!(self, call_span, "Module '{}' requires parameters", module_name)
            }
            [] => None,
            [defaults] => Some(self.ast.exprs.get_span(defaults.value)),
            _ => {
                kbail!(self, call_span, "k1/module-params takes at most one argument: the defaults")
            }
        };

        let mut expr = String::new();
        let mut expr_plan_chunks: SV8<PlanChunk> = SV8::new();
        let mut emitted_chunks: SV4<CodeChunkPos> = SV4::new();
        for span in [defaults, deps_provider_expr_span].into_iter().flatten() {
            if !expr.is_empty() {
                expr.push_str(" | ");
            }
            self.push_plan_chunk(planner, span, &mut expr, &mut expr_plan_chunks);
            let c = expr_plan_chunks.last().unwrap();
            emitted_chunks.push(CodeChunkPos { start: c.start, end: c.end, source: span });
        }
        let emitted_chunks = self.mem.pushn(&emitted_chunks);
        let source_file_id = self.add_emitted_source(&expr, emitted_chunks, call_span);
        let ParseMetaprogramResult::Expr(parsed) =
            self.parse_metaprogram_source(host, source_file_id, ParseAdHocKind::Expr)?
        else {
            self.ice_span(call_span, "module params parsed as definitions");
        };
        let ctx = EvalExprContext::make(Scopes::ROOT_SCOPE_ID)
            .with_expected_type(Some(schema_type))
            .with_static_ctx(Some(StaticExecContext { expected_return_type: Some(schema_type) }));
        let value_id = self.execute_static_expr(parsed, ctx, &[])?;
        let value_type = self.get_static_value_type(value_id);
        if let Err(msg) = self.check_types(schema_type, value_type, Scopes::ROOT_SCOPE_ID) {
            kbail!(
                self,
                call_span,
                "Type mismatch in params for module '{}': {}",
                module_name,
                msg
            );
        }
        self.globals.get_mut(global_id).initial_value = GlobalInitialValue::Value(value_id);

        // The value above serves fn setup in this host program; the program being built
        // shares nothing with it but the plan, so it gets the same `let` as source text
        let parsed_global = self.ast.globals.get(global.ast_id);
        let name_start = parsed_global.name_span.global_start;
        let schema_end = self.ast.get_type_expr_span(parsed_global.type_expr).end();
        let mut text = String::from("let ");
        let mut let_plan_chunks: SV8<PlanChunk> = SV8::new();
        self.push_plan_chunk(
            planner,
            Span { global_start: name_start, len: schema_end - name_start },
            &mut text,
            &mut let_plan_chunks,
        );
        text.push_str(" = ");
        let shift = text.len() as u32;
        for c in &expr_plan_chunks {
            let_plan_chunks.push(PlanChunk { start: c.start + shift, end: c.end + shift, ..*c });
        }
        text.push_str(&expr);
        text.push('\n');
        let chunks = planner.plan.mem.pushn(&let_plan_chunks);
        Ok(PlannedSource { text: planner.plan.strings.intern(&text), chunks })
    }

    fn push_plan_chunk(
        &self,
        planner: &Planner,
        span: Span,
        text: &mut String,
        chunks: &mut SV8<PlanChunk>,
    ) {
        let file_id = self.ast.sources.file_of(span.global_start);
        let source = self.ast.sources.get(file_id);
        let offset = source.local_from_global_offset(span.global_start);
        let from = planner
            .entries
            .iter()
            .position(|e| {
                e.host.is_some_and(|h| self.modules.get(h).contains_file(&self.mem, file_id))
            })
            .unwrap() as u32;
        let start = text.len() as u32;
        text.push_str(
            &source.content(&self.ast.mem)[offset as usize..(offset + span.len) as usize],
        );
        chunks.push(PlanChunk { start, end: text.len() as u32, from, offset, len: span.len });
    }

    fn finish_plan(&mut self, mut planner: Planner, order: &[usize]) -> BuildPlan {
        let mut modules: SV8<PlannedModule> = SV8::new();
        let k1_home = self.ast.idents.get_string(self.config.k1_home);
        let tmp = self.get_tmp_unsafe();
        planner.plan.push_fixed_modules(&mut modules, &self.ast.idents, tmp, k1_home);
        let mut position: SV8<u32> = smallvec![0; planner.entries.len()];
        for (offset, &index) in order.iter().enumerate() {
            position[index] = (modules.len() + offset) as u32;
        }
        for &index in order {
            let entry = &planner.entries[index];
            let mut m = entry.module;
            let mut deps: SV8<u32> = SV8::new();
            for dep in &entry.deps {
                deps.push(position[*dep as usize]);
            }
            m.deps = planner.plan.mem.pushn(&deps);
            if let Some(params) = m.params {
                let mut chunks: SV8<PlanChunk> = SV8::new();
                for c in planner.plan.mem.getn(params.chunks) {
                    chunks.push(PlanChunk { from: position[c.from as usize], ..*c });
                }
                let chunks = planner.plan.mem.pushn(&chunks);
                m.params = Some(PlannedSource { chunks, ..params });
            }
            modules.push(m);
        }
        planner.plan.modules = planner.plan.mem.pushn(&modules);
        planner.plan
    }
}
