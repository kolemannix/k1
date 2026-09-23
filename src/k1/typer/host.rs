// Copyright (c) 2026 knix
// All rights reserved.

use super::*;
use crate::compiler::{
    BuildConfig, BuildRequest, Command, CompileProgramError, CompilerConfig, LspCompileOptions,
    ModuleSources, Target,
};
use crate::parse::Interner;
use crate::plan::{
    BUILD_FILE_NAME, BuildPlan, ParamsProvider, PlanMem, PlannedLib, PlannedModule, PlannedSetup,
};

const PRIMARY: usize = 0;

#[derive(Clone)]
struct Entry {
    module: PlannedModule,
    host: Option<ModuleId>,
    manifest_span: SpanId,
    resolved_by: usize,
    deps: SV8<u32>,
}

struct Planner {
    plan: BuildPlan,
    entries: SV8<Entry>,
    providers: SV8<(usize, ParamsProvider)>,
}

type Pending = SV8<(usize, Option<ModuleSources>)>;

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
            "Unsupported host platform; fn build runs on the host",
        ));
    };
    let host_build = BuildConfig::new(host_target, &mut host_strings)
        .resolve(&mut host_strings)
        .map_err(CompileProgramError::Build)?;
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
        let mut planner = Planner { plan, entries: SV8::new(), providers: SV8::new() };
        let src_path = self.ast.idents.get_string(self.config.src_path);
        let primary_name = self.ast.idents.get_string(self.ast.name_id);
        let mut pending: Pending = SV8::new();
        let is_dir = Path::new(src_path).is_dir();
        self.reach(&mut planner, primary_name, src_path, is_dir, PRIMARY, &mut pending);

        let mut build_value = None;
        let mut wave = 0;
        while !pending.is_empty() {
            let frame = self.trace_push(TraceKind::HostWave, wave, 0);
            let result = self.run_wave(&mut planner, pending, &mut build_value, request);
            self.trace_pop(frame);
            pending = result?;
            wave += 1;
        }

        let order = self.order_modules(&planner)?;
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
            manifest_span: SpanId::NONE,
            resolved_by,
            deps: SV8::new(),
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
                let files = self.read_module_sources(read)?;
                let hash = files[0].content_hash;
                let name = self.ast.idents.intern(planner.plan.get(module.name));
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
                if let Some(params_span) = params_span {
                    let s = self.ast.spans.get(params_span);
                    let provider =
                        ParamsProvider { from: index as u32, offset: s.start, len: s.len };
                    planner.providers.push((dep, provider));
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
    ) -> K1Result<Option<(FunctionId, SpanId)>> {
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
        let build_fn = match planner.entries[PRIMARY].host {
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
            None => (request.default, SpanId::NONE),
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
        span: SpanId,
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
        if planner.plan.get(planner.entries[PRIMARY].module.name) == name {
            return Ok(Some(PRIMARY));
        }
        let primary_dir = planner.plan.module_dir(&planner.entries[PRIMARY].module);
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
    ) -> K1Result<SV8<(StringId, Option<SpanId>)>> {
        let mut deps: SV8<(StringId, Option<SpanId>)> = SV8::new();
        planner.entries[index].module.kind =
            if index == PRIMARY { ModuleKind::Executable } else { ModuleKind::Library };
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
        if index != PRIMARY && planner.entries[index].module.kind == ModuleKind::Executable {
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
            let StaticValue::Int(TypedIntValue::U32(raw)) =
                *self.static_values.get(self.static_field(*dep, "params-span"))
            else {
                self.ice_span(span, "dep-entry params-span was not a u32");
            };
            let params_span = match SpanId::from_u32(raw) {
                None => None,
                Some(s) if self.ast.spans.span_pool.get_opt(s).is_none() => {
                    kbail!(self, span, "invalid params span for dependency {}", name)
                }
                Some(s) if self.ast.spans.get(s).file_id != build_file_id => kbail!(
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
            self.ice_span(SpanId::NONE, format!("static struct has no field '{name}'"))
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
            self.ice_span(SpanId::NONE, "enum value matches no member")
        };
        self.ast.idents.get_string(member.name)
    }

    fn enum_named(&mut self, enum_type: TypeId, name: &str) -> StaticValueId {
        let name_id = self.ast.idents.intern(name);
        let members = self.mem.getn(self.types.get(enum_type).expect_enum().member_values);
        let Some(member) = members.iter().find(|m| m.name == name_id) else {
            self.ice_span(SpanId::NONE, format!("enum has no member '{name}'"))
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
                self.ice_span(SpanId::NONE, format!("no value for field '{name}'"))
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
        span: SpanId,
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

    fn order_modules(&self, planner: &Planner) -> K1Result<SV8<usize>> {
        let mut state: SV8<u8> = smallvec![0; planner.entries.len()];
        let mut stack: SV8<usize> = SV8::new();
        let mut order: SV8<usize> = SV8::new();
        self.visit_module(planner, PRIMARY, &mut state, &mut stack, &mut order)?;
        Ok(order)
    }

    fn visit_module(
        &self,
        planner: &Planner,
        index: usize,
        state: &mut SV8<u8>,
        stack: &mut SV8<usize>,
        order: &mut SV8<usize>,
    ) -> K1Result<()> {
        match state[index] {
            2 => return Ok(()),
            1 => {
                let mut cycle: Vec<&str> = vec![];
                for open in stack.iter().skip_while(|open| **open != index) {
                    cycle.push(planner.plan.get(planner.entries[*open].module.name));
                }
                cycle.push(planner.plan.get(planner.entries[index].module.name));
                let span = planner.entries[*stack.last().unwrap()].manifest_span;
                kbail!(self, span, "Module dependency cycle: {}", cycle.join(" -> "));
            }
            _ => {}
        }
        state[index] = 1;
        stack.push(index);
        for dep in &planner.entries[index].deps {
            self.visit_module(planner, *dep as usize, state, stack, order)?;
        }
        stack.pop();
        state[index] = 2;
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
            let force = force_primary && index == PRIMARY;
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
            let dir_value = self.static_values.add_string(req.module_dir);
            let ctx_value = self.encode_struct(
                setup_ctx_type,
                &[("module-dir", dir_value), ("build", build_value)],
            );
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
            let mut providers: SV4<ParamsProvider> = SV4::new();
            for (to, provider) in &planner.providers {
                if *to == index {
                    providers.push(ParamsProvider {
                        from: position[provider.from as usize],
                        ..*provider
                    });
                }
            }
            m.providers = planner.plan.mem.pushn(&providers);
            modules.push(m);
        }
        planner.plan.modules = planner.plan.mem.pushn(&modules);
        planner.plan
    }
}
