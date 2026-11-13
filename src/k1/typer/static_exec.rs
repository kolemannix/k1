// Copyright (c) 2026 knix
// All rights reserved.

use crate::vm::k1_types;

use super::trace::{FrameId, TraceKind};
use super::*;

impl TypedProgram {
    pub(super) fn convert_trivial_static_expr(
        &mut self,
        expr_id: TypedExprId,
    ) -> Option<StaticValueId> {
        match self.exprs.get(expr_id) {
            TypedExpr::StaticValue(s) => Some(s.value_id),
            TypedExpr::Variable(v) => {
                let global_id = self.variables.get(v.variable_id).global_id()?;
                if !self.globals.get(global_id).is_constant {
                    return None;
                }
                if self.globals.get(global_id).initial_value.is_pending() {
                    if let Err(e) = self.eval_global_body(global_id) {
                        self.report(e);
                        return None;
                    }
                }
                self.globals.get(global_id).initial_value.as_value()
            }
            _ => None,
        }
    }

    pub(super) fn convert_trivial_static_block(
        &mut self,
        expr_id: TypedExprId,
    ) -> Option<StaticValueId> {
        let TypedExpr::Block(b) = self.exprs.get(expr_id) else { return None };

        match self.mem.getn(b.statements) {
            &[s1] => {
                let Some(expr_id) = self.stmts.get(s1).as_expr() else { return None };
                match self.exprs.get(expr_id) {
                    TypedExpr::Return(return_expr) => {
                        self.convert_trivial_static_expr(return_expr.value)
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn warn_if_not_zerosafe(&mut self, type_id: TypeId, span: Span) {
        if !self.get_type_variable_counts(type_id).is_zero_safe {
            self.report_warn(span, k1_format_user!(self, "Type {} is not zero-safe", type_id))
        }
    }

    pub fn require_function_body(&mut self, function_id: FunctionId, span: Span) -> K1Result<()> {
        if let Err(e) = self.eval_function_body(function_id) {
            self.report(e)
        }
        let function = self.get_function(function_id);
        let needs_body = function.builtin_type.is_none() && !function.linkage.is_external();
        match function.body_failure {
            Some(_) => {
                kbail!(self, span, "Function '{}' failed to compile", function.name)
            }
            None if needs_body && function.body_block.is_none() => {
                kbail!(
                    self,
                    span,
                    "Function '{}' has no body",
                    self.function_id_to_string(function_id, false)
                )
            }
            None => Ok(()),
        }
    }

    pub(super) fn is_generic_pass(&self, scope_id: ScopeId) -> bool {
        let mut scope_id = scope_id;
        loop {
            let scope = self.scopes.get_scope(scope_id);
            if scope.scope_type == ScopeType::StaticBody {
                return false;
            }
            match scope.owner_id {
                ScopeOwnerId::Function(function_id) => {
                    return !self.get_function(function_id).is_concrete();
                }
                ScopeOwnerId::GenericSignature => return true,
                _ => match scope.parent {
                    Some(parent) => scope_id = parent,
                    None => return false,
                },
            }
        }
    }

    pub(super) fn synth_deferred_static(
        &mut self,
        ctx: EvalExprContext,
        span: Span,
    ) -> K1Result<TypedExprId> {
        let mut scope_id = Some(ctx.scope_id);
        while let Some(id) = scope_id {
            let scope = self.scopes.get_scope(id);
            if scope.scope_type == ScopeType::Namespace {
                break;
            }
            scope_id = scope.parent;
            for (_, variable_id, _) in self.scopes.iter_scope_variables(id) {
                self.variables.get_mut(variable_id).usage_count += 1;
            }
        }
        let expected_type = match ctx.expected_type_id {
            Some(expected_type) => expected_type,
            None if ctx.is_discarded() => self.builtin_types.empty,
            None => kbail!(
                self,
                span,
                "The type of this compile-time result is unknown in generic code; it needs an expected type, such as a type on its variable"
            ),
        };
        Ok(self.synth_phony(expected_type, span))
    }

    pub fn compile_function_for_exec(
        &mut self,
        function_id: FunctionId,
        requester_frame: Option<FrameId>,
        span: Span,
    ) -> K1Result<()> {
        if let Err(e) = ir::compile_function(self, function_id, requester_frame)
            && self.get_function(function_id).body_failure.is_none()
        {
            self.get_function_mut(function_id).body_failure = Some(e);
            self.report(e);
        }
        self.require_function_body(function_id, span)
    }

    pub fn compile_all_pending_ir(&mut self, on_behalf_of_span: Span) -> K1Result<()> {
        loop {
            if let Some((function_id, requester)) = self.ir.units_pending_compile.pop() {
                if let Err(e) =
                    self.compile_function_for_exec(function_id, requester, on_behalf_of_span)
                {
                    self.ir.units_pending_compile.push(function_id, requester);
                    return Err(e);
                }
            } else if let Some((global_id, ())) = self.ir.globals_pending_eval.pop() {
                if let Err(e) = self.eval_global_body(global_id) {
                    self.report(e)
                }
                if let GlobalInitialValue::Failed(_) = self.globals.get(global_id).initial_value {
                    self.ir.globals_pending_eval.push(global_id, ());
                    kbail!(
                        self,
                        on_behalf_of_span,
                        "Global '{}' failed to compile",
                        self.variables.get(self.globals.get(global_id).variable_id).name
                    );
                }
            } else {
                break;
            }
        }
        Ok(())
    }

    pub(super) fn execute_parsed_expr_with_vm(
        &mut self,
        vm: &mut vm::Vm,
        parsed_expr: ParsedExprId,
        ctx: EvalExprContext,
    ) -> K1Result<StaticValueId> {
        let expr = self.typecheck_parsed_expr_for_exec(parsed_expr, ctx)?;

        // Intercepts very simple expressions to avoid a VM execution
        if let Some(shortcut_value_id) = self.convert_trivial_static_block(expr) {
            return Ok(shortcut_value_id);
        }
        self.compile_static_unit(expr, self.ast.exprs.is_debug(parsed_expr))?;
        let execution_result = bc::exec::execute_compiled_expr(self, vm, expr, true);

        vm.reset(self.global_id_k1_arena);

        let static_value_id = execution_result?;

        Ok(static_value_id)
    }

    pub(super) fn typecheck_parsed_expr_for_exec(
        &mut self,
        parsed_expr: ParsedExprId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        if let ParsedExpr::Static(_static) = self.ast.exprs.get(parsed_expr) {
            self.report_warn(
                self.ast.exprs.get_span(parsed_expr),
                "This #static is immediately inside a static",
            )
        }

        let parsed_expr_as_block =
            self.ensure_parsed_expr_to_block(parsed_expr, ParsedBlockKind::FunctionBody);
        let static_block_scope =
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::StaticBody, ScopeOwnerId::None);

        let static_eval_ctx = EvalExprContext {
            scope_id: static_block_scope,
            static_expected_return_type: ctx.expected_type_id,
            flags: (ctx.flags | EvalExprFlags::Static) - EvalExprFlags::Inference,
            ..ctx
        };
        let expr = self.eval_block(&parsed_expr_as_block, static_eval_ctx, true)?;
        if self.ast.exprs.is_debug(parsed_expr) {
            eprintln!("COMPILED TO BLOCK\n\n{}", self.expr_to_string_with_type(expr));
        }
        Ok(expr)
    }

    pub(super) fn compile_static_unit(
        &mut self,
        expr: TypedExprId,
        is_debug: bool,
    ) -> K1Result<()> {
        ir::compile_top_level_expr(self, expr, is_debug)?;
        ir::optimize_unit(self, IrUnitId::Expr(expr))?;
        if is_debug {
            eprintln!(
                "executing optimized unit.\n{}",
                ir::unit_to_string(self, IrUnitId::Expr(expr), false)
            );
        }
        Ok(())
    }

    pub(super) fn execute_static_expr(
        &mut self,
        parsed_expr: ParsedExprId,
        ctx: EvalExprContext,
    ) -> K1Result<StaticValueId> {
        let span = self.ast.exprs.get_span(parsed_expr);
        self.do_with_vm(span, ctx.trace_flags(), |k1, vm| {
            k1.execute_parsed_expr_with_vm(vm, parsed_expr, ctx)
        })
    }

    pub(super) fn do_with_vm<T>(
        &mut self,
        span: Span,
        trace_flags: u8,
        f: impl FnOnce(&mut TypedProgram, &mut vm::Vm) -> T,
    ) -> T {
        let frame = self.trace_push(TraceKind::StaticExec, span.global_start, trace_flags);
        let (mut vm, used_alt) = match *std::mem::take(&mut self.vm) {
            None => {
                let maybe_alt = self.vm_alts.pop();
                let alt_vm = match maybe_alt {
                    None => {
                        // self.report_warn(span, "Had to make a new alt VM");
                        let new_vm = vm::Vm::make();
                        new_vm
                    }
                    Some(alt_vm) => alt_vm,
                };
                (alt_vm, true)
            }
            Some(vm) => (vm, false),
        };
        let res = f(self, &mut vm);
        if !used_alt {
            *self.vm = Some(vm);
        } else {
            debug!("Restoring alt VM to pool");
            self.vm_alts.push(vm);
        }
        self.trace_pop(frame);
        res
    }

    pub(super) fn execute_static_function(
        &mut self,
        function_id: FunctionId,
        function_parameters: &[StaticValueId],
        span: Span,
    ) -> K1Result<StaticValueId> {
        self.do_with_vm(span, 0, |k1, vm| {
            let result =
                Self::static_exec_function_with_vm(k1, vm, function_id, function_parameters, span);
            vm.reset(k1.global_id_k1_arena);
            result
        })
    }

    pub fn run_static_tests(&mut self, tests: &[TestCase]) {
        for test in tests {
            if !self.get_function(test.function_id).test_runs_static() {
                continue;
            }
            let start = std::time::Instant::now();
            let (result, output) = self.do_with_vm(test.span, 0, |k1, vm| {
                vm.quiet_messages = true;
                let result = Self::static_exec_prepare_function(k1, test.thunk_id, test.span)
                    .and_then(|()| {
                        bc::exec::execute_compiled_function(k1, vm, test.thunk_id, &[], false)
                    });
                vm.quiet_messages = false;
                let mut output = String::new();
                for message in vm.compiler_messages.drain(..) {
                    output.push_str(k1.get_string(message.message));
                    if message.level == MessageLevel::Error {
                        let (source, line) = k1.get_span_location(message.span);
                        let filename = source.filename_str(&k1.ast.idents);
                        output.push_str(&format!(" at {filename}:{}\n", line.line_number()));
                    }
                }
                vm.reset(k1.global_id_k1_arena);
                (result, output)
            });
            let ending = match result {
                Err(e) => Some(self.ident_str(e.message).to_string()),
                Ok(value_id) => {
                    let payload = self.static_values.get(value_id).as_sum().unwrap().payload;
                    match payload {
                        None => None,
                        Some(message) => {
                            let message = self.static_values.get(message).as_string().unwrap();
                            Some(self.ident_str(message).to_string())
                        }
                    }
                }
            };
            let mut haystack = output;
            if !haystack.is_empty() && !haystack.ends_with('\n') {
                haystack.push('\n');
            }
            let failure = match (test.expected_failure, ending) {
                (None, None) => None,
                (None, Some(ending)) => Some(format!("{haystack}{ending}")),
                (Some(needle), None) => Some(format!(
                    "expected a failure containing \"{}\", but the test passed",
                    self.ident_str(needle)
                )),
                (Some(needle), Some(ending)) => {
                    let needle = self.ident_str(needle);
                    haystack.push_str(&ending);
                    if haystack.contains(needle) {
                        None
                    } else {
                        Some(format!(
                            "expected a failure containing \"{needle}\"; got:\n{haystack}"
                        ))
                    }
                }
            };
            let failure = match failure {
                None => None,
                Some(failure) => Some(self.ast.idents.intern(failure)),
            };
            self.test_results.push(StaticTestResult {
                name: test.name,
                span: test.span,
                failure,
                elapsed: start.elapsed(),
            });
        }
    }

    pub fn build_test_suite(&mut self, tests: &[TestCase]) -> K1Result<()> {
        let Some(suite_fn) = self
            .find_function_namespaced(self.scopes.core_scope_id, &self.ast.idents.f.test_suite)?
        else {
            self.ice("core is missing k1/test/suite", None)
        };
        let suite_type = self.get_function_type(suite_fn).return_type;
        let suite_fields = self.types.get(suite_type).expect_struct().fields;
        let cases_type = self.mem.getn(suite_fields)[1].type_id;
        let case_type = self.get_instance_arg(cases_type, self.builtin_types.span()).unwrap();
        let case_fields = self.types.get(case_type).expect_struct().fields;
        let mut cases: Vec<StaticValueId> = Vec::with_capacity(tests.len());
        for test in tests {
            if !self.get_function(test.function_id).test_runs_runtime() {
                continue;
            }
            let mut fields: SV4<StaticValueId> = smallvec![];
            for field in self.mem.getn(case_fields).to_vec() {
                let value = match self.ident_str(field.name) {
                    "name" => self.static_values.add_string(test.name),
                    "location" => self.static_source_location(test.span, Some(test.function_id)),
                    "run-fn" => self.add_static_function_pointer(field.type_id, test.thunk_id),
                    "expected-failure" => {
                        let (variant_index, payload) = match test.expected_failure {
                            Some(needle) => (
                                OPT_SOME_VARIANT_INDEX,
                                Some(self.static_values.add_string(needle)),
                            ),
                            None => (OPT_NONE_VARIANT_INDEX, None),
                        };
                        self.static_values.add(StaticValue::Sum(StaticSum {
                            sum_type_id: field.type_id,
                            variant_index: variant_index as u32,
                            payload,
                        }))
                    }
                    _ => self.ice_span(field.span, "unexpected core/test/test-case field"),
                };
                fields.push(value);
            }
            cases.push(self.static_values.add_struct_from_slice(case_type, &fields));
        }
        let cases =
            self.add_static_container_from_ids(StaticContainerKind::Span, cases_type, &cases);
        let module_name = self.static_values.add_string(self.primary_module().name);
        self.test_suite =
            Some(self.static_values.add_struct_from_slice(suite_type, &[module_name, cases]));
        Ok(())
    }

    pub(super) fn static_exec_prepare_function(
        k1: &mut TypedProgram,
        function_id: FunctionId,
        span: Span,
    ) -> K1Result<()> {
        let requester = k1.trace.top();
        k1.compile_function_for_exec(function_id, requester, span)?;
        ir::optimize_unit(k1, IrUnitId::Function(function_id))
    }

    /// Compile, optimize, and run; no VM reset, so callers control when the
    /// result's VM memory dies
    pub(super) fn static_exec_function_with_vm(
        k1: &mut TypedProgram,
        vm: &mut vm::Vm,
        function_id: FunctionId,
        function_parameters: &[StaticValueId],
        span: Span,
    ) -> K1Result<StaticValueId> {
        Self::static_exec_prepare_function(k1, function_id, span)?;
        bc::exec::execute_compiled_function(k1, vm, function_id, function_parameters, true)
    }

    pub(super) fn execute_static_condition(
        &mut self,
        cond: Option<ParsedExprId>,
        scope_id: ScopeId,
    ) -> bool {
        if let Some(condition_expr) = cond {
            match self.execute_static_bool(condition_expr, EvalExprContext::make(scope_id)) {
                Err(e) => {
                    self.report(e);
                    false
                }
                Ok(b) => b,
            }
        } else {
            true
        }
    }

    pub(super) fn execute_static_bool(
        &mut self,
        cond: ParsedExprId,
        ctx: EvalExprContext,
    ) -> K1Result<bool> {
        let vm_cond_result =
            self.execute_static_expr(cond, ctx.with_expected_type(Some(BOOL_TYPE_ID)))?;
        let StaticValue::Bool(condition_bool) = self.static_values.get(vm_cond_result) else {
            let cond_span = self.ast.exprs.get_span(cond);
            kbail!(self, cond_span, "Condition is not a boolean");
        };
        Ok(*condition_bool)
    }

    pub(super) fn declare_global(
        &mut self,
        parsed_global_id: ParsedGlobalId,
        scope_id: ScopeId,
    ) -> K1Result<Option<TypedGlobalId>> {
        if let ParsedGlobalDeclareOutcome::Declared(global_id) =
            self.ast.globals.get(parsed_global_id).typer_state
        {
            return Ok(Some(global_id));
        }
        let parsed = *self.ast.get_global(parsed_global_id);
        if !self.execute_static_condition(parsed.compile_condition, scope_id) {
            self.ast.globals.get_mut(parsed_global_id).typer_state =
                ParsedGlobalDeclareOutcome::IfDefedOut;
            self.emit_semantic_token(parsed.span, parse::SemanticTokenKind::Comment);
            return Ok(None);
        }
        let owner_ns = match self.scopes.get_scope(scope_id).owner_id {
            ScopeOwnerId::Namespace(ns) => ns,
            _ => kbail!(self, Span::NONE, "declare_global's scope_id should be an namespace"),
        };
        if parsed.is_thread_local {
            self.fail_if_reload_ns(owner_ns, parsed.span, "tls globals")?;
        }
        if parsed.linkage != Linkage::Standard {
            self.fail_if_reload_ns(owner_ns, parsed.span, "extern and exported globals")?;
        }
        let type_id = self.eval_type_expr(parsed.type_expr, scope_id)?;
        if let Type::StaticValue(_) = self.types.get(type_id) {
            kbail!(
                self,
                parsed.span,
                "Globals cannot be typed as 'static t'; you can use them in type position directly"
            )
        }

        let reload_ns = if self.namespaces.get(owner_ns).reload { Some(owner_ns) } else { None };
        let linkage = parsed.linkage.with_default_lib(self.namespaces.get(owner_ns).lib_name);
        let global_id = self.globals.next_id();
        let variable_id = self.variables.add(Variable {
            name: parsed.name,
            type_id,
            owner_scope: scope_id,
            kind: VariableKind::Global(global_id),
            flags: VariableFlags::empty(),
            usage_count: 0,
            defn_span: parsed.name_span,
        });
        self.globals.add_expected_id(
            TypedGlobal {
                variable_id,
                initial_value: GlobalInitialValue::Pending,
                parsed_expr: parsed.value_expr,
                type_id,
                span: parsed.span,
                is_constant: !parsed.is_mutable,
                is_tls: parsed.is_thread_local,
                linkage,
                ast_id: parsed_global_id,
                reload_ns,
            },
            global_id,
        );

        if scope_id == self.scopes.mem_scope_id && parsed.name == self.ast.idents.b.arena_tmp {
            self.global_id_k1_arena = Some(global_id)
        };
        self.ast.globals.get_mut(parsed_global_id).typer_state =
            ParsedGlobalDeclareOutcome::Declared(global_id);
        self.scopes.add_variable(scope_id, parsed.name, variable_id);

        self.emit_ls_entity(parsed.name_span, LsEntityKind::Variable { variable_id });

        Ok(Some(global_id))
    }

    pub fn get_global_name(&self, global_id: TypedGlobalId) -> StringId {
        self.variables.get(self.globals.get(global_id).variable_id).name
    }

    pub fn eval_global_body(&mut self, global_id: TypedGlobalId) -> K1Result<()> {
        // the pre-execution drain may get here before the body phase
        if !self.globals.get(global_id).initial_value.is_pending() {
            return Ok(());
        }
        if self.trace.stack_contains_key(TraceKind::GlobalEval, global_id.as_u32()) {
            let name = self.get_global_name(global_id);
            let mut cycle = String::new();
            for key in self.trace.stack_keys(TraceKind::GlobalEval) {
                let stack_global_id = TypedGlobalId::from_u32(key).unwrap();
                cycle.push_str(self.ident_str(self.get_global_name(stack_global_id)));
                cycle.push_str(" -> ");
            }
            cycle.push_str(self.ident_str(name));
            let span = self.globals.get(global_id).span;
            kbail!(self, span, "Global initializer cycle: {}", cycle,);
        }
        let result = self.traced(TraceKind::GlobalEval, global_id.as_u32(), 0, |k1| {
            k1.with_clean_inference(|k1| k1.eval_global_body_inner(global_id))
        });
        if let Err(e) = result {
            self.globals.get_mut(global_id).initial_value = GlobalInitialValue::Failed(e);
        }
        result
    }
    pub fn eval_global_body_from_parsed(
        &mut self,
        parsed_global_id: ParsedGlobalId,
    ) -> K1Result<()> {
        match self.ast.globals.get(parsed_global_id).typer_state {
            ParsedGlobalDeclareOutcome::Parsed => {
                kbail!(
                    self,
                    self.ast.globals.get(parsed_global_id).span,
                    "[internal error] eval undeclared global"
                )
            }
            ParsedGlobalDeclareOutcome::IfDefedOut => Ok(()),
            ParsedGlobalDeclareOutcome::Failed => Ok(()),
            ParsedGlobalDeclareOutcome::Declared(global_id) => {
                if self.module_params_phase == ModuleParamsPhase::HostDiscovery
                    && let Some(expr) = self.globals.get(global_id).parsed_expr
                    && self.find_module_params_call_args(expr).is_some()
                {
                    self.deferred_module_params.push(global_id);
                    return Ok(());
                }
                self.eval_global_body(global_id)
            }
        }
    }

    pub(super) fn find_module_params_call_args(
        &self,
        expr_id: ParsedExprId,
    ) -> Option<AstSlice<ParsedCallArg>> {
        let ParsedExpr::Call(call) = self.ast.exprs.get(expr_id) else { return None };
        if call.name.name != self.ast.idents.b.module_params {
            return None;
        }
        let path = self.ast.mem.getn(call.name.path(&self.ast.mem));
        if path.len() != 1 || path[0].name != self.ast.idents.b.k1 {
            return None;
        }
        Some(call.args)
    }

    pub(super) fn eval_global_body_inner(&mut self, global_id: TypedGlobalId) -> K1Result<()> {
        let typed_global = self.globals.get(global_id);
        let global_span = typed_global.span;
        let is_external = typed_global.linkage.is_external();
        let parsed_expr = typed_global.parsed_expr;
        let declared_type = typed_global.type_id;
        let variable_id = typed_global.variable_id;
        let scope_id = self.variables.get(variable_id).owner_scope;
        let value_expr_id = if is_external {
            match parsed_expr {
                None => {
                    self.globals.get_mut(global_id).initial_value = GlobalInitialValue::Uninit;
                    return Ok(());
                }
                Some(_id) => {
                    kbail!(self, global_span, "External globals cannot have initializers");
                }
            }
        } else {
            match parsed_expr {
                None => kbail!(self, global_span, "non-extern global cannot be uninit"),
                Some(id) => id,
            }
        };

        let expected_type_for_execution = self.get_type_family_type(declared_type);

        let static_value_id = if let ParsedExpr::Builtin = self.ast.exprs.get(value_expr_id) {
            let span = self.ast.exprs.get_span(value_expr_id);
            let global_name = self.variables.get(variable_id).name;
            self.eval_builtin_global(global_name, scope_id, expected_type_for_execution, span)?
        } else if self.find_module_params_call_args(value_expr_id).is_some() {
            let span = self.ast.exprs.get_span(value_expr_id);
            match self.module_params_phase {
                ModuleParamsPhase::Program => kbail!(
                    self,
                    span,
                    "k1/module-params is declared in a module's build.k1, not in its source"
                ),
                ModuleParamsPhase::HostDiscovery => kbail!(
                    self,
                    span,
                    "module params are not final while build.k1 compiles: read the program's from fn build on, a library's in fn setup"
                ),
                ModuleParamsPhase::HostSetup => {
                    self.ice_span(span, "module params global was not finalized")
                }
            }
        } else {
            let ctx = EvalExprContext::make(scope_id)
                .with_expected_type(Some(expected_type_for_execution));
            self.execute_static_expr(value_expr_id, ctx)?
        };
        let static_value_type_id = self.get_static_value_type(static_value_id);

        match self.get_static_type_of_type(declared_type) {
            None => {
                if let Err(msg) = self.check_types(declared_type, static_value_type_id, scope_id) {
                    let global_name = self.variables.get(variable_id).name;
                    kbail!(self, global_span, "Type mismatch for global {}: {}", global_name, msg);
                }
            }
            Some(static_type) => {
                // declared static, with a specific value, must match
                if let Some(expected_value_id) = static_type.value_id {
                    if expected_value_id != static_value_id {
                        kbail!(
                            self,
                            global_span,
                            "Wrong static value: expected {} but got {}",
                            self.static_value_to_string(expected_value_id),
                            self.static_value_to_string(static_value_id)
                        );
                    }
                } else {
                    // declared static, without a value, we must
                    // update the type of the global as well as the type of its tracking variable
                    let static_type_id_with_value =
                        self.add_value_type(static_type.family_type_id, Some(static_value_id));
                    self.globals.get_mut(global_id).type_id = static_type_id_with_value;
                    self.variables.get_mut(variable_id).type_id = static_type_id_with_value
                }
            }
        }

        self.globals.get_mut(global_id).initial_value = GlobalInitialValue::Value(static_value_id);

        Ok(())
    }

    /// Produces the value of a compiler-known constant whose initializer is `builtin`.
    pub(super) fn eval_builtin_global(
        &mut self,
        defn_name: StringId,
        scope_id: ScopeId,
        expected_type_id: TypeId,
        span: Span,
    ) -> K1Result<StaticValueId> {
        let name = self.ident_str(defn_name);
        let float = match (name, expected_type_id) {
            ("nan", F32_TYPE_ID) => Some(TypedFloatValue::F32(f32::NAN)),
            ("inf", F32_TYPE_ID) => Some(TypedFloatValue::F32(f32::INFINITY)),
            ("neg-inf", F32_TYPE_ID) => Some(TypedFloatValue::F32(f32::NEG_INFINITY)),
            ("nan", F64_TYPE_ID) => Some(TypedFloatValue::F64(f64::NAN)),
            ("inf", F64_TYPE_ID) => Some(TypedFloatValue::F64(f64::INFINITY)),
            ("neg-inf", F64_TYPE_ID) => Some(TypedFloatValue::F64(f64::NEG_INFINITY)),
            _ => None,
        };
        if let Some(float) = float {
            let owner = self.scopes.get_scope_owner(scope_id).as_namespace();
            let in_float_companion = owner.is_some_and(|namespace_id| {
                self.namespaces.get(namespace_id).companion_type_id == Some(expected_type_id)
            });
            if !in_float_companion {
                kbail!(self, span, "Float builtin constants live in the matching type companion");
            }
            return Ok(self.static_values.add(StaticValue::Float(float)));
        }
        if scope_id != self.get_k1_scope_id() {
            kbail!(self, span, "Unknown builtin name: {name}");
        }
        let bool_value = match name {
            "test" => self.config.test,
            "debug" => self.plan.config.debug,
            // The VM overrides this global's value during static execution
            "is-static" => false,
            "platform" | "host-platform" | "arch" => {
                let member = match name {
                    "platform" => self.plan.config.target.platform().to_str(),
                    "host-platform" => self.host_platform().to_str(),
                    _ => self.plan.config.target.arch().to_str(),
                };
                return Ok(self.enum_named(expected_type_id, member));
            }
            "simd-bytes" => {
                let width = self.plan.config.simd_bytes(&self.plan.strings) as i64;
                return Ok(self.static_values.add(StaticValue::Int(TypedIntValue::I64(width))));
            }
            s => kbail!(self, span, "Unknown builtin name: {s}"),
        };
        Ok(StaticValuePool::get_bool_id(bool_value))
    }

    pub(super) fn add_static_value_expr(
        &mut self,
        value_id: StaticValueId,
        span: Span,
    ) -> TypedExprId {
        let inner_type_id = self.get_static_value_type(value_id);
        let static_type_id = self.add_value_type(inner_type_id, Some(value_id));
        self.exprs.add_static(value_id, static_type_id, true, span)
    }

    pub(super) fn add_static_constant_expr(
        &mut self,
        value_id: StaticValueId,
        span: Span,
    ) -> TypedExprId {
        let type_id = self.get_static_value_type(value_id);
        self.exprs.add_static(value_id, type_id, false, span)
    }

    pub(crate) fn add_static_function_pointer(
        &mut self,
        type_id: TypeId,
        function_id: FunctionId,
    ) -> StaticValueId {
        self.get_function_mut(function_id)
            .flags
            .insert(TypedFunctionFlags::AddressTaken | TypedFunctionFlags::AddressBaked);
        self.static_values.add(StaticValue::FunctionPointer(type_id, function_id))
    }

    pub(crate) fn add_static_container_from_ids(
        &mut self,
        kind: StaticContainerKind,
        container_type_id: TypeId,
        elements: &[StaticValueId],
    ) -> StaticValueId {
        let element_type = self.get_linear_container_element(container_type_id).unwrap();
        match self.get_raw_scalar(element_type) {
            Some(scalar) => {
                let size = scalar.get_layout().size;
                let align = self.get_layout(container_type_id).unwrap().align;
                let tmp_mark = self.tmp.mark();
                let (bytes, dst) = self.tmp.push_bytes_uninit(size * elements.len() as u32, align);
                for (index, element) in elements.iter().enumerate() {
                    let element_dst = unsafe { dst.byte_add(index * size as usize) };
                    vm::store_static_value(
                        self,
                        &mut vm::StaticStack::Shared,
                        element_dst,
                        *element,
                    );
                }
                let bytes = self.tmp.getn(bytes);
                let container_id = self.static_values.add_with_storage(|mem| {
                    StaticValue::RawContainer(StaticRawContainer {
                        bytes: mem.push_bytes_aligned(bytes, align),
                        type_id: container_type_id,
                        kind,
                        scalar,
                    })
                });
                self.tmp.reset_to(tmp_mark);
                container_id
            }
            None => self.static_values.add_with_storage(|mem| {
                StaticValue::LinearContainer(StaticContainer {
                    elements: mem.pushn(elements),
                    kind,
                    type_id: container_type_id,
                })
            }),
        }
    }

    pub(crate) fn add_static_container_from_bytes(
        &mut self,
        kind: StaticContainerKind,
        container_type_id: TypeId,
        scalar: ScalarType,
        src: *const u8,
        count: usize,
    ) -> StaticValueId {
        let byte_len = scalar.get_layout().size as usize * count;
        let align = self.get_layout(container_type_id).unwrap().align;
        let src_bytes: &[u8] =
            if byte_len == 0 { &[] } else { unsafe { std::slice::from_raw_parts(src, byte_len) } };
        self.static_values.add_with_storage(|mem| {
            StaticValue::RawContainer(StaticRawContainer {
                bytes: mem.push_bytes_aligned(src_bytes, align),
                type_id: container_type_id,
                kind,
                scalar,
            })
        })
    }

    pub(crate) fn decode_raw_element(&self, raw: &StaticRawContainer, index: usize) -> StaticValue {
        let element_type = self.get_linear_container_element(raw.type_id).unwrap();
        let size = raw.scalar.get_layout().size as usize;
        let bytes = self.static_values.mem.getn(raw.bytes);
        let value = vm::load_scalar(raw.scalar, unsafe { bytes.as_ptr().byte_add(index * size) });
        vm::scalar_vm_value_to_static_value(self, element_type, value).unwrap()
    }

    pub(crate) fn static_container_len(&self, value_id: StaticValueId) -> Option<usize> {
        match self.static_values.get(value_id) {
            StaticValue::LinearContainer(cont) => Some(cont.len()),
            StaticValue::RawContainer(raw) => Some(raw.len()),
            StaticValue::Zero(type_id) | StaticValue::Empty(type_id) => {
                match self.types.get(*type_id) {
                    Type::Array(array) => {
                        Some(self.get_concrete_count_of_array(array.size_type)? as usize)
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub(crate) fn get_static_container_element(
        &self,
        value_id: StaticValueId,
        index: usize,
    ) -> StaticValue {
        match *self.static_values.get(value_id) {
            StaticValue::LinearContainer(cont) => {
                *self.static_values.get(*self.static_values.mem.get_nth(cont.elements, index))
            }
            StaticValue::RawContainer(raw) => self.decode_raw_element(&raw, index),
            StaticValue::Zero(type_id) => {
                StaticValue::Zero(self.get_linear_container_element(type_id).unwrap())
            }
            StaticValue::Empty(type_id) => {
                StaticValue::Empty(self.get_linear_container_element(type_id).unwrap())
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn static_container_element(
        &mut self,
        value_id: StaticValueId,
        index: usize,
    ) -> StaticValueId {
        if let StaticValue::LinearContainer(cont) = *self.static_values.get(value_id) {
            return *self.static_values.mem.get_nth(cont.elements, index);
        }
        let element = self.get_static_container_element(value_id, index);
        self.static_values.add(element)
    }

    /// A constant `code` value: one text with one source span covering it
    pub(super) fn make_static_code_value(&mut self, text: StringId, source: Span) -> StaticValueId {
        let code_type = self.builtin_types.code();
        let span_type = self.builtin_types.code_span();
        let spans_list_type =
            self.instantiate_generic_type(self.builtin_types.list(), &[span_type]);
        let len = self.ast.idents.get_string(text).len() as u32;
        let text_value = self.static_values.add_string(text);
        let start_value = self.static_values.add_int(TypedIntValue::U32(0));
        let len_value = self.static_values.add_int(TypedIntValue::U32(len));
        let source_value = self.static_values.add_int(TypedIntValue::U64(source.encoded_k1_u64()));
        let span_value = self
            .static_values
            .add_struct_from_slice(span_type, &[start_value, len_value, source_value]);
        let spans_value = self.add_static_container_from_ids(
            StaticContainerKind::List,
            spans_list_type,
            &[span_value],
        );
        self.static_values.add_struct_from_slice(code_type, &[text_value, spans_value])
    }

    fn get_expr_span_quoted(&self, parsed_expr: ParsedExprId) -> Span {
        match self.ast.exprs.get(parsed_expr) {
            ParsedExpr::Code(code) => code.content,
            _ => self.ast.exprs.get_span(parsed_expr),
        }
    }

    pub(super) fn code_from_parsed_expr(&mut self, parsed_expr: ParsedExprId) -> StaticValueId {
        let span = self.get_expr_span_quoted(parsed_expr);
        let content = self.ast.sources.get_span_content(&self.ast.mem, span);
        let string_id = self.ast.idents.intern(content);
        self.make_static_code_value(string_id, span)
    }

    pub(super) fn tokens_from_parsed_expr(
        &mut self,
        parsed_expr: ParsedExprId,
    ) -> K1Result<StaticValueId> {
        let code_span = self.get_expr_span_quoted(parsed_expr);
        let content = self.ast.sources.get_span_content(&self.ast.mem, code_span).to_string();
        let mut lexed = crate::lex::lex(&content, std::mem::take(&mut self.buffers.lexed));
        if let Some(e) = lexed.error.take() {
            let span = Span { global_start: code_span.global_start + e.start, len: e.len };
            self.buffers.lexed = lexed;
            kbail!(self, span, "{}", e.message);
        }
        let token_count = lexed.tokens.len();
        let tokens = self.code_tokens_from_lexed(&lexed, &content, code_span, 0, token_count);
        self.buffers.lexed = lexed;
        tokens
    }

    fn code_tokens_from_lexed(
        &mut self,
        lexed: &crate::lex::Lexed,
        content: &str,
        code_span: Span,
        start: usize,
        end: usize,
    ) -> K1Result<StaticValueId> {
        use crate::lex::{StringPiece, TokenKind as K};
        let token_type = self.builtin_types.code_token();
        let kind_type = self.get_struct_field(token_type, 0).type_id;
        let spacing_type = self.get_struct_field(token_type, 3).type_id;
        let parts_type = self.get_struct_field(token_type, 4).type_id;
        let no_parts =
            self.add_static_container_from_ids(StaticContainerKind::Span, parts_type, &[]);
        let token_span = |token: crate::lex::Token| {
            let (token_start, token_len) = lexed.get_token_start_len(token);
            Span { global_start: code_span.global_start + token_start, len: token_len }
        };
        let mut elements = self.tmp.new_list((end - start) as u32);
        let mut i = start;
        while i < end {
            let first = lexed.tokens[i];
            let first_index = i;
            let (token_kind, parts) = match first.kind {
                K::Eof => {
                    i += 1;
                    continue;
                }
                K::Ident => (k1_types::CodeTokenKind::ident, no_parts),
                k if k.is_keyword() => (k1_types::CodeTokenKind::ident, no_parts),
                K::Numeric => (k1_types::CodeTokenKind::number, no_parts),
                K::Char => (k1_types::CodeTokenKind::char, no_parts),
                k if k.is_string() => {
                    let literal = |index: usize| {
                        let token = lexed.tokens[index];
                        let (token_start, token_len) = lexed.get_token_start_len(token);
                        StringPiece::Literal {
                            token,
                            text: &content
                                [token_start as usize..(token_start + token_len) as usize],
                            starts_fresh: index == first_index,
                        }
                    };
                    let mut pieces: SV8<StringPiece<(usize, usize)>> = smallvec![literal(i)];
                    let mut done = k.as_string().unwrap().done;
                    while !done {
                        i += 1;
                        match lexed.tokens[i].kind {
                            K::OpenBrace => {
                                let mut close = i + 1;
                                let mut depth = 1;
                                loop {
                                    match lexed.tokens[close].kind {
                                        K::OpenBrace => depth += 1,
                                        K::CloseBrace if depth == 1 => break,
                                        K::CloseBrace => depth -= 1,
                                        _ => {}
                                    }
                                    close += 1;
                                }
                                pieces.push(StringPiece::Hole((i + 1, close)));
                                i = close;
                            }
                            k if k.is_string() => {
                                pieces.push(literal(i));
                                done = k.as_string().unwrap().done;
                            }
                            _ => pieces.push(StringPiece::Hole((i, i + 1))),
                        }
                    }
                    let mut cooked = String::new();
                    let mut ends: SV8<u32> = smallvec![];
                    if let Err(message) = crate::lex::cook_string(&pieces, &mut cooked, &mut ends) {
                        kbail!(self, token_span(first), "{message}");
                    }

                    let part_type = self.get_linear_container_element(parts_type).unwrap();
                    let part_variants = self.types.get(part_type).expect_sum().variants;
                    let text_payload_type =
                        self.sum_variant_by_index(part_variants, 0).payload.unwrap();
                    let expr_payload_type =
                        self.sum_variant_by_index(part_variants, 1).payload.unwrap();
                    let mut part_values = self.tmp.new_list(pieces.len() as u32);
                    let mut cooked_ends = ends.iter();
                    let mut cooked_start = 0;
                    for piece in &pieces {
                        let (variant_index, payload) = match *piece {
                            StringPiece::Literal { token, .. } => {
                                let cooked_end = *cooked_ends.next().unwrap() as usize;
                                let text = &cooked[cooked_start..cooked_end];
                                cooked_start = cooked_end;
                                if text.is_empty() {
                                    continue;
                                }
                                let value =
                                    self.static_values.add_string(self.ast.idents.intern(text));
                                let source = self.static_values.add_int(TypedIntValue::U64(
                                    token_span(token).encoded_k1_u64(),
                                ));
                                (
                                    0,
                                    self.static_values
                                        .add_struct_from_slice(text_payload_type, &[value, source]),
                                )
                            }
                            StringPiece::Hole((lo, hi)) => {
                                let mut depth = 0;
                                for index in lo..hi {
                                    let token = lexed.tokens[index];
                                    match token.kind {
                                        K::OpenParen | K::OpenBracket | K::OpenBrace => depth += 1,
                                        K::CloseParen | K::CloseBracket | K::CloseBrace => {
                                            depth -= 1
                                        }
                                        K::Colon
                                            if depth == 0
                                                && index > lo
                                                && !token.is_whitespace_preceded()
                                                && matches!(
                                                    lexed.tokens[index - 1].kind,
                                                    K::Ident
                                                        | K::Numeric
                                                        | K::Char
                                                        | K::CloseParen
                                                        | K::CloseBracket
                                                        | K::CloseBrace
                                                ) =>
                                        {
                                            kbail!(
                                                self,
                                                token_span(token),
                                                "{}",
                                                crate::lex::FmtSpec::INVALID_MESSAGE
                                            )
                                        }
                                        _ => {}
                                    }
                                }
                                let (hi, spec) =
                                    if hi > lo && lexed.tokens[hi - 1].kind == K::FmtSpec {
                                        let (spec_start, spec_len) =
                                            lexed.get_token_start_len(lexed.tokens[hi - 1]);
                                        let spec = &content[spec_start as usize + 1
                                            ..(spec_start + spec_len) as usize];
                                        (hi - 1, spec)
                                    } else {
                                        (hi, "")
                                    };
                                let spec =
                                    self.static_values.add_string(self.ast.idents.intern(spec));
                                let expr_span = if lo < hi {
                                    let mut span = token_span(lexed.tokens[lo]);
                                    span.extend_to(token_span(lexed.tokens[hi - 1]).end());
                                    span
                                } else {
                                    let mut span = token_span(lexed.tokens[lo - 1]);
                                    span.extend_to(token_span(lexed.tokens[hi]).end());
                                    span
                                };
                                let tokens =
                                    self.code_tokens_from_lexed(lexed, content, code_span, lo, hi)?;
                                let source = self
                                    .static_values
                                    .add_int(TypedIntValue::U64(expr_span.encoded_k1_u64()));
                                (
                                    1,
                                    self.static_values.add_struct_from_slice(
                                        expr_payload_type,
                                        &[tokens, spec, source],
                                    ),
                                )
                            }
                        };
                        part_values.push(self.static_values.add(StaticValue::Sum(StaticSum {
                            sum_type_id: part_type,
                            variant_index,
                            payload: Some(payload),
                        })));
                    }
                    let parts = self.add_static_container_from_ids(
                        StaticContainerKind::Span,
                        parts_type,
                        part_values.as_slice(),
                    );
                    (k1_types::CodeTokenKind::string, parts)
                }
                _ => (k1_types::CodeTokenKind::punct, no_parts),
            };
            let output_span = Span {
                global_start: token_span(first).global_start,
                len: token_span(lexed.tokens[i]).end() - token_span(first).global_start,
            };
            let text = self.ast.sources.get_span_content(&self.ast.mem, output_span);
            let text_value = self.static_values.add_string(self.ast.idents.intern(text));
            let source_value =
                self.static_values.add_int(TypedIntValue::U64(output_span.encoded_k1_u64()));
            let kind_value = self
                .static_values
                .add(StaticValue::Enum(kind_type, TypedIntValue::U8(token_kind as u8)));
            let token_spacing = match first_index {
                _ if first_index == start => k1_types::CodeTokenSpacing::none,
                _ if first.is_newline_preceded() => k1_types::CodeTokenSpacing::newline,
                _ if first.is_whitespace_preceded() => k1_types::CodeTokenSpacing::inline,
                _ => k1_types::CodeTokenSpacing::none,
            };
            let spacing_value = self
                .static_values
                .add(StaticValue::Enum(spacing_type, TypedIntValue::U8(token_spacing as u8)));
            elements.push(self.static_values.add_struct_from_slice(
                token_type,
                &[kind_value, text_value, source_value, spacing_value, parts],
            ));
            i += 1;
        }
        Ok(self.add_static_container_from_ids(
            StaticContainerKind::Span,
            self.builtin_types.code_token_span(),
            elements.as_slice(),
        ))
    }

    pub(super) fn materialize_static_value(
        &mut self,
        family_type_id: TypeId,
        value_id: Option<StaticValueId>,
        span: Span,
    ) -> TypedExprId {
        match value_id {
            Some(value_id) => self.add_static_constant_expr(value_id, span),
            None => self.synth_phony(family_type_id, span),
        }
    }

    /// Compiles `#static <expr>` and `#meta <expr>` constructs
    pub(super) fn compile_static_or_meta(
        &mut self,
        expr_id: ParsedExprId,
        stat: ParsedStaticExpr,
        is_definition: bool,
        ctx: EvalExprContext,
    ) -> K1Result<StaticExecutionResult> {
        let span = self.ast.exprs.get_span(expr_id);
        let base_expr = stat.base_expr;

        if matches!(stat.kind, ParsedStaticBlockKind::MacroCall) {
            debug_assert!(is_definition);
            let ParsedExpr::Call(call) = self.ast.exprs.get(base_expr) else {
                kbail!(self, span, "Expected a macro call following '$'");
            };
            let call = *call;
            let Some(function_id) = self.find_function_namespaced(ctx.scope_id, &call.name)? else {
                kbail!(
                    self,
                    span,
                    "Unknown macro '{}'; a definition-level macro must live in the module's `pre` namespace or an upstream module",
                    &call.name
                );
            };
            if !self.get_function(function_id).is_macro() {
                kbail!(
                    self,
                    span,
                    "'{}' is not a macro; only macros can be invoked with '$'",
                    &call.name
                );
            }

            self.emit_ls_entity(
                call.name.name_span,
                LsEntityKind::Function { function_id, is_defn: false },
            );

            let mut macro_args: SV8<_> = smallvec![];
            for parsed_arg in self.ast.mem.getn(call.args) {
                macro_args.push(*parsed_arg)
            }
            return self.execute_macro_call(
                self.ast.mem.getn(call.type_args(&self.ast.mem)),
                &macro_args,
                span,
                function_id,
                true,
                ctx,
            );
        }

        if self.is_generic_pass(ctx.scope_id) {
            return Ok(StaticExecutionResult::TypedExpr(self.synth_deferred_static(ctx, span)?));
        }

        let kind = stat.kind;
        let expected_type_for_execution = match kind {
            ParsedStaticBlockKind::Value => {
                ctx.expected_type_id.map(|t| self.get_type_family_type(t))
            }
            ParsedStaticBlockKind::Metaprogram => self.builtin_types.code,
            ParsedStaticBlockKind::MacroCall => unreachable!(),
        };
        let exec_ctx = ctx.with_expected_type(expected_type_for_execution);
        match kind {
            ParsedStaticBlockKind::Value => {
                let vm_result = self.execute_static_expr(base_expr, exec_ctx)?;
                let expr = self.add_static_value_expr(vm_result, span);
                Ok(StaticExecutionResult::TypedExpr(expr))
            }
            ParsedStaticBlockKind::Metaprogram => {
                self.traced(TraceKind::Metaprogram, span.global_start, ctx.trace_flags(), |k1| {
                    let mut content = String::new();
                    k1.write_meta_emission_header(
                        &mut content,
                        EmissionOrigin::Label("#meta block"),
                        span,
                        Some(ctx.scope_id),
                    );
                    k1.execute_and_compile_emitted_code(
                        span,
                        ctx,
                        content,
                        is_definition,
                        |k1, vm| {
                            let expr = k1.typecheck_parsed_expr_for_exec(base_expr, exec_ctx)?;
                            k1.compile_static_unit(expr, k1.ast.exprs.is_debug(base_expr))?;
                            bc::exec::execute_compiled_expr_raw(k1, vm, expr, true)
                        },
                    )
                })
            }
            ParsedStaticBlockKind::MacroCall => unreachable!(),
        }
    }

    fn execute_and_compile_emitted_code(
        &mut self,
        span: Span,
        ctx: EvalExprContext,
        content: String,
        is_definition: bool,
        run: impl FnOnce(&mut TypedProgram, &mut vm::Vm) -> K1Result<bc::exec::RawUnitResult>,
    ) -> K1Result<StaticExecutionResult> {
        let emitted = self.do_with_vm(span, ctx.trace_flags(), |k1, vm| {
            vm.meta_call_site = Some((ctx, span));
            let result = (|| {
                let raw = run(k1, vm)?;
                // Read the emitted code while VM memory is still good
                if !raw.returns_value || Some(raw.result_type_id) != k1.builtin_types.code {
                    kbail!(
                        k1,
                        span,
                        "Metaprogram must evaluate to `code`; got {}. Wrap a plain string with code/from-string",
                        raw.result_type_id
                    );
                }
                let code = unsafe { *(raw.ret_addr as *const k1_types::K1Code) };
                k1.read_emitted_code(code, span, content, is_definition)
            })();
            vm.reset(k1.global_id_k1_arena);
            result
        })?;
        let Some((content, table)) = emitted else {
            return if is_definition {
                Ok(StaticExecutionResult::Definitions(MSlice::empty()))
            } else {
                Ok(StaticExecutionResult::TypedExpr(self.synth_empty_value(span)))
            };
        };
        if is_definition {
            let file_id = self.add_emitted_source(&content, self.tmp.getn(table), span);
            let module_id = self.get_scope_module(ctx.scope_id);
            let generated_path = self.ast.sources.get(file_id).file_path;
            let definitions = self.traced(TraceKind::Parse, generated_path.as_u32(), 0, |k1| {
                k1.with_parser(module_id, file_id, |p| {
                    Ok(p.parse_definitions(TokenKind::Eof).to_mslice())
                })
            })?;
            return Ok(StaticExecutionResult::Definitions(definitions));
        }
        let parsed_expr = self.parse_emitted_expr(content, table, span, ctx.scope_id)?;
        let typed_metaprogram = self.eval_expr(parsed_expr, ctx)?;
        debug!("Emitted compiled expr:\n{}", self.expr_to_string(typed_metaprogram));
        Ok(StaticExecutionResult::TypedExpr(typed_metaprogram))
    }

    fn read_emitted_code(
        &mut self,
        code: k1_types::K1Code,
        span: Span,
        content: String,
        is_definition: bool,
    ) -> K1Result<Option<(String, TmpSlice<CodeChunkPos>)>> {
        use crate::vm::k1_types::K1CodeSpan;
        #[cfg(debug_assertions)]
        self.assert_code_layouts();
        let frame = self.trace_push(TraceKind::VmValueFerry, span.global_start, 0);
        let Ok(text) = (unsafe { code.text.to_str() }) else {
            kbail!(self, span, "Metaprogram produced non-utf8 code");
        };
        let spans: &[K1CodeSpan] = if code.spans.len == 0 {
            &[]
        } else {
            unsafe {
                std::slice::from_raw_parts(
                    code.spans.k1_buffer.data as *const K1CodeSpan,
                    code.spans.len as usize,
                )
            }
        };
        let emitted = self.build_emitted_source(content, is_definition, text, spans);
        self.trace_pop(frame);
        emitted
    }

    pub(super) fn write_meta_emission_header(
        &self,
        content: &mut String,
        origin: EmissionOrigin,
        span: Span,
        scope_id: Option<ScopeId>,
    ) {
        content.push_str("// generated by ");
        match origin {
            EmissionOrigin::Macro(function_id) => {
                content.push_str("macro ");
                write_header_value(content, |w| self.display_specialization_label(w, function_id));
            }
            EmissionOrigin::Label(label) => content.push_str(label),
        }
        let (source, line) = self.get_span_location(span);
        writeln!(
            content,
            " at {}:{}",
            self.ast.idents.get_string(source.file_path),
            line.line_number()
        )
        .unwrap();
        if let Some(scope_id) = scope_id
            && let Some(function_id) = self.scopes.nearest_parent_function(scope_id)
        {
            content.push_str("// in ");
            write_header_value(content, |w| self.display_specialization_label(w, function_id));
            content.push('\n');
        }
    }

    pub(super) fn build_emitted_source(
        &mut self,
        mut content: String,
        is_definition: bool,
        text: &str,
        spans: &[vm::k1_types::K1CodeSpan],
    ) -> K1Result<Option<(String, TmpSlice<CodeChunkPos>)>> {
        if text.is_empty() {
            return Ok(None);
        }
        // The source keeps `content` forever, so size it exactly and move it
        // in; 4 covers the block wrapper
        content.reserve_exact(text.len() + 4);
        if !is_definition {
            content.push_str("{\n");
        }
        let base = content.len() as u32;
        content.push_str(text);
        let mut table: TmpList<CodeChunkPos> = self.tmp.new_list(spans.len() as u32);
        for s in spans {
            if s.len == 0 {
                continue;
            }
            if let Some(source) = Span::from_encoded_k1_u64(s.source) {
                let start = base + s.start;
                table.push(CodeChunkPos { start, end: start + s.len, source });
            }
        }
        if !is_definition {
            content.push_str("\n}");
        }
        Ok(Some((content, table.to_slice())))
    }

    #[cfg(debug_assertions)]
    pub(super) fn assert_code_layouts(&mut self) {
        use crate::vm::k1_types::{K1Code, K1CodeSpan};
        let code_type = self.builtin_types.code.unwrap();
        let span_type = self.builtin_types.code_span.unwrap();
        assert_eq!(self.get_layout(code_type).unwrap().size as usize, size_of::<K1Code>());
        assert_eq!(self.get_layout(span_type).unwrap().size as usize, size_of::<K1CodeSpan>());
        let fields = self.get_struct_layout(code_type);
        assert_eq!(fields[0].offset as usize, std::mem::offset_of!(K1Code, text));
        assert_eq!(fields[1].offset as usize, std::mem::offset_of!(K1Code, spans));
        let fields = self.get_struct_layout(span_type);
        assert_eq!(fields[0].offset as usize, std::mem::offset_of!(K1CodeSpan, start));
        assert_eq!(fields[1].offset as usize, std::mem::offset_of!(K1CodeSpan, len));
        assert_eq!(fields[2].offset as usize, std::mem::offset_of!(K1CodeSpan, source));
    }

    pub(crate) fn builtin_meta_eval(
        &mut self,
        code: k1_types::K1Code,
        type_id: TypeId,
        span: Span,
        ctx: EvalExprContext,
    ) -> K1Result<StaticValueId> {
        let mut content = String::new();
        self.write_meta_emission_header(
            &mut content,
            EmissionOrigin::Label("meta/eval"),
            span,
            Some(ctx.scope_id),
        );
        let Some((content, table)) = self.read_emitted_code(code, span, content, false)? else {
            kbail!(self, span, "Expected an expression; got empty code");
        };
        let parsed_expr = self.parse_emitted_expr(content, table, span, ctx.scope_id)?;
        self.execute_static_arg(parsed_expr, type_id, ctx.with_speculative())
    }

    fn execute_static_arg(
        &mut self,
        parsed_expr: ParsedExprId,
        type_id: TypeId,
        ctx: EvalExprContext,
    ) -> K1Result<StaticValueId> {
        let value_id =
            self.execute_static_expr(parsed_expr, ctx.with_expected_type(Some(type_id)))?;
        let value_type = self.get_static_value_type(value_id);
        if let Err(msg) = self.check_types(type_id, value_type, ctx.scope_id) {
            kbail!(self, self.ast.exprs.get_span(parsed_expr), "Static type mismatch: {}", msg);
        }
        Ok(value_id)
    }

    fn parse_emitted_expr(
        &mut self,
        content: String,
        table: TmpSlice<CodeChunkPos>,
        span: Span,
        scope_id: ScopeId,
    ) -> K1Result<ParsedExprId> {
        let hash = Self::hash_emitted_expr(&content, span);
        let cached = self
            .emitted_parse_cache
            .find(hash, |&(emitted_index, _)| {
                let emitted = self.emitted_sources[emitted_index as usize];
                emitted.call_span == span
                    && self.ast.sources.get(emitted.file_id).content(&self.ast.mem) == content
                    && self.mem.getn(emitted.entries) == self.tmp.getn(table)
            })
            .copied();
        if let Some((emitted_index, cached_root)) = cached {
            if let Some(log) = &mut self.debug_emissions {
                log.push(emitted_index)
            }
            return Ok(cached_root);
        }
        // Parse the code as its own file (cohesive spans, a source containing
        // the full text), then compile it in place of the invocation
        let source_for_emission = self.add_emitted_source(&content, self.tmp.getn(table), span);
        let emitted_index = self.emitted_sources.len() as u32 - 1;
        let generated_path = self.ast.sources.get(source_for_emission).file_path;
        let module_id = self.get_scope_module(scope_id);
        let parsed_expr = self.traced(TraceKind::Parse, generated_path.as_u32(), 0, |k1| {
            k1.with_parser(module_id, source_for_emission, |p| p.expect_expression())
        })?;
        let (sources, ast_mem) = (&self.ast.sources, &self.ast.mem);
        let emitted_sources = &self.emitted_sources;
        self.emitted_parse_cache.insert_unique(
            hash,
            (emitted_index, parsed_expr),
            |&(index, _)| {
                let emitted = emitted_sources[index as usize];
                Self::hash_emitted_expr(
                    sources.get(emitted.file_id).content(ast_mem),
                    emitted.call_span,
                )
            },
        );
        Ok(parsed_expr)
    }

    fn hash_emitted_expr(content: &str, call_span: Span) -> u64 {
        use std::hash::{Hash, Hasher};
        let mut h = ahash::AHasher::default();
        content.hash(&mut h);
        call_span.hash(&mut h);
        h.finish()
    }

    pub(super) fn add_emitted_source(
        &mut self,
        content: &str,
        table: &[CodeChunkPos],
        span: Span,
    ) -> FileId {
        debug!("Emitted source:\n---\n{content}\n---");
        let (source, line) = self.get_span_location(span);
        let line_number = line.line_number();
        let column = source.local_from_global_offset(span.global_start) - line.start_char;
        let stem = source.filename_str(&self.ast.idents).strip_suffix(".k1").unwrap();
        let hash = xxhash_rust::xxh3::xxh3_64_with_seed(content.as_bytes(), column as u64) as u32;
        let generated_filename = k1_format!(self, &(), "meta_{stem}_{line_number}_{hash:08x}.k1");
        let generated_path = kpath::join_id(
            &self.ast.idents,
            &mut self.tmp,
            self.config.out_dir_generated,
            generated_filename.as_str(),
        );
        let emitted_file =
            crate::parse::SourceFile::make(&mut self.ast.mem, generated_path, content);
        let file_id = self.ast.sources.add_file(emitted_file);
        debug_assert!(
            self.emitted_sources.last().is_none_or(|e| e.file_id < file_id),
            "emitted_sources must stay sorted by file_id for binary search"
        );
        if let Some(log) = &mut self.debug_emissions {
            log.push(self.emitted_sources.len() as u32)
        }
        self.emitted_sources.push(EmittedSource {
            file_id,
            call_span: span,
            entries: self.mem.pushn(table),
            write_out: false,
        });
        file_id
    }

    pub(super) fn collect_debug_emissions<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> T,
    ) -> (T, Vec<u32>) {
        let outer = self.debug_emissions.replace(Vec::new());
        let result = f(self);
        let emissions = std::mem::replace(&mut self.debug_emissions, outer).unwrap_or_default();
        if let Some(outer) = &mut self.debug_emissions {
            outer.extend_from_slice(&emissions)
        }
        (result, emissions)
    }

    pub(super) fn report_debug_compile(
        &mut self,
        span: Span,
        emissions: &[u32],
        is_ok: bool,
        typed: Option<(TypedExprId, Option<TypeId>)>,
    ) {
        let mut message =
            String::from(if is_ok { "DEBUG COMPILE DONE" } else { "DEBUG COMPILE FAILED" });
        for (i, &index) in emissions.iter().enumerate() {
            if emissions[..i].contains(&index) {
                continue;
            }
            let emitted = &mut self.emitted_sources[index as usize];
            emitted.write_out = true;
            let path = self.ast.sources.get(emitted.file_id).file_path;
            message.push_str("\nemitted: ");
            message.push_str(self.ast.idents.get_string(path));
        }
        if let Some((expr, expected_type)) = typed {
            message.push_str("\ntype hint: ");
            message.push_str(&self.type_id_to_string_opt(expected_type));
            message.push('\n');
            message.push_str(&self.expr_to_string_with_type(expr));
        }
        let message = self.ast.idents.intern(message);
        self.report(make_message(message, span, MessageLevel::Info));
    }

    pub fn write_emitted_sources(&self, write_all: bool) {
        use std::os::unix::fs::OpenOptionsExt;
        let start = std::time::Instant::now();
        let mut current: fxhash::FxHashSet<&str> = fxhash::FxHashSet::default();
        let mut written = 0usize;
        for emitted in &self.emitted_sources {
            let source = self.ast.sources.get(emitted.file_id);
            let path = self.ast.idents.get_string(source.file_path);
            current.insert(kpath::file_name(path));
            if !write_all && !emitted.write_out {
                continue;
            }
            let content = source.content(&self.ast.mem);
            if std::fs::metadata(Path::new(path)).is_ok_and(|m| m.len() == content.len() as u64) {
                continue;
            }
            let _ = std::fs::remove_file(Path::new(path));
            let result = std::fs::OpenOptions::new()
                .write(true)
                .create_new(true)
                .mode(0o444)
                .open(Path::new(path))
                .and_then(|mut file| std::io::Write::write_all(&mut file, content.as_bytes()));
            match result {
                Ok(()) => written += 1,
                Err(e) => eprintln!("Failed to write out generated metaprogram at {path}. {e}"),
            }
        }
        let mut swept = 0usize;
        let dir = Path::new(self.ident_str(self.config.out_dir_generated));
        if let Ok(entries) = std::fs::read_dir(dir) {
            for entry in entries.flatten() {
                let is_current = entry.file_name().to_str().is_some_and(|n| current.contains(n));
                if !is_current && std::fs::remove_file(entry.path()).is_ok() {
                    swept += 1;
                }
            }
        }
        if self.config.tools.chatty {
            let elapsed = start.elapsed();
            eprintln!("Wrote {written} emitted sources, swept {swept} stale in {elapsed:.2?}");
            let mut real_files = 0usize;
            let mut real_bytes = 0usize;
            let mut emitted_bytes = 0usize;
            let mut newline_bytes = 0usize;
            let mut trivia_bytes = 0usize;
            for (file_id, source) in self.ast.sources.iter() {
                if self.emitted_source_for_file(file_id).is_some() {
                    emitted_bytes += source.content_len();
                } else {
                    real_files += 1;
                    real_bytes += source.content_len();
                }
                newline_bytes += source.newline_count() * size_of::<u32>();
                trivia_bytes += source.trivia.len() as usize * size_of::<crate::lex::TriviaEntry>();
            }
            let mut unique_emissions: ahash::HashSet<u64> = ahash::HashSet::default();
            let mut unique_bytes = 0usize;
            for emitted in &self.emitted_sources {
                use std::hash::{Hash, Hasher};
                let content = self.ast.sources.get(emitted.file_id).content(&self.ast.mem);
                let mut h = ahash::AHasher::default();
                content.hash(&mut h);
                if unique_emissions.insert(h.finish()) {
                    unique_bytes += content.len();
                }
            }
            eprintln!(
                "\tsources: {} files {}kb + {} emitted {}kb ({} unique, {}kb); line tables {}kb, trivia {}kb",
                real_files,
                real_bytes / crate::KILOBYTE,
                self.emitted_sources.len(),
                emitted_bytes / crate::KILOBYTE,
                unique_emissions.len(),
                unique_bytes / crate::KILOBYTE,
                newline_bytes / crate::KILOBYTE,
                trivia_bytes / crate::KILOBYTE,
            );
        }
    }

    pub(super) fn execute_macro_call(
        &mut self,
        type_args: &[NamedTypeArg],
        args: &[ParsedCallArg],
        span: Span,
        function_id: FunctionId,
        is_definition: bool,
        ctx: EvalExprContext,
    ) -> K1Result<StaticExecutionResult> {
        self.traced(TraceKind::MacroCall, function_id.as_u32(), ctx.trace_flags(), |k1| {
            k1.execute_macro_call_body(type_args, args, span, function_id, is_definition, ctx)
        })
    }

    fn execute_macro_call_body(
        &mut self,
        type_args: &[NamedTypeArg],
        args: &[ParsedCallArg],
        span: Span,
        function_id: FunctionId,
        is_definition: bool,
        ctx: EvalExprContext,
    ) -> K1Result<StaticExecutionResult> {
        let function = self.get_function(function_id);
        let is_generic = !function.is_concrete();
        let function_name = function.name;
        let function_type_params = function.type_params;
        let Some(parsed_macro_id) = function.parsed_id.as_macro_id() else {
            self.ice_span(span, "Macro function without a parsed macro")
        };
        let typed_type_args = self.check_type_args_against_params(
            function_type_params,
            type_args,
            ctx.scope_id,
            span,
        )?;

        let parsed_params = self.ast.get_macro(parsed_macro_id).params;
        if args.len() != parsed_params.len() as usize {
            kbail!(
                self,
                span,
                "Macro '{}' takes {} arguments, got {}",
                function_name,
                parsed_params.len(),
                args.len()
            );
        }

        if self.is_generic_pass(ctx.scope_id) {
            return Ok(StaticExecutionResult::TypedExpr(self.synth_deferred_static(ctx, span)?));
        }

        self.require_function_body(function_id, span)?;

        // Specialize if needed
        let function_to_run = if is_generic {
            let spec_fn_id = self.specialize_function_declaration(
                self.tmp.getn(typed_type_args),
                &[],
                function_id,
            );
            if !self.get_function(spec_fn_id).is_concrete() {
                return Ok(StaticExecutionResult::TypedExpr(
                    self.synth_deferred_static(ctx, span)?,
                ));
            }
            self.require_function_body(spec_fn_id, span)?;
            spec_fn_id
        } else {
            function_id
        };

        let mut content = String::new();
        self.write_meta_emission_header(
            &mut content,
            EmissionOrigin::Macro(function_to_run),
            span,
            Some(ctx.scope_id),
        );
        let fn_param_types = self
            .types
            .get(self.get_function(function_to_run).type_id)
            .as_function()
            .unwrap()
            .logical_params();

        let mut static_args: SV8<StaticValueId> = smallvec![];
        for (index, (param, arg)) in
            self.mem.getn(fn_param_types).iter().zip(args.iter()).enumerate()
        {
            let parsed_arg = *arg;
            let param_name = self.get_declared_param_name(function_to_run, index);
            if let Some(passed_name) = parsed_arg.name {
                if passed_name != param_name {
                    kbail!(
                        self,
                        span,
                        "Macro argument name mismatch: expected {}, got {}",
                        param_name,
                        passed_name,
                    );
                }
            }
            match param.kind {
                FnParamKind::MacroCode => {
                    static_args.push(self.code_from_parsed_expr(parsed_arg.value));
                }
                FnParamKind::MacroTokens => {
                    static_args.push(self.tokens_from_parsed_expr(parsed_arg.value)?);
                }
                FnParamKind::Plain | FnParamKind::Context => {
                    let value_id = self.execute_static_arg(parsed_arg.value, param.type_id, ctx)?;
                    write!(&mut content, "// {} = ", self.ident_str(param_name)).unwrap();
                    write_header_value(&mut content, |w| {
                        self.display_static_value(w, value_id, false)
                    });
                    content.push('\n');
                    static_args.push(value_id);
                }
            };
        }

        // Run the macro and compile its output
        self.execute_and_compile_emitted_code(span, ctx, content, is_definition, |k1, vm| {
            Self::static_exec_prepare_function(k1, function_to_run, span)?;
            bc::exec::execute_compiled_function_raw(k1, vm, function_to_run, &static_args, true)
        })
    }

    pub(super) fn with_parser<R>(
        &mut self,
        module_id: ModuleId,
        file_id: FileId,
        f: impl FnOnce(&mut parse::Parser) -> parse::ParseResult<R>,
    ) -> K1Result<R> {
        let buffer = std::mem::take(&mut self.buffers.lexed);

        let module = self.modules.get(module_id);
        let parsed_namespace_id = module.parsed_namespace_id;
        let code_str = self.ast.sources.get(file_id).content(&self.ast.mem);
        let mut lexed = crate::lex::lex(code_str, buffer);
        if let Err(e) = self.ast.register_lexed_file(file_id, &mut lexed) {
            parse::print_error(&self.ast, &e);
            self.buffers.lexed = lexed;
            kbail!(self, e.span(), "Failed to lex code emitted from here");
        };

        let mut p = crate::parse::Parser::make_for_file(
            module.id,
            parsed_namespace_id,
            &mut self.ast,
            &lexed,
            file_id,
        );

        let error_count_start = p.ast.errors.len();
        let r = f(&mut p);
        let recovered = p.ast.errors.drain(error_count_start..).next_back();
        self.buffers.lexed = lexed;
        match (r, recovered) {
            (Err(e), _) | (Ok(_), Some(e)) => {
                Err(kerr!(self, e.span(), "Failed to parse the code you returned: {e}"))
            }
            (Ok(r), None) => Ok(r),
        }
    }

    pub fn get_extern_symbol(
        &mut self,
        linkage: Linkage,
        name: StringId,
        span: Span,
    ) -> K1Result<*mut std::ffi::c_void> {
        let Linkage::External { module_id, lib_name, .. } = linkage else {
            self.ice_span(span, "extern symbol lookup for a non-extern item")
        };
        let handle = match lib_name {
            None => self.vm_process_dlopen_handle,
            Some(lib_name) => self.get_dylib_handle(module_id, lib_name, span)?,
        };
        let symbol = linkage.link_symbol(name).unwrap();
        let symbol_cstr = std::ffi::CString::new(self.ident_str(symbol)).unwrap();
        let address = unsafe { libc::dlsym(handle, symbol_cstr.as_ptr()) };
        if address.is_null() {
            kbail!(self, span, "Could not find extern symbol '{}'", symbol);
        }
        Ok(address)
    }

    pub fn get_dylib_handle(
        &mut self,
        module_id: ModuleId,
        lib_name_ident: StringId,
        span: Span,
    ) -> K1Result<*mut std::ffi::c_void> {
        if let Some(handle) = self.vm_dylib_handles.get(&(module_id, lib_name_ident)) {
            return Ok(*handle);
        }

        let lib_name_str = self.ast.idents.get_string(lib_name_ident);
        let ext = self.host_platform().dylib_ext();
        debug!("cwd is: {}", std::env::current_dir().unwrap().display());
        debug!("src_path is: {}", self.ast.idents.get_string(self.config.src_path));

        let search_path = kpath::join_tmp(
            self.get_tmp_unsafe(),
            &self.ast.idents,
            self.modules.get(module_id).home_dir,
            (compiler::LIBS_DIR_NAME, format_args!("lib{lib_name_str}.{ext}")),
        );
        if let Some(handle) = self.attempt_dlopen(search_path.as_str()) {
            self.vm_dylib_handles.insert((module_id, lib_name_ident), handle);
            return Ok(handle);
        }

        // System lookup
        let logical_lib_path = lib_name_str;
        if let Some(handle) = self.attempt_dlopen(logical_lib_path) {
            self.vm_dylib_handles.insert((module_id, lib_name_ident), handle);
            return Ok(handle);
        }
        // For the system lookup, we call dlerror() to see everywhere it tried.
        let dlopen_error_message =
            unsafe { std::ffi::CStr::from_ptr(libc::dlerror()) }.to_string_lossy();
        Err(kerr!(
            self,
            span,
            "Failed to dlopen library: '{}'. I tried the project's libs: `{}`, then tried a system lookup: {}",
            lib_name_str,
            search_path,
            dlopen_error_message
        ))
    }

    pub fn attempt_dlopen(&self, name: &str) -> Option<*mut c_void> {
        let c_lib_name = std::ffi::CString::new(name).unwrap();
        let handle = unsafe { libc::dlopen(c_lib_name.as_ptr(), libc::RTLD_LAZY) };
        if handle.is_null() { None } else { Some(handle) }
    }
}

pub(super) enum EmissionOrigin {
    Macro(FunctionId),
    Label(&'static str),
}

const HEADER_VALUE_MAX_BYTES: usize = 160;

struct HeaderValueWriter<'a> {
    content: &'a mut String,
    budget: usize,
}

impl std::fmt::Write for HeaderValueWriter<'_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        for c in s.chars() {
            let escape =
                crate::lex::SHARED_STRING_ESCAPED_CHARS.iter().find(|e| e.output as char == c);
            let len = if escape.is_some() { 2 } else { c.len_utf8() };
            if len > self.budget {
                return Err(std::fmt::Error);
            }
            self.budget -= len;
            match escape {
                Some(e) => {
                    self.content.push('\\');
                    self.content.push(e.sentinel);
                }
                None => self.content.push(c),
            }
        }
        Ok(())
    }
}

fn write_header_value(
    content: &mut String,
    write: impl FnOnce(&mut HeaderValueWriter) -> std::fmt::Result,
) {
    let mut w = HeaderValueWriter { content, budget: HEADER_VALUE_MAX_BYTES };
    if write(&mut w).is_err() {
        w.content.push('…');
    }
}
