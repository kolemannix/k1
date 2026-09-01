// Copyright (c) 2026 knix
// All rights reserved.

use super::*;
use crate::ice_span;

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
                    let ast_id = self.globals.get(global_id).ast_id;
                    if self.eval_global_body(ast_id).is_err() {
                        return None;
                    }
                }
                self.globals.get(global_id).initial_value.as_value()
            }
            TypedExpr::Call { call_id, .. } => {
                // desugar calls to zeroed() to the optimized zero repr for that type
                let call = self.calls.get(*call_id);
                let function_id = call.callee.maybe_function_id()?;
                let function = self.functions.get(function_id);
                if let Some(Builtin::Ir(BuiltinIr::Zeroed)) = function.builtin_type {
                    let return_type_id = self.exprs.get_type(expr_id);
                    let expr_span = self.exprs.get_span(expr_id);
                    self.warn_if_not_zerosafe(return_type_id, expr_span);
                    Some(self.static_values.add(StaticValue::Zero(return_type_id)))
                } else {
                    None
                }
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

    pub fn warn_if_not_zerosafe(&mut self, type_id: TypeId, span: SpanId) {
        if !self.get_type_variable_counts(type_id).is_zero_safe {
            self.report_warn(span, k1_format_user!(self, "Type {} is not zero-safe", type_id))
        }
    }

    pub fn compile_all_pending_ir(&mut self, on_behalf_of_span: SpanId) -> K1Result<()> {
        loop {
            if let Some(function_id) = self.ir.units_pending_compile.keys().next().copied() {
                self.ir.units_pending_compile.remove(&function_id);
                if let Err(e) = self.eval_function_body(function_id) {
                    // re-insert so all future attempts to drain also fail.
                    // we could instead set a failure flag and not even try, but this is ok for now
                    self.ir.units_pending_compile.insert(function_id, ());
                    return Err(e);
                }
                if let Err(e) = ir::compile_function(self, function_id) {
                    // re-insert so all future attempts to drain also fail.
                    // we could instead set a failure flag and not even try, but this is ok for now
                    self.ir.units_pending_compile.insert(function_id, ());
                    kbail!(
                        self,
                        on_behalf_of_span,
                        "Failed to compile ir for function execution: {}",
                        e.message
                    );
                };
            } else if let Some(global_id) = self.ir.globals_pending_eval.keys().next().copied() {
                self.ir.globals_pending_eval.remove(&global_id);
                let ast_id = self.globals.get(global_id).ast_id;
                self.eval_global_body(ast_id)?;
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
        input_parameters: &[(VariableId, StaticValueId)],
    ) -> K1Result<StaticValueId> {
        let expr = self.compile_parsed_expr_for_exec(parsed_expr, ctx, input_parameters)?;

        // Intercepts very simple expressions to avoid a VM execution
        if let Some(shortcut_value_id) = self.convert_trivial_static_block(expr) {
            return Ok(shortcut_value_id);
        }
        let execution_result = bc::exec::execute_compiled_expr(self, vm, expr, true);

        vm.reset(self.global_id_k1_arena);

        let static_value_id = execution_result?;

        Ok(static_value_id)
    }

    /// Typecheck, compile, and optimize a static expr's unit for execution
    pub(super) fn compile_parsed_expr_for_exec(
        &mut self,
        parsed_expr: ParsedExprId,
        ctx: EvalExprContext,
        input_parameters: &[(VariableId, StaticValueId)],
    ) -> K1Result<TypedExprId> {
        if let ParsedExpr::Static(_static) = self.ast.exprs.get(parsed_expr) {
            self.report_warn(
                self.ast.exprs.get_span(parsed_expr),
                "This #static is immediately inside a static",
            )
        }

        let parsed_expr_as_block =
            self.ensure_parsed_expr_to_block(parsed_expr, ParsedBlockKind::FunctionBody);
        let expr_span = parsed_expr_as_block.span;
        let static_block_scope =
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::LexicalBlock, ScopeOwnerId::None);
        let mut cur_scope = ctx.scope_id;
        let mut locals_to_mask = self.tmp.new_list(0);

        // Mask every local up to and including the enclosing function scope, minus
        // the input_parameters. Namespace scopes hold globals, which the VM can read.
        // A better system would be just to run those expressions statically like we do for value macro args
        loop {
            let s = self.scopes.get_scope(cur_scope);
            let parent = s.parent;
            if s.scope_type == ScopeType::Namespace {
                break;
            }

            for (name, vis, _) in self.scopes.iter_scope_variables(cur_scope) {
                let Some(variable_id) = vis.variable_id() else {
                    continue;
                };
                if !input_parameters.iter().any(|(input_var_id, _)| *input_var_id == variable_id) {
                    locals_to_mask.push_grow(&mut self.tmp, name);
                }
            }

            if s.scope_type == ScopeType::FunctionScope {
                break;
            }
            if let Some(parent) = parent { cur_scope = parent } else { break }
        }
        for name in locals_to_mask.as_slice() {
            self.scopes.mask_variable(static_block_scope, *name);
        }

        let static_eval_ctx = ctx.with_scope(static_block_scope);
        let expr = self.eval_block(&parsed_expr_as_block, static_eval_ctx, true)?;
        let is_debug = self.ast.exprs.is_debug(parsed_expr);
        if is_debug {
            eprintln!("COMPILED TO BLOCK\n\n{}", self.expr_to_string_with_type(expr));
        }

        ir::compile_top_level_expr(self, expr, input_parameters, is_debug)?;
        self.compile_all_pending_ir(expr_span)?;
        ir::optimize_unit(self, IrUnitId::Expr(expr));
        if is_debug {
            eprintln!(
                "executing optimized unit.\n{}",
                ir::unit_to_string(self, IrUnitId::Expr(expr), false)
            );
        }
        Ok(expr)
    }

    pub(super) fn execute_static_expr(
        &mut self,
        parsed_expr: ParsedExprId,
        ctx: EvalExprContext,
        input_parameters: &[(VariableId, StaticValueId)],
    ) -> K1Result<StaticValueId> {
        let span = self.ast.exprs.get(parsed_expr).get_span();
        let infer_start = if ctx.is_inference() { Some(self.timing.clock.raw()) } else { None };
        let result = self.do_with_vm(span, |k1, vm| {
            k1.execute_parsed_expr_with_vm(vm, parsed_expr, ctx, input_parameters)
        });
        if let Some(start) = infer_start {
            self.timing.total_infer_execs += 1;
            self.timing.total_infer_exec_nanos += self.timing.clock.elapsed_nanos(start) as i64;
        }
        result
    }

    pub(super) fn do_with_vm<T>(
        &mut self,
        _span: SpanId,
        mut f: impl FnMut(&mut TypedProgram, &mut vm::Vm) -> T,
    ) -> T {
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

        res
    }

    pub(super) fn execute_static_function(
        &mut self,
        function_id: FunctionId,
        function_parameters: &[StaticValueId],
        span: SpanId,
    ) -> K1Result<StaticValueId> {
        self.do_with_vm(span, |k1, vm| {
            let result =
                Self::static_exec_function_with_vm(k1, vm, function_id, function_parameters, span);
            vm.reset(k1.global_id_k1_arena);
            result
        })
    }

    pub(super) fn static_exec_prepare_function(
        k1: &mut TypedProgram,
        function_id: FunctionId,
        span: SpanId,
    ) -> K1Result<()> {
        ir::compile_function(k1, function_id)?;
        k1.compile_all_pending_ir(span)?;
        // Macros execute repeatedly; inline_done guards re-optimizing
        let unit_id = IrUnitId::Function(function_id);
        if !ir::get_compiled_unit(&k1.ir, unit_id).unwrap().inline_done {
            ir::optimize_unit(k1, unit_id);
        }
        Ok(())
    }

    /// Compile, optimize, and run; no VM reset, so callers control when the
    /// result's VM memory dies
    pub(super) fn static_exec_function_with_vm(
        k1: &mut TypedProgram,
        vm: &mut vm::Vm,
        function_id: FunctionId,
        function_parameters: &[StaticValueId],
        span: SpanId,
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
        let vm_cond_result = self.execute_static_expr(
            cond,
            ctx.with_expected_type(Some(BOOL_TYPE_ID)).with_static_ctx(Some(StaticExecContext {
                expected_return_type: Some(BOOL_TYPE_ID),
            })),
            &[],
        )?;
        let StaticValue::Bool(condition_bool) = self.static_values.get(vm_cond_result) else {
            let cond_span = self.ast.get_expr_span(cond);
            kbail!(self, cond_span, "Condition is not a boolean");
        };
        Ok(*condition_bool)
    }

    pub(super) fn declare_global(
        &mut self,
        parsed_global_id: ParsedGlobalId,
        scope_id: ScopeId,
    ) -> K1Result<Option<VariableId>> {
        if let Some(global_id) = self.global_ast_mappings.get(&parsed_global_id) {
            return Ok(Some(self.globals.get(*global_id).variable_id));
        }
        let parsed = *self.ast.get_global(parsed_global_id);
        if !self.execute_static_condition(parsed.compile_condition, scope_id) {
            return Ok(None);
        }
        let owner_ns = match self.scopes.get_scope(scope_id).owner_id {
            ScopeOwnerId::Namespace(ns) => ns,
            _ => kbail!(self, SpanId::NONE, "declare_global's scope_id should be an namespace"),
        };
        if parsed.is_thread_local {
            self.fail_if_reload_ns(owner_ns, parsed.span, "tls globals")?;
        }
        if parsed.is_external || parsed.is_export {
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
                is_exported: parsed.is_export,
                is_external: parsed.is_external,
                link_name: parsed.link_name,
                ast_id: parsed_global_id,
                parent_scope: scope_id,
                reload_ns,
            },
            global_id,
        );

        if scope_id == self.scopes.mem_scope_id && parsed.name == self.ast.idents.b.arena_tmp {
            self.global_id_k1_arena = Some(global_id)
        };
        self.global_ast_mappings.insert(parsed_global_id, global_id);
        self.scopes.add_variable(scope_id, parsed.name, variable_id);

        self.emit_ls_entity(parsed.name_span, LsEntityKind::Variable { variable_id });

        Ok(Some(variable_id))
    }

    pub fn eval_global_body(&mut self, parsed_global_id: ParsedGlobalId) -> K1Result<()> {
        let Some(global_id) = self.global_ast_mappings.get(&parsed_global_id).copied() else {
            // This means we failed to compile the definition; or we have a bug!
            // TODO: Store failures so we can be certain which is true!
            debug!("skipping rest of global body");
            return Ok(());
        };
        // Evaluation is one-shot; the pre-execution drain may get here before the body phase
        if !self.globals.get(global_id).initial_value.is_pending() {
            return Ok(());
        }
        if self.globals_in_progress.contains(&global_id) {
            let global_name = |id: &TypedGlobalId| {
                self.ident_str(self.variables.get(self.globals.get(*id).variable_id).name)
            };
            let mut cycle = String::new();
            for id in self.globals_in_progress.iter() {
                cycle.push_str(global_name(id));
                cycle.push_str(" -> ");
            }
            cycle.push_str(global_name(&global_id));
            kbail!(
                self,
                self.ast.get_global(parsed_global_id).span,
                "Global initializer cycle: {}",
                cycle,
            );
        }
        self.globals_in_progress.push(global_id);
        let result =
            self.with_clean_inference(|k1| k1.eval_global_body_inner(parsed_global_id, global_id));
        let popped = self.globals_in_progress.pop();
        debug_assert_eq!(popped, Some(global_id));
        result
    }

    pub(super) fn eval_global_body_inner(
        &mut self,
        parsed_global_id: ParsedGlobalId,
        global_id: TypedGlobalId,
    ) -> K1Result<()> {
        let parsed_global = *self.ast.get_global(parsed_global_id);
        let typed_global = self.globals.get(global_id);
        let is_external = typed_global.is_external;
        let parsed_expr = typed_global.parsed_expr;
        let scope_id = typed_global.parent_scope;
        let declared_type = typed_global.type_id;
        let variable_id = typed_global.variable_id;
        let value_expr_id = if is_external {
            match parsed_expr {
                None => {
                    // Evaluated, but there is no compile-time value: storage arrives at
                    // link time. Recording this keeps evaluation one-shot
                    self.globals.get_mut(global_id).initial_value = GlobalInitialValue::Uninit;
                    return Ok(());
                }
                Some(_id) => {
                    kbail!(self, parsed_global.span, "External globals cannot have initializers");
                }
            }
        } else {
            match parsed_expr {
                None => kbail!(self, parsed_global.span, "Global has no initializer"),
                Some(id) => id,
            }
        };

        let global_name = parsed_global.name;
        let global_span = parsed_global.span;

        let expected_type_for_execution = match self.get_static_type_of_type(declared_type) {
            Some(s) => s.family_type_id,
            None => declared_type,
        };

        let static_value_id = if let ParsedExpr::Builtin(span) = self.ast.exprs.get(value_expr_id) {
            let span = *span;
            self.eval_builtin_global(global_name, scope_id, expected_type_for_execution, span)?
        } else if let ParsedExpr::Call(call) = self.ast.exprs.get(value_expr_id)
            && call.name.name == self.ast.idents.b.module_params
            && {
                let path = self.ast.mem.getn(call.name.path);
                path.len() == 1 && path[0].name == self.ast.idents.b.k1
            }
        {
            let (call_span, call_args) = (call.span, call.args);
            self.handle_module_params_decl_call(
                global_id,
                call_span,
                call_args,
                expected_type_for_execution,
                scope_id,
            )?
        } else {
            let ctx = EvalExprContext::make(scope_id)
                .with_expected_type(Some(expected_type_for_execution))
                .with_static_ctx(Some(StaticExecContext {
                    expected_return_type: Some(expected_type_for_execution),
                }));
            self.execute_static_expr(value_expr_id, ctx, &[])?
        };
        let static_value_type_id = self.get_static_value_type(static_value_id);

        match self.get_static_type_of_type(declared_type) {
            None => {
                if let Err(msg) = self.check_types(declared_type, static_value_type_id, scope_id) {
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
        span: SpanId,
    ) -> K1Result<StaticValueId> {
        let name = self.ident_str(defn_name);
        let float = match (name, expected_type_id) {
            ("NAN", F32_TYPE_ID) => Some(TypedFloatValue::F32(f32::NAN)),
            ("INFINITY", F32_TYPE_ID) => Some(TypedFloatValue::F32(f32::INFINITY)),
            ("NEG_INFINITY", F32_TYPE_ID) => Some(TypedFloatValue::F32(f32::NEG_INFINITY)),
            ("NAN", F64_TYPE_ID) => Some(TypedFloatValue::F64(f64::NAN)),
            ("INFINITY", F64_TYPE_ID) => Some(TypedFloatValue::F64(f64::INFINITY)),
            ("NEG_INFINITY", F64_TYPE_ID) => Some(TypedFloatValue::F64(f64::NEG_INFINITY)),
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
            "test" => self.config.is_test_build,
            "no-std" => self.config.no_std,
            "debug" => self.config.debug,
            // The VM overrides this global's value during static execution
            "is-static" => false,
            "platform" => {
                let platform_tag = self.config.target.platform() as u8;
                let static_enum =
                    StaticValue::Enum(expected_type_id, TypedIntValue::U8(platform_tag));
                return Ok(self.static_values.add(static_enum));
            }
            "host-platform" => {
                let host_platform = self.config.host_platform();
                let static_enum =
                    StaticValue::Enum(expected_type_id, TypedIntValue::U8(host_platform as u8));
                return Ok(self.static_values.add(static_enum));
            }
            "simd-bytes" => {
                let width = self.config.simd_bytes as i64;
                return Ok(self.static_values.add(StaticValue::Int(TypedIntValue::I64(width))));
            }
            s => kbail!(self, span, "Unknown builtin name: {s}"),
        };
        Ok(self.static_values.add(StaticValue::Bool(bool_value)))
    }

    pub(super) fn handle_module_params_decl_call(
        &mut self,
        global_id: TypedGlobalId,
        call_span: SpanId,
        call_args: AstSlice<ParsedCallArg>,
        schema_type: TypeId,
        scope_id: ScopeId,
    ) -> K1Result<StaticValueId> {
        let global = self.globals.get(global_id);
        let global_span = global.span;
        let global_parent_scope = global.parent_scope;
        let file_id = self.ast.spans.get(global_span).file_id;
        let Some(module_id) =
            self.modules.iter().find(|m| m.root_file_id(&self.mem) == file_id).map(|m| m.id)
        else {
            kbail!(
                self,
                call_span,
                "k1/module-params must be declared in the module's root file \
                 (module.k1 or <module-name>.k1)"
            );
        };
        let module = self.modules.get(module_id);
        let module_name = module.name;
        if global_parent_scope != module.namespace_scope_id {
            kbail!(
                self,
                global_span,
                "k1/module-params must initialize a top-level global of module '{}'",
                self.ident_str(module_name)
            );
        }
        if module.params.is_some() {
            kbail!(
                self,
                global_span,
                "Module '{}' already declared its parameters; k1/module-params may \
                 appear once per module",
                self.ident_str(module_name)
            );
        }
        let Some(schema_struct) = self.types.get(schema_type).as_struct() else {
            kbail!(
                self,
                global_span,
                "The type annotation of a k1/module-params global is the params schema \
                 and must be a struct type"
            );
        };
        let schema_fields = self.mem.getn(schema_struct.fields);

        let args = self.ast.mem.getn(call_args);
        let defaults: Option<SV8<StaticValueId>> = match args {
            [] => None,
            [defaults_arg] => {
                let defaults_expr = defaults_arg.value;
                let defaults_span = self.ast.exprs.get_span(defaults_expr);
                let ctx = EvalExprContext::make(scope_id)
                    .with_expected_type(Some(schema_type))
                    .with_static_ctx(Some(StaticExecContext {
                        expected_return_type: Some(schema_type),
                    }));
                let value_id = self.execute_static_expr(defaults_expr, ctx, &[])?;
                let value_type = self.get_static_value_type(value_id);
                if let Err(msg) = self.check_types(schema_type, value_type, scope_id) {
                    kbail!(self, defaults_span, "Type mismatch in params defaults: {msg}");
                }
                let StaticValue::Struct(s) = self.static_values.get(value_id) else {
                    kbail!(
                        self,
                        defaults_span,
                        "params defaults did not evaluate to a struct value"
                    );
                };
                Some(SmallVec::from_slice(self.static_values.get_slice(s.fields)))
            }
            _ => kbail!(
                self,
                call_span,
                "k1/module-params takes at most one argument: the defaults struct"
            ),
        };

        let mut providers: SV4<(StringId, ParsedExprId)> = smallvec![];
        for m in self.modules.iter() {
            for entry in self.mem.getn(m.manifest.deps) {
                if entry.name == module_name
                    && let Some(params_expr) = entry.params_struct_literal
                {
                    providers.push((m.name, params_expr));
                }
            }
        }

        let mut bound: SV8<Option<(StaticValueId, StringId)>> =
            smallvec![None; schema_fields.len()];
        for (provider_name, params_expr) in providers {
            let ParsedExpr::Struct(s) = self.ast.exprs.get(params_expr) else {
                self.ice_span(call_span, "captured dep params was not a struct literal");
            };
            let literal_fields = self.ast.mem.getn(s.fields);
            for field in literal_fields {
                let Some(field_index) = schema_fields.iter().position(|f| f.name == field.name)
                else {
                    kbail!(
                        self,
                        field.span,
                        "Module '{}' has no parameter '{}'",
                        self.ident_str(module_name),
                        self.ident_str(field.name)
                    );
                };
                let field_type = schema_fields[field_index].type_id;
                let StructValueFieldKind::Expr(field_expr) = field.value else {
                    kbail!(
                        self,
                        field.span,
                        "module params must be provided as explicit field values"
                    );
                };
                let ctx = EvalExprContext::make(Scopes::ROOT_SCOPE_ID)
                    .with_expected_type(Some(field_type))
                    .with_static_ctx(Some(StaticExecContext {
                        expected_return_type: Some(field_type),
                    }));
                let value_id = self.execute_static_expr(field_expr, ctx, &[])?;
                let value_type = self.get_static_value_type(value_id);
                if let Err(msg) = self.check_types(field_type, value_type, Scopes::ROOT_SCOPE_ID) {
                    kbail!(
                        self,
                        field.span,
                        "Type mismatch for parameter '{}' of module '{}': {}",
                        self.ident_str(field.name),
                        self.ident_str(module_name),
                        msg
                    );
                }
                match bound[field_index] {
                    None => bound[field_index] = Some((value_id, provider_name)),
                    Some((existing_id, existing_provider)) => {
                        if existing_id != value_id {
                            kbail!(
                                self,
                                field.span,
                                "Conflicting values for parameter '{}' of module '{}': \
                                 {} (from '{}') vs {} (from '{}')",
                                field.name,
                                module_name,
                                self.static_value_to_string(existing_id),
                                existing_provider,
                                self.static_value_to_string(value_id),
                                provider_name,
                            );
                        }
                    }
                }
            }
        }

        let mut merged: List<StaticValueId, _> =
            self.static_values.mem.new_list(schema_fields.len() as u32);
        for (i, schema_field) in schema_fields.iter().enumerate() {
            let value_id = match bound[i] {
                Some((id, _)) => id,
                None => match &defaults {
                    Some(d) => d[i],
                    None => kbail!(
                        self,
                        call_span,
                        "Missing required parameter '{}' for module '{}'",
                        self.ident_str(schema_field.name),
                        self.ident_str(module_name)
                    ),
                },
            };
            merged.push(value_id);
        }
        let fields_slice = merged.to_slice();
        let merged_id = self
            .static_values
            .add(StaticValue::Struct(StaticStruct { type_id: schema_type, fields: fields_slice }));
        self.modules.get_mut(module_id).params =
            Some(ModuleParams { schema_type, value_id: merged_id });
        Ok(merged_id)
    }

    pub(super) fn add_static_value_expr(
        &mut self,
        value_id: StaticValueId,
        span: SpanId,
    ) -> TypedExprId {
        let inner_type_id = self.get_static_value_type(value_id);
        let static_type_id = self.add_value_type(inner_type_id, Some(value_id));
        self.exprs.add_static(value_id, static_type_id, true, span)
    }

    pub(super) fn add_static_constant_expr(
        &mut self,
        value_id: StaticValueId,
        span: SpanId,
    ) -> TypedExprId {
        let type_id = self.get_static_value_type(value_id);
        self.exprs.add_static(value_id, type_id, false, span)
    }

    /// A constant `code` value: a list of chunks, each text plus its source span
    pub(super) fn make_static_code_value(
        &mut self,
        chunks: &[(StringId, SpanId)],
    ) -> StaticValueId {
        let code_type = self.builtin_types.code();
        let chunk_type = self.builtin_types.code_chunk();
        let chunks_list_type =
            self.instantiate_generic_type(self.builtin_types.list(), &[chunk_type]);
        let mut elements = self.static_values.mem.new_list(chunks.len() as u32);
        for (text, source) in chunks {
            let text_value = self.static_values.add_string(*text);
            let source_value =
                self.static_values.add_int(TypedIntValue::U64(source.as_u32() as u64));
            let chunk_value =
                self.static_values.add_struct_from_slice(chunk_type, &[text_value, source_value]);
            elements.push(chunk_value);
        }
        let elements = elements.to_slice();
        let chunks_value = self.static_values.add(StaticValue::LinearContainer(StaticContainer {
            elements,
            kind: StaticContainerKind::List,
            type_id: chunks_list_type,
        }));
        self.static_values.add_struct_from_slice(code_type, &[chunks_value])
    }

    pub(super) fn code_from_parsed_expr(&mut self, parsed_expr: ParsedExprId) -> StaticValueId {
        let arg_span = self.ast.exprs.get_span(parsed_expr);
        let content =
            self.ast.sources.get_span_content(&self.ast.mem, self.ast.spans.get(arg_span));
        let string_id = self.ast.idents.intern(content);
        let value_id = self.make_static_code_value(&[(string_id, arg_span)]);
        value_id
    }

    pub(super) fn materialize_static_value(
        &mut self,
        family_type_id: TypeId,
        value_id: Option<StaticValueId>,
        span: SpanId,
    ) -> TypedExprId {
        match value_id {
            Some(value_id) => self.add_static_constant_expr(value_id, span),
            None => self.synth_phony(family_type_id, span),
        }
    }

    /// Compiles `#static <expr>` and `#meta <expr>` constructs
    pub(super) fn compile_static_or_meta(
        &mut self,
        _expr_id: ParsedExprId,
        stat: ParsedStaticExpr,
        is_definition: bool,
        ctx: EvalExprContext,
    ) -> K1Result<StaticExecutionResult> {
        let span = stat.span;
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
            let mut macro_args: SV8<_> = smallvec![];
            for parsed_arg in self.ast.mem.getn(call.args) {
                macro_args.push(MacroArg::Parsed(*parsed_arg))
            }
            return self.execute_macro_call(
                self.ast.mem.getn(call.type_args),
                &macro_args,
                span,
                function_id,
                true,
                ctx,
            );
        }

        // We don't execute statics during the generic pass, since there's no point
        // 1. we don't know the real types of generics, thus values of things like schemas, etc
        // 2. There's not really a use-case for it, metaprograms always want to generate
        //    real code
        //
        // So we just return the expected type, or a unit
        debug!("eval_static_expr ctx.is_generic_pass={}", ctx.is_generic_pass());
        if ctx.is_generic_pass() {
            let phony_type = ctx.expected_type_id.unwrap_or(EMPTY_TYPE_ID);
            let phony_expr = self.synth_phony(phony_type, span);
            return Ok(StaticExecutionResult::TypedExpr(phony_expr));
        }

        let kind = stat.kind;
        let expected_type_for_execution = match kind {
            ParsedStaticBlockKind::Value => match ctx.expected_type_id {
                None => None,
                Some(expected_type_id) => match self.get_static_type_of_type(expected_type_id) {
                    Some(s) => Some(s.family_type_id),
                    None => Some(expected_type_id),
                },
            },
            ParsedStaticBlockKind::Metaprogram => self.builtin_types.code,
            ParsedStaticBlockKind::MacroCall => unreachable!(),
        };
        let mut static_parameters: SV4<(VariableId, StaticValueId)> = smallvec![];
        for param in self.ast.mem.getn(stat.parameter_names) {
            let variable_expr = self.ast.exprs.add(ParsedExpr::Variable(ParsedVariable {
                name: QIdent::naked(param.name, param.span),
                span: param.span,
            }));
            let (variable_id, variable_expr) = self.eval_variable(variable_expr, ctx, false)?;
            let Some(variable_id) = variable_id else {
                kbail!(self, param.span, "Must be a plain variable");
            };
            let variable_type = self.exprs.get_type(variable_expr);

            self.register_variable_usage(variable_id, param.span);
            debug!(
                "Variable {} will be passed in to static execution",
                self.ident_str(self.variables.get(variable_id).name),
            );
            match self.types.get(variable_type) {
                Type::StaticValue(svt) => {
                    if let Some(value_id) = svt.value_id {
                        static_parameters.push((variable_id, value_id));
                    } else {
                        kbail!(
                            self,
                            param.span,
                            "Value type parameter `{}` is unresolved",
                            param.name
                        );
                    }
                }
                Type::TypeParameter(tp) if tp.static_constraint.is_some() => {
                    let static_type =
                        self.types.get(tp.static_constraint.unwrap()).as_value_type().unwrap();
                    let Some(value_id) = static_type.value_id else {
                        kbail!(
                            self,
                            param.span,
                            "Expected a statically-known, 'value' type for argument {}, got a value family",
                            param.name,
                        );
                    };
                    static_parameters.push((variable_id, value_id));
                }
                _ => {
                    kbail!(
                        self,
                        param.span,
                        "Expected a statically-known, 'value' type for argument {}, but got a {}",
                        param.name,
                        variable_type
                    );
                }
            }
        }
        let exec_ctx = ctx.with_expected_type(expected_type_for_execution).with_static_ctx(Some(
            StaticExecContext { expected_return_type: expected_type_for_execution },
        ));
        match kind {
            ParsedStaticBlockKind::Value => {
                let vm_result =
                    self.execute_static_expr(base_expr, exec_ctx, &static_parameters)?;
                let expr = self.add_static_value_expr(vm_result, span);
                Ok(StaticExecutionResult::TypedExpr(expr))
            }
            ParsedStaticBlockKind::Metaprogram => {
                let emitted = self.do_with_vm(span, |k1, vm| {
                    let result = (|| {
                        let expr = k1.compile_parsed_expr_for_exec(
                            base_expr,
                            exec_ctx,
                            &static_parameters,
                        )?;
                        let raw = bc::exec::execute_compiled_expr_raw(k1, vm, expr, true)?;
                        Self::read_emitted_code_raw(k1, &raw, span, is_definition)
                    })();
                    vm.reset(k1.global_id_k1_arena);
                    result
                })?;
                self.compile_emitted_code(emitted, span, ctx, is_definition)
            }
            ParsedStaticBlockKind::MacroCall => unreachable!(),
        }
    }

    /// must run while vm memory is still good
    pub(super) fn read_emitted_code_raw(
        k1: &mut TypedProgram,
        raw: &bc::exec::RawUnitResult,
        span: SpanId,
        is_definition: bool,
    ) -> K1Result<Option<(String, PermSlice<CodeChunkPos>)>> {
        use crate::vm::k1_types::{K1Code, K1CodeChunk};
        if !raw.returns_value || Some(raw.result_type_id) != k1.builtin_types.code {
            kbail!(
                k1,
                span,
                "Metaprogram must evaluate to `code`; got {}. Wrap a plain string with code/from-string",
                raw.result_type_id
            );
        }
        #[cfg(debug_assertions)]
        k1.assert_code_layouts();
        let ferry_start = k1.timing.clock.raw();
        let code = unsafe { *(raw.ret_addr as *const K1Code) };
        let chunks = unsafe {
            std::slice::from_raw_parts(
                code.chunks.k1_buffer.data as *const K1CodeChunk,
                code.chunks.len as usize,
            )
        };
        let emitted = k1.build_emitted_source(span, is_definition, chunks);
        k1.timing.total_ferry_nanos += k1.timing.elapsed_nanos(ferry_start) as i64;
        emitted
    }

    pub(super) fn build_emitted_source(
        &mut self,
        span: SpanId,
        is_definition: bool,
        chunks: &[vm::k1_types::K1CodeChunk],
    ) -> K1Result<Option<(String, PermSlice<CodeChunkPos>)>> {
        let chunks_len: usize = chunks.iter().map(|c| c.text.len as usize).sum();
        if chunks_len == 0 {
            return Ok(None);
        }
        let (source, line) = self.get_span_location(span);
        let filepath = self.ast.idents.get_string(source.file_path);
        // The source keeps `content` forever, so size it exactly and move it
        // in; 48 covers the header boilerplate and block wrapper
        let mut content = String::with_capacity(chunks_len + filepath.len() + 48);
        writeln!(
            &mut content,
            "// generated by #meta block at {}:{}",
            filepath,
            line.line_number()
        )
        .unwrap();
        if !is_definition {
            content.push_str("{\n");
        }
        let mut table: List<CodeChunkPos, _> = self.mem.new_list(chunks.len() as u32);
        for chunk in chunks {
            let Ok(text) = (unsafe { chunk.text.to_str() }) else {
                kbail!(self, span, "Metaprogram produced a non-utf8 chunk");
            };
            if text.is_empty() {
                continue;
            }
            let start = content.len() as u32;
            content.push_str(text);
            if let Some(source) = SpanId::from_u32(chunk.source as u32) {
                table.push(CodeChunkPos { start, end: start + text.len() as u32, source });
            }
        }
        if !is_definition {
            content.push_str("\n}");
        }
        Ok(Some((content, table.to_slice())))
    }

    #[cfg(debug_assertions)]
    pub(super) fn assert_code_layouts(&mut self) {
        use crate::vm::k1_types::{K1Code, K1CodeChunk};
        let code_type = self.builtin_types.code.unwrap();
        let chunk_type = self.builtin_types.code_chunk.unwrap();
        assert_eq!(self.get_layout(code_type).unwrap().size as usize, size_of::<K1Code>());
        assert_eq!(self.get_layout(chunk_type).unwrap().size as usize, size_of::<K1CodeChunk>());
        let fields = self.get_struct_layout(chunk_type);
        assert_eq!(fields[0].offset as usize, std::mem::offset_of!(K1CodeChunk, text));
        assert_eq!(fields[1].offset as usize, std::mem::offset_of!(K1CodeChunk, source));
    }

    /// parses emitted source as a fresh file, then compiles it in place of the invocation
    pub(super) fn compile_emitted_code(
        &mut self,
        emitted: Option<(String, PermSlice<CodeChunkPos>)>,
        span: SpanId,
        ctx: EvalExprContext,
        is_definition: bool,
    ) -> K1Result<StaticExecutionResult> {
        let Some((content, table)) = emitted else {
            return if is_definition {
                Ok(StaticExecutionResult::Definitions(MSlice::empty()))
            } else {
                Ok(StaticExecutionResult::TypedExpr(self.synth_empty_value(span)))
            };
        };
        let content_hash = if is_definition {
            None
        } else {
            use std::hash::{Hash, Hasher};
            let mut h = ahash::AHasher::default();
            content.hash(&mut h);
            let hash = h.finish();
            if let Some(&cached_root) = self.emitted_parse_cache.get(&hash) {
                let typed_metaprogram = self.eval_expr(cached_root, ctx)?;
                return Ok(StaticExecutionResult::TypedExpr(typed_metaprogram));
            }
            Some(hash)
        };
        // Parse the code as its own file (cohesive spans, a source containing
        // the full text), then compile it in place of the invocation
        //
        // TODO: when specializing, include the specialization context in the
        //       filename and print the types at the top of the file; a
        //       'what are we compiling' stack would provide it
        let (source, line) = self.get_span_location(span);
        let line_number = line.line_number();
        let stem = source.filename_str(&self.ast.idents).strip_suffix(".k1").unwrap();
        let serial = self.emitted_sources.len() + 1;
        let generated_filename = k1_format!(self, &(), "meta_{stem}_{line_number}_{serial}.k1");
        let generated_dir = self.config.out_dir_generated;
        let generated_path = kpath::join_id(
            &self.ast.idents,
            &mut self.tmp,
            generated_dir,
            generated_filename.as_str(),
        );
        debug!("Emitted source:\n---\n{content}\n---");
        let emitted_file =
            crate::parse::SourceFile::make(&mut self.ast.mem, generated_path, &content);
        let source_for_emission = self.ast.sources.add_file(emitted_file);
        debug_assert!(
            self.emitted_sources.last().is_none_or(|e| e.file_id < source_for_emission),
            "emitted_sources must stay sorted by file_id for binary search"
        );
        self.emitted_sources.push(EmittedSource {
            file_id: source_for_emission,
            call_span: span,
            entries: table,
            has_diagnostic: false,
        });

        let parse_kind =
            if is_definition { ParseAdHocKind::Definitions } else { ParseAdHocKind::Expr };
        let parsed_metaprogram = self.parse_metaprogram_source(source_for_emission, parse_kind)?;
        match parsed_metaprogram {
            ParseMetaprogramResult::Expr(parsed_expr_id) => {
                if let Some(hash) = content_hash {
                    self.emitted_parse_cache.insert(hash, parsed_expr_id);
                }
                let typed_metaprogram = self.eval_expr(parsed_expr_id, ctx)?;
                debug!("Emitted compiled expr:\n{}", self.expr_to_string(typed_metaprogram));
                Ok(StaticExecutionResult::TypedExpr(typed_metaprogram))
            }
            ParseMetaprogramResult::Definitions(defns_slice) => {
                Ok(StaticExecutionResult::Definitions(defns_slice))
            }
        }
    }

    /// Emitted sources accumulate in the sources pool during typechecking; this
    /// writes them out for inspection in one pass, off the expansion path
    pub fn write_emitted_sources(&self) {
        let start = std::time::Instant::now();
        for emitted in &self.emitted_sources {
            if emitted.has_diagnostic {
                let source = self.ast.sources.get(emitted.file_id);
                let path = self.ast.idents.get_string(source.file_path);
                if let Err(e) = std::fs::write(Path::new(path), source.content(&self.ast.mem)) {
                    eprintln!("Failed to write out generated metaprogram at {path}. {e}");
                }
            }
        }
        if self.config.chatty {
            let elapsed = start.elapsed();
            eprintln!("Wrote {} emitted sources in {:.2?}", self.emitted_sources.len(), elapsed);
            let mut real_files = 0usize;
            let mut real_bytes = 0usize;
            let mut emitted_bytes = 0usize;
            let mut newline_bytes = 0usize;
            let mut trivia_bytes = 0usize;
            let mut token_bytes = 0usize;
            for (file_id, source) in self.ast.sources.iter() {
                if self.emitted_source_for_file(file_id).is_some() {
                    emitted_bytes += source.content_len();
                } else {
                    real_files += 1;
                    real_bytes += source.content_len();
                }
                newline_bytes += source.newline_count() * size_of::<u32>();
                trivia_bytes += source.trivia.len() as usize * size_of::<crate::lex::TriviaEntry>();
                token_bytes += source.tokens.len() as usize * size_of::<crate::lex::Token>();
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
                "\tsources: {} files {}kb + {} emitted {}kb ({} unique, {}kb); line tables {}kb, trivia {}kb, tokens {}kb",
                real_files,
                real_bytes / crate::KILOBYTE,
                self.emitted_sources.len(),
                emitted_bytes / crate::KILOBYTE,
                unique_emissions.len(),
                unique_bytes / crate::KILOBYTE,
                newline_bytes / crate::KILOBYTE,
                trivia_bytes / crate::KILOBYTE,
                token_bytes / crate::KILOBYTE,
            );
        }
    }

    pub(super) fn execute_macro_call(
        &mut self,
        type_args: &[NamedTypeArg],
        args: &[MacroArg],
        span: SpanId,
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
        if function.type_params.len() as usize != type_args.len() {
            kbail!(
                self,
                span,
                "Takes {} type arguments; got {}",
                function.type_params.len(),
                type_args.len()
            );
        }

        let type_expr_context = EvalTypeExprContext {
            is_direct_function_parameter: true,
            is_inside_type_definition_rhs: false,
            is_inside_static_type: false,
        };
        let (typed_type_args, _subst_pairs) = self.check_type_args_against_params(
            function_type_params,
            type_args,
            ctx.scope_id,
            type_expr_context,
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

        // Ensure the function is compiled.
        self.eval_function_body(function_id)?;

        // Specialize if needed
        let function_to_run = if is_generic {
            let type_args = TypeArgs::from_slice_in(self.mem.getn(typed_type_args), &mut self.mem);
            let spec_fn_id =
                self.specialize_function_declaration(type_args, TypeArgs::empty(), function_id);
            self.specialize_function_body(spec_fn_id)?;
            spec_fn_id
        } else {
            function_id
        };

        let fn_param_types = self
            .types
            .get(self.get_function(function_to_run).type_id)
            .as_function()
            .unwrap()
            .logical_params();

        let mut static_args: SV8<StaticValueId> = smallvec![];
        for (param, arg) in self.mem.getn(fn_param_types).iter().zip(args.iter()) {
            match *arg {
                MacroArg::Parsed(parsed_arg) => {
                    if let Some(passed_name) = parsed_arg.name {
                        if passed_name != param.name {
                            kbail!(
                                self,
                                span,
                                "Macro argument name mismatch: expected {}, got {}",
                                param.name,
                                passed_name,
                            );
                        }
                    }
                    match param.is_macro_code {
                        true => {
                            // when the parameter is of type code,
                            // it gets auto 'quoted' at the callsite
                            // So we just yoink the source text from its span
                            // and actually ignore the previously parsed value.
                            // This means its kinda hard to synthesize one of these arguments
                            //
                            // which is why we're accepting MaybeTypedExpr here
                            let code_value_id = self.code_from_parsed_expr(parsed_arg.value);
                            static_args.push(code_value_id);
                        }
                        false => {
                            let value_id = self.execute_static_expr(
                                parsed_arg.value,
                                ctx.with_expected_type(Some(param.type_id)),
                                &[],
                            )?;
                            let value_type = self.get_static_value_type(value_id);
                            if let Err(msg) =
                                self.check_types(param.type_id, value_type, ctx.scope_id)
                            {
                                kbail!(
                                    self,
                                    span,
                                    "Macro argument '{}' type mismatch: {}",
                                    param.name,
                                    msg,
                                );
                            }
                            static_args.push(value_id);
                        }
                    };
                }
                MacroArg::Typed(typed_arg) => {
                    // We require a static value
                    if !param.is_macro_code {
                        kbail!(
                            self,
                            span, /* call span since we want to point the finger at the bad caller */
                            "bug: we only allow pre-typed macro arguments for code args",
                        );
                    }
                    let TypedExpr::StaticValue(sce) = self.exprs.get(typed_arg) else {
                        kbail!(
                            self,
                            span,
                            "bug: we expect a static value for pre-typed macro arguments",
                        );
                    };
                    static_args.push(sce.value_id)
                }
            }
        }

        self.run_macro_and_compile_output(function_to_run, &static_args, span, is_definition, ctx)
    }

    pub(super) fn run_macro_and_compile_output(
        &mut self,
        function_id: FunctionId,
        static_args: &[StaticValueId],
        span: SpanId,
        is_definition: bool,
        ctx: EvalExprContext,
    ) -> K1Result<StaticExecutionResult> {
        let emitted = self.do_with_vm(span, |k1, vm| {
            let result =
                Self::macro_emit_with_vm(k1, vm, function_id, static_args, span, is_definition);
            vm.reset(k1.global_id_k1_arena);
            result
        })?;
        self.compile_emitted_code(emitted, span, ctx, is_definition)
    }

    pub(super) fn macro_emit_with_vm(
        k1: &mut TypedProgram,
        vm: &mut vm::Vm,
        function_id: FunctionId,
        static_args: &[StaticValueId],
        span: SpanId,
        is_definition: bool,
    ) -> K1Result<Option<(String, PermSlice<CodeChunkPos>)>> {
        Self::static_exec_prepare_function(k1, function_id, span)?;
        let raw = bc::exec::execute_compiled_function_raw(k1, vm, function_id, static_args, true)?;
        Self::read_emitted_code_raw(k1, &raw, span, is_definition)
    }

    pub(super) fn with_parser<R>(
        &mut self,
        file_id: FileId,
        f: impl FnOnce(&mut parse::Parser) -> K1Result<R>,
    ) -> K1Result<R> {
        let mut tokens = std::mem::take(&mut self.buffers.lexer_tokens);
        tokens.clear();

        let module = self.modules.get(self.module_in_progress.unwrap());
        let parsed_namespace_id = module.parsed_namespace_id;
        let code_str = self.ast.sources.get(file_id).content(&self.ast.mem);
        let mut lexer = crate::lex::Lexer::make(code_str, &mut self.ast.spans, file_id);
        if let Err(e) = lexer.run(&mut tokens) {
            let e = ParseError::Lex(e);
            parse::print_error(&self.ast, &e);
            tokens.clear();
            self.buffers.lexer_tokens = tokens;
            kbail!(self, e.span(), "Failed to lex code emitted from here");
        };

        let mut p = crate::parse::Parser::make_for_file(
            module.id,
            module.name,
            parsed_namespace_id,
            &mut self.ast,
            &tokens,
            file_id,
        );

        let r = f(&mut p);
        tokens.clear();
        self.buffers.lexer_tokens = tokens;
        r
    }

    pub(super) fn parse_metaprogram_source(
        &mut self,
        file_id: FileId,
        kind: ParseAdHocKind,
    ) -> K1Result<ParseMetaprogramResult> {
        self.with_parser(file_id, move |p| {
            let msg_base = "Failed to parse the code you returned: ";
            let error_count_start = p.ast.errors.len();
            match kind {
                ParseAdHocKind::Expr => match p.expect_expression() {
                    Err(e) => Err(make_message(
                        &p.ast.idents,
                        format!("{msg_base}{e}"),
                        e.span(),
                        MessageLevel::Error,
                    )),
                    Ok(parsed_expr) => {
                        if p.ast.errors.len() > error_count_start {
                            let e = p.ast.errors.last().unwrap().clone();
                            Err(make_message(
                                &p.ast.idents,
                                format!("{msg_base}{e}"),
                                e.span(),
                                MessageLevel::Error,
                            ))
                        } else {
                            Ok(ParseMetaprogramResult::Expr(parsed_expr))
                        }
                    }
                },
                ParseAdHocKind::Definitions => {
                    let defns = p.parse_definitions(TokenKind::Eof);
                    if p.ast.errors.len() > error_count_start {
                        let e = p.ast.errors.last().unwrap().clone();
                        Err(make_message(
                            &p.ast.idents,
                            format!("{msg_base}{e}"),
                            e.span(),
                            MessageLevel::Error,
                        ))
                    } else {
                        Ok(ParseMetaprogramResult::Definitions(defns.to_mslice()))
                    }
                }
            }
        })
    }

    pub fn get_dylib_handle(
        &mut self,
        function_id: FunctionId,
        lib_name_ident: StringId,
        span: SpanId,
    ) -> K1Result<*mut std::ffi::c_void> {
        let Linkage::External { module_id, .. } = self.functions.get(function_id).linkage else {
            ice_span!(self, span, "get_dylib_handle called for non-external function");
        };
        if let Some(handle) = self.vm_dylib_handles.get(&(module_id, lib_name_ident)) {
            return Ok(*handle);
        }

        let lib_name_str = self.ast.idents.get_string(lib_name_ident);
        let ext = self.config.host_platform().dylib_ext();
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
