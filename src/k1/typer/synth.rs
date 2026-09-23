// Copyright (c) 2026 knix
// All rights reserved.

use crate::{kbail, kerr};

/// `synth`, synthesis, aka spitting out typed code, used for features that desugar, as well
/// as some general lowering
use super::*;

impl TypedProgram {
    pub(super) fn synth_bool(&mut self, value: bool, span: SpanId) -> TypedExprId {
        let value_id = self.static_values.add(StaticValue::Bool(value));
        let expr_id = self.add_static_constant_expr(value_id, span);
        expr_id
    }

    pub(super) fn synth_empty_value(&mut self, span: SpanId) -> TypedExprId {
        let value_id = self.static_values.empty_id();
        self.add_static_constant_expr(value_id, span)
    }

    pub(super) fn synth_equals_call_simple(
        &mut self,
        lhs: TypedExprId,
        rhs: TypedExprId,
        span: SpanId,
    ) -> TypedExprId {
        let ty = self.exprs.get_type(lhs);
        let rhs_type = self.exprs.get_type(rhs);
        debug_assert_eq!(ty, rhs_type);
        let equals_name = self.ast.idents.f.equals__equals.name;
        let Some(equals_function_id) = self.direct_ability_fn(ty, ABILITY_ID_EQUALS, equals_name)
        else {
            self.ice_span(span, "expected equals impl")
        };
        self.synth_static_call(equals_function_id, &[lhs, rhs], BOOL_TYPE_ID, span)
    }

    pub(super) fn apply_self_adjust(
        &mut self,
        expr: TypedExprId,
        self_adjust: SelfAdjust,
        span: SpanId,
    ) -> K1Result<TypedExprId> {
        match self_adjust {
            SelfAdjust::None => Ok(expr),
            SelfAdjust::Deref => Ok(self.synth_dereference(expr)),
            SelfAdjust::AddrOf => self.synth_address_of(expr, span, true),
        }
    }

    pub(super) fn synth_ability_call(
        &mut self,
        impl_handle: AbilityImplHandle,
        fn_name: QIdent,
        fn_type_args: &[TypeId],
        args: &[TypedExprId],
        ctx: EvalExprContext,
        span: SpanId,
    ) -> K1Result<TypedExprId> {
        let ability_fn = self
            .abilities
            .get(impl_handle.base_ability_id)
            .find_function_by_name(&self.mem, fn_name.name)
            .unwrap();
        let impl_fn = *self
            .ability_impls
            .get(impl_handle.full_impl_id)
            .function_at_index(&self.mem, ability_fn.index);
        let (callee, type_args) = match impl_fn {
            AbilityImplFunction::FunctionId(function_id) => {
                let signature = self.get_function(function_id).signature();
                if !signature.has_type_params() {
                    (Callee::StaticFunction(function_id), TypeArgs::empty())
                } else if !ctx.is_inference()
                    && signature.fnlike_type_params.is_empty()
                    && fn_type_args.len() == signature.type_params.len() as usize
                {
                    let type_args = TypeArgs::from_slice_in(fn_type_args, &mut self.mem);
                    let specialized = match self.find_function_specialization(
                        function_id,
                        type_args,
                        TypeArgs::empty(),
                    ) {
                        Some(specialized) => specialized,
                        None => self.specialize_function_declaration(
                            type_args,
                            TypeArgs::empty(),
                            function_id,
                        ),
                    };
                    (Callee::StaticFunction(specialized), type_args)
                } else {
                    return self.synth_typed_call_typed_args(
                        fn_name.with_span(span),
                        fn_type_args,
                        args,
                        ctx,
                        false,
                    );
                }
            }
            AbilityImplFunction::Abstract(function_sig) if !function_sig.has_type_params() => {
                (Callee::Abstract { function_sig }, TypeArgs::empty())
            }
            AbilityImplFunction::Abstract(_) | AbilityImplFunction::Unavailable => {
                return self.synth_typed_call_typed_args(
                    fn_name.with_span(span),
                    fn_type_args,
                    args,
                    ctx,
                    false,
                );
            }
        };
        let function_type = self.get_callee_function_type(&callee);
        let params = self.types.get(function_type).as_function().unwrap().logical_params();
        if params.len() as usize != args.len() {
            self.ice_span(span, "synthesized ability call has the wrong number of arguments")
        }
        let mut checked_args: SV4<TypedExprId> = smallvec![];
        for (index, param) in self.mem.getn(params).iter().enumerate() {
            let arg = args[index];
            if self.exprs.get_type(arg) == NEVER_TYPE_ID {
                return Ok(self.make_never_block(&args[..=index], ctx.scope_id, span));
            }
            checked_args.push(self.check_and_coerce_expr(
                param.type_id,
                arg,
                ctx.scope_id,
                false,
            )?);
        }
        let args = self.mem.pushn(&checked_args);
        let return_type = self.types.get(function_type).as_function().unwrap().return_type;
        self.finish_call(Call { callee, args, type_args, return_type, span }, ctx)
    }

    pub(super) fn synth_negated(
        &mut self,
        expr: TypedExprId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> K1Result<TypedExprId> {
        let (neg_impl, _) =
            self.expect_ability_impl(BOOL_TYPE_ID, ABILITY_ID_NEG, false, ctx.scope_id, span)?;
        self.synth_ability_call(neg_impl, self.ast.idents.f.neg__negated, &[], &[expr], ctx, span)
    }

    pub(super) fn synth_static_call(
        &mut self,
        function_id: FunctionId,
        args: &[TypedExprId],
        return_type: TypeId,
        span: SpanId,
    ) -> TypedExprId {
        let call_id = self.calls.add(Call {
            callee: Callee::StaticFunction(function_id),
            args: self.mem.pushn(args),
            type_args: TypeArgs::empty(),
            return_type,
            span,
        });
        self.exprs.add(TypedExpr::Call { call_id }, return_type, span)
    }

    pub(super) fn synth_if_else(
        &mut self,
        result_type: TypeId,
        condition: TypedExprId,
        consequent: TypedExprId,
        alternate: TypedExprId,
        span: SpanId,
    ) -> TypedExprId {
        let cons_arm = TypedMatchArm {
            case: None,
            condition: MatchingCondition {
                instrs: self.mem.pushn(&[MatchingConditionInstr::cond(condition)]),
            },
            consequent_expr: consequent,
        };
        let alt_arm = TypedMatchArm {
            case: None,
            condition: MatchingCondition { instrs: MSlice::empty() },
            consequent_expr: alternate,
        };
        self.exprs.add(
            TypedExpr::Match(TypedMatchExpr {
                subject_defn: None,
                scrutinee: None,
                arms: self.mem.pushn(&[cons_arm, alt_arm]),
            }),
            result_type,
            span,
        )
    }

    pub(super) fn synth_cast(
        &mut self,
        expr: TypedExprId,
        target_type: TypeId,
        cast_type: CastType,
        span: Option<SpanId>,
    ) -> TypedExprId {
        let span = span.unwrap_or_else(|| self.exprs.get_span(expr));
        self.exprs.add(TypedExpr::Cast(TypedCast { cast_type, base_expr: expr }), target_type, span)
    }

    pub(super) fn synth_optional_type(&mut self, inner_type: TypeId) -> TypeId {
        self.instantiate_generic_type(self.builtin_types.opt(), &[inner_type])
    }

    pub(super) fn synth_optional_some(&mut self, expr_id: TypedExprId) -> (TypedExprId, TypeId) {
        let span = self.exprs.get_span(expr_id);
        let inner_type = self.exprs.get_type(expr_id);
        let optional_type = self.synth_optional_type(inner_type);

        let some_expr = self.exprs.add(
            TypedExpr::SumConstructor(TypedSumConstructor {
                variant_index: 1,
                payload: Some(expr_id),
            }),
            optional_type,
            span,
        );
        (some_expr, optional_type)
    }

    pub(super) fn synth_optional_none(&mut self, type_id: TypeId, span: SpanId) -> TypedExprId {
        let optional_type = self.synth_optional_type(type_id);
        let none_expr = self.exprs.add(
            TypedExpr::SumConstructor(TypedSumConstructor { variant_index: 0, payload: None }),
            optional_type,
            span,
        );
        none_expr
    }

    pub(super) fn synth_dereference(&mut self, base: TypedExprId) -> TypedExprId {
        let type_id = self.get_expr_type(base).expect_reference().inner_type;
        self.exprs.add(TypedExpr::Deref(DerefExpr { target: base }), type_id, SpanId::NONE)
    }

    pub(super) fn synth_dereference_when(
        &mut self,
        base: TypedExprId,
        is_reference: bool,
    ) -> TypedExprId {
        if is_reference { self.synth_dereference(base) } else { base }
    }

    pub(super) fn synth_enum_get_value(
        &mut self,
        enum_expr: TypedExprId,
        span: SpanId,
    ) -> TypedExprId {
        let Type::Enum(e) = self.types.get(self.exprs.get_type(enum_expr)) else {
            self.ice_span(span, "need enum")
        };
        let int_type = e.int_type;
        self.exprs.add(
            TypedExpr::EnumGetValue(EnumGetValue { enum_expr }),
            int_type.type_id(),
            span,
        )
    }

    pub(super) fn synth_sum_get_tag(&mut self, sum_expr: TypedExprId, span: SpanId) -> TypedExprId {
        let Type::Sum(sum) = self.types.get(self.exprs.get_type(sum_expr)) else {
            self.ice_span(span, "need sum")
        };
        let int_type = sum.tag_type;
        self.exprs.add(TypedExpr::SumGetTag(GetSumTag { sum_expr }), int_type.type_id(), span)
    }

    pub(super) fn new_block_builder(
        &mut self,
        parent_scope: ScopeId,
        scope_type: ScopeType,
        span: SpanId,
        max_stmt_len: u32,
    ) -> BlockBuilder {
        let block_scope_id =
            self.scopes.add_child_scope(parent_scope, scope_type, ScopeOwnerId::None);
        BlockBuilder { statements: self.mem.new_list(max_stmt_len), scope_id: block_scope_id, span }
    }

    /// Sometimes a sub-expression of a larger construct has type `never`, for example a function argument
    /// In that case, we'd rather generate code that just runs each expression up to the NEVER one,
    /// than a function call that can never actually be a function call! Also true for while loop
    /// conditions.
    ///
    /// We use this function to generate such 'never' blocks from a series of expressions
    pub(super) fn make_never_block(
        &mut self,
        exprs: &[TypedExprId],
        scope_id: ScopeId,
        span: SpanId,
    ) -> TypedExprId {
        let mut b =
            BlockBuilder { statements: self.mem.new_list(exprs.len() as u32), scope_id, span };
        for e in exprs {
            let e_type_id = self.exprs.get_type(*e);
            self.push_block_stmt(&mut b, TypedStmt::Expr(*e, e_type_id));
        }
        self.exprs.add_block(b, NEVER_TYPE_ID)
    }

    pub(super) fn make_never_condition_block(
        &mut self,
        instrs: &[MatchingConditionInstr],
        scope_id: ScopeId,
        span: SpanId,
    ) -> TypedExprId {
        let mut b =
            BlockBuilder { statements: self.mem.new_list(instrs.len() as u32), scope_id, span };
        for i in instrs {
            match i {
                MatchingConditionInstr::Binding { let_stmt } => b.statements.push(*let_stmt),
                MatchingConditionInstr::Cond { value } => {
                    self.push_block_expr_id(&mut b, *value);
                }
                MatchingConditionInstr::IntEquals { subject, .. } => {
                    self.push_block_expr_id(&mut b, *subject);
                }
            }
        }
        self.exprs.add_block(b, NEVER_TYPE_ID)
    }

    /// Creates a non-mutable, mangled, non-referencing variable defn.
    /// This is the vastly most common case
    pub(super) fn synth_variable_defn_simple(
        &mut self,
        name: StringId,
        initializer: TypedExprId,
        owner_scope: ScopeId,
    ) -> SynthedVariable {
        self.synth_variable_defn(name, initializer, false, owner_scope, None)
    }

    /// Creates a user-code-visible variable
    pub(super) fn synth_variable_defn_visible(
        &mut self,
        name: StringId,
        initializer: TypedExprId,
        owner_scope: ScopeId,
        span: SpanId,
    ) -> SynthedVariable {
        self.synth_variable_defn(name, initializer, true, owner_scope, Some(span))
    }

    /// no_mangle: Skip mangling if we want the variable to be accessible from user code
    pub(super) fn synth_variable_defn(
        &mut self,
        name: StringId,
        initializer_id: TypedExprId,
        user_visible: bool,
        owner_scope: ScopeId,
        span: Option<SpanId>,
    ) -> SynthedVariable {
        let initializer_type = self.exprs.get_type(initializer_id);
        let span = match span {
            None => self.exprs.get_span(initializer_id),
            Some(span) => span,
        };
        let type_id = initializer_type;
        let mut flags = VariableFlags::empty();
        flags.set(VariableFlags::UserHidden, !user_visible);
        // let reassignable = true;
        // flags.set(VariableFlags::Reassigned, reassignable);
        // We used to generate a unique name, in case say a nested list literal's synthed var
        // shadowed another's. But we don't resolve these by name; rather we synth the expr by
        // variable id anyway. So there is no need for a unique name, even for hidden, since we
        // no longer put a hidden variable into the scope either. This might hurt debugging, a bit,
        // but, scopes are for visibility. So I think it is good.
        let variable = Variable {
            name,
            owner_scope,
            type_id,
            kind: VariableKind::StackSynthetic,
            flags,
            usage_count: 0,
            defn_span: span,
        };
        let variable_id = self.variables.add(variable);
        if !flags.contains(VariableFlags::UserHidden) {
            self.emit_ls_entity(span, LsEntityKind::Variable { variable_id })
        }
        let variable_expr =
            self.exprs.add(TypedExpr::Variable(VariableExpr { variable_id }), type_id, span);
        let defn_stmt = self.stmts.add(TypedStmt::Let(LetStmt {
            variable_id,
            variable_type: type_id,
            initializer: Some(initializer_id),
            span,
        }));
        if user_visible {
            self.scopes.add_variable(owner_scope, name, variable_id);
        }
        SynthedVariable { variable_id, defn_stmt, variable_expr }
    }

    pub(super) fn synth_typed_call_typed_args(
        &mut self,
        name: QIdent,
        type_args: &[TypeId],
        args: &[TypedExprId],
        ctx: EvalExprContext,
        is_method: bool,
    ) -> K1Result<TypedExprId> {
        let call = ParsedCall::without_type_args(name, MSlice::empty(), is_method);
        let known_callee = if is_method { None } else { self.core_fn_callee(&name) };
        self.eval_function_call(&call, name.name_span, Some((type_args, args)), ctx, known_callee)
    }

    fn core_fn_callee(&mut self, name: &QIdent) -> Option<Callee> {
        let key = (name.path_handle(), name.name);
        if let Some(function_id) = self.core_fns_by_name.get(&key) {
            return Some(Callee::StaticFunction(*function_id));
        }
        let function_id =
            self.find_function_namespaced(self.scopes.core_scope_id, name).ok().flatten()?;
        let function = self.get_function(function_id);
        if function.kind.ability_id().is_some() || function.is_macro() {
            return None;
        }
        self.core_fns_by_name.insert(key, function_id);
        Some(Callee::StaticFunction(function_id))
    }

    pub(super) fn synth_printto_call(
        &mut self,
        to_print: TypedExprId,
        writer: TypedExprId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let span = self.exprs.get_span(to_print);
        let writer_type_id = self.exprs.get_type(writer);
        let to_print_type = self.exprs.get_type(to_print);
        let (print_impl, self_adjust) =
            self.expect_ability_impl(to_print_type, ABILITY_ID_PRINT, true, ctx.scope_id, span)?;
        let to_print = self.apply_self_adjust(to_print, self_adjust, span)?;
        self.synth_ability_call(
            print_impl,
            self.ast.idents.f.core_print_print_to,
            &[writer_type_id],
            &[to_print, writer],
            ctx.with_no_expected_type(),
            span,
        )
    }

    pub(super) fn synth_code_append_call(
        &mut self,
        code_expr: TypedExprId,
        writer: TypedExprId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let span = self.exprs.get_span(code_expr);
        self.synth_typed_call_typed_args(
            self.ast.idents.f.CodeBuilder_code.with_span(span),
            &[],
            &[writer, code_expr],
            ctx.with_no_expected_type(),
            false,
        )
    }

    pub(super) fn synth_string_literal(
        &mut self,
        string_id: StringId,
        span: SpanId,
    ) -> TypedExprId {
        let string_value = self.static_values.add_string(string_id);
        self.add_static_constant_expr(string_value, span)
    }

    /// synth a block that just returns `expr`
    pub(super) fn synth_return_only_block(
        &mut self,
        scope_id: ScopeId,
        expr: TypedExprId,
        span: SpanId,
    ) -> TypedExprId {
        let return_expr = self.exprs.add_return(expr, None, span);
        let mut block_builder = BlockBuilder { statements: self.mem.new_list(1), scope_id, span };
        self.push_block_stmt(&mut block_builder, TypedStmt::Expr(return_expr, NEVER_TYPE_ID));
        self.exprs.add_block(block_builder, NEVER_TYPE_ID)
    }

    pub(super) fn synth_string_literal_from_str(
        &mut self,
        s: impl AsRef<str>,
        span: SpanId,
    ) -> TypedExprId {
        let string_id = self.ast.idents.intern(s);
        self.synth_string_literal(string_id, span)
    }

    pub(super) fn synth_int(&mut self, int_value: TypedIntValue, span: SpanId) -> TypedExprId {
        let int_value_id = self.static_values.add_int(int_value);
        self.add_static_constant_expr(int_value_id, span)
    }

    pub(super) fn synth_type_id_literal(&mut self, type_id: TypeId, span: SpanId) -> TypedExprId {
        let type_id_struct_value_id = self.add_type_id_value(type_id);
        self.add_static_constant_expr(type_id_struct_value_id, span)
    }

    pub(super) fn synth_i64(&mut self, value: i64, span: SpanId) -> TypedExprId {
        self.synth_int(TypedIntValue::I64(value), span)
    }

    pub(super) fn synth_source_location(&mut self, span: SpanId) -> TypedExprId {
        let span = self.remap_to_source_span(span);
        let the_span = self.ast.spans.get(span);
        let source = self.ast.sources.get(the_span.file_id);
        let line_number =
            source.get_line_for_span_start(&self.ast.mem, the_span).unwrap().line_number();
        let filename_string_id = source.filename(&self.ast.idents);
        let filename_value = self.static_values.add_string(filename_string_id);
        let line_value = self.static_values.add_int(TypedIntValue::U32(line_number));
        let span_value = self.static_values.add_int(TypedIntValue::U32(span.as_u32()));
        let source_location_type_id = self.builtin_types.source_location.unwrap();
        let value = self.static_values.add_struct_from_slice(
            source_location_type_id,
            &[filename_value, line_value, span_value],
        );
        self.add_static_constant_expr(value, span)
    }

    pub(super) fn synth_module_dir(&mut self, span: SpanId) -> TypedExprId {
        let module_id = self.module_of_span(span);
        let dir_value = self.static_values.add_string(self.modules.get(module_id).home_dir);
        self.add_static_constant_expr(dir_value, span)
    }

    pub(super) fn synth_discard_then(
        &mut self,
        discarded: TypedExprId,
        result: TypedExprId,
        scope_id: ScopeId,
    ) -> TypedExprId {
        let span = self.exprs.get_span(result);
        let mut b = BlockBuilder { statements: self.mem.new_list(2), scope_id, span };
        self.push_block_expr_id(&mut b, discarded);
        self.push_block_expr_id(&mut b, result);
        self.exprs.add_block(b, self.exprs.get_type(result))
    }

    pub(super) fn synth_discard_call(
        &mut self,
        value: TypedExprId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let span = self.exprs.get_span(value);
        self.synth_typed_call_typed_args(
            self.ast.idents.f.core_discard.with_span(span),
            &[],
            &[value],
            ctx,
            false,
        )
    }

    /// Used when we skip static execution, but still need to typecheck the rest of the
    /// body; this expression should never be executed; it should either be a call to
    /// crash, but transmuted to the expected type, or a special Unreachable node
    pub(super) fn synth_phony(&mut self, type_id: TypeId, span: SpanId) -> TypedExprId {
        let type_args = TypeArgs::one(type_id);
        let phony_fn_id =
            self.scopes.find_function(self.scopes.core_scope_id, self.ast.idents.b.phony).unwrap();
        let specialized_phony_fn_id =
            self.specialize_function_declaration(type_args, TypeArgs::empty(), phony_fn_id);
        let call = Call {
            callee: Callee::StaticFunction(specialized_phony_fn_id),
            args: MSlice::empty(),
            type_args,
            return_type: type_id,
            span,
        };
        let call_id = self.calls.add(call);
        self.exprs.add(TypedExpr::Call { call_id }, type_id, span)
    }

    pub(super) fn synth_phony_expected_type(
        &mut self,
        expected_type: Option<TypeId>,
        span: SpanId,
    ) -> TypedExprId {
        self.synth_phony(expected_type.unwrap_or(self.builtin_types.empty), span)
    }

    pub(super) fn synth_field_access(
        &mut self,
        struct_expr: TypedExprId,
        field_index: usize,
        span: SpanId,
    ) -> TypedExprId {
        let struct_type_id = self.exprs.get_type(struct_expr);
        let Type::Struct(s) = self.types.get(struct_type_id) else {
            self.ice_span(span, "bad struct field access: base is not a struct");
        };
        let packed = self.is_field_access_packed(struct_expr, s.record_kind);
        let field = self.get_struct_field(struct_type_id, field_index);
        let expr_id = self.exprs.add(
            TypedExpr::StructFieldAccess(FieldAccess {
                base_struct: struct_expr,
                field_index: field_index as u32,
                packed,
            }),
            field.type_id,
            span,
        );
        expr_id
    }

    pub(super) fn synth_sum_is_variant(
        &mut self,
        sum_expr: TypedExprId,
        variant_index: u32,
        span: Option<SpanId>,
    ) -> K1Result<TypedExprId> {
        let sum_type = self.types.get(self.exprs.get_type(sum_expr)).expect_sum();
        let tag_type = sum_type.tag_type;
        let variant_tag = self.sum_variant_by_index(sum_type.variants, variant_index).tag_value;
        let span = span.unwrap_or(self.exprs.get_span(sum_expr));
        let get_tag =
            self.exprs.add(TypedExpr::SumGetTag(GetSumTag { sum_expr }), tag_type.type_id(), span);
        let variant_tag_expr = self.synth_int(variant_tag, span);
        let tag_equals = self.synth_equals_call_simple(get_tag, variant_tag_expr, span);
        Ok(tag_equals)
    }

    /// Produces a series of printTo calls to the given writer,
    /// inside a fresh block, of type 'empty'
    pub(super) fn synth_format_calls(
        &mut self,
        writer_expr: TypedExprId,
        parts: MSlice<InterpolatedStringPart, ParsedProgram>,
        args_expr: TypedExprId,
        span: SpanId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let code_mode = match self.types.get(self.exprs.get_type(writer_expr)) {
            Type::Reference(r) => Some(r.inner_type) == self.builtin_types.code_builder,
            _ => false,
        };
        let mut block =
            self.new_block_builder(ctx.scope_id, ScopeType::LexicalBlock, span, parts.len() + 1);
        let block_scope = block.scope_id;
        let args_variable =
            self.synth_variable_defn_simple(self.ast.idents.b.fmtargs, args_expr, block_scope);
        self.push_block_stmt_id(&mut block, args_variable.defn_stmt);
        let block_ctx = ctx.with_scope(block_scope).with_no_expected_type();
        let mut hole_index = 0;
        fn get_named_arg(
            k1: &mut TypedProgram,
            args: TypedExprId,
            name: StringId,
            span: SpanId,
        ) -> Option<TypedExprId> {
            let type_id = k1.exprs.get_type(args);
            match k1.types.get(type_id) {
                Type::Struct(_) => {
                    if type_id == k1.builtin_types.string() {
                        return None;
                    }

                    let (field_index, _field) = k1.get_struct_field_by_name(type_id, name)?;
                    let field_expr = k1.synth_field_access(args, field_index, span);
                    Some(field_expr)
                }
                _ => None,
            }
        }
        fn get_nth_arg(
            k1: &mut TypedProgram,
            args: TypedExprId,
            n: usize,
            span: SpanId,
        ) -> K1Result<TypedExprId> {
            let type_id = k1.exprs.get_type(args);
            match k1.types.get(type_id) {
                Type::Char
                | Type::Bool
                | Type::Pointer
                | Type::Integer(_)
                | Type::Float(_)
                | Type::Reference(_)
                | Type::Array(_)
                | Type::Vector(_)
                | Type::Sum(_) => Ok(args),
                Type::Struct(s) => {
                    if type_id == k1.builtin_types.string() {
                        if n == 0 {
                            Ok(args)
                        } else {
                            Err(kerr!(
                                k1,
                                span,
                                "this hole asks for field {} on a string, which provides just 1 value",
                                n + 1
                            ))
                        }
                    } else {
                        if n >= s.fields.len() as usize {
                            kbail!(
                                k1,
                                span,
                                "Format args struct only has {} fields, but this hole asks for field {}",
                                s.fields.len(),
                                n + 1
                            );
                        }
                        let field_expr = k1.synth_field_access(args, n, span);
                        Ok(field_expr)
                    }
                }
                _ => Err(kerr!(k1, span, "Not formattable currently: {}", type_id)),
            }
        }

        let mut i = 0usize;
        while i < parts.len() as usize {
            match self.ast.mem.get_nth(parts, i) {
                parse::InterpolatedStringPart::String { string_id, span: part_span } => {
                    if code_mode {
                        // Per-part constants, so each keeps its own source span
                        let string_id = *string_id;
                        let part_span = *part_span;
                        if !self.ast.idents.get_string(string_id).is_empty() {
                            let value_id = self.make_static_code_value(&[(string_id, part_span)]);
                            let code_expr = self.add_static_constant_expr(value_id, part_span);
                            let code_call = self.synth_code_append_call(
                                code_expr,
                                writer_expr,
                                block_ctx.with_hidden_calls(true),
                            )?;
                            self.push_block_expr_id(&mut block, code_call);
                        }
                    } else {
                        // Combine consecutive strings into a single constant
                        let mut string_to_print = *string_id;
                        let mut combined: Option<String> = None;
                        while i + 1 < parts.len() as usize {
                            let next = self.ast.mem.get_nth(parts, i + 1);
                            if let InterpolatedStringPart::String {
                                string_id: next_string, ..
                            } = next
                            {
                                let buf = combined.get_or_insert_with(|| {
                                    String::from(self.ast.idents.get_string(string_to_print))
                                });
                                buf.push_str(self.ast.idents.get_string(*next_string));
                                i += 1;
                            } else {
                                break;
                            }
                        }
                        if let Some(combined) = combined {
                            string_to_print = self.ast.idents.intern(combined);
                        }
                        if !self.ast.idents.get_string(string_to_print).is_empty() {
                            let string_expr = self.synth_string_literal(string_to_print, span);
                            let print_call = self.synth_printto_call(
                                string_expr,
                                writer_expr,
                                block_ctx.with_hidden_calls(true),
                            )?;
                            self.push_block_expr_id(&mut block, print_call);
                        }
                    }
                }
                parse::InterpolatedStringPart::Expr(expr_id, _fmt_settings) => {
                    let parsed_expr = self.ast.exprs.get(*expr_id);
                    let expr_span = self.ast.exprs.get_span(*expr_id);
                    let naked_variable_name = match parsed_expr {
                        ParsedExpr::Variable(ParsedVariable { name, .. }) if !name.has_path() => {
                            Some(name.name)
                        }
                        _ => None,
                    };
                    // Must be a naked variable expr, look for it in the struct, then in the scope
                    let typed_expr = match naked_variable_name {
                        Some(name) => {
                            let struct_arg =
                                get_named_arg(self, args_variable.variable_expr, name, expr_span);
                            match struct_arg {
                                Some(field_expr) => field_expr,
                                None => {
                                    self.eval_expr(*expr_id, block_ctx.with_hidden_calls(false))?
                                }
                            }
                        }
                        None => {
                            let typed_expr =
                                self.eval_expr(*expr_id, block_ctx.with_hidden_calls(false))?;
                            typed_expr
                        }
                    };

                    let part_call = if code_mode && self.expr_type_is_code(typed_expr) {
                        self.synth_code_append_call(
                            typed_expr,
                            writer_expr,
                            block_ctx.with_hidden_calls(true),
                        )?
                    } else {
                        self.synth_printto_call(
                            typed_expr,
                            writer_expr,
                            block_ctx.with_hidden_calls(true),
                        )?
                    };
                    self.push_block_expr_id(&mut block, part_call);
                }
                parse::InterpolatedStringPart::Hole { fmt_settings: _, span } => {
                    // Grab the hole_index'th argument from args_variable.
                    let arg = get_nth_arg(self, args_variable.variable_expr, hole_index, *span)?;
                    hole_index += 1;
                    let part_call = if code_mode && self.expr_type_is_code(arg) {
                        self.synth_code_append_call(arg, writer_expr, ctx)?
                    } else {
                        self.synth_printto_call(arg, writer_expr, ctx)?
                    };
                    self.push_block_expr_id(&mut block, part_call);
                }
            }
            i += 1
        }
        if block.statements.is_empty() {
            Ok(self.synth_empty_value(span))
        } else {
            Ok(self.exprs.add_block(block, EMPTY_TYPE_ID))
        }
    }

    /// Produces a string, resulting from using a core/string-builder
    /// to write the given interpolated string expr + arguments into it
    pub(super) fn synth_interpolated_string(
        &mut self,
        expr_id: ParsedExprId,
        ctx: EvalExprContext,
        args_expr: Option<TypedExprId>,
    ) -> K1Result<TypedExprId> {
        let span = self.ast.exprs.get_span(expr_id);
        let ParsedExpr::InterpolatedString(interpolated_string) = *self.ast.exprs.get(expr_id)
        else {
            panic!()
        };

        let part_count = interpolated_string.parts.len();
        if part_count == 1 {
            let parse::InterpolatedStringPart::String { string_id, span } =
                self.ast.mem.get_nth(interpolated_string.parts, 0)
            else {
                panic!()
            };
            let e = self.synth_string_literal(*string_id, *span);
            return Ok(e);
        }

        let mut block = self.new_block_builder(ctx.scope_id, ScopeType::LexicalBlock, span, 3);
        let block_scope = block.scope_id;
        let block_ctx = ctx.with_scope(block_scope).with_no_expected_type();
        let ctx_for_calls = block_ctx.with_hidden_calls(true);
        let new_string_builder = self.synth_typed_call_typed_args(
            self.ast.idents.f.StringBuilder_new.with_span(span),
            &[],
            &[],
            ctx_for_calls,
            false,
        )?;
        let string_builder_var = self.synth_variable_defn(
            self.ast.idents.b.builder,
            new_string_builder,
            false,
            block.scope_id,
            None,
        );
        let string_builder_expr =
            self.synth_address_of(string_builder_var.variable_expr, SpanId::NONE, true).unwrap();
        self.push_block_stmt_id(&mut block, string_builder_var.defn_stmt);
        let args_expr = args_expr.unwrap_or(self.synth_empty_value(span));
        let format_block = self.synth_format_calls(
            string_builder_expr,
            interpolated_string.parts,
            args_expr,
            span,
            ctx,
        )?;
        self.push_block_expr_id(&mut block, format_block);
        let build_call = self.synth_typed_call_typed_args(
            self.ast.idents.f.StringBuilder_build_tmp.with_span(span),
            &[],
            &[string_builder_expr],
            ctx_for_calls,
            false,
        )?;
        self.push_block_expr_id(&mut block, build_call);

        // build_call_type should definitely be string
        let build_call_type = self.exprs.get_type(build_call);
        Ok(self.exprs.add_block(block, build_call_type))
    }

    fn expr_type_is_code(&self, expr: TypedExprId) -> bool {
        let type_id = self.get_type_family_type(self.exprs.get_type(expr));
        Some(type_id) == self.builtin_types.code
    }

    pub(super) fn expected_type_is_code(&self, expected: Option<TypeId>) -> bool {
        let Some(expected) = expected else { return false };
        Some(self.get_type_family_type(expected)) == self.builtin_types.code
    }

    /// Produces a `code` value, resulting from using a core/code-builder
    /// to write the given format parts + arguments into it
    pub(super) fn synth_interpolated_code(
        &mut self,
        parts: MSlice<InterpolatedStringPart, ParsedProgram>,
        span: SpanId,
        ctx: EvalExprContext,
        args_expr: Option<TypedExprId>,
    ) -> K1Result<TypedExprId> {
        if parts.len() == 1 {
            if let parse::InterpolatedStringPart::String { string_id, span: part_span } =
                self.ast.mem.get_nth(parts, 0)
            {
                let string_id = *string_id;
                let part_span = *part_span;
                let value_id = self.make_static_code_value(&[(string_id, part_span)]);
                return Ok(self.add_static_constant_expr(value_id, span));
            }
        }

        let mut block = self.new_block_builder(ctx.scope_id, ScopeType::LexicalBlock, span, 3);
        let block_ctx = ctx.with_scope(block.scope_id).with_no_expected_type();
        let ctx_for_calls = block_ctx.with_hidden_calls(true);
        let new_code_builder = self.synth_typed_call_typed_args(
            self.ast.idents.f.CodeBuilder_new.with_span(span),
            &[],
            &[],
            ctx_for_calls,
            false,
        )?;
        let code_builder_var = self.synth_variable_defn(
            self.ast.idents.b.builder,
            new_code_builder,
            false,
            block.scope_id,
            None,
        );
        let code_builder_expr =
            self.synth_address_of(code_builder_var.variable_expr, SpanId::NONE, true).unwrap();
        self.push_block_stmt_id(&mut block, code_builder_var.defn_stmt);
        let args_expr = args_expr.unwrap_or(self.synth_empty_value(span));
        let format_block =
            self.synth_format_calls(code_builder_expr, parts, args_expr, span, ctx)?;
        self.push_block_expr_id(&mut block, format_block);
        let build_call = self.synth_typed_call_typed_args(
            self.ast.idents.f.CodeBuilder_build.with_span(span),
            &[],
            &[code_builder_expr],
            ctx_for_calls,
            false,
        )?;
        self.push_block_expr_id(&mut block, build_call);

        let build_call_type = self.exprs.get_type(build_call);
        Ok(self.exprs.add_block(block, build_call_type))
    }

    pub(crate) fn synth_variable_expr(
        &mut self,
        variable_id: VariableId,
        span: SpanId,
    ) -> TypedExprId {
        let type_id = self.variables.get(variable_id).type_id;
        let expr = self.exprs.add(TypedExpr::Variable(VariableExpr { variable_id }), type_id, span);
        self.register_variable_usage(variable_id, span);
        expr
    }
}

pub(super) fn synth_static_option(
    static_values: &mut StaticValuePool,
    option_type_id: TypeId,
    value_id: Option<StaticValueId>,
) -> StaticValueId {
    let static_enum = match value_id {
        None => StaticSum { sum_type_id: option_type_id, variant_index: 0, payload: None },
        Some(value_id) => {
            StaticSum { sum_type_id: option_type_id, variant_index: 1, payload: Some(value_id) }
        }
    };

    static_values.add(StaticValue::Sum(static_enum))
}
