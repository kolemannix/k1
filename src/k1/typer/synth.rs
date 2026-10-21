// Copyright (c) 2026 knix
// All rights reserved.

use crate::lex::{FmtAlign, FmtSpec};
use crate::{kbail, kerr};

/// `synth`, synthesis, aka spitting out typed code, used for features that desugar, as well
/// as some general lowering
use super::*;

impl TypedProgram {
    pub(super) fn synth_bool(&mut self, value: bool, span: Span) -> TypedExprId {
        let expr_id = self.add_static_constant_expr(StaticValuePool::get_bool_id(value), span);
        expr_id
    }

    pub(super) fn synth_empty_value(&mut self, span: Span) -> TypedExprId {
        let value_id = StaticValuePool::EMPTY;
        self.add_static_constant_expr(value_id, span)
    }

    pub(super) fn synth_equals_call_simple(
        &mut self,
        lhs: TypedExprId,
        rhs: TypedExprId,
        span: Span,
    ) -> TypedExprId {
        let ty = self.exprs.get_type(lhs);
        let rhs_type = self.exprs.get_type(rhs);
        debug_assert_eq!(ty, rhs_type);
        let equals_name = self.ast.idents.f.equals__equals.name;
        let Some((_, equals_function_id)) =
            self.direct_ability_fn(ty, ABILITY_ID_EQUALS, equals_name)
        else {
            self.ice_span(span, "expected equals impl")
        };
        self.synth_static_call(equals_function_id, &[lhs, rhs], BOOL_TYPE_ID, span)
    }

    pub(super) fn apply_self_adjust(
        &mut self,
        expr: TypedExprId,
        self_adjust: SelfAdjust,
        span: Span,
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
        span: Span,
    ) -> K1Result<TypedExprId> {
        let ability_fn = self
            .abilities
            .get(impl_handle.base_ability_id)
            .find_function_by_name(&self.mem, fn_name.name)
            .unwrap();
        let (callee, signature) = self.resolve_ability_impl_fn_callee(
            impl_handle.full_impl_id,
            ability_fn.index,
            ctx.scope_id,
            span,
        )?;
        let (callee, type_args) = match callee {
            _ if !signature.has_type_params() => (callee, EMPTY_TYPE_SLICE),
            Callee::StaticFunction(function_id)
                if !ctx.is_inference()
                    && signature.fnlike_type_params.is_empty()
                    && fn_type_args.len() == signature.type_params.len() as usize =>
            {
                let specialized =
                    self.specialize_function_declaration(fn_type_args, &[], function_id);
                let type_args =
                    self.get_function(specialized).specialization_info.unwrap().type_arguments;
                (Callee::StaticFunction(specialized), type_args)
            }
            _ => {
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
        self.finish_call(Call { callee, args, type_args }, Some(ability_fn.function_id), span, ctx)
    }

    pub(super) fn synth_test_thunk(
        &mut self,
        function_id: FunctionId,
        span: Span,
    ) -> K1Result<FunctionId> {
        let test = *self.get_function(function_id);
        let ns_scope = self.namespaces.get_scope(self.get_function_namespace(function_id));
        let fn_scope =
            self.scopes.add_child_scope(ns_scope, ScopeType::FunctionScope, ScopeOwnerId::None);
        let test_return_type = self.types.get(test.type_id).as_function().unwrap().return_type;
        let call = self.synth_static_call(function_id, &[], test_return_type, span);
        let failure_call = self.synth_typed_call_typed_args(
            self.ast.idents.f.test_outcome_failure.with_span(span),
            &[],
            &[call],
            EvalExprContext::make(fn_scope),
            false,
        )?;
        let body = self.synth_return_only_block(fn_scope, failure_call, span);
        let thunk_type = self.add_function_type(&[], self.exprs.get_type(failure_call), false);
        let thunk_name = self.ast.idents.intern("test-thunk");
        let thunk_id = self.add_function(TypedFunction::synthesized(
            thunk_name,
            fn_scope,
            MSlice::empty(),
            body,
            test.parsed_id,
            thunk_type,
            TypedFunctionKind::Standard,
        ));
        self.scopes.set_scope_owner_id(fn_scope, ScopeOwnerId::Function(thunk_id));
        Ok(thunk_id)
    }

    pub(super) fn synth_container_literal(
        &mut self,
        list_kind: ContainerKind,
        element_type: TypeId,
        elements: &[TypedExprId],
        ctx: EvalExprContext,
        span: Span,
    ) -> K1Result<TypedExprId> {
        let mut list_lit_block = self.new_block_builder(
            ctx.scope_id,
            ScopeType::LexicalBlock,
            span,
            2 + elements.len() as u32,
        );
        let list_lit_scope = list_lit_block.scope_id;
        let list_lit_ctx = ctx.with_scope(list_lit_scope).with_no_expected_type();
        let count_expr = self.synth_i64(elements.len() as i64, span);

        let make_dest_coll = match list_kind {
            ContainerKind::List => self.synth_typed_call_typed_args(
                self.ast.idents.f.list_with_capacity.with_span(span),
                &[element_type],
                &[count_expr],
                list_lit_ctx,
                false,
            )?,
            ContainerKind::Buffer | ContainerKind::Span => self.synth_typed_call_typed_args(
                self.ast.idents.f.buffer_allocate.with_span(span),
                &[element_type],
                &[count_expr],
                list_lit_ctx,
                false,
            )?,
            ContainerKind::Array(array_type_id) => {
                let value_id = self.static_values.add(StaticValue::Zero(array_type_id));
                self.exprs.add_static(value_id, array_type_id, false, span)
            }
        };
        let needs_address_of = matches!(list_kind, ContainerKind::List);
        let dest_coll_variable = self.synth_variable_defn(
            self.ast.idents.b.list_lit,
            make_dest_coll,
            false,
            list_lit_scope,
            None,
        );
        let dest_coll_ref = self.synth_variable_ref(dest_coll_variable.variable_id);
        let dest_coll_expr = if needs_address_of {
            self.synth_address_of(dest_coll_ref, Span::NONE, true).unwrap()
        } else {
            dest_coll_ref
        };

        list_lit_block.statements.push(dest_coll_variable.defn_stmt);
        for (index, element_value_expr) in elements.iter().enumerate() {
            let index_expr = self.synth_i64(index as i64, span);
            let element_place = match list_kind {
                ContainerKind::List => {
                    let push_call = self.synth_typed_call_typed_args(
                        self.ast.idents.f.list_push.with_span(span),
                        &[element_type],
                        &[dest_coll_expr, *element_value_expr],
                        list_lit_ctx,
                        false,
                    )?;
                    let type_id = self.exprs.get_type(push_call);
                    let push_stmt = self.stmts.add(TypedStmt::Expr(push_call, type_id));
                    self.push_block_stmt_id(&mut list_lit_block, push_stmt);
                    continue;
                }
                ContainerKind::Buffer | ContainerKind::Span => {
                    let element_ref = self.synth_typed_call_typed_args(
                        self.ast.idents.f.buffer_index_unchecked.with_span(span),
                        &[element_type],
                        &[dest_coll_expr, index_expr],
                        list_lit_ctx,
                        false,
                    )?;
                    self.exprs.add(
                        TypedExpr::Deref(DerefExpr { target: element_ref }),
                        element_type,
                        span,
                    )
                }
                ContainerKind::Array(_) => self.exprs.add(
                    TypedExpr::ArrayGetElement(ArrayGetElement {
                        base_array: dest_coll_expr,
                        index: index_expr,
                        packed: false,
                    }),
                    element_type,
                    span,
                ),
            };
            let store_stmt = self.add_assignment_stmt(element_place, *element_value_expr, span);
            self.push_block_stmt_id(&mut list_lit_block, store_stmt);
        }
        let dest_coll_final = self.synth_variable_ref(dest_coll_variable.variable_id);
        let final_expr = match list_kind {
            ContainerKind::List | ContainerKind::Buffer | ContainerKind::Array(_) => {
                dest_coll_final
            }
            ContainerKind::Span => self.synth_typed_call_typed_args(
                self.ast.idents.f.span_wrapBuffer.with_span(span),
                &[element_type],
                &[dest_coll_final],
                ctx.with_no_expected_type(),
                false,
            )?,
        };
        self.push_block_expr_id(&mut list_lit_block, final_expr);
        let final_expr_type = self.exprs.get_type(final_expr);
        Ok(self.exprs.add_block(list_lit_block, final_expr_type))
    }

    pub(super) fn synth_static_call(
        &mut self,
        function_id: FunctionId,
        args: &[TypedExprId],
        return_type: TypeId,
        span: Span,
    ) -> TypedExprId {
        let call = Call {
            callee: Callee::StaticFunction(function_id),
            args: self.mem.pushn(args),
            type_args: EMPTY_TYPE_SLICE,
        };
        self.add_call_expr(call, return_type, span)
    }

    pub(super) fn synth_if_else(
        &mut self,
        result_type: TypeId,
        condition: TypedExprId,
        consequent: TypedExprId,
        alternate: TypedExprId,
        span: Span,
    ) -> TypedExprId {
        let condition = MatchingCondition {
            instrs: self.mem.pushn(&[MatchingConditionInstr::cond(condition)]),
        };
        self.synth_matching_if_else(result_type, condition, consequent, alternate, span)
    }

    pub(super) fn synth_matching_if_else(
        &mut self,
        result_type: TypeId,
        condition: MatchingCondition,
        consequent: TypedExprId,
        alternate: TypedExprId,
        span: Span,
    ) -> TypedExprId {
        let cons_arm = TypedMatchArm { case: None, condition, consequent_expr: consequent };
        let alt_arm = TypedMatchArm {
            case: None,
            condition: MatchingCondition { instrs: MSlice::empty() },
            consequent_expr: alternate,
        };
        let arms = self.mem.pushn(&[cons_arm, alt_arm]);
        self.add_match_expr(
            TypedMatchExpr { subject_defn: None, scrutinee: None, arms },
            result_type,
            span,
        )
    }

    pub(super) fn synth_cast(
        &mut self,
        expr: TypedExprId,
        target_type: TypeId,
        cast_type: CastType,
        span: Option<Span>,
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

    pub(super) fn synth_optional_none(&mut self, type_id: TypeId, span: Span) -> TypedExprId {
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
        self.exprs.add(TypedExpr::Deref(DerefExpr { target: base }), type_id, Span::NONE)
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
        span: Span,
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

    pub(super) fn synth_sum_get_tag(&mut self, sum_expr: TypedExprId, span: Span) -> TypedExprId {
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
        span: Span,
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
        span: Span,
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
        span: Span,
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
        span: Span,
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
        span: Option<Span>,
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
        let defn_stmt = self.stmts.add(TypedStmt::Let(LetStmt {
            variable_id,
            variable_type: type_id,
            initializer: Some(initializer_id),
            span,
        }));
        if user_visible {
            self.scopes.add_variable(owner_scope, name, variable_id);
        }
        SynthedVariable { variable_id, defn_stmt }
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
        let resolution =
            if is_method { None } else { self.core_fn_callee(&name).map(CallResolution::Call) };
        self.eval_function_call(&call, name.name_span, Some((type_args, args)), ctx, resolution)
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

    pub(super) fn synth_print_call(
        &mut self,
        to_print: TypedExprId,
        writer: TypedExprId,
        precision: Option<u32>,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let span = self.exprs.get_span(to_print);
        let writer_type_id = self.exprs.get_type(writer);
        let to_print_type = self.get_type_family_type(self.exprs.get_type(to_print));
        let (ability_id, fn_name) = match precision {
            None => (ABILITY_ID_PRINT, self.ast.idents.f.core_print_print_to),
            Some(_) => {
                (ABILITY_ID_PRINT_PRECISE, self.ast.idents.f.core_print_precise_print_precise_to)
            }
        };
        let (print_impl, self_adjust) =
            self.expect_ability_impl(to_print_type, ability_id, true, ctx.scope_id, span)?;
        let to_print = self.apply_self_adjust(to_print, self_adjust, span)?;
        let precision_arg = precision.map(|p| self.synth_int(TypedIntValue::U32(p), span));
        let args: &[TypedExprId] = match &precision_arg {
            None => &[to_print, writer],
            Some(precision_arg) => &[to_print, writer, *precision_arg],
        };
        self.synth_ability_call(
            print_impl,
            fn_name,
            &[writer_type_id],
            args,
            ctx.with_no_expected_type(),
            span,
        )
    }

    fn synth_format_part(
        &mut self,
        value: TypedExprId,
        writer: TypedExprId,
        spec: Option<FmtSpec>,
        code_mode: bool,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let span = self.exprs.get_span(value);
        if code_mode && self.expr_type_is_code(value) {
            if spec.is_some() {
                kbail!(self, span, "A format spec does not apply to a code value");
            }
            return self.synth_code_append_call(value, writer, ctx);
        }
        let Some(spec) = spec else { return self.synth_print_call(value, writer, None, ctx) };
        let is_number =
            matches!(self.types.get(self.exprs.get_type(value)), Type::Integer(_) | Type::Float(_));
        if spec.group && !is_number {
            kbail!(self, span, "The `,` format option groups digits; it needs an integer or float");
        }
        if spec.width == 0 && !spec.group {
            return self.synth_print_call(value, writer, spec.precision, ctx);
        }
        let mut block = self.new_block_builder(ctx.scope_id, ScopeType::LexicalBlock, span, 4);
        let block_ctx = ctx.with_scope(block.scope_id);
        let new_pad = self.synth_typed_call_typed_args(
            self.ast.idents.f.fmt_pad_new.with_span(span),
            &[],
            &[],
            block_ctx,
            false,
        )?;
        let pad_var =
            self.synth_variable_defn_simple(self.ast.idents.b.fmt, new_pad, block.scope_id);
        self.push_block_stmt_id(&mut block, pad_var.defn_stmt);
        let pad_ref = self.synth_variable_ref(pad_var.variable_id);
        let pad = self.synth_address_of(pad_ref, Span::NONE, true)?;
        let print_call = self.synth_print_call(value, pad, spec.precision, block_ctx)?;
        self.push_block_expr_id(&mut block, print_call);
        if spec.group {
            let group_call = self.synth_typed_call_typed_args(
                self.ast.idents.f.fmt_pad_group.with_span(span),
                &[],
                &[pad],
                block_ctx,
                false,
            )?;
            self.push_block_expr_id(&mut block, group_call);
        }

        let align = match spec.align {
            Some(align) => align,
            None if is_number => FmtAlign::Right,
            None => FmtAlign::Left,
        };
        let width = self.synth_int(TypedIntValue::U32(spec.width), span);
        let fill = self.synth_string_literal_from_str(spec.fill.to_string(), span);
        let f = &self.ast.idents.f;
        let (align_fn, args): (QIdent, &[TypedExprId]) = match align {
            FmtAlign::Left => (f.fmt_pad_align_left, &[pad, writer, fill, width]),
            FmtAlign::Right => (f.fmt_pad_align_right, &[pad, writer, fill, width]),
            FmtAlign::Center => (f.fmt_pad_align_center, &[pad, writer, fill, width]),
            FmtAlign::SignZeros => (f.fmt_pad_align_zeros, &[pad, writer, width]),
        };
        let align_call = self.synth_typed_call_typed_args(
            align_fn.with_span(span),
            &[],
            args,
            block_ctx,
            false,
        )?;
        self.push_block_expr_id(&mut block, align_call);
        Ok(self.exprs.add_block(block, EMPTY_TYPE_ID))
    }

    pub(super) fn synth_code_append_call(
        &mut self,
        code_expr: TypedExprId,
        writer: TypedExprId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let span = self.exprs.get_span(code_expr);
        self.synth_typed_call_typed_args(
            self.ast.idents.f.CodeBuilder_write_code.with_span(span),
            &[],
            &[writer, code_expr],
            ctx.with_no_expected_type(),
            false,
        )
    }

    pub(super) fn synth_string_literal(&mut self, string_id: StringId, span: Span) -> TypedExprId {
        let string_value = self.static_values.add_string(string_id);
        self.add_static_constant_expr(string_value, span)
    }

    /// synth a block that just returns `expr`
    pub(super) fn synth_return_only_block(
        &mut self,
        scope_id: ScopeId,
        expr: TypedExprId,
        span: Span,
    ) -> TypedExprId {
        let return_expr = self.exprs.add_return(expr, None, span);
        let mut block_builder = BlockBuilder { statements: self.mem.new_list(1), scope_id, span };
        self.push_block_stmt(&mut block_builder, TypedStmt::Expr(return_expr, NEVER_TYPE_ID));
        self.exprs.add_block(block_builder, NEVER_TYPE_ID)
    }

    pub(super) fn synth_string_literal_from_str(
        &mut self,
        s: impl AsRef<str>,
        span: Span,
    ) -> TypedExprId {
        let string_id = self.ast.idents.intern(s);
        self.synth_string_literal(string_id, span)
    }

    pub(super) fn synth_int(&mut self, int_value: TypedIntValue, span: Span) -> TypedExprId {
        let int_value_id = self.static_values.add_int(int_value);
        self.add_static_constant_expr(int_value_id, span)
    }

    pub(super) fn synth_type_id_literal(&mut self, type_id: TypeId, span: Span) -> TypedExprId {
        let type_id_struct_value_id = self.add_type_id_value(type_id);
        self.add_static_constant_expr(type_id_struct_value_id, span)
    }

    pub(super) fn synth_i64(&mut self, value: i64, span: Span) -> TypedExprId {
        self.synth_int(TypedIntValue::I64(value), span)
    }

    pub(super) fn synth_source_location(&mut self, span: Span) -> TypedExprId {
        let value = self.static_source_location(span);
        self.add_static_constant_expr(value, self.remap_span(span))
    }

    pub(super) fn static_source_location(&mut self, span: Span) -> StaticValueId {
        let span = self.remap_span(span);
        let source = self.ast.sources.source_by_span(span);
        let line_number =
            source.get_line_for_span_start(&self.ast.mem, span).unwrap().line_number();
        let filename_string_id = source.filename(&self.ast.idents);
        let filename_value = self.static_values.add_string(filename_string_id);
        let line_value = self.static_values.add_int(TypedIntValue::U32(line_number));
        let span_value = self.static_values.add_int(TypedIntValue::U64(span.encoded_k1_u64()));
        let source_location_type_id = self.builtin_types.source_location.unwrap();
        self.static_values.add_struct_from_slice(
            source_location_type_id,
            &[filename_value, line_value, span_value],
        )
    }

    pub(super) fn synth_module_dir(&mut self, scope_id: ScopeId, span: Span) -> TypedExprId {
        let module_id = self.get_namespace_module(self.scopes.nearest_parent_namespace(scope_id));
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

    pub(super) fn synth_discard(&mut self, value: TypedExprId) -> TypedExprId {
        self.synth_cast(value, self.builtin_types.empty, CastType::Discard, None)
    }

    /// Used when we skip static execution, but still need to typecheck the rest of the
    /// body; this expression should never be executed; it should either be a call to
    /// crash, but transmuted to the expected type, or a special Unreachable node
    pub(super) fn synth_phony(&mut self, type_id: TypeId, span: Span) -> TypedExprId {
        let phony_fn_id =
            self.scopes.find_function(self.scopes.core_scope_id, self.ast.idents.b.phony).unwrap();
        let specialized_phony_fn_id =
            self.specialize_function_declaration(&[type_id], &[], phony_fn_id);
        let call = Call {
            callee: Callee::StaticFunction(specialized_phony_fn_id),
            args: MSlice::empty(),
            type_args: self
                .get_function(specialized_phony_fn_id)
                .specialization_info
                .unwrap()
                .type_arguments,
        };
        let call_id = self.calls.add(call);
        self.exprs.add(TypedExpr::Call { call_id }, type_id, span)
    }

    pub(super) fn synth_phony_expected_type(
        &mut self,
        expected_type: Option<TypeId>,
        span: Span,
    ) -> TypedExprId {
        self.synth_phony(expected_type.unwrap_or(self.builtin_types.empty), span)
    }

    pub(super) fn synth_field_access(
        &mut self,
        struct_expr: TypedExprId,
        field_index: usize,
        span: Span,
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
        span: Span,
    ) -> MatchingCondition {
        let sum_type = self.types.get(self.exprs.get_type(sum_expr)).expect_sum();
        let variant_tag = self.sum_variant_by_index(sum_type.variants, variant_index).tag_value;
        let subject = self.synth_sum_get_tag(sum_expr, span);
        let value = self.static_values.add_int(variant_tag);
        MatchingCondition {
            instrs: self.mem.pushn(&[MatchingConditionInstr::IntEquals { subject, value }]),
        }
    }

    /// Produces a series of printTo calls to the given writer,
    /// inside a fresh block, of type 'empty'
    pub(super) fn synth_format_calls(
        &mut self,
        writer_expr: TypedExprId,
        parts: MSlice<InterpolatedStringPart, ParsedProgram>,
        args_expr: TypedExprId,
        span: Span,
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
            args: VariableId,
            name: StringId,
            span: Span,
        ) -> Option<TypedExprId> {
            let type_id = k1.variables.get(args).type_id;
            match k1.types.get(type_id) {
                Type::Struct(_) => {
                    if type_id == k1.builtin_types.string() {
                        return None;
                    }

                    let (field_index, _field) = k1.get_struct_field_by_name(type_id, name)?;
                    let args_ref = k1.synth_variable_ref(args);
                    let field_expr = k1.synth_field_access(args_ref, field_index, span);
                    Some(field_expr)
                }
                _ => None,
            }
        }
        fn get_nth_arg(
            k1: &mut TypedProgram,
            args: VariableId,
            n: usize,
            span: Span,
        ) -> K1Result<TypedExprId> {
            let type_id = k1.variables.get(args).type_id;
            match k1.types.get(type_id) {
                Type::Char
                | Type::Bool
                | Type::Pointer
                | Type::Integer(_)
                | Type::Float(_)
                | Type::Reference(_)
                | Type::Array(_)
                | Type::Vector(_)
                | Type::Sum(_) => Ok(k1.synth_variable_ref(args)),
                Type::Struct(s) => {
                    if type_id == k1.builtin_types.string() {
                        if n == 0 {
                            Ok(k1.synth_variable_ref(args))
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
                        let args_ref = k1.synth_variable_ref(args);
                        let field_expr = k1.synth_field_access(args_ref, n, span);
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
                            let text = self.synth_string_literal(string_id, part_span);
                            let source = self.synth_int(
                                TypedIntValue::U64(part_span.encoded_k1_u64()),
                                part_span,
                            );
                            let call = self.synth_typed_call_typed_args(
                                self.ast.idents.f.CodeBuilder_write_spanned.with_span(part_span),
                                &[],
                                &[writer_expr, text, source],
                                block_ctx.with_hidden_calls(true).with_no_expected_type(),
                                false,
                            )?;
                            self.push_block_expr_id(&mut block, call);
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
                            let print_call = self.synth_print_call(
                                string_expr,
                                writer_expr,
                                None,
                                block_ctx.with_hidden_calls(true),
                            )?;
                            self.push_block_expr_id(&mut block, print_call);
                        }
                    }
                }
                parse::InterpolatedStringPart::Expr(expr_id, spec) => {
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
                                get_named_arg(self, args_variable.variable_id, name, expr_span);
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

                    let part_call = self.synth_format_part(
                        typed_expr,
                        writer_expr,
                        *spec,
                        code_mode,
                        block_ctx.with_hidden_calls(true),
                    )?;
                    self.push_block_expr_id(&mut block, part_call);
                }
                parse::InterpolatedStringPart::Hole { spec, span } => {
                    let arg = get_nth_arg(self, args_variable.variable_id, hole_index, *span)?;
                    hole_index += 1;
                    let part_call = self.synth_format_part(
                        arg,
                        writer_expr,
                        *spec,
                        code_mode,
                        block_ctx.with_hidden_calls(true),
                    )?;
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
        let string_builder_ref = self.synth_variable_ref(string_builder_var.variable_id);
        let string_builder_expr =
            self.synth_address_of(string_builder_ref, Span::NONE, true).unwrap();
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
        let take_call = self.synth_typed_call_typed_args(
            self.ast.idents.f.StringBuilder_take.with_span(span),
            &[],
            &[string_builder_expr],
            ctx_for_calls,
            false,
        )?;
        self.push_block_expr_id(&mut block, take_call);

        let string_type = self.exprs.get_type(take_call);
        Ok(self.exprs.add_block(block, string_type))
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
        span: Span,
        ctx: EvalExprContext,
        args_expr: Option<TypedExprId>,
    ) -> K1Result<TypedExprId> {
        if parts.len() == 1 {
            if let parse::InterpolatedStringPart::String { string_id, span: part_span } =
                self.ast.mem.get_nth(parts, 0)
            {
                let string_id = *string_id;
                let part_span = *part_span;
                let value_id = self.make_static_code_value(string_id, part_span);
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
        let code_builder_ref = self.synth_variable_ref(code_builder_var.variable_id);
        let code_builder_expr = self.synth_address_of(code_builder_ref, Span::NONE, true).unwrap();
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
        span: Span,
    ) -> TypedExprId {
        let type_id = self.variables.get(variable_id).type_id;
        let expr = self.exprs.add(TypedExpr::Variable(VariableExpr { variable_id }), type_id, span);
        self.register_variable_usage(variable_id, span);
        expr
    }

    pub(super) fn synth_variable_ref(&mut self, variable_id: VariableId) -> TypedExprId {
        let variable = self.variables.get(variable_id);
        let (type_id, span) = (variable.type_id, variable.defn_span);
        self.exprs.add(TypedExpr::Variable(VariableExpr { variable_id }), type_id, span)
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
