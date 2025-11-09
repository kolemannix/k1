// Copyright (c) 2025 knix
// All rights reserved.

/// `synth`, synthesis, aka spitting out typed code, used for features that desugar, as well
/// as some general lowering
use super::*;

impl TypedProgram {
    pub(super) fn synth_bool(&mut self, value: bool, span: SpanId) -> TypedExprId {
        let value_id = self.static_values.add(StaticValue::Bool(value));
        let expr_id = self.add_static_constant_expr(value_id, span);
        expr_id
    }

    pub(super) fn synth_unit(&mut self, span: SpanId) -> TypedExprId {
        let value_id = self.static_values.add(StaticValue::Unit);
        self.add_static_constant_expr(value_id, span)
    }

    pub(super) fn synth_equals_call(
        &mut self,
        lhs: TypedExprId,
        rhs: TypedExprId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        self.synth_typed_call_typed_args(
            self.ast.idents.f.Equals_equals.with_span(span),
            &[],
            &[lhs, rhs],
            ctx.with_no_expected_type(),
            false,
        )
    }

    pub(super) fn synth_add_call(
        &mut self,
        lhs: TypedExprId,
        rhs: TypedExprId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        self.synth_typed_call_typed_args(
            self.ast.idents.f.Add_add.with_span(span),
            &[],
            &[lhs, rhs],
            ctx.with_no_expected_type(),
            false,
        )
    }

    pub(super) fn synth_if_else(
        &mut self,
        result_type: TypeId,
        condition: TypedExprId,
        consequent: TypedExprId,
        alternate: TypedExprId,
        span: SpanId,
    ) -> TypedExprId {
        let condition_diverges = self.exprs.get_type(condition) == NEVER_TYPE_ID;
        let condition_span = self.exprs.get_span(condition);
        let cons_arm = TypedMatchArm {
            condition: MatchingCondition {
                instrs: self.mem.pushn(&[MatchingConditionInstr::Cond { value: condition }]),
                diverges: condition_diverges,
                span: condition_span,
            },
            consequent_expr: consequent,
        };
        let alt_arm = TypedMatchArm {
            condition: MatchingCondition {
                instrs: MSlice::empty(),
                diverges: false,
                span: condition_span,
            },
            consequent_expr: alternate,
        };
        self.exprs.add(
            TypedExpr::Match(TypedMatchExpr {
                initial_let_statements: MSlice::empty(),
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
        let args = self.types.mem.pushn(&[inner_type]);
        self.instantiate_generic_type(OPTIONAL_TYPE_ID, args)
    }

    pub(super) fn synth_optional_some(&mut self, expr_id: TypedExprId) -> (TypedExprId, TypeId) {
        let span = self.exprs.get_span(expr_id);
        let inner_type = self.exprs.get_type(expr_id);
        let optional_type = self.synth_optional_type(inner_type);
        let some_variant = self
            .types
            .get(optional_type)
            .expect_enum()
            .variant_by_name(self.ast.idents.b.Some)
            .unwrap();

        let some_expr = self.exprs.add(
            TypedExpr::EnumConstructor(TypedEnumConstructor {
                variant_index: some_variant.index,
                payload: Some(expr_id),
            }),
            some_variant.my_type_id,
            span,
        );
        let casted =
            self.synth_cast(some_expr, some_variant.enum_type_id, CastType::VariantToEnum, None);
        (casted, optional_type)
    }

    pub(super) fn synth_optional_none(&mut self, type_id: TypeId, span: SpanId) -> TypedExprId {
        let optional_type = self.synth_optional_type(type_id);
        let none_variant = self
            .types
            .get(optional_type)
            .expect_enum()
            .variant_by_name(self.ast.idents.b.None)
            .unwrap();
        let none_expr = self.exprs.add(
            TypedExpr::EnumConstructor(TypedEnumConstructor {
                variant_index: none_variant.index,
                payload: None,
            }),
            none_variant.my_type_id,
            span,
        );
        let casted =
            self.synth_cast(none_expr, none_variant.enum_type_id, CastType::VariantToEnum, None);
        casted
    }

    pub(super) fn synth_dereference(&mut self, base: TypedExprId) -> TypedExprId {
        let span = self.exprs.get_span(base);
        let type_id = self.get_expr_type(base).expect_reference().inner_type;
        self.exprs.add_tmp(TypedExpr::Deref(DerefExpr { type_id, span, target: base }))
    }

    pub(super) fn synth_block(
        &mut self,
        parent_scope: ScopeId,
        scope_type: ScopeType,
        span: SpanId,
        max_len: u32,
    ) -> BlockBuilder {
        let block_scope_id = self.scopes.add_child_scope(parent_scope, scope_type, None, None);
        BlockBuilder { statements: self.mem.new_vec(max_len), scope_id: block_scope_id, span }
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
        let mut b = self.synth_block(scope_id, ScopeType::LexicalBlock, span, exprs.len() as u32);
        for e in exprs {
            let e_type_id = self.exprs.get_type(*e);
            self.push_block_stmt(&mut b, TypedStmt::Expr(*e, e_type_id));
        }
        return self.exprs.add_block(&mut self.mem, b, NEVER_TYPE_ID);
    }

    pub(super) fn make_never_condition_block(
        &mut self,
        instrs: &[MatchingConditionInstr],
        scope_id: ScopeId,
        span: SpanId,
    ) -> TypedExprId {
        let mut b =
            BlockBuilder { statements: self.mem.new_vec(instrs.len() as u32), scope_id, span };
        for i in instrs {
            match i {
                MatchingConditionInstr::Binding { let_stmt } => b.statements.push(*let_stmt),
                MatchingConditionInstr::Cond { value } => {
                    self.push_expr_id_to_block(&mut b, *value);
                }
            }
        }
        return self.exprs.add_block(&mut self.mem, b, NEVER_TYPE_ID);
    }

    /// Creates a non-mutable, mangled, non-referencing variable defn.
    /// This is the vastly most common case
    pub(super) fn synth_variable_defn_simple(
        &mut self,
        name: Ident,
        initializer: TypedExprId,
        owner_scope: ScopeId,
    ) -> SynthedVariable {
        self.synth_variable_defn(name, initializer, false, false, false, owner_scope)
    }

    /// Creates a user-code-visible variable
    pub(super) fn synth_variable_defn_visible(
        &mut self,
        name: Ident,
        initializer: TypedExprId,
        owner_scope: ScopeId,
    ) -> SynthedVariable {
        self.synth_variable_defn(name, initializer, true, false, false, owner_scope)
    }

    /// no_mangle: Skip mangling if we want the variable to be accessible from user code
    pub(super) fn synth_variable_defn(
        &mut self,
        name: Ident,
        initializer_id: TypedExprId,
        no_mangle: bool,
        is_mutable: bool,
        is_referencing: bool,
        owner_scope: ScopeId,
    ) -> SynthedVariable {
        let initializer_type = self.exprs.get_type(initializer_id);
        let span = self.exprs.get_span(initializer_id);
        let type_id = if is_referencing {
            self.types.add_reference_type(initializer_type, is_mutable)
        } else {
            initializer_type
        };
        let new_ident = if no_mangle {
            name
        } else {
            self.build_ident_with(|k1, s| {
                write!(s, "__{}_{}", k1.ast.idents.get_name(name), k1.variables.len()).unwrap();
            })
        };
        let mut flags = VariableFlags::empty();
        flags.set(VariableFlags::UserHidden, !no_mangle);
        let reassignable = !is_referencing && is_mutable;
        flags.set(VariableFlags::Reassigned, reassignable);
        let variable = Variable { name: new_ident, owner_scope, type_id, global_id: None, flags };
        let variable_id = self.variables.add(variable);
        let variable_expr =
            self.exprs.add_tmp(TypedExpr::Variable(VariableExpr { type_id, variable_id, span }));
        let defn_stmt = self.stmts.add(TypedStmt::Let(LetStmt {
            variable_id,
            variable_type: type_id,
            initializer: Some(initializer_id),
            is_referencing,
            span,
        }));
        let parsed_expr = self.ast.exprs.add_expression(
            ParsedExpr::Variable(parse::ParsedVariable { name: QIdent::naked(name, span) }),
            false,
            None,
        );
        self.scopes.add_variable(owner_scope, new_ident, variable_id);
        SynthedVariable { variable_id, defn_stmt, variable_expr, parsed_expr }
    }

    pub(super) fn synth_parsed_function_call(
        &mut self,
        name: QIdent,
        type_args: &[ParsedTypeExprId],
        args: &[ParsedExprId],
        is_method: bool,
    ) -> ParsedExprId {
        let span = name.span;
        let type_args_iter = type_args.iter().map(|id| NamedTypeArg::unnamed(*id, span));
        let type_args = self.ast.p_type_args.add_slice_from_iter(type_args_iter);
        let args = self
            .ast
            .p_call_args
            .add_slice_from_iter(args.iter().map(|id| parse::ParsedCallArg::unnamed(*id)));
        self.ast.exprs.add_expression(
            ParsedExpr::Call(ParsedCall {
                name,
                type_args,
                args,
                span,
                is_method,
                id: ParsedExprId::PENDING,
            }),
            false,
            None,
        )
    }

    pub(super) fn synth_typed_call_typed_args(
        &mut self,
        name: QIdent,
        type_args: &[TypeId],
        args: &[TypedExprId],
        ctx: EvalExprContext,
        is_method: bool,
    ) -> TyperResult<TypedExprId> {
        let call_id = self.synth_parsed_function_call(name, &[], &[], is_method);
        let call = self.ast.exprs.get(call_id).expect_call().clone();
        self.eval_function_call(&call, Some((type_args, args)), ctx, None)
    }

    pub(super) fn synth_typed_call_parsed_args(
        &mut self,
        name: QIdent,
        type_args: &[ParsedTypeExprId],
        args: &[ParsedExprId],
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let call_id = self.synth_parsed_function_call(name, type_args, args, false);
        let call = self.ast.exprs.get(call_id).expect_call().clone();
        self.eval_function_call(&call, None, ctx, None)
    }

    pub(super) fn synth_parsed_bool_not(&mut self, base: ParsedExprId) -> ParsedExprId {
        let span = self.ast.exprs.get_span(base);
        self.synth_parsed_function_call(
            self.ast.idents.f.bool_negated.with_span(span),
            &[],
            &[base],
            false,
        )
    }

    pub(super) fn synth_printto_call(
        &mut self,
        to_print: TypedExprId,
        writer: TypedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let span = self.exprs.get_span(to_print);
        let writer_type_id = self.exprs.get_type(writer);
        self.synth_typed_call_typed_args(
            self.ast.idents.f.core_Print_printTo.with_span(span),
            &[writer_type_id],
            &[to_print, writer],
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

    pub(super) fn synth_int(&mut self, int_value: TypedIntValue, span: SpanId) -> TypedExprId {
        let int_value_id = self.static_values.add_int(int_value);
        self.add_static_constant_expr(int_value_id, span)
    }

    pub(super) fn synth_uword(&mut self, value: usize, span: SpanId) -> TypedExprId {
        let value = match self.target_word_size() {
            WordSize::W32 => TypedIntValue::UWord32(value as u32),
            WordSize::W64 => TypedIntValue::UWord64(value as u64),
        };
        self.synth_int(value, span)
    }

    pub(super) fn synth_source_location(&mut self, span: SpanId) -> TypedExprId {
        let (_, line) = self.get_span_location(span);
        let line_number = line.line_number();

        let source = self.ast.sources.source_by_span(self.ast.spans.get(span));
        let filename_string_id = self.ast.strings.intern(&source.filename);
        let filename_expr = self.synth_string_literal(filename_string_id, span);

        let line_number_expr = self.synth_int(TypedIntValue::U64(line_number as u64), span);
        let struct_expr = TypedExpr::Struct(StructLiteral {
            fields: self.mem.pushn(&[
                StructLiteralField { name: self.ast.idents.b.filename, expr: filename_expr },
                StructLiteralField { name: self.ast.idents.b.line, expr: line_number_expr },
            ]),
        });
        self.exprs.add(struct_expr, COMPILER_SOURCE_LOC_TYPE_ID, span)
    }

    pub(super) fn synth_crash_call(
        &mut self,
        message: &str,
        span: SpanId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let message_string_id = self.ast.strings.intern(message);
        let message_expr = self.synth_string_literal(message_string_id, span);
        self.synth_typed_call_typed_args(
            self.ast.idents.f.core_crash.with_span(span),
            &[],
            &[message_expr],
            ctx,
            false,
        )
    }

    pub(super) fn synth_discard_call(
        &mut self,
        value: TypedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let span = self.exprs.get_span(value);
        self.synth_typed_call_typed_args(
            self.ast.idents.f.core_discard.with_span(span),
            &[],
            &[value],
            ctx,
            false,
        )
    }

    pub(super) fn synth_enum_is_variant(
        &mut self,
        enum_expr_or_reference: TypedExprId,
        variant_index: u32,
        ctx: EvalExprContext,
        span: Option<SpanId>,
    ) -> TyperResult<TypedExprId> {
        let enum_type = self
            .types
            .get_type_dereferenced(self.exprs.get_type(enum_expr_or_reference))
            .expect_enum();
        let tag_type = enum_type.tag_type;
        let variant_tag = enum_type.variant_by_index(variant_index).tag_value;
        let span = span.unwrap_or(self.exprs.get_span(enum_expr_or_reference));
        let get_tag = self.exprs.add_tmp(TypedExpr::EnumGetTag(GetEnumTag {
            enum_expr_or_reference,
            result_type_id: tag_type,
            span,
        }));
        let variant_tag_expr = self.synth_int(variant_tag, span);
        let tag_equals = self.synth_equals_call(get_tag, variant_tag_expr, ctx, span)?;
        Ok(tag_equals)
    }
}

pub(super) fn synth_static_option(
    types: &TypePool,
    static_values: &mut StaticValuePool,
    option_type_id: TypeId,
    value_id: Option<StaticValueId>,
) -> StaticValueId {
    let opt_enum_type = types.get(option_type_id).expect_enum();
    let static_enum = match value_id {
        None => StaticEnum {
            variant_type_id: opt_enum_type.variant_by_index(0).my_type_id,
            variant_index: 0,
            typed_as_enum: true,
            payload: None,
        },
        Some(value_id) => StaticEnum {
            variant_type_id: opt_enum_type.variant_by_index(1).my_type_id,
            variant_index: 1,
            typed_as_enum: true,
            payload: Some(value_id),
        },
    };

    static_values.add(StaticValue::Enum(static_enum))
}
