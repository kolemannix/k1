// Copyright (c) 2025 knix
// All rights reserved.

/// `synth`, synthesis, aka spitting out typed code, used for features that desugar, as well
/// as some general lowering
use super::*;

impl TypedProgram {
    pub(super) fn synth_uword(&mut self, value: usize, span: SpanId) -> TypedExprId {
        let value = match self.target_word_size() {
            WordSize::W32 => TypedIntValue::UWord32(value as u32),
            WordSize::W64 => TypedIntValue::UWord64(value as u64),
        };
        let expr_id = self.exprs.add(TypedExpr::Integer(TypedIntegerExpr { value, span }));
        expr_id
    }

    pub(super) fn synth_bool(&mut self, value: bool, span: SpanId) -> TypedExprId {
        let expr_id = self.exprs.add(TypedExpr::Bool(value, span));
        expr_id
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
        patterns: MSlice<TypedPatternId>,
        result_type: TypeId,
        condition: TypedExprId,
        consequent: TypedExprId,
        alternate: TypedExprId,
        span: SpanId,
    ) -> TypedExprId {
        let condition_diverges = self.exprs.get(condition).get_type() == NEVER_TYPE_ID;
        let cons_arm = TypedMatchArm {
            condition: MatchingCondition {
                patterns,
                instrs: eco_vec![MatchingConditionInstr::Cond { value: condition }],
                binding_eligible: true,
                diverges: condition_diverges,
            },
            consequent_expr: consequent,
        };
        let alt_arm = TypedMatchArm {
            condition: MatchingCondition {
                patterns: MSlice::empty(),
                instrs: eco_vec![],
                binding_eligible: true,
                diverges: false,
            },
            consequent_expr: alternate,
        };
        self.exprs.add(TypedExpr::Match(TypedMatchExpr {
            initial_let_statements: eco_vec![],
            result_type,
            arms: eco_vec![cons_arm, alt_arm],
            span,
        }))
    }

    pub(super) fn synth_cast(
        &mut self,
        expr: TypedExprId,
        target_type: TypeId,
        cast_type: CastType,
        span: Option<SpanId>,
    ) -> TypedExprId {
        let span = span.unwrap_or_else(|| self.exprs.get(expr).get_span());
        self.exprs.add(TypedExpr::Cast(TypedCast {
            cast_type,
            base_expr: expr,
            target_type_id: target_type,
            span,
        }))
    }

    pub(super) fn synth_optional_type(&mut self, inner_type: TypeId) -> TypeId {
        let args = self.types.type_slices.add_slice_copy(&[inner_type]);
        self.instantiate_generic_type(OPTIONAL_TYPE_ID, args)
    }

    pub(super) fn synth_optional_some(&mut self, expression: TypedExpr) -> (TypedExprId, TypeId) {
        let optional_type = self.synth_optional_type(expression.get_type());
        let span = expression.get_span();
        let expr_id = self.exprs.add(expression);
        let some_variant = self
            .types
            .get(optional_type)
            .expect_enum()
            .variant_by_name(self.ast.idents.b.Some)
            .unwrap();

        let some_expr = self.exprs.add(TypedExpr::EnumConstructor(TypedEnumConstructor {
            variant_type_id: some_variant.my_type_id,
            variant_index: some_variant.index,
            span,
            payload: Some(expr_id),
        }));
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
        let none_expr = self.exprs.add(TypedExpr::EnumConstructor(TypedEnumConstructor {
            variant_type_id: none_variant.my_type_id,
            variant_index: none_variant.index,
            span,
            payload: None,
        }));
        let casted =
            self.synth_cast(none_expr, none_variant.enum_type_id, CastType::VariantToEnum, None);
        casted
    }

    pub(super) fn synth_dereference(&mut self, base: TypedExprId) -> TypedExprId {
        let base_expr = self.exprs.get(base);
        let span = base_expr.get_span();
        let type_id = self.types.get(base_expr.get_type()).expect_reference().inner_type;
        self.exprs.add(TypedExpr::UnaryOp(UnaryOp {
            kind: UnaryOpKind::Dereference,
            type_id,
            span,
            expr: base,
        }))
    }

    pub(super) fn synth_block(&mut self, parent_scope: ScopeId, span: SpanId) -> TypedBlock {
        let block_scope_id =
            self.scopes.add_child_scope(parent_scope, ScopeType::LexicalBlock, None, None);
        TypedBlock {
            expr_type: UNIT_TYPE_ID,
            statements: eco_vec![],
            scope_id: block_scope_id,
            span,
        }
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
        let initializer = self.exprs.get(initializer_id);
        let initializer_type = initializer.get_type();
        let span = initializer.get_span();
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
            self.exprs.add(TypedExpr::Variable(VariableExpr { type_id, variable_id, span }));
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
        let span = self.exprs.get(to_print).get_span();
        let writer_type_id = self.exprs.get(writer).get_type();
        self.synth_typed_call_typed_args(
            self.ast.idents.f.core_Print_printTo.with_span(span),
            &[writer_type_id],
            &[to_print, writer],
            ctx.with_no_expected_type(),
            false,
        )
    }

    pub fn synth_struct_expr(
        &mut self,
        struct_type_id: TypeId,
        field_exprs: Vec<TypedExprId>,
        scope_id: ScopeId,
        span: SpanId,
    ) -> TypedExprId {
        let struct_type = self.types.get(struct_type_id).expect_struct();
        debug_assert_eq!(struct_type.fields.len(), field_exprs.len());
        let mut fields: EcoVec<StructField> = EcoVec::with_capacity(struct_type.fields.len());
        for (index, field_expr) in field_exprs.into_iter().enumerate() {
            let field = &struct_type.fields[index];
            #[cfg(debug_assertions)]
            {
                let field_expr_type = self.exprs.get(field_expr).get_type();
                if let Err(msg) = self.check_types(field.type_id, field_expr_type, scope_id) {
                    panic!("synthed struct fields failed typechecking: {}", msg)
                }
            }
            fields.push(StructField { name: field.name, expr: field_expr });
        }
        self.exprs.add(TypedExpr::Struct(StructLiteral { fields, type_id: struct_type_id, span }))
    }

    pub(super) fn synth_string_literal(
        &mut self,
        msg: impl AsRef<str>,
        span: SpanId,
    ) -> TypedExprId {
        let string_id = self.ast.strings.intern(msg);
        self.exprs.add(TypedExpr::String(string_id, span))
    }

    pub(super) fn synth_source_location(&mut self, span: SpanId) -> TypedExprId {
        let (_, line) = self.get_span_location(span);
        let line_number = line.line_number();

        let source = self.ast.sources.source_by_span(self.ast.spans.get(span));
        let filename_string_id = self.ast.strings.intern(&source.filename);

        let struct_expr = TypedExpr::Struct(StructLiteral {
            fields: eco_vec![
                StructField {
                    name: self.ast.idents.b.filename,
                    expr: self.exprs.add(TypedExpr::String(filename_string_id, span)),
                },
                StructField {
                    name: self.ast.idents.b.line,
                    expr: self.exprs.add(TypedExpr::Integer(TypedIntegerExpr {
                        value: TypedIntValue::U64(line_number as u64),
                        span,
                    })),
                },
            ],
            type_id: COMPILER_SOURCE_LOC_TYPE_ID,
            span,
        });
        self.exprs.add(struct_expr)
    }

    pub(super) fn synth_crash_call(
        &mut self,
        message: &str,
        span: SpanId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let message_string_id = self.ast.strings.intern(message);
        let message_expr = self.exprs.add(TypedExpr::String(message_string_id, span));
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
        let span = self.exprs.get(value).get_span();
        self.synth_typed_call_typed_args(
            self.ast.idents.f.core_discard.with_span(span),
            &[],
            &[value],
            ctx,
            false,
        )
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
