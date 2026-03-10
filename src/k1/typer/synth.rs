// Copyright (c) 2025 knix
// All rights reserved.

use crate::failf;

/// `synth`, synthesis, aka spitting out typed code, used for features that desugar, as well
/// as some general lowering
use super::*;

impl TypedProgram {
    pub(super) fn synth_bool(&mut self, value: bool, span: SpanId) -> TypedExprId {
        let value_id = self.static_values.add(StaticValue::Bool(value));
        let expr_id = self.add_static_constant_expr(value_id, span);
        expr_id
    }

    pub(super) fn synth_empty_struct(&mut self, span: SpanId) -> TypedExprId {
        let value_id = self.static_values.empty_id();
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
            self.ast.idents.f.equals__equals.with_span(span),
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
            self.ast.idents.f.add__add.with_span(span),
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
        let cons_arm = TypedMatchArm {
            condition: MatchingCondition {
                instrs: self.mem.pushn(&[MatchingConditionInstr::Cond { value: condition }]),
            },
            consequent_expr: consequent,
        };
        let alt_arm = TypedMatchArm {
            condition: MatchingCondition { instrs: MSlice::empty() },
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

        let some_expr = self.exprs.add(
            TypedExpr::EnumConstructor(TypedEnumConstructor {
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
            TypedExpr::EnumConstructor(TypedEnumConstructor { variant_index: 0, payload: None }),
            optional_type,
            span,
        );
        none_expr
    }

    pub(super) fn synth_dereference(&mut self, base: TypedExprId) -> TypedExprId {
        let span = self.exprs.get_span(base);
        let type_id = self.get_expr_type(base).expect_reference().inner_type;
        self.exprs.add(TypedExpr::Deref(DerefExpr { target: base }), type_id, span)
    }

    pub(super) fn synth_block(
        &mut self,
        parent_scope: ScopeId,
        scope_type: ScopeType,
        span: SpanId,
        max_len: u32,
    ) -> BlockBuilder {
        let block_scope_id = self.scopes.add_child_scope(parent_scope, scope_type, None, None);
        BlockBuilder { statements: self.mem.new_list(max_len), scope_id: block_scope_id, span }
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
        self.exprs.add_block(&mut self.mem, b, NEVER_TYPE_ID)
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
                    self.push_expr_id_to_block(&mut b, *value);
                }
            }
        }
        self.exprs.add_block(&mut self.mem, b, NEVER_TYPE_ID)
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
        let defn_stmt = self.stmts.next_id();
        let variable = Variable {
            name: new_ident,
            owner_scope,
            type_id,
            kind: VariableKind::Let(defn_stmt),
            flags,
            usage_count: 0,
        };
        let variable_id = self.variables.add(variable);
        let variable_expr =
            self.exprs.add(TypedExpr::Variable(VariableExpr { variable_id }), type_id, span);
        self.stmts.add_expected_id(
            TypedStmt::Let(LetStmt {
                variable_id,
                variable_type: type_id,
                initializer: Some(initializer_id),
                is_referencing,
                span,
            }),
            defn_stmt,
        );
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
            self.ast.idents.f.bool__negated.with_span(span),
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

    pub(super) fn synth_i64(&mut self, value: i64, span: SpanId) -> TypedExprId {
        self.synth_int(TypedIntValue::I64(value), span)
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
                StructLiteralField { name: self.ast.idents.b.filename, expr: Some(filename_expr) },
                StructLiteralField { name: self.ast.idents.b.line, expr: Some(line_number_expr) },
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

    /// Used when we skip static execution, but still need to typecheck the rest of the
    /// body; this expression should never be executed; it should either be a call to
    /// crash, but transmuted to the expected type, or a special Unreachable node
    pub(super) fn synth_phony(&mut self, type_id: TypeId, span: SpanId) -> TypedExprId {
        let type_args =
            self.named_types.add_slice_copy(&[NameAndType { name: self.ast.idents.b.t, type_id }]);
        let phony_fn_id =
            self.scopes.find_function(self.scopes.core_scope_id, self.ast.idents.b.phony).unwrap();
        let specialized_phony_fn_id = self
            .specialize_function_signature(type_args, SliceHandle::empty(), phony_fn_id)
            .unwrap();
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

    pub(super) fn synth_enum_is_variant(
        &mut self,
        enum_expr_or_reference: TypedExprId,
        variant_index: u32,
        is_reference: bool,
        ctx: EvalExprContext,
        span: Option<SpanId>,
    ) -> TyperResult<TypedExprId> {
        let enum_type = self
            .types
            .get_type_dereferenced(self.exprs.get_type(enum_expr_or_reference))
            .expect_enum();
        let tag_type = enum_type.tag_type;
        let variant_tag =
            self.types.enum_variant_by_index(enum_type.variants, variant_index).tag_value;
        let span = span.unwrap_or(self.exprs.get_span(enum_expr_or_reference));
        let get_tag = self.exprs.add(
            TypedExpr::EnumGetTag(GetEnumTag { enum_expr_or_reference, is_reference }),
            tag_type,
            span,
        );
        let variant_tag_expr = self.synth_int(variant_tag, span);
        let tag_equals = self.synth_equals_call(get_tag, variant_tag_expr, ctx, span)?;
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
    ) -> TyperResult<TypedExprId> {
        let mut block = self.synth_block(ctx.scope_id, ScopeType::LexicalBlock, span, parts.len());
        let block_scope = block.scope_id;
        let block_ctx = ctx.with_scope(block_scope).with_no_expected_type();
        if self.ast.config.no_std {
            return failf!(span, "Interpolated strings are not supported in no_std mode");
        }
        let mut hole_index = 0;
        fn get_named_arg(
            k1: &mut TypedProgram,
            args: TypedExprId,
            name: Ident,
            span: SpanId,
        ) -> Option<TypedExprId> {
            let type_id = k1.exprs.get_type(args);
            match k1.types.get(type_id) {
                Type::Struct(_) => {
                    let Some((field_index, field)) =
                        k1.types.get_struct_field_by_name(type_id, name)
                    else {
                        return None;
                    };
                    let field_expr = k1.exprs.add(
                        TypedExpr::StructFieldAccess(FieldAccess {
                            base: args,
                            field_index: field_index as u32,
                            struct_type: type_id,
                            access_kind: FieldAccessKind::ValueToValue,
                        }),
                        field.type_id,
                        span,
                    );
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
        ) -> TyperResult<TypedExprId> {
            let type_id = k1.exprs.get_type(args);
            match k1.types.get(type_id) {
                Type::Char
                | Type::Bool
                | Type::Pointer
                | Type::Integer(_)
                | Type::Float(_)
                | Type::Reference(_)
                | Type::Array(_)
                | Type::Enum(_) => Ok(args),
                Type::Struct(s) => {
                    if n >= s.fields.len() as usize {
                        return failf!(
                            span,
                            "Format args struct only has {} fields, but this hole asks for field {}",
                            s.fields.len(),
                            n + 1
                        );
                    }
                    let nth_field = k1.types.get_struct_field(type_id, n);
                    let field_expr = k1.exprs.add(
                        TypedExpr::StructFieldAccess(FieldAccess {
                            base: args,
                            field_index: n as u32,
                            struct_type: type_id,
                            access_kind: FieldAccessKind::ValueToValue,
                        }),
                        nth_field.type_id,
                        span,
                    );
                    Ok(field_expr)
                }
                _ => {
                    failf!(span, "Not formattable currently: {}", k1.type_id_to_string(type_id))
                }
            }
        }

        for part in self.ast.mem.getn(parts) {
            match part {
                parse::InterpolatedStringPart::String(string_id) => {
                    let rust_str = self.ast.strings.get_string(*string_id);
                    if !rust_str.is_empty() {
                        let string_expr = self.synth_string_literal(*string_id, span);
                        let print_call =
                            self.synth_printto_call(string_expr, writer_expr, block_ctx)?;
                        self.push_expr_id_to_block(&mut block, print_call);
                    }
                }
                parse::InterpolatedStringPart::Expr(expr_id, _fmt_settings) => {
                    let parsed_expr = self.ast.exprs.get(*expr_id);
                    let expr_span = self.ast.exprs.get_span(*expr_id);
                    let naked_variable_name = match parsed_expr {
                        ParsedExpr::Variable(ParsedVariable { name }) if name.path.is_empty() => {
                            Some(name.name)
                        }
                        _ => None,
                    };
                    // Must be a naked variable expr, look for it in the struct, then in the scope
                    let typed_expr = match naked_variable_name {
                        None => {
                            let typed_expr = self.eval_expr(*expr_id, block_ctx)?;
                            typed_expr
                        }
                        Some(name) => {
                            let struct_arg = get_named_arg(self, args_expr, name, expr_span);
                            match struct_arg {
                                Some(field_expr) => field_expr,
                                None => self.eval_expr(*expr_id, block_ctx)?,
                            }
                        }
                    };

                    let print_expr_call = self.synth_printto_call(typed_expr, writer_expr, ctx)?;
                    self.push_expr_id_to_block(&mut block, print_expr_call);
                }
                parse::InterpolatedStringPart::Hole { fmt_settings: _, span } => {
                    // Grab the hole_index'th argument from args_expr.
                    let arg = get_nth_arg(self, args_expr, hole_index, *span)?;
                    hole_index += 1;
                    let print_expr_call = self.synth_printto_call(arg, writer_expr, ctx)?;
                    self.push_expr_id_to_block(&mut block, print_expr_call);
                }
            };
        }
        Ok(self.exprs.add_block(&mut self.mem, block, EMPTY_TYPE_ID))
    }

    /// Produces a string, resulting from using a core/string-builder
    /// to write the given interpolated string expr + arguments into it
    pub(super) fn synth_interpolated_string(
        &mut self,
        expr_id: ParsedExprId,
        ctx: EvalExprContext,
        args_expr: Option<TypedExprId>,
    ) -> TyperResult<TypedExprId> {
        let span = self.ast.exprs.get_span(expr_id);
        let ParsedExpr::InterpolatedString(interpolated_string) = *self.ast.exprs.get(expr_id)
        else {
            panic!()
        };

        let part_count = interpolated_string.parts.len();
        if part_count == 1 {
            let parse::InterpolatedStringPart::String(string_id) =
                self.ast.mem.get_nth(interpolated_string.parts, 0)
            else {
                panic!()
            };
            let e = self.synth_string_literal(*string_id, span);
            return Ok(e);
        }

        let mut block = self.synth_block(ctx.scope_id, ScopeType::LexicalBlock, span, 3);
        let block_scope = block.scope_id;
        let block_ctx = ctx.with_scope(block_scope).with_no_expected_type();
        if self.ast.config.no_std {
            return failf!(span, "Interpolated strings are not supported in no_std mode");
        }
        let new_string_builder = self.synth_typed_call_typed_args(
            self.ast.idents.f.StringBuilder_new.with_span(span),
            &[],
            &[],
            block_ctx,
            false,
        )?;
        let string_builder_var = self.synth_variable_defn(
            self.ast.idents.b.builder,
            new_string_builder,
            false,
            true, // is_mutable
            true, // is_referencing
            block.scope_id,
        );
        self.push_stmt_id_to_block(&mut block, string_builder_var.defn_stmt);
        let args_expr = args_expr.unwrap_or(self.synth_empty_struct(span));
        let format_block = self.synth_format_calls(
            string_builder_var.variable_expr,
            interpolated_string.parts,
            args_expr,
            span,
            ctx,
        )?;
        self.push_expr_id_to_block(&mut block, format_block);
        let build_call = self.synth_typed_call_typed_args(
            self.ast.idents.f.StringBuilder_buildTmp.with_span(span),
            &[],
            &[string_builder_var.variable_expr],
            block_ctx,
            false,
        )?;
        self.push_expr_id_to_block(&mut block, build_call);

        // build_call_type should definitely be string
        let build_call_type = self.exprs.get_type(build_call);
        Ok(self.exprs.add_block(&mut self.mem, block, build_call_type))
    }
}

pub(super) fn synth_static_option(
    static_values: &mut StaticValuePool,
    option_type_id: TypeId,
    value_id: Option<StaticValueId>,
) -> StaticValueId {
    let static_enum = match value_id {
        None => StaticEnum { enum_type_id: option_type_id, variant_index: 0, payload: None },
        Some(value_id) => {
            StaticEnum { enum_type_id: option_type_id, variant_index: 1, payload: Some(value_id) }
        }
    };

    static_values.add(StaticValue::Enum(static_enum))
}
