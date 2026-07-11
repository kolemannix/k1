// Copyright (c) 2026 knix
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

    pub(super) fn synth_equals_call_simple(
        &mut self,
        lhs: TypedExprId,
        rhs: TypedExprId,
        span: SpanId,
    ) -> TypedExprId {
        let ty = self.exprs.get_type(lhs);
        let rhs_type = self.exprs.get_type(rhs);
        debug_assert_eq!(ty, rhs_type);
        let Some(impl_id) = self
            .ability_impl_table
            .get(&ty)
            .and_then(|impls| impls.iter().find(|i| i.base_ability_id == ABILITY_ID_EQUALS))
        else {
            self.ice_span(span, "expected equals impl")
        };
        let AbilityImplFunction::FunctionId(equals_function_id) =
            self.ability_impls.get(impl_id.full_impl_id).function_at_index(&self.mem, 0)
        else {
            self.ice_span(span, "got abstract equals impl")
        };
        let call_id = self.calls.add(Call {
            callee: Callee::StaticFunction(*equals_function_id),
            args: self.mem.pushn(&[lhs, rhs]),
            type_args: MSlice::empty(),
            return_type: BOOL_TYPE_ID,
            span,
        });
        let expr_id = self.exprs.add(TypedExpr::Call { call_id }, BOOL_TYPE_ID, span);
        expr_id
    }

    pub(super) fn synth_add_call(
        &mut self,
        lhs: TypedExprId,
        rhs: TypedExprId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> K1Result<TypedExprId> {
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
                instrs: self.mem.pushn(&[MatchingConditionInstr::cond(condition)]),
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
        self.instantiate_generic_type(self.types.builtins.opt(), args)
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
        // nocommit: Try no span
        let span = self.exprs.get_span(base);
        let type_id = self.get_expr_type(base).expect_reference().inner_type;
        self.exprs.add(TypedExpr::Deref(DerefExpr { target: base }), type_id, span)
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
            self.new_block_builder(scope_id, ScopeType::LexicalBlock, span, exprs.len() as u32);
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
                    self.push_block_expr_id(&mut b, *value);
                }
            }
        }
        self.exprs.add_block(&mut self.mem, b, NEVER_TYPE_ID)
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
        no_mangle: bool,
        owner_scope: ScopeId,
        span: Option<SpanId>,
    ) -> SynthedVariable {
        let initializer_type = self.exprs.get_type(initializer_id);
        let span = match span {
            None => self.exprs.get_span(initializer_id),
            Some(span) => span,
        };
        let type_id = initializer_type;
        let new_ident = if no_mangle {
            name
        } else {
            self.build_ident_with(|k1, s| {
                write!(s, "__{}_{}", k1.ast.idents.get_string(name), k1.variables.len()).unwrap();
            })
        };
        let mut flags = VariableFlags::empty();
        flags.set(VariableFlags::UserHidden, !no_mangle);
        // let reassignable = true;
        // flags.set(VariableFlags::Reassigned, reassignable);
        let defn_stmt = self.stmts.next_id();
        let variable = Variable {
            name: new_ident,
            owner_scope,
            type_id,
            kind: VariableKind::StackSynthetic(defn_stmt),
            flags,
            usage_count: 0,
            usages: vec![],
            defn_span: span,
        };
        let variable_id = self.variables.add(variable);
        if !flags.contains(VariableFlags::UserHidden) {
            self.emit_ls_entity(span, LsEntityKind::Variable { variable_id })
        }
        let variable_expr =
            self.exprs.add(TypedExpr::Variable(VariableExpr { variable_id }), type_id, span);
        self.stmts.add_expected_id(
            TypedStmt::Let(LetStmt {
                variable_id,
                variable_type: type_id,
                initializer: Some(initializer_id),
                span,
            }),
            defn_stmt,
        );
        let parsed_expr = self.ast.exprs.add(
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
        let span = name.name_span;
        let type_args_iter = type_args.iter().map(|id| NamedTypeArg::unnamed(*id, span));
        let type_args = self.ast.mem.pushn_iter(type_args_iter);
        let args =
            self.ast.mem.pushn_iter(args.iter().map(|id| parse::ParsedCallArg::unnamed(*id)));
        self.ast.exprs.add(
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
    ) -> K1Result<TypedExprId> {
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
    ) -> K1Result<TypedExprId> {
        let call_id = self.synth_parsed_function_call(name, type_args, args, false);
        let call = self.ast.exprs.get(call_id).expect_call().clone();
        self.eval_function_call(&call, None, ctx, None)
    }

    pub(super) fn synth_parsed_bool_not(
        &mut self,
        base: ParsedExprId,
        span: SpanId,
    ) -> ParsedExprId {
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
    ) -> K1Result<TypedExprId> {
        let span = self.exprs.get_span(to_print);
        let writer_type_id = self.exprs.get_type(writer);
        self.synth_typed_call_typed_args(
            self.ast.idents.f.core_print_print_to.with_span(span),
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

    pub(super) fn synth_i64(&mut self, value: i64, span: SpanId) -> TypedExprId {
        self.synth_int(TypedIntValue::I64(value), span)
    }

    pub(super) fn synth_source_location(&mut self, span: SpanId) -> TypedExprId {
        let the_span = self.ast.spans.get(span);
        let source = self.ast.sources.get(the_span.file_id);
        let line_number = source.get_line_for_span_start(the_span).unwrap().line_number();

        let filename_string_id = match self.filename_string_ids.get(&the_span.file_id) {
            Some(id) => *id,
            None => {
                let id = self.ast.idents.intern(&source.filename);
                self.filename_string_ids.insert(the_span.file_id, id);
                id
            }
        };
        let filename_expr = self.synth_string_literal(filename_string_id, span);

        let line_number_expr = self.synth_int(TypedIntValue::U64(line_number as u64), span);
        let struct_expr = TypedExpr::Struct(StructLiteral {
            fields: self.mem.pushn(&[
                StructLiteralField { name: self.ast.idents.b.filename, expr: Some(filename_expr) },
                StructLiteralField { name: self.ast.idents.b.line, expr: Some(line_number_expr) },
            ]),
        });
        let source_location_type_id = self.types.builtins.source_location.unwrap();
        self.exprs.add(struct_expr, source_location_type_id, span)
    }

    pub(super) fn synth_crash_call(
        &mut self,
        message: &str,
        span: SpanId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let message_string_id = self.ast.idents.intern(message);
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
        let type_args = self.mem.pushn(&[NameAndType { name: self.ast.idents.b.t, type_id }]);
        let phony_fn_id =
            self.scopes.find_function(self.scopes.core_scope_id, self.ast.idents.b.phony).unwrap();
        let specialized_phony_fn_id =
            self.specialize_function_declaration(type_args, MSlice::empty(), phony_fn_id);
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

    // nocommit can I use synth_struct_field_access in more places now that its always the same
    pub(super) fn synth_field_access(
        &mut self,
        struct_expr: TypedExprId,
        field_index: usize,
        span: SpanId,
    ) -> TypedExprId {
        let struct_type_id = self.exprs.get_type(struct_expr);
        let Type::Struct(_s) = self.types.get(struct_type_id) else {
            self.ice_span(span, "bad struct field access: base is not a struct");
        };
        let field = self.types.get_struct_field(struct_type_id, field_index);
        let expr_id = self.exprs.add(
            TypedExpr::StructFieldAccess(FieldAccess {
                base: struct_expr,
                field_index: field_index as u32,
                struct_type: struct_type_id,
            }),
            field.type_id,
            span,
        );
        expr_id
    }

    pub(super) fn synth_parsed_variable_expr(
        &mut self,
        name: StringId,
        span: SpanId,
    ) -> ParsedExprId {
        self.ast.exprs.add(
            ParsedExpr::Variable(parse::ParsedVariable { name: QIdent::naked(name, span) }),
            false,
            None,
        )
    }

    pub(super) fn synth_parsed_type_app(
        &mut self,
        name: StringId,
        span: SpanId,
    ) -> ParsedTypeExprId {
        self.ast.type_exprs.add(ParsedTypeExpr::TypeApplication(parse::TypeApplication {
            name: QIdent::naked(name, span),
            args: MSlice::empty(),
            span,
        }))
    }

    pub(super) fn synth_sum_is_variant(
        &mut self,
        sum_expr: TypedExprId,
        variant_index: u32,
        span: Option<SpanId>,
    ) -> K1Result<TypedExprId> {
        let sum_type = self.types.get(self.exprs.get_type(sum_expr)).expect_sum();
        let tag_type = sum_type.tag_type;
        let variant_tag =
            self.types.sum_variant_by_index(sum_type.variants, variant_index).tag_value;
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
        let mut block =
            self.new_block_builder(ctx.scope_id, ScopeType::LexicalBlock, span, parts.len() + 1);
        let block_scope = block.scope_id;
        let args_variable =
            self.synth_variable_defn_simple(self.ast.idents.b.fmtargs, args_expr, block_scope);
        self.push_block_stmt_id(&mut block, args_variable.defn_stmt);
        let block_ctx = ctx.with_scope(block_scope).with_no_expected_type();
        if self.config.no_std {
            return failf!(span, "Interpolated strings are not supported in no_std mode");
        }
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
                    if type_id == k1.types.builtins.string() {
                        return None;
                    }

                    let (field_index, _field) = k1.types.get_struct_field_by_name(type_id, name)?;
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
                | Type::Sum(_) => Ok(args),
                Type::Struct(s) => {
                    if type_id == k1.types.builtins.string() {
                        if n == 0 {
                            Ok(args)
                        } else {
                            failf!(
                                span,
                                "this hole asks for field {} on a string, which provides just 1 value",
                                n + 1
                            )
                        }
                    } else {
                        if n >= s.fields.len() as usize {
                            return failf!(
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
                _ => {
                    failf!(span, "Not formattable currently: {}", k1.type_id_to_string(type_id))
                }
            }
        }

        let mut i = 0usize;
        while i < parts.len() as usize {
            match self.ast.mem.get_nth(parts, i) {
                parse::InterpolatedStringPart::String { string_id, .. } => {
                    // Combine consecutive strings into a single constant
                    let mut string_to_print = *string_id;
                    let mut combined: Option<String> = None;
                    while i + 1 < parts.len() as usize {
                        let next = self.ast.mem.get_nth(parts, i + 1);
                        if let InterpolatedStringPart::String { string_id: next_string, .. } = next
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

                    let print_expr_call = self.synth_printto_call(
                        typed_expr,
                        writer_expr,
                        block_ctx.with_hidden_calls(true),
                    )?;
                    self.push_block_expr_id(&mut block, print_expr_call);
                }
                parse::InterpolatedStringPart::Hole { fmt_settings: _, span } => {
                    // Grab the hole_index'th argument from args_variable.
                    let arg = get_nth_arg(self, args_variable.variable_expr, hole_index, *span)?;
                    hole_index += 1;
                    let print_expr_call = self.synth_printto_call(arg, writer_expr, ctx)?;
                    self.push_block_expr_id(&mut block, print_expr_call);
                }
            }
            i += 1
        }
        if block.statements.is_empty() {
            Ok(self.synth_empty_struct(span))
        } else {
            Ok(self.exprs.add_block(&mut self.mem, block, EMPTY_TYPE_ID))
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
        if self.config.no_std {
            return failf!(span, "Interpolated strings are not supported in no_std mode");
        }
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
        let args_expr = args_expr.unwrap_or(self.synth_empty_struct(span));
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
        Ok(self.exprs.add_block(&mut self.mem, block, build_call_type))
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
