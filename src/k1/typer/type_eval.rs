// Copyright (c) 2026 knix
// All rights reserved.

use super::*;
use crate::panic_at_disco;

impl TypedProgram {
    pub(super) fn eval_type_defn(
        &mut self,
        parsed_type_defn_id: ParsedTypeDefnId,
        namespace_scope_id: ScopeId,
    ) -> K1Result<TypeId> {
        let parse::ParsedTypeDefn { name_span, name, typer_state, .. } =
            *self.ast.get_type_defn(parsed_type_defn_id);
        match typer_state {
            ParsedTypeDefnDeclareOutcome::Defined(type_id) => return Ok(type_id),
            ParsedTypeDefnDeclareOutcome::Failed => {
                return Err(kerr!(self, name_span, "Type {} has an invalid definition", name));
            }
            ParsedTypeDefnDeclareOutcome::IfDefedOut => {
                self.ice_span(name_span, "eval ifdefed-out type defn")
            }
            ParsedTypeDefnDeclareOutcome::Parsed => {}
        }
        let result = self.eval_type_defn_inner(parsed_type_defn_id, namespace_scope_id);
        self.ast.type_defns.get_mut(parsed_type_defn_id).typer_state = match result {
            Ok(type_id) => ParsedTypeDefnDeclareOutcome::Defined(type_id),
            Err(_) => ParsedTypeDefnDeclareOutcome::Failed,
        };
        result
    }

    fn eval_type_defn_inner(
        &mut self,
        parsed_type_defn_id: ParsedTypeDefnId,
        namespace_scope_id: ScopeId,
    ) -> K1Result<TypeId> {
        let parsed_type_defn = *self.ast.get_type_defn(parsed_type_defn_id);
        let is_generic_defn = !parsed_type_defn.type_params.is_empty();
        let is_alias = parsed_type_defn.flags.is_alias();
        debug!("eval_type_defn {}", self.ident_str(parsed_type_defn.name));

        if parsed_type_defn.name == self.ast.idents.b.some {
            kbail!(self, parsed_type_defn.name_span, "'some' is not a valid type name");
        }

        if is_alias && is_generic_defn {
            kbail!(
                self,
                parsed_type_defn.name_span,
                "Alias types cannot have type parameters (yet)"
            );
        }

        let reserved_type_id = if !is_alias { Some(self.reserve_type_id()) } else { None };
        self.type_defn_context
            .stack
            .push(TypeDefnStackEntry { parsed_id: parsed_type_defn_id, reserved_type_id });

        let type_eval_context = EvalTypeExprContext {
            is_inside_type_definition_rhs: !parsed_type_defn.flags.is_alias(),
            is_direct_function_parameter: false,
            is_inside_static_type: false,
        };

        // No need to create a scope for non-generic type definitions
        let defn_scope_id = if is_generic_defn {
            let defn_scope_id = self.scopes.add_child_scope(
                namespace_scope_id,
                ScopeType::TypeDefn,
                ScopeOwnerId::None,
            );
            defn_scope_id
        } else {
            namespace_scope_id
        };

        let mut type_params: List<TypeId, _> =
            self.mem.new_list(parsed_type_defn.type_params.len());
        for type_param in self.ast.mem.getn(parsed_type_defn.type_params).iter() {
            let maybe_static_constraint =
                match ParsedTypeConstraintExpr::single_static_constraint_or_fail(
                    &self.ast.mem,
                    type_param.constraints,
                ) {
                    Ok(Some(parsed_static_constraint)) => {
                        Some(self.eval_type_expr(parsed_static_constraint, defn_scope_id)?)
                    }
                    Ok(None) => None,
                    Err(msg) => kbail!(self, type_param.span, "{}", msg),
                };
            let mut ability_constraint_signatures: SV4<TypedAbilitySignature> = smallvec![];
            let mut predicate_functions: SV4<QIdent> = smallvec![];
            for parsed_constraint in self.ast.mem.getn(type_param.constraints) {
                match parsed_constraint {
                    ParsedTypeConstraintExpr::Ability(ability_expr) => {
                        let ability_sig = self.eval_ability_expr(
                            *ability_expr,
                            AbilityExprRole::Constraint,
                            defn_scope_id,
                        )?;
                        ability_constraint_signatures.push(ability_sig);
                    }
                    ParsedTypeConstraintExpr::Predicate(qident) => {
                        predicate_functions.push(*qident);
                    }
                    ParsedTypeConstraintExpr::Static(_) => {}
                };
            }
            let predicate_functions_handle = self.mem.pushn(&predicate_functions);
            let type_variable_id = self.add_type_parameter(
                TypeParameter {
                    name: type_param.name,
                    static_constraint: maybe_static_constraint,
                    predicate_functions: predicate_functions_handle,
                    scope_id: defn_scope_id,
                    span: type_param.span,
                },
                ability_constraint_signatures,
            );
            type_params.push(type_variable_id);
            let added = self.scopes.add_type(defn_scope_id, type_param.name, type_variable_id);
            if !added {
                kbail!(
                    self,
                    type_param.span,
                    "Type variable name '{}' is taken",
                    self.ident_str(type_param.name).blue()
                );
            }
        }
        let type_params_handle = type_params.to_slice();

        // Actually compile the RHS
        let rhs_expr = self.ast.type_exprs.get(parsed_type_defn.value_expr);
        let (rhs_type_id, builtin_value) = match rhs_expr {
            ParsedTypeExpr::Builtin(_) => {
                let defn_type_id = reserved_type_id.unwrap();
                let type_value = match defn_type_id {
                    CHAR_TYPE_ID => Type::Char,
                    BOOL_TYPE_ID => Type::Bool,
                    NEVER_TYPE_ID => Type::Never,
                    POINTER_TYPE_ID => Type::Pointer,
                    F32_TYPE_ID => Type::Float(FloatType::F32),
                    F64_TYPE_ID => Type::Float(FloatType::F64),
                    U8_TYPE_ID => Type::Integer(IntegerType::U8),
                    U16_TYPE_ID => Type::Integer(IntegerType::U16),
                    U32_TYPE_ID => Type::Integer(IntegerType::U32),
                    U64_TYPE_ID => Type::Integer(IntegerType::U64),
                    I8_TYPE_ID => Type::Integer(IntegerType::I8),
                    I16_TYPE_ID => Type::Integer(IntegerType::I16),
                    I32_TYPE_ID => Type::Integer(IntegerType::I32),
                    I64_TYPE_ID => Type::Integer(IntegerType::I64),
                    _ => {
                        kbail!(
                            self,
                            parsed_type_defn.name_span,
                            "Unknown builtin type id: {}",
                            defn_type_id
                        );
                    }
                };
                (defn_type_id, Some(type_value))
            }
            _ => {
                let rhs_type_id = self.eval_type_expr_ext(
                    parsed_type_defn.value_expr,
                    defn_scope_id,
                    type_eval_context,
                )?;
                let rhs_is_reserved = self
                    .type_defn_context
                    .stack
                    .iter()
                    .any(|e| e.reserved_type_id == Some(rhs_type_id));
                let rhs_is_pending = self
                    .type_defn_context
                    .pending_instances
                    .iter()
                    .any(|p| p.type_id == rhs_type_id);
                let rhs_still_defining = !is_alias && (rhs_is_reserved || rhs_is_pending);
                if rhs_still_defining {
                    kbail!(
                        self,
                        parsed_type_defn.name_span,
                        "The right-hand side of a type definition cannot be a bare reference to a type that is still being defined; wrap it in a struct or either, or use an alias"
                    );
                }
                (rhs_type_id, None)
            }
        };

        // not an alias, so only allow named struct or sum or builtin
        // TODO: revisit this? why can't we have nominal integer types?
        if !is_alias && builtin_value.is_none() && !self.types.get(rhs_type_id).is_nominal_kind() {
            kbail!(
                self,
                parsed_type_defn.name_span,
                "Non-alias type definition must be a struct, either or opaque; not a '{}'. Perhaps you meant to create an alias `type(alias) <name> = <type>`",
                rhs_type_id
            );
        }

        let defn_info = if is_alias {
            None
        } else {
            Some(TypeDefnInfo {
                name: parsed_type_defn.name,
                namespace_id: self
                    .scopes
                    .get_scope_owner(namespace_scope_id)
                    .as_namespace()
                    .unwrap(),
                ast_id: ParsedId::TypeDefn(parsed_type_defn_id),
            })
        };

        let type_id = if is_generic_defn {
            let gen_type = GenericType { params: type_params_handle, inner: rhs_type_id };
            let generic_id = reserved_type_id.unwrap();
            self.set_type(generic_id, Type::Generic(gen_type), defn_info);
            self.instantiate_generic_type(generic_id, self.mem.getn(type_params_handle));
            generic_id
        } else if is_alias {
            rhs_type_id
        } else {
            let t = match builtin_value {
                Some(value) => value,
                None => *self.types.get(rhs_type_id),
            };
            self.set_type(reserved_type_id.unwrap(), t, defn_info);
            reserved_type_id.unwrap()
        };

        if !is_alias && let Some(companion_namespace_id) = self.get_companion_namespace(type_id) {
            self.attach_companion_namespace(companion_namespace_id, type_id);
        }
        let name = parsed_type_defn.name;
        let added = self.scopes.add_type(namespace_scope_id, name, type_id);
        if !added {
            let span = parsed_type_defn.name_span;
            self.report(kerr!(self, span, "Type {} exists", name));
        }

        // Capture type_ids of compiler-known types
        if namespace_scope_id == self.scopes.core_scope_id {
            let b = &self.ast.idents.b;
            if name == b.string {
                self.builtin_types.string = Some(type_id);
            } else if name == b.bool {
                self.builtin_types.bool = Some(type_id);
            } else if name == b.char {
                self.builtin_types.char = Some(type_id);
            } else if name == b.ptr {
                self.builtin_types.ptr = Some(type_id);
            } else if name == b.buffer {
                self.builtin_types.buffer = Some(type_id);
            } else if name == b.span {
                self.builtin_types.span = Some(type_id);
            } else if name == b.list {
                self.builtin_types.list = Some(type_id);
            } else if name == b.opt {
                self.builtin_types.opt = Some(type_id);
            } else if name == b.ordering {
                self.builtin_types.ordering = Some(type_id);
            } else if name == b.code {
                self.builtin_types.code = Some(type_id);
            } else if name == b.code_span {
                self.builtin_types.code_span = Some(type_id);
            } else if name == b.code_token {
                self.builtin_types.code_token = Some(type_id);
            } else if name == b.code_builder {
                self.builtin_types.code_builder = Some(type_id);
            }
        } else if namespace_scope_id == self.scopes.types_scope_id {
            if name == self.ast.idents.b.type_schema {
                self.builtin_types.types_type_schema = Some(type_id);
            } else if name == self.ast.idents.b.type_info {
                self.builtin_types.types_type_info = Some(type_id);
            } else if name == self.ast.idents.b.impl_info {
                self.builtin_types.types_impl_info = Some(type_id);
            } else if name == self.ast.idents.b.int_kind {
                self.builtin_types.types_int_kind = Some(type_id);
            } else if name == self.ast.idents.b.int_value {
                self.builtin_types.types_int_value = Some(type_id)
            } else if name == self.ast.idents.b.float_kind {
                self.builtin_types.types_float_kind = Some(type_id)
            } else if name == self.ast.idents.b.float_value {
                self.builtin_types.types_float_value = Some(type_id)
            } else if name == self.ast.idents.b.type_id {
                self.builtin_types.types_type_id = Some(type_id)
            } else if name == self.ast.idents.b.layout {
                self.builtin_types.types_layout = Some(type_id)
            }
        } else if namespace_scope_id == self.scopes.k1_scope_id {
            if name == self.ast.idents.b.source_location {
                self.builtin_types.source_location = Some(type_id)
            } else if name == self.ast.idents.b.module {
                self.builtin_types.k1_module = Some(type_id)
            } else if name == self.ast.idents.b.setup_ctx {
                self.builtin_types.k1_setup_ctx = Some(type_id)
            } else if name == self.ast.idents.b.build_config {
                self.builtin_types.k1_build_config = Some(type_id)
            } else if name == self.ast.idents.b.build_request {
                self.builtin_types.k1_build_request = Some(type_id)
            } else if name == self.ast.idents.b.editor_settings {
                self.builtin_types.k1_editor_settings = Some(type_id)
            }
        }

        let Some(defn_stack_entry) = self.type_defn_context.stack.pop() else {
            self.ice_span(parsed_type_defn.name_span, "No defn stack entry");
        };
        debug_assert_eq!(defn_stack_entry.parsed_id, parsed_type_defn_id);
        debug_assert_eq!(defn_stack_entry.reserved_type_id, reserved_type_id);
        if !is_alias {
            self.type_defn_context.completed.push((type_id, parsed_type_defn.name_span));
        }
        if self.type_defn_context.stack.is_empty() {
            self.finish_type_defn_cluster()
        }
        Ok(type_id)
    }

    pub(super) fn eval_type_expr(
        &mut self,
        type_expr_id: ParsedTypeExprId,
        scope_id: ScopeId,
    ) -> K1Result<TypeId> {
        self.eval_type_expr_ext(type_expr_id, scope_id, EvalTypeExprContext::EMPTY)
    }

    pub(super) fn eval_type_expr_ext(
        &mut self,
        type_expr_id: ParsedTypeExprId,
        scope_id: ScopeId,
        // The context is mostly about 'where we are' - where this type expression is located -
        // with respect to the source language. Is this a type definition? A function parameter
        // type? Are we immediately inside a function parameter?
        context: EvalTypeExprContext,
    ) -> K1Result<TypeId> {
        let base = match self.ast.type_exprs.get(type_expr_id) {
            ParsedTypeExpr::Builtin(_) => unreachable!(),
            ParsedTypeExpr::Struct(struct_defn) => {
                let struct_defn = *struct_defn;
                let kind = match struct_defn.record_kind {
                    parse::ParsedRecordKind::Struct => RecordKind::Struct,
                    parse::ParsedRecordKind::Union => RecordKind::Union,
                    parse::ParsedRecordKind::Packed => RecordKind::Packed,
                };
                let mut fields = self.tmp.new_list(struct_defn.fields.len());
                for ast_field in self.ast.mem.getn(struct_defn.fields) {
                    let ty = self.eval_type_expr_ext(
                        ast_field.type_expr,
                        scope_id,
                        context.descended(),
                    )?;
                    fields.push(StructTypeField {
                        name: ast_field.name,
                        type_id: ty,
                        span: ast_field.name_span,
                    })
                }
                self.add_struct_type(fields.as_slice(), kind)
            }
            ParsedTypeExpr::TypeApplication(ty_app) => {
                let ty_app = *ty_app;
                let type_op_result =
                    self.detect_and_eval_type_operator(type_expr_id, scope_id, context)?;
                match type_op_result {
                    None => self.eval_type_application(ty_app, scope_id, context),
                    Some(type_op_result) => Ok(type_op_result),
                }
            }
            ParsedTypeExpr::Optional(opt) => {
                // Rewrite the sugar and compile the parsed
                let opt_app = parse::TypeApplication {
                    span: opt.span,
                    name: QIdent::naked(self.ast.idents.b.opt, opt.span),
                    args: self.ast.mem.pushn(&[NamedTypeArg {
                        name: None,
                        type_expr: Some(opt.base),
                        span: opt.span,
                    }]),
                };
                self.eval_type_application(opt_app, scope_id, EvalTypeExprContext::EMPTY)
            }
            ParsedTypeExpr::Reference(r) => {
                let inner_ty = self.eval_type_expr_ext(r.base, scope_id, context.descended())?;
                Ok(self.add_reference_type(inner_ty))
            }
            ParsedTypeExpr::Sum(sum) => {
                let sum = *sum;
                let explicit_tag_type = match sum.tag_type {
                    None => None,
                    Some(tag_type_expr) => Some((
                        self.eval_type_expr(tag_type_expr, scope_id)?,
                        self.ast.get_type_expr_span(tag_type_expr),
                    )),
                };
                let tag_type =
                    self.choose_either_tag_type(explicit_tag_type, sum.variants.len(), sum.span)?;
                let mut variants = self.tmp.new_list(sum.variants.len());
                for v in self.ast.mem.getn(sum.variants) {
                    let payload = match v.payload {
                        None => None,
                        Some(payload_type_expr) => Some(self.eval_type_expr_ext(
                            payload_type_expr,
                            scope_id,
                            context.descended(),
                        )?),
                    };
                    let tag = match v.explicit_value {
                        None => None,
                        Some(explicit_value) => Some(self.eval_integer_value(
                            explicit_value,
                            v.name_span,
                            Some(tag_type.type_id()),
                        )?),
                    };
                    variants.push(EitherVariantPart {
                        name: v.tag_name,
                        name_span: v.name_span,
                        payload,
                        tag,
                    });
                }
                self.add_either_type(tag_type, variants.as_slice(), sum.partial, sum.span)
            }
            ParsedTypeExpr::MemberAccess(acc) => {
                let is_dot = matches!(acc.member_kind, parse::TypeMemberAccessKind::Dot);
                let acc = *acc;
                let base_type = self.eval_type_expr_ext(acc.base, scope_id, context.descended())?;
                if let Some(spec_info) = self.get_instance_info(base_type) {
                    let generic = self.types.get(spec_info.generic_parent).expect_generic();
                    if let Some(matching_type_var_pos) = self
                        .mem
                        .getn(generic.params)
                        .iter()
                        .position(|tp| self.get_type_parameter(*tp).name == acc.member_name)
                    {
                        let type_arg_type_id =
                            self.get_type_slice(spec_info.type_args)[matching_type_var_pos];
                        return Ok(type_arg_type_id);
                    }
                }
                match self.types.get(base_type) {
                    // You can do dot access on sums to get their variant payloads,
                    // and .tag-enum for the scalar enum of just the tags
                    Type::Sum(sum) => {
                        let sum = *sum;
                        match self.sum_variant_by_name(sum.variants, acc.member_name) {
                            Some(matching_variant) => {
                                let Some(payload) = matching_variant.payload else {
                                    kbail!(
                                        self,
                                        acc.span,
                                        "Variant '{}' has no payload type",
                                        acc.member_name
                                    );
                                };
                                if is_dot {
                                    kbail!(
                                        self,
                                        acc.span,
                                        "Use :, not ., to access this variant's data type"
                                    );
                                }
                                Ok(payload)
                            }
                            None => {
                                if is_dot && acc.member_name == self.ast.idents.b.tag_enum {
                                    let mut members = self.tmp.new_list(sum.variants.len());
                                    for variant in self.mem.getn(sum.variants) {
                                        members.push(EitherVariantPart {
                                            name: variant.name,
                                            name_span: variant.name_span,
                                            payload: None,
                                            tag: Some(variant.tag_value),
                                        })
                                    }
                                    self.add_either_type(
                                        sum.tag_type,
                                        members.as_slice(),
                                        false,
                                        acc.span,
                                    )
                                } else {
                                    kbail!(
                                        self,
                                        acc.span,
                                        "Variant '{}' does not exist on either '{}'",
                                        acc.member_name,
                                        base_type
                                    );
                                }
                            }
                        }
                    }
                    // You can do dot access on structs to get their members!
                    Type::Struct(s) => {
                        let Some(field) = s.find_field(&self.mem, acc.member_name) else {
                            kbail!(
                                self,
                                acc.span,
                                "Field {} does not exist on struct {}",
                                acc.member_name,
                                base_type
                            );
                        };
                        if !is_dot {
                            kbail!(
                                self,
                                acc.span,
                                "Use ., not :, to access this struct member's data type"
                            );
                        }
                        Ok(field.1.type_id)
                    }
                    // You can do dot access on References to get out their 'value' types
                    Type::Reference(r) => {
                        if acc.member_name != self.ast.idents.b.value {
                            return make_fail_ast_id(
                                &self.ast,
                                "Invalid member access on Optional type; try '.value'",
                                type_expr_id.into(),
                            );
                        }
                        Ok(r.inner_type)
                    }
                    Type::Function(_) | Type::FunctionIdReference(_) => {
                        let (function_type_id, declaration) = match self.types.get(base_type) {
                            Type::FunctionIdReference(fr) => {
                                (fr.function_type, Some(fr.function_id))
                            }
                            _ => (base_type, None),
                        };
                        let fun = self.types.get(function_type_id).as_function().unwrap();
                        if acc.member_name == self.ast.idents.b.return_ {
                            return Ok(fun.return_type);
                        }
                        for (index, param) in self.mem.getn(fun.logical_params()).iter().enumerate()
                        {
                            if self.get_param_name(declaration, index) == acc.member_name {
                                return Ok(param.type_id);
                            }
                        }
                        make_fail_ast_id(
                            &self.ast,
                            &format!(
                                "Function has no parameter named {}",
                                self.ast.idents.get_string(acc.member_name)
                            ),
                            type_expr_id.into(),
                        )
                    }
                    Type::Array(array_type) => {
                        let member_name = self.ast.idents.get_string(acc.member_name);
                        match member_name {
                            "element" => Ok(array_type.element_type),
                            _ => {
                                kbail!(
                                    self,
                                    acc.span,
                                    "Array type has no member named {}; try '.element'",
                                    member_name
                                );
                            }
                        }
                    }
                    _ => {
                        kbail!(
                            self,
                            acc.span,
                            "Type '{}' has no {} member named {}",
                            base_type,
                            if is_dot { "." } else { ":" },
                            self.ast.idents.get_string(acc.member_name)
                        );
                    }
                }
            }
            ParsedTypeExpr::Function(fun_type) => {
                let fun_type = *fun_type;
                let mut params: TmpList<FnParamType> = self.tmp.new_list(fun_type.params.len());

                for param in self.ast.mem.getn(fun_type.params) {
                    let type_id = self.eval_type_expr(*param, scope_id)?;
                    params.push(FnParamType { type_id, kind: FnParamKind::Plain });
                }
                let return_type = self.eval_type_expr(fun_type.return_type, scope_id)?;
                Ok(self.add_function_type(params.as_slice(), return_type, false))
            }
            ParsedTypeExpr::TypeOf(tof) => {
                let expr = self.eval_expr(tof.target_expr, EvalExprContext::make(scope_id))?;
                let ty = self.exprs.get_type(expr);
                Ok(ty)
            }
            ParsedTypeExpr::SomeQuant(quant) => {
                if !context.is_direct_function_parameter {
                    kbail!(
                        self,
                        quant.span,
                        "some quantifier is only allowed in function parameters"
                    );
                }
                let span = quant.span;
                let inner = self.eval_type_expr(quant.inner_type_expr, scope_id)?;
                let Type::Function(_function_type) = self.types.get(inner) else {
                    kbail!(self, span, "Expected a function type following 'some'");
                };
                let name = self.ast.idents.intern(format!("some_fn_{}", type_expr_id));
                let function_type_variable =
                    self.add_function_type_parameter(FunctionTypeParameter {
                        name,
                        scope_id,
                        span,
                        function_type: inner,
                    });
                Ok(function_type_variable)
            }
            ParsedTypeExpr::Static(parsed_static) => {
                if context.is_inside_static_type {
                    kbail!(
                        self,
                        parsed_static.span,
                        "Static type cannot appear inside static type"
                    );
                }
                if context.is_inside_type_definition_rhs {
                    kbail!(
                        self,
                        parsed_static.span,
                        "Static type cannot appear in type definitions"
                    );
                }
                let parsed_static = *parsed_static;
                let inner_type_id = self.eval_type_expr_ext(
                    parsed_static.family_type_expr,
                    scope_id,
                    EvalTypeExprContext { is_inside_static_type: true, ..context },
                )?;
                if let Type::StaticValue(_) = self.types.get(inner_type_id) {
                    kbail!(
                        self,
                        parsed_static.span,
                        "Value type cannot be nested inside another value type"
                    );
                };
                let value_type = StaticValueType { family_type_id: inner_type_id, value_id: None };
                let static_type_id = self.add_type(Type::StaticValue(value_type));
                Ok(static_type_id)
            }
            ParsedTypeExpr::StaticLiteral(parsed_literal, span) => {
                let (static_value_id, inner_type_id) =
                    self.literal_to_static_value_and_type(*parsed_literal, *span)?;
                let static_type_id = self.add_value_type(inner_type_id, Some(static_value_id));
                Ok(static_type_id)
            }
            ParsedTypeExpr::ExecStatic(es) => {
                let es = *es;
                let expr_ctx = EvalExprContext::make(scope_id).with_is_static(true);
                let resulting_value_id = self.execute_static_expr(es.body_expr, expr_ctx)?;
                let resulting_type_id = self.get_static_value_type(resulting_value_id);
                match self.types.get(resulting_type_id) {
                    Type::Struct(_) if resulting_type_id == self.builtin_types.type_id() => {
                        match self.get_type_id_from_value(self.static_values.get(resulting_value_id))
                        {
                            Some(type_id) => Ok(type_id),
                            None => kbail!(self, es.span, "Unknown type id"),
                        }
                    }
                    Type::StaticValue(_svt) => {
                        // They returned an actual statically-typed value already
                        Ok(resulting_type_id)
                    }
                    _ => {
                        // For any other type, we take the type of their interned/baked/static
                        // value. For example, the type of 42u8 itself
                        let static_type_id = self.add_type(Type::StaticValue(StaticValueType {
                            family_type_id: resulting_type_id,
                            value_id: Some(resulting_value_id),
                        }));
                        Ok(static_type_id)
                    }
                }
            }
        }?;
        Ok(base)
    }

    pub(super) fn literal_to_static_value_and_type(
        &mut self,
        parsed_literal: ParsedLiteral,
        span: Span,
    ) -> K1Result<(StaticValueId, TypeId)> {
        match parsed_literal {
            ParsedLiteral::Char(byte) => {
                Ok((self.static_values.add(StaticValue::Char(byte)), CHAR_TYPE_ID))
            }
            ParsedLiteral::Bool(b) => Ok((StaticValuePool::get_bool_id(b), BOOL_TYPE_ID)),
            ParsedLiteral::String(s) => {
                Ok((self.static_values.add(StaticValue::String(s)), self.builtin_types.string()))
            }
            ParsedLiteral::Int(int_literal) => {
                let int_value = self.eval_integer_value(int_literal, span, None)?;
                let type_id = int_value.get_type();
                Ok((self.static_values.add(StaticValue::Int(int_value)), type_id))
            }
            ParsedLiteral::Float { text_span } => {
                let float_value = self.eval_float_value(text_span, None)?;
                let type_id = float_value.get_type();
                Ok((self.static_values.add(StaticValue::Float(float_value)), type_id))
            }
        }
    }

    /// Temporary home for our type operators until I decide on syntax
    pub(super) fn detect_and_eval_type_operator(
        &mut self,
        ty_app_id: ParsedTypeExprId,
        scope_id: ScopeId,
        context: EvalTypeExprContext,
    ) -> K1Result<Option<TypeId>> {
        let ParsedTypeExpr::TypeApplication(ty_app) = self.ast.type_exprs.get(ty_app_id) else {
            panic_at_disco!("Expected TypeApplication")
        };
        if ty_app.name.has_path() {
            return Ok(None);
        }
        let ty_app = *ty_app;
        match self.ident_str(ty_app.name.name) {
            "dyn" => {
                if ty_app.args.len() != 1 {
                    kbail!(self, ty_app.span, "Expected 1 type parameter for dyn");
                }
                let Some(inner_type_expr_id) = self.ast.mem.get_nth(ty_app.args, 0).type_expr
                else {
                    kbail!(self, ty_app.span, "Wildcard type `_` not accepted here");
                };
                // dyn over an ability: the inner expr names an ability, which is not
                // a type, so it must be intercepted before type evaluation
                if let Some(object_type) =
                    self.try_eval_dyn_ability_type(inner_type_expr_id, scope_id)?
                {
                    return Ok(Some(object_type));
                }
                let inner =
                    self.eval_type_expr_ext(inner_type_expr_id, scope_id, context.descended())?;
                let Some(&inner_fn) = self.types.get(inner).as_function() else {
                    kbail!(
                        self,
                        ty_app.span,
                        "Expected function type or ability signature for dyn"
                    );
                };
                let new_function_type = self.add_function_type(
                    self.mem.getn(inner_fn.logical_params()),
                    inner_fn.return_type,
                    true,
                );
                let lambda_object_type =
                    self.add_lambda_object(new_function_type, ty_app_id.into());

                Ok(Some(lambda_object_type))
            }
            "_struct_combine" => {
                if ty_app.args.len() != 2 {
                    kbail!(self, ty_app.span, "Expected 2 type parameters for _struct_combine");
                }
                let args = self.ast.mem.getn(ty_app.args);
                let Some(arg1_expr) = args[0].type_expr else {
                    kbail!(self, ty_app.span, "Wildcard type `_` not accepted here");
                };
                let Some(arg2_expr) = args[1].type_expr else {
                    kbail!(self, ty_app.span, "Wildcard type `_` not accepted here");
                };
                let arg1 = self.eval_type_expr_ext(arg1_expr, scope_id, context.descended())?;
                let arg2 = self.eval_type_expr_ext(arg2_expr, scope_id, context.descended())?;

                let struct1 = *self
                    .types
                    .get(arg1)
                    .as_struct()
                    .ok_or_else(|| kerr!(self, ty_app.span, "Expected struct"))?;
                let struct2 = *self
                    .types
                    .get(arg2)
                    .as_struct()
                    .ok_or_else(|| kerr!(self, ty_app.span, "Expected struct"))?;
                let record_kind = struct1.record_kind;
                if struct2.record_kind != record_kind {
                    kbail!(
                        self,
                        ty_app.span,
                        "Cannot combine a {} and a {}",
                        struct1.record_kind.kind_name(),
                        struct2.record_kind.kind_name(),
                    );
                }

                let mut combined_fields =
                    self.tmp.new_list(struct1.fields.len() + struct2.fields.len());
                combined_fields.extend(self.mem.getn(struct1.fields));
                for field in self.mem.getn(struct2.fields).iter() {
                    let collision = combined_fields.iter().find(|f| f.name == field.name).copied();
                    match collision {
                        None => combined_fields.push(*field),
                        Some(collision) if collision.type_id == field.type_id => {}
                        Some(_) => {
                            kbail!(
                                self,
                                ty_app.span,
                                "Field '{}' has conflicting types in the two structs",
                                self.ident_str(field.name).blue()
                            );
                        }
                    }
                }
                Ok(Some(self.add_struct_type(combined_fields.as_slice(), record_kind)?))
            }
            "_struct_remove" => {
                if ty_app.args.len() != 2 {
                    kbail!(self, ty_app.span, "Expected 2 type parameters for _struct_remove");
                }
                let args = self.ast.mem.getn(ty_app.args);
                let Some(arg1_expr) = args[0].type_expr else {
                    kbail!(self, ty_app.span, "Wildcard type `_` not accepted here");
                };
                let Some(arg2_expr) = args[1].type_expr else {
                    kbail!(self, ty_app.span, "Wildcard type `_` not accepted here");
                };
                let arg1 = self.eval_type_expr_ext(arg1_expr, scope_id, context.descended())?;
                let arg2 = self.eval_type_expr_ext(arg2_expr, scope_id, context.descended())?;

                let struct1 = self
                    .types
                    .get(arg1)
                    .as_struct()
                    .ok_or_else(|| kerr!(self, ty_app.span, "Expected struct"))?;
                let struct2 = self
                    .types
                    .get(arg2)
                    .as_struct()
                    .ok_or_else(|| kerr!(self, ty_app.span, "Expected struct"))?;
                let record_kind = struct1.record_kind;
                if struct2.record_kind != record_kind {
                    kbail!(
                        self,
                        ty_app.span,
                        "Cannot combine a {} and a {}",
                        struct1.record_kind.kind_name(),
                        struct2.record_kind.kind_name(),
                    );
                }
                let struct2_fields = self.mem.getn(struct2.fields);
                let mut new_fields = self.tmp.new_list(struct1.fields.len());
                for field in self.mem.getn(struct1.fields) {
                    if !struct2_fields.iter().any(|f| f.name == field.name) {
                        new_fields.push(*field);
                    }
                }
                Ok(Some(self.add_struct_type(new_fields.as_slice(), record_kind)?))
            }
            _ => Ok(None),
        }
    }

    pub(super) fn eval_type_application(
        &mut self,
        ty_app: parse::TypeApplication,
        scope_id: ScopeId,
        context: EvalTypeExprContext,
    ) -> K1Result<TypeId> {
        if self.string_is_completion_marker(ty_app.name.name, false) {
            self.record_qident_completion_site(scope_id, &ty_app.name);
            return Ok(NEVER_TYPE_ID);
        }

        match self.find_type_namespaced(scope_id, &ty_app.name)? {
            Some((type_id, _)) => match self.types.get(type_id) {
                Type::Generic(g) => {
                    let g_params = g.params;
                    if ty_app.args.is_empty() {
                        self.emit_ls_entity(
                            ty_app.name.name_span,
                            LsEntityKind::Type { type_id, applied_type_id: None },
                        );
                        return Ok(type_id);
                    }
                    let type_arguments_slice = self.check_type_args_against_params(
                        g_params,
                        self.ast.mem.getn(ty_app.args),
                        scope_id,
                        context,
                        ty_app.span,
                    )?;
                    let instantiated_type_id =
                        self.instantiate_generic_type(type_id, self.tmp.getn(type_arguments_slice));

                    self.emit_ls_entity(
                        ty_app.name.name_span,
                        LsEntityKind::Type { type_id, applied_type_id: Some(instantiated_type_id) },
                    );
                    Ok(instantiated_type_id)
                }
                _other => {
                    if !ty_app.args.is_empty() {
                        kbail!(
                            self,
                            ty_app.span,
                            "Type {} is not generic, but got {} type arguments",
                            &ty_app.name,
                            ty_app.args.len()
                        );
                    }
                    let resolved_type_id = self.get_type_id_resolved(type_id, scope_id);
                    self.emit_ls_entity(
                        ty_app.name.name_span,
                        LsEntityKind::Type { type_id, applied_type_id: None },
                    );
                    Ok(resolved_type_id)
                }
            },
            None => match self.find_pending_type_namespaced(scope_id, &ty_app.name)? {
                None => {
                    if ty_app.args.is_empty() {
                        match self.find_function_namespaced(scope_id, &ty_app.name)? {
                            Some(function_id) => {
                                let function_type = self.get_function(function_id).type_id;
                                Ok(self.add_function_id_reference_type(function_id, function_type))
                            }
                            None => match self
                                .resolve_qident_to_constant_type(scope_id, &ty_app.name)?
                            {
                                Some(static_type_id) => Ok(static_type_id),
                                None => Err(kerr!(
                                    self,
                                    ty_app.name.name_span,
                                    "Type '{}' not found",
                                    &ty_app.name,
                                )),
                            },
                        }
                    } else {
                        if let Some(opaque_type_id) = self.handle_opaque_tyapp(&ty_app)? {
                            Ok(opaque_type_id)
                        } else if let Some(vector_type_id) =
                            self.handle_vector_tyapp(&ty_app, scope_id, context)?
                        {
                            Ok(vector_type_id)
                        } else if let Some(array_type_id) =
                            self.handle_array_tyapp(&ty_app, scope_id, context)?
                        {
                            Ok(array_type_id)
                        } else {
                            Err(kerr!(
                                self,
                                ty_app.name.name_span,
                                "Type '{}' not found",
                                &ty_app.name
                            ))
                        }
                    }
                }
                Some((pending_parsed_id, pending_scope_id)) => {
                    let stack_entry = self
                        .type_defn_context
                        .stack
                        .iter()
                        .find(|e| e.parsed_id == pending_parsed_id)
                        .map(|e| e.reserved_type_id);
                    if let Some(reserved_entry) = stack_entry {
                        let type_params = self
                            .ast
                            .mem
                            .getn(self.ast.get_type_defn(pending_parsed_id).type_params);
                        let ordered_args = self.bind_type_args_to_params(
                            type_params.len(),
                            |_, p| type_params[p].name,
                            self.ast.mem.getn(ty_app.args),
                            ty_app.span,
                        )?;
                        let Some(reserved_type_id) = reserved_entry else {
                            // An in-progress alias is transparent, so this reference is just
                            // its rhs; nominal members of the cycle short-circuit to their
                            // reserved ids above, so expansion terminates unless the alias
                            // cycle contains no nominal type at all
                            if self.type_defn_context.expanding_aliases.contains(&pending_parsed_id)
                            {
                                kbail!(
                                    self,
                                    ty_app.span,
                                    "Type alias cycle: '{}' refers back to itself without passing through a struct or either",
                                    &ty_app.name
                                );
                            }
                            let alias_rhs = self.ast.get_type_defn(pending_parsed_id).value_expr;
                            self.type_defn_context.expanding_aliases.push(pending_parsed_id);
                            let rhs_result = self.eval_type_expr_ext(
                                alias_rhs,
                                pending_scope_id,
                                EvalTypeExprContext::EMPTY,
                            );
                            self.type_defn_context.expanding_aliases.pop();
                            let type_id = rhs_result?;
                            self.emit_ls_entity(
                                ty_app.name.name_span,
                                LsEntityKind::Type { type_id, applied_type_id: None },
                            );
                            return Ok(type_id);
                        };
                        if ty_app.args.is_empty() {
                            self.type_defn_context.has_recursive_mention = true;
                            self.emit_ls_entity(
                                ty_app.name.name_span,
                                LsEntityKind::Type {
                                    type_id: reserved_type_id,
                                    applied_type_id: None,
                                },
                            );
                            Ok(reserved_type_id)
                        } else {
                            // A recursive application like the `node[t]` inside node's own
                            // definition: the generic doesn't exist yet, so reserve the
                            // instance's id now; it is filled once the defn cluster completes
                            let mut type_arguments: SV4<TypeId> = smallvec![];
                            for parsed_arg in self.tmp.getn(ordered_args) {
                                let Some(parsed_arg_expr) = parsed_arg.type_expr else {
                                    kbail!(
                                        self,
                                        parsed_arg.span,
                                        "Wildcard _ type not accepted here"
                                    );
                                };
                                let arg_type_id = self.eval_type_expr_ext(
                                    parsed_arg_expr,
                                    scope_id,
                                    context.descended(),
                                )?;
                                if self.recursive_arg_violates_uniformity(arg_type_id) {
                                    kbail!(
                                        self,
                                        parsed_arg.span,
                                        "Polymorphic recursion is not supported: a recursive type argument must be a bare type parameter or contain no type parameters, got {}",
                                        arg_type_id
                                    );
                                }
                                type_arguments.push(arg_type_id);
                            }
                            let instance_type_id = self.get_or_reserve_recursive_instance(
                                reserved_type_id,
                                &type_arguments,
                                Some((ty_app.span, scope_id)),
                            );
                            self.emit_ls_entity(
                                ty_app.name.name_span,
                                LsEntityKind::Type {
                                    type_id: reserved_type_id,
                                    applied_type_id: Some(instance_type_id),
                                },
                            );
                            Ok(instance_type_id)
                        }
                    } else {
                        debug!(
                            "Evaluating {} inside {} on demand",
                            self.ident_str(self.ast.get_type_defn(pending_parsed_id).name),
                            self.scope_id_to_string(scope_id)
                        );

                        let _result = self.eval_type_defn(pending_parsed_id, pending_scope_id)?;
                        self.eval_type_application(ty_app, scope_id, context)
                    }
                }
            },
        }
    }

    pub(super) fn instantiate_generic_type(
        &mut self,
        generic_type: TypeId,
        type_arguments: &[TypeId],
    ) -> TypeId {
        match self.get_specialization(generic_type, type_arguments) {
            Some(existing) => existing,
            None => {
                let args = self.intern_type_slice(type_arguments);
                self.traced(
                    super::trace::TraceKind::TypeInstantiate,
                    generic_type.as_u32(),
                    0,
                    |k1| k1.instantiate_generic_type_miss(generic_type, args),
                )
            }
        }
    }

    pub(super) fn instantiate_generic_type_miss(
        &mut self,
        generic_type: TypeId,
        type_arguments: TypeSliceId,
    ) -> TypeId {
        let result_type = self.reserve_instance_id(generic_type, type_arguments);
        self.build_generic_instance(generic_type, type_arguments, result_type);
        if log::log_enabled!(log::Level::Debug) {
            let inner = self.types.get(generic_type).expect_generic().inner;
            eprintln!(
                "instantiated\n{} with params\n{} got expanded type:\n{}\n\n",
                self.type_id_to_string_ext(inner, dump::TypeDisplayMode::Expand),
                self.pretty_print_types(self.get_type_slice(type_arguments), ", "),
                self.type_id_to_string_ext(result_type, dump::TypeDisplayMode::Expand)
            );
        }
        result_type
    }

    fn build_generic_instance(
        &mut self,
        generic_type: TypeId,
        type_arguments: TypeSliceId,
        reserved: TypeId,
    ) {
        let gen_type = self.types.get(generic_type).expect_generic();
        debug_assert!(gen_type.params.len() as usize == self.get_type_slice(type_arguments).len());
        let defn_info = self.get_defn_info(generic_type).unwrap();
        let substitution_pairs: SV8<TypeSubstitutionPair> = self.zip_types_to_subst_pairs(
            self.mem.getn(gen_type.params),
            self.get_type_slice(type_arguments),
        );
        let instance_type = match *self.types.get(gen_type.inner) {
            Type::Struct(struc) => {
                let mut fields = self.mem.new_list(struc.fields.len());
                for field in self.mem.getn(struc.fields) {
                    let mut new_field = *field;
                    new_field.type_id = self.substitute_in_type(field.type_id, &substitution_pairs);
                    fields.push(new_field);
                }
                Type::Struct(StructType {
                    fields: fields.to_slice(),
                    record_kind: struc.record_kind,
                })
            }
            Type::Sum(sum) => {
                let mut variants = self.mem.new_list(sum.variants.len());
                for variant in self.mem.getn(sum.variants) {
                    let mut new_variant = *variant;
                    if let Some(payload) = variant.payload {
                        new_variant.payload =
                            Some(self.substitute_in_type(payload, &substitution_pairs));
                    }
                    variants.push(new_variant);
                }
                Type::Sum(SumType { variants: variants.to_slice(), tag_type: sum.tag_type })
            }
            other => other,
        };
        self.set_type(reserved, instance_type, Some(defn_info));
    }

    /// A recursive application's args must be bare type parameters or contain no type
    /// parameters at all; anything else is polymorphic recursion, whose instantiations
    /// would never terminate
    pub(super) fn recursive_arg_violates_uniformity(&self, arg: TypeId) -> bool {
        self.type_variable_counts.get(arg).type_parameter_count > 0
            && !matches!(self.types.get(arg), Type::TypeParameter(_))
    }

    pub(super) fn get_or_reserve_recursive_instance(
        &mut self,
        generic_parent: TypeId,
        type_args: &[TypeId],
        written_at: Option<(Span, ScopeId)>,
    ) -> TypeId {
        if let Some(existing) = self.get_specialization(generic_parent, type_args) {
            return existing;
        }
        let args = self.intern_type_slice(type_args);
        let type_id = self.reserve_instance_id(generic_parent, args);
        self.type_defn_context.pending_instances.push(PendingRecursiveInstance {
            type_id,
            generic_parent,
            type_args: args,
            written_at,
        });
        self.type_defn_context.has_recursive_mention = true;
        type_id
    }

    pub(super) fn finish_type_defn_cluster(&mut self) {
        let pending = std::mem::take(&mut self.type_defn_context.pending_instances);
        for p in &pending {
            let defn_info = self.get_defn_info(p.generic_parent).unwrap();
            let defn_span = self.ast.get_span_for_id(defn_info.ast_id);
            for arg in self.get_type_slice(p.type_args) {
                if self.recursive_arg_violates_uniformity(*arg) {
                    self.report(kerr!(self,
                        defn_span,
                        "Polymorphic recursion is not supported: a recursive type argument must be a bare type parameter or contain no type parameters, got {}",
                        *arg));
                }
            }
            self.build_generic_instance(p.generic_parent, p.type_args, p.type_id);
        }
        if self.type_defn_context.has_recursive_mention {
            let completed = std::mem::take(&mut self.type_defn_context.completed);
            // Outermost defn first; one cycle report is enough for the whole cluster
            let mut seen: List<TypeId, MemTmp> = self.tmp.new_list(16);
            for (type_id, span) in completed.iter().rev() {
                debug_assert!(seen.is_empty());
                if let Some(cycled_type_id) = self.check_type_finite_rec(*type_id, false, &mut seen)
                {
                    self.report(kerr!(self,
                        *span,
                        "This type has an infinite size due to a cycle; {} was mentioned inside {} with no indirection",
                        cycled_type_id,
                        *type_id,
                    ));
                    break;
                }
            }
        }
        self.type_defn_context.reset();
        for p in &pending {
            let Some((span, scope_id)) = p.written_at else { continue };
            let gen_params = self.types.get(p.generic_parent).expect_generic().params;
            let subst_pairs: SV8<TypeSubstitutionPair> = self.zip_types_to_subst_pairs(
                self.mem.getn(gen_params),
                self.get_type_slice(p.type_args),
            );
            if let Err(e) = self.check_type_args_constraints(&subst_pairs, span, scope_id) {
                self.report(e);
            }
        }
    }

    pub(super) fn place_args<A: Copy>(
        &self,
        placed: &mut [Option<A>],
        args_len: usize,
        get_arg: impl Fn(usize) -> (Option<StringId>, A),
        get_arg_span: impl Fn(A) -> Span,
        get_param_name: impl Fn(usize) -> StringId,
        span: Span,
        arg_kind: &str,
    ) -> K1Result<()> {
        for arg_index in 0..args_len {
            let (arg_name, arg) = get_arg(arg_index);
            let param_index = match arg_name {
                None if arg_index < placed.len() => arg_index,
                None => kbail!(
                    self,
                    span,
                    "Expected {} {}arguments, got {}",
                    placed.len(),
                    arg_kind,
                    args_len
                ),
                Some(name) => match (0..placed.len()).find(|p| get_param_name(*p) == name) {
                    Some(param_index) => param_index,
                    None => {
                        kbail!(self, get_arg_span(arg), "No {}parameter named {}", arg_kind, name)
                    }
                },
            };
            if placed[param_index].is_some() {
                kbail!(
                    self,
                    get_arg_span(arg),
                    "Duplicate argument for {}parameter {}",
                    arg_kind,
                    get_param_name(param_index)
                );
            }
            placed[param_index] = Some(arg);
        }
        Ok(())
    }

    pub(super) fn bind_type_args_to_params(
        &mut self,
        params_len: usize,
        get_param_name: impl Fn(&Self, usize) -> StringId,
        parsed_args: &[NamedTypeArg],
        span: Span,
    ) -> K1Result<TmpSlice<NamedTypeArg>> {
        let mut placed: SV8<Option<NamedTypeArg>> = smallvec![None; params_len];
        self.place_args(
            &mut placed,
            parsed_args.len(),
            |i| (parsed_args[i].name, parsed_args[i]),
            |arg| arg.span,
            |p| get_param_name(self, p),
            span,
            "type ",
        )?;
        let mut ordered_args: TmpList<NamedTypeArg> = self.tmp.new_list(params_len as u32);
        for placed_arg in placed {
            let Some(parsed_arg) = placed_arg else {
                kbail!(
                    self,
                    span,
                    "Expected {} type arguments, got {}",
                    params_len,
                    parsed_args.len()
                );
            };
            ordered_args.push(parsed_arg);
        }
        Ok(ordered_args.to_slice())
    }

    pub(super) fn eval_call_type_args(
        &mut self,
        type_params: &[TypeId],
        parsed_args: &[NamedTypeArg],
        span: Span,
        scope_id: ScopeId,
    ) -> K1Result<TmpSlice<CallTypeArg>> {
        if parsed_args.is_empty() && type_params.is_empty() {
            return Ok(TmpSlice::empty());
        }
        let mut placed: SV8<Option<NamedTypeArg>> = smallvec![None; type_params.len()];
        self.place_args(
            &mut placed,
            parsed_args.len(),
            |i| (parsed_args[i].name, parsed_args[i]),
            |arg| arg.span,
            |p| self.get_type_parameter(type_params[p]).name,
            span,
            "type ",
        )?;
        let infers_unplaced = parsed_args.iter().all(|arg| arg.name.is_some());
        let mut call_type_args: TmpList<CallTypeArg> = self.tmp.new_list(type_params.len() as u32);
        for placed_arg in placed {
            call_type_args.push(match placed_arg {
                None if infers_unplaced => CallTypeArg::Inferred,
                None => kbail!(
                    self,
                    span,
                    "Expected {} type arguments, got {}",
                    type_params.len(),
                    parsed_args.len()
                ),
                Some(NamedTypeArg { type_expr: None, .. }) => CallTypeArg::Inferred,
                Some(NamedTypeArg { type_expr: Some(type_expr), span, .. }) => {
                    CallTypeArg::Passed { type_id: self.eval_type_expr(type_expr, scope_id)?, span }
                }
            });
        }
        Ok(call_type_args.to_slice())
    }

    pub(super) fn check_type_args_against_params(
        &mut self,
        params: TypeIdSlice,
        parsed_args: &[NamedTypeArg],
        scope_id: ScopeId,
        context: EvalTypeExprContext,
        span: Span,
    ) -> K1Result<TmpSlice<TypeId>> {
        let type_params = self.mem.getn(params);
        let ordered_args = self.bind_type_args_to_params(
            type_params.len(),
            |k1, p| k1.get_type_parameter(type_params[p]).name,
            parsed_args,
            span,
        )?;
        let ordered_args = self.tmp.getn(ordered_args);
        let mut type_arguments: TmpList<TypeId> = self.tmp.new_list(params.len());
        for parsed_arg in ordered_args {
            let Some(parsed_arg_expr) = parsed_arg.type_expr else {
                kbail!(self, parsed_arg.span, "Wildcard _ type not accepted here");
            };
            let arg_type_id =
                self.eval_type_expr_ext(parsed_arg_expr, scope_id, context.descended())?;
            type_arguments.push(arg_type_id);
        }
        let type_arguments = type_arguments.to_slice();
        let subst_pairs: SV4<TypeSubstitutionPair> =
            self.zip_types_to_subst_pairs(type_params, self.tmp.getn(type_arguments));
        for (pair, parsed_arg) in subst_pairs.iter().zip(ordered_args) {
            self.check_type_constraints(
                pair.from,
                pair.to,
                &subst_pairs,
                scope_id,
                parsed_arg.span,
            )?;
        }
        Ok(type_arguments)
    }

    pub(super) fn check_type_args_constraints(
        &mut self,
        subst_pairs: &[TypeSubstitutionPair],
        span: Span,
        scope_id: ScopeId,
    ) -> K1Result<()> {
        for pair in subst_pairs {
            self.check_type_constraints(pair.from, pair.to, subst_pairs, scope_id, span)?;
        }
        Ok(())
    }

    pub(super) fn resolve_qident_to_constant_type(
        &mut self,
        scope_id: ScopeId,
        name: &QIdent,
    ) -> K1Result<Option<TypeId>> {
        let global_id = match self.find_variable_namespaced(scope_id, name)? {
            Some((variable_id, _scope)) => match self.variables.get(variable_id).global_id() {
                Some(global_id) => global_id,
                None => return Ok(None),
            },
            None => match self.find_pending_global_namespaced(scope_id, name)? {
                Some((parsed_id, defn_scope)) => {
                    match self.declare_global(parsed_id, defn_scope)? {
                        None => return Ok(None),
                        Some(global_id) => global_id,
                    }
                }
                None => return Ok(None),
            },
        };
        if self.globals.get(global_id).initial_value.is_pending() {
            self.eval_global_body(global_id)?;
        }
        let g = self.globals.get(global_id);
        if !g.is_constant {
            return Ok(None);
        }
        let Some(static_value_id) = g.initial_value.as_value() else {
            return Ok(None);
        };
        let inner_type_id = self.get_static_value_type(static_value_id);
        Ok(Some(self.add_value_type(inner_type_id, Some(static_value_id))))
    }

    pub(super) fn handle_opaque_tyapp(
        &mut self,
        ty_app: &parse::TypeApplication,
    ) -> K1Result<Option<TypeId>> {
        if self.ident_str(ty_app.name.name) != "opaque" {
            return Ok(None);
        }
        if ty_app.name.has_path() {
            kbail!(
                self,
                ty_app.span,
                "Expected 'opaque' with no namespace, got '{}'",
                &ty_app.name
            );
        }
        if ty_app.args.len() != 2 {
            kbail!(
                self,
                ty_app.span,
                "Expected 2 type parameters for opaque, got {}",
                ty_app.args.len()
            );
        }
        let Some(size_expr) = self.ast.mem.get_nth(ty_app.args, 0).type_expr else {
            kbail!(self, ty_app.span, "Wildcard _ type not accepted here");
        };
        let Some(align_expr) = self.ast.mem.get_nth(ty_app.args, 1).type_expr else {
            kbail!(self, ty_app.span, "Wildcard _ type not accepted here");
        };
        let ParsedTypeExpr::StaticLiteral(ParsedLiteral::Int(size_literal), size_span) =
            *self.ast.type_exprs.get(size_expr)
        else {
            kbail!(self, ty_app.span, "Expected an integer literal for opaque size");
        };
        let ParsedTypeExpr::StaticLiteral(ParsedLiteral::Int(align_literal), align_span) =
            *self.ast.type_exprs.get(align_expr)
        else {
            kbail!(self, ty_app.span, "Expected an integer literal for opaque alignment");
        };
        let TypedIntValue::U32(size) =
            self.eval_integer_value(size_literal, size_span, Some(IntegerType::U32.type_id()))?
        else {
            kbail!(self, size_span, "Expected a u32 value for opaque size");
        };
        let TypedIntValue::U32(align) =
            self.eval_integer_value(align_literal, align_span, Some(IntegerType::U32.type_id()))?
        else {
            kbail!(self, align_span, "Expected a u32 value for opaque alignment");
        };

        if align == 0 || !align.is_power_of_two() || align > 128 {
            kbail!(
                self,
                align_span,
                "Alignment must be a non-zero power of two, not exceeding 128, got {}",
                align
            );
        }

        let opaque_type = self.add_type(Type::Opaque(OpaqueType { size, align }));
        Ok(Some(opaque_type))
    }

    fn eval_sized_tyapp(
        &mut self,
        ty_app: &parse::TypeApplication,
        what: &str,
        scope_id: ScopeId,
        context: EvalTypeExprContext,
    ) -> K1Result<(TypeId, TypeId)> {
        if ty_app.name.has_path() {
            kbail!(
                self,
                ty_app.span,
                "Expected '{}' with no namespace, got '{}'",
                self.ident_str(ty_app.name.name),
                &ty_app.name
            );
        }
        if ty_app.args.len() != 2 {
            kbail!(
                self,
                ty_app.span,
                "Expected 2 type parameters for {}, got {}",
                self.ident_str(ty_app.name.name),
                ty_app.args.len()
            );
        }
        let Some(element_expr) = self.ast.mem.get_nth(ty_app.args, 0).type_expr else {
            kbail!(self, ty_app.span, "Wildcard _ type not accepted here");
        };
        let Some(size_expr) = self.ast.mem.get_nth(ty_app.args, 1).type_expr else {
            kbail!(self, ty_app.span, "Wildcard _ type not accepted here");
        };
        let element_type = self.eval_type_expr_ext(element_expr, scope_id, context.descended())?;
        let size_type_id = self.eval_type_expr_ext(size_expr, scope_id, context.descended())?;
        let Some(static_type) = self.get_static_type_of_type(size_type_id) else {
            kbail!(self, ty_app.span, "{} must be a static type", what);
        };
        if static_type.family_type_id != I64_TYPE_ID {
            kbail!(
                self,
                ty_app.span,
                "{} must be an int; got {}",
                what,
                static_type.family_type_id
            );
        }
        Ok((element_type, size_type_id))
    }

    pub(super) fn check_unique_field_names<T>(
        &self,
        fields: &[T],
        get_name_and_span: impl Fn(&T) -> (StringId, Span),
    ) -> K1Result<()> {
        for (index, field) in fields.iter().enumerate() {
            let (name, span) = get_name_and_span(field);
            if fields[..index].iter().any(|prev| get_name_and_span(prev).0 == name) {
                kbail!(self, span, "Duplicate field name '{}'", name);
            }
        }
        Ok(())
    }

    pub(super) fn add_struct_type(
        &mut self,
        fields: &[StructTypeField],
        record_kind: RecordKind,
    ) -> K1Result<TypeId> {
        self.check_unique_field_names(fields, |f| (f.name, f.span))?;
        Ok(self.add_type_with_storage(|mem| {
            Type::Struct(StructType { fields: mem.pushn(fields), record_kind })
        }))
    }

    pub(super) fn choose_either_tag_type(
        &self,
        explicit_tag_type: Option<(TypeId, Span)>,
        variant_count: u32,
        span: Span,
    ) -> K1Result<IntegerType> {
        match explicit_tag_type {
            Some((tag_type, tag_span)) => match self.types.get(tag_type) {
                Type::Integer(int_type) => Ok(*int_type),
                _ => kbail!(self, tag_span, "Tag type must be an integer type; got {}", tag_type),
            },
            None => {
                const U8_MAX_VARIANTS: u32 = u8::MAX as u32 + 1;
                const MAX_VARIANTS: u32 = u16::MAX as u32 + 1;
                match variant_count {
                    c if c <= U8_MAX_VARIANTS => Ok(IntegerType::U8),
                    c if c <= MAX_VARIANTS => Ok(IntegerType::U16),
                    _ => kbail!(self, span, "sum cannot have more than {MAX_VARIANTS} variants"),
                }
            }
        }
    }

    pub(super) fn add_either_type(
        &mut self,
        tag_type: IntegerType,
        variants: &[EitherVariantPart],
        partial: bool,
        span: Span,
    ) -> K1Result<TypeId> {
        if variants.is_empty() {
            kbail!(self, span, "either must have at least one variant");
        }
        let has_payloads = variants.iter().any(|v| v.payload.is_some());
        if has_payloads && partial {
            kbail!(self, span, "Only an either without payloads can be partial");
        }
        let mut typed_variants: List<TypedSumVariant, MemTmp> =
            self.tmp.new_list(variants.len() as u32);
        let mut next_tag = Some(tag_type.zero());
        for (index, v) in variants.iter().enumerate() {
            if variants[..index].iter().any(|prev| prev.name == v.name) {
                kbail!(self, v.name_span, "Duplicate variant name '{}'", v.name);
            }
            let tag_value = match (v.tag, next_tag) {
                (Some(tag), _) if tag.get_integer_type() != tag_type => {
                    kbail!(
                        self,
                        v.name_span,
                        "Tag value for '{}' is {}, but the tag type is {}",
                        v.name,
                        tag,
                        tag_type
                    )
                }
                (Some(tag), _) | (None, Some(tag)) => tag,
                (None, None) => {
                    kbail!(
                        self,
                        v.name_span,
                        "Tag value for '{}' does not fit in {}",
                        v.name,
                        tag_type
                    )
                }
            };
            if let Some(existing) = typed_variants.iter().find(|prev| prev.tag_value == tag_value) {
                kbail!(self, v.name_span, "Duplicate tag value: {}", existing.tag_value);
            }
            next_tag = tag_value.incr();
            typed_variants.push(TypedSumVariant {
                name: v.name,
                index: index as u32,
                payload: v.payload,
                tag_value,
                name_span: v.name_span,
            });
        }
        if has_payloads {
            return Ok(self.add_type_with_storage(|mem| {
                Type::Sum(SumType { variants: mem.pushn(typed_variants.as_slice()), tag_type })
            }));
        }
        let mut member_values: TmpList<ScalarEnumValue> = self.tmp.new_list(variants.len() as u32);
        for v in typed_variants.iter() {
            member_values.push(ScalarEnumValue {
                name: v.name,
                int_value: v.tag_value,
                name_span: v.name_span,
            });
        }
        Ok(self.add_type_with_storage(|mem| {
            Type::Enum(ScalarEnumType {
                member_values: mem.pushn(member_values.as_slice()),
                int_type: tag_type,
                partial,
            })
        }))
    }

    pub(super) fn handle_array_tyapp(
        &mut self,
        ty_app: &parse::TypeApplication,
        scope_id: ScopeId,
        context: EvalTypeExprContext,
    ) -> K1Result<Option<TypeId>> {
        if ty_app.name.name != self.ast.idents.b.array {
            return Ok(None);
        }
        let (element_type, size_type) =
            self.eval_sized_tyapp(ty_app, "array size", scope_id, context)?;
        Ok(Some(self.add_type(Type::Array(ArrayType { element_type, size_type }))))
    }

    pub(super) fn handle_vector_tyapp(
        &mut self,
        ty_app: &parse::TypeApplication,
        scope_id: ScopeId,
        context: EvalTypeExprContext,
    ) -> K1Result<Option<TypeId>> {
        if ty_app.name.name != self.ast.idents.b.vector {
            return Ok(None);
        }
        let (element_type, size_type) =
            self.eval_sized_tyapp(ty_app, "Vector lane count", scope_id, context)?;
        self.validate_vector_parts(element_type, size_type, ty_app.span)?;
        Ok(Some(self.add_type(Type::Vector(ArrayType { element_type, size_type }))))
    }

    pub(super) fn validate_vector_parts(
        &self,
        element_type: TypeId,
        size_type_id: TypeId,
        span: Span,
    ) -> K1Result<()> {
        let element_size = match self.types.get(element_type) {
            Type::Integer(it) => Some(it.width().bits() / 8),
            Type::Char => Some(1),
            Type::Float(ft) => Some(ft.width().bits() / 8),
            Type::TypeParameter(_) | Type::InferenceHole(_) | Type::FunctionTypeParameter(_) => {
                None
            }
            _ => {
                kbail!(
                    self,
                    span,
                    "Vector elements must be an int, char, or float type; got {}",
                    element_type
                );
            }
        };
        let Some(count) = self.get_type_as_i64(size_type_id) else {
            return Ok(());
        };
        #[allow(clippy::manual_range_contains)]
        if count < 2 || count > 64 || !(count as u64).is_power_of_two() {
            kbail!(
                self,
                span,
                "Vector lane count must be a power of two between 2 and 64; got {}",
                count
            );
        }
        if let Some(element_size) = element_size {
            let total = element_size as i64 * count;
            if total > 64 {
                kbail!(
                    self,
                    span,
                    "Vector total size must not exceed 64 bytes (512 bits); got {}",
                    total
                );
            }
        }
        Ok(())
    }

    pub(super) fn substitute_in_type(
        &mut self,
        type_id: TypeId,
        substitution_pairs: &[TypeSubstitutionPair],
    ) -> TypeId {
        // The empty substitution is the identity
        if substitution_pairs.is_empty() {
            return type_id;
        }
        let mut from_kinds = SubstitutionFromKinds::default();
        for pair in substitution_pairs {
            let counts = self.type_variable_counts.get(pair.from);
            match (counts.inference_hole_count > 0, counts.type_parameter_count > 0) {
                (true, false) => from_kinds.holes = true,
                (false, true) => from_kinds.params = true,
                (true, true) => from_kinds.holes_and_params = true,
                (false, false) => from_kinds.other = true,
            }
        }
        let tmp_mark = self.tmp.mark();
        let res = self.substitute_in_type_inner(type_id, substitution_pairs, from_kinds);
        self.tmp.reset_to(tmp_mark);
        res
    }

    fn substitute_in_type_inner(
        &mut self,
        type_id: TypeId,
        substitution_pairs: &[TypeSubstitutionPair],
        from_kinds: SubstitutionFromKinds,
    ) -> TypeId {
        if from_kinds.no_from_occurs_in(self.type_variable_counts.get(type_id)) {
            return type_id;
        }

        // If this type is already a generic instance of something, just
        // re-specialize it on the right inputs. So find out what the new value
        // of each type param should be and call instantiate_generic_type
        //
        // This happens when specializing a type that contains an Opt[T], for example.
        // This lets us hit our cache as well
        if let Some(spec_info) = self.get_instance_info(type_id) {
            // A,   B,    T
            // int, bool, char
            // Opt[T] -> Opt[char]
            let generic_parent = spec_info.generic_parent;
            let original_args = self.get_type_slice(spec_info.type_args);
            let mut new_type_args = self.tmp.new_list(original_args.len() as u32);
            let mut any_change = false;
            for prev_arg in original_args {
                let new_type =
                    self.substitute_in_type_inner(*prev_arg, substitution_pairs, from_kinds);
                if new_type != *prev_arg {
                    any_change = true;
                }
                new_type_args.push(new_type);
            }
            if !any_change {
                return type_id;
            }
            let parent_pending = self
                .type_defn_context
                .stack
                .iter()
                .any(|e| e.reserved_type_id == Some(generic_parent));
            return if parent_pending {
                // The parent generic is still being defined (mutual recursion), so its
                // template cannot be substituted yet; defer to a reserved instance id
                self.get_or_reserve_recursive_instance(
                    generic_parent,
                    new_type_args.as_slice(),
                    None,
                )
            } else {
                self.instantiate_generic_type(generic_parent, new_type_args.as_slice())
            };
        };

        let matching_subst_pair = substitution_pairs.iter().find(|pair| pair.from == type_id);
        if let Some(matching_pair) = matching_subst_pair {
            return matching_pair.to;
        }

        let res = match self.types.get(type_id) {
            Type::InferenceHole(_) => type_id,
            Type::Char
            | Type::Integer(_)
            | Type::Enum(_)
            | Type::Float(_)
            | Type::Bool
            | Type::Pointer
            | Type::Never => type_id,
            Type::Struct(struc) => {
                let record_kind = struc.record_kind;
                let old_fields = struc.fields;
                let mut any_change = false;
                // Build the candidate in tmp; only commit to the arena if something changed
                let mut new_fields = self.tmp.new_list(old_fields.len());
                for field in self.mem.getn(old_fields) {
                    let new_field_type_id = self.substitute_in_type_inner(
                        field.type_id,
                        substitution_pairs,
                        from_kinds,
                    );
                    if new_field_type_id != field.type_id {
                        any_change = true;
                    }
                    let mut new_field = *field;
                    new_field.type_id = new_field_type_id;
                    new_fields.push(new_field);
                }
                if any_change {
                    self.add_type_with_storage(|mem| {
                        Type::Struct(StructType {
                            fields: mem.pushn(new_fields.as_slice()),
                            record_kind,
                        })
                    })
                } else {
                    type_id
                }
            }
            Type::Sum(e) => {
                let original_tag_type = e.tag_type;
                let old_variants = e.variants;
                let mut any_changed = false;
                // Build the candidate in tmp; only commit to the arena if something changed
                let mut new_variants = self.tmp.new_list(old_variants.len());
                for variant in self.mem.getn(old_variants) {
                    let mut new_variant = *variant;
                    if let Some(p) = variant.payload {
                        let new_payload_id =
                            self.substitute_in_type_inner(p, substitution_pairs, from_kinds);
                        if new_payload_id != p {
                            any_changed = true;
                            new_variant.payload = Some(new_payload_id)
                        };
                    }
                    new_variants.push(new_variant);
                }
                if any_changed {
                    self.add_type_with_storage(|mem| {
                        Type::Sum(SumType {
                            variants: mem.pushn(new_variants.as_slice()),
                            tag_type: original_tag_type,
                        })
                    })
                } else {
                    type_id
                }
            }
            Type::Opaque(_) => type_id,
            Type::Reference(reference) => {
                let reference = *reference;
                let new_inner = self.substitute_in_type_inner(
                    reference.inner_type,
                    substitution_pairs,
                    from_kinds,
                );
                if new_inner != reference.inner_type {
                    self.add_reference_type(new_inner)
                } else {
                    type_id
                }
            }
            Type::TypeParameter(_type_param) => type_id,
            Type::FunctionIdReference(fr) => {
                let fr = *fr;
                let new_fn_type =
                    self.substitute_in_type_inner(fr.function_type, substitution_pairs, from_kinds);
                if new_fn_type != fr.function_type {
                    self.add_function_id_reference_type(fr.function_id, new_fn_type)
                } else {
                    type_id
                }
            }
            Type::FunctionTypeParameter(ftp) => {
                let function_type_id = ftp.function_type;
                let new_fn_type =
                    self.substitute_in_type_inner(function_type_id, substitution_pairs, from_kinds);
                if new_fn_type != function_type_id {
                    let type_param = self.types.get(type_id).as_function_type_parameter().unwrap();
                    self.add_type(Type::FunctionTypeParameter(FunctionTypeParameter {
                        name: type_param.name,
                        scope_id: type_param.scope_id,
                        span: type_param.span,
                        function_type: new_fn_type,
                    }))
                } else {
                    type_id
                }
            }
            Type::Generic(_) => {
                unreachable!("substitute_in_type is not expected to be called on a Generic")
            }
            Type::Function(fun_type) => {
                let mut any_new = false;
                let is_lambda = fun_type.is_lambda;
                let old_return_type = fun_type.return_type;
                let old_params = fun_type.logical_params();
                let new_return_type =
                    self.substitute_in_type_inner(old_return_type, substitution_pairs, from_kinds);
                if new_return_type != old_return_type {
                    any_new = true
                };
                let mut new_params: List<FnParamType, _> = self.tmp.new_list(old_params.len());
                for param in self.mem.getn(old_params) {
                    let new_param_type = self.substitute_in_type_inner(
                        param.type_id,
                        substitution_pairs,
                        from_kinds,
                    );
                    if new_param_type != param.type_id {
                        any_new = true;
                    }
                    new_params.push(FnParamType { type_id: new_param_type, kind: param.kind });
                }
                if any_new {
                    self.add_function_type(new_params.as_slice(), new_return_type, is_lambda)
                } else {
                    type_id
                }
            }
            Type::FunctionPointer(fp) => {
                let fp = *fp;
                let new_fn_type = self.substitute_in_type_inner(
                    fp.function_type_id,
                    substitution_pairs,
                    from_kinds,
                );
                if new_fn_type != fp.function_type_id {
                    self.add_function_pointer_type(new_fn_type)
                } else {
                    type_id
                }
            }
            Type::Lambda(_) => {
                unreachable!("substitute_in_type is not expected to be called on a Lambda")
            }
            Type::LambdaObject(lam_obj) => {
                let fn_type = lam_obj.function_type;
                let parsed_id = lam_obj.parsed_id;
                let new_fn_type =
                    self.substitute_in_type_inner(fn_type, substitution_pairs, from_kinds);
                if new_fn_type != fn_type {
                    self.add_lambda_object(new_fn_type, parsed_id)
                } else {
                    type_id
                }
            }
            Type::AbilityObject(ao) => {
                let signature = ao.signature;
                let new_signature =
                    self.substitute_in_ability_signature(substitution_pairs, signature, Span::NONE);
                if new_signature == signature {
                    type_id
                } else {
                    match self.eval_dyn_ability_object_type(new_signature, Span::NONE) {
                        Ok(t) => t,
                        Err(e) => {
                            self.ice("dyn ability substitution produced invalid object", Some(&e))
                        }
                    }
                }
            }
            Type::StaticValue(value_type) => {
                if value_type.value_id.is_some() {
                    // Can't substitute inside the type "static[string; "hello"]"
                    // But you can inside type "static[T; _]"
                    type_id
                } else {
                    let family_type = value_type.family_type_id;
                    let new_inner_type =
                        self.substitute_in_type_inner(family_type, substitution_pairs, from_kinds);
                    if new_inner_type == family_type {
                        type_id
                    } else {
                        self.add_type(Type::StaticValue(StaticValueType {
                            family_type_id: new_inner_type,
                            value_id: None,
                        }))
                    }
                }
            }
            typ @ (Type::Array(arr) | Type::Vector(arr)) => {
                let arr = *arr;
                let make: fn(ArrayType) -> Type =
                    if matches!(typ, Type::Vector(_)) { Type::Vector } else { Type::Array };
                let new_element_type =
                    self.substitute_in_type_inner(arr.element_type, substitution_pairs, from_kinds);
                let new_size_type =
                    self.substitute_in_type_inner(arr.size_type, substitution_pairs, from_kinds);
                if new_element_type == arr.element_type && new_size_type == arr.size_type {
                    type_id
                } else {
                    self.add_type(make(ArrayType {
                        element_type: new_element_type,
                        size_type: new_size_type,
                    }))
                }
            }
        };
        debug!(
            "substitute in type on {}.\npairs: {}.\nGot: {}",
            self.type_id_to_string(type_id),
            self.pretty_print_type_substitutions(substitution_pairs, ", "),
            self.dump_type_id_to_string(res)
        );
        res
    }

    pub(super) fn check_type_finite_rec(
        &mut self,
        type_id: TypeId,
        behind_indirection: bool,
        stack: &mut List<TypeId, MemTmp>,
    ) -> Option<TypeId> {
        // Any repeat on the current path is a cycle; only indirection-free ones are
        // infinite
        if stack.contains(&type_id) {
            return if behind_indirection { None } else { Some(type_id) };
        }
        stack.push_grow(&mut self.tmp, type_id);

        let result = 'walk: {
            match self.types.get(type_id) {
                Type::Struct(struct_type) => {
                    let fields = struct_type.fields;
                    for i in 0..fields.len() as usize {
                        let field_type_id = self.mem.get_nth(fields, i).type_id;
                        if let Some(t) =
                            self.check_type_finite_rec(field_type_id, behind_indirection, stack)
                        {
                            break 'walk Some(t);
                        }
                    }

                    None
                }

                Type::Sum(sum_type) => {
                    let variants = sum_type.variants;
                    for i in 0..variants.len() as usize {
                        if let Some(payload) = self.mem.get_nth(variants, i).payload {
                            if let Some(t) =
                                self.check_type_finite_rec(payload, behind_indirection, stack)
                            {
                                break 'walk Some(t);
                            }
                        }
                    }

                    None
                }

                Type::Reference(reference_type) => {
                    let inner_type = reference_type.inner_type;
                    self.check_type_finite_rec(inner_type, true, stack)
                }

                Type::Generic(generic) => {
                    let inner = generic.inner;
                    self.check_type_finite_rec(inner, behind_indirection, stack)
                }

                Type::Array(array_type) => {
                    let size_type = array_type.size_type;
                    let element_type = array_type.element_type;
                    if self.get_concrete_count_of_array(size_type) != Some(0) {
                        self.check_type_finite_rec(element_type, behind_indirection, stack)
                    } else {
                        None
                    }
                }

                _ => None,
            }
        };

        stack.pop();
        result
    }
}
