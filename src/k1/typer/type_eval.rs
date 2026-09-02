// Copyright (c) 2026 knix
// All rights reserved.

use super::*;
use crate::{panic_at_disco, spair};

impl TypedProgram {
    pub(super) fn eval_type_defn(
        &mut self,
        parsed_type_defn_id: ParsedTypeDefnId,
        namespace_scope_id: ScopeId,
    ) -> K1Result<TypeId> {
        let parsed_type_defn = *self.ast.get_type_defn(parsed_type_defn_id);
        let is_generic_defn = !parsed_type_defn.type_params.is_empty();
        let is_alias = parsed_type_defn.flags.is_alias();
        debug!("eval_type_defn {}", self.ident_str(parsed_type_defn.name));

        if parsed_type_defn.name == self.ast.idents.b.some {
            kbail!(self, parsed_type_defn.span, "'some' is not a valid type name");
        }

        if is_alias && is_generic_defn {
            kbail!(self, parsed_type_defn.span, "Alias types cannot have type parameters (yet)");
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
            let mut predicate_functions = self.mem.new_list(0);
            for parsed_constraint in self.ast.mem.getn(type_param.constraints) {
                match parsed_constraint {
                    ParsedTypeConstraintExpr::Ability(ability_expr) => {
                        let ability_sig =
                            self.eval_ability_expr(*ability_expr, false, defn_scope_id)?;
                        ability_constraint_signatures.push(ability_sig);
                    }
                    ParsedTypeConstraintExpr::Predicate(qident) => {
                        predicate_functions.push_grow(&mut self.mem, *qident);
                    }
                    ParsedTypeConstraintExpr::Static(_) => {}
                };
            }
            let predicate_functions_handle = predicate_functions.to_slice_trim(&mut self.mem);
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
        let rhs_type_id = match self.ast.type_exprs.get(parsed_type_defn.value_expr) {
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
                            parsed_type_defn.span,
                            "Unknown builtin type id: {}",
                            defn_type_id
                        );
                    }
                };
                self.set_type(defn_type_id, type_value, None, None);
                defn_type_id
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
                        parsed_type_defn.span,
                        "The right-hand side of a type definition cannot be a bare reference to a type that is still being defined; wrap it in a struct or either, or use an alias"
                    );
                }
                rhs_type_id
            }
        };

        // not an alias, so only allow named struct or sum or builtin
        // TODO: revisit this? why can't we have nominal integer types?
        if !is_alias {
            match self.types.get(rhs_type_id) {
                Type::Char
                | Type::Bool
                | Type::Never
                | Type::Pointer
                | Type::Integer(_)
                | Type::Float(_) => {
                    if let ParsedTypeExpr::Builtin(_) =
                        self.ast.type_exprs.get(parsed_type_defn.value_expr)
                    {
                        // fine
                    } else {
                        kbail!(
                            self,
                            parsed_type_defn.span,
                            "Non-alias type definition must be a struct or sum; not a '{}'. Perhaps you intended to create an alias `type(alias) <name> = <type>`",
                            rhs_type_id
                        );
                    }
                }
                // Allowed nominal types
                Type::Struct(_) | Type::Sum(_) | Type::Enum(_) | Type::Opaque(_) => {}
                _other => {
                    kbail!(
                        self,
                        parsed_type_defn.span,
                        "Non-alias type definition must be a struct or either or opaque or builtin; not a '{}'. Perhaps you meant to create an alias `type(alias) <name> = <type>`",
                        rhs_type_id
                    );
                }
            };
        }

        let found_namespace_id =
            self.scopes.find_namespace_local(namespace_scope_id, parsed_type_defn.name);
        let companion_namespace_id = match found_namespace_id {
            None => None,
            Some(ns_id) => {
                let companion_ns = self.namespaces.get(ns_id);
                if companion_ns.namespace_type == NamespaceKind::TypeCompanion {
                    Some(ns_id)
                } else {
                    self.report_hint(parsed_type_defn.span, "matching namespace is not declared as type companion; use an `ns for {}` declaration");
                    None
                }
            }
        };
        let defn_info = if is_alias {
            None
        } else {
            Some(TypeDefnInfo {
                name: parsed_type_defn.name,
                scope: namespace_scope_id,
                companion_namespace: companion_namespace_id,
                ast_id: ParsedId::TypeDefn(parsed_type_defn_id),
                // Over-approximate: anything defined while the cluster has recursive mentions
                // is treated as recursive
                recursive: !self.type_defn_context.recursive_mentions.is_empty(),
            })
        };

        let type_id = if is_generic_defn {
            let gen_type = GenericType { params: type_params_handle, inner: rhs_type_id };
            let generic_id = reserved_type_id.unwrap();
            self.set_type(generic_id, Type::Generic(gen_type), None, defn_info);
            let type_args = self.intern_type_slice_handle(type_params_handle);
            if self.get_specialization(generic_id, type_args).is_none() {
                let inner_content = *self.types.get(rhs_type_id);
                let self_instance = self.add_type(
                    inner_content,
                    defn_info,
                    Some(GenericInstanceInfo { generic_parent: generic_id, type_args }),
                );
                self.insert_specialization(generic_id, type_args, self_instance);
            }
            generic_id
        } else {
            if is_alias {
                rhs_type_id
            } else {
                let t = *self.types.get(rhs_type_id);
                let instance_info = self.get_instance_info(rhs_type_id).cloned();
                self.set_type(reserved_type_id.unwrap(), t, instance_info, defn_info);
                reserved_type_id.unwrap()
            }
        };

        if let Some(companion_namespace_id) = companion_namespace_id {
            self.namespaces.get_mut(companion_namespace_id).companion_type_id = Some(type_id);
        }
        let name = parsed_type_defn.name;
        let added = self.scopes.add_type(namespace_scope_id, name, type_id);
        if !added {
            let span = parsed_type_defn.span;
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
            } else if name == b.code_chunk {
                self.builtin_types.code_chunk = Some(type_id);
            } else if name == b.code_builder {
                self.builtin_types.code_builder = Some(type_id);
            }
        } else if namespace_scope_id == self.scopes.types_scope_id {
            if name == self.ast.idents.b.type_schema {
                self.builtin_types.types_type_schema = Some(type_id);
            } else if name == self.ast.idents.b.type_info {
                self.builtin_types.types_type_info = Some(type_id);
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
            }
        }

        let Some(defn_stack_entry) = self.type_defn_context.stack.pop() else {
            self.ice_span(parsed_type_defn.span, "No defn stack entry");
        };
        debug_assert_eq!(defn_stack_entry.parsed_id, parsed_type_defn_id);
        debug_assert_eq!(defn_stack_entry.reserved_type_id, reserved_type_id);
        if !is_alias {
            self.type_defn_context.completed.push((type_id, parsed_type_defn.span));
        }
        if self.type_defn_context.stack.is_empty() {
            self.finish_type_defn_cluster()
        }
        if let Some(idx) = self
            .types_pending_definition
            .iter()
            .position(|tpd| tpd.parsed_id == parsed_type_defn_id)
        {
            // eprintln!("removing pending defn {idx} {}", self.ident_str(name));
            self.types_pending_definition.remove(idx);
        } else {
            self.ice_span(parsed_type_defn.span, "the type we defined was not pending")
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
                let mut fields: List<StructTypeField, TypedProgram> =
                    self.mem.new_list(struct_defn.fields.len());
                for ast_field in self.ast.mem.getn(struct_defn.fields) {
                    if let Some(existing_field) = fields.iter().find(|f| f.name == ast_field.name) {
                        kbail!(
                            self,
                            struct_defn.span,
                            "Duplicate field name '{}' in {}",
                            existing_field.name,
                            struct_defn.record_kind.kind_name(),
                        );
                    }
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

                let struct_defn =
                    Type::Struct(StructType { fields: fields.to_slice(), record_kind: kind });
                let type_id = self.add_type_anon(struct_defn);

                Ok(type_id)
            }
            ParsedTypeExpr::TypeApplication(_ty_app) => {
                let type_op_result =
                    self.detect_and_eval_type_operator(type_expr_id, scope_id, context)?;
                match type_op_result {
                    None => self.eval_type_application(type_expr_id, scope_id, context),
                    Some(type_op_result) => Ok(type_op_result),
                }
            }
            ParsedTypeExpr::Optional(opt) => {
                // Rewrite the sugar and compile the parsed
                let parsed_ty_app = ParsedTypeExpr::TypeApplication(parse::TypeApplication {
                    span: opt.span,
                    name: QIdent::naked(self.ast.idents.b.opt, opt.span),
                    args: self.ast.mem.pushn(&[NamedTypeArg {
                        name: None,
                        type_expr: Some(opt.base),
                        span: opt.span,
                    }]),
                });
                let parsed_ty_app_id = self.ast.type_exprs.add(parsed_ty_app);
                self.eval_type_expr(parsed_ty_app_id, scope_id)
            }
            ParsedTypeExpr::Reference(r) => {
                let inner_ty = self.eval_type_expr_ext(r.base, scope_id, context.descended())?;
                if let Type::Function(_) = self.types.get(inner_ty) {
                    let function_pointer_type = self.add_function_pointer_type(inner_ty);
                    Ok(function_pointer_type)
                } else {
                    let type_id = self.add_reference_type(inner_ty);
                    Ok(type_id)
                }
            }
            ParsedTypeExpr::Array(arr) => {
                let arr = *arr;
                let element_type =
                    self.eval_type_expr_ext(arr.element_type, scope_id, context.descended())?;

                let size_type_id =
                    self.eval_type_expr_ext(arr.size_expr, scope_id, context.descended())?;

                let Some(static_type) = self.get_static_type_of_type(size_type_id) else {
                    kbail!(self, arr.span, "array size must be a static type");
                };
                if static_type.family_type_id != I64_TYPE_ID {
                    kbail!(
                        self,
                        arr.span,
                        "array size must be an int; got {}",
                        static_type.family_type_id
                    );
                }

                let array_type = Type::Array(ArrayType { element_type, size_type: size_type_id });
                let type_id = self.add_type_anon(array_type);
                Ok(type_id)
            }
            ParsedTypeExpr::Sum(sum) => {
                let sum = *sum;
                let variant_count = sum.variants.len();
                if variant_count == 0 {
                    kbail!(self, sum.span, "either must have at least one variant");
                }

                let tag_type = match sum.tag_type {
                    None => {
                        const U8_MAX_VARIANTS: u32 = u8::MAX as u32 + 1;
                        const MAX_VARIANTS: u32 = u16::MAX as u32 + 1;
                        let min_viable = match variant_count {
                            c if c <= U8_MAX_VARIANTS => IntegerType::U8,
                            c if c <= MAX_VARIANTS => IntegerType::U16,
                            _ => {
                                kbail!(
                                    self,
                                    sum.span,
                                    "sum cannot have more than {MAX_VARIANTS} variants"
                                );
                            }
                        };
                        min_viable
                    }
                    Some(tag_type_expr) => {
                        let tag_type = self.eval_type_expr(tag_type_expr, scope_id)?;
                        match self.types.get(tag_type) {
                            Type::Integer(int_type) => *int_type,
                            _ => {
                                kbail!(
                                    self,
                                    self.ast.get_type_expr_span(tag_type_expr),
                                    "Must be an integer type"
                                );
                            }
                        }
                    }
                };

                let has_payloads =
                    self.ast.mem.getn(sum.variants).iter().any(|v| v.payload.is_some());
                if has_payloads {
                    let mut variants: List<TypedSumVariant, _> = self.mem.new_list(variant_count);
                    let mut next_tag = tag_type.zero();
                    for (index, v) in self.ast.mem.getn(sum.variants).iter().enumerate() {
                        let payload_type_id = match &v.payload {
                            None => None,
                            Some(payload_type_expr) => {
                                let type_id = self.eval_type_expr_ext(
                                    *payload_type_expr,
                                    scope_id,
                                    context.descended(),
                                )?;
                                Some(type_id)
                            }
                        };
                        let tag_int = match v.explicit_value {
                            None => next_tag,
                            Some(explicit_value) => {
                                let parsed = self.eval_integer_value(
                                    explicit_value.text_span,
                                    Some(tag_type.type_id()),
                                )?;
                                parsed
                            }
                        };
                        if let Some(existing) = variants.iter().find(|v| v.tag_value == tag_int) {
                            kbail!(
                                self,
                                v.name_span,
                                "Duplicate tag value: {}",
                                existing.tag_value
                            );
                        }
                        let variant = TypedSumVariant {
                            name: v.tag_name,
                            index: index as u32,
                            payload: payload_type_id,
                            tag_value: tag_int,
                            name_span: v.name_span,
                        };

                        next_tag = tag_int.incr();
                        variants.push(variant);
                    }

                    debug!(
                        "variants and tags: {}",
                        variants
                            .iter()
                            .map(|v| format!("  {} {}", self.ident_str(v.name), v.tag_value))
                            .join("\n")
                    );
                    let sum_type = Type::Sum(SumType { variants: variants.to_slice(), tag_type });
                    let sum_type_id = self.add_type_anon(sum_type);
                    Ok(sum_type_id)
                } else {
                    let mut member_values: List<ScalarEnumValue, _> =
                        self.mem.new_list(variant_count);
                    let mut next_tag = tag_type.zero();
                    for v in self.ast.mem.getn(sum.variants).iter() {
                        let tag_value = match v.explicit_value {
                            None => next_tag,
                            Some(explicit_value) => {
                                let parsed = self.eval_integer_value(
                                    explicit_value.text_span,
                                    Some(tag_type.type_id()),
                                )?;
                                parsed
                            }
                        };
                        if let Some(existing) =
                            member_values.iter().find(|v| v.int_value == tag_value)
                        {
                            kbail!(
                                self,
                                v.name_span,
                                "Duplicate enum value: {}",
                                existing.int_value
                            );
                        }
                        let variant = ScalarEnumValue {
                            name: v.tag_name,
                            int_value: tag_value,
                            name_span: v.name_span,
                        };
                        next_tag = tag_value.incr();
                        member_values.push(variant);
                    }

                    debug!(
                        "members and tags: {}",
                        member_values
                            .iter()
                            .map(|v| format!("  {} {}", self.ident_str(v.name), v.int_value))
                            .join("\n")
                    );
                    let enum_type = Type::Enum(ScalarEnumType {
                        member_values: member_values.to_slice(),
                        int_type: tag_type,
                    });
                    let sum_type_id = self.add_type_anon(enum_type);
                    Ok(sum_type_id)
                }
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
                                    let mut members = self.mem.new_list(sum.variants.len());
                                    for variant in self.mem.getn(sum.variants) {
                                        members.push(ScalarEnumValue {
                                            name: variant.name,
                                            int_value: variant.tag_value,
                                            name_span: variant.name_span,
                                        })
                                    }
                                    let enum_type_id =
                                        self.add_type_anon(Type::Enum(ScalarEnumType {
                                            member_values: members.to_slice(),
                                            int_type: sum.tag_type,
                                        }));
                                    Ok(enum_type_id)
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
                    Type::Function(fun) => {
                        let member_name = self.ast.idents.get_string(acc.member_name);
                        match member_name {
                            "return" => Ok(fun.return_type),
                            _other => {
                                if let Some(param) = self
                                    .mem
                                    .getn(fun.logical_params())
                                    .iter()
                                    .find(|p| p.name == acc.member_name)
                                {
                                    Ok(param.type_id)
                                } else {
                                    return make_fail_ast_id(
                                        &self.ast,
                                        &format!("Function has no parameter named {}", member_name),
                                        type_expr_id.into(),
                                    );
                                }
                            }
                        }
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
                let mut params: List<FnParamType, _> = self.mem.new_list(fun_type.params.len());

                for (index, param) in self.ast.mem.getn(fun_type.params).iter().enumerate() {
                    let type_id = self.eval_type_expr(*param, scope_id)?;

                    let name = self.positional_param_name(index);
                    params.push(FnParamType {
                        type_id,
                        name,
                        is_context: false,
                        is_lambda_env: false,
                        is_macro_code: false,
                    });
                }
                let return_type = self.eval_type_expr(fun_type.return_type, scope_id)?;
                let params_handle = params.to_slice();
                let function_type_id = self.add_type_anon(Type::Function(FunctionType {
                    physical_params: params_handle,
                    is_lambda: false,
                    return_type,
                }));
                Ok(function_type_id)
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
                let static_type_id = self.add_type_anon(Type::StaticValue(value_type));
                Ok(static_type_id)
            }
            ParsedTypeExpr::StaticLiteral(parsed_literal) => {
                let parsed_literal = *parsed_literal;
                let (static_value_id, inner_type_id) =
                    self.literal_to_static_value_and_type(&parsed_literal, scope_id, None)?;
                let static_type_id = self.add_value_type(inner_type_id, Some(static_value_id));
                Ok(static_type_id)
            }
            ParsedTypeExpr::ExecStatic(es) => {
                let es = *es;
                let expr_ctx = EvalExprContext::make(scope_id).with_is_static(true);
                let resulting_value_id = self.execute_static_expr(es.body_expr, expr_ctx, &[])?;
                let resulting_type_id = self.get_static_value_type(resulting_value_id);
                match self.types.get(resulting_type_id) {
                    Type::Struct(_) if resulting_type_id == self.builtin_types.type_id() => {
                        // The user's code returned a type-id struct. The value inside is a type id
                        // and that's our result
                        let struct_value =
                            self.static_values.get(resulting_value_id).as_struct().unwrap();
                        let first_field_id = self.static_values.get_slice(struct_value.fields)[0];
                        let StaticValue::Int(TypedIntValue::U64(type_id_u64)) =
                            self.static_values.get(first_field_id)
                        else {
                            kbail!(self, es.span, "[ice] type-id should contain a u64")
                        };
                        if *type_id_u64 == 0 {
                            kbail!(self, es.span, "[ice] type-id should not be zero")
                        }
                        let type_id = TypeId::from_u32(*type_id_u64 as u32).unwrap();
                        match self.types.get_opt(type_id) {
                            None => kbail!(self, es.span, "Unknown type id: "),
                            Some(_) => Ok(type_id),
                        }
                    }
                    Type::StaticValue(_svt) => {
                        // They returned an actual statically-typed value already
                        Ok(resulting_type_id)
                    }
                    _ => {
                        // For any other type, we take the type of their interned/baked/static
                        // value. For example, the type of 42u8 itself
                        let static_type_id =
                            self.add_type_anon(Type::StaticValue(StaticValueType {
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
        parsed_literal: &ParsedLiteral,
        scope_id: ScopeId,
        expected_type_hint: Option<TypeId>,
    ) -> K1Result<(StaticValueId, TypeId)> {
        match parsed_literal {
            ParsedLiteral::Char(byte, _) => {
                Ok((self.static_values.add(StaticValue::Char(*byte)), CHAR_TYPE_ID))
            }
            ParsedLiteral::Bool(b, _) => {
                Ok((self.static_values.add(StaticValue::Bool(*b)), BOOL_TYPE_ID))
            }
            ParsedLiteral::String(s, _) => {
                Ok((self.static_values.add(StaticValue::String(*s)), self.builtin_types.string()))
            }
            ParsedLiteral::Numeric(numeric) => {
                // Parse the numeric literal and determine its type and value
                // Use the expected type hint if provided (e.g., i64 for array sizes)
                let eval_context =
                    EvalExprContext::make(scope_id).with_expected_type(expected_type_hint);
                let num_static_value_id =
                    self.eval_numeric_value(numeric.text_span, eval_context)?;
                Ok((num_static_value_id, self.get_static_value_type(num_static_value_id)))
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
        if !ty_app.name.path.is_empty() {
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
                if let ParsedTypeExpr::TypeApplication(inner_app) =
                    self.ast.type_exprs.get(inner_type_expr_id)
                {
                    let inner_app = *inner_app;
                    if self.name_resolves_to_ability(scope_id, &inner_app.name)? {
                        let ability_expr = self.ast.mem.push_h(ParsedAbilityExpr {
                            name: inner_app.name,
                            arguments: inner_app.args,
                            span: inner_app.span,
                        });
                        let signature = self.eval_ability_expr(ability_expr, false, scope_id)?;
                        let object_type =
                            self.eval_dyn_ability_object_type(signature, inner_app.span)?;
                        return Ok(Some(object_type));
                    }
                }
                let inner =
                    self.eval_type_expr_ext(inner_type_expr_id, scope_id, context.descended())?;
                if self.types.get(inner).as_function().is_none() {
                    kbail!(
                        self,
                        ty_app.span,
                        "Expected function type or ability signature for dyn"
                    );
                }
                let new_function_type = self.add_lambda_env_to_function_type(inner);
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
                    self.mem.new_list(struct1.fields.len() + struct2.fields.len());
                combined_fields.extend(self.mem.getn(struct1.fields));
                for field in self.mem.getn(struct2.fields).iter() {
                    let collision = combined_fields.iter().find(|f| f.name == field.name);
                    if let Some(collision) = collision {
                        if collision.type_id != field.type_id {
                            kbail!(
                                self,
                                ty_app.span,
                                "Field '{}' has conflicting types in the two structs",
                                self.ident_str(field.name).blue()
                            );
                        }
                    }
                    combined_fields.push(*field);
                }

                let new_struct =
                    Type::Struct(StructType { fields: combined_fields.to_slice(), record_kind });
                let type_id = self.add_type_anon(new_struct);

                Ok(Some(type_id))
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
                let new_fields = self
                    .mem
                    .getn(struct1.fields)
                    .iter()
                    .filter(|f| !struct2_fields.iter().any(|sf| sf.name == f.name))
                    .cloned();
                let new_fields = self.mem.pushn_iter(new_fields);

                let new_struct = Type::Struct(StructType { fields: new_fields, record_kind });
                let type_id = self.add_type_anon(new_struct);
                Ok(Some(type_id))
            }
            _ => Ok(None),
        }
    }

    pub(super) fn eval_type_application(
        &mut self,
        ty_app_id: ParsedTypeExprId,
        scope_id: ScopeId,
        context: EvalTypeExprContext,
    ) -> K1Result<TypeId> {
        let ParsedTypeExpr::TypeApplication(ty_app) = *self.ast.type_exprs.get(ty_app_id) else {
            panic_at_disco!("Expected TypeApplication")
        };

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
                    if ty_app.args.len() != g_params.len() {
                        kbail!(
                            self,
                            ty_app.span,
                            "Type {} expects {} type arguments, got {}",
                            &ty_app.name,
                            g_params.len(),
                            ty_app.args.len()
                        );
                    }
                    let (type_arguments_slice, _subst_pairs) = self
                        .check_type_args_against_params(
                            g_params,
                            self.ast.mem.getn(ty_app.args),
                            scope_id,
                            context,
                        )?;
                    let instantiated_type_id =
                        self.instantiate_generic_type(type_id, self.mem.getn(type_arguments_slice));

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
                            Some(function_id) => Ok(self.get_function(function_id).type_id),
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
                        let params = self.ast.get_type_defn(pending_parsed_id).type_params;
                        if ty_app.args.len() != params.len() {
                            kbail!(
                                self,
                                ty_app.span,
                                "Type {} expects {} type arguments, got {}",
                                &ty_app.name,
                                params.len(),
                                ty_app.args.len()
                            );
                        }
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
                            self.type_defn_context.recursive_mentions.push(reserved_type_id);
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
                            for parsed_arg in self.ast.mem.getn(ty_app.args) {
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

                        // Just re-call this function from the top now that the type exists. (hack? idk)
                        self.eval_type_application(ty_app_id, scope_id, context)
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
        let args = self.intern_type_slice(type_arguments);
        match self.get_specialization(generic_type, args) {
            Some(existing) => existing,
            None => self.instantiate_generic_type_miss(generic_type, args),
        }
    }

    pub(super) fn instantiate_generic_type_miss(
        &mut self,
        generic_type: TypeId,
        type_arguments: TypeSliceId,
    ) -> TypeId {
        let gen_type = self.types.get(generic_type).expect_generic();
        debug_assert!(gen_type.params.len() as usize == self.get_type_slice(type_arguments).len());
        let defn_info = self.get_defn_info(generic_type).unwrap();
        // Note: This is where we'd check constraints on the pairs:
        // that each passed params meets the constraints of the generic param
        let mut substitution_pairs: SV8<TypeSubstitutionPair> = smallvec![];
        for (type_param, passed_type_arg) in
            self.mem.getn(gen_type.params).iter().zip(self.get_type_slice(type_arguments))
        {
            substitution_pairs
                .push(TypeSubstitutionPair { from: *type_param, to: *passed_type_arg });
        }
        let inner = gen_type.inner;

        // For a recursive generic, register the result id in the specialization cache before
        // walking the template, so the recursive mention inside it resolves to this id
        let reserved_result = if defn_info.recursive {
            Some(self.reserve_instance_id(generic_type, type_arguments))
        } else {
            None
        };
        let specialized_type = self.substitute_in_type_ext(
            inner,
            &substitution_pairs,
            Some(generic_type),
            Some(defn_info),
        );
        let result_type = match reserved_result {
            None => {
                self.insert_specialization(generic_type, type_arguments, specialized_type);
                specialized_type
            }
            Some(reserved) => {
                let t = *self.types.get(specialized_type);
                self.set_type(
                    reserved,
                    t,
                    Some(GenericInstanceInfo {
                        generic_parent: generic_type,
                        type_args: type_arguments,
                    }),
                    Some(defn_info),
                );
                self.discard_type_if_last(specialized_type);
                reserved
            }
        };
        if log::log_enabled!(log::Level::Debug) {
            eprintln!(
                "instantiated\n{} with params\n{} got expanded type:\n{}\n\n",
                self.type_id_to_string_ext(inner, dump::TypeDisplayMode::Expand),
                self.pretty_print_types(self.get_type_slice(type_arguments), ", "),
                self.type_id_to_string_ext(result_type, dump::TypeDisplayMode::Expand)
            );
        }
        result_type
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
    ) -> TypeId {
        let args = self.intern_type_slice(type_args);
        if let Some(existing) = self.get_specialization(generic_parent, args) {
            return existing;
        }
        let type_id = self.reserve_instance_id(generic_parent, args);
        self.type_defn_context.pending_instances.push(PendingRecursiveInstance {
            type_id,
            generic_parent,
            type_args: args,
        });
        self.type_defn_context.recursive_mentions.push(type_id);
        type_id
    }

    /// Runs when the type defn stack empties: every defn in the cluster is complete, so fill
    /// the reserved recursive instances by substitution, then check the cluster for infinite
    /// (indirection-free) cycles
    pub(super) fn finish_type_defn_cluster(&mut self) {
        let pending = std::mem::take(&mut self.type_defn_context.pending_instances);
        for p in &pending {
            let gen_type = self.types.get(p.generic_parent).expect_generic();
            let gen_params = gen_type.params;
            let inner = gen_type.inner;
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
            let mut substitution_pairs: SV8<TypeSubstitutionPair> = smallvec![];
            for (param, arg) in
                self.mem.getn(gen_params).iter().zip(self.get_type_slice(p.type_args))
            {
                substitution_pairs.push(spair! { *param => *arg });
            }
            let specialized = self.substitute_in_type_ext(
                inner,
                &substitution_pairs,
                Some(p.generic_parent),
                Some(defn_info),
            );
            let t = *self.types.get(specialized);
            self.set_type(
                p.type_id,
                t,
                Some(GenericInstanceInfo {
                    generic_parent: p.generic_parent,
                    type_args: p.type_args,
                }),
                Some(defn_info),
            );
        }

        if !self.type_defn_context.recursive_mentions.is_empty() {
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
        self.type_defn_context.reset()
    }

    pub(super) fn check_type_args_against_params(
        &mut self,
        params: TypeIdSlice,
        parsed_args: &[NamedTypeArg],
        scope_id: ScopeId,
        context: EvalTypeExprContext,
    ) -> K1Result<(TypeIdSlice, TmpList<TypeSubstitutionPair>)> {
        let mut type_arguments = self.mem.new_list(params.len());
        let mut subst_pairs: List<TypeSubstitutionPair, MemTmp> = self.tmp.new_list(params.len());
        for (param, parsed_arg) in self.mem.getn(params).iter().zip(parsed_args) {
            let Some(parsed_arg_expr) = parsed_arg.type_expr else {
                kbail!(self, parsed_arg.span, "Wildcard _ type not accepted here");
            };
            let arg_type_id =
                self.eval_type_expr_ext(parsed_arg_expr, scope_id, context.descended())?;
            subst_pairs.push(spair! { *param => arg_type_id });
            type_arguments.push(arg_type_id);
        }

        // Repeat the loop, this time checking constraints
        // now that we have all the pairs
        for ((param, parsed_arg), arg_type) in
            self.mem.getn(params).iter().zip(parsed_args).zip(type_arguments.as_slice())
        {
            self.check_type_constraints(
                self.get_type_parameter(*param).name,
                *param,
                *arg_type,
                subst_pairs.as_slice(),
                scope_id,
                parsed_arg.span,
            )?;
        }

        Ok((type_arguments.to_slice(), subst_pairs))
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
                    if self.declare_global(parsed_id, defn_scope)?.is_none() {
                        return Ok(None);
                    }
                    *self.global_ast_mappings.get(&parsed_id).unwrap()
                }
                None => return Ok(None),
            },
        };
        if self.globals.get(global_id).initial_value.is_pending() {
            let ast_id = self.globals.get(global_id).ast_id;
            self.eval_global_body(ast_id)?;
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
        if !ty_app.name.path.is_empty() {
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
        let ParsedTypeExpr::StaticLiteral(ParsedLiteral::Numeric(size_lit)) =
            *self.ast.type_exprs.get(size_expr)
        else {
            kbail!(self, ty_app.span, "Expected a static literal for opaque size");
        };
        let ParsedTypeExpr::StaticLiteral(ParsedLiteral::Numeric(align_lit)) =
            *self.ast.type_exprs.get(align_expr)
        else {
            kbail!(self, ty_app.span, "Expected a static literal for opaque alignment");
        };
        let TypedIntValue::U32(size) =
            self.eval_integer_value(size_lit.text_span, Some(IntegerType::U32.type_id()))?
        else {
            kbail!(self, size_lit.span, "Expected a u32 value for opaque size");
        };
        let TypedIntValue::U32(align) =
            self.eval_integer_value(align_lit.text_span, Some(IntegerType::U32.type_id()))?
        else {
            kbail!(self, align_lit.span, "Expected a u32 value for opaque alignment");
        };

        if align == 0 || !align.is_power_of_two() || align > 128 {
            kbail!(
                self,
                align_lit.span,
                "Alignment must be a non-zero power of two, not exceeding 128, got {}",
                align
            );
        }

        let opaque_type = self.add_type(Type::Opaque(OpaqueType { size, align }), None, None);
        Ok(Some(opaque_type))
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
        if !ty_app.name.path.is_empty() {
            kbail!(
                self,
                ty_app.span,
                "Expected 'vector' with no namespace, got '{}'",
                &ty_app.name
            );
        }
        if ty_app.args.len() != 2 {
            kbail!(
                self,
                ty_app.span,
                "Expected 2 type parameters for vector, got {}",
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
            kbail!(self, ty_app.span, "Vector lane count must be a static type");
        };
        if static_type.family_type_id != I64_TYPE_ID {
            kbail!(
                self,
                ty_app.span,
                "Vector lane count must be an int; got {}",
                static_type.family_type_id
            );
        }

        self.validate_vector_parts(element_type, size_type_id, ty_app.span)?;

        let vector_type = Type::Vector(VectorType { element_type, size_type: size_type_id });
        Ok(Some(self.add_type_anon(vector_type)))
    }

    /// Checks whatever is concrete; abstract element/lane-count parts are checked
    /// again at instantiation sites (IR lowering re-validates for intrinsics)
    pub(super) fn validate_vector_parts(
        &self,
        element_type: TypeId,
        size_type_id: TypeId,
        span: SpanId,
    ) -> K1Result<()> {
        let element_size = match self.types.get(element_type) {
            Type::Integer(it) => Some(it.width().bits() / 8),
            Type::Char => Some(1),
            Type::Float(ft) => Some(ft.size().bits() / 8),
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
        self.substitute_in_type_ext(type_id, substitution_pairs, None, None)
    }

    pub(super) fn substitute_in_type_ext(
        &mut self,
        type_id: TypeId,
        substitution_pairs: &[TypeSubstitutionPair],
        generic_parent_to_attach: Option<TypeId>,
        defn_info_to_attach: Option<TypeDefnInfo>,
    ) -> TypeId {
        // The empty substitution is the identity
        if substitution_pairs.is_empty()
            && generic_parent_to_attach.is_none()
            && defn_info_to_attach.is_none()
        {
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
        let res = self.substitute_in_type_ext_inner(
            type_id,
            substitution_pairs,
            from_kinds,
            generic_parent_to_attach,
            defn_info_to_attach,
        );
        self.tmp.reset_to(tmp_mark);
        res
    }

    pub(super) fn substitute_in_type_ext_inner(
        &mut self,
        type_id: TypeId,
        substitution_pairs: &[TypeSubstitutionPair],
        from_kinds: SubstitutionFromKinds,
        generic_parent_to_attach: Option<TypeId>,
        defn_info_to_attach: Option<TypeDefnInfo>,
    ) -> TypeId {
        if generic_parent_to_attach.is_none()
            && defn_info_to_attach.is_none()
            && from_kinds.no_from_occurs_in(self.type_variable_counts.get(type_id))
        {
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
                let new_type = self.substitute_in_type_ext_inner(
                    *prev_arg,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                if new_type != *prev_arg {
                    any_change = true;
                }
                new_type_args.push(new_type);
            }
            let parent_pending = self
                .type_defn_context
                .stack
                .iter()
                .any(|e| e.reserved_type_id == Some(generic_parent));
            // On no change, or a cache hit, we avoid committing the args to the arena
            return if parent_pending {
                // The parent generic is still being defined (mutual recursion), so its
                // template cannot be substituted yet; defer to a reserved instance id
                self.get_or_reserve_recursive_instance(generic_parent, new_type_args.as_slice())
            } else if any_change {
                self.instantiate_generic_type(generic_parent, new_type_args.as_slice())
            } else {
                self.instantiate_generic_type(generic_parent, original_args)
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
                let original_defn_info = self.get_defn_info(type_id);
                let defn_info_to_use = defn_info_to_attach.or(original_defn_info);
                // Build the candidate in tmp; only commit to the arena if something changed
                let mut new_fields = self.tmp.new_list(old_fields.len());
                for field in self.mem.getn(old_fields) {
                    let new_field_type_id = self.substitute_in_type_ext_inner(
                        field.type_id,
                        substitution_pairs,
                        from_kinds,
                        None,
                        None,
                    );
                    if new_field_type_id != field.type_id {
                        any_change = true;
                    }
                    let mut new_field = *field;
                    new_field.type_id = new_field_type_id;
                    new_fields.push(new_field);
                }
                if any_change {
                    let generic_instance_info = match generic_parent_to_attach {
                        Some(parent) => {
                            let mut args: List<TypeId, MemTmp> =
                                self.tmp.new_list(substitution_pairs.len() as u32);
                            for pair in substitution_pairs {
                                args.push(pair.to);
                            }
                            Some(GenericInstanceInfo {
                                generic_parent: parent,
                                type_args: self.intern_type_slice(args.as_slice()),
                            })
                        }
                        None => self.get_instance_info(type_id).cloned(),
                    };

                    let new_fields_handle = self.mem.pushn(new_fields.as_slice());
                    let specialized_struct = StructType { fields: new_fields_handle, record_kind };
                    self.add_type(
                        Type::Struct(specialized_struct),
                        defn_info_to_use,
                        generic_instance_info,
                    )
                } else {
                    type_id
                }
            }
            Type::Sum(e) => {
                let original_tag_type = e.tag_type;
                let old_variants = e.variants;
                let mut any_changed = false;
                let original_defn_info = self.get_defn_info(type_id);
                let defn_info_to_use = defn_info_to_attach.or(original_defn_info);
                // Build the candidate in tmp; only commit to the arena if something changed
                let mut new_variants = self.tmp.new_list(old_variants.len());
                for variant in self.mem.getn(old_variants) {
                    let mut new_variant = *variant;
                    if let Some(p) = variant.payload {
                        let new_payload_id = self.substitute_in_type_ext_inner(
                            p,
                            substitution_pairs,
                            from_kinds,
                            None,
                            None,
                        );
                        if new_payload_id != p {
                            any_changed = true;
                            new_variant.payload = Some(new_payload_id)
                        };
                    }
                    new_variants.push(new_variant);
                }
                if any_changed {
                    let generic_instance_info = match generic_parent_to_attach {
                        Some(parent) => {
                            let mut args: List<TypeId, MemTmp> =
                                self.tmp.new_list(substitution_pairs.len() as u32);
                            for pair in substitution_pairs {
                                args.push(pair.to);
                            }
                            Some(GenericInstanceInfo {
                                generic_parent: parent,
                                type_args: self.intern_type_slice(args.as_slice()),
                            })
                        }
                        None => self.get_instance_info(type_id).cloned(),
                    };
                    let new_variants_handle = self.mem.pushn(new_variants.as_slice());
                    let new_sum =
                        SumType { variants: new_variants_handle, tag_type: original_tag_type };
                    let new_sum_id =
                        self.add_type(Type::Sum(new_sum), defn_info_to_use, generic_instance_info);
                    new_sum_id
                } else {
                    type_id
                }
            }
            Type::Opaque(_) => type_id,
            Type::Reference(reference) => {
                let reference = *reference;
                let new_inner = self.substitute_in_type_ext_inner(
                    reference.inner_type,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                if new_inner != reference.inner_type {
                    self.add_reference_type(new_inner)
                } else {
                    type_id
                }
            }
            Type::TypeParameter(_type_param) => type_id,
            Type::FunctionTypeParameter(ftp) => {
                let function_type_id = ftp.function_type;
                let new_fn_type = self.substitute_in_type_ext_inner(
                    function_type_id,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                if new_fn_type != function_type_id {
                    let type_param = self.types.get(type_id).as_function_type_parameter().unwrap();
                    self.add_type_anon(Type::FunctionTypeParameter(FunctionTypeParameter {
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
                let old_params = fun_type.physical_params;
                let new_return_type = self.substitute_in_type_ext_inner(
                    old_return_type,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                if new_return_type != old_return_type {
                    any_new = true
                };
                let mut new_params: List<FnParamType, _> = self.tmp.new_list(old_params.len());
                for param in self.mem.getn(old_params) {
                    let new_param_type = self.substitute_in_type_ext_inner(
                        param.type_id,
                        substitution_pairs,
                        from_kinds,
                        None,
                        None,
                    );
                    if new_param_type != param.type_id {
                        any_new = true;
                    }
                    let new_param = FnParamType {
                        name: param.name,
                        type_id: new_param_type,
                        is_context: param.is_context,
                        is_lambda_env: param.is_lambda_env,
                        is_macro_code: param.is_macro_code,
                    };
                    new_params.push(new_param);
                }
                if any_new {
                    let new_params_handle = self.mem.pushn(new_params.as_slice());
                    let new_fun_type = FunctionType {
                        physical_params: new_params_handle,
                        return_type: new_return_type,
                        is_lambda,
                    };
                    let new_function_type_id = self.add_type_anon(Type::Function(new_fun_type));
                    new_function_type_id
                } else {
                    type_id
                }
            }
            Type::FunctionPointer(fp) => {
                let fp = *fp;
                let new_fn_type = self.substitute_in_type_ext_inner(
                    fp.function_type_id,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
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
                let new_fn_type = self.substitute_in_type_ext_inner(
                    fn_type,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                if new_fn_type != fn_type {
                    self.add_lambda_object(new_fn_type, parsed_id)
                } else {
                    type_id
                }
            }
            Type::AbilityObject(ao) => {
                let ao = *ao;
                let mut any_change = false;
                let mut new_args = self.tmp.new_list(ao.impl_arguments.len());
                for arg in self.mem.getn(ao.impl_arguments) {
                    let new_arg_type = self.substitute_in_type_ext_inner(
                        *arg,
                        substitution_pairs,
                        from_kinds,
                        None,
                        None,
                    );
                    if new_arg_type != *arg {
                        any_change = true;
                    }
                    new_args.push(new_arg_type);
                }
                if any_change {
                    let impl_arguments = self.mem.pushn(new_args.as_slice());
                    let new_signature = TypedAbilitySignature {
                        specialized_ability_id: ao.specialized_ability_id,
                        impl_arguments,
                    };
                    match self.eval_dyn_ability_object_type(new_signature, SpanId::NONE) {
                        Ok(t) => t,
                        Err(e) => {
                            self.ice("dyn ability substitution produced invalid object", Some(&e))
                        }
                    }
                } else {
                    type_id
                }
            }
            Type::StaticValue(value_type) => {
                if value_type.value_id.is_some() {
                    // Can't substitute inside the type "static[string; "hello"]"
                    // But you can inside type "static[T; _]"
                    type_id
                } else {
                    let family_type = value_type.family_type_id;
                    let new_inner_type = self.substitute_in_type_ext_inner(
                        family_type,
                        substitution_pairs,
                        from_kinds,
                        None,
                        None,
                    );
                    if new_inner_type == family_type {
                        type_id
                    } else {
                        self.add_type_anon(Type::StaticValue(StaticValueType {
                            family_type_id: new_inner_type,
                            value_id: None,
                        }))
                    }
                }
            }
            Type::Array(arr) => {
                let arr = *arr;
                let element_type = arr.element_type;
                let new_element_type = self.substitute_in_type_ext_inner(
                    element_type,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                let new_size_type = self.substitute_in_type_ext_inner(
                    arr.size_type,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                if new_element_type == element_type && new_size_type == arr.size_type {
                    type_id // No change needed
                } else {
                    // Create new Array type with substituted element type
                    let new_array_type = Type::Array(ArrayType {
                        element_type: new_element_type,
                        size_type: new_size_type,
                    });
                    self.add_type_anon(new_array_type)
                }
            }
            Type::Vector(vec) => {
                let vec = *vec;
                let new_element_type = self.substitute_in_type_ext_inner(
                    vec.element_type,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                let new_size_type = self.substitute_in_type_ext_inner(
                    vec.size_type,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                if new_element_type == vec.element_type && new_size_type == vec.size_type {
                    type_id
                } else {
                    self.add_type_anon(Type::Vector(VectorType {
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
        // infinite. Guarding on every visited id (not just the cluster's recursive
        // mentions) is what terminates the walk on self-referential instances minted
        // by instantiate_generic_type_miss, which are in no mentions list
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
