// Copyright (c) 2026 knix
// All rights reserved.

use super::*;

impl TypedProgram {
    pub fn type_id_from_raw(&mut self, raw: vm::k1_types::TypeId, span: Span) -> K1Result<TypeId> {
        let type_id_ok = u32::try_from(raw.inner)
            .ok()
            .and_then(TypeId::from_u32)
            .filter(|id| self.types.get_opt(*id).is_some());
        let Some(type_id) = type_id_ok else {
            kbail!(self, span, "unknown type id {}", raw.inner);
        };
        Ok(type_id)
    }

    pub fn make_struct_raw(
        &mut self,
        record_kind: RecordKind,
        raw_field_descs: &[vm::k1_types::K1MakeStructField],
        span: Span,
    ) -> K1Result<TypeId> {
        let mut fields = self.tmp.new_list(raw_field_descs.len() as u32);
        for desc in raw_field_descs {
            let name_str = match unsafe { desc.name.to_str() } {
                Ok(s) => s,
                Err(msg) => kbail!(self, span, "make-struct field name: {}", msg),
            };
            let name = self.ast.idents.intern(name_str);
            let type_id = self.type_id_from_raw(desc.type_id, span)?;
            fields.push(StructTypeField { name, type_id, span });
        }
        let new_type_id = self.add_struct_type(fields.as_slice(), record_kind)?;
        self.register_type_metainfo(new_type_id);
        Ok(new_type_id)
    }

    pub fn make_either_raw(
        &mut self,
        explicit_tag_type: Option<vm::k1_types::TypeId>,
        raw_variant_descs: &[vm::k1_types::K1MakeEitherVariant],
        span: Span,
    ) -> K1Result<TypeId> {
        let explicit_tag_type = match explicit_tag_type {
            None => None,
            Some(raw) => Some((self.type_id_from_raw(raw, span)?, span)),
        };
        let variant_count = raw_variant_descs.len() as u32;
        let tag_type = self.choose_either_tag_type(explicit_tag_type, variant_count, span)?;
        let mut variants = self.tmp.new_list(variant_count);
        for desc in raw_variant_descs {
            let name_str = match unsafe { desc.name.to_str() } {
                Ok(s) => s,
                Err(msg) => kbail!(self, span, "make-either variant name: {}", msg),
            };
            let name = self.ast.idents.intern(name_str);
            let payload = if desc.payload.tag == 0 {
                None
            } else {
                Some(self.type_id_from_raw(desc.payload.payload, span)?)
            };
            let tag = if desc.tag.tag == 0 {
                None
            } else {
                let int_value = desc.tag.payload;
                let Some(kind) = IntegerType::from_tag(int_value.kind) else {
                    kbail!(self, span, "make-either: bad int-value kind {}", int_value.kind as u32);
                };
                Some(TypedIntValue::from_u64_bits(kind, int_value.value_bits))
            };
            variants.push(EitherVariantPart { name, name_span: span, payload, tag });
        }
        let new_type_id = self.add_either_type(tag_type, variants.as_slice(), false, span)?;
        self.register_type_metainfo(new_type_id);
        Ok(new_type_id)
    }

    pub fn make_reference_raw(&mut self, inner: TypeId) -> TypeId {
        let new_type_id = if let Type::Function(_) = self.types.get(inner) {
            self.add_function_pointer_type(inner)
        } else {
            self.add_reference_type(inner)
        };
        self.register_type_metainfo(new_type_id);
        new_type_id
    }

    pub fn make_array_raw(
        &mut self,
        element_type: TypeId,
        size: i64,
        span: Span,
    ) -> K1Result<TypeId> {
        if size < 0 {
            kbail!(self, span, "make-array: negative size {}", size);
        }
        let size_value_id = self.static_values.add_size(size);
        let size_type = self.add_type(Type::StaticValue(StaticValueType {
            family_type_id: I64_TYPE_ID,
            value_id: Some(size_value_id),
        }));
        let new_type_id = self.add_type(Type::Array(ArrayType { element_type, size_type }));
        self.register_type_metainfo(new_type_id);
        Ok(new_type_id)
    }

    pub fn make_fn_raw(
        &mut self,
        raw_param_types: &[vm::k1_types::TypeId],
        return_type: TypeId,
        span: Span,
    ) -> K1Result<TypeId> {
        let mut params: TmpList<FnParamType> = self.tmp.new_list(raw_param_types.len() as u32);
        for raw in raw_param_types {
            let type_id = self.type_id_from_raw(*raw, span)?;
            params.push(FnParamType { type_id, kind: FnParamKind::Plain });
        }
        let new_type_id = self.add_function_type(params.as_slice(), return_type, false);
        self.register_type_metainfo(new_type_id);
        Ok(new_type_id)
    }

    pub fn make_generic_instance_raw(
        &mut self,
        parent: TypeId,
        raw_args: &[vm::k1_types::TypeId],
        span: Span,
    ) -> K1Result<TypeId> {
        let Some(generic) = self.types.get(parent).as_generic() else {
            kbail!(self, span, "make-instance: {} is not a generic type", parent);
        };
        let params = generic.params;
        if raw_args.len() != params.len() as usize {
            kbail!(
                self,
                span,
                "make-instance: {} expects {} type arguments, got {}",
                parent,
                params.len(),
                raw_args.len()
            );
        }
        let mut args: SV4<TypeId> = smallvec![];
        for raw in raw_args {
            args.push(self.type_id_from_raw(*raw, span)?);
        }
        let subst_pairs: SV4<TypeSubstitutionPair> =
            self.zip_types_to_subst_pairs(self.mem.getn(params), &args);
        let defn_scope =
            self.namespaces.get(self.get_defn_info(parent).unwrap().namespace_id).scope_id;
        self.check_type_args_constraints(&subst_pairs, span, defn_scope)?;
        let new_type_id = self.instantiate_generic_type(parent, &args);
        self.register_type_metainfo(new_type_id);
        Ok(new_type_id)
    }

    pub fn impl_of_raw(
        &mut self,
        self_type_id: TypeId,
        raw_ability_id: u32,
        raw_impl_args: &[vm::k1_types::TypeId],
        span: Span,
    ) -> K1Result<StaticValueId> {
        let ability_id_ok =
            AbilityId::from_u32(raw_ability_id).filter(|id| self.abilities.get_opt(*id).is_some());
        let Some(ability_id) = ability_id_ok else {
            kbail!(self, span, "impl-of: unknown ability id {}", raw_ability_id);
        };
        let ability = self.abilities.get(ability_id);
        if matches!(ability.kind, TypedAbilityKind::Generic { .. }) {
            kbail!(self, span, "impl-of: {} needs its ability arguments", ability.name);
        }
        let scope_id = ability.scope_id;
        let impl_params = ability.parameters;
        if raw_impl_args.len() != impl_params.len() as usize {
            kbail!(
                self,
                span,
                "impl-of: {} has {} impl params, got {} args",
                ability.name,
                impl_params.len(),
                raw_impl_args.len()
            );
        }
        if self.get_type_variable_counts(self_type_id).is_abstract()
            || self.types.get(self_type_id).as_generic().is_some()
        {
            kbail!(self, span, "impl-of needs a concrete type, got {}", self_type_id);
        }
        let mut impl_args = self.tmp.new_list(impl_params.len());
        for raw in raw_impl_args {
            impl_args.push(self.type_id_from_raw(*raw, span)?);
        }
        let signature = TypedAbilitySignature {
            specialized_ability_id: ability_id,
            impl_arguments: self.intern_type_slice(impl_args.as_slice()),
        };

        let impl_info_type_id = self.builtin_types.impl_info();
        let opt_type_id = self.synth_optional_type(impl_info_type_id);
        let Ok((impl_handle, _)) = self.find_or_generate_ability_impl_for_signature(
            self_type_id,
            signature,
            false,
            scope_id,
            span,
        ) else {
            return Ok(synth_static_option(&mut self.static_values, opt_type_id, None));
        };
        let found = self.ability_impls.get(impl_handle.full_impl_id);
        let found_impl_args = self.get_type_slice(found.impl_arguments);
        let kind_index = match found.kind {
            AbilityImplKind::Concrete => 0,
            AbilityImplKind::BlanketInstance { .. } => 1,
            AbilityImplKind::Builtin => 2,
            AbilityImplKind::Blanket | AbilityImplKind::TypeParamConstraint => {
                self.ice_span(span, format!("impl-of resolved to a {:?} impl", found.kind))
            }
        };

        // kind: impl-kind
        let kind_type_id = self.get_struct_field(impl_info_type_id, 0).type_id;
        let kind_member = self.types.get(kind_type_id).expect_enum().member_values;
        let kind_value = self.mem.get_nth(kind_member, kind_index).int_value;
        let kind_value_id = self.static_values.add(StaticValue::Enum(kind_type_id, kind_value));

        // args: span[{ name: string, type-id }]
        let args_span_type_id = self.get_struct_field(impl_info_type_id, 1).type_id;
        let arg_struct_type_id = self.get_as_span_instance(args_span_type_id).unwrap();
        let mut arg_value_ids = self.tmp.new_list(found_impl_args.len() as u32);
        for (param, &arg) in self.mem.getn(impl_params).iter().zip(found_impl_args) {
            self.register_type_metainfo(arg);
            let name_value_id = self.static_values.add_string(param.name);
            let type_id_value_id = self.add_type_id_value(arg);
            arg_value_ids.push(
                self.static_values
                    .add_struct_from_slice(arg_struct_type_id, &[name_value_id, type_id_value_id]),
            );
        }
        let args_value_id = self.add_static_container_from_ids(
            StaticContainerKind::Span,
            args_span_type_id,
            arg_value_ids.as_slice(),
        );
        let info_value_id = self
            .static_values
            .add_struct_from_slice(impl_info_type_id, &[kind_value_id, args_value_id]);
        Ok(synth_static_option(&mut self.static_values, opt_type_id, Some(info_value_id)))
    }

    pub(super) fn get_type_info(&mut self, type_id: TypeId) -> StaticValueId {
        if let Some(existing) = self.type_infos.get(&type_id) {
            return *existing;
        }
        let reserved_id = self.static_values.pool.reserve_id();
        self.type_infos.insert(type_id, reserved_id);

        let family_type_id = self.get_type_family_type(type_id);
        let name_value_id = self.build_type_name(type_id);
        let schema_value_id = self.build_type_schema(family_type_id);
        let instance_value_id = self.build_instance_info(family_type_id);
        let layout_value_id = self.build_layout(type_id);
        let zero_safe_value_id =
            StaticValuePool::get_bool_id(self.get_type_variable_counts(type_id).is_zero_safe);
        let fields = self.static_values.mem.pushn(&[
            name_value_id,
            schema_value_id,
            instance_value_id,
            layout_value_id,
            zero_safe_value_id,
        ]);
        let type_info_type_id = self.builtin_types.type_info();
        self.static_values.set(
            reserved_id,
            StaticValue::Struct(StaticStruct { type_id: type_info_type_id, fields }),
        );
        reserved_id
    }

    fn build_layout(&mut self, type_id: TypeId) -> StaticValueId {
        let layout_type_id = self.builtin_types.types_layout();
        let layout = self.get_layout(type_id).unwrap_or(Layout::ZERO_SIZED);
        let size_value_id = self.static_values.add_size(layout.size as i64);
        let align_value_id = self.static_values.add_size(layout.align as i64);
        self.static_values.add_struct_from_slice(layout_type_id, &[size_value_id, align_value_id])
    }

    fn build_instance_info(&mut self, type_id: TypeId) -> StaticValueId {
        let type_info_type_id = self.builtin_types.type_info();
        let instance_opt_type_id = self.get_struct_field(type_info_type_id, 2).type_id;
        let Some(info) = self.get_instance_info(type_id).copied() else {
            return synth_static_option(&mut self.static_values, instance_opt_type_id, None);
        };
        let instance_info_type_id = self.get_as_opt_instance(instance_opt_type_id).unwrap();
        let args_span_type_id = self.get_struct_field(instance_info_type_id, 1).type_id;

        self.register_type_metainfo(info.generic_parent);
        let parent_value_id = self.add_type_id_value(info.generic_parent);

        let mut type_args: SV4<TypeId> = smallvec![];
        for arg in self.get_type_slice(info.type_args) {
            type_args.push(*arg);
        }
        let mut arg_value_ids = self.tmp.new_list(type_args.len() as u32);
        for arg in type_args {
            self.register_type_metainfo(arg);
            arg_value_ids.push(self.add_type_id_value(arg));
        }
        let args_span_value_id = self.add_static_container_from_ids(
            StaticContainerKind::Span,
            args_span_type_id,
            arg_value_ids.as_slice(),
        );
        let instance_value_id = self
            .static_values
            .add_struct_from_slice(instance_info_type_id, &[parent_value_id, args_span_value_id]);
        synth_static_option(&mut self.static_values, instance_opt_type_id, Some(instance_value_id))
    }

    fn build_type_schema(&mut self, type_id: TypeId) -> StaticValueId {
        let type_schema_type_id = self.builtin_types.types_type_schema.unwrap();
        let type_schema = *self.types.get(type_schema_type_id).expect_sum();
        let int_kind_type_id = self.builtin_types.types_int_kind.unwrap();
        let float_kind_type_id = self.builtin_types.types_float_kind.unwrap();
        let get_schema_variant = |self_: &TypedProgram, ident| {
            self_.sum_variant_by_name(type_schema.variants, ident).unwrap()
        };
        let make_variant =
            |self_: &TypedProgram, name: StringId, payload: Option<StaticValueId>| {
                let v = get_schema_variant(self_, name);
                StaticSum { sum_type_id: type_schema_type_id, variant_index: v.index, payload }
            };

        let typ = self.types.get(type_id);
        let schema_static_sum = match typ {
            Type::Char => make_variant(self, self.ast.idents.b.char, None),
            Type::Bool => make_variant(self, self.ast.idents.b.bool, None),
            Type::Pointer => make_variant(self, self.ast.idents.b.ptr, None),
            Type::Integer(integer_type) => {
                let int_kind_enum_value =
                    TypedProgram::make_int_kind(int_kind_type_id, *integer_type);

                let payload_value_id = self.static_values.add(int_kind_enum_value);
                let enum_value = make_variant(self, self.ast.idents.b.int, Some(payload_value_id));
                enum_value
            }
            Type::Float(float_type) => {
                let float_kind_enum_value =
                    TypedProgram::make_float_kind(float_kind_type_id, *float_type);
                let payload_value_id = self.static_values.add(float_kind_enum_value);
                make_variant(self, self.ast.idents.b.float, Some(payload_value_id))
            }
            Type::Enum(enum_type) => {
                let target_enum_members = enum_type.member_values;
                let enum_schema_payload_type_id =
                    get_schema_variant(self, self.ast.idents.b.enum_).payload.unwrap();
                let values_span_type_id =
                    self.get_struct_field(enum_schema_payload_type_id, 1).type_id;
                let value_struct_type_id = self.get_as_span_instance(values_span_type_id).unwrap();
                let int_type = enum_type.int_type;
                let partial = enum_type.partial;
                let int_type_value_id =
                    self.static_values.add(TypedProgram::make_int_kind(int_kind_type_id, int_type));
                let mut member_values = self.tmp.new_list(target_enum_members.len());
                // values: span[{
                //   name: string,
                //   value: int-value,
                // }]
                for member_value in self.mem.getn(target_enum_members) {
                    let name_value_id = self.static_values.add_string(member_value.name);

                    let int_value_type_id = self.builtin_types.types_int_value.unwrap();
                    let int_value_sum = self.types.get(int_value_type_id).expect_sum();
                    let int_value_sum_value = TypedProgram::make_int_value(
                        &mut self.static_values,
                        int_value_type_id,
                        self.mem.getn(int_value_sum.variants),
                        member_value.int_value,
                    );
                    let int_value_sum_value_id =
                        self.static_values.add(StaticValue::Sum(int_value_sum_value));

                    member_values.push(self.static_values.add_struct_from_slice(
                        value_struct_type_id,
                        &[
                            // name: string,
                            name_value_id,
                            // value: int-value,
                            int_value_sum_value_id,
                        ],
                    ))
                }
                let variants_span_value_id = self.add_static_container_from_ids(
                    StaticContainerKind::Span,
                    values_span_type_id,
                    member_values.as_slice(),
                );
                let partial_value_id = StaticValuePool::get_bool_id(partial);
                let payload_value_id = self.static_values.add_struct_from_slice(
                    enum_schema_payload_type_id,
                    &[int_type_value_id, variants_span_value_id, partial_value_id],
                );
                make_variant(self, self.ast.idents.b.enum_, Some(payload_value_id))
            }
            Type::Struct(_struct_type) if type_id == self.builtin_types.string() => {
                make_variant(self, self.ast.idents.b.string, None)
            }
            Type::Struct(struct_type) => {
                let record_kind = struct_type.record_kind;
                let variant_name = match record_kind {
                    RecordKind::Struct | RecordKind::Packed => self.ast.idents.b.struct_,
                    RecordKind::Union => self.ast.idents.b.union,
                };
                // { fields: span[{ ... }], packed: bool } or { fields: span[{ ... }] }
                let struct_schema_payload_type_id =
                    get_schema_variant(self, variant_name).payload.unwrap();
                let struct_type_fields = struct_type.fields;
                let struct_schema_fields_span_type_id =
                    self.get_struct_field(struct_schema_payload_type_id, 0).type_id;
                let struct_schema_field_item_struct_type_id =
                    self.get_as_span_instance(struct_schema_fields_span_type_id).unwrap();

                // for offsets
                let struct_layout = match record_kind {
                    RecordKind::Struct | RecordKind::Packed => self.get_struct_layout(type_id),
                    RecordKind::Union => &[],
                };
                // { name: string), typeId: u64, offset: size }
                let mut field_values = self.tmp.new_list(struct_type_fields.len());
                for (index, f) in self.mem.getn(struct_type_fields).iter().enumerate() {
                    let name_string_value_id = self.static_values.add_string(f.name);

                    // We need to ensure that any and all typeIds that we share with the user
                    // are available at runtime, by calling these functions at least once.
                    self.register_type_metainfo(f.type_id);

                    let type_id_value_id = self.add_type_id_value(f.type_id);
                    let offset_u32 =
                        if struct_layout.is_empty() { 0 } else { struct_layout[index].offset };
                    let offset_value_id = self.static_values.add_size(offset_u32 as i64);
                    field_values.push(self.static_values.add_struct_from_slice(
                        struct_schema_field_item_struct_type_id,
                        &[name_string_value_id, type_id_value_id, offset_value_id],
                    ));
                }
                let span_value_id = self.add_static_container_from_ids(
                    StaticContainerKind::Span,
                    struct_schema_fields_span_type_id,
                    field_values.as_slice(),
                );
                let packed_value_id =
                    StaticValuePool::get_bool_id(record_kind == RecordKind::Packed);
                let payload_values: &[StaticValueId] = match record_kind {
                    RecordKind::Struct | RecordKind::Packed => &[span_value_id, packed_value_id],
                    RecordKind::Union => &[span_value_id],
                };
                let payload = self
                    .static_values
                    .add_struct_from_slice(struct_schema_payload_type_id, payload_values);
                make_variant(self, variant_name, Some(payload))
            }
            Type::Reference(reference_type) => {
                let reference_type = *reference_type;
                let reference_schema_payload_type_id =
                    get_schema_variant(self, self.ast.idents.b.reference).payload.unwrap();
                // { innerTypeId: type-id, mutable: bool }
                let inner_type_id_value_id = self.add_type_id_value(reference_type.inner_type);

                // We need to ensure that any and all typeIds that we share with the user
                // are available at runtime, by calling these functions at least once.
                self.register_type_metainfo(reference_type.inner_type);

                let payload_struct_id = self.static_values.add_struct_from_slice(
                    reference_schema_payload_type_id,
                    &[inner_type_id_value_id],
                );
                make_variant(self, self.ast.idents.b.reference, Some(payload_struct_id))
            }
            Type::Array(ArrayType { element_type, size_type })
            | Type::Vector(ArrayType { element_type, size_type }) => {
                let (element_type, size_type) = (*element_type, *size_type);
                let variant_name = if matches!(typ, Type::Vector(_)) {
                    self.ast.idents.b.vector
                } else {
                    self.ast.idents.b.array
                };
                let concrete_count = self.get_concrete_count_of_array(size_type);
                let schema_payload_type_id =
                    get_schema_variant(self, variant_name).payload.unwrap();
                let element_type_id_value_id = self.add_type_id_value(element_type);
                self.register_type_metainfo(element_type);

                let maybe_concrete_size_value_id = match concrete_count {
                    None => None,
                    Some(size) => Some(self.static_values.add_size(size)),
                };
                let option_size = self.synth_optional_type(SIZE_TYPE_ID);
                let size_value_id = synth::synth_static_option(
                    &mut self.static_values,
                    option_size,
                    maybe_concrete_size_value_id,
                );

                let payload_struct_id = self.static_values.add_struct_from_slice(
                    schema_payload_type_id,
                    &[element_type_id_value_id, size_value_id],
                );
                make_variant(self, variant_name, Some(payload_struct_id))
            }
            Type::Sum(typed_sum) => {
                let target_sum_variants = typed_sum.variants;
                let either_payload_type_id =
                    get_schema_variant(self, self.ast.idents.b.either).payload.unwrap();
                let variants_span_type_id =
                    self.get_struct_field(either_payload_type_id, 2).type_id;
                let variant_struct_type_id =
                    self.get_as_span_instance(variants_span_type_id).unwrap();
                let tag_type = typed_sum.tag_type;
                let tag_type_value_id =
                    self.static_values.add(TypedProgram::make_int_kind(int_kind_type_id, tag_type));
                let sum_agg_id = self.get_physical_type(type_id).unwrap().expect_agg();
                let sum_pt = self.agg_types.get(sum_agg_id).agg_type.expect_sum();
                let payload_offset = sum_pt.payload_offset;
                let payload_offset_value_id =
                    self.static_values.add_size(to_k1_size_usize(payload_offset as usize));
                let mut variant_values = self.tmp.new_list(target_sum_variants.len());
                for variant in self.mem.getn(target_sum_variants) {
                    let name_value_id = self.static_values.add_string(variant.name);

                    let int_value_type_id = self.builtin_types.types_int_value.unwrap();
                    let int_value_enum = self.types.get(int_value_type_id).expect_sum();
                    let tag_value_sum_value = TypedProgram::make_int_value(
                        &mut self.static_values,
                        int_value_type_id,
                        self.mem.getn(int_value_enum.variants),
                        variant.tag_value,
                    );
                    let tag_value_id =
                        self.static_values.add(StaticValue::Sum(tag_value_sum_value));

                    let payload_info_opt_type_id =
                        self.get_struct_field(variant_struct_type_id, 2).type_id;
                    let payload_info_struct_id =
                        self.get_as_opt_instance(payload_info_opt_type_id).unwrap();

                    let payload_info_value_id = match variant.payload {
                        None => synth_static_option(
                            &mut self.static_values,
                            payload_info_opt_type_id,
                            None,
                        ),
                        Some(payload_type_id) => {
                            let type_id_value_id = self.add_type_id_value(payload_type_id);
                            // We need to ensure that any and all typeIds that we share with the user
                            // are available at runtime, by calling these functions at least once.
                            self.register_type_metainfo(payload_type_id);

                            let payload_info_struct_id = self
                                .static_values
                                .add_struct_from_slice(payload_info_struct_id, &[type_id_value_id]);
                            synth_static_option(
                                &mut self.static_values,
                                payload_info_opt_type_id,
                                Some(payload_info_struct_id),
                            )
                        }
                    };

                    variant_values.push(self.static_values.add_struct_from_slice(
                        variant_struct_type_id,
                        &[
                            // name: string,
                            name_value_id,
                            // tag: IntValue,
                            tag_value_id,
                            // payload: { typeId: u64 },
                            payload_info_value_id,
                        ],
                    ))
                }
                let variants_span_value_id = self.add_static_container_from_ids(
                    StaticContainerKind::Span,
                    variants_span_type_id,
                    variant_values.as_slice(),
                );
                let payload_value_id = self.static_values.add_struct_from_slice(
                    either_payload_type_id,
                    &[tag_type_value_id, payload_offset_value_id, variants_span_value_id],
                );
                make_variant(self, self.ast.idents.b.either, Some(payload_value_id))
            }
            Type::Opaque(opaque) => {
                // FIXME: Proper opaque type schema
                let s = self.static_values.add(StaticValue::String(
                    self.ast
                        .idents
                        .intern(format!("opaque[size={}, align={}]", opaque.size, opaque.align)),
                ));
                make_variant(self, self.ast.idents.b.other, Some(s))
            }
            Type::Never => make_variant(self, self.ast.idents.b.never, None),
            Type::Function(fn_type) => {
                let fn_type = *fn_type;
                let function_schema_payload_type_id =
                    get_schema_variant(self, self.ast.idents.b.function).payload.unwrap();
                let function_params_span_type_id =
                    self.get_struct_field(function_schema_payload_type_id, 0).type_id;

                let mut params_value_ids = self.tmp.new_list(fn_type.logical_params().len());
                for param in self.mem.getn(fn_type.logical_params()) {
                    self.register_type_metainfo(param.type_id);
                    params_value_ids.push(self.add_type_id_value(param.type_id));
                }
                let params_span_value_id = self.add_static_container_from_ids(
                    StaticContainerKind::Span,
                    function_params_span_type_id,
                    params_value_ids.as_slice(),
                );

                self.register_type_metainfo(fn_type.return_type);
                let return_type_id_value_id = self.add_type_id_value(fn_type.return_type);

                let payload = self.static_values.add_struct_from_slice(
                    function_schema_payload_type_id,
                    &[params_span_value_id, return_type_id_value_id],
                );
                make_variant(self, self.ast.idents.b.function, Some(payload))
            }
            Type::FunctionIdReference(fr) => {
                let fr = *fr;
                let named_function_schema_payload_type_id =
                    get_schema_variant(self, self.ast.idents.b.named_function).payload.unwrap();
                let param_names_span_type_id =
                    self.get_struct_field(named_function_schema_payload_type_id, 1).type_id;

                self.register_type_metainfo(fr.function_type);
                let function_type_id_value_id = self.add_type_id_value(fr.function_type);

                let param_count =
                    self.types.get(fr.function_type).as_function().unwrap().logical_params().len();
                let mut param_name_value_ids = self.tmp.new_list(param_count);
                for index in 0..param_count as usize {
                    let name = self.get_declared_param_name(fr.function_id, index);
                    param_name_value_ids.push(self.static_values.add_string(name));
                }
                let param_names_span_value_id = self.add_static_container_from_ids(
                    StaticContainerKind::Span,
                    param_names_span_type_id,
                    param_name_value_ids.as_slice(),
                );

                let payload = self.static_values.add_struct_from_slice(
                    named_function_schema_payload_type_id,
                    &[function_type_id_value_id, param_names_span_value_id],
                );
                make_variant(self, self.ast.idents.b.named_function, Some(payload))
            }
            Type::FunctionPointer(fp) => {
                let fp = *fp;
                let function_pointer_schema_payload_type_id =
                    get_schema_variant(self, self.ast.idents.b.function_pointer).payload.unwrap();

                let function_type_id_value_id = self.add_type_id_value(fp.function_type_id);
                self.register_type_metainfo(fp.function_type_id);

                let payload = self.static_values.add_struct_from_slice(
                    function_pointer_schema_payload_type_id,
                    &[function_type_id_value_id],
                );
                make_variant(self, self.ast.idents.b.function_pointer, Some(payload))
            }
            Type::Lambda(_)
            | Type::LambdaObject(_)
            | Type::AbilityObject(_)
            | Type::TypeParameter(_)
            | Type::Generic(_)
            | Type::FunctionTypeParameter(_)
            | Type::InferenceHole(_)
            | Type::StaticValue(_) => {
                let s = self
                    .static_values
                    .add(StaticValue::String(self.ast.idents.intern(typ.kind_name())));
                make_variant(self, self.ast.idents.b.other, Some(s))
            }
        };

        self.static_values.add(StaticValue::Sum(schema_static_sum))
    }

    fn build_type_name(&mut self, type_id: TypeId) -> StaticValueId {
        let mut s = std::mem::take(&mut self.buffers.name_builder);
        self.display_type_id(&mut s, type_id, dump::TypeDisplayMode::Name).unwrap();
        let string_id = self.ast.idents.intern(&s);
        s.clear();
        self.buffers.name_builder = s;
        self.static_values.add_string(string_id)
    }

    pub(crate) fn register_type_metainfo(&mut self, type_id: TypeId) {
        let _ = self.get_type_info(type_id);
    }

    pub(crate) fn add_type_id_value(&mut self, type_id: TypeId) -> StaticValueId {
        let type_id_type_id = self.builtin_types.type_id();
        self.static_values.add_type_id_value(type_id_type_id, type_id)
    }

    pub(super) fn make_int_kind(
        int_kind_type_id: TypeId,
        integer_type: IntegerType,
    ) -> StaticValue {
        StaticValue::Enum(int_kind_type_id, TypedIntValue::U8(integer_type.to_tag()))
    }

    pub(super) fn make_float_kind(
        float_kind_type_id: TypeId,
        float_type: FloatType,
    ) -> StaticValue {
        StaticValue::Enum(float_kind_type_id, TypedIntValue::U8(float_type.to_tag()))
    }

    pub(super) fn make_int_value(
        static_values: &mut StaticValuePool,
        int_value_type_id: TypeId,
        int_value_variants: &[TypedSumVariant],
        integer_value: TypedIntValue,
    ) -> StaticSum {
        let variant = int_value_variants[integer_value.get_integer_type().to_tag() as usize];
        StaticSum {
            sum_type_id: int_value_type_id,
            variant_index: variant.index,
            payload: Some(static_values.add(StaticValue::Int(integer_value))),
        }
    }
}
