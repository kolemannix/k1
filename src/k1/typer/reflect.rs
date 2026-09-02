// Copyright (c) 2026 knix
// All rights reserved.

use super::*;

impl TypedProgram {
    pub fn type_id_from_raw(
        &mut self,
        raw: vm::k1_types::TypeId,
        span: SpanId,
    ) -> K1Result<TypeId> {
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
        span: SpanId,
    ) -> K1Result<TypeId> {
        let mut fields: List<StructTypeField, _> = self.mem.new_list(raw_field_descs.len() as u32);
        for desc in raw_field_descs {
            let name_str = match unsafe { desc.name.to_str() } {
                Ok(s) => s,
                Err(msg) => kbail!(self, span, "make-struct field name: {}", msg),
            };
            let name = self.ast.idents.intern(name_str);
            for prev in fields.as_slice() {
                if prev.name == name {
                    kbail!(self, span, "make-struct: duplicate field name '{}'", name_str);
                }
            }
            let type_id = self.type_id_from_raw(desc.type_id, span)?;
            fields.push(StructTypeField { name, type_id, span: SpanId::NONE });
        }
        let new_type_id =
            self.add_type_anon(Type::Struct(StructType { fields: fields.to_slice(), record_kind }));
        self.register_type_metainfo(new_type_id);
        Ok(new_type_id)
    }

    pub fn make_either_raw(
        &mut self,
        explicit_tag_type: Option<vm::k1_types::TypeId>,
        raw_variant_descs: &[vm::k1_types::K1MakeEitherVariant],
        span: SpanId,
    ) -> K1Result<TypeId> {
        let variant_count = raw_variant_descs.len() as u32;
        let tag_type = match explicit_tag_type {
            Some(raw) => {
                let tag_type_id = self.type_id_from_raw(raw, span)?;
                match self.types.get(tag_type_id) {
                    Type::Integer(int_type) => *int_type,
                    _ => {
                        kbail!(
                            self,
                            span,
                            "make-either: tag-type must be an integer type, got {}",
                            tag_type_id
                        );
                    }
                }
            }
            None => {
                const U8_MAX_VARIANTS: u32 = u8::MAX as u32 + 1;
                const MAX_VARIANTS: u32 = u16::MAX as u32 + 1;
                match variant_count {
                    c if c <= U8_MAX_VARIANTS => IntegerType::U8,
                    c if c <= MAX_VARIANTS => IntegerType::U16,
                    _ => {
                        kbail!(self, span, "sum cannot have more than {MAX_VARIANTS} variants");
                    }
                }
            }
        };

        let mut has_payloads = false;
        let mut variants: List<TypedSumVariant, _> = self.mem.new_list(variant_count);
        let mut next_tag = tag_type.zero();
        for (index, desc) in raw_variant_descs.iter().enumerate() {
            let name_str = match unsafe { desc.name.to_str() } {
                Ok(s) => s,
                Err(msg) => kbail!(self, span, "make-either variant name: {}", msg),
            };
            let name = self.ast.idents.intern(name_str);
            for prev in variants.as_slice() {
                if prev.name == name {
                    kbail!(self, span, "make-either: duplicate variant name '{}'", name_str);
                }
            }
            let payload = if desc.payload.tag == 0 {
                None
            } else {
                Some(self.type_id_from_raw(desc.payload.payload, span)?)
            };
            let tag_value = if desc.tag.tag == 0 {
                next_tag
            } else {
                let int_value = desc.tag.payload;
                let Some(kind) = IntegerType::from_int_kind_tag(int_value.kind) else {
                    kbail!(self, span, "make-either: bad int-value kind {}", int_value.kind as u32);
                };
                if kind != tag_type {
                    kbail!(
                        self,
                        span,
                        "make-either: tag for '{}' is {}, but the tag type is {}",
                        name_str,
                        kind,
                        tag_type
                    );
                }
                TypedIntValue::from_u64_bits(kind, int_value.value_bits)
            };
            if let Some(existing) = variants.iter().find(|v| v.tag_value == tag_value) {
                kbail!(self, span, "Duplicate tag value: {}", existing.tag_value);
            }
            has_payloads |= payload.is_some();
            next_tag = tag_value.incr();
            variants.push(TypedSumVariant {
                name,
                index: index as u32,
                payload,
                tag_value,
                name_span: SpanId::NONE,
            });
        }

        let new_type_id = if has_payloads {
            self.add_type_anon(Type::Sum(SumType { variants: variants.to_slice(), tag_type }))
        } else {
            let mut members: List<ScalarEnumValue, _> = self.mem.new_list(variant_count);
            for v in variants.iter() {
                members.push(ScalarEnumValue {
                    name: v.name,
                    int_value: v.tag_value,
                    name_span: v.name_span,
                });
            }
            self.add_type_anon(Type::Enum(ScalarEnumType {
                member_values: members.to_slice(),
                int_type: tag_type,
            }))
        };
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
        span: SpanId,
    ) -> K1Result<TypeId> {
        if size < 0 {
            kbail!(self, span, "make-array: negative size {}", size);
        }
        let size_value_id = self.static_values.add_size(size);
        let size_type = self.add_type_anon(Type::StaticValue(StaticValueType {
            family_type_id: I64_TYPE_ID,
            value_id: Some(size_value_id),
        }));
        let new_type_id = self.add_type_anon(Type::Array(ArrayType { element_type, size_type }));
        self.register_type_metainfo(new_type_id);
        Ok(new_type_id)
    }

    pub fn make_fn_raw(
        &mut self,
        raw_param_types: &[vm::k1_types::TypeId],
        return_type: TypeId,
        span: SpanId,
    ) -> K1Result<TypeId> {
        let mut params: List<FnParamType, _> = self.mem.new_list(raw_param_types.len() as u32);
        for (index, raw) in raw_param_types.iter().enumerate() {
            let type_id = self.type_id_from_raw(*raw, span)?;
            let name = self.positional_param_name(index);
            params.push(FnParamType {
                type_id,
                name,
                is_context: false,
                is_lambda_env: false,
                is_macro_code: false,
            });
        }
        let new_type_id = self.add_type_anon(Type::Function(FunctionType {
            physical_params: params.to_slice(),
            is_lambda: false,
            return_type,
        }));
        self.register_type_metainfo(new_type_id);
        Ok(new_type_id)
    }

    pub(super) fn get_type_schema(&mut self, type_id: TypeId) -> StaticValueId {
        let reserved_id = if let Some(static_value_id) = self.type_schemas.get(&type_id) {
            return *static_value_id;
        } else {
            let reserved_value_id = self.static_values.pool.reserve_id();
            self.type_schemas.insert(type_id, reserved_value_id);
            reserved_value_id
        };

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

        // For now, introspection does not support 'static' types, it just sees through them

        // Temporarily, we could provide a separate boolean-returning function to get a type's
        // static value or something
        let chased_type_id = self.get_static_family_id_if_static(type_id);

        let typ = self.types.get(chased_type_id);
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
                let payload_value_id = self.static_values.add_struct_from_slice(
                    enum_schema_payload_type_id,
                    &[int_type_value_id, variants_span_value_id],
                );
                make_variant(self, self.ast.idents.b.enum_, Some(payload_value_id))
            }
            Type::Struct(_struct_type) if chased_type_id == self.builtin_types.string() => {
                make_variant(self, self.ast.idents.b.string, None)
            }
            Type::Struct(struct_type) => {
                let record_kind = struct_type.record_kind;
                let struct_schema_payload_type_id =
                    get_schema_variant(self, self.ast.idents.b.struct_).payload.unwrap();
                // { fields: span[{}] }
                let struct_schema_payload_struct =
                    self.types.get(struct_schema_payload_type_id).expect_struct();
                // { fields: span[{ ... }] }
                let struct_type_fields = struct_type.fields;
                let struct_schema_fields_span_type_id =
                    self.mem.get_nth(struct_schema_payload_struct.fields, 0).type_id;
                let struct_schema_field_item_struct_type_id =
                    self.get_as_span_instance(struct_schema_fields_span_type_id).unwrap();

                // for offsets
                let struct_layout = match record_kind {
                    RecordKind::Struct | RecordKind::Packed => {
                        Some(self.get_struct_layout(type_id))
                    }
                    RecordKind::Union => None,
                };
                // { name: string), typeId: u64, offset: size }
                let mut field_values = self.tmp.new_list(struct_type_fields.len());
                for (index, f) in self.mem.getn(struct_type_fields).iter().enumerate() {
                    let name_string_value_id = self.static_values.add_string(f.name);

                    // We need to ensure that any and all typeIds that we share with the user
                    // are available at runtime, by calling these functions at least once.
                    self.register_type_metainfo(f.type_id);

                    let type_id_value_id = self.add_type_id_value(f.type_id);
                    let offset_u32 = match &struct_layout {
                        None => 0,
                        Some(struct_layout) => struct_layout[index].offset,
                    };
                    let offset_value_id = self.static_values.add_size(offset_u32 as i64);
                    let field_struct_fields = self.static_values.mem.pushn(&[
                        // name: string
                        name_string_value_id,
                        // typeId: u64
                        type_id_value_id,
                        // offset: size
                        offset_value_id,
                    ]);
                    field_values.push(
                        self.static_values.add_struct(
                            struct_schema_field_item_struct_type_id,
                            field_struct_fields,
                        ),
                    );
                }
                let span_value_id = self.add_static_container_from_ids(
                    StaticContainerKind::Span,
                    struct_schema_fields_span_type_id,
                    field_values.as_slice(),
                );
                let payload = self
                    .static_values
                    .add_struct_from_slice(struct_schema_payload_type_id, &[span_value_id]);
                let variant_name = match record_kind {
                    RecordKind::Struct | RecordKind::Packed => self.ast.idents.b.struct_,
                    RecordKind::Union => self.ast.idents.b.union,
                };
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
            Type::Array(array_type) => {
                let array_type = *array_type;
                let concrete_count = self.get_concrete_count_of_array(array_type.size_type);
                let array_schema_payload_type_id =
                    get_schema_variant(self, self.ast.idents.b.array).payload.unwrap();
                // { elementTypeId: u64, size: size }
                let element_type_id_value_id = self.add_type_id_value(array_type.element_type);
                self.register_type_metainfo(array_type.element_type);

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
                    array_schema_payload_type_id,
                    &[element_type_id_value_id, size_value_id],
                );
                make_variant(self, self.ast.idents.b.array, Some(payload_struct_id))
            }
            Type::Vector(vector_type) => {
                let vector_type = *vector_type;
                let concrete_count = self.get_concrete_count_of_array(vector_type.size_type);
                let vector_schema_payload_type_id =
                    get_schema_variant(self, self.ast.idents.b.vector).payload.unwrap();
                let element_type_id_value_id = self.add_type_id_value(vector_type.element_type);
                self.register_type_metainfo(vector_type.element_type);

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
                    vector_schema_payload_type_id,
                    &[element_type_id_value_id, size_value_id],
                );
                make_variant(self, self.ast.idents.b.vector, Some(payload_struct_id))
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
                //Function({
                //  params: span[{ name: string, typeId: type-id }],
                //  returnTypeId: type-id,
                //}),
                let function_schema_payload_struct =
                    self.types.get(function_schema_payload_type_id).expect_struct();
                let function_params_span_field =
                    self.mem.get_nth(function_schema_payload_struct.fields, 0);
                let function_params_span_type_id = function_params_span_field.type_id;
                let function_param_struct_type_id =
                    self.get_as_span_instance(function_params_span_type_id).unwrap();

                let mut params_value_ids = self.tmp.new_list(fn_type.logical_params().len());
                // Skipping lambda environment parameters;
                // knowing what is a lambda is covered by the type
                // kind the function appears within

                for param in self.mem.getn(fn_type.logical_params()) {
                    self.register_type_metainfo(param.type_id);

                    let param_name_value_id = self.static_values.add_string(param.name);
                    let param_type_id_value_id = self.add_type_id_value(param.type_id);
                    let param_struct_value_id = self.static_values.add_struct_from_slice(
                        function_param_struct_type_id,
                        &[
                            // name: string
                            param_name_value_id,
                            // type-id: type-id
                            param_type_id_value_id,
                        ],
                    );
                    params_value_ids.push(param_struct_value_id)
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
                    &[
                        // params
                        params_span_value_id,
                        // returnTypeId
                        return_type_id_value_id,
                    ],
                );
                make_variant(self, self.ast.idents.b.function, Some(payload))
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

        self.static_values.set(reserved_id, StaticValue::Sum(schema_static_sum));
        reserved_id
    }

    pub(super) fn get_type_name(&mut self, type_id: TypeId) -> StaticValueId {
        if let Some(existing) = self.type_names.get(&type_id) {
            return *existing;
        }

        let mut s = std::mem::take(&mut self.buffers.name_builder);
        self.display_type_id(&mut s, type_id, dump::TypeDisplayMode::Name).unwrap();
        let string_id = self.ast.idents.intern(&s);
        s.clear();
        self.buffers.name_builder = s;
        let value_id = self.static_values.add_string(string_id);

        self.type_names.insert(type_id, value_id);
        value_id
    }

    pub(crate) fn register_type_metainfo(&mut self, type_id: TypeId) {
        let _ = self.get_type_schema(type_id);
        let _ = self.get_type_name(type_id);
    }

    pub(crate) fn add_type_id_value(&mut self, type_id: TypeId) -> StaticValueId {
        let type_id_type_id = self.builtin_types.type_id();
        self.static_values.add_type_id_value(type_id_type_id, type_id)
    }

    pub(super) fn make_int_kind(
        int_kind_type_id: TypeId,
        integer_type: IntegerType,
    ) -> StaticValue {
        StaticValue::Enum(int_kind_type_id, TypedIntValue::U8(integer_type as u8))
    }

    pub(super) fn make_float_kind(
        float_kind_type_id: TypeId,
        float_type: FloatType,
    ) -> StaticValue {
        StaticValue::Enum(float_kind_type_id, TypedIntValue::U8(float_type as u8))
    }

    pub(super) fn make_int_value(
        static_values: &mut StaticValuePool,
        int_value_type_id: TypeId,
        int_value_variants: &[TypedSumVariant],
        integer_value: TypedIntValue,
    ) -> StaticSum {
        let variant = match integer_value {
            TypedIntValue::U8(_) => int_value_variants[0],
            TypedIntValue::U16(_) => int_value_variants[1],
            TypedIntValue::U32(_) => int_value_variants[2],
            TypedIntValue::U64(_) => int_value_variants[3],
            TypedIntValue::I8(_) => int_value_variants[4],
            TypedIntValue::I16(_) => int_value_variants[5],
            TypedIntValue::I32(_) => int_value_variants[6],
            TypedIntValue::I64(_) => int_value_variants[7],
        };
        StaticSum {
            sum_type_id: int_value_type_id,
            variant_index: variant.index,
            payload: Some(static_values.add(StaticValue::Int(integer_value))),
        }
    }
}
