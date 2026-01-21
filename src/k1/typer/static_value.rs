// Copyright (c) 2025 knix
// All rights reserved.

use std::collections::hash_map::Entry;

use crate::nz_u32_id;
use crate::typer::*;

pub type StaticValueSlice = MSlice<StaticValueId, StaticValuePool>;

#[derive(Clone, Copy)]
pub struct StaticStruct {
    pub type_id: TypeId,
    pub fields: StaticValueSlice,
}

#[derive(Clone, Copy)]
pub struct StaticEnum {
    pub enum_type_id: TypeId,
    pub variant_index: u32,
    pub payload: Option<StaticValueId>,
}

#[derive(Clone, Copy)]
pub enum StaticContainerKind {
    View,
    Array,
}

#[derive(Clone, Copy)]
pub struct StaticContainer {
    pub elements: StaticValueSlice,
    pub kind: StaticContainerKind,
    pub type_id: TypeId,
}

impl StaticContainer {
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.elements.len() as usize
    }

    pub fn is_array(&self) -> bool {
        matches!(self.kind, StaticContainerKind::Array)
    }
}

nz_u32_id!(StaticValueId);

static_assert_size!(StaticValue, 24);
#[derive(Clone)]
pub enum StaticValue {
    Empty,
    Bool(bool),
    Char(u8),
    Int(TypedIntValue),
    Float(TypedFloatValue),
    String(StringId),
    Zero(TypeId),
    Struct(StaticStruct),
    Enum(StaticEnum),
    LinearContainer(StaticContainer),
}

impl StaticValue {
    pub fn kind_name(&self) -> &'static str {
        match self {
            StaticValue::Empty => "empty",
            StaticValue::Bool(_) => "bool",
            StaticValue::Char(_) => "char",
            StaticValue::Int(i) => i.kind_name(),
            StaticValue::Float(_) => "float",
            StaticValue::String(_) => "string",
            StaticValue::Zero(_) => "zero",
            StaticValue::Struct(_) => "struct",
            StaticValue::Enum(_) => "enum",
            StaticValue::LinearContainer(c) => match c.kind {
                StaticContainerKind::View => "view",
                StaticContainerKind::Array => "array",
            },
        }
    }

    pub fn get_type(&self) -> TypeId {
        match self {
            StaticValue::Empty => EMPTY_TYPE_ID,
            StaticValue::Bool(_) => BOOL_TYPE_ID,
            StaticValue::Char(_) => CHAR_TYPE_ID,
            StaticValue::Int(typed_integer_value) => typed_integer_value.get_type(),
            StaticValue::Float(typed_float_value) => typed_float_value.get_type(),
            StaticValue::String(_) => STRING_TYPE_ID,
            StaticValue::Zero(type_id) => *type_id,
            StaticValue::Struct(s) => s.type_id,
            StaticValue::Enum(e) => e.enum_type_id,
            StaticValue::LinearContainer(v) => v.type_id,
        }
    }

    pub fn as_boolean(&self) -> Option<bool> {
        match self {
            StaticValue::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<&StaticEnum> {
        match self {
            StaticValue::Enum(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_container(&self) -> Option<&StaticContainer> {
        match self {
            StaticValue::LinearContainer(view) => Some(view),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<StringId> {
        match self {
            StaticValue::String(s) => Some(*s),
            _ => None,
        }
    }

    #[doc(alias = "as_i64")]
    pub(crate) fn as_size(&self) -> Option<i64> {
        match self {
            StaticValue::Int(TypedIntValue::I64(size)) => Some(*size),
            _ => None,
        }
    }
}

impl DepHash<StaticValuePool> for StaticValue {
    fn dep_hash<H: std::hash::Hasher>(&self, d: &StaticValuePool, state: &mut H) {
        use std::hash::Hash;
        std::mem::discriminant(self).hash(state);
        match self {
            StaticValue::Empty => {}
            StaticValue::Bool(b) => b.hash(state),
            StaticValue::Char(c) => c.hash(state),
            StaticValue::Int(i) => i.hash(state),
            StaticValue::Float(f) => {
                // Use bit representation since f32/f64 don't implement Hash (NaN issues)
                match f {
                    TypedFloatValue::F32(val) => val.to_bits().hash(state),
                    TypedFloatValue::F64(val) => val.to_bits().hash(state),
                }
            }
            StaticValue::String(s) => s.hash(state),
            StaticValue::Zero(type_id) => type_id.hash(state),
            StaticValue::Struct(s) => {
                s.type_id.hash(state);
                s.fields.len().hash(state);
                for &field_id in d.mem.getn(s.fields).iter() {
                    field_id.hash(state);
                }
            }
            StaticValue::Enum(e) => {
                e.enum_type_id.hash(state);
                e.variant_index.hash(state);
                if let Some(payload_id) = e.payload {
                    d.get(payload_id).dep_hash(d, state);
                }
            }
            StaticValue::LinearContainer(v) => {
                v.type_id.hash(state);
                v.elements.len().hash(state);
                for &element_id in d.mem.getn(v.elements).iter() {
                    element_id.hash(state);
                }
            }
        }
    }
}

impl DepEq<StaticValuePool> for StaticValue {
    fn dep_eq(&self, other: &Self, pool: &StaticValuePool) -> bool {
        match (self, other) {
            (StaticValue::Empty, StaticValue::Empty) => true,
            (StaticValue::Bool(a), StaticValue::Bool(b)) => a == b,
            (StaticValue::Char(a), StaticValue::Char(b)) => a == b,
            (StaticValue::Int(a), StaticValue::Int(b)) => a == b,
            (StaticValue::Float(a), StaticValue::Float(b)) => a == b,
            (StaticValue::String(a), StaticValue::String(b)) => a == b,
            (StaticValue::Zero(t1), StaticValue::Zero(t2)) => *t1 == *t2,
            (StaticValue::Struct(a), StaticValue::Struct(b)) => {
                a.type_id == b.type_id
                    && a.fields.len() == b.fields.len()
                    && pool.mem.getn(a.fields) == pool.mem.getn(b.fields)
            }
            (StaticValue::Enum(a), StaticValue::Enum(b)) => {
                a.enum_type_id == b.enum_type_id
                    && a.variant_index == b.variant_index
                    && match (a.payload, b.payload) {
                        (None, None) => true,
                        (Some(a_payload), Some(b_payload)) => a_payload == b_payload,
                        _ => false,
                    }
            }
            (StaticValue::LinearContainer(a), StaticValue::LinearContainer(b)) => {
                a.type_id == b.type_id
                    && a.elements.len() == b.elements.len()
                    && pool.mem.getn(a.elements) == pool.mem.getn(b.elements)
            }
            _ => false,
        }
    }
}

pub struct StaticValuePool {
    pub mem: kmem::Mem<StaticValuePool>,
    pub pool: VPool<StaticValue, StaticValueId>,
    pub hashes: FxHashMap<u64, StaticValueId>,

    empty_id: StaticValueId,
    false_id: StaticValueId,
    true_id: StaticValueId,
}

impl StaticValuePool {
    pub fn make_with_hint(size_hint: usize) -> StaticValuePool {
        let mut pool = VPool::make_with_hint("static_values", size_hint);
        let false_id = pool.add(StaticValue::Bool(false));
        let true_id = pool.add(StaticValue::Bool(true));
        let empty_id = pool.add(StaticValue::Empty);
        StaticValuePool {
            mem: kmem::Mem::make(),
            pool,
            hashes: FxHashMap::with_capacity(size_hint),
            empty_id,
            false_id,
            true_id,
        }
    }

    pub fn empty_id(&mut self) -> StaticValueId {
        self.empty_id
    }

    pub fn false_id(&self) -> StaticValueId {
        self.false_id
    }

    pub fn true_id(&self) -> StaticValueId {
        self.true_id
    }

    pub fn next_id(&self) -> StaticValueId {
        self.pool.next_id()
    }

    pub fn len(&self) -> usize {
        self.pool.len()
    }

    pub fn is_empty(&self) -> bool {
        self.pool.is_empty()
    }

    pub fn hash(&self, value: &StaticValue) -> u64 {
        use fxhash::FxHasher;
        use std::hash::Hasher;

        let mut hasher = FxHasher::default();
        value.dep_hash(self, &mut hasher);
        let hash = hasher.finish();
        hash
    }

    pub fn add_type_id_int_value(&mut self, type_id: TypeId) -> StaticValueId {
        self.add(StaticValue::Int(TypedIntValue::U64(type_id.as_u32() as u64)))
    }

    pub fn add_string(&mut self, string_id: StringId) -> StaticValueId {
        self.add(StaticValue::String(string_id))
    }

    pub fn add_struct(&mut self, type_id: TypeId, fields: StaticValueSlice) -> StaticValueId {
        self.add(StaticValue::Struct(StaticStruct { type_id, fields }))
    }

    pub fn add_struct_from_slice(
        &mut self,
        type_id: TypeId,
        fields: &[StaticValueId],
    ) -> StaticValueId {
        let slice = self.mem.pushn(fields);
        self.add_struct(type_id, slice)
    }

    pub fn add_int(&mut self, value: TypedIntValue) -> StaticValueId {
        self.add(StaticValue::Int(value))
    }

    pub fn add_size(&mut self, size: i64) -> StaticValueId {
        if size < 0 {
            panic!("Size cannot be negative: {}", size);
        };
        self.add(StaticValue::Int(TypedIntValue::I64(size)))
    }

    pub fn add_view(&mut self, view_type_id: TypeId, elements: StaticValueSlice) -> StaticValueId {
        self.add(StaticValue::LinearContainer(StaticContainer {
            type_id: view_type_id,
            kind: StaticContainerKind::View,
            elements,
        }))
    }

    pub fn add(&mut self, value: StaticValue) -> StaticValueId {
        match value {
            StaticValue::Struct(s) if s.fields.is_empty() => return self.empty_id,
            StaticValue::Bool(false) => return self.false_id(),
            StaticValue::Bool(true) => return self.true_id(),
            _ => {}
        };
        let hash = self.hash(&value);
        if let Entry::Occupied(entry) = self.hashes.entry(hash) {
            let existing_id = *entry.get();
            let existing = self.pool.get(existing_id);
            if value.dep_eq(existing, self) {
                return existing_id;
            }
        }
        let new_id = self.pool.add(value);
        self.hashes.insert(hash, new_id);
        new_id
    }

    pub fn get(&self, id: StaticValueId) -> &StaticValue {
        self.pool.get(id)
    }

    pub fn get_opt(&self, id: StaticValueId) -> Option<&StaticValue> {
        self.pool.get_opt(id)
    }

    pub fn get_slice(&self, slice_handle: StaticValueSlice) -> &[StaticValueId] {
        self.mem.getn(slice_handle)
    }

    pub fn iter_with_ids(&self) -> impl Iterator<Item = (StaticValueId, &StaticValue)> {
        self.pool.iter_with_ids()
    }
}
