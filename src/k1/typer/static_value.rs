// Copyright (c) 2025 knix
// All rights reserved.

use std::collections::hash_map::Entry;

use crate::nz_u32_id;
use crate::typer::*;

#[derive(Debug, Clone)]
pub struct StaticStruct {
    pub type_id: TypeId,
    pub fields: EcoVec<StaticValueId>,
}

#[derive(Debug, Clone)]
pub struct StaticEnum {
    pub variant_type_id: TypeId,
    pub variant_index: u32,
    /// Whether or not this thing is typed as the variant itself
    /// or as its enum
    pub typed_as_enum: bool,
    pub payload: Option<StaticValueId>,
}

#[derive(Debug, Clone)]
pub struct StaticView {
    pub elements: EcoVec<StaticValueId>,
    pub type_id: TypeId,
}

impl StaticView {
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.elements.len()
    }
}

nz_u32_id!(StaticValueId);

static_assert_size!(StaticValue, 32);
#[derive(Debug, Clone)]
pub enum StaticValue {
    Unit,
    Boolean(bool),
    Char(u8),
    Int(TypedIntValue),
    Float(TypedFloatValue),
    String(StringId),
    NullPointer,
    Struct(StaticStruct),
    Enum(StaticEnum),
    View(StaticView),
}

impl StaticValue {
    pub fn kind_name(&self) -> &'static str {
        match self {
            StaticValue::Unit => "unit",
            StaticValue::Boolean(_) => "bool",
            StaticValue::Char(_) => "char",
            StaticValue::Int(i) => i.kind_name(),
            StaticValue::Float(_) => "float",
            StaticValue::String(_) => "string",
            StaticValue::NullPointer => "nullptr",
            StaticValue::Struct(_) => "struct",
            StaticValue::Enum(_) => "enum",
            StaticValue::View(_) => "view",
        }
    }

    pub fn get_type(&self) -> TypeId {
        match self {
            StaticValue::Unit => UNIT_TYPE_ID,
            StaticValue::Boolean(_) => BOOL_TYPE_ID,
            StaticValue::Char(_) => CHAR_TYPE_ID,
            StaticValue::Int(typed_integer_value) => typed_integer_value.get_type(),
            StaticValue::Float(typed_float_value) => typed_float_value.get_type(),
            StaticValue::String(_) => STRING_TYPE_ID,
            StaticValue::NullPointer => POINTER_TYPE_ID,
            StaticValue::Struct(s) => s.type_id,
            StaticValue::Enum(e) => e.variant_type_id,
            StaticValue::View(v) => v.type_id,
        }
    }

    pub fn as_boolean(&self) -> Option<bool> {
        match self {
            StaticValue::Boolean(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<&StaticEnum> {
        match self {
            StaticValue::Enum(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_uword(&self) -> Option<usize> {
        match self {
            StaticValue::Int(int) => int.as_uword(),
            _ => None,
        }
    }

    pub fn as_view(&self) -> Option<&StaticView> {
        match self {
            StaticValue::View(view) => Some(view),
            _ => None,
        }
    }
}

impl DepHash<Pool<StaticValue, StaticValueId>> for StaticValue {
    fn dep_hash<H: std::hash::Hasher>(&self, d: &Pool<StaticValue, StaticValueId>, state: &mut H) {
        use std::hash::Hash;
        std::mem::discriminant(self).hash(state);
        match self {
            StaticValue::Unit => {}
            StaticValue::Boolean(b) => b.hash(state),
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
            StaticValue::NullPointer => {}
            StaticValue::Struct(s) => {
                s.type_id.hash(state);
                s.fields.len().hash(state);
                for &field_id in &s.fields {
                    d.get(field_id).dep_hash(d, state);
                }
            }
            StaticValue::Enum(e) => {
                e.variant_type_id.hash(state);
                e.variant_index.hash(state);
                e.typed_as_enum.hash(state);
                if let Some(payload_id) = e.payload {
                    d.get(payload_id).dep_hash(d, state);
                }
            }
            StaticValue::View(v) => {
                v.type_id.hash(state);
                v.elements.len().hash(state);
                for &element_id in &v.elements {
                    d.get(element_id).dep_hash(d, state);
                }
            }
        }
    }
}

impl DepEq<Pool<StaticValue, StaticValueId>> for StaticValue {
    fn dep_eq(&self, other: &Self, pool: &Pool<StaticValue, StaticValueId>) -> bool {
        match (self, other) {
            (StaticValue::Unit, StaticValue::Unit) => true,
            (StaticValue::Boolean(a), StaticValue::Boolean(b)) => a == b,
            (StaticValue::Char(a), StaticValue::Char(b)) => a == b,
            (StaticValue::Int(a), StaticValue::Int(b)) => a == b,
            (StaticValue::Float(a), StaticValue::Float(b)) => a == b,
            (StaticValue::String(a), StaticValue::String(b)) => a == b,
            (StaticValue::NullPointer, StaticValue::NullPointer) => true,
            (StaticValue::Struct(a), StaticValue::Struct(b)) => {
                a.type_id == b.type_id
                    && a.fields.len() == b.fields.len()
                    && a.fields.iter().zip(b.fields.iter()).all(|(&a_field, &b_field)| {
                        pool.get(a_field).dep_eq(pool.get(b_field), pool)
                    })
            }
            (StaticValue::Enum(a), StaticValue::Enum(b)) => {
                a.variant_type_id == b.variant_type_id
                    && a.variant_index == b.variant_index
                    && a.typed_as_enum == b.typed_as_enum
                    && match (a.payload, b.payload) {
                        (None, None) => true,
                        (Some(a_payload), Some(b_payload)) => {
                            pool.get(a_payload).dep_eq(pool.get(b_payload), pool)
                        }
                        _ => false,
                    }
            }
            (StaticValue::View(a), StaticValue::View(b)) => {
                a.type_id == b.type_id
                    && a.elements.len() == b.elements.len()
                    && a.elements
                        .iter()
                        .zip(b.elements.iter())
                        .all(|(&a_elem, &b_elem)| pool.get(a_elem).dep_eq(pool.get(b_elem), pool))
            }
            _ => false,
        }
    }
}

pub struct StaticValuePool {
    pub pool: Pool<StaticValue, StaticValueId>,
    pub hashes: FxHashMap<u64, StaticValueId>,
}

impl StaticValuePool {
    pub fn with_capacity(capacity: usize) -> StaticValuePool {
        StaticValuePool {
            pool: Pool::with_capacity("static_values", capacity),
            hashes: FxHashMap::with_capacity(capacity),
        }
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
        value.dep_hash(&self.pool, &mut hasher);
        let hash = hasher.finish();
        hash
    }

    pub fn add(&mut self, value: StaticValue) -> StaticValueId {
        let hash = self.hash(&value);
        if let Entry::Occupied(entry) = self.hashes.entry(hash) {
            let existing_id = *entry.get();
            let existing = self.pool.get(existing_id);
            if value.dep_eq(existing, &self.pool) {
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

    pub fn iter_with_ids(&self) -> impl Iterator<Item = (StaticValueId, &StaticValue)> {
        self.pool.iter_with_ids()
    }
}
