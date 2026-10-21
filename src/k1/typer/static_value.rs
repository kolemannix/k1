// Copyright (c) 2026 knix
// All rights reserved.

use crate::nz_u32_id;
use crate::typer::*;

pub type StaticValueSlice = MSlice<StaticValueId, StaticValuePool>;

#[derive(Clone, Copy)]
pub struct StaticStruct {
    pub type_id: TypeId,
    pub fields: StaticValueSlice,
}

#[derive(Clone, Copy)]
pub struct StaticSum {
    pub sum_type_id: TypeId,
    pub variant_index: u32,
    pub payload: Option<StaticValueId>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum StaticContainerKind {
    Span,
    Buffer,
    Array,
    Vector,
    List,
}

impl StaticContainerKind {
    pub fn name(&self) -> &'static str {
        match self {
            StaticContainerKind::Span => "span",
            StaticContainerKind::Array => "array",
            StaticContainerKind::Vector => "vector",
            StaticContainerKind::Buffer => "buffer",
            StaticContainerKind::List => "list",
        }
    }
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
}

#[derive(Clone, Copy)]
pub struct StaticRawContainer {
    pub bytes: MSlice<u8, StaticValuePool>,
    pub type_id: TypeId,
    pub kind: StaticContainerKind,
    pub scalar: ScalarType,
}

impl StaticRawContainer {
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.bytes.len() as usize / self.scalar.get_layout().size as usize
    }
}

nz_u32_id!(StaticValueId);

static_assert_size!(StaticValue, 24);
#[derive(Clone, Copy)]
pub enum StaticValue {
    Empty(TypeId),
    Bool(bool),
    Char(u8),
    Int(TypedIntValue),
    Float(TypedFloatValue),
    String(StringId),
    Enum(TypeId, TypedIntValue),
    FunctionPointer(TypeId, FunctionId),
    Struct(StaticStruct),
    Sum(StaticSum),
    LinearContainer(StaticContainer),
    RawContainer(StaticRawContainer),

    Zero(TypeId),
}

impl StaticValue {
    pub fn kind_name(&self) -> &'static str {
        match self {
            StaticValue::Empty(_) => "empty",
            StaticValue::Bool(_) => "bool",
            StaticValue::Char(_) => "char",
            StaticValue::Int(i) => i.kind_name(),
            StaticValue::Float(_) => "float",
            StaticValue::String(_) => "string",
            StaticValue::Enum(_, _) => "enum",
            StaticValue::FunctionPointer(_, _) => "function pointer",
            StaticValue::Zero(_) => "zero",
            StaticValue::Struct(_) => "struct",
            StaticValue::Sum(_) => "sum",
            StaticValue::LinearContainer(c) => c.kind.name(),
            StaticValue::RawContainer(c) => c.kind.name(),
        }
    }

    pub fn as_boolean(&self) -> Option<bool> {
        match self {
            StaticValue::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_sum(&self) -> Option<&StaticSum> {
        match self {
            StaticValue::Sum(sum) => Some(sum),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<(TypeId, TypedIntValue)> {
        match self {
            StaticValue::Enum(type_id, int_value) => Some((*type_id, *int_value)),
            _ => None,
        }
    }

    pub fn as_container(&self) -> Option<&StaticContainer> {
        match self {
            StaticValue::LinearContainer(cont) => Some(cont),
            _ => None,
        }
    }

    pub fn as_raw_container(&self) -> Option<&StaticRawContainer> {
        match self {
            StaticValue::RawContainer(cont) => Some(cont),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<StringId> {
        match self {
            StaticValue::String(s) => Some(*s),
            _ => None,
        }
    }

    pub fn as_struct(&self) -> Option<&StaticStruct> {
        match self {
            StaticValue::Struct(s) => Some(s),
            _ => None,
        }
    }

    #[doc(alias = "as_i64")]
    pub(crate) fn as_size(&self) -> Option<i64> {
        match self {
            StaticValue::Int(TypedIntValue::I64(size)) => Some(*size),
            StaticValue::Zero(SIZE_TYPE_ID) => Some(0),
            _ => None,
        }
    }
}

impl DepHash<StaticValuePool> for StaticValue {
    fn dep_hash<H: std::hash::Hasher>(&self, d: &StaticValuePool, state: &mut H) {
        use std::hash::Hash;
        std::mem::discriminant(self).hash(state);
        match self {
            StaticValue::Empty(type_id) => type_id.hash(state),
            StaticValue::Bool(b) => b.hash(state),
            StaticValue::Char(c) => c.hash(state),
            StaticValue::Int(i) => i.hash(state),
            StaticValue::Enum(type_id, int_value) => {
                type_id.hash(state);
                int_value.hash(state)
            }
            StaticValue::Float(f) => {
                // Use bit representation since f32/f64 don't implement Hash (NaN issues)
                match f {
                    TypedFloatValue::F32(val) => val.to_bits().hash(state),
                    TypedFloatValue::F64(val) => val.to_bits().hash(state),
                }
            }
            StaticValue::String(s) => s.hash(state),
            StaticValue::FunctionPointer(type_id, function_id) => {
                type_id.hash(state);
                function_id.hash(state)
            }
            StaticValue::Zero(type_id) => type_id.hash(state),
            StaticValue::Struct(s) => {
                s.type_id.hash(state);
                d.mem.getn(s.fields).hash(state);
            }
            StaticValue::Sum(e) => {
                e.sum_type_id.hash(state);
                e.variant_index.hash(state);
                e.payload.hash(state);
            }
            StaticValue::LinearContainer(v) => {
                v.type_id.hash(state);
                v.kind.hash(state);
                d.mem.getn(v.elements).hash(state);
            }
            StaticValue::RawContainer(r) => {
                r.type_id.hash(state);
                r.kind.hash(state);
                r.scalar.hash(state);
                xxhash_rust::xxh3::xxh3_64(d.mem.getn(r.bytes)).hash(state);
            }
        }
    }
}

impl DepEq<StaticValuePool> for StaticValue {
    fn dep_eq(&self, other: &Self, pool: &StaticValuePool) -> bool {
        match (self, other) {
            (StaticValue::Empty(t1), StaticValue::Empty(t2)) => *t1 == *t2,
            (StaticValue::Bool(a), StaticValue::Bool(b)) => *a == *b,
            (StaticValue::Char(a), StaticValue::Char(b)) => *a == *b,
            (StaticValue::Int(a), StaticValue::Int(b)) => *a == *b,
            (StaticValue::Enum(ta, va), StaticValue::Enum(tb, vb)) => *ta == *tb && *va == *vb,
            (
                StaticValue::Float(TypedFloatValue::F32(a)),
                StaticValue::Float(TypedFloatValue::F32(b)),
            ) => a.to_bits() == b.to_bits(),
            (
                StaticValue::Float(TypedFloatValue::F64(a)),
                StaticValue::Float(TypedFloatValue::F64(b)),
            ) => a.to_bits() == b.to_bits(),
            (StaticValue::String(a), StaticValue::String(b)) => *a == *b,
            (StaticValue::FunctionPointer(ta, fa), StaticValue::FunctionPointer(tb, fb)) => {
                *ta == *tb && *fa == *fb
            }
            (StaticValue::Zero(t1), StaticValue::Zero(t2)) => *t1 == *t2,
            (StaticValue::Struct(a), StaticValue::Struct(b)) => {
                a.type_id == b.type_id && pool.mem.getn(a.fields) == pool.mem.getn(b.fields)
            }
            (StaticValue::Sum(a), StaticValue::Sum(b)) => {
                a.sum_type_id == b.sum_type_id
                    && a.variant_index == b.variant_index
                    && a.payload == b.payload
            }
            (StaticValue::LinearContainer(a), StaticValue::LinearContainer(b)) => {
                a.type_id == b.type_id
                    && a.kind == b.kind
                    && pool.mem.getn(a.elements) == pool.mem.getn(b.elements)
            }
            (StaticValue::RawContainer(a), StaticValue::RawContainer(b)) => {
                a.type_id == b.type_id
                    && a.kind == b.kind
                    && a.scalar == b.scalar
                    && pool.mem.getn(a.bytes) == pool.mem.getn(b.bytes)
            }
            _ => false,
        }
    }
}

pub struct StaticValuePool {
    pub mem: kmem::Mem<StaticValuePool>,
    pub pool: VPool<StaticValue, StaticValueId>,
    hashes: hashbrown::HashTable<(u64, StaticValueId)>,
}

impl StaticValuePool {
    pub const FALSE: StaticValueId = StaticValueId::from_u32(1).unwrap();
    pub const TRUE: StaticValueId = StaticValueId::from_u32(2).unwrap();
    pub const EMPTY: StaticValueId = StaticValueId::from_u32(3).unwrap();
    pub const NULLPTR: StaticValueId = StaticValueId::from_u32(4).unwrap();

    pub const fn get_bool_id(b: bool) -> StaticValueId {
        if b { Self::TRUE } else { Self::FALSE }
    }

    pub fn make() -> StaticValuePool {
        let mut values = StaticValuePool {
            mem: kmem::Mem::make(),
            pool: VPool::make("static_values"),
            hashes: hashbrown::HashTable::new(),
        };
        assert_eq!(values.add(StaticValue::Bool(false)), Self::FALSE);
        assert_eq!(values.add(StaticValue::Bool(true)), Self::TRUE);
        assert_eq!(values.add(StaticValue::Empty(EMPTY_TYPE_ID)), Self::EMPTY);
        assert_eq!(values.add(StaticValue::Zero(POINTER_TYPE_ID)), Self::NULLPTR);
        values
    }

    pub fn snap(&self, w: &mut crate::snap::SnapWriter) {
        w.write_section("static_values");
        self.mem.snap(w);
        self.pool.snap(w);
    }

    pub fn restore(&mut self, r: &mut crate::snap::SnapReader) {
        r.section("static_values");
        self.mem.restore(r);
        self.pool.restore(r);
        self.hashes.clear();
        for id in self.pool.iter_ids() {
            self.index(id);
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
        value.dep_hash(self, &mut hasher);
        let hash = hasher.finish();
        hash
    }

    pub fn add_type_id_value(&mut self, type_id_type_id: TypeId, type_id: TypeId) -> StaticValueId {
        let int_value_id = self.add_int(TypedIntValue::U64(type_id.as_u32() as u64));
        let type_id_struct_value_id = self.add_struct_from_slice(type_id_type_id, &[int_value_id]);
        type_id_struct_value_id
    }

    pub fn add_string(&mut self, string_id: StringId) -> StaticValueId {
        self.add(StaticValue::String(string_id))
    }

    pub fn add_struct_from_slice(
        &mut self,
        type_id: TypeId,
        fields: &[StaticValueId],
    ) -> StaticValueId {
        self.add_with_storage(|mem| {
            StaticValue::Struct(StaticStruct { type_id, fields: mem.pushn(fields) })
        })
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

    pub fn add(&mut self, value: StaticValue) -> StaticValueId {
        let hash = self.hash(&value);
        if let Some(&(_, existing_id)) =
            self.hashes.find(hash, |&(_, id)| value.dep_eq(self.pool.get(id), self))
        {
            return existing_id;
        }
        let new_id = self.pool.add(value);
        self.hashes.insert_unique(hash, (hash, new_id), |&(h, _)| h);
        new_id
    }

    pub fn add_with_storage(
        &mut self,
        make_value: impl FnOnce(&mut kmem::Mem<StaticValuePool>) -> StaticValue,
    ) -> StaticValueId {
        let mark = self.mem.mark();
        let value = make_value(&mut self.mem);
        let value_count = self.pool.len();
        let id = self.add(value);
        if self.pool.len() == value_count {
            self.mem.reset_to(mark);
        }
        id
    }

    pub fn set(&mut self, reserved_id: StaticValueId, value: StaticValue) {
        *self.pool.get_mut(reserved_id) = value;
        self.index(reserved_id);
    }

    fn index(&mut self, id: StaticValueId) {
        let value = self.pool.get(id);
        let hash = self.hash(value);
        let existing =
            self.hashes.find(hash, |&(_, other)| value.dep_eq(self.pool.get(other), self)).copied();
        match existing {
            None => {
                self.hashes.insert_unique(hash, (hash, id), |&(h, _)| h);
            }
            Some((_, existing_id)) if id.as_u32() < existing_id.as_u32() => {
                self.hashes.find_mut(hash, |&(_, other)| other == existing_id).unwrap().1 = id;
            }
            Some(_) => {}
        }
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
