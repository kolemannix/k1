// Copyright (c) 2025 knix
// All rights reserved.

use crate::typer::*;
use fxhash::FxHashMap;

struct TestDedupSystem {
    static_values: VPool<StaticValue, StaticValueId>,
    static_values_dedup: FxHashMap<u64, StaticValueId>,
}

impl TestDedupSystem {
    fn new() -> Self {
        Self {
            static_values: VPool::make_mb("test_static_values", 1),
            static_values_dedup: FxHashMap::default(),
        }
    }

    fn add_static_value(&mut self, value: StaticValue) -> StaticValueId {
        use fxhash::FxHasher;
        use std::hash::Hasher;

        let mut hasher = FxHasher::default();
        value.dep_hash(&self.static_values, &mut hasher);
        let hash = hasher.finish();

        if let Some(&existing_id) = self.static_values_dedup.get(&hash) {
            if value.dep_eq(self.static_values.get(existing_id), &self.static_values) {
                return existing_id;
            }
        }

        let new_id = self.static_values.add(value);
        self.static_values_dedup.insert(hash, new_id);
        new_id
    }
}

#[test]
fn test_basic() {
    let mut system = TestDedupSystem::new();

    let id1 = system.add_static_value(StaticValue::Unit);
    let id2 = system.add_static_value(StaticValue::Unit);

    assert_eq!(id1, id2, "Identical values should deduplicate");
    assert_eq!(system.static_values.len(), 1, "Should only have one instance");

    let true1 = system.add_static_value(StaticValue::Bool(true));
    let true2 = system.add_static_value(StaticValue::Bool(true));
    let false1 = system.add_static_value(StaticValue::Bool(false));
    let false2 = system.add_static_value(StaticValue::Bool(false));

    assert_eq!(true1, true2, "True values should deduplicate");
    assert_eq!(false1, false2, "False values should deduplicate");
    assert_ne!(true1, false1, "True and false should be different");

    let a1 = system.add_static_value(StaticValue::Char(b'a'));
    let a2 = system.add_static_value(StaticValue::Char(b'a'));
    let b1 = system.add_static_value(StaticValue::Char(b'b'));

    assert_eq!(a1, a2, "Same char values should deduplicate");
    assert_ne!(a1, b1, "Different chars should be different");

    let int1 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(42)));
    let int2 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(42)));
    let int3 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(43)));
    let int4 = system.add_static_value(StaticValue::Int(TypedIntValue::U32(42)));

    assert_eq!(int1, int2, "Same i32 values should deduplicate");
    assert_ne!(int1, int3, "Different i32 values should be different");
    assert_ne!(int1, int4, "i32(42) and u32(42) should be different types");

    let null1 = system.add_static_value(StaticValue::NullPointer);
    let null2 = system.add_static_value(StaticValue::NullPointer);

    assert_eq!(null1, null2, "Null pointers should deduplicate");
}

#[test]
fn test_float() {
    let mut system = TestDedupSystem::new();

    let f1 =
        system.add_static_value(StaticValue::Float(TypedFloatValue::F32(std::f32::consts::PI)));
    let f2 =
        system.add_static_value(StaticValue::Float(TypedFloatValue::F32(std::f32::consts::PI)));
    let f3 =
        system.add_static_value(StaticValue::Float(TypedFloatValue::F64(std::f64::consts::PI)));
    let f4 = system.add_static_value(StaticValue::Float(TypedFloatValue::F32(2.71)));

    assert_eq!(f1, f2, "Same f32 values should deduplicate");
    assert_ne!(f1, f3, "f32 and f64 should be different");
    assert_ne!(f1, f4, "Different f32 values should be different");
    assert_eq!(system.static_values.len(), 3, "Should have 3 unique float values");
}

// TODO: Add string deduplication test once we figure out StringId construction
#[test]
fn test_string() {}

#[test]
fn test_struct() {
    let mut system = TestDedupSystem::new();

    // Create some field values
    let field1 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(10)));
    let field2 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(20)));
    let field3 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(10)));

    let struct1 = system.add_static_value(StaticValue::Struct(StaticStruct {
        type_id: TypeId::from(crate::nzu32_from_incr(1)),
        fields: eco_vec![field1, field2],
    }));

    let struct2 = system.add_static_value(StaticValue::Struct(StaticStruct {
        type_id: TypeId::from(crate::nzu32_from_incr(1)),
        fields: eco_vec![field3, field2],
    }));

    let struct3 = system.add_static_value(StaticValue::Struct(StaticStruct {
        type_id: TypeId::from(crate::nzu32_from_incr(2)),
        fields: eco_vec![field1, field2],
    }));

    assert_eq!(field1, field3, "Same int values should deduplicate first");
    assert_eq!(struct1, struct2, "Structs with same type and equivalent fields should deduplicate");
    assert_ne!(struct1, struct3, "Structs with different types should be different");
}

#[test]
fn test_enum() {
    let mut system = TestDedupSystem::new();

    let payload1 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(42)));
    let payload2 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(42)));

    let enum1 = system.add_static_value(StaticValue::Enum(StaticEnum {
        variant_type_id: TypeId::from(crate::nzu32_from_incr(1)),
        variant_index: 0,
        typed_as_enum: true,
        payload: Some(payload1),
    }));

    let enum2 = system.add_static_value(StaticValue::Enum(StaticEnum {
        variant_type_id: TypeId::from(crate::nzu32_from_incr(1)),
        variant_index: 0,
        typed_as_enum: true,
        payload: Some(payload2),
    }));

    let enum3 = system.add_static_value(StaticValue::Enum(StaticEnum {
        variant_type_id: TypeId::from(crate::nzu32_from_incr(1)),
        variant_index: 1,
        typed_as_enum: true,
        payload: Some(payload1),
    }));

    assert_eq!(payload1, payload2, "Payloads should deduplicate first");
    assert_eq!(enum1, enum2, "Enums with same variant and equivalent payloads should deduplicate");
    assert_ne!(enum1, enum3, "Enums with different variants should be different");
}

#[test]
fn test_buffer() {
    let mut system = TestDedupSystem::new();

    let elem1 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(1)));
    let elem2 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(2)));
    let elem3 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(1)));

    let buffer1 = system.add_static_value(StaticValue::View(StaticView {
        type_id: TypeId::from(crate::nzu32_from_incr(1)),
        elements: eco_vec![elem1, elem2],
    }));

    let buffer2 = system.add_static_value(StaticValue::View(StaticView {
        type_id: TypeId::from(crate::nzu32_from_incr(1)),
        elements: eco_vec![elem3, elem2],
    }));

    let buffer3 = system.add_static_value(StaticValue::View(StaticView {
        type_id: TypeId::from(crate::nzu32_from_incr(1)),
        elements: eco_vec![elem2, elem1],
    }));

    assert_eq!(elem1, elem3, "Elements should deduplicate first");
    assert_eq!(buffer1, buffer2, "Views with equivalent elements should deduplicate");
    assert_ne!(buffer1, buffer3, "Views with different element order should be different");
}

#[test]
fn test_recurse() {
    let mut system = TestDedupSystem::new();

    // Tests recursive deduplication through multiple nesting levels
    let int_val = system.add_static_value(StaticValue::Int(TypedIntValue::I32(99)));

    let enum_val = system.add_static_value(StaticValue::Enum(StaticEnum {
        variant_type_id: TypeId::from(crate::nzu32_from_incr(10)),
        variant_index: 0,
        typed_as_enum: true,
        payload: Some(int_val),
    }));

    let buffer_val = system.add_static_value(StaticValue::View(StaticView {
        type_id: TypeId::from(crate::nzu32_from_incr(20)),
        elements: eco_vec![enum_val, enum_val],
    }));

    let struct_val1 = system.add_static_value(StaticValue::Struct(StaticStruct {
        type_id: TypeId::from(crate::nzu32_from_incr(30)),
        fields: eco_vec![buffer_val],
    }));

    // Recreate identical nested structure - should deduplicate at every level
    let int_val2 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(99)));

    let enum_val2 = system.add_static_value(StaticValue::Enum(StaticEnum {
        variant_type_id: TypeId::from(crate::nzu32_from_incr(10)),
        variant_index: 0,
        typed_as_enum: true,
        payload: Some(int_val2),
    }));

    let buffer_val2 = system.add_static_value(StaticValue::View(StaticView {
        type_id: TypeId::from(crate::nzu32_from_incr(20)),
        elements: eco_vec![enum_val2, enum_val2],
    }));

    let struct_val2 = system.add_static_value(StaticValue::Struct(StaticStruct {
        type_id: TypeId::from(crate::nzu32_from_incr(30)),
        fields: eco_vec![buffer_val2],
    }));

    assert_eq!(int_val, int_val2, "Deep int values should deduplicate");
    assert_eq!(enum_val, enum_val2, "Deep enum values should deduplicate");
    assert_eq!(buffer_val, buffer_val2, "Deep buffer values should deduplicate");
    assert_eq!(struct_val1, struct_val2, "Deep struct values should deduplicate");

    // Critical: only 4 unique values despite creating 8 (4 twice)
    assert_eq!(system.static_values.len(), 4);
}
