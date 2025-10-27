// Copyright (c) 2025 knix
// All rights reserved.

use crate::typer::*;

const BUILTIN_VALUE_COUNT: usize = 3;

#[test]
fn test_basic() {
    let mut system = StaticValuePool::make_with_hint(512);

    let id1 = system.add(StaticValue::Unit);
    let id2 = system.add(StaticValue::Unit);

    assert_eq!(id1, id2);
    assert_eq!(system.pool.len(), BUILTIN_VALUE_COUNT);

    let true1 = system.add(StaticValue::Bool(true));
    let true2 = system.add(StaticValue::Bool(true));
    let false1 = system.add(StaticValue::Bool(false));
    let false2 = system.add(StaticValue::Bool(false));

    assert_eq!(true1, true2);
    assert_eq!(false1, false2);
    assert_ne!(true1, false1);

    let a1 = system.add(StaticValue::Char(b'a'));
    let a2 = system.add(StaticValue::Char(b'a'));
    let b1 = system.add(StaticValue::Char(b'b'));

    assert_eq!(a1, a2);
    assert_ne!(a1, b1);

    let int1 = system.add(StaticValue::Int(TypedIntValue::I32(42)));
    let int2 = system.add(StaticValue::Int(TypedIntValue::I32(42)));
    let int3 = system.add(StaticValue::Int(TypedIntValue::I32(43)));
    let int4 = system.add(StaticValue::Int(TypedIntValue::U32(42)));

    assert_eq!(int1, int2, "Same i32 values should deduplicate");
    assert_ne!(int1, int3, "Different i32 values should be different");
    assert_ne!(int1, int4, "i32(42) and u32(42) should be different types");

    let null1 = system.add(StaticValue::Zero(POINTER_TYPE_ID));
    let null2 = system.add(StaticValue::Zero(POINTER_TYPE_ID));

    assert_eq!(null1, null2);
}

#[test]
fn test_float() {
    let mut system = StaticValuePool::make_with_hint(512);

    let f1 = system.add(StaticValue::Float(TypedFloatValue::F32(std::f32::consts::PI)));
    let f2 = system.add(StaticValue::Float(TypedFloatValue::F32(std::f32::consts::PI)));
    let f3 = system.add(StaticValue::Float(TypedFloatValue::F64(std::f64::consts::PI)));
    let f4 = system.add(StaticValue::Float(TypedFloatValue::F32(2.71)));

    assert_eq!(f1, f2, "Same f32 values should deduplicate");
    assert_ne!(f1, f3, "f32 and f64 should be different");
    assert_ne!(f1, f4, "Different f32 values should be different");
    assert_eq!(system.pool.len(), BUILTIN_VALUE_COUNT + 3, "Should have 3 unique float values");
}

const TYPE1: TypeId = TypeId::from_u32(1).unwrap();
const TYPE2: TypeId = TypeId::from_u32(2).unwrap();
const TYPE3: TypeId = TypeId::from_u32(3).unwrap();

#[test]
fn test_struct() {
    let mut system = StaticValuePool::make_with_hint(512);

    // Create some field values
    let field1 = system.add(StaticValue::Int(TypedIntValue::I32(10)));
    let field2 = system.add(StaticValue::Int(TypedIntValue::I32(20)));
    let field3 = system.add(StaticValue::Int(TypedIntValue::I32(10)));

    let struct1 = system.add_struct_from_slice(TYPE1, &[field1, field2]);

    let struct2 = system.add_struct_from_slice(TYPE1, &[field3, field2]);

    let struct3 = system.add_struct_from_slice(TYPE2, &[field1, field2]);

    assert_eq!(field1, field3, "Same int values should deduplicate first");
    assert_eq!(struct1, struct2, "Structs with same type and equivalent fields should deduplicate");
    assert_ne!(struct1, struct3, "Structs with different types should be different");
}

#[test]
fn test_enum() {
    let mut system = StaticValuePool::make_with_hint(512);

    let payload1 = system.add(StaticValue::Int(TypedIntValue::I32(42)));
    let payload2 = system.add(StaticValue::Int(TypedIntValue::I32(42)));

    let enum1 = system.add(StaticValue::Enum(StaticEnum {
        variant_type_id: TYPE1,
        variant_index: 0,
        typed_as_enum: true,
        payload: Some(payload1),
    }));

    let enum2 = system.add(StaticValue::Enum(StaticEnum {
        variant_type_id: TYPE1,
        variant_index: 0,
        typed_as_enum: true,
        payload: Some(payload2),
    }));

    let enum3 = system.add(StaticValue::Enum(StaticEnum {
        variant_type_id: TYPE1,
        variant_index: 1,
        typed_as_enum: true,
        payload: Some(payload1),
    }));

    assert_eq!(payload1, payload2, "Payloads should deduplicate first");
    assert_eq!(enum1, enum2, "Enums with same variant and equivalent payloads should deduplicate");
    assert_ne!(enum1, enum3, "Enums with different variants should be different");
}

#[test]
fn test_view() {
    let mut system = StaticValuePool::make_with_hint(512);

    let elem1 = system.add(StaticValue::Int(TypedIntValue::I32(1)));
    let elem2 = system.add(StaticValue::Int(TypedIntValue::I32(2)));
    let elem3 = system.add(StaticValue::Int(TypedIntValue::I32(1)));

    let view1_items = system.mem.push_slice(&[elem1, elem2]);
    let view1 = system.add_view(TYPE1, view1_items);

    let view2_items = system.mem.push_slice(&[elem3, elem2]);
    let view2 = system.add_view(TYPE1, view2_items);

    let view3_items = system.mem.push_slice(&[elem2, elem1]);
    let view3 = system.add_view(TYPE1, view3_items);

    assert_eq!(elem1, elem3, "Elements should deduplicate first");
    assert_eq!(view1, view2, "Views with equivalent elements should deduplicate");
    assert_ne!(view1, view3, "Views with different element order should be different");
}

#[test]
fn test_recurse() {
    let mut system = StaticValuePool::make_with_hint(512);

    // Tests recursive deduplication through multiple nesting levels
    let int_val = system.add(StaticValue::Int(TypedIntValue::I32(99)));

    let enum_val = system.add(StaticValue::Enum(StaticEnum {
        variant_type_id: TYPE1,
        variant_index: 0,
        typed_as_enum: true,
        payload: Some(int_val),
    }));

    let view_items = system.mem.push_slice(&[enum_val, enum_val]);
    let view_val = system.add_view(TYPE2, view_items);

    let struct_val1 = system.add_struct_from_slice(TYPE3, &[view_val]);

    // Recreate identical nested structure - should deduplicate at every level
    let int_val2 = system.add(StaticValue::Int(TypedIntValue::I32(99)));

    let enum_val2 = system.add(StaticValue::Enum(StaticEnum {
        variant_type_id: TYPE1,
        variant_index: 0,
        typed_as_enum: true,
        payload: Some(int_val2),
    }));

    let view2_items = system.mem.push_slice(&[enum_val2, enum_val2]);
    let view_val2 = system.add_view(TYPE2, view2_items);

    let struct_val2 = system.add_struct_from_slice(TYPE3, &[view_val2]);

    assert_eq!(int_val, int_val2, "Deep int values should deduplicate");
    assert_eq!(enum_val, enum_val2, "Deep enum values should deduplicate");
    assert_eq!(view_val, view_val2, "Deep view values should deduplicate");
    assert_eq!(struct_val1, struct_val2, "Deep struct values should deduplicate");

    // 3 values are builtin
    assert_eq!(system.pool.len(), BUILTIN_VALUE_COUNT + 4);
}
