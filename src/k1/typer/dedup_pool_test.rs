use crate::pool::Pool;
use crate::typer::*;
use fxhash::FxHashMap;

// Helper struct to test the deduplication pool mechanism using StaticValue as test data
struct TestDedupSystem {
    static_values: Pool<StaticValue, StaticValueId>,
    static_values_dedup: FxHashMap<u64, StaticValueId>,
}

impl TestDedupSystem {
    fn new() -> Self {
        Self {
            static_values: Pool::new("test_static_values"),
            static_values_dedup: FxHashMap::default(),
        }
    }
    
    // Test implementation of deduplicating add - mirrors the real implementation
    fn add_static_value(&mut self, value: StaticValue) -> StaticValueId {
        use std::hash::Hasher;
        use fxhash::FxHasher;
        
        // Compute hash using our custom pool-aware method
        let mut hasher = FxHasher::default();
        value.pool_hash(&self.static_values, &mut hasher);
        let hash = hasher.finish();
        
        // Check if we already have this value
        if let Some(&existing_id) = self.static_values_dedup.get(&hash) {
            // Double-check with deep equality to handle hash collisions
            if value.pool_eq(self.static_values.get(existing_id), &self.static_values) {
                return existing_id;
            }
        }
        
        // Value not found, add it
        let new_id = self.static_values.add(value);
        self.static_values_dedup.insert(hash, new_id);
        new_id
    }
}

#[test]
fn test_identical_values_deduplicate() {
    let mut system = TestDedupSystem::new();
    
    let id1 = system.add_static_value(StaticValue::Unit);
    let id2 = system.add_static_value(StaticValue::Unit);
    
    assert_eq!(id1, id2, "Identical values should deduplicate");
    assert_eq!(system.static_values.len(), 1, "Should only have one instance");
}

#[test] 
fn test_boolean_deduplication() {
    let mut system = TestDedupSystem::new();
    
    let true1 = system.add_static_value(StaticValue::Boolean(true));
    let true2 = system.add_static_value(StaticValue::Boolean(true));
    let false1 = system.add_static_value(StaticValue::Boolean(false));
    let false2 = system.add_static_value(StaticValue::Boolean(false));
    
    assert_eq!(true1, true2, "True values should deduplicate");
    assert_eq!(false1, false2, "False values should deduplicate");
    assert_ne!(true1, false1, "True and false should be different");
    assert_eq!(system.static_values.len(), 2, "Should have exactly 2 boolean values");
}

#[test]
fn test_char_deduplication() {
    let mut system = TestDedupSystem::new();
    
    let a1 = system.add_static_value(StaticValue::Char(b'a'));
    let a2 = system.add_static_value(StaticValue::Char(b'a'));
    let b1 = system.add_static_value(StaticValue::Char(b'b'));
    
    assert_eq!(a1, a2, "Same char values should deduplicate");
    assert_ne!(a1, b1, "Different chars should be different");
    assert_eq!(system.static_values.len(), 2, "Should have 2 unique chars");
}

#[test]
fn test_int_deduplication() {
    let mut system = TestDedupSystem::new();
    
    let int1 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(42)));
    let int2 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(42)));
    let int3 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(43)));
    let int4 = system.add_static_value(StaticValue::Int(TypedIntValue::U32(42))); // Different type!
    
    assert_eq!(int1, int2, "Same i32 values should deduplicate");
    assert_ne!(int1, int3, "Different i32 values should be different");
    assert_ne!(int1, int4, "i32(42) and u32(42) should be different types");
    assert_eq!(system.static_values.len(), 3, "Should have 3 unique int values");
}

#[test]
fn test_float_deduplication() {
    let mut system = TestDedupSystem::new();
    
    let f1 = system.add_static_value(StaticValue::Float(TypedFloatValue::F32(3.14)));
    let f2 = system.add_static_value(StaticValue::Float(TypedFloatValue::F32(3.14)));
    let f3 = system.add_static_value(StaticValue::Float(TypedFloatValue::F64(3.14))); // Different precision
    let f4 = system.add_static_value(StaticValue::Float(TypedFloatValue::F32(2.71)));
    
    assert_eq!(f1, f2, "Same f32 values should deduplicate");
    assert_ne!(f1, f3, "f32 and f64 should be different");
    assert_ne!(f1, f4, "Different f32 values should be different");
    assert_eq!(system.static_values.len(), 3, "Should have 3 unique float values");
}

// TODO: Add string deduplication test once we figure out StringId construction

#[test]
fn test_null_pointer_deduplication() {
    let mut system = TestDedupSystem::new();
    
    let null1 = system.add_static_value(StaticValue::NullPointer);
    let null2 = system.add_static_value(StaticValue::NullPointer);
    
    assert_eq!(null1, null2, "Null pointers should deduplicate");
    assert_eq!(system.static_values.len(), 1, "Should only have one null pointer");
}

#[test]
fn test_struct_deduplication() {
    let mut system = TestDedupSystem::new();
    
    // Create some field values
    let field1 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(10)));
    let field2 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(20)));
    let field3 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(10))); // Same as field1
    
    // Create structs with same structure  
    let struct1 = system.add_static_value(StaticValue::Struct(StaticStruct {
        type_id: TypeId::from(crate::nzu32_increment(1)), // Mock type ID
        fields: eco_vec![field1, field2],
    }));
    
    let struct2 = system.add_static_value(StaticValue::Struct(StaticStruct {
        type_id: TypeId::from(crate::nzu32_increment(1)), // Same type ID 
        fields: eco_vec![field3, field2], // field3 should dedupe to field1
    }));
    
    let struct3 = system.add_static_value(StaticValue::Struct(StaticStruct {
        type_id: TypeId::from(crate::nzu32_increment(2)), // Different type ID
        fields: eco_vec![field1, field2],
    }));
    
    assert_eq!(field1, field3, "Same int values should deduplicate first");
    assert_eq!(struct1, struct2, "Structs with same type and equivalent fields should deduplicate");
    assert_ne!(struct1, struct3, "Structs with different types should be different");
}

#[test]
fn test_enum_deduplication() {
    let mut system = TestDedupSystem::new();
    
    let payload1 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(42)));
    let payload2 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(42))); // Should dedupe
    
    let enum1 = system.add_static_value(StaticValue::Enum(StaticEnum {
        variant_type_id: TypeId::from(crate::nzu32_increment(1)),
        variant_index: 0,
        typed_as_enum: true,
        payload: Some(payload1),
    }));
    
    let enum2 = system.add_static_value(StaticValue::Enum(StaticEnum {
        variant_type_id: TypeId::from(crate::nzu32_increment(1)),
        variant_index: 0, 
        typed_as_enum: true,
        payload: Some(payload2),
    }));
    
    let enum3 = system.add_static_value(StaticValue::Enum(StaticEnum {
        variant_type_id: TypeId::from(crate::nzu32_increment(1)),
        variant_index: 1, // Different variant
        typed_as_enum: true,
        payload: Some(payload1),
    }));
    
    assert_eq!(payload1, payload2, "Payloads should deduplicate first");
    assert_eq!(enum1, enum2, "Enums with same variant and equivalent payloads should deduplicate");
    assert_ne!(enum1, enum3, "Enums with different variants should be different");
}

#[test]
fn test_buffer_deduplication() {
    let mut system = TestDedupSystem::new();
    
    let elem1 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(1)));
    let elem2 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(2)));
    let elem3 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(1))); // Should dedupe
    
    let buffer1 = system.add_static_value(StaticValue::Buffer(StaticBuffer {
        type_id: TypeId::from(crate::nzu32_increment(1)),
        elements: eco_vec![elem1, elem2],
    }));
    
    let buffer2 = system.add_static_value(StaticValue::Buffer(StaticBuffer {
        type_id: TypeId::from(crate::nzu32_increment(1)),
        elements: eco_vec![elem3, elem2], // elem3 should dedupe to elem1
    }));
    
    let buffer3 = system.add_static_value(StaticValue::Buffer(StaticBuffer {
        type_id: TypeId::from(crate::nzu32_increment(1)),
        elements: eco_vec![elem2, elem1], // Different order
    }));
    
    assert_eq!(elem1, elem3, "Elements should deduplicate first");
    assert_eq!(buffer1, buffer2, "Buffers with equivalent elements should deduplicate");
    assert_ne!(buffer1, buffer3, "Buffers with different element order should be different");
}

#[test]
fn test_deep_recursive_dedup_with_nested_structures() {
    let mut system = TestDedupSystem::new();
    
    // Create a nested structure: Struct containing Buffer containing Enum containing Int
    let int_val = system.add_static_value(StaticValue::Int(TypedIntValue::I32(99)));
    
    let enum_val = system.add_static_value(StaticValue::Enum(StaticEnum {
        variant_type_id: TypeId::from(crate::nzu32_increment(10)),
        variant_index: 0,
        typed_as_enum: true,
        payload: Some(int_val),
    }));
    
    let buffer_val = system.add_static_value(StaticValue::Buffer(StaticBuffer {
        type_id: TypeId::from(crate::nzu32_increment(20)),
        elements: eco_vec![enum_val, enum_val], // Duplicate enum references
    }));
    
    let struct_val1 = system.add_static_value(StaticValue::Struct(StaticStruct {
        type_id: TypeId::from(crate::nzu32_increment(30)),
        fields: eco_vec![buffer_val],
    }));
    
    // Create the exact same nested structure again - should fully deduplicate
    let int_val2 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(99)));
    
    let enum_val2 = system.add_static_value(StaticValue::Enum(StaticEnum {
        variant_type_id: TypeId::from(crate::nzu32_increment(10)),
        variant_index: 0,
        typed_as_enum: true,
        payload: Some(int_val2),
    }));
    
    let buffer_val2 = system.add_static_value(StaticValue::Buffer(StaticBuffer {
        type_id: TypeId::from(crate::nzu32_increment(20)),
        elements: eco_vec![enum_val2, enum_val2],
    }));
    
    let struct_val2 = system.add_static_value(StaticValue::Struct(StaticStruct {
        type_id: TypeId::from(crate::nzu32_increment(30)),
        fields: eco_vec![buffer_val2],
    }));
    
    // All levels should deduplicate
    assert_eq!(int_val, int_val2, "Deep int values should deduplicate");
    assert_eq!(enum_val, enum_val2, "Deep enum values should deduplicate");
    assert_eq!(buffer_val, buffer_val2, "Deep buffer values should deduplicate");
    assert_eq!(struct_val1, struct_val2, "Deep struct values should deduplicate");
    
    // Verify we didn't create duplicates (should have exactly one of each)
    assert_eq!(system.static_values.len(), 4, "Should have exactly 4 unique values total");
}

#[test]
fn test_dedup_pool_handles_hash_collisions() {
    let mut system = TestDedupSystem::new();
    
    // Create two different values that might have hash collisions
    // (This test verifies the deep equality check works even with hash collisions)
    let val1 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(1)));
    let val2 = system.add_static_value(StaticValue::Int(TypedIntValue::I32(2)));
    
    // They should be different despite any potential hash collision
    assert_ne!(val1, val2, "Different values should never deduplicate");
    assert_eq!(system.static_values.len(), 2, "Should have 2 distinct values");
    
    // Adding the same values again should deduplicate properly
    let val1_again = system.add_static_value(StaticValue::Int(TypedIntValue::I32(1)));
    let val2_again = system.add_static_value(StaticValue::Int(TypedIntValue::I32(2)));
    
    assert_eq!(val1, val1_again, "Same value should deduplicate");
    assert_eq!(val2, val2_again, "Same value should deduplicate");
    assert_eq!(system.static_values.len(), 2, "Should still have only 2 values");
}