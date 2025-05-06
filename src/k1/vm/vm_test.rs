#[cfg(test)]
mod stack_tests {
    use ecow::eco_vec;

    use crate::parse::Identifier;
    use crate::typer::types::{StructType, StructTypeField, U32_TYPE_ID, U8_TYPE_ID};
    use crate::typer::*;
    use crate::vm::*;

    fn test_stack() -> Stack {
        let mut s = Stack::make(1024 * 1024);
        s.push_new_frame(None);
        s
    }

    #[test]
    fn test_initial_state() {
        let stack = test_stack();
        assert_eq!(stack.current_offset_bytes(), 0);
        assert_eq!(stack.frames.len(), 1);
        assert_eq!(stack.frames[0].index, 0);
        assert_eq!(stack.frames[0].base_ptr, stack.base_ptr());
        assert_eq!(stack.frames[0].base_ptr, stack.cursor);
    }

    #[test]
    fn test_align_to_bytes() {
        let mut stack = test_stack();
        // Push 3 bytes to misalign
        stack.push_slice(&[1, 2, 3]);
        assert_eq!(stack.current_offset_bytes(), 3);
        stack.align_to_bytes(4);
        assert_eq!(stack.current_offset_bytes(), 4);
        assert_eq!(stack.to_bytes(), [1, 2, 3, 0]);

        // Test with already aligned pointer
        let mut stack = test_stack();
        stack.push_slice(&[1, 2, 3, 4]);
        stack.align_to_bytes(4);
        assert_eq!(stack.current_offset_bytes(), 4);
    }

    #[test]
    fn test_push_byte() {
        let mut stack = test_stack();
        let ptr = stack.push_t(42u8);
        assert_eq!(stack.current_offset_bytes(), 1);
        assert_eq!(stack.to_bytes()[0], 42);
        assert_eq!(ptr.addr(), stack.base_addr());
    }

    #[test]
    fn test_push_usize() {
        let mut stack = test_stack();
        let value = 0x12345678_usize;
        let ptr = stack.push_usize(value);
        assert!(stack.current_offset_bytes() == 4 || stack.current_offset_bytes() == 8); // Depends on platform
        unsafe {
            let read_back = *(ptr as *const usize);
            assert_eq!(read_back as usize, value);
        };
    }

    #[test]
    fn test_pop_frame() {
        let mut stack = test_stack();
        let _ = stack.push_t(10u64);
        let _ = stack.push_t(10u64);
        let f = stack.push_new_frame(None);
        assert_eq!(stack.current_offset_bytes(), 16);
        assert_eq!(f.index, 1);
        assert_eq!(f.base_ptr, unsafe { stack.base_ptr().byte_add(16) });
        assert_eq!(f.base_ptr, stack.cursor);
        stack.push_t(10u64);
        stack.push_t(10u64);
        assert_eq!(stack.frames.len(), 2);
        assert_eq!(stack.current_offset_bytes(), 32);
        stack.pop_frame();
        assert_eq!(stack.frames.len(), 1);
        assert_eq!(stack.current_offset_bytes(), 16);
        assert_eq!(stack.cursor, f.base_ptr);
    }

    #[test]
    fn test_push_value_basic() {
        let mut stack = test_stack();
        let types = Types::with_builtin_types();

        // Test basic value types
        stack.push_value_no_align(&types, Value::Unit);
        assert_eq!(stack.current_offset_bytes(), 1);
        assert_eq!(stack.to_bytes()[0], crate::typer::UNIT_BYTE_VALUE);

        stack.push_value_no_align(&types, Value::Bool(true));
        assert_eq!(stack.to_bytes()[1], 1);

        stack.push_value_no_align(&types, Value::Char(b'A'));
        assert_eq!(stack.to_bytes()[2], b'A');

        let int_ptr =
            stack.push_value_no_align(&types, Value::Int(TypedIntValue::U32(42))) as *const i32;
        assert_eq!(stack.current_offset_bytes(), 7);
        unsafe {
            let read_back = int_ptr.read_unaligned();
            assert_eq!(read_back, 42);
        }
    }

    #[test]
    fn test_push_value_float() {
        let mut stack = test_stack();
        let types = Types::with_builtin_types();

        let f32_val = Value::Float(TypedFloatValue::F32(std::f32::consts::PI));
        let f32ptr = stack.push_value_no_align(&types, f32_val) as *const f32;
        assert_eq!(stack.current_offset_bytes(), 4);
        let read_f32 = unsafe { f32ptr.read() };
        assert_eq!(read_f32, std::f32::consts::PI);

        let f64_val = Value::Float(TypedFloatValue::F64(std::f64::consts::PI));
        let f64_ptr = stack.push_value_no_align(&types, f64_val) as *const f64;

        assert_eq!(stack.current_offset_bytes(), 12);
        assert!(!f64_ptr.is_aligned());
        let read_f64 = unsafe { f64_ptr.read_unaligned() };
        assert_eq!(read_f64, std::f64::consts::PI);

        stack.align_to_bytes(align_of::<f64>());
        let f64_ptr_aligned = stack.push_value_no_align(&types, f64_val) as *const f64;
        assert!(f64_ptr_aligned.is_aligned());
        assert_eq!(stack.current_offset_bytes(), 24);
        let read_f64_aligned = unsafe { f64_ptr.read() };
        assert_eq!(read_f64_aligned, std::f64::consts::PI);
    }

    #[test]
    fn test_push_raw_copy() {
        let mut stack = test_stack();
        let source = [1u8, 2, 3, 4];

        let result_ptr = stack.push_raw_copy(4, 4, source.as_ptr());
        assert_eq!(stack.current_offset_bytes(), 4);
        assert_eq!(stack.to_bytes(), [1, 2, 3, 4]);
        unsafe {
            let result_slice = std::slice::from_raw_parts(result_ptr, 4);
            assert_eq!(result_slice, [1, 2, 3, 4])
        }
    }

    #[test]
    fn test_push_struct() {
        let mut types = Types::with_builtin_types();
        let mut stack = test_stack();
        let struct_type = types.add_anon(Type::Struct(StructType {
            fields: eco_vec![
                StructTypeField { name: Identifier::forged(), type_id: U8_TYPE_ID, private: false },
                StructTypeField {
                    name: Identifier::forged(),
                    type_id: U32_TYPE_ID,
                    private: false,
                }
            ],
            generic_instance_info: None,
        }));
        stack.advance_cursor(1);
        let struct_ptr = stack
            .push_struct_values(
                &types,
                struct_type,
                &[Value::Int(TypedIntValue::U8(42)), Value::Int(TypedIntValue::U32(1337))],
            )
            .expect_agg();
        let struct_ptr_offset = struct_ptr.addr() - stack.base_addr();
        assert_eq!(struct_ptr_offset, 4); // since this struct's alignment is 4
        let v1_locn = gep_struct_field(&types, struct_type, struct_ptr, 0);
        let v1_offset = v1_locn.addr() - stack.base_addr();
        assert_eq!(v1_offset, 4);
        assert_eq!(unsafe { (v1_locn as *const u8).read() }, 42);

        let v2_locn = gep_struct_field(&types, struct_type, struct_ptr, 1);
        let v2_offset = v2_locn.addr() - stack.base_addr();
        assert_eq!(v2_offset, 8);
        assert_eq!(unsafe { (v2_locn as *const u32).read() }, 1337);
    }
}
