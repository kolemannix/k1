// Copyright (c) 2025 knix
// All rights reserved.

#[cfg(test)]
mod stack_tests {

    use crate::kmem::MSlice;
    use crate::parse::Ident;
    use crate::typer::types::{StructType, StructTypeField, U8_TYPE_ID, U32_TYPE_ID};
    use crate::typer::*;
    use crate::vmbc::*;

    fn fake_unit() -> CompiledUnit {
        CompiledUnit {
            unit: CompilableUnit::Function(FunctionId::PENDING),
            expr_ret: None,
            inst_offset: 0,
            blocks: MSlice::empty(),
        }
    }

    fn test_stack() -> Stack {
        let mut s = Stack::make(1024 * 1024);
        s.push_new_frame(None, None, fake_unit(), None);
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
        let f = stack.push_new_frame(None, None, fake_unit(), None);
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

        // Test basic value types
        stack.push_t(crate::typer::UNIT_BYTE_VALUE);
        assert_eq!(stack.current_offset_bytes(), 1);
        assert_eq!(stack.to_bytes()[0], crate::typer::UNIT_BYTE_VALUE);

        stack.push_t(true);
        assert_eq!(stack.to_bytes()[1], 1);

        stack.push_t(b'A');
        assert_eq!(stack.to_bytes()[2], b'A');

        let int_ptr = stack.push_t(42u32) as *const i32;
        assert_eq!(stack.current_offset_bytes(), 8);
        unsafe {
            let read_back = int_ptr.read_unaligned();
            assert_eq!(read_back, 42);
        }

        assert_eq!(
            stack.to_bytes(),
            &[
                0, 1, b'A', 0, // 1 byte of alignment padding
                0, 0, 0, 42 // a little-endian 42u32?
            ]
        );
    }

    #[test]
    fn test_push_value_float() {
        let mut stack = test_stack();

        let f32ptr = stack.push_t(std::f32::consts::PI) as *const f32;
        assert_eq!(stack.current_offset_bytes(), 4);
        let read_f32 = unsafe { f32ptr.read() };
        assert_eq!(read_f32, std::f32::consts::PI);

        let f64_ptr = stack.push_t(std::f64::consts::PI) as *const f64;

        assert_eq!(stack.current_offset_bytes(), 16);
        assert!(f64_ptr.is_aligned());
        let read_f64 = unsafe { f64_ptr.read_unaligned() };
        assert_eq!(read_f64, std::f64::consts::PI);
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
}

#[cfg(test)]
mod value_tests {}

#[cfg(test)]
mod value_roundtrip_tests {
    use crate::typer::types::IntegerType;
    use crate::vmbc::*;

    // Helper macro to test integer roundtrips
    macro_rules! test_int {
        ($name:ident, $typ:ty, $int_type:expr, $variant:ident) => {
            #[test]
            fn $name() {
                for val in [
                    <$typ>::MIN,
                    <$typ>::MAX,
                    -1,
                    0,
                    1,
                    -2,
                    42,
                    -42,
                ] {
                    let v = Value::$name(val);
                    assert_eq!(v.as_typed_int($int_type), TypedIntValue::$variant(val));
                }
            }
        };
        (unsigned $name:ident, $typ:ty, $int_type:expr, $variant:ident) => {
            #[test]
            fn $name() {
                for val in [0, <$typ>::MAX, 1, 127, 128, 42] {
                    let v = Value::$name(val);
                    assert_eq!(v.as_typed_int($int_type), TypedIntValue::$variant(val));
                }
            }
        };
    }

    test_int!(i8, i8, IntegerType::I8, I8);
    test_int!(i16, i16, IntegerType::I16, I16);
    test_int!(i32, i32, IntegerType::I32, I32);
    test_int!(i64, i64, IntegerType::I64, I64);

    test_int!(unsigned u8, u8, IntegerType::U8, U8);
    test_int!(unsigned u16, u16, IntegerType::U16, U16);
    test_int!(unsigned u32, u32, IntegerType::U32, U32);
    test_int!(unsigned u64, u64, IntegerType::U64, U64);

    #[test]
    fn test_f32_roundtrip() {
        for val in [
            0.0,
            -0.0,
            1.0,
            -1.0,
            std::f32::consts::PI,
            -std::f32::consts::PI,
            f32::MIN,
            f32::MAX,
            f32::MIN_POSITIVE,
            f32::INFINITY,
            f32::NEG_INFINITY,
            0.5,
            -0.5,
            123.456,
        ] {
            let v = Value::f32(val);
            assert_eq!(v.as_f32(), val);
        }
        // NaN special case
        assert!(Value::f32(f32::NAN).as_f32().is_nan());
        // Verify -0.0 sign is preserved
        assert!(Value::f32(-0.0).as_f32().is_sign_negative());
    }

    #[test]
    fn test_f64_roundtrip() {
        for val in [
            0.0,
            -0.0,
            1.0,
            -1.0,
            std::f64::consts::PI,
            -std::f64::consts::PI,
            f64::MIN,
            f64::MAX,
            f64::MIN_POSITIVE,
            f64::INFINITY,
            f64::NEG_INFINITY,
            0.5,
            -0.5,
            123.456789012345,
        ] {
            let v = Value::f64(val);
            assert_eq!(v.as_f64(), val);
        }
        // NaN special case
        assert!(Value::f64(f64::NAN).as_f64().is_nan());
        // Verify -0.0 sign is preserved
        assert!(Value::f64(-0.0).as_f64().is_sign_negative());
    }

    #[test]
    fn test_bool_roundtrip() {
        assert_eq!(Value::bool(true).as_bool(), true);
        assert_eq!(Value::bool(false).as_bool(), false);
        assert_eq!(Value::TRUE.as_bool(), true);
    }

    #[test]
    fn test_pointer_roundtrip() {
        assert_eq!(Value::NULLPTR.as_ptr(), std::ptr::null());

        let data = [1u8, 2, 3, 4];
        let ptr = data.as_ptr();
        assert_eq!(Value::ptr(ptr).as_ptr(), ptr);

        let other_data = 42u64;
        let other_ptr = &other_data as *const u64 as *const u8;
        assert_eq!(Value::ptr(other_ptr).as_ptr(), other_ptr);
    }
}
