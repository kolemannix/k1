// Copyright (c) 2025 knix
// All rights reserved.

#[cfg(test)]
mod stack_tests {

    use crate::kmem::MSlice;
    use crate::typer::types::AbiMode;
    use crate::typer::*;
    use crate::vm::*;

    fn fake_unit() -> CompiledUnit {
        CompiledUnit {
            unit_id: CompilableUnitId::Function(FunctionId::PENDING),
            fn_type: bc::PhysicalFunctionType { return_type: InstKind::Void, params: MSlice::empty(), abi_mode: AbiMode::Internal },
            inst_offset: 0,
            inst_count: 0,
            blocks: MSlice::empty(),
            is_debug: false,
            function_builtin_kind: None
        }
    }

    fn test_stack() -> Stack {
        let mut s = Stack::make();
        s.push_new_frame(None, &fake_unit(), None);
        s
    }

    #[test]
    fn test_params_and_instrs() {
        let mut stack = test_stack();
        let mut unit = fake_unit();
        unit.inst_count = 42;
        unit.fn_type.params = MSlice::forged(1, 10);
        stack.push_new_frame(None, &unit, None);
        let frame_space_for_registers = stack.cursor().addr() - stack.current_frame().base_ptr.addr();
        assert_eq!(frame_space_for_registers, (42 + 10) * size_of::<Value>());
        let x_addr = stack.push_t(b'X');
        stack.set_param_value(1, 0, Value(23));
        stack.set_param_value(1, 9, Value(24));

        stack.set_inst_value(1, 41, Value(41));
        stack.set_inst_value(1, 1, Value(1));
        stack.set_inst_value(1, 2, Value(2));

        assert_eq!(stack.get_param_value(1, 0).bits(), Value(23).bits());
        assert_eq!(stack.get_param_value(1, 9).bits(), Value(24).bits());

        assert_eq!(stack.get_inst_value(1, 41).bits(), Value(41).bits());
        assert_eq!(stack.get_inst_value(1, 1).bits(), Value(1).bits());
        assert_eq!(stack.get_inst_value(1, 2).bits(), Value(2).bits());

        assert_eq!(unsafe { *x_addr }, b'X');
    }
}

#[cfg(test)]
mod value_tests {}

#[cfg(test)]
mod value_roundtrip_tests {
    use crate::typer::types::IntegerType;
    use crate::vm::*;

    // Helper macro to test integer roundtrips
    macro_rules! test_int {
        ($name:ident, $typ:ty, $int_type:expr, $variant:ident) => {
            #[test]
            fn $name() {
                for val in [<$typ>::MIN, <$typ>::MAX, -1, 0, 1, -2, 42, -42] {
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
        assert!(Value::bool(true).as_bool());
        assert!(!Value::bool(false).as_bool());
        assert!(Value::TRUE.as_bool());
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
