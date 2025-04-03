#[cfg(test)]
mod stack_frame_tests {
    use crate::typer::*;
    use crate::vm::*;

    fn test_vm() -> Vm {
        let mut vm = Vm::make();
        vm.push_new_frame("test".to_string());
        vm
    }

    #[test]
    fn test_initial_state() {
        let vm = test_vm();
        let frame = vm.current_frame();
        assert_eq!(frame.current_offset_bytes(), 0);
        assert_eq!(frame.debug_name, "test");
    }

    #[test]
    fn test_align_to_bytes() {
        let mut vm = test_vm();
        let frame = vm.current_frame_mut();
        // Push 3 bytes to misalign
        frame.push_slice(&[1, 2, 3]);
        assert_eq!(frame.current_offset_bytes(), 3);
        frame.align_to_bytes(4);
        assert_eq!(frame.current_offset_bytes(), 4);
        assert_eq!(frame.to_bytes(), [1, 2, 3, 0]);

        // Test with already aligned pointer
        let mut vm = test_vm();
        let frame = vm.current_frame_mut();
        frame.push_slice(&[1, 2, 3, 4]);
        frame.align_to_bytes(4);
        assert_eq!(frame.current_offset_bytes(), 4);
    }

    #[test]
    fn test_push_byte() {
        let mut vm = test_vm();
        let frame = vm.current_frame_mut();
        let ptr = frame.push_n(42u8);
        assert_eq!(frame.current_offset_bytes(), 1);
        assert_eq!(frame.to_bytes()[0], 42);
        assert_eq!(ptr.addr(), frame.base_ptr().addr());
    }

    #[test]
    fn test_push_usize() {
        let mut vm = test_vm();
        let frame = vm.current_frame_mut();
        let value = 0x12345678_usize;
        let ptr = frame.push_usize(value);
        assert!(frame.current_offset_bytes() == 4 || frame.current_offset_bytes() == 8); // Depends on platform
        unsafe {
            let read_back = *(ptr as *const usize);
            assert_eq!(read_back as usize, value);
        };
    }

    #[test]
    fn test_push_value_basic() {
        let mut vm = test_vm();
        let types = Types::with_builtin_types();
        let frame = vm.current_frame_mut();

        // Test basic value types
        frame.push_value_no_align(&types, Value::Unit);
        assert_eq!(frame.current_offset_bytes(), 1);
        assert_eq!(frame.to_bytes()[0], crate::typer::UNIT_BYTE_VALUE);

        frame.push_value_no_align(&types, Value::Bool(true));
        assert_eq!(frame.to_bytes()[1], 1);

        frame.push_value_no_align(&types, Value::Char(b'A'));
        assert_eq!(frame.to_bytes()[2], b'A');

        let int_ptr = frame.push_value_no_align(&types, Value::Integer(TypedIntegerValue::U32(42)))
            as *const i32;
        assert_eq!(frame.current_offset_bytes(), 7);
        unsafe {
            let read_back = int_ptr.read_unaligned();
            assert_eq!(read_back, 42);
        }
    }

    #[test]
    fn test_push_value_float() {
        let mut vm = test_vm();
        let types = Types::with_builtin_types();
        let frame = vm.current_frame_mut();

        let f32_val = Value::Float(TypedFloatValue::F32(std::f32::consts::PI));
        let f32ptr = frame.push_value_no_align(&types, f32_val) as *const f32;
        assert_eq!(frame.current_offset_bytes(), 4);
        let read_f32 = unsafe { f32ptr.read() };
        assert_eq!(read_f32, std::f32::consts::PI);

        let f64_val = Value::Float(TypedFloatValue::F64(std::f64::consts::PI));
        let f64_ptr = frame.push_value_no_align(&types, f64_val) as *const f64;

        assert_eq!(frame.current_offset_bytes(), 12);
        assert!(!f64_ptr.is_aligned());
        let read_f64 = unsafe { f64_ptr.read_unaligned() };
        assert_eq!(read_f64, std::f64::consts::PI);

        frame.align_to_bytes(align_of::<f64>());
        let f64_ptr_aligned = frame.push_value_no_align(&types, f64_val) as *const f64;
        assert!(f64_ptr_aligned.is_aligned());
        assert_eq!(frame.current_offset_bytes(), 24);
        let read_f64_aligned = unsafe { f64_ptr.read() };
        assert_eq!(read_f64_aligned, std::f64::consts::PI);
    }

    #[test]
    fn test_push_raw_copy() {
        let mut vm = test_vm();
        let frame = vm.current_frame_mut();
        let source = [1u8, 2, 3, 4];

        let result_ptr = frame.push_raw_copy(4, 4, source.as_ptr());
        assert_eq!(frame.current_offset_bytes(), 4);
        assert_eq!(frame.to_bytes(), [1, 2, 3, 4]);
        unsafe {
            let result_slice = std::slice::from_raw_parts(result_ptr as *const u8, 4);
            assert_eq!(result_slice, [1, 2, 3, 4])
        }
    }

    #[test]
    fn test_push_struct() {
        let mut vm = test_vm();
        let frame = vm.current_frame_mut();
        //frame.push_struct_values
    }
}
