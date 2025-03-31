mod stack_frame {
    use crate::typer::*;
    use crate::vm::StackFrame;

    #[test]
    fn stack_frame_1() {
        let frame = StackFrame { debug_name: "unit".to_string(), vec![] };
    }
}
