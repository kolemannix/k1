; ModuleID = 'resources/test_src/codegen_fn.nx'
source_filename = "resources/test_src/codegen_fn.nx"

@formatString = unnamed_addr constant [2 x i8] c"%s"
@z = addrspace(4) constant i32 1337

define i32 @main(i32 %x, i32 %y) {
entry:
  %b = alloca i32, align 4
  %foo = alloca i32, align 4
}
