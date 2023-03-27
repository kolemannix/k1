; ModuleID = 'resources/test_src/codegen_fn.nx'
source_filename = "resources/test_src/codegen_fn.nx"

@formatString = unnamed_addr constant [2 x i8] c"%i"
@z = addrspace(4) constant i64 1337

define i64 @add(i64 %x, i64 %y) {
entry:
  %x1 = alloca i64, align 8
  %y2 = alloca i64, align 8
  %0 = alloca i64, align 8
  store i64 1, i64* %0, align 4
  %load_lhs = load i64, i64* %x1, align 4
  %load_rhs = load i64, i64* %0, align 4
  %add = add i64 %load_lhs, %load_rhs
  %add_res_ptr = alloca i64, align 8
  store i64 %add, i64* %add_res_ptr, align 4
  %global_ptr = alloca i64, align 8
  store i64 1337, i64* %global_ptr, align 4
  %load_lhs3 = load i64, i64* %add_res_ptr, align 4
  %load_rhs4 = load i64, i64* %global_ptr, align 4
  %add5 = add i64 %load_lhs3, %load_rhs4
  %add_res_ptr6 = alloca i64, align 8
  store i64 %add5, i64* %add_res_ptr6, align 4
  %load_lhs7 = load i64, i64* %add_res_ptr6, align 4
  %load_rhs8 = load i64, i64* %y2, align 4
  %add9 = add i64 %load_lhs7, %load_rhs8
  %add_res_ptr10 = alloca i64, align 8
  store i64 %add9, i64* %add_res_ptr10, align 4
  %ret_val = load i64, i64* %add_res_ptr10, align 4
  ret i64 %ret_val
}

define i64 @main(i64 %x) {
entry:
  %x1 = alloca i64, align 8
  %0 = alloca i64, align 8
  store i64 1, i64* %0, align 4
  %fn_arg = load i64, i64* %0, align 4
  %1 = alloca i64, align 8
  store i64 2, i64* %1, align 4
  %fn_arg2 = load i64, i64* %1, align 4
  %add = call i64 @add(i64 %fn_arg, i64 %fn_arg2)
  %call_result_ptr = alloca i64, align 8
  store i64 %add, i64* %call_result_ptr, align 4
  %println_arg = load i64, i64* %call_result_ptr, align 4
  %println_res = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @formatString, i32 0, i32 0), i64 %println_arg)
  %println_res_ptr = alloca i1, align 1
  store i1 false, i1* %println_res_ptr, align 1
  %2 = alloca i64, align 8
  store i64 0, i64* %2, align 4
  %ret_val = load i64, i64* %2, align 4
  ret i64 %ret_val
}

declare i32 @printf(i8*, ...)
