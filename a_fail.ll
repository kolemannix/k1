; ModuleID = 'a'
source_filename = "builtin.k1"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-darwin24.3.0"

@_root__K1_TEST = constant i8 0
@str_data = constant [5 x i8] c"macos"
@str = constant { { i64, ptr } } { { i64, ptr } { i64 5, ptr @str_data } }
@_root__K1_OS = constant ptr @str
@_root__K1_NO_STD = constant i8 0
@_root__files__unix__SEEK_END = constant i32 2
@_root__files__unix__SEEK_SET = constant i32 0
@_root__Arena__mb = constant i64 1048576
@_root__Arena__gb = constant i64 1073741824

define i64 @main() {
entry:
  %struct_literal = alloca {}, align 8
  %env_ptr = alloca ptr, align 8
  store ptr %struct_literal, ptr %env_ptr, align 8
  %0 = insertvalue { ptr, ptr } { ptr @"main_{lambda}_1075", ptr undef }, ptr %struct_literal, 1
  %lam_obj_ptr = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %0, ptr %lam_obj_ptr, align 8
  %1 = call i8 @simpleDyn(ptr %lam_obj_ptr)
  ret i64 0
}

define i8 @"main_{lambda}_1075"(ptr %__lambda_env_arg) {
entry:
  %__lambda_env = alloca ptr, align 8
  store ptr %__lambda_env_arg, ptr %__lambda_env, align 8
  call void @sys.exit(i32 22)
  unreachable
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #0

define void @sys.exit(i32 %code_arg) {
entry:
  %code = alloca i32, align 4
  store i32 %code_arg, ptr %code, align 4
  %code_loaded = load i32, ptr %code, align 4
  call void @exit(i32 %code_loaded)
  unreachable
}

declare void @exit(i32)

define i8 @simpleDyn(ptr %thunk_arg) {
entry:
  %thunk = alloca { ptr, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %thunk, ptr align 8 %thunk_arg, i64 16, i1 false)
  %fn_ptr_gep = getelementptr inbounds { ptr, ptr }, ptr %thunk, i32 0, i32 0
  %fn_ptr = load ptr, ptr %fn_ptr_gep, align 8
  %env_ptr_gep = getelementptr inbounds { ptr, ptr }, ptr %thunk, i32 0, i32 1
  %env_ptr = load ptr, ptr %env_ptr_gep, align 8
  %0 = call i8 %fn_ptr(ptr %env_ptr)
  ret i8 %0
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #1

attributes #0 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #1 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }

!llvm.module.flags = !{!0, !1, !2, !3}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 14, i32 0]}
!1 = !{i32 2, !"Dwarf Version", i32 4}
!2 = !{i32 2, !"Debug Info Version", i32 3}
!3 = !{i32 1, !"PIE Level", i32 2}
