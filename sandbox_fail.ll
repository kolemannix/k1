; ModuleID = 'sandbox'
source_filename = "builtin.k1"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx15.0.0"

@_root__core__k1__is-static = private unnamed_addr constant i8 0, align 1
@_root__core__io__stdout-buf = private thread_local(localexec) unnamed_addr global { [8192 x i8], i32 } zeroinitializer, align 4
@string_data_static_190 = private unnamed_addr constant [7 x i8] c"core.k1"
@static_190 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_190, i64 7 } } }
@string_data_static_25 = private unnamed_addr constant [119 x i8] c"No cases matched but match was meant to be exhaustive. Either the match subject is corrupt, or there is a compiler bug."
@static_25 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_25, i64 119 } } }
@string_data_static_69 = private unnamed_addr constant [4 x i8] c" at "
@static_69 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_69, i64 4 } } }
@string_data_static_70 = private unnamed_addr constant [1 x i8] c":"
@static_70 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_70, i64 1 } } }
@_root__core__io__stderr = private unnamed_addr constant <{ i32 }> <{ i32 2 }>, align 4
@string_data_static_72 = private unnamed_addr constant [1 x i8] c"\0A"
@static_72 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_72, i64 1 } } }
@_root__core__io__stdout-mode = private thread_local(localexec) unnamed_addr global i8 0, align 1
@_root__core__platform__io__STDOUT = private unnamed_addr constant <{ i32 }> <{ i32 1 }>, align 4
@static_244 = private unnamed_addr constant <{ i64 }> <{ i64 1158 }>, align 8
@string_data_static_195 = private unnamed_addr constant [11 x i8] c"fix-list.k1"
@static_195 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_195, i64 11 } } }
@string_data_static_197 = private unnamed_addr constant [22 x i8] c" cannot fulfill push. "
@static_197 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_197, i64 22 } } }
@string_data_static_130 = private unnamed_addr constant [3 x i8] c" > "
@static_130 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_130, i64 3 } } }
@string_data_static_26 = private unnamed_addr constant [10 x i8] c"builtin.k1"
@static_26 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_26, i64 10 } } }
@string_data_static_53 = private unnamed_addr constant [25 x i8] c"Array index out of bounds"
@static_53 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_53, i64 25 } } }
@string_data_static_73 = private unnamed_addr constant [2 x i8] c": "
@static_73 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_73, i64 2 } } }
@string_data_static_74 = private unnamed_addr constant [4 x i8] c" >= "
@static_74 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_74, i64 4 } } }
@string_data_static_126 = private unnamed_addr constant [9 x i8] c"buffer.k1"
@static_126 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_126, i64 9 } } }
@string_data_static_129 = private unnamed_addr constant [37 x i8] c"Buffer copy dst index out of bounds: "
@static_129 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_129, i64 37 } } }
@_root__core__ptr__null = private unnamed_addr constant ptr null, align 8
@string_data_static_104 = private unnamed_addr constant [8 x i8] c"arena.k1"
@static_104 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_104, i64 8 } } }
@string_data_static_110 = private unnamed_addr constant [21 x i8] c"fixed arena exhausted"
@static_110 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_110, i64 21 } } }
@_root__core__platform__port__posix__c__PROT_READ = private unnamed_addr constant i32 1, align 4
@_root__core__platform__port__posix__c__PROT_WRITE = private unnamed_addr constant i32 2, align 4
@string_data_static_255 = private unnamed_addr constant [11 x i8] c"platform.k1"
@static_255 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_255, i64 11 } } }
@string_data_static_260 = private unnamed_addr constant [15 x i8] c"mprotect failed"
@static_260 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_260, i64 15 } } }
@_root__core__platform__port__posix__c__MAP_ANON = private unnamed_addr constant i32 4096, align 4
@_root__core__platform__port__posix__c__MAP_PRIVATE = private unnamed_addr constant i32 2, align 4
@_root__core__platform__port__posix__c__PROT_NONE = private unnamed_addr constant i32 0, align 4
@_root__core__platform__port__posix__c__MAP_FAILED = private unnamed_addr constant i64 -1, align 8
@string_data_static_257 = private unnamed_addr constant [11 x i8] c"mmap failed"
@static_257 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_257, i64 11 } } }
@_root__core__mem__arena-tmp = private thread_local(localexec) unnamed_addr global { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } } zeroinitializer, align 8
@string_data_static_158 = private unnamed_addr constant [6 x i8] c"mem.k1"
@static_158 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_158, i64 6 } } }
@string_data_static_180 = private unnamed_addr constant [29 x i8] c"arena-tmp already initialized"
@static_180 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_180, i64 29 } } }
@string_data_static_107 = private unnamed_addr constant [0 x i8] zeroinitializer
@static_107 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_107, i64 0 } } }
@string_data_static_64 = private unnamed_addr constant [13 x i8] c"ASSERT FAILED"
@static_64 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_64, i64 13 } } }
@string_data_static_136 = private unnamed_addr constant [33 x i8] c"slice start index out of bounds: "
@static_136 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_136, i64 33 } } }
@string_data_static_137 = private unnamed_addr constant [13 x i8] c". length is: "
@static_137 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_137, i64 13 } } }
@string_data_static_133 = private unnamed_addr constant [21 x i8] c"index out of bounds: "
@static_133 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_133, i64 21 } } }
@string_data_static_134 = private unnamed_addr constant [3 x i8] c" / "
@static_134 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_134, i64 3 } } }
@_root__core__mem__arena-stack = private thread_local(localexec) unnamed_addr global { [64 x ptr], i32 } zeroinitializer, align 8
@string_data_typename_2 = private unnamed_addr constant [2 x i8] c"u8"
@typename_2 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_2, i64 2 } } }
@string_data_typename_4 = private unnamed_addr constant [3 x i8] c"u32"
@typename_4 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_4, i64 3 } } }
@string_data_typename_5 = private unnamed_addr constant [3 x i8] c"u64"
@typename_5 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_5, i64 3 } } }
@string_data_typename_9 = private unnamed_addr constant [3 x i8] c"i64"
@typename_9 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_9, i64 3 } } }
@string_data_typename_10 = private unnamed_addr constant [4 x i8] c"char"
@typename_10 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_10, i64 4 } } }
@string_data_typename_13 = private unnamed_addr constant [3 x i8] c"ptr"
@typename_13 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_13, i64 3 } } }
@string_data_typename_33 = private unnamed_addr constant [6 x i8] c"string"
@typename_33 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_33, i64 6 } } }
@string_data_typename_104 = private unnamed_addr constant [7 x i8] c"*string"
@typename_104 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_104, i64 7 } } }
@string_data_typename_105 = private unnamed_addr constant [14 x i8] c"buffer[string]"
@typename_105 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_105, i64 14 } } }
@string_data_typename_106 = private unnamed_addr constant [12 x i8] c"list[string]"
@typename_106 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_106, i64 12 } } }
@string_data_typename_150 = private unnamed_addr constant [10 x i8] c"code-chunk"
@typename_150 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_150, i64 10 } } }
@string_data_typename_152 = private unnamed_addr constant [4 x i8] c"code"
@typename_152 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_152, i64 4 } } }
@string_data_typename_153 = private unnamed_addr constant [11 x i8] c"*code-chunk"
@typename_153 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_153, i64 11 } } }
@string_data_typename_154 = private unnamed_addr constant [18 x i8] c"buffer[code-chunk]"
@typename_154 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_154, i64 18 } } }
@string_data_typename_155 = private unnamed_addr constant [16 x i8] c"list[code-chunk]"
@typename_155 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_155, i64 16 } } }
@string_data_typename_1157 = private unnamed_addr constant [15 x i8] c"array[u8, 8192]"
@typename_1157 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_1157, i64 15 } } }
@string_data_typename_1158 = private unnamed_addr constant [30 x i8] c"fixlist[u8, static[i64, 8192]]"
@typename_1158 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_1158, i64 30 } } }
@string_data_typename_1178 = private unnamed_addr constant [11 x i8] c"opt[string]"
@typename_1178 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_1178, i64 11 } } }
@string_data_typename_1851 = private unnamed_addr constant [14 x i8] c"array[ptr, 64]"
@typename_1851 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_1851, i64 14 } } }
@string_data_typename_1852 = private unnamed_addr constant [29 x i8] c"fixlist[ptr, static[i64, 64]]"
@typename_1852 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_1852, i64 29 } } }
@string_data_typename_3235 = private unnamed_addr constant [15 x i8] c"array[char, 64]"
@typename_3235 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_3235, i64 15 } } }
@string_data_typename_3236 = private unnamed_addr constant [30 x i8] c"fixlist[char, static[i64, 64]]"
@typename_3236 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_typename_3236, i64 30 } } }
@static_221 = private unnamed_addr constant <{ i64 }> <{ i64 3236 }>, align 8
@string_data_static_192 = private unnamed_addr constant [25 x i8] c"u8/to-ascii-digit: value "
@static_192 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_192, i64 25 } } }
@string_data_static_193 = private unnamed_addr constant [25 x i8] c" is out of range (max 61)"
@static_193 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_193, i64 25 } } }
@string_data_static_225 = private unnamed_addr constant [20 x i8] c"fixlist out of space"
@static_225 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_225, i64 20 } } }
@_root__core__platform__port__posix__c__EINTR = private unnamed_addr constant i32 4, align 4
@static_elems_1154 = private unnamed_addr constant <{ i64, i64, i64, i64, i64 }> <{ i64 1, i64 2, i64 3, i64 4, i64 5 }>, align 8
@static_1154 = private unnamed_addr constant { ptr, i64 } { ptr @static_elems_1154, i64 5 }, align 8
@_root__core__io__stdout = private unnamed_addr constant <{ i32 }> <{ i32 1 }>, align 4
@string_data_static_59 = private unnamed_addr constant [41 x i8] c"array.from-buffer with mismatching length"
@static_59 = private unnamed_addr constant { { { ptr, i64 } } } { { { ptr, i64 } } { { ptr, i64 } { ptr @string_data_static_59, i64 41 } } }

; Function Attrs: nounwind
define internal i32 @sandbox.main_6208() #0 !dbg !7 {
prelude:
  %abi_caller_copy = alloca [5 x i64], align 8
  %0 = alloca [5 x i64], align 8
  br label %entry

entry:                                            ; preds = %prelude
  %1 = load [2 x i64], ptr @static_1154, align 8
  call fastcc void @core.span.to-array_6209(ptr noalias sret([5 x i64]) align 8 dereferenceable(40) %0, [2 x i64] %1)
  call void @llvm.lifetime.start.p0(i64 40, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %0, i64 40, i1 false)
  call fastcc void @core.println_6211(ptr %abi_caller_copy), !dbg !12
  call void @llvm.lifetime.end.p0(i64 40, ptr %abi_caller_copy), !dbg !12
  ret i32 0, !dbg !13
}

; Function Attrs: noreturn nounwind
define internal fastcc void @core.k1.program-exit_244(i32 %code) #1 !dbg !14 {
prelude:
  br label %entry

entry:                                            ; preds = %prelude
  call fastcc void @core.sys.exit_255(i32 %code), !dbg !19
  unreachable, !dbg !19
}

; Function Attrs: noreturn nounwind
define internal fastcc void @core.sys.exit_255(i32 %code) #1 !dbg !20 {
prelude:
  br label %entry

entry:                                            ; preds = %prelude
  call fastcc void @core.io.flush-stdout_583(), !dbg !24
  %0 = load i8, ptr @_root__core__k1__is-static, align 1, !dbg !25
  %1 = trunc i8 %0 to i1, !dbg !25
  br i1 %1, label %arm_cons, label %entry1, !dbg !25

arm_cons:                                         ; preds = %entry
  call void @core.k1.exit_243(i32 %code), !dbg !25
  unreachable, !dbg !25

entry1:                                           ; preds = %entry
  call void @exit(i32 %code), !dbg !25
  unreachable, !dbg !25
}

; Function Attrs: nounwind
define internal fastcc void @core.io.flush-stdout_583() #0 !dbg !26 {
prelude:
  %abi_pair_storage = alloca [2 x i64], align 8
  %0 = alloca { ptr, i64 }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %1 = load i32, ptr getelementptr inbounds nuw ({ [8192 x i8], i32 }, ptr @_root__core__io__stdout-buf, i32 0, i32 1), align 4, !dbg !31
  %2 = zext i32 %1 to i64, !dbg !31
  %3 = icmp sgt i64 %2, 0, !dbg !31
  %4 = zext i1 %3 to i8, !dbg !31
  %5 = trunc i8 %4 to i1, !dbg !32
  br i1 %5, label %arm_cons, label %match_end, !dbg !32

arm_cons:                                         ; preds = %entry
  %6 = call fastcc [2 x i64] @core.impl_23as-span.as-span_for_t3313_1990(ptr @_root__core__io__stdout-buf), !dbg !33
  store [2 x i64] %6, ptr %abi_pair_storage, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !33
  %7 = load ptr, ptr %0, align 8, !dbg !33
  %8 = load i32, ptr getelementptr inbounds nuw ({ [8192 x i8], i32 }, ptr @_root__core__io__stdout-buf, i32 0, i32 1), align 4, !dbg !34
  %9 = zext i32 %8 to i64, !dbg !34
  call fastcc void @core.io.write-all_582(i32 1, ptr %7, i64 %9), !dbg !35
  store i32 0, ptr getelementptr inbounds nuw ({ [8192 x i8], i32 }, ptr @_root__core__io__stdout-buf, i32 0, i32 1), align 4, !dbg !36
  br label %match_end, !dbg !32

match_end:                                        ; preds = %arm_cons, %entry
  ret void, !dbg !32
}

; Function Attrs: noreturn nounwind
declare !dbg !37 void @exit(i32) #1

; Function Attrs: noreturn nounwind
define internal void @core.k1.exit_243(i32 %code) #1 !dbg !41 {
prelude:
  unreachable, !dbg !42
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.impl_23as-span.as-span_for_t3313_1990(ptr %self) #0 !dbg !43 {
prelude:
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  %0 = alloca { { ptr, i64 } }, align 8
  %1 = alloca { ptr, i64 }, align 8
  %2 = alloca { { ptr, i64 } }, align 8
  %3 = alloca { ptr, i64 }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %4 = call fastcc [2 x i64] @core.impl_22as-buffer.as-buffer_for_t3358_2019(ptr %self), !dbg !55
  store [2 x i64] %4, ptr %abi_pair_storage, align 8
  %5 = getelementptr inbounds nuw { [8192 x i8], i32 }, ptr %self, i32 0, i32 1, !dbg !55
  %6 = load i32, ptr %5, align 4, !dbg !55
  %7 = zext i32 %6 to i64, !dbg !55
  %8 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !55
  %9 = call fastcc [2 x i64] @core.buffer.slice_1776([2 x i64] %8, i64 0, i64 %7), !dbg !55
  store [2 x i64] %9, ptr %abi_pair_storage1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %abi_pair_storage1, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %1, i64 16, i1 false), !dbg !55
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %3, i64 16, i1 false), !dbg !56
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %2, i64 16, i1 false), !dbg !56
  %10 = load [2 x i64], ptr %0, align 8, !dbg !56
  ret [2 x i64] %10, !dbg !56
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias writeonly captures(none), ptr noalias readonly captures(none), i64, i1 immarg) #2

; Function Attrs: nounwind
define internal fastcc void @core.io.write-all_582(i32 %file-no, ptr %data, i64 %count) #0 !dbg !57 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  %abi_struct_int = alloca i64, align 8
  %0 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %1 = alloca { i64 }, align 8
  %2 = alloca i64, align 8
  %3 = alloca i32, align 4
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  %6 = alloca { i32 }, align 4
  %7 = alloca { i8, { i64 } }, align 8
  %8 = alloca i64, align 8
  %9 = alloca i64, align 8
  %10 = alloca { i64 }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  store i64 0, ptr %9, align 8, !dbg !61
  br label %while_loop_condition, !dbg !62

while_loop_condition:                             ; preds = %match_end5, %entry
  %11 = load i64, ptr %9, align 8, !dbg !63
  %12 = icmp slt i64 %11, %count, !dbg !63
  %13 = zext i1 %12 to i8, !dbg !63
  %14 = trunc i8 %13 to i1, !dbg !62
  br i1 %14, label %while_loop_body, label %while_loop_end, !dbg !62

while_loop_body:                                  ; preds = %while_loop_condition
  store i32 %file-no, ptr %6, align 4, !dbg !64
  %15 = load i64, ptr %9, align 8, !dbg !65
  %16 = getelementptr inbounds i8, ptr %data, i64 %15, !dbg !66
  %17 = load i64, ptr %9, align 8, !dbg !67
  %18 = sub i64 %count, %17, !dbg !68
  %19 = load i8, ptr @_root__core__k1__is-static, align 1, !dbg !69
  %20 = trunc i8 %19 to i1, !dbg !69
  br i1 %20, label %arm_cons, label %arm_cons1, !dbg !69

arm_cons:                                         ; preds = %while_loop_body
  %21 = load i32, ptr %6, align 4, !dbg !69
  %22 = call fastcc i64 @core.platform.port.posix.write_1043(i32 %21, ptr %16, i64 %18), !dbg !69
  store i64 %22, ptr %2, align 8, !dbg !69
  br label %match_end, !dbg !69

arm_cons1:                                        ; preds = %while_loop_body
  %23 = load i32, ptr %6, align 4, !dbg !69
  %24 = call fastcc i64 @core.platform.port.posix.write_1043(i32 %23, ptr %16, i64 %18), !dbg !69
  store i64 %24, ptr %2, align 8, !dbg !69
  br label %match_end, !dbg !69

match_end:                                        ; preds = %arm_cons, %arm_cons1
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %2, i64 8, i1 false), !dbg !69
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %10, ptr align 8 %1, i64 8, i1 false), !dbg !69
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int)
  store i64 0, ptr %abi_struct_int, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int, ptr align 8 %10, i64 8, i1 false)
  %25 = load i64, ptr %abi_struct_int, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int)
  %26 = call fastcc [2 x i64] @core.platform.io.impl_16try.result_for_t185_962(i64 %25), !dbg !69
  store [2 x i64] %26, ptr %abi_pair_storage, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 8 %abi_pair_storage, i64 16, i1 false)
  %27 = load i8, ptr %7, align 1, !dbg !70
  %28 = icmp eq i8 %27, 0, !dbg !70
  %29 = zext i1 %28 to i8, !dbg !70
  %30 = trunc i8 %29 to i1, !dbg !69
  br i1 %30, label %matching_cond_continue, label %arm_cond, !dbg !69

while_loop_end:                                   ; preds = %while_loop_condition
  ret void, !dbg !62

arm_cond:                                         ; preds = %match_end
  %31 = load i8, ptr %7, align 1, !dbg !71
  %32 = icmp eq i8 %31, 1, !dbg !71
  %33 = zext i1 %32 to i8, !dbg !71
  %34 = trunc i8 %33 to i1, !dbg !69
  br i1 %34, label %matching_cond_continue3, label %arm_cons2, !dbg !69

arm_cons2:                                        ; preds = %arm_cond
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @static_190, i64 16, i1 false), !dbg !69
  %35 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %0, i32 0, i32 1, !dbg !69
  store i64 69, ptr %35, align 8, !dbg !69
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %0, i64 24, i1 false)
  %36 = load [2 x i64], ptr @static_25, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %36), !dbg !69
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !69
  unreachable, !dbg !69

matching_cond_continue:                           ; preds = %match_end
  %37 = getelementptr inbounds nuw { i8, { i64 } }, ptr %7, i32 0, i32 1, !dbg !70
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %37, i64 8, i1 false), !dbg !70
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %5, i64 8, i1 false), !dbg !70
  %38 = load i64, ptr %4, align 8, !dbg !72
  store i64 %38, ptr %8, align 8, !dbg !69
  %39 = load i64, ptr %8, align 8, !dbg !73
  %40 = icmp eq i64 %39, 0, !dbg !73
  %41 = zext i1 %40 to i8, !dbg !73
  %42 = trunc i8 %41 to i1, !dbg !74
  br i1 %42, label %arm_cons4, label %match_end5, !dbg !74

matching_cond_continue3:                          ; preds = %arm_cond
  %43 = getelementptr inbounds nuw { i8, { i64 } }, ptr %7, i32 0, i32 1, !dbg !71
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %3, ptr align 4 %43, i64 4, i1 false), !dbg !71
  ret void, !dbg !75

arm_cons4:                                        ; preds = %matching_cond_continue
  ret void, !dbg !76

match_end5:                                       ; preds = %matching_cond_continue
  %44 = load i64, ptr %9, align 8, !dbg !77
  %45 = load i64, ptr %8, align 8, !dbg !78
  %46 = add i64 %44, %45, !dbg !77
  store i64 %46, ptr %9, align 8, !dbg !79
  br label %while_loop_condition, !dbg !62
}

; Function Attrs: nounwind
define internal fastcc i64 @core.platform.port.posix.write_1043(i32 %file-no, ptr %data, i64 %count) #0 !dbg !80 {
prelude:
  %0 = alloca ptr, align 8
  %1 = alloca ptr, align 8
  %2 = alloca i64, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %3 = call i64 @write(i32 %file-no, ptr %data, i64 %count), !dbg !83
  store i64 %3, ptr %2, align 8, !dbg !83
  br label %while_loop_condition, !dbg !84

while_loop_condition:                             ; preds = %entry1, %entry
  %4 = load i64, ptr %2, align 8, !dbg !85
  %5 = icmp slt i64 %4, 0, !dbg !85
  %6 = zext i1 %5 to i8, !dbg !85
  %7 = trunc i8 %6 to i1, !dbg !84
  br i1 %7, label %arm_cond, label %while_loop_end, !dbg !84

entry1:                                           ; preds = %match_end
  %8 = call i64 @write(i32 %file-no, ptr %data, i64 %count), !dbg !86
  store i64 %8, ptr %2, align 8, !dbg !87
  br label %while_loop_condition, !dbg !84

while_loop_end:                                   ; preds = %match_end, %while_loop_condition
  %9 = load i64, ptr %2, align 8, !dbg !88
  %10 = icmp slt i64 %9, 0, !dbg !88
  %11 = zext i1 %10 to i8, !dbg !88
  %12 = trunc i8 %11 to i1, !dbg !89
  br i1 %12, label %arm_cond3, label %arm_cons7, !dbg !89

arm_cond:                                         ; preds = %while_loop_condition
  %13 = load i8, ptr @_root__core__k1__is-static, align 1, !dbg !90
  %14 = trunc i8 %13 to i1, !dbg !90
  br i1 %14, label %arm_cons, label %arm_cons2, !dbg !90

arm_cons:                                         ; preds = %arm_cond
  %15 = call ptr @__error(), !dbg !90
  store ptr %15, ptr %0, align 8, !dbg !90
  br label %match_end, !dbg !90

arm_cons2:                                        ; preds = %arm_cond
  %16 = call ptr @__error(), !dbg !90
  store ptr %16, ptr %0, align 8, !dbg !90
  br label %match_end, !dbg !90

match_end:                                        ; preds = %arm_cons, %arm_cons2
  %17 = load ptr, ptr %0, align 8, !dbg !90
  %18 = load i32, ptr %17, align 4, !dbg !90
  %19 = load i32, ptr @_root__core__platform__port__posix__c__EINTR, align 4, !dbg !91
  %20 = icmp eq i32 %18, %19, !dbg !90
  %21 = zext i1 %20 to i8, !dbg !90
  %22 = trunc i8 %21 to i1, !dbg !84
  br i1 %22, label %entry1, label %while_loop_end, !dbg !84

arm_cond3:                                        ; preds = %while_loop_end
  %23 = load i8, ptr @_root__core__k1__is-static, align 1, !dbg !92
  %24 = trunc i8 %23 to i1, !dbg !92
  br i1 %24, label %arm_cons4, label %arm_cons5, !dbg !92

arm_cons4:                                        ; preds = %arm_cond3
  %25 = call ptr @__error(), !dbg !92
  store ptr %25, ptr %1, align 8, !dbg !92
  br label %match_end6, !dbg !92

arm_cons5:                                        ; preds = %arm_cond3
  %26 = call ptr @__error(), !dbg !92
  store ptr %26, ptr %1, align 8, !dbg !92
  br label %match_end6, !dbg !92

match_end6:                                       ; preds = %arm_cons4, %arm_cons5
  %27 = load ptr, ptr %1, align 8, !dbg !92
  %28 = load i32, ptr %27, align 4, !dbg !92
  %29 = sext i32 %28 to i64, !dbg !92
  %30 = sub i64 0, %29, !dbg !93
  br label %match_end8, !dbg !89

arm_cons7:                                        ; preds = %while_loop_end
  %31 = load i64, ptr %2, align 8, !dbg !94
  br label %match_end8, !dbg !89

match_end8:                                       ; preds = %match_end6, %arm_cons7
  %32 = phi i64 [ %30, %match_end6 ], [ %31, %arm_cons7 ], !dbg !89
  ret i64 %32, !dbg !89
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.platform.io.impl_16try.result_for_t185_962(i64 %self) #0 !dbg !95 {
prelude:
  %0 = alloca { i8, { i64 } }, align 8
  %1 = alloca { i8, { i64 } }, align 8
  %2 = alloca { i8, { i64 } }, align 8
  %struct_in_integer_storage = alloca { i64 }, align 8
  store i64 %self, ptr %struct_in_integer_storage, align 8, !dbg !108
  br label %entry

entry:                                            ; preds = %prelude
  %3 = load i64, ptr %struct_in_integer_storage, align 8, !dbg !109
  %4 = icmp sge i64 %3, 0, !dbg !109
  %5 = zext i1 %4 to i8, !dbg !109
  %6 = trunc i8 %5 to i1, !dbg !110
  br i1 %6, label %arm_cons, label %arm_cons1, !dbg !110

arm_cons:                                         ; preds = %entry
  store i8 0, ptr %1, align 1, !dbg !111
  %7 = getelementptr inbounds nuw { i8, { i64 } }, ptr %1, i32 0, i32 1, !dbg !111
  %8 = load i64, ptr %struct_in_integer_storage, align 8, !dbg !112
  store i64 %8, ptr %7, align 8, !dbg !112
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %1, i64 16, i1 false), !dbg !110
  br label %match_end, !dbg !110

arm_cons1:                                        ; preds = %entry
  store i8 1, ptr %0, align 1, !dbg !113
  %9 = getelementptr inbounds nuw { i8, { i64 } }, ptr %0, i32 0, i32 1, !dbg !113
  %10 = load i64, ptr %struct_in_integer_storage, align 8, !dbg !114
  %11 = sub i64 0, %10, !dbg !114
  %12 = trunc i64 %11 to i32, !dbg !114
  store i32 %12, ptr %9, align 4, !dbg !114
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %0, i64 16, i1 false), !dbg !110
  br label %match_end, !dbg !110

match_end:                                        ; preds = %arm_cons, %arm_cons1
  %13 = load [2 x i64], ptr %2, align 8, !dbg !110
  ret [2 x i64] %13, !dbg !110
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr captures(none)) #3

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr captures(none)) #3

; Function Attrs: noreturn nounwind
define internal fastcc void @core.crash_335(ptr %locn, [2 x i64] %msg) #1 !dbg !115 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_struct_int = alloca i64, align 8
  %0 = alloca { ptr, i64 }, align 8
  %1 = alloca { { ptr, i64 } }, align 8
  %2 = alloca { { ptr, i64 } }, align 8
  %3 = alloca { { ptr, i64 } }, align 8
  %4 = alloca { ptr, i64 }, align 8
  %5 = alloca { ptr, i64 }, align 8
  %6 = alloca { i64 }, align 8
  %7 = alloca i64, align 8
  %8 = alloca { { { ptr, i64 } } }, align 8
  %9 = alloca { i64 }, align 8
  %10 = alloca { { { ptr, i64 } } }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %msg, ptr %abi_pair_storage, align 8, !dbg !134
  br label %entry

entry:                                            ; preds = %prelude
  %11 = load i8, ptr @_root__core__k1__is-static, align 1, !dbg !135
  %12 = trunc i8 %11 to i1, !dbg !136
  br i1 %12, label %arm_cons, label %arm_cons3, !dbg !136

arm_cons:                                         ; preds = %entry
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %locn, i64 24, i1 false)
  %13 = load [2 x i64], ptr %abi_pair_storage, align 8
  call void @core.k1.emit-compiler-message_245(ptr %abi_caller_copy, i8 2, [2 x i64] %13), !dbg !137
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !137
  call fastcc void @core.io.flush-stdout_583(), !dbg !138
  %14 = load i8, ptr @_root__core__k1__is-static, align 1, !dbg !138
  %15 = trunc i8 %14 to i1, !dbg !138
  br i1 %15, label %arm_cons1, label %entry2, !dbg !138

arm_cons1:                                        ; preds = %arm_cons
  call void @core.k1.exit_243(i32 111), !dbg !138
  unreachable, !dbg !138

entry2:                                           ; preds = %arm_cons
  call void @exit(i32 111), !dbg !138
  unreachable, !dbg !138

arm_cons3:                                        ; preds = %entry
  call fastcc void @core.io.flush-stdout_583(), !dbg !139
  call void @_k1_print_backtrace(i32 128), !dbg !140
  %16 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !141
  call fastcc void @core.eprint_1488([2 x i64] %16), !dbg !141
  %17 = load [2 x i64], ptr @static_69, align 8, !dbg !142
  call fastcc void @core.eprint_1488([2 x i64] %17), !dbg !142
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %8, ptr align 8 %locn, i64 16, i1 false), !dbg !143
  %18 = load [2 x i64], ptr %8, align 8, !dbg !144
  call fastcc void @core.eprint_1488([2 x i64] %18), !dbg !144
  %19 = load [2 x i64], ptr @static_70, align 8, !dbg !145
  call fastcc void @core.eprint_1488([2 x i64] %19), !dbg !145
  %20 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %locn, i32 0, i32 1, !dbg !146
  %21 = load i64, ptr %20, align 8, !dbg !146
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int)
  store i64 0, ptr %abi_struct_int, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int, ptr align 4 @_root__core__io__stderr, i64 4, i1 false)
  %22 = load i64, ptr %abi_struct_int, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int)
  call fastcc void @core.format-uint_1489(i64 %22, i64 %21, i64 10), !dbg !147
  %23 = load [2 x i64], ptr @static_72, align 8, !dbg !148
  call fastcc void @core.eprint_1488([2 x i64] %23), !dbg !148
  %24 = load i8, ptr @_root__core__k1__is-static, align 1, !dbg !149
  %25 = trunc i8 %24 to i1, !dbg !149
  br i1 %25, label %entry4, label %entry5, !dbg !149

entry4:                                           ; preds = %arm_cons3
  call void @abort(), !dbg !149
  unreachable, !dbg !149

entry5:                                           ; preds = %arm_cons3
  call void @abort(), !dbg !149
  unreachable, !dbg !149
}

; Function Attrs: nounwind
declare !dbg !150 void @_k1_print_backtrace(i32) #0

; Function Attrs: nounwind
define internal fastcc void @core.eprint_1488([2 x i64] %t) #0 !dbg !154 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_struct_int = alloca i64, align 8
  %abi_pair_storage3 = alloca [2 x i64], align 8
  %0 = alloca { { { ptr, i64 } } }, align 8
  %1 = alloca { { ptr, i64 } }, align 8
  %2 = alloca { { ptr, i64 } }, align 8
  %3 = alloca { { ptr, i64 } }, align 8
  %4 = alloca { { ptr, i64 } }, align 8
  %5 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %6 = alloca { { { ptr, i64 } } }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %t, ptr %abi_pair_storage, align 8, !dbg !157
  br label %entry

entry:                                            ; preds = %prelude
  %7 = load i8, ptr @_root__core__k1__is-static, align 1, !dbg !158
  %8 = trunc i8 %7 to i1, !dbg !159
  br i1 %8, label %entry1, label %entry2, !dbg !159

entry1:                                           ; preds = %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !160
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %0, i64 16, i1 false), !dbg !160
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 @static_190, i64 16, i1 false), !dbg !161
  %9 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %5, i32 0, i32 1, !dbg !161
  store i64 39, ptr %9, align 8, !dbg !161
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %5, i64 24, i1 false)
  %10 = load [2 x i64], ptr %6, align 8
  call void @core.k1.emit-compiler-message_245(ptr %abi_caller_copy, i8 1, [2 x i64] %10), !dbg !162
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !162
  br label %match_end, !dbg !159

entry2:                                           ; preds = %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !163
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %3, i64 16, i1 false), !dbg !163
  %11 = load [2 x i64], ptr %4, align 8, !dbg !163
  %12 = call fastcc [2 x i64] @core.span.as-byte-span_1175([2 x i64] %11), !dbg !163
  store [2 x i64] %12, ptr %abi_pair_storage3, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %abi_pair_storage3, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %2, i64 16, i1 false), !dbg !163
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int)
  store i64 0, ptr %abi_struct_int, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int, ptr align 4 @_root__core__io__stderr, i64 4, i1 false)
  %13 = load i64, ptr %abi_struct_int, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int)
  %14 = load [2 x i64], ptr %1, align 8
  call fastcc void @core.io.impl_4writer.write-bytes_for_t162_586(i64 %13, [2 x i64] %14), !dbg !163
  br label %match_end, !dbg !159

match_end:                                        ; preds = %entry1, %entry2
  ret void, !dbg !159
}

; Function Attrs: nounwind
define internal fastcc void @core.format-uint_1489(i64 %w, i64 %value, i64 %base) #0 !dbg !164 {
prelude:
  %abi_struct_int13 = alloca i64, align 8
  %abi_struct_int12 = alloca i64, align 8
  %abi_struct_int11 = alloca i64, align 8
  %abi_struct_int10 = alloca i64, align 8
  %abi_struct_int = alloca i64, align 8
  %abi_pair_storage9 = alloca [2 x i64], align 8
  %abi_pair_storage8 = alloca [2 x i64], align 8
  %abi_pair_storage7 = alloca [2 x i64], align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  %0 = alloca { [64 x i8], i32 }, align 4
  %1 = alloca { ptr, i64 }, align 8
  %2 = alloca { ptr, i64 }, align 8
  %3 = alloca i8, align 1
  %4 = alloca i64, align 8
  %5 = alloca { [64 x i8], i32 }, align 4
  %6 = alloca i64, align 8
  %7 = alloca i8, align 1
  %8 = alloca i64, align 8
  %9 = alloca i8, align 1
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %struct_in_integer_storage = alloca { i32 }, align 4
  %12 = trunc i64 %w to i32, !dbg !170
  store i32 %12, ptr %struct_in_integer_storage, align 4, !dbg !170
  br label %entry

entry:                                            ; preds = %prelude
  %13 = icmp eq i64 %value, 0, !dbg !171
  %14 = zext i1 %13 to i8, !dbg !171
  %15 = trunc i8 %14 to i1, !dbg !172
  br i1 %15, label %entry1, label %arm_cond, !dbg !172

entry1:                                           ; preds = %entry
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int13)
  store i64 0, ptr %abi_struct_int13, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int13, ptr align 4 %struct_in_integer_storage, i64 4, i1 false)
  %16 = load i64, ptr %abi_struct_int13, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int13)
  call fastcc void @core.io.impl_4writer.write-byte_for_t162_585(i64 %16, i8 48), !dbg !173
  ret void, !dbg !174

arm_cond:                                         ; preds = %entry
  %17 = icmp ult i64 %value, %base, !dbg !175
  %18 = zext i1 %17 to i8, !dbg !175
  %19 = trunc i8 %18 to i1, !dbg !176
  br i1 %19, label %arm_cons, label %arm_cond2, !dbg !176

arm_cons:                                         ; preds = %arm_cond
  %20 = trunc i64 %value to i8, !dbg !177
  %21 = call fastcc i8 @core.u8.to-ascii-digit_644(i8 %20), !dbg !178
  store i8 %21, ptr %11, align 1, !dbg !178
  %22 = load i8, ptr %11, align 1, !dbg !179
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int12)
  store i64 0, ptr %abi_struct_int12, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int12, ptr align 4 %struct_in_integer_storage, i64 4, i1 false)
  %23 = load i64, ptr %abi_struct_int12, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int12)
  call fastcc void @core.io.impl_4writer.write-byte_for_t162_585(i64 %23, i8 %22), !dbg !180
  br label %match_end, !dbg !176

match_end:                                        ; preds = %arm_cons, %match_end5
  ret void, !dbg !172

arm_cond2:                                        ; preds = %arm_cond
  %24 = mul i64 %base, 2, !dbg !181
  %25 = icmp ult i64 %value, %24, !dbg !182
  %26 = zext i1 %25 to i8, !dbg !182
  %27 = trunc i8 %26 to i1, !dbg !183
  br i1 %27, label %arm_cons3, label %arm_cons4, !dbg !183

arm_cons3:                                        ; preds = %arm_cond2
  %28 = udiv i64 %value, %base, !dbg !184
  store i64 %28, ptr %10, align 8, !dbg !184
  %29 = load i64, ptr %10, align 8, !dbg !185
  %30 = trunc i64 %29 to i8, !dbg !185
  %31 = call fastcc i8 @core.u8.to-ascii-digit_644(i8 %30), !dbg !186
  store i8 %31, ptr %9, align 1, !dbg !186
  %32 = urem i64 %value, %base, !dbg !187
  store i64 %32, ptr %8, align 8, !dbg !187
  %33 = load i64, ptr %8, align 8, !dbg !188
  %34 = trunc i64 %33 to i8, !dbg !188
  %35 = call fastcc i8 @core.u8.to-ascii-digit_644(i8 %34), !dbg !189
  store i8 %35, ptr %7, align 1, !dbg !189
  %36 = load i8, ptr %9, align 1, !dbg !190
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int10)
  store i64 0, ptr %abi_struct_int10, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int10, ptr align 4 %struct_in_integer_storage, i64 4, i1 false)
  %37 = load i64, ptr %abi_struct_int10, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int10)
  call fastcc void @core.io.impl_4writer.write-byte_for_t162_585(i64 %37, i8 %36), !dbg !191
  %38 = load i8, ptr %7, align 1, !dbg !192
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int11)
  store i64 0, ptr %abi_struct_int11, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int11, ptr align 4 %struct_in_integer_storage, i64 4, i1 false)
  %39 = load i64, ptr %abi_struct_int11, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int11)
  call fastcc void @core.io.impl_4writer.write-byte_for_t162_585(i64 %39, i8 %38), !dbg !193
  br label %match_end5, !dbg !183

arm_cons4:                                        ; preds = %arm_cond2
  store i64 %value, ptr %6, align 8, !dbg !194
  %40 = getelementptr inbounds nuw { [64 x i8], i32 }, ptr %0, i32 0, i32 1, !dbg !195
  store i32 0, ptr %40, align 4, !dbg !195
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %5, ptr align 4 %0, i64 68, i1 false), !dbg !195
  br label %while_loop_condition, !dbg !196

match_end5:                                       ; preds = %arm_cons3, %entry6
  br label %match_end, !dbg !176

while_loop_condition:                             ; preds = %while_loop_body, %arm_cons4
  %41 = load i64, ptr %6, align 8, !dbg !197
  %42 = icmp ugt i64 %41, 0, !dbg !197
  %43 = zext i1 %42 to i8, !dbg !197
  %44 = trunc i8 %43 to i1, !dbg !196
  br i1 %44, label %while_loop_body, label %entry6, !dbg !196

while_loop_body:                                  ; preds = %while_loop_condition
  %45 = load i64, ptr %6, align 8, !dbg !198
  %46 = urem i64 %45, %base, !dbg !198
  store i64 %46, ptr %4, align 8, !dbg !198
  %47 = load i64, ptr %4, align 8, !dbg !199
  %48 = trunc i64 %47 to i8, !dbg !199
  %49 = call fastcc i8 @core.u8.to-ascii-digit_644(i8 %48), !dbg !200
  store i8 %49, ptr %3, align 1, !dbg !200
  %50 = load i8, ptr %3, align 1, !dbg !201
  call fastcc void @core.fixlist.push_1882(ptr %5, i8 %50), !dbg !202
  %51 = load i64, ptr %6, align 8, !dbg !203
  %52 = udiv i64 %51, %base, !dbg !203
  store i64 %52, ptr %6, align 8, !dbg !204
  br label %while_loop_condition, !dbg !196

entry6:                                           ; preds = %while_loop_condition
  %53 = call fastcc [2 x i64] @core.impl_22as-buffer.as-buffer_for_t3265_1928(ptr %5), !dbg !205
  store [2 x i64] %53, ptr %abi_pair_storage, align 8
  %54 = getelementptr inbounds nuw { [64 x i8], i32 }, ptr %5, i32 0, i32 1, !dbg !205
  %55 = load i32, ptr %54, align 4, !dbg !205
  %56 = zext i32 %55 to i64, !dbg !205
  %57 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !205
  %58 = call fastcc [2 x i64] @core.buffer.slice_1938([2 x i64] %57, i64 0, i64 %56), !dbg !205
  store [2 x i64] %58, ptr %abi_pair_storage7, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %abi_pair_storage7, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %2, i64 16, i1 false), !dbg !205
  %59 = load [2 x i64], ptr %1, align 8, !dbg !205
  call fastcc void @core.buffer.reverse_1922([2 x i64] %59), !dbg !205
  %60 = call fastcc [2 x i64] @core.impl_23as-span.as-span_for_t3238_1893(ptr %5), !dbg !206
  store [2 x i64] %60, ptr %abi_pair_storage8, align 8
  %61 = load [2 x i64], ptr %abi_pair_storage8, align 8, !dbg !206
  %62 = call fastcc [2 x i64] @core.span.as-byte-span_1175([2 x i64] %61), !dbg !206
  store [2 x i64] %62, ptr %abi_pair_storage9, align 8
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int)
  store i64 0, ptr %abi_struct_int, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int, ptr align 4 %struct_in_integer_storage, i64 4, i1 false)
  %63 = load i64, ptr %abi_struct_int, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int)
  %64 = load [2 x i64], ptr %abi_pair_storage9, align 8
  call fastcc void @core.io.impl_4writer.write-bytes_for_t162_586(i64 %63, [2 x i64] %64), !dbg !207
  br label %match_end5, !dbg !183
}

; Function Attrs: noreturn nounwind
declare !dbg !208 void @abort() #1

; Function Attrs: nounwind
define internal void @core.k1.emit-compiler-message_245(ptr %locn, i8 %level, [2 x i64] %msg) #0 !dbg !211 {
prelude:
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %msg, ptr %abi_pair_storage, align 8, !dbg !214
  ret void, !dbg !214
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.impl_22as-buffer.as-buffer_for_t3265_1928(ptr %self) #0 !dbg !215 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %0 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %1 = alloca { ptr, i64 }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  br i1 true, label %match_end, label %arm_cons, !dbg !218

arm_cons:                                         ; preds = %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @static_26, i64 16, i1 false), !dbg !218
  %2 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %0, i32 0, i32 1, !dbg !218
  store i64 985, ptr %2, align 8, !dbg !218
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %0, i64 24, i1 false)
  %3 = load [2 x i64], ptr @static_53, align 8
  call fastcc void @core.crash-bounds_336(ptr %abi_caller_copy, i64 64, i64 0, [2 x i64] %3), !dbg !218
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !218
  unreachable, !dbg !218

match_end:                                        ; preds = %entry
  %4 = getelementptr inbounds i8, ptr %self, i64 0, !dbg !218
  store ptr %4, ptr %1, align 8, !dbg !218
  %5 = getelementptr inbounds nuw { ptr, i64 }, ptr %1, i32 0, i32 1, !dbg !219
  store i64 64, ptr %5, align 8, !dbg !220
  %6 = load [2 x i64], ptr %1, align 8, !dbg !219
  ret [2 x i64] %6, !dbg !219
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.buffer.slice_1938([2 x i64] %self, i64 %start, i64 %end) #0 !dbg !221 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage12 = alloca [2 x i64], align 8
  %abi_pair_storage11 = alloca [2 x i64], align 8
  %0 = alloca ptr, align 8
  %1 = alloca { { ptr, i64 }, i64 }, align 8
  %2 = alloca { { { ptr, i64 } } }, align 8
  %3 = alloca i64, align 8
  %4 = alloca { { ptr, i64 } }, align 8
  %5 = alloca { ptr, i64 }, align 8
  %6 = alloca { ptr, i64 }, align 8
  %7 = alloca { ptr, i64 }, align 8
  %8 = alloca { { ptr, i64 } }, align 8
  %9 = alloca i64, align 8
  %10 = alloca { { { ptr, i64 } } }, align 8
  %11 = alloca { ptr, i64 }, align 8
  %12 = alloca { { ptr, i64 } }, align 8
  %13 = alloca { ptr, i64 }, align 8
  %14 = alloca { ptr, i64 }, align 8
  %15 = alloca { { ptr, i64 } }, align 8
  %16 = alloca { { { ptr, i64 } } }, align 8
  %17 = alloca { { { ptr, i64 }, i64 } }, align 8
  %18 = alloca { ptr, i64 }, align 8
  %19 = alloca { { ptr, i64 } }, align 8
  %20 = alloca { ptr, i64 }, align 8
  %21 = alloca { ptr, i64 }, align 8
  %22 = alloca { ptr, i64 }, align 8
  %23 = alloca { { { ptr, i64 } } }, align 8
  %24 = alloca { ptr, i64 }, align 8
  %25 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %26 = alloca { { { ptr, i64 } } }, align 8
  %27 = alloca { { ptr, i64 } }, align 8
  %28 = alloca { ptr, i64 }, align 8
  %29 = alloca { { { ptr, i64 } } }, align 8
  %30 = alloca { { ptr, i64 } }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !225
  br label %entry

entry:                                            ; preds = %prelude
  %31 = icmp slt i64 %start, 0, !dbg !226
  %32 = zext i1 %31 to i8, !dbg !226
  %33 = trunc i8 %32 to i1, !dbg !226
  br i1 %33, label %arm_cons1, label %arm_cons2, !dbg !226

arm_cons:                                         ; preds = %match_end
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %25, ptr align 8 @static_126, i64 16, i1 false), !dbg !227
  %34 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %25, i32 0, i32 1, !dbg !227
  store i64 199, ptr %34, align 8, !dbg !227
  call fastcc void @core.string-builder.new_1255(ptr noalias sret({ { { ptr, i64 }, i64 } }) align 8 dereferenceable(24) %17), !dbg !228
  %35 = load [2 x i64], ptr @static_136, align 8, !dbg !228
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %17, [2 x i64] %35), !dbg !228
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %start, ptr %17), !dbg !229
  %36 = load [2 x i64], ptr @static_137, align 8, !dbg !228
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %17, [2 x i64] %36), !dbg !228
  %37 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !230
  %38 = load i64, ptr %37, align 8, !dbg !230
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %38, ptr %17), !dbg !230
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %17, i64 24, i1 false), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %1, i64 16, i1 false), !dbg !228
  %39 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %1, i32 0, i32 1, !dbg !228
  %40 = load i64, ptr %39, align 8, !dbg !228
  %41 = load [2 x i64], ptr %6, align 8, !dbg !228
  %42 = call fastcc [2 x i64] @core.buffer.slice_1776([2 x i64] %41, i64 0, i64 %40), !dbg !228
  store [2 x i64] %42, ptr %abi_pair_storage11, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 8 %abi_pair_storage11, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %28, ptr align 8 %7, i64 16, i1 false), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %8, ptr align 8 %28, i64 16, i1 false), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %8, i64 16, i1 false), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %27, ptr align 8 %4, i64 16, i1 false), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %11, ptr align 8 %27, i64 16, i1 false), !dbg !228
  %43 = load ptr, ptr %11, align 8, !dbg !228
  %44 = getelementptr inbounds nuw { ptr, i64 }, ptr %11, i32 0, i32 1, !dbg !228
  %45 = load i64, ptr %44, align 8, !dbg !228
  store ptr %43, ptr %14, align 8, !dbg !228
  %46 = getelementptr inbounds nuw { ptr, i64 }, ptr %14, i32 0, i32 1, !dbg !228
  store i64 %45, ptr %46, align 8, !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %13, ptr align 8 %14, i64 16, i1 false), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %12, ptr align 8 %13, i64 16, i1 false), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %10, ptr align 8 %12, i64 16, i1 false), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %29, ptr align 8 %10, i64 16, i1 false), !dbg !228
  %47 = call fastcc ptr @core.mem.tmp_926(), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %15, ptr align 8 %29, i64 16, i1 false), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %18, ptr align 8 %15, i64 16, i1 false), !dbg !228
  %48 = getelementptr inbounds nuw { ptr, i64 }, ptr %18, i32 0, i32 1, !dbg !228
  %49 = load i64, ptr %48, align 8, !dbg !228
  %50 = call fastcc [2 x i64] @core.buffer.allocate-in_1861(ptr %47, i64 %49), !dbg !228
  store [2 x i64] %50, ptr %abi_pair_storage12, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %22, ptr align 8 %abi_pair_storage12, i64 16, i1 false)
  %51 = load [2 x i64], ptr %18, align 8, !dbg !228
  %52 = load [2 x i64], ptr %22, align 8, !dbg !228
  call fastcc void @core.buffer.copy_1862([2 x i64] %51, [2 x i64] %52), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %21, ptr align 8 %22, i64 16, i1 false), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %20, ptr align 8 %21, i64 16, i1 false), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %19, ptr align 8 %20, i64 16, i1 false), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %30, ptr align 8 %19, i64 16, i1 false), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %23, ptr align 8 %30, i64 16, i1 false), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %16, ptr align 8 %23, i64 16, i1 false), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %16, i64 16, i1 false), !dbg !228
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %26, ptr align 8 %2, i64 16, i1 false), !dbg !228
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %25, i64 24, i1 false)
  %53 = load [2 x i64], ptr %26, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %53), !dbg !227
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !227
  unreachable, !dbg !227

arm_cons1:                                        ; preds = %entry
  br label %match_end, !dbg !226

arm_cons2:                                        ; preds = %entry
  %54 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !231
  %55 = load i64, ptr %54, align 8, !dbg !231
  %56 = icmp sgt i64 %start, %55, !dbg !232
  %57 = zext i1 %56 to i8, !dbg !232
  br label %match_end, !dbg !226

match_end:                                        ; preds = %arm_cons1, %arm_cons2
  %58 = phi i8 [ 1, %arm_cons1 ], [ %57, %arm_cons2 ], !dbg !226
  %59 = trunc i8 %58 to i1, !dbg !233
  br i1 %59, label %arm_cons, label %arm_cond, !dbg !233

arm_cond:                                         ; preds = %match_end
  %60 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !234
  %61 = load i64, ptr %60, align 8, !dbg !234
  %62 = icmp sgt i64 %end, %61, !dbg !235
  %63 = zext i1 %62 to i8, !dbg !235
  %64 = trunc i8 %63 to i1, !dbg !236
  br i1 %64, label %arm_cons3, label %arm_cons4, !dbg !236

arm_cons3:                                        ; preds = %arm_cond
  %65 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !237
  %66 = load i64, ptr %65, align 8, !dbg !237
  store i64 %66, ptr %9, align 8, !dbg !236
  br label %match_end5, !dbg !236

arm_cons4:                                        ; preds = %arm_cond
  store i64 %end, ptr %9, align 8, !dbg !236
  br label %match_end5, !dbg !236

match_end5:                                       ; preds = %arm_cons3, %arm_cons4
  %67 = load i64, ptr %9, align 8, !dbg !238
  %68 = icmp sle i64 %67, %start, !dbg !238
  %69 = zext i1 %68 to i8, !dbg !238
  %70 = trunc i8 %69 to i1, !dbg !238
  br i1 %70, label %arm_cons8, label %arm_cons9, !dbg !238

entry6:                                           ; preds = %match_end10
  %71 = load ptr, ptr @_root__core__ptr__null, align 8, !dbg !239
  store ptr %71, ptr %24, align 8, !dbg !239
  %72 = getelementptr inbounds nuw { ptr, i64 }, ptr %24, i32 0, i32 1, !dbg !239
  store i64 0, ptr %72, align 8, !dbg !239
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %24, i64 16, i1 false), !dbg !239
  %73 = load [2 x i64], ptr %5, align 8, !dbg !240
  ret [2 x i64] %73, !dbg !240

match_end7:                                       ; preds = %match_end10
  %74 = load i64, ptr %9, align 8, !dbg !241
  %75 = sub i64 %74, %start, !dbg !241
  store i64 %75, ptr %3, align 8, !dbg !241
  %76 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !242
  call fastcc void @core.buffer.check-bounds_1925([2 x i64] %76, i64 %start), !dbg !242
  %77 = load ptr, ptr %abi_pair_storage, align 8, !dbg !242
  %78 = getelementptr inbounds i8, ptr %77, i64 %start, !dbg !242
  store ptr %78, ptr %0, align 8, !dbg !242
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %0, i64 8, i1 false), !dbg !243
  %79 = getelementptr inbounds nuw { ptr, i64 }, ptr %5, i32 0, i32 1, !dbg !244
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %79, ptr align 8 %3, i64 8, i1 false), !dbg !245
  %80 = load [2 x i64], ptr %5, align 8, !dbg !244
  ret [2 x i64] %80, !dbg !244

arm_cons8:                                        ; preds = %match_end5
  br label %match_end10, !dbg !238

arm_cons9:                                        ; preds = %match_end5
  %81 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !246
  %82 = load i64, ptr %81, align 8, !dbg !246
  %83 = icmp eq i64 %82, 0, !dbg !246
  %84 = zext i1 %83 to i8, !dbg !246
  br label %match_end10, !dbg !238

match_end10:                                      ; preds = %arm_cons8, %arm_cons9
  %85 = phi i8 [ 1, %arm_cons8 ], [ %84, %arm_cons9 ], !dbg !238
  %86 = trunc i8 %85 to i1, !dbg !247
  br i1 %86, label %entry6, label %match_end7, !dbg !247
}

; Function Attrs: nounwind
define internal fastcc void @core.buffer.reverse_1922([2 x i64] %self) #0 !dbg !248 {
prelude:
  %0 = alloca i64, align 8
  %1 = alloca i64, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !252
  br label %entry

entry:                                            ; preds = %prelude
  store i64 0, ptr %1, align 8, !dbg !253
  br label %while_loop_condition, !dbg !254

while_loop_condition:                             ; preds = %while_loop_body, %entry
  %2 = load i64, ptr %1, align 8, !dbg !255
  %3 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !256
  %4 = load i64, ptr %3, align 8, !dbg !256
  %5 = sdiv i64 %4, 2, !dbg !257
  %6 = icmp slt i64 %2, %5, !dbg !258
  %7 = zext i1 %6 to i8, !dbg !258
  %8 = trunc i8 %7 to i1, !dbg !254
  br i1 %8, label %while_loop_body, label %while_loop_end, !dbg !254

while_loop_body:                                  ; preds = %while_loop_condition
  %9 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !259
  %10 = load i64, ptr %9, align 8, !dbg !259
  %11 = load i64, ptr %1, align 8, !dbg !260
  %12 = sub i64 %10, %11, !dbg !259
  %13 = sub i64 %12, 1, !dbg !259
  store i64 %13, ptr %0, align 8, !dbg !259
  %14 = load i64, ptr %1, align 8, !dbg !261
  %15 = load i64, ptr %0, align 8, !dbg !262
  %16 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !263
  call fastcc void @core.buffer.swap_1923([2 x i64] %16, i64 %14, i64 %15), !dbg !263
  %17 = load i64, ptr %1, align 8, !dbg !264
  %18 = add i64 %17, 1, !dbg !264
  store i64 %18, ptr %1, align 8, !dbg !265
  br label %while_loop_condition, !dbg !254

while_loop_end:                                   ; preds = %while_loop_condition
  ret void, !dbg !254
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.impl_23as-span.as-span_for_t3238_1893(ptr %self) #0 !dbg !266 {
prelude:
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  %0 = alloca { { ptr, i64 } }, align 8
  %1 = alloca { ptr, i64 }, align 8
  %2 = alloca { { ptr, i64 } }, align 8
  %3 = alloca { ptr, i64 }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %4 = call fastcc [2 x i64] @core.impl_22as-buffer.as-buffer_for_t3265_1928(ptr %self), !dbg !269
  store [2 x i64] %4, ptr %abi_pair_storage, align 8
  %5 = getelementptr inbounds nuw { [64 x i8], i32 }, ptr %self, i32 0, i32 1, !dbg !269
  %6 = load i32, ptr %5, align 4, !dbg !269
  %7 = zext i32 %6 to i64, !dbg !269
  %8 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !269
  %9 = call fastcc [2 x i64] @core.buffer.slice_1938([2 x i64] %8, i64 0, i64 %7), !dbg !269
  store [2 x i64] %9, ptr %abi_pair_storage1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %abi_pair_storage1, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %1, i64 16, i1 false), !dbg !269
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %3, i64 16, i1 false), !dbg !270
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %2, i64 16, i1 false), !dbg !270
  %10 = load [2 x i64], ptr %0, align 8, !dbg !270
  ret [2 x i64] %10, !dbg !270
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.span.as-byte-span_1175([2 x i64] %self) #0 !dbg !271 {
prelude:
  %0 = alloca { ptr, i64 }, align 8
  %1 = alloca { ptr, i64 }, align 8
  %2 = alloca { ptr, i64 }, align 8
  %3 = alloca { { ptr, i64 } }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !275
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !276
  %4 = load ptr, ptr %2, align 8, !dbg !276
  %5 = getelementptr inbounds nuw { ptr, i64 }, ptr %2, i32 0, i32 1, !dbg !276
  %6 = load i64, ptr %5, align 8, !dbg !276
  store ptr %4, ptr %0, align 8, !dbg !276
  %7 = getelementptr inbounds nuw { ptr, i64 }, ptr %0, i32 0, i32 1, !dbg !276
  store i64 %6, ptr %7, align 8, !dbg !276
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %0, i64 16, i1 false), !dbg !276
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %1, i64 16, i1 false), !dbg !276
  %8 = load [2 x i64], ptr %3, align 8, !dbg !277
  ret [2 x i64] %8, !dbg !277
}

; Function Attrs: nounwind
define internal fastcc void @core.io.impl_4writer.write-bytes_for_t162_586(i64 %self, [2 x i64] %bytes) #0 !dbg !278 {
prelude:
  %0 = alloca { ptr, i64 }, align 8
  %1 = alloca { { ptr, i64 } }, align 8
  %2 = alloca { { ptr, i64 } }, align 8
  %3 = alloca { ptr, i64 }, align 8
  %4 = alloca { ptr, i64 }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  %struct_in_integer_storage = alloca { i32 }, align 4
  %5 = trunc i64 %self to i32, !dbg !281
  store i32 %5, ptr %struct_in_integer_storage, align 4, !dbg !281
  store [2 x i64] %bytes, ptr %abi_pair_storage, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %6 = load i32, ptr %struct_in_integer_storage, align 4, !dbg !282
  %7 = icmp eq i32 %6, 1, !dbg !282
  %8 = zext i1 %7 to i8, !dbg !282
  %9 = trunc i8 %8 to i1, !dbg !283
  br i1 %9, label %arm_cons, label %arm_cons1, !dbg !283

arm_cons:                                         ; preds = %entry
  %10 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !284
  call fastcc void @core.io.stdout-buffer-bytes_584([2 x i64] %10), !dbg !284
  br label %match_end, !dbg !283

arm_cons1:                                        ; preds = %entry
  %11 = load i32, ptr %struct_in_integer_storage, align 4, !dbg !285
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !286
  %12 = load ptr, ptr %0, align 8, !dbg !286
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !287
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %2, i64 16, i1 false), !dbg !287
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %1, i64 16, i1 false), !dbg !287
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %4, i64 16, i1 false), !dbg !287
  %13 = getelementptr inbounds nuw { ptr, i64 }, ptr %3, i32 0, i32 1, !dbg !287
  %14 = load i64, ptr %13, align 8, !dbg !287
  call fastcc void @core.io.write-all_582(i32 %11, ptr %12, i64 %14), !dbg !288
  br label %match_end, !dbg !283

match_end:                                        ; preds = %arm_cons, %arm_cons1
  ret void, !dbg !289
}

; Function Attrs: nounwind
define internal fastcc i8 @core.u8.to-ascii-digit_644(i8 %v) #0 !dbg !290 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage6 = alloca [2 x i64], align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  %0 = alloca { { { ptr, i64 }, i64 } }, align 8
  %1 = alloca { { ptr, i64 }, i64 }, align 8
  %2 = alloca { { { ptr, i64 } } }, align 8
  %3 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %4 = alloca { { ptr, i64 } }, align 8
  %5 = alloca { { { ptr, i64 } } }, align 8
  %6 = alloca { ptr, i64 }, align 8
  %7 = alloca { ptr, i64 }, align 8
  %8 = alloca { { ptr, i64 } }, align 8
  %9 = alloca { { ptr, i64 } }, align 8
  %10 = alloca { { { ptr, i64 } } }, align 8
  %11 = alloca { ptr, i64 }, align 8
  %12 = alloca { { ptr, i64 } }, align 8
  %13 = alloca { ptr, i64 }, align 8
  %14 = alloca { ptr, i64 }, align 8
  %15 = alloca { { ptr, i64 } }, align 8
  %16 = alloca { { { ptr, i64 } } }, align 8
  %17 = alloca { ptr, i64 }, align 8
  %18 = alloca { ptr, i64 }, align 8
  %19 = alloca { { ptr, i64 } }, align 8
  %20 = alloca { ptr, i64 }, align 8
  %21 = alloca { ptr, i64 }, align 8
  %22 = alloca { ptr, i64 }, align 8
  %23 = alloca { { { ptr, i64 } } }, align 8
  %24 = alloca { { { ptr, i64 } } }, align 8
  %25 = alloca { { ptr, i64 } }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %26 = icmp ult i8 %v, 10, !dbg !294
  %27 = zext i1 %26 to i8, !dbg !294
  %28 = trunc i8 %27 to i1, !dbg !295
  br i1 %28, label %arm_cons, label %arm_cond, !dbg !295

arm_cons:                                         ; preds = %entry
  %29 = add i8 %v, 48, !dbg !296
  br label %match_end, !dbg !295

match_end:                                        ; preds = %arm_cons, %match_end2
  %30 = phi i8 [ %29, %arm_cons ], [ %35, %match_end2 ], !dbg !295
  ret i8 %30, !dbg !295

arm_cond:                                         ; preds = %entry
  %31 = icmp ult i8 %v, 36, !dbg !297
  %32 = zext i1 %31 to i8, !dbg !297
  %33 = trunc i8 %32 to i1, !dbg !298
  br i1 %33, label %arm_cons1, label %arm_cond3, !dbg !298

arm_cons1:                                        ; preds = %arm_cond
  %34 = add i8 %v, 87, !dbg !299
  br label %match_end2, !dbg !298

match_end2:                                       ; preds = %arm_cons1, %arm_cons4
  %35 = phi i8 [ %34, %arm_cons1 ], [ %39, %arm_cons4 ], !dbg !298
  br label %match_end, !dbg !295

arm_cond3:                                        ; preds = %arm_cond
  %36 = icmp ult i8 %v, 62, !dbg !300
  %37 = zext i1 %36 to i8, !dbg !300
  %38 = trunc i8 %37 to i1, !dbg !301
  br i1 %38, label %arm_cons4, label %arm_cons5, !dbg !301

arm_cons4:                                        ; preds = %arm_cond3
  %39 = add i8 %v, 29, !dbg !302
  br label %match_end2, !dbg !298

arm_cons5:                                        ; preds = %arm_cond3
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 @static_190, i64 16, i1 false), !dbg !303
  %40 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %3, i32 0, i32 1, !dbg !303
  store i64 446, ptr %40, align 8, !dbg !303
  call fastcc void @core.string-builder.new_1255(ptr noalias sret({ { { ptr, i64 }, i64 } }) align 8 dereferenceable(24) %0), !dbg !304
  %41 = load [2 x i64], ptr @static_192, align 8, !dbg !304
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %0, [2 x i64] %41), !dbg !304
  %42 = zext i8 %v to i64, !dbg !305
  call fastcc void @core.format-uint_1879(ptr %0, i64 %42, i64 10), !dbg !305
  %43 = load [2 x i64], ptr @static_193, align 8, !dbg !304
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %0, [2 x i64] %43), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %0, i64 24, i1 false), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %1, i64 16, i1 false), !dbg !304
  %44 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %1, i32 0, i32 1, !dbg !304
  %45 = load i64, ptr %44, align 8, !dbg !304
  %46 = load [2 x i64], ptr %6, align 8, !dbg !304
  %47 = call fastcc [2 x i64] @core.buffer.slice_1776([2 x i64] %46, i64 0, i64 %45), !dbg !304
  store [2 x i64] %47, ptr %abi_pair_storage, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 8 %abi_pair_storage, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %17, ptr align 8 %7, i64 16, i1 false), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %8, ptr align 8 %17, i64 16, i1 false), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %8, i64 16, i1 false), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %9, ptr align 8 %4, i64 16, i1 false), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %11, ptr align 8 %9, i64 16, i1 false), !dbg !304
  %48 = load ptr, ptr %11, align 8, !dbg !304
  %49 = getelementptr inbounds nuw { ptr, i64 }, ptr %11, i32 0, i32 1, !dbg !304
  %50 = load i64, ptr %49, align 8, !dbg !304
  store ptr %48, ptr %14, align 8, !dbg !304
  %51 = getelementptr inbounds nuw { ptr, i64 }, ptr %14, i32 0, i32 1, !dbg !304
  store i64 %50, ptr %51, align 8, !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %13, ptr align 8 %14, i64 16, i1 false), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %12, ptr align 8 %13, i64 16, i1 false), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %10, ptr align 8 %12, i64 16, i1 false), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %24, ptr align 8 %10, i64 16, i1 false), !dbg !304
  %52 = call fastcc ptr @core.mem.tmp_926(), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %15, ptr align 8 %24, i64 16, i1 false), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %18, ptr align 8 %15, i64 16, i1 false), !dbg !304
  %53 = getelementptr inbounds nuw { ptr, i64 }, ptr %18, i32 0, i32 1, !dbg !304
  %54 = load i64, ptr %53, align 8, !dbg !304
  %55 = call fastcc [2 x i64] @core.buffer.allocate-in_1861(ptr %52, i64 %54), !dbg !304
  store [2 x i64] %55, ptr %abi_pair_storage6, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %22, ptr align 8 %abi_pair_storage6, i64 16, i1 false)
  %56 = load [2 x i64], ptr %18, align 8, !dbg !304
  %57 = load [2 x i64], ptr %22, align 8, !dbg !304
  call fastcc void @core.buffer.copy_1862([2 x i64] %56, [2 x i64] %57), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %21, ptr align 8 %22, i64 16, i1 false), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %20, ptr align 8 %21, i64 16, i1 false), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %19, ptr align 8 %20, i64 16, i1 false), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %25, ptr align 8 %19, i64 16, i1 false), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %23, ptr align 8 %25, i64 16, i1 false), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %16, ptr align 8 %23, i64 16, i1 false), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %16, i64 16, i1 false), !dbg !304
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %2, i64 16, i1 false), !dbg !304
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %3, i64 24, i1 false)
  %58 = load [2 x i64], ptr %5, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %58), !dbg !303
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !303
  unreachable, !dbg !303
}

; Function Attrs: nounwind
define internal fastcc void @core.fixlist.push_1882(ptr %self, i8 %value) #0 !dbg !306 {
prelude:
  %abi_caller_copy5 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage4 = alloca [2 x i64], align 8
  %abi_pair_storage3 = alloca [2 x i64], align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  %abi_struct_int = alloca i64, align 8
  %0 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %1 = alloca { { { ptr, i64 } } }, align 8
  %2 = alloca { { { ptr, i64 }, i64 } }, align 8
  %3 = alloca { { ptr, i64 }, i64 }, align 8
  %4 = alloca { { { ptr, i64 } } }, align 8
  %5 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %6 = alloca { { ptr, i64 } }, align 8
  %7 = alloca { { { ptr, i64 } } }, align 8
  %8 = alloca { ptr, i64 }, align 8
  %9 = alloca { ptr, i64 }, align 8
  %10 = alloca { { ptr, i64 } }, align 8
  %11 = alloca i64, align 8
  %12 = alloca { { { ptr, i64 } } }, align 8
  %13 = alloca { ptr, i64 }, align 8
  %14 = alloca { { ptr, i64 } }, align 8
  %15 = alloca { ptr, i64 }, align 8
  %16 = alloca { ptr, i64 }, align 8
  %17 = alloca { { ptr, i64 } }, align 8
  %18 = alloca { { { ptr, i64 } } }, align 8
  %19 = alloca { { { ptr, i64 } } }, align 8
  %20 = alloca { ptr, i64 }, align 8
  %21 = alloca { { ptr, i64 } }, align 8
  %22 = alloca { ptr, i64 }, align 8
  %23 = alloca { ptr, i64 }, align 8
  %24 = alloca { ptr, i64 }, align 8
  %25 = alloca { { { ptr, i64 } } }, align 8
  %26 = alloca { { ptr, i64 } }, align 8
  %27 = alloca { ptr, i64 }, align 8
  %28 = alloca { { { ptr, i64 } } }, align 8
  %29 = alloca { { ptr, i64 } }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %30 = getelementptr inbounds nuw { [64 x i8], i32 }, ptr %self, i32 0, i32 1, !dbg !310
  %31 = load i32, ptr %30, align 4, !dbg !310
  %32 = zext i32 %31 to i64, !dbg !310
  store i64 %32, ptr %11, align 8, !dbg !310
  %33 = load i64, ptr %11, align 8, !dbg !311
  %34 = icmp slt i64 %33, 64, !dbg !311
  %35 = zext i1 %34 to i8, !dbg !311
  %36 = trunc i8 %35 to i1, !dbg !312
  br i1 %36, label %arm_cons, label %entry2, !dbg !312

arm_cons:                                         ; preds = %entry
  %37 = load i64, ptr %11, align 8, !dbg !313
  %38 = icmp slt i64 %37, 64, !dbg !314
  %39 = zext i1 %38 to i8, !dbg !314
  %40 = trunc i8 %39 to i1, !dbg !314
  br i1 %40, label %match_end, label %arm_cons1, !dbg !314

arm_cons1:                                        ; preds = %arm_cons
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @static_26, i64 16, i1 false), !dbg !314
  %41 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %0, i32 0, i32 1, !dbg !314
  store i64 1007, ptr %41, align 8, !dbg !314
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy5)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy5, ptr align 8 %0, i64 24, i1 false)
  %42 = load [2 x i64], ptr @static_53, align 8
  call fastcc void @core.crash-bounds_336(ptr %abi_caller_copy5, i64 64, i64 %37, [2 x i64] %42), !dbg !314
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy5), !dbg !314
  unreachable, !dbg !314

match_end:                                        ; preds = %arm_cons
  %43 = getelementptr inbounds i8, ptr %self, i64 %37, !dbg !314
  store i8 %value, ptr %43, align 1, !dbg !314
  %44 = getelementptr inbounds nuw { [64 x i8], i32 }, ptr %self, i32 0, i32 1, !dbg !315
  %45 = load i64, ptr %11, align 8, !dbg !316
  %46 = trunc i64 %45 to i32, !dbg !316
  %47 = add i32 %46, 1, !dbg !316
  store i32 %47, ptr %44, align 4, !dbg !315
  ret void, !dbg !312

entry2:                                           ; preds = %entry
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int)
  store i64 0, ptr %abi_struct_int, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int, ptr align 8 @static_221, i64 8, i1 false)
  %48 = load i64, ptr %abi_struct_int, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int)
  %49 = call [2 x i64] @core.types.type-id.name_2(i64 %48), !dbg !317
  store [2 x i64] %49, ptr %abi_pair_storage, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %abi_pair_storage, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 8 %1, i64 16, i1 false), !dbg !317
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 @static_195, i64 16, i1 false), !dbg !318
  %50 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %5, i32 0, i32 1, !dbg !318
  store i64 31, ptr %50, align 8, !dbg !318
  call fastcc void @core.string-builder.new_1255(ptr noalias sret({ { { ptr, i64 }, i64 } }) align 8 dereferenceable(24) %2), !dbg !319
  %51 = load [2 x i64], ptr %7, align 8, !dbg !320
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %2, [2 x i64] %51), !dbg !320
  %52 = load [2 x i64], ptr @static_197, align 8, !dbg !319
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %2, [2 x i64] %52), !dbg !319
  %53 = load i64, ptr %11, align 8, !dbg !321
  %54 = add i64 %53, 1, !dbg !321
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %54, ptr %2), !dbg !321
  %55 = load [2 x i64], ptr @static_130, align 8, !dbg !319
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %2, [2 x i64] %55), !dbg !319
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 64, ptr %2), !dbg !322
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %2, i64 24, i1 false), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %8, ptr align 8 %3, i64 16, i1 false), !dbg !319
  %56 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %3, i32 0, i32 1, !dbg !319
  %57 = load i64, ptr %56, align 8, !dbg !319
  %58 = load [2 x i64], ptr %8, align 8, !dbg !319
  %59 = call fastcc [2 x i64] @core.buffer.slice_1776([2 x i64] %58, i64 0, i64 %57), !dbg !319
  store [2 x i64] %59, ptr %abi_pair_storage3, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %9, ptr align 8 %abi_pair_storage3, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %27, ptr align 8 %9, i64 16, i1 false), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %10, ptr align 8 %27, i64 16, i1 false), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %10, i64 16, i1 false), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %26, ptr align 8 %6, i64 16, i1 false), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %13, ptr align 8 %26, i64 16, i1 false), !dbg !319
  %60 = load ptr, ptr %13, align 8, !dbg !319
  %61 = getelementptr inbounds nuw { ptr, i64 }, ptr %13, i32 0, i32 1, !dbg !319
  %62 = load i64, ptr %61, align 8, !dbg !319
  store ptr %60, ptr %16, align 8, !dbg !319
  %63 = getelementptr inbounds nuw { ptr, i64 }, ptr %16, i32 0, i32 1, !dbg !319
  store i64 %62, ptr %63, align 8, !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %15, ptr align 8 %16, i64 16, i1 false), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %14, ptr align 8 %15, i64 16, i1 false), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %12, ptr align 8 %14, i64 16, i1 false), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %28, ptr align 8 %12, i64 16, i1 false), !dbg !319
  %64 = call fastcc ptr @core.mem.tmp_926(), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %17, ptr align 8 %28, i64 16, i1 false), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %20, ptr align 8 %17, i64 16, i1 false), !dbg !319
  %65 = getelementptr inbounds nuw { ptr, i64 }, ptr %20, i32 0, i32 1, !dbg !319
  %66 = load i64, ptr %65, align 8, !dbg !319
  %67 = call fastcc [2 x i64] @core.buffer.allocate-in_1861(ptr %64, i64 %66), !dbg !319
  store [2 x i64] %67, ptr %abi_pair_storage4, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %24, ptr align 8 %abi_pair_storage4, i64 16, i1 false)
  %68 = load [2 x i64], ptr %20, align 8, !dbg !319
  %69 = load [2 x i64], ptr %24, align 8, !dbg !319
  call fastcc void @core.buffer.copy_1862([2 x i64] %68, [2 x i64] %69), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %23, ptr align 8 %24, i64 16, i1 false), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %22, ptr align 8 %23, i64 16, i1 false), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %21, ptr align 8 %22, i64 16, i1 false), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %29, ptr align 8 %21, i64 16, i1 false), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %25, ptr align 8 %29, i64 16, i1 false), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %18, ptr align 8 %25, i64 16, i1 false), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %18, i64 16, i1 false), !dbg !319
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %19, ptr align 8 %4, i64 16, i1 false), !dbg !319
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %5, i64 24, i1 false)
  %70 = load [2 x i64], ptr %19, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %70), !dbg !318
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !318
  unreachable, !dbg !318
}

; Function Attrs: nounwind
define internal fastcc void @core.io.impl_4writer.write-byte_for_t162_585(i64 %self, i8 %value) #0 !dbg !323 {
prelude:
  %0 = alloca i8, align 1
  %1 = alloca i8, align 1
  %struct_in_integer_storage = alloca { i32 }, align 4
  %2 = trunc i64 %self to i32, !dbg !326
  store i32 %2, ptr %struct_in_integer_storage, align 4, !dbg !326
  br label %entry

entry:                                            ; preds = %prelude
  %3 = load i32, ptr %struct_in_integer_storage, align 4, !dbg !327
  %4 = icmp eq i32 %3, 1, !dbg !327
  %5 = zext i1 %4 to i8, !dbg !327
  %6 = trunc i8 %5 to i1, !dbg !328
  br i1 %6, label %entry1, label %arm_cons, !dbg !328

arm_cons:                                         ; preds = %entry
  store i8 %value, ptr %0, align 1, !dbg !329
  %7 = load i32, ptr %struct_in_integer_storage, align 4, !dbg !330
  call fastcc void @core.io.write-all_582(i32 %7, ptr %0, i64 1), !dbg !331
  br label %match_end, !dbg !328

match_end:                                        ; preds = %match_end5, %arm_cons
  ret void, !dbg !332

entry1:                                           ; preds = %entry
  %8 = load i32, ptr getelementptr inbounds nuw ({ [8192 x i8], i32 }, ptr @_root__core__io__stdout-buf, i32 0, i32 1), align 4, !dbg !333
  %9 = zext i32 %8 to i64, !dbg !333
  %10 = icmp eq i64 %9, 8192, !dbg !333
  %11 = zext i1 %10 to i8, !dbg !333
  %12 = trunc i8 %11 to i1, !dbg !334
  br i1 %12, label %arm_cons2, label %match_end3, !dbg !334

arm_cons2:                                        ; preds = %entry1
  call fastcc void @core.io.flush-stdout_583(), !dbg !335
  br label %match_end3, !dbg !334

match_end3:                                       ; preds = %arm_cons2, %entry1
  call fastcc void @core.fixlist.push_2014(ptr @_root__core__io__stdout-buf, i8 %value), !dbg !336
  %13 = icmp eq i8 %value, 10, !dbg !337
  %14 = zext i1 %13 to i8, !dbg !337
  %15 = trunc i8 %14 to i1, !dbg !338
  br i1 %15, label %matching_cond_continue, label %arm_cond, !dbg !338

arm_cons4:                                        ; preds = %matching_cond_continue
  call fastcc void @core.io.flush-stdout_583(), !dbg !339
  br label %match_end5, !dbg !338

arm_cond:                                         ; preds = %matching_cond_continue, %match_end3
  br label %match_end5, !dbg !338

match_end5:                                       ; preds = %arm_cons4, %arm_cond
  br label %match_end, !dbg !328

matching_cond_continue:                           ; preds = %match_end3
  %16 = call fastcc i8 @core.io.stdout-buffering-mode_581(), !dbg !340
  store i8 %16, ptr %1, align 1, !dbg !340
  %17 = load i8, ptr %1, align 1, !dbg !340
  %18 = icmp eq i8 %17, 1, !dbg !341
  %19 = zext i1 %18 to i8, !dbg !341
  %20 = trunc i8 %19 to i1, !dbg !338
  br i1 %20, label %arm_cons4, label %arm_cond, !dbg !338
}

; Function Attrs: nounwind
define internal fastcc void @core.fixlist.push_2014(ptr %self, i8 %value) #0 !dbg !342 {
prelude:
  %abi_caller_copy4 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage3 = alloca [2 x i64], align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  %abi_struct_int = alloca i64, align 8
  %0 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %1 = alloca { { { ptr, i64 } } }, align 8
  %2 = alloca { { { ptr, i64 }, i64 } }, align 8
  %3 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %4 = alloca { { { ptr, i64 } } }, align 8
  %5 = alloca i64, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %6 = getelementptr inbounds nuw { [8192 x i8], i32 }, ptr %self, i32 0, i32 1, !dbg !346
  %7 = load i32, ptr %6, align 4, !dbg !346
  %8 = zext i32 %7 to i64, !dbg !346
  store i64 %8, ptr %5, align 8, !dbg !346
  %9 = load i64, ptr %5, align 8, !dbg !347
  %10 = icmp slt i64 %9, 8192, !dbg !347
  %11 = zext i1 %10 to i8, !dbg !347
  %12 = trunc i8 %11 to i1, !dbg !348
  br i1 %12, label %arm_cons, label %entry2, !dbg !348

arm_cons:                                         ; preds = %entry
  %13 = load i64, ptr %5, align 8, !dbg !349
  %14 = icmp slt i64 %13, 8192, !dbg !350
  %15 = zext i1 %14 to i8, !dbg !350
  %16 = trunc i8 %15 to i1, !dbg !350
  br i1 %16, label %match_end, label %arm_cons1, !dbg !350

arm_cons1:                                        ; preds = %arm_cons
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @static_26, i64 16, i1 false), !dbg !350
  %17 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %0, i32 0, i32 1, !dbg !350
  store i64 1007, ptr %17, align 8, !dbg !350
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy4)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy4, ptr align 8 %0, i64 24, i1 false)
  %18 = load [2 x i64], ptr @static_53, align 8
  call fastcc void @core.crash-bounds_336(ptr %abi_caller_copy4, i64 8192, i64 %13, [2 x i64] %18), !dbg !350
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy4), !dbg !350
  unreachable, !dbg !350

match_end:                                        ; preds = %arm_cons
  %19 = getelementptr inbounds i8, ptr %self, i64 %13, !dbg !350
  store i8 %value, ptr %19, align 1, !dbg !350
  %20 = getelementptr inbounds nuw { [8192 x i8], i32 }, ptr %self, i32 0, i32 1, !dbg !351
  %21 = load i64, ptr %5, align 8, !dbg !352
  %22 = trunc i64 %21 to i32, !dbg !352
  %23 = add i32 %22, 1, !dbg !352
  store i32 %23, ptr %20, align 4, !dbg !351
  ret void, !dbg !348

entry2:                                           ; preds = %entry
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int)
  store i64 0, ptr %abi_struct_int, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int, ptr align 8 @static_244, i64 8, i1 false)
  %24 = load i64, ptr %abi_struct_int, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int)
  %25 = call [2 x i64] @core.types.type-id.name_2(i64 %24), !dbg !353
  store [2 x i64] %25, ptr %abi_pair_storage, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %abi_pair_storage, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %1, i64 16, i1 false), !dbg !353
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 @static_195, i64 16, i1 false), !dbg !354
  %26 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %3, i32 0, i32 1, !dbg !354
  store i64 31, ptr %26, align 8, !dbg !354
  call fastcc void @core.string-builder.new_1255(ptr noalias sret({ { { ptr, i64 }, i64 } }) align 8 dereferenceable(24) %2), !dbg !355
  %27 = load [2 x i64], ptr %4, align 8, !dbg !356
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %2, [2 x i64] %27), !dbg !356
  %28 = load [2 x i64], ptr @static_197, align 8, !dbg !355
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %2, [2 x i64] %28), !dbg !355
  %29 = load i64, ptr %5, align 8, !dbg !357
  %30 = add i64 %29, 1, !dbg !357
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %30, ptr %2), !dbg !357
  %31 = load [2 x i64], ptr @static_130, align 8, !dbg !355
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %2, [2 x i64] %31), !dbg !355
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 8192, ptr %2), !dbg !358
  %32 = call fastcc [2 x i64] @core.string-builder.build-tmp_1259(ptr %2), !dbg !355
  store [2 x i64] %32, ptr %abi_pair_storage3, align 8
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %3, i64 24, i1 false)
  %33 = load [2 x i64], ptr %abi_pair_storage3, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %33), !dbg !354
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !354
  unreachable, !dbg !354
}

; Function Attrs: nounwind
define internal fastcc i8 @core.io.stdout-buffering-mode_581() #0 !dbg !359 {
prelude:
  %abi_struct_int = alloca i64, align 8
  %0 = alloca i8, align 1
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %0, ptr align 1 @_root__core__io__stdout-mode, i64 1, i1 false), !dbg !362
  %1 = load i8, ptr %0, align 1, !dbg !362
  %2 = icmp eq i8 %1, 0, !dbg !363
  %3 = zext i1 %2 to i8, !dbg !363
  %4 = trunc i8 %3 to i1, !dbg !364
  br i1 %4, label %arm_cond, label %match_end, !dbg !364

match_end:                                        ; preds = %match_end2, %entry
  %5 = load i8, ptr @_root__core__io__stdout-mode, align 1, !dbg !365
  ret i8 %5, !dbg !365

arm_cond:                                         ; preds = %entry
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int)
  store i64 0, ptr %abi_struct_int, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int, ptr align 4 @_root__core__platform__io__STDOUT, i64 4, i1 false)
  %6 = load i64, ptr %abi_struct_int, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int)
  %7 = call fastcc i8 @core.platform.io.is-tty_965(i64 %6), !dbg !366
  %8 = trunc i8 %7 to i1, !dbg !367
  br i1 %8, label %arm_cons, label %arm_cons1, !dbg !367

arm_cons:                                         ; preds = %arm_cond
  br label %match_end2, !dbg !367

arm_cons1:                                        ; preds = %arm_cond
  br label %match_end2, !dbg !367

match_end2:                                       ; preds = %arm_cons, %arm_cons1
  %9 = phi i8 [ 1, %arm_cons ], [ 2, %arm_cons1 ], !dbg !367
  store i8 %9, ptr @_root__core__io__stdout-mode, align 1, !dbg !368
  br label %match_end, !dbg !364
}

; Function Attrs: nounwind
define internal fastcc i8 @core.platform.io.is-tty_965(i64 %file) #0 !dbg !369 {
prelude:
  %struct_in_integer_storage = alloca { i32 }, align 4
  %0 = trunc i64 %file to i32, !dbg !376
  store i32 %0, ptr %struct_in_integer_storage, align 4, !dbg !376
  br label %entry

entry:                                            ; preds = %prelude
  %1 = load i8, ptr @_root__core__k1__is-static, align 1, !dbg !377
  %2 = trunc i8 %1 to i1, !dbg !378
  br i1 %2, label %arm_cons, label %arm_cons1, !dbg !378

arm_cons:                                         ; preds = %entry
  %3 = load i32, ptr %struct_in_integer_storage, align 4, !dbg !379
  %4 = call i32 @isatty(i32 %3), !dbg !380
  %5 = icmp eq i32 %4, 1, !dbg !380
  %6 = zext i1 %5 to i8, !dbg !380
  br label %match_end, !dbg !378

arm_cons1:                                        ; preds = %entry
  %7 = load i32, ptr %struct_in_integer_storage, align 4, !dbg !381
  %8 = call i32 @isatty(i32 %7), !dbg !382
  %9 = icmp eq i32 %8, 1, !dbg !382
  %10 = zext i1 %9 to i8, !dbg !382
  br label %match_end, !dbg !378

match_end:                                        ; preds = %arm_cons, %arm_cons1
  %11 = phi i8 [ %6, %arm_cons ], [ %10, %arm_cons1 ], !dbg !378
  ret i8 %11, !dbg !378
}

; Function Attrs: nounwind
declare !dbg !383 i32 @isatty(i32) #0

; Function Attrs: nounwind
define internal [2 x i64] @core.types.type-id.name_2(i64 %self) #0 !dbg !386 {
prelude:
  %0 = alloca { { { ptr, i64 } } }, align 8
  %struct_in_integer_storage = alloca { i64 }, align 8
  store i64 %self, ptr %struct_in_integer_storage, align 8, !dbg !392
  %1 = load i64, ptr %struct_in_integer_storage, align 8, !dbg !392
  switch i64 %1, label %miss [
    i64 2, label %arm_type_2
    i64 4, label %arm_type_4
    i64 5, label %arm_type_5
    i64 9, label %arm_type_9
    i64 10, label %arm_type_10
    i64 13, label %arm_type_13
    i64 33, label %arm_type_33
    i64 104, label %arm_type_104
    i64 105, label %arm_type_105
    i64 106, label %arm_type_106
    i64 150, label %arm_type_150
    i64 152, label %arm_type_152
    i64 153, label %arm_type_153
    i64 154, label %arm_type_154
    i64 155, label %arm_type_155
    i64 1157, label %arm_type_1157
    i64 1158, label %arm_type_1158
    i64 1178, label %arm_type_1178
    i64 1851, label %arm_type_1851
    i64 1852, label %arm_type_1852
    i64 3235, label %arm_type_3235
    i64 3236, label %arm_type_3236
  ]

miss:                                             ; preds = %prelude
  unreachable

finish:                                           ; preds = %arm_type_3236, %arm_type_3235, %arm_type_1852, %arm_type_1851, %arm_type_1178, %arm_type_1158, %arm_type_1157, %arm_type_155, %arm_type_154, %arm_type_153, %arm_type_152, %arm_type_150, %arm_type_106, %arm_type_105, %arm_type_104, %arm_type_33, %arm_type_13, %arm_type_10, %arm_type_9, %arm_type_5, %arm_type_4, %arm_type_2
  %2 = load [2 x i64], ptr %0, align 8
  ret [2 x i64] %2

arm_type_2:                                       ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_2, i64 16, i1 false)
  br label %finish

arm_type_4:                                       ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_4, i64 16, i1 false)
  br label %finish

arm_type_5:                                       ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_5, i64 16, i1 false)
  br label %finish

arm_type_9:                                       ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_9, i64 16, i1 false)
  br label %finish

arm_type_10:                                      ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_10, i64 16, i1 false)
  br label %finish

arm_type_13:                                      ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_13, i64 16, i1 false)
  br label %finish

arm_type_33:                                      ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_33, i64 16, i1 false)
  br label %finish

arm_type_104:                                     ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_104, i64 16, i1 false)
  br label %finish

arm_type_105:                                     ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_105, i64 16, i1 false)
  br label %finish

arm_type_106:                                     ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_106, i64 16, i1 false)
  br label %finish

arm_type_150:                                     ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_150, i64 16, i1 false)
  br label %finish

arm_type_152:                                     ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_152, i64 16, i1 false)
  br label %finish

arm_type_153:                                     ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_153, i64 16, i1 false)
  br label %finish

arm_type_154:                                     ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_154, i64 16, i1 false)
  br label %finish

arm_type_155:                                     ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_155, i64 16, i1 false)
  br label %finish

arm_type_1157:                                    ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_1157, i64 16, i1 false)
  br label %finish

arm_type_1158:                                    ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_1158, i64 16, i1 false)
  br label %finish

arm_type_1178:                                    ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_1178, i64 16, i1 false)
  br label %finish

arm_type_1851:                                    ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_1851, i64 16, i1 false)
  br label %finish

arm_type_1852:                                    ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_1852, i64 16, i1 false)
  br label %finish

arm_type_3235:                                    ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_3235, i64 16, i1 false)
  br label %finish

arm_type_3236:                                    ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @typename_3236, i64 16, i1 false)
  br label %finish
}

; Function Attrs: nounwind
define internal fastcc void @core.string-builder.new_1255(ptr noalias sret({ { { ptr, i64 }, i64 } }) align 8 dereferenceable(24) %sret) #0 !dbg !393 {
prelude:
  %abi_pair_storage = alloca [2 x i64], align 8
  %0 = alloca { ptr, i64 }, align 8
  %1 = alloca { { ptr, i64 }, i64 }, align 8
  %2 = alloca { ptr, i64 }, align 8
  %3 = alloca { { ptr, i64 }, i64 }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %4 = call fastcc ptr @core.mem.current-arena_924(), !dbg !404
  %5 = call fastcc [2 x i64] @core.buffer.allocate-in_1786(ptr %4, i64 0), !dbg !404
  store [2 x i64] %5, ptr %abi_pair_storage, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %abi_pair_storage, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %0, i64 16, i1 false), !dbg !404
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %2, i64 16, i1 false), !dbg !404
  %6 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %1, i32 0, i32 1, !dbg !404
  store i64 0, ptr %6, align 8, !dbg !404
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %1, i64 24, i1 false), !dbg !404
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret, ptr align 8 %3, i64 24, i1 false), !dbg !404
  ret void, !dbg !405
}

; Function Attrs: nounwind
define internal fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %self, [2 x i64] %value) #0 !dbg !406 {
prelude:
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %0 = alloca { { ptr, i64 } }, align 8
  %1 = alloca { { ptr, i64 } }, align 8
  %2 = alloca { { ptr, i64 } }, align 8
  %3 = alloca { { ptr, i64 } }, align 8
  %4 = alloca { ptr, i64 }, align 8
  %5 = alloca { ptr, i64 }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %value, ptr %abi_pair_storage, align 8, !dbg !410
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !411
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %2, i64 16, i1 false), !dbg !411
  %6 = load [2 x i64], ptr %3, align 8, !dbg !411
  %7 = call fastcc [2 x i64] @core.span.as-byte-span_1175([2 x i64] %6), !dbg !411
  store [2 x i64] %7, ptr %abi_pair_storage1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %abi_pair_storage1, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %1, i64 16, i1 false), !dbg !411
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %0, i64 16, i1 false), !dbg !412
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %5, i64 16, i1 false), !dbg !412
  %8 = call fastcc ptr @core.mem.current-arena_924(), !dbg !412
  %9 = load [2 x i64], ptr %4, align 8, !dbg !412
  call fastcc void @core.list.append-buffer-in_1873(ptr %self, ptr %8, [2 x i64] %9), !dbg !412
  ret void, !dbg !412
}

; Function Attrs: nounwind
define internal fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %self, ptr %w) #0 !dbg !413 {
prelude:
  %0 = alloca i64, align 8
  %1 = alloca i8, align 1
  br label %entry

entry:                                            ; preds = %prelude
  %2 = icmp slt i64 %self, 0, !dbg !416
  %3 = zext i1 %2 to i8, !dbg !416
  store i8 %3, ptr %1, align 1, !dbg !416
  %4 = load i8, ptr %1, align 1, !dbg !417
  %5 = trunc i8 %4 to i1, !dbg !418
  br i1 %5, label %entry1, label %match_end, !dbg !418

entry1:                                           ; preds = %entry
  %6 = call fastcc ptr @core.mem.current-arena_924(), !dbg !419
  call fastcc void @core.list.push-in_1941(ptr %w, ptr %6, i8 45), !dbg !419
  br label %match_end, !dbg !418

match_end:                                        ; preds = %entry1, %entry
  %7 = load i8, ptr %1, align 1, !dbg !420
  %8 = trunc i8 %7 to i1, !dbg !421
  br i1 %8, label %arm_cons, label %arm_cons2, !dbg !421

arm_cons:                                         ; preds = %match_end
  %9 = sub i64 0, %self, !dbg !422
  store i64 %9, ptr %0, align 8, !dbg !421
  br label %match_end3, !dbg !421

arm_cons2:                                        ; preds = %match_end
  store i64 %self, ptr %0, align 8, !dbg !421
  br label %match_end3, !dbg !421

match_end3:                                       ; preds = %arm_cons, %arm_cons2
  %10 = load i64, ptr %0, align 8, !dbg !423
  call fastcc void @core.format-uint_1879(ptr %w, i64 %10, i64 10), !dbg !424
  ret void, !dbg !424
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.string-builder.build-tmp_1259(ptr %self) #0 !dbg !425 {
prelude:
  %abi_pair_storage2 = alloca [2 x i64], align 8
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  %0 = alloca { { ptr, i64 }, i64 }, align 8
  %1 = alloca { { ptr, i64 } }, align 8
  %2 = alloca { ptr, i64 }, align 8
  %3 = alloca { ptr, i64 }, align 8
  %4 = alloca { { ptr, i64 } }, align 8
  %5 = alloca { ptr, i64 }, align 8
  %6 = alloca { { { ptr, i64 } } }, align 8
  %7 = alloca { { { ptr, i64 } } }, align 8
  %8 = alloca { { ptr, i64 } }, align 8
  %9 = alloca { { { ptr, i64 } } }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %self, i64 24, i1 false), !dbg !428
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %0, i64 16, i1 false), !dbg !428
  %10 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %0, i32 0, i32 1, !dbg !428
  %11 = load i64, ptr %10, align 8, !dbg !428
  %12 = load [2 x i64], ptr %2, align 8, !dbg !428
  %13 = call fastcc [2 x i64] @core.buffer.slice_1776([2 x i64] %12, i64 0, i64 %11), !dbg !428
  store [2 x i64] %13, ptr %abi_pair_storage, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %abi_pair_storage, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %3, i64 16, i1 false), !dbg !428
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %5, i64 16, i1 false), !dbg !428
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %4, i64 16, i1 false), !dbg !428
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %8, ptr align 8 %1, i64 16, i1 false), !dbg !428
  %14 = load [2 x i64], ptr %8, align 8, !dbg !429
  %15 = call fastcc [2 x i64] @core.span.as-char-span_1174([2 x i64] %14), !dbg !429
  store [2 x i64] %15, ptr %abi_pair_storage1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 8 %abi_pair_storage1, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %9, ptr align 8 %7, i64 16, i1 false), !dbg !429
  %16 = call fastcc ptr @core.mem.tmp_926(), !dbg !430
  %17 = load [2 x i64], ptr %9, align 8, !dbg !429
  %18 = call fastcc [2 x i64] @core.string.cloned-in_1774([2 x i64] %17, ptr %16), !dbg !429
  store [2 x i64] %18, ptr %abi_pair_storage2, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %abi_pair_storage2, i64 16, i1 false)
  %19 = load [2 x i64], ptr %6, align 8, !dbg !429
  ret [2 x i64] %19, !dbg !429
}

; Function Attrs: noreturn nounwind
define internal fastcc void @core.crash-bounds_336(ptr %locn, i64 %size, i64 %index, [2 x i64] %msg) #1 !dbg !431 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage3 = alloca [2 x i64], align 8
  %abi_pair_storage2 = alloca [2 x i64], align 8
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %0 = alloca { { { ptr, i64 }, i64 } }, align 8
  %1 = alloca { { ptr, i64 }, i64 }, align 8
  %2 = alloca { { ptr, i64 }, i64 }, align 8
  %3 = alloca { ptr, i64 }, align 8
  %4 = alloca { ptr, i64 }, align 8
  %5 = alloca { { { ptr, i64 }, i64 } }, align 8
  %6 = alloca { { ptr, i64 }, i64 }, align 8
  %7 = alloca { { { ptr, i64 } } }, align 8
  %8 = alloca { { { ptr, i64 } } }, align 8
  %9 = alloca { { ptr, i64 } }, align 8
  %10 = alloca { { ptr, i64 } }, align 8
  %11 = alloca { ptr, i64 }, align 8
  %12 = alloca { ptr, i64 }, align 8
  %13 = alloca { { ptr, i64 } }, align 8
  %14 = alloca { ptr, i64 }, align 8
  %15 = alloca { { { ptr, i64 } } }, align 8
  %16 = alloca { ptr, i64 }, align 8
  %17 = alloca { { ptr, i64 } }, align 8
  %18 = alloca { ptr, i64 }, align 8
  %19 = alloca { ptr, i64 }, align 8
  %20 = alloca { { ptr, i64 } }, align 8
  %21 = alloca { { { ptr, i64 } } }, align 8
  %22 = alloca { { { ptr, i64 } } }, align 8
  %23 = alloca { ptr, i64 }, align 8
  %24 = alloca { { ptr, i64 } }, align 8
  %25 = alloca { ptr, i64 }, align 8
  %26 = alloca { ptr, i64 }, align 8
  %27 = alloca { ptr, i64 }, align 8
  %28 = alloca { { { ptr, i64 } } }, align 8
  %29 = alloca { { ptr, i64 } }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %msg, ptr %abi_pair_storage, align 8, !dbg !434
  br label %entry

entry:                                            ; preds = %prelude
  %30 = call fastcc ptr @core.mem.current-arena_924(), !dbg !435
  %31 = call fastcc [2 x i64] @core.buffer.allocate-in_1786(ptr %30, i64 0), !dbg !435
  store [2 x i64] %31, ptr %abi_pair_storage1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %abi_pair_storage1, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %4, i64 16, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %3, i64 16, i1 false), !dbg !435
  %32 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %2, i32 0, i32 1, !dbg !435
  store i64 0, ptr %32, align 8, !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %2, i64 24, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %1, i64 24, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %0, i64 24, i1 false), !dbg !435
  %33 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !436
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %5, [2 x i64] %33), !dbg !436
  %34 = load [2 x i64], ptr @static_73, align 8, !dbg !435
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %5, [2 x i64] %34), !dbg !435
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %index, ptr %5), !dbg !437
  %35 = load [2 x i64], ptr @static_74, align 8, !dbg !435
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %5, [2 x i64] %35), !dbg !435
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %size, ptr %5), !dbg !438
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %5, i64 24, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %11, ptr align 8 %6, i64 16, i1 false), !dbg !435
  %36 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %6, i32 0, i32 1, !dbg !435
  %37 = load i64, ptr %36, align 8, !dbg !435
  %38 = load [2 x i64], ptr %11, align 8, !dbg !435
  %39 = call fastcc [2 x i64] @core.buffer.slice_1776([2 x i64] %38, i64 0, i64 %37), !dbg !435
  store [2 x i64] %39, ptr %abi_pair_storage2, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %12, ptr align 8 %abi_pair_storage2, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %14, ptr align 8 %12, i64 16, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %13, ptr align 8 %14, i64 16, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %9, ptr align 8 %13, i64 16, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %10, ptr align 8 %9, i64 16, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %16, ptr align 8 %10, i64 16, i1 false), !dbg !435
  %40 = load ptr, ptr %16, align 8, !dbg !435
  %41 = getelementptr inbounds nuw { ptr, i64 }, ptr %16, i32 0, i32 1, !dbg !435
  %42 = load i64, ptr %41, align 8, !dbg !435
  store ptr %40, ptr %19, align 8, !dbg !435
  %43 = getelementptr inbounds nuw { ptr, i64 }, ptr %19, i32 0, i32 1, !dbg !435
  store i64 %42, ptr %43, align 8, !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %18, ptr align 8 %19, i64 16, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %17, ptr align 8 %18, i64 16, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %15, ptr align 8 %17, i64 16, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %22, ptr align 8 %15, i64 16, i1 false), !dbg !435
  %44 = call fastcc ptr @core.mem.tmp_926(), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %20, ptr align 8 %22, i64 16, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %23, ptr align 8 %20, i64 16, i1 false), !dbg !435
  %45 = getelementptr inbounds nuw { ptr, i64 }, ptr %23, i32 0, i32 1, !dbg !435
  %46 = load i64, ptr %45, align 8, !dbg !435
  %47 = call fastcc [2 x i64] @core.buffer.allocate-in_1861(ptr %44, i64 %46), !dbg !435
  store [2 x i64] %47, ptr %abi_pair_storage3, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %27, ptr align 8 %abi_pair_storage3, i64 16, i1 false)
  %48 = load [2 x i64], ptr %23, align 8, !dbg !435
  %49 = load [2 x i64], ptr %27, align 8, !dbg !435
  call fastcc void @core.buffer.copy_1862([2 x i64] %48, [2 x i64] %49), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %26, ptr align 8 %27, i64 16, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %25, ptr align 8 %26, i64 16, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %24, ptr align 8 %25, i64 16, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %29, ptr align 8 %24, i64 16, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %28, ptr align 8 %29, i64 16, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %21, ptr align 8 %28, i64 16, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 8 %21, i64 16, i1 false), !dbg !435
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %8, ptr align 8 %7, i64 16, i1 false), !dbg !435
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %locn, i64 24, i1 false)
  %50 = load [2 x i64], ptr %8, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %50), !dbg !439
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !439
  unreachable, !dbg !439
}

; Function Attrs: nounwind
define internal fastcc ptr @core.mem.current-arena_924() #0 !dbg !440 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  %0 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %1 = alloca ptr, align 8
  %2 = alloca ptr, align 8
  %3 = alloca { i8, { i64 } }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %4 = call fastcc [2 x i64] @core.impl_23as-span.last_for_t3148_1809(ptr @_root__core__mem__arena-stack), !dbg !444
  store [2 x i64] %4, ptr %abi_pair_storage, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %abi_pair_storage, i64 16, i1 false)
  %5 = load i8, ptr %3, align 1, !dbg !445
  %6 = icmp eq i8 %5, 1, !dbg !445
  %7 = zext i1 %6 to i8, !dbg !445
  %8 = trunc i8 %7 to i1, !dbg !444
  br i1 %8, label %matching_cond_continue, label %arm_cond, !dbg !444

arm_cond:                                         ; preds = %entry
  %9 = load i8, ptr %3, align 1, !dbg !446
  %10 = icmp eq i8 %9, 0, !dbg !446
  %11 = zext i1 %10 to i8, !dbg !446
  %12 = trunc i8 %11 to i1, !dbg !444
  br i1 %12, label %arm_cons, label %arm_cons1, !dbg !444

arm_cons:                                         ; preds = %arm_cond
  %13 = call fastcc ptr @core.mem.tmp_926(), !dbg !447
  br label %match_end, !dbg !444

arm_cons1:                                        ; preds = %arm_cond
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @static_158, i64 16, i1 false), !dbg !444
  %14 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %0, i32 0, i32 1, !dbg !444
  store i64 79, ptr %14, align 8, !dbg !444
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %0, i64 24, i1 false)
  %15 = load [2 x i64], ptr @static_25, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %15), !dbg !444
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !444
  unreachable, !dbg !444

match_end:                                        ; preds = %matching_cond_continue, %arm_cons
  %16 = phi ptr [ %18, %matching_cond_continue ], [ %13, %arm_cons ], !dbg !444
  ret ptr %16, !dbg !444

matching_cond_continue:                           ; preds = %entry
  %17 = getelementptr inbounds nuw { i8, { i64 } }, ptr %3, i32 0, i32 1, !dbg !445
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %17, i64 8, i1 false), !dbg !445
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %2, i64 8, i1 false), !dbg !445
  %18 = load ptr, ptr %1, align 8, !dbg !448
  br label %match_end, !dbg !444
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.buffer.allocate-in_1786(ptr %alloc, i64 %count) #0 !dbg !449 {
prelude:
  %0 = alloca { ptr, i64 }, align 8
  %1 = alloca ptr, align 8
  %2 = alloca i64, align 8
  %3 = alloca ptr, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %4 = icmp eq i64 %count, 0, !dbg !452
  %5 = zext i1 %4 to i8, !dbg !452
  %6 = trunc i8 %5 to i1, !dbg !453
  br i1 %6, label %arm_cons, label %entry1, !dbg !453

arm_cons:                                         ; preds = %entry
  %7 = load ptr, ptr @_root__core__ptr__null, align 8, !dbg !454
  store ptr %7, ptr %3, align 8, !dbg !453
  br label %match_end, !dbg !453

entry1:                                           ; preds = %entry
  %8 = mul i64 %count, 1, !dbg !455
  store i64 %8, ptr %2, align 8, !dbg !455
  %9 = load i64, ptr %2, align 8, !dbg !456
  %10 = call fastcc ptr @core.arena.alloc-layout_350(ptr %alloc, i64 %9, i64 1), !dbg !457
  store ptr %10, ptr %3, align 8, !dbg !453
  br label %match_end, !dbg !453

match_end:                                        ; preds = %arm_cons, %entry1
  %11 = load ptr, ptr %3, align 8, !dbg !458
  store ptr %11, ptr %1, align 8, !dbg !458
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %1, i64 8, i1 false), !dbg !459
  %12 = getelementptr inbounds nuw { ptr, i64 }, ptr %0, i32 0, i32 1, !dbg !460
  store i64 %count, ptr %12, align 8, !dbg !461
  %13 = load [2 x i64], ptr %0, align 8, !dbg !460
  ret [2 x i64] %13, !dbg !460
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.buffer.slice_1776([2 x i64] %self, i64 %start, i64 %end) #0 !dbg !462 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage12 = alloca [2 x i64], align 8
  %abi_pair_storage11 = alloca [2 x i64], align 8
  %0 = alloca ptr, align 8
  %1 = alloca { { ptr, i64 }, i64 }, align 8
  %2 = alloca { { { ptr, i64 } } }, align 8
  %3 = alloca i64, align 8
  %4 = alloca { { ptr, i64 } }, align 8
  %5 = alloca { ptr, i64 }, align 8
  %6 = alloca { ptr, i64 }, align 8
  %7 = alloca { ptr, i64 }, align 8
  %8 = alloca { { ptr, i64 } }, align 8
  %9 = alloca i64, align 8
  %10 = alloca { { { ptr, i64 } } }, align 8
  %11 = alloca { ptr, i64 }, align 8
  %12 = alloca { { ptr, i64 } }, align 8
  %13 = alloca { ptr, i64 }, align 8
  %14 = alloca { ptr, i64 }, align 8
  %15 = alloca { { ptr, i64 } }, align 8
  %16 = alloca { { { ptr, i64 } } }, align 8
  %17 = alloca { { { ptr, i64 }, i64 } }, align 8
  %18 = alloca { ptr, i64 }, align 8
  %19 = alloca { { ptr, i64 } }, align 8
  %20 = alloca { ptr, i64 }, align 8
  %21 = alloca { ptr, i64 }, align 8
  %22 = alloca { ptr, i64 }, align 8
  %23 = alloca { { { ptr, i64 } } }, align 8
  %24 = alloca { ptr, i64 }, align 8
  %25 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %26 = alloca { { { ptr, i64 } } }, align 8
  %27 = alloca { { ptr, i64 } }, align 8
  %28 = alloca { ptr, i64 }, align 8
  %29 = alloca { { { ptr, i64 } } }, align 8
  %30 = alloca { { ptr, i64 } }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !465
  br label %entry

entry:                                            ; preds = %prelude
  %31 = icmp slt i64 %start, 0, !dbg !466
  %32 = zext i1 %31 to i8, !dbg !466
  %33 = trunc i8 %32 to i1, !dbg !466
  br i1 %33, label %arm_cons1, label %arm_cons2, !dbg !466

arm_cons:                                         ; preds = %match_end
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %25, ptr align 8 @static_126, i64 16, i1 false), !dbg !467
  %34 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %25, i32 0, i32 1, !dbg !467
  store i64 199, ptr %34, align 8, !dbg !467
  call fastcc void @core.string-builder.new_1255(ptr noalias sret({ { { ptr, i64 }, i64 } }) align 8 dereferenceable(24) %17), !dbg !468
  %35 = load [2 x i64], ptr @static_136, align 8, !dbg !468
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %17, [2 x i64] %35), !dbg !468
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %start, ptr %17), !dbg !469
  %36 = load [2 x i64], ptr @static_137, align 8, !dbg !468
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %17, [2 x i64] %36), !dbg !468
  %37 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !470
  %38 = load i64, ptr %37, align 8, !dbg !470
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %38, ptr %17), !dbg !470
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %17, i64 24, i1 false), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %1, i64 16, i1 false), !dbg !468
  %39 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %1, i32 0, i32 1, !dbg !468
  %40 = load i64, ptr %39, align 8, !dbg !468
  %41 = load [2 x i64], ptr %6, align 8, !dbg !468
  %42 = call fastcc [2 x i64] @core.buffer.slice_1776([2 x i64] %41, i64 0, i64 %40), !dbg !468
  store [2 x i64] %42, ptr %abi_pair_storage11, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 8 %abi_pair_storage11, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %28, ptr align 8 %7, i64 16, i1 false), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %8, ptr align 8 %28, i64 16, i1 false), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %8, i64 16, i1 false), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %27, ptr align 8 %4, i64 16, i1 false), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %11, ptr align 8 %27, i64 16, i1 false), !dbg !468
  %43 = load ptr, ptr %11, align 8, !dbg !468
  %44 = getelementptr inbounds nuw { ptr, i64 }, ptr %11, i32 0, i32 1, !dbg !468
  %45 = load i64, ptr %44, align 8, !dbg !468
  store ptr %43, ptr %14, align 8, !dbg !468
  %46 = getelementptr inbounds nuw { ptr, i64 }, ptr %14, i32 0, i32 1, !dbg !468
  store i64 %45, ptr %46, align 8, !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %13, ptr align 8 %14, i64 16, i1 false), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %12, ptr align 8 %13, i64 16, i1 false), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %10, ptr align 8 %12, i64 16, i1 false), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %29, ptr align 8 %10, i64 16, i1 false), !dbg !468
  %47 = call fastcc ptr @core.mem.tmp_926(), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %15, ptr align 8 %29, i64 16, i1 false), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %18, ptr align 8 %15, i64 16, i1 false), !dbg !468
  %48 = getelementptr inbounds nuw { ptr, i64 }, ptr %18, i32 0, i32 1, !dbg !468
  %49 = load i64, ptr %48, align 8, !dbg !468
  %50 = call fastcc [2 x i64] @core.buffer.allocate-in_1861(ptr %47, i64 %49), !dbg !468
  store [2 x i64] %50, ptr %abi_pair_storage12, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %22, ptr align 8 %abi_pair_storage12, i64 16, i1 false)
  %51 = load [2 x i64], ptr %18, align 8, !dbg !468
  %52 = load [2 x i64], ptr %22, align 8, !dbg !468
  call fastcc void @core.buffer.copy_1862([2 x i64] %51, [2 x i64] %52), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %21, ptr align 8 %22, i64 16, i1 false), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %20, ptr align 8 %21, i64 16, i1 false), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %19, ptr align 8 %20, i64 16, i1 false), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %30, ptr align 8 %19, i64 16, i1 false), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %23, ptr align 8 %30, i64 16, i1 false), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %16, ptr align 8 %23, i64 16, i1 false), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %16, i64 16, i1 false), !dbg !468
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %26, ptr align 8 %2, i64 16, i1 false), !dbg !468
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %25, i64 24, i1 false)
  %53 = load [2 x i64], ptr %26, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %53), !dbg !467
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !467
  unreachable, !dbg !467

arm_cons1:                                        ; preds = %entry
  br label %match_end, !dbg !466

arm_cons2:                                        ; preds = %entry
  %54 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !471
  %55 = load i64, ptr %54, align 8, !dbg !471
  %56 = icmp sgt i64 %start, %55, !dbg !472
  %57 = zext i1 %56 to i8, !dbg !472
  br label %match_end, !dbg !466

match_end:                                        ; preds = %arm_cons1, %arm_cons2
  %58 = phi i8 [ 1, %arm_cons1 ], [ %57, %arm_cons2 ], !dbg !466
  %59 = trunc i8 %58 to i1, !dbg !473
  br i1 %59, label %arm_cons, label %arm_cond, !dbg !473

arm_cond:                                         ; preds = %match_end
  %60 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !474
  %61 = load i64, ptr %60, align 8, !dbg !474
  %62 = icmp sgt i64 %end, %61, !dbg !475
  %63 = zext i1 %62 to i8, !dbg !475
  %64 = trunc i8 %63 to i1, !dbg !476
  br i1 %64, label %arm_cons3, label %arm_cons4, !dbg !476

arm_cons3:                                        ; preds = %arm_cond
  %65 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !477
  %66 = load i64, ptr %65, align 8, !dbg !477
  store i64 %66, ptr %9, align 8, !dbg !476
  br label %match_end5, !dbg !476

arm_cons4:                                        ; preds = %arm_cond
  store i64 %end, ptr %9, align 8, !dbg !476
  br label %match_end5, !dbg !476

match_end5:                                       ; preds = %arm_cons3, %arm_cons4
  %67 = load i64, ptr %9, align 8, !dbg !478
  %68 = icmp sle i64 %67, %start, !dbg !478
  %69 = zext i1 %68 to i8, !dbg !478
  %70 = trunc i8 %69 to i1, !dbg !478
  br i1 %70, label %arm_cons8, label %arm_cons9, !dbg !478

entry6:                                           ; preds = %match_end10
  %71 = load ptr, ptr @_root__core__ptr__null, align 8, !dbg !479
  store ptr %71, ptr %24, align 8, !dbg !479
  %72 = getelementptr inbounds nuw { ptr, i64 }, ptr %24, i32 0, i32 1, !dbg !479
  store i64 0, ptr %72, align 8, !dbg !479
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %24, i64 16, i1 false), !dbg !479
  %73 = load [2 x i64], ptr %5, align 8, !dbg !480
  ret [2 x i64] %73, !dbg !480

match_end7:                                       ; preds = %match_end10
  %74 = load i64, ptr %9, align 8, !dbg !481
  %75 = sub i64 %74, %start, !dbg !481
  store i64 %75, ptr %3, align 8, !dbg !481
  %76 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !482
  call fastcc void @core.buffer.check-bounds_1868([2 x i64] %76, i64 %start), !dbg !482
  %77 = load ptr, ptr %abi_pair_storage, align 8, !dbg !482
  %78 = getelementptr inbounds i8, ptr %77, i64 %start, !dbg !482
  store ptr %78, ptr %0, align 8, !dbg !482
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %0, i64 8, i1 false), !dbg !483
  %79 = getelementptr inbounds nuw { ptr, i64 }, ptr %5, i32 0, i32 1, !dbg !484
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %79, ptr align 8 %3, i64 8, i1 false), !dbg !485
  %80 = load [2 x i64], ptr %5, align 8, !dbg !484
  ret [2 x i64] %80, !dbg !484

arm_cons8:                                        ; preds = %match_end5
  br label %match_end10, !dbg !478

arm_cons9:                                        ; preds = %match_end5
  %81 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !486
  %82 = load i64, ptr %81, align 8, !dbg !486
  %83 = icmp eq i64 %82, 0, !dbg !486
  %84 = zext i1 %83 to i8, !dbg !486
  br label %match_end10, !dbg !478

match_end10:                                      ; preds = %arm_cons8, %arm_cons9
  %85 = phi i8 [ 1, %arm_cons8 ], [ %84, %arm_cons9 ], !dbg !478
  %86 = trunc i8 %85 to i1, !dbg !487
  br i1 %86, label %entry6, label %match_end7, !dbg !487
}

; Function Attrs: nounwind
define internal fastcc ptr @core.mem.tmp_926() #0 !dbg !488 {
prelude:
  %0 = alloca ptr, align 8
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @_root__core__mem__arena-tmp, i64 8, i1 false), !dbg !489
  %1 = load ptr, ptr %0, align 8, !dbg !489
  %2 = ptrtoint ptr %1 to i64, !dbg !490
  %3 = icmp eq i64 %2, 0, !dbg !490
  %4 = zext i1 %3 to i8, !dbg !490
  %5 = trunc i8 %4 to i1, !dbg !491
  br i1 %5, label %arm_cons, label %match_end, !dbg !491

arm_cons:                                         ; preds = %entry
  call fastcc void @core.mem.init-tmp-arena_925(), !dbg !492
  br label %match_end, !dbg !491

match_end:                                        ; preds = %arm_cons, %entry
  %6 = load ptr, ptr @_root__core__mem__arena-tmp, align 8, !dbg !493
  ret ptr %6, !dbg !493
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.buffer.allocate-in_1861(ptr %alloc, i64 %count) #0 !dbg !494 {
prelude:
  %0 = alloca { ptr, i64 }, align 8
  %1 = alloca ptr, align 8
  %2 = alloca i64, align 8
  %3 = alloca ptr, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %4 = icmp eq i64 %count, 0, !dbg !497
  %5 = zext i1 %4 to i8, !dbg !497
  %6 = trunc i8 %5 to i1, !dbg !498
  br i1 %6, label %arm_cons, label %entry1, !dbg !498

arm_cons:                                         ; preds = %entry
  %7 = load ptr, ptr @_root__core__ptr__null, align 8, !dbg !499
  store ptr %7, ptr %3, align 8, !dbg !498
  br label %match_end, !dbg !498

entry1:                                           ; preds = %entry
  %8 = mul i64 %count, 1, !dbg !500
  store i64 %8, ptr %2, align 8, !dbg !500
  %9 = load i64, ptr %2, align 8, !dbg !501
  %10 = call fastcc ptr @core.arena.alloc-layout_350(ptr %alloc, i64 %9, i64 1), !dbg !502
  store ptr %10, ptr %3, align 8, !dbg !498
  br label %match_end, !dbg !498

match_end:                                        ; preds = %arm_cons, %entry1
  %11 = load ptr, ptr %3, align 8, !dbg !503
  store ptr %11, ptr %1, align 8, !dbg !503
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %1, i64 8, i1 false), !dbg !504
  %12 = getelementptr inbounds nuw { ptr, i64 }, ptr %0, i32 0, i32 1, !dbg !505
  store i64 %count, ptr %12, align 8, !dbg !506
  %13 = load [2 x i64], ptr %0, align 8, !dbg !505
  ret [2 x i64] %13, !dbg !505
}

; Function Attrs: nounwind
define internal fastcc void @core.buffer.copy_1862([2 x i64] %src, [2 x i64] %dst) #0 !dbg !507 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage4 = alloca [2 x i64], align 8
  %abi_pair_storage3 = alloca [2 x i64], align 8
  %0 = alloca i64, align 8
  %1 = alloca { { ptr, i64 }, i64 }, align 8
  %2 = alloca { { { ptr, i64 } } }, align 8
  %3 = alloca { { { ptr, i64 }, i64 } }, align 8
  %4 = alloca { { ptr, i64 } }, align 8
  %5 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %6 = alloca { ptr, i64 }, align 8
  %7 = alloca { ptr, i64 }, align 8
  %8 = alloca { { ptr, i64 } }, align 8
  %9 = alloca { { { ptr, i64 } } }, align 8
  %10 = alloca { { { ptr, i64 } } }, align 8
  %11 = alloca { ptr, i64 }, align 8
  %12 = alloca { { ptr, i64 } }, align 8
  %13 = alloca { ptr, i64 }, align 8
  %14 = alloca { ptr, i64 }, align 8
  %15 = alloca { { ptr, i64 } }, align 8
  %16 = alloca { { { ptr, i64 } } }, align 8
  %17 = alloca { { ptr, i64 } }, align 8
  %18 = alloca { ptr, i64 }, align 8
  %19 = alloca { { ptr, i64 } }, align 8
  %20 = alloca { ptr, i64 }, align 8
  %21 = alloca { ptr, i64 }, align 8
  %22 = alloca { ptr, i64 }, align 8
  %23 = alloca { { { ptr, i64 } } }, align 8
  %24 = alloca { ptr, i64 }, align 8
  %25 = alloca { { { ptr, i64 } } }, align 8
  %26 = alloca { { ptr, i64 } }, align 8
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %src, ptr %abi_pair_storage, align 8, !dbg !511
  store [2 x i64] %dst, ptr %abi_pair_storage1, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %27 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !512
  %28 = load i64, ptr %27, align 8, !dbg !512
  %29 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage1, i32 0, i32 1, !dbg !513
  %30 = load i64, ptr %29, align 8, !dbg !513
  %31 = icmp sgt i64 %28, %30, !dbg !512
  %32 = zext i1 %31 to i8, !dbg !512
  %33 = trunc i8 %32 to i1, !dbg !514
  br i1 %33, label %arm_cons, label %arm_cond, !dbg !514

arm_cons:                                         ; preds = %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 @static_126, i64 16, i1 false), !dbg !515
  %34 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %5, i32 0, i32 1, !dbg !515
  store i64 124, ptr %34, align 8, !dbg !515
  call fastcc void @core.string-builder.new_1255(ptr noalias sret({ { { ptr, i64 }, i64 } }) align 8 dereferenceable(24) %3), !dbg !516
  %35 = load [2 x i64], ptr @static_129, align 8, !dbg !516
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %3, [2 x i64] %35), !dbg !516
  %36 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !517
  %37 = load i64, ptr %36, align 8, !dbg !517
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %37, ptr %3), !dbg !517
  %38 = load [2 x i64], ptr @static_130, align 8, !dbg !516
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %3, [2 x i64] %38), !dbg !516
  %39 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage1, i32 0, i32 1, !dbg !518
  %40 = load i64, ptr %39, align 8, !dbg !518
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %40, ptr %3), !dbg !518
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %3, i64 24, i1 false), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %1, i64 16, i1 false), !dbg !516
  %41 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %1, i32 0, i32 1, !dbg !516
  %42 = load i64, ptr %41, align 8, !dbg !516
  %43 = load [2 x i64], ptr %6, align 8, !dbg !516
  %44 = call fastcc [2 x i64] @core.buffer.slice_1776([2 x i64] %43, i64 0, i64 %42), !dbg !516
  store [2 x i64] %44, ptr %abi_pair_storage3, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 8 %abi_pair_storage3, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %24, ptr align 8 %7, i64 16, i1 false), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %8, ptr align 8 %24, i64 16, i1 false), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %8, i64 16, i1 false), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %17, ptr align 8 %4, i64 16, i1 false), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %11, ptr align 8 %17, i64 16, i1 false), !dbg !516
  %45 = load ptr, ptr %11, align 8, !dbg !516
  %46 = getelementptr inbounds nuw { ptr, i64 }, ptr %11, i32 0, i32 1, !dbg !516
  %47 = load i64, ptr %46, align 8, !dbg !516
  store ptr %45, ptr %14, align 8, !dbg !516
  %48 = getelementptr inbounds nuw { ptr, i64 }, ptr %14, i32 0, i32 1, !dbg !516
  store i64 %47, ptr %48, align 8, !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %13, ptr align 8 %14, i64 16, i1 false), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %12, ptr align 8 %13, i64 16, i1 false), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %10, ptr align 8 %12, i64 16, i1 false), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %25, ptr align 8 %10, i64 16, i1 false), !dbg !516
  %49 = call fastcc ptr @core.mem.tmp_926(), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %15, ptr align 8 %25, i64 16, i1 false), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %18, ptr align 8 %15, i64 16, i1 false), !dbg !516
  %50 = getelementptr inbounds nuw { ptr, i64 }, ptr %18, i32 0, i32 1, !dbg !516
  %51 = load i64, ptr %50, align 8, !dbg !516
  %52 = call fastcc [2 x i64] @core.buffer.allocate-in_1861(ptr %49, i64 %51), !dbg !516
  store [2 x i64] %52, ptr %abi_pair_storage4, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %22, ptr align 8 %abi_pair_storage4, i64 16, i1 false)
  %53 = load [2 x i64], ptr %18, align 8, !dbg !516
  %54 = load [2 x i64], ptr %22, align 8, !dbg !516
  call fastcc void @core.buffer.copy_1862([2 x i64] %53, [2 x i64] %54), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %21, ptr align 8 %22, i64 16, i1 false), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %20, ptr align 8 %21, i64 16, i1 false), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %19, ptr align 8 %20, i64 16, i1 false), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %26, ptr align 8 %19, i64 16, i1 false), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %23, ptr align 8 %26, i64 16, i1 false), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %16, ptr align 8 %23, i64 16, i1 false), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %16, i64 16, i1 false), !dbg !516
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %9, ptr align 8 %2, i64 16, i1 false), !dbg !516
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %5, i64 24, i1 false)
  %55 = load [2 x i64], ptr %9, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %55), !dbg !515
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !515
  unreachable, !dbg !515

arm_cond:                                         ; preds = %entry
  %56 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !519
  %57 = load i64, ptr %56, align 8, !dbg !519
  %58 = icmp eq i64 %57, 0, !dbg !519
  %59 = zext i1 %58 to i8, !dbg !519
  %60 = trunc i8 %59 to i1, !dbg !520
  br i1 %60, label %arm_cons2, label %match_end, !dbg !520

arm_cons2:                                        ; preds = %arm_cond
  ret void, !dbg !521

match_end:                                        ; preds = %arm_cond
  %61 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !522
  %62 = load i64, ptr %61, align 8, !dbg !522
  %63 = mul i64 %62, 1, !dbg !523
  store i64 %63, ptr %0, align 8, !dbg !523
  %64 = load ptr, ptr %abi_pair_storage1, align 8, !dbg !524
  %65 = load ptr, ptr %abi_pair_storage, align 8, !dbg !525
  %66 = load i64, ptr %0, align 8, !dbg !526
  call void @core.mem.copy_234(ptr %64, ptr %65, i64 %66), !dbg !527
  ret void, !dbg !527
}

; Function Attrs: nounwind
define internal void @core.mem.copy_234(ptr %dst, ptr %src, i64 %count) #0 !dbg !528 {
prelude:
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %dst, ptr align 1 %src, i64 %count, i1 false), !dbg !532
  ret void, !dbg !532
}

; Function Attrs: nounwind
define internal fastcc ptr @core.arena.alloc-layout_350(ptr %self, i64 %size, i64 %align) #0 !dbg !533 {
prelude:
  %0 = alloca ptr, align 8
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  %6 = alloca ptr, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %7 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %self, i32 0, i32 1, !dbg !537
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %7, i64 8, i1 false), !dbg !537
  %8 = icmp sgt i64 %align, 8, !dbg !538
  %9 = zext i1 %8 to i8, !dbg !538
  %10 = trunc i8 %9 to i1, !dbg !539
  br i1 %10, label %arm_cons, label %arm_cons1, !dbg !539

arm_cons:                                         ; preds = %entry
  %11 = load ptr, ptr %6, align 8, !dbg !540
  %12 = ptrtoint ptr %11 to i64, !dbg !540
  %13 = sub i64 %align, %12, !dbg !541
  %14 = sub i64 %align, 1, !dbg !541
  %15 = and i64 %13, %14, !dbg !541
  store i64 %15, ptr %5, align 8, !dbg !539
  br label %match_end, !dbg !539

arm_cons1:                                        ; preds = %entry
  store i64 0, ptr %5, align 8, !dbg !539
  br label %match_end, !dbg !539

match_end:                                        ; preds = %arm_cons, %arm_cons1
  %16 = load i64, ptr %5, align 8, !dbg !542
  %17 = add i64 %size, %16, !dbg !543
  store i64 %17, ptr %4, align 8, !dbg !543
  %18 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %self, i32 0, i32 2, !dbg !544
  %19 = load ptr, ptr %18, align 8, !dbg !544
  %20 = load ptr, ptr %6, align 8, !dbg !545
  %21 = ptrtoint ptr %19 to i64, !dbg !544
  %22 = ptrtoint ptr %20 to i64, !dbg !544
  %23 = sub i64 %21, %22, !dbg !544
  store i64 %23, ptr %3, align 8, !dbg !544
  %24 = load i64, ptr %3, align 8, !dbg !546
  %25 = load i64, ptr %4, align 8, !dbg !547
  %26 = icmp sge i64 %24, %25, !dbg !546
  %27 = zext i1 %26 to i8, !dbg !546
  %28 = trunc i8 %27 to i1, !dbg !548
  br i1 %28, label %arm_cons2, label %arm_cons3, !dbg !548

arm_cons2:                                        ; preds = %match_end
  %29 = load i64, ptr %4, align 8, !dbg !549
  %30 = sub i64 8, %29, !dbg !550
  %31 = and i64 %30, 7, !dbg !550
  store i64 %31, ptr %2, align 8, !dbg !550
  %32 = load i64, ptr %4, align 8, !dbg !551
  %33 = load i64, ptr %2, align 8, !dbg !552
  %34 = add i64 %32, %33, !dbg !551
  store i64 %34, ptr %1, align 8, !dbg !551
  %35 = load ptr, ptr %6, align 8, !dbg !553
  %36 = load i64, ptr %5, align 8, !dbg !554
  %37 = getelementptr inbounds i8, ptr %35, i64 %36, !dbg !553
  store ptr %37, ptr %0, align 8, !dbg !553
  %38 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %self, i32 0, i32 1, !dbg !555
  %39 = load ptr, ptr %6, align 8, !dbg !556
  %40 = load i64, ptr %1, align 8, !dbg !557
  %41 = getelementptr inbounds i8, ptr %39, i64 %40, !dbg !556
  store ptr %41, ptr %38, align 8, !dbg !555
  %42 = load ptr, ptr %0, align 8, !dbg !558
  br label %match_end4, !dbg !548

arm_cons3:                                        ; preds = %match_end
  call fastcc void @core.arena._grow_351(ptr %self, i64 %size, i64 %align), !dbg !559
  %43 = call fastcc ptr @core.arena.alloc-layout_350(ptr %self, i64 %size, i64 %align), !dbg !560
  br label %match_end4, !dbg !548

match_end4:                                       ; preds = %arm_cons2, %arm_cons3
  %44 = phi ptr [ %42, %arm_cons2 ], [ %43, %arm_cons3 ], !dbg !548
  ret ptr %44, !dbg !548
}

; Function Attrs: nounwind
define internal fastcc void @core.arena._grow_351(ptr %self, i64 %size, i64 %align) #0 !dbg !561 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  %0 = alloca ptr, align 8
  %1 = alloca { { ptr, i64 }, ptr }, align 8
  %2 = alloca ptr, align 8
  %3 = alloca { ptr, i64 }, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  %8 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %9 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %self, i32 0, i32 6, !dbg !565
  %10 = load i8, ptr %9, align 1, !dbg !565
  %11 = trunc i8 %10 to i1, !dbg !566
  br i1 %11, label %arm_cons, label %match_end, !dbg !566

arm_cons:                                         ; preds = %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %8, ptr align 8 @static_104, i64 16, i1 false), !dbg !567
  %12 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %8, i32 0, i32 1, !dbg !567
  store i64 137, ptr %12, align 8, !dbg !567
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %8, i64 24, i1 false)
  %13 = load [2 x i64], ptr @static_110, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %13), !dbg !567
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !567
  unreachable, !dbg !567

match_end:                                        ; preds = %entry
  %14 = add i64 32, %align, !dbg !568
  store i64 %14, ptr %7, align 8, !dbg !568
  %15 = load i64, ptr %7, align 8, !dbg !569
  %16 = add i64 %size, %15, !dbg !570
  store i64 %16, ptr %6, align 8, !dbg !570
  %17 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %self, i32 0, i32 4, !dbg !571
  %18 = load ptr, ptr %17, align 8, !dbg !571
  %19 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %self, i32 0, i32 3, !dbg !572
  %20 = load ptr, ptr %19, align 8, !dbg !572
  %21 = ptrtoint ptr %18 to i64, !dbg !571
  %22 = ptrtoint ptr %20 to i64, !dbg !571
  %23 = sub i64 %21, %22, !dbg !571
  store i64 %23, ptr %5, align 8, !dbg !571
  %24 = load i64, ptr %6, align 8, !dbg !573
  %25 = load i64, ptr %5, align 8, !dbg !574
  %26 = icmp sgt i64 %24, %25, !dbg !573
  %27 = zext i1 %26 to i8, !dbg !573
  %28 = trunc i8 %27 to i1, !dbg !575
  br i1 %28, label %arm_cons1, label %arm_cons2, !dbg !575

arm_cons1:                                        ; preds = %match_end
  %29 = load i64, ptr %6, align 8, !dbg !576
  store i64 %29, ptr %4, align 8, !dbg !575
  br label %match_end3, !dbg !575

arm_cons2:                                        ; preds = %match_end
  %30 = load i64, ptr %5, align 8, !dbg !577
  store i64 %30, ptr %4, align 8, !dbg !575
  br label %match_end3, !dbg !575

match_end3:                                       ; preds = %arm_cons1, %arm_cons2
  %31 = load i64, ptr %4, align 8, !dbg !578
  %32 = call fastcc [2 x i64] @core.platform.memory.acquire_953(i64 %31), !dbg !579
  store [2 x i64] %32, ptr %abi_pair_storage, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %abi_pair_storage, i64 16, i1 false)
  %33 = load ptr, ptr %3, align 8, !dbg !580
  store ptr %33, ptr %2, align 8, !dbg !580
  %34 = load ptr, ptr %2, align 8, !dbg !581
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %3, i64 16, i1 false), !dbg !582
  %35 = getelementptr inbounds nuw { { ptr, i64 }, ptr }, ptr %1, i32 0, i32 1, !dbg !583
  %36 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %self, i32 0, i32 5, !dbg !584
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %35, ptr align 8 %36, i64 8, i1 false), !dbg !584
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %34, ptr align 8 %1, i64 24, i1 false), !dbg !581
  %37 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %self, i32 0, i32 5, !dbg !585
  %38 = load ptr, ptr %3, align 8, !dbg !586
  store ptr %38, ptr %37, align 8, !dbg !585
  %39 = load ptr, ptr %3, align 8, !dbg !587
  %40 = getelementptr inbounds i8, ptr %39, i64 24, !dbg !587
  %41 = ptrtoint ptr %40 to i64, !dbg !587
  %42 = sub i64 8, %41, !dbg !587
  %43 = and i64 %42, 7, !dbg !587
  %44 = getelementptr inbounds i8, ptr %40, i64 %43, !dbg !587
  store ptr %44, ptr %0, align 8, !dbg !587
  %45 = load ptr, ptr %0, align 8, !dbg !588
  store ptr %45, ptr %self, align 8, !dbg !589
  %46 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %self, i32 0, i32 1, !dbg !590
  %47 = load ptr, ptr %0, align 8, !dbg !591
  store ptr %47, ptr %46, align 8, !dbg !590
  %48 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %self, i32 0, i32 2, !dbg !592
  %49 = load ptr, ptr %3, align 8, !dbg !593
  %50 = getelementptr inbounds nuw { ptr, i64 }, ptr %3, i32 0, i32 1, !dbg !594
  %51 = load i64, ptr %50, align 8, !dbg !594
  %52 = getelementptr inbounds i8, ptr %49, i64 %51, !dbg !593
  store ptr %52, ptr %48, align 8, !dbg !592
  ret void, !dbg !592
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.platform.memory.acquire_953(i64 %min-size) #0 !dbg !595 {
prelude:
  %abi_pair_storage5 = alloca [2 x i64], align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  %0 = alloca { ptr, i64 }, align 8
  %1 = alloca { ptr, i64 }, align 8
  %2 = alloca { ptr, i64 }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %3 = load i8, ptr @_root__core__k1__is-static, align 1, !dbg !602
  %4 = trunc i8 %3 to i1, !dbg !602
  br i1 %4, label %arm_cons, label %arm_cons1, !dbg !602

arm_cons:                                         ; preds = %entry
  %5 = call fastcc [2 x i64] @core.platform.port.posix.reserve_1037(i64 %min-size), !dbg !602
  store [2 x i64] %5, ptr %abi_pair_storage5, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %abi_pair_storage5, i64 16, i1 false), !dbg !602
  br label %match_end, !dbg !602

arm_cons1:                                        ; preds = %entry
  %6 = call fastcc [2 x i64] @core.platform.port.posix.reserve_1037(i64 %min-size), !dbg !602
  store [2 x i64] %6, ptr %abi_pair_storage, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !602
  br label %match_end, !dbg !602

match_end:                                        ; preds = %arm_cons, %arm_cons1
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %0, i64 16, i1 false), !dbg !602
  %7 = load ptr, ptr %2, align 8, !dbg !603
  %8 = getelementptr inbounds nuw { ptr, i64 }, ptr %2, i32 0, i32 1, !dbg !604
  %9 = load i64, ptr %8, align 8, !dbg !604
  %10 = load i8, ptr @_root__core__k1__is-static, align 1, !dbg !605
  %11 = trunc i8 %10 to i1, !dbg !605
  br i1 %11, label %arm_cons2, label %arm_cons3, !dbg !605

arm_cons2:                                        ; preds = %match_end
  call fastcc void @core.platform.port.posix.commit_1038(ptr %7, i64 %9), !dbg !605
  br label %match_end4, !dbg !605

arm_cons3:                                        ; preds = %match_end
  call fastcc void @core.platform.port.posix.commit_1038(ptr %7, i64 %9), !dbg !605
  br label %match_end4, !dbg !605

match_end4:                                       ; preds = %arm_cons2, %arm_cons3
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %2, i64 16, i1 false), !dbg !606
  %12 = load [2 x i64], ptr %1, align 8, !dbg !606
  ret [2 x i64] %12, !dbg !606
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.platform.port.posix.reserve_1037(i64 %min-size) #0 !dbg !607 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %0 = alloca { ptr, i64 }, align 8
  %1 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %2 = alloca ptr, align 8
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  br label %entry

entry:                                            ; preds = %prelude
  %5 = load i8, ptr @_root__core__k1__is-static, align 1, !dbg !608
  %6 = trunc i8 %5 to i1, !dbg !609
  br i1 %6, label %arm_cons, label %arm_cons1, !dbg !609

arm_cons:                                         ; preds = %entry
  store i32 4096, ptr %4, align 4, !dbg !609
  br label %match_end, !dbg !609

arm_cons1:                                        ; preds = %entry
  %7 = load i32, ptr @_root__core__platform__port__posix__c__MAP_ANON, align 4, !dbg !610
  store i32 %7, ptr %4, align 4, !dbg !609
  br label %match_end, !dbg !609

match_end:                                        ; preds = %arm_cons, %arm_cons1
  %8 = load i32, ptr @_root__core__platform__port__posix__c__MAP_PRIVATE, align 4, !dbg !611
  %9 = load i32, ptr %4, align 4, !dbg !612
  %10 = or i32 %8, %9, !dbg !613
  store i32 %10, ptr %3, align 4, !dbg !613
  %11 = load ptr, ptr @_root__core__ptr__null, align 8, !dbg !614
  %12 = load i32, ptr @_root__core__platform__port__posix__c__PROT_NONE, align 4, !dbg !615
  %13 = load i32, ptr %3, align 4, !dbg !616
  %14 = call ptr @mmap(ptr %11, i64 %min-size, i32 %12, i32 %13, i32 -1, i64 0), !dbg !617
  store ptr %14, ptr %2, align 8, !dbg !617
  %15 = load ptr, ptr %2, align 8, !dbg !618
  %16 = ptrtoint ptr %15 to i64, !dbg !618
  %17 = load i64, ptr @_root__core__platform__port__posix__c__MAP_FAILED, align 8, !dbg !619
  %18 = icmp eq i64 %16, %17, !dbg !618
  %19 = zext i1 %18 to i8, !dbg !618
  %20 = trunc i8 %19 to i1, !dbg !620
  br i1 %20, label %arm_cons2, label %match_end3, !dbg !620

arm_cons2:                                        ; preds = %match_end
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 @static_255, i64 16, i1 false), !dbg !621
  %21 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %1, i32 0, i32 1, !dbg !621
  store i64 541, ptr %21, align 8, !dbg !621
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %1, i64 24, i1 false)
  %22 = load [2 x i64], ptr @static_257, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %22), !dbg !621
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !621
  unreachable, !dbg !621

match_end3:                                       ; preds = %match_end
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %2, i64 8, i1 false), !dbg !622
  %23 = getelementptr inbounds nuw { ptr, i64 }, ptr %0, i32 0, i32 1, !dbg !623
  store i64 %min-size, ptr %23, align 8, !dbg !624
  %24 = load [2 x i64], ptr %0, align 8, !dbg !623
  ret [2 x i64] %24, !dbg !623
}

; Function Attrs: nounwind
define internal fastcc void @core.platform.port.posix.commit_1038(ptr %base, i64 %len) #0 !dbg !625 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %0 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %4 = call i32 @getpagesize(), !dbg !629
  %5 = sext i32 %4 to i64, !dbg !629
  store i64 %5, ptr %3, align 8, !dbg !629
  %6 = ptrtoint ptr %base to i64, !dbg !630
  %7 = load i64, ptr %3, align 8, !dbg !631
  %8 = sdiv i64 %6, %7, !dbg !630
  %9 = load i64, ptr %3, align 8, !dbg !632
  %10 = mul i64 %8, %9, !dbg !630
  store i64 %10, ptr %2, align 8, !dbg !630
  %11 = ptrtoint ptr %base to i64, !dbg !633
  %12 = add i64 %11, %len, !dbg !633
  %13 = load i64, ptr %3, align 8, !dbg !634
  %14 = add i64 %12, %13, !dbg !633
  %15 = sub i64 %14, 1, !dbg !635
  %16 = load i64, ptr %3, align 8, !dbg !636
  %17 = sdiv i64 %15, %16, !dbg !635
  %18 = load i64, ptr %3, align 8, !dbg !637
  %19 = mul i64 %17, %18, !dbg !635
  store i64 %19, ptr %1, align 8, !dbg !635
  %20 = load i64, ptr %2, align 8, !dbg !638
  %21 = inttoptr i64 %20 to ptr, !dbg !638
  %22 = load i64, ptr %1, align 8, !dbg !639
  %23 = load i64, ptr %2, align 8, !dbg !640
  %24 = sub i64 %22, %23, !dbg !639
  %25 = load i32, ptr @_root__core__platform__port__posix__c__PROT_READ, align 4, !dbg !641
  %26 = load i32, ptr @_root__core__platform__port__posix__c__PROT_WRITE, align 4, !dbg !642
  %27 = or i32 %25, %26, !dbg !643
  %28 = call i32 @mprotect(ptr %21, i64 %24, i32 %27), !dbg !644
  %29 = icmp eq i32 %28, 0, !dbg !644
  %30 = zext i1 %29 to i8, !dbg !644
  %31 = trunc i8 %30 to i1, !dbg !644
  %32 = xor i1 %31, true, !dbg !644
  %33 = zext i1 %32 to i8, !dbg !644
  %34 = trunc i8 %33 to i1, !dbg !645
  br i1 %34, label %arm_cons, label %match_end, !dbg !645

arm_cons:                                         ; preds = %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @static_255, i64 16, i1 false), !dbg !646
  %35 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %0, i32 0, i32 1, !dbg !646
  store i64 551, ptr %35, align 8, !dbg !646
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %0, i64 24, i1 false)
  %36 = load [2 x i64], ptr @static_260, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %36), !dbg !646
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !646
  unreachable, !dbg !646

match_end:                                        ; preds = %entry
  ret void, !dbg !645
}

; Function Attrs: nounwind
declare !dbg !647 i32 @getpagesize() #0

; Function Attrs: nounwind
declare !dbg !648 i32 @mprotect(ptr, i64, i32) #0

; Function Attrs: nounwind
declare !dbg !651 ptr @mmap(ptr, i64, i32, i32, i32, i64) #0

; Function Attrs: nounwind
define internal fastcc void @core.mem.init-tmp-arena_925() #0 !dbg !654 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  %0 = alloca ptr, align 8
  %1 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %2 = alloca ptr, align 8
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 @_root__core__mem__arena-tmp, i64 8, i1 false), !dbg !658
  %3 = load ptr, ptr %2, align 8, !dbg !658
  %4 = ptrtoint ptr %3 to i64, !dbg !659
  %5 = icmp eq i64 %4, 0, !dbg !659
  %6 = zext i1 %5 to i8, !dbg !659
  %7 = trunc i8 %6 to i1, !dbg !660
  br i1 %7, label %arm_cons1, label %arm_cons2, !dbg !660

arm_cons:                                         ; preds = %match_end3
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 @static_158, i64 16, i1 false), !dbg !661
  %8 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %1, i32 0, i32 1, !dbg !661
  store i64 86, ptr %8, align 8, !dbg !661
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %1, i64 24, i1 false)
  %9 = load [2 x i64], ptr @static_180, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %9), !dbg !661
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !661
  unreachable, !dbg !661

match_end:                                        ; preds = %match_end3
  %10 = call fastcc [2 x i64] @core.platform.memory.acquire_953(i64 1073741824), !dbg !662
  store [2 x i64] %10, ptr %abi_pair_storage, align 8
  %11 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !662
  %12 = call fastcc ptr @core.arena.store-header_342([2 x i64] %11), !dbg !662
  store ptr %12, ptr %0, align 8, !dbg !662
  %13 = load ptr, ptr %0, align 8, !dbg !662
  %14 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %13, i32 0, i32 6, !dbg !662
  store i8 1, ptr %14, align 1, !dbg !662
  %15 = load ptr, ptr %0, align 8, !dbg !662
  store ptr %15, ptr @_root__core__mem__arena-tmp, align 8, !dbg !663
  ret void, !dbg !663

arm_cons1:                                        ; preds = %entry
  br label %match_end3, !dbg !660

arm_cons2:                                        ; preds = %entry
  br label %match_end3, !dbg !660

match_end3:                                       ; preds = %arm_cons1, %arm_cons2
  %16 = phi i8 [ 1, %arm_cons1 ], [ 0, %arm_cons2 ], !dbg !660
  %17 = trunc i8 %16 to i1, !dbg !664
  %18 = xor i1 %17, true, !dbg !664
  %19 = zext i1 %18 to i8, !dbg !664
  %20 = trunc i8 %19 to i1, !dbg !665
  br i1 %20, label %arm_cons, label %match_end, !dbg !665
}

; Function Attrs: nounwind
define internal fastcc ptr @core.arena.store-header_342([2 x i64] %region) #0 !dbg !666 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %0 = alloca { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, align 8
  %1 = alloca ptr, align 8
  %2 = alloca ptr, align 8
  %3 = alloca ptr, align 8
  %4 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %region, ptr %abi_pair_storage, align 8, !dbg !669
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 @static_104, i64 16, i1 false), !dbg !670
  %5 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %4, i32 0, i32 1, !dbg !670
  store i64 40, ptr %5, align 8, !dbg !670
  %6 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !671
  %7 = load i64, ptr %6, align 8, !dbg !671
  %8 = icmp sge i64 %7, 144, !dbg !671
  %9 = zext i1 %8 to i8, !dbg !671
  %10 = trunc i8 %9 to i1, !dbg !670
  %11 = xor i1 %10, true, !dbg !670
  %12 = zext i1 %11 to i8, !dbg !670
  %13 = trunc i8 %12 to i1, !dbg !670
  br i1 %13, label %arm_cons, label %entry1, !dbg !670

arm_cons:                                         ; preds = %entry
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %4, i64 24, i1 false)
  %14 = load [2 x i64], ptr @static_64, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %14), !dbg !670
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !670
  unreachable, !dbg !670

entry1:                                           ; preds = %entry
  %15 = load ptr, ptr %abi_pair_storage, align 8, !dbg !672
  %16 = getelementptr inbounds i8, ptr %15, i64 72, !dbg !672
  store ptr %16, ptr %3, align 8, !dbg !672
  %17 = load ptr, ptr %abi_pair_storage, align 8, !dbg !673
  %18 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !674
  %19 = load i64, ptr %18, align 8, !dbg !674
  %20 = getelementptr inbounds i8, ptr %17, i64 %19, !dbg !673
  store ptr %20, ptr %2, align 8, !dbg !673
  %21 = load ptr, ptr %abi_pair_storage, align 8, !dbg !675
  store ptr %21, ptr %1, align 8, !dbg !675
  %22 = load ptr, ptr %1, align 8, !dbg !676
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %3, i64 8, i1 false), !dbg !677
  %23 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %0, i32 0, i32 1, !dbg !678
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %23, ptr align 8 %3, i64 8, i1 false), !dbg !679
  %24 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %0, i32 0, i32 2, !dbg !678
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %24, ptr align 8 %2, i64 8, i1 false), !dbg !680
  %25 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %0, i32 0, i32 3, !dbg !678
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %25, ptr align 8 %3, i64 8, i1 false), !dbg !681
  %26 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %0, i32 0, i32 4, !dbg !678
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %26, ptr align 8 %2, i64 8, i1 false), !dbg !682
  %27 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %0, i32 0, i32 5, !dbg !678
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %27, ptr align 8 @_root__core__ptr__null, i64 8, i1 false), !dbg !683
  %28 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %0, i32 0, i32 6, !dbg !678
  store i8 0, ptr %28, align 1, !dbg !684
  %29 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %0, i32 0, i32 7, !dbg !678
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %29, ptr align 8 @static_107, i64 16, i1 false), !dbg !685
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %22, ptr align 8 %0, i64 72, i1 false), !dbg !676
  %30 = load ptr, ptr %1, align 8, !dbg !686
  ret ptr %30, !dbg !686
}

; Function Attrs: nounwind
define internal fastcc void @core.buffer.check-bounds_1868([2 x i64] %self, i64 %index) #0 !dbg !687 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %0 = alloca { { { ptr, i64 }, i64 } }, align 8
  %1 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !691
  br label %entry

entry:                                            ; preds = %prelude
  %2 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !692
  %3 = load i64, ptr %2, align 8, !dbg !692
  %4 = icmp uge i64 %index, %3, !dbg !693
  %5 = zext i1 %4 to i8, !dbg !693
  %6 = trunc i8 %5 to i1, !dbg !694
  br i1 %6, label %arm_cons, label %match_end, !dbg !694

arm_cons:                                         ; preds = %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 @static_126, i64 16, i1 false), !dbg !695
  %7 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %1, i32 0, i32 1, !dbg !695
  store i64 170, ptr %7, align 8, !dbg !695
  call fastcc void @core.string-builder.new_1255(ptr noalias sret({ { { ptr, i64 }, i64 } }) align 8 dereferenceable(24) %0), !dbg !696
  %8 = load [2 x i64], ptr @static_133, align 8, !dbg !696
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %0, [2 x i64] %8), !dbg !696
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %index, ptr %0), !dbg !697
  %9 = load [2 x i64], ptr @static_134, align 8, !dbg !696
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %0, [2 x i64] %9), !dbg !696
  %10 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !698
  %11 = load i64, ptr %10, align 8, !dbg !698
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %11, ptr %0), !dbg !698
  %12 = call fastcc [2 x i64] @core.string-builder.build-tmp_1259(ptr %0), !dbg !696
  store [2 x i64] %12, ptr %abi_pair_storage1, align 8
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %1, i64 24, i1 false)
  %13 = load [2 x i64], ptr %abi_pair_storage1, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %13), !dbg !695
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !695
  unreachable, !dbg !695

match_end:                                        ; preds = %entry
  ret void, !dbg !694
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.impl_23as-span.last_for_t3148_1809(ptr %self) #0 !dbg !699 {
prelude:
  %abi_pair_storage = alloca [2 x i64], align 8
  %0 = alloca { ptr, i64 }, align 8
  %1 = alloca ptr, align 8
  %2 = alloca { i8, { i64 } }, align 8
  %3 = alloca { i8, { i64 } }, align 8
  %4 = alloca { i8, { i64 } }, align 8
  %5 = alloca { ptr, i64 }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %6 = call fastcc [2 x i64] @core.impl_23as-span.as-span_for_t3148_1802(ptr %self), !dbg !707
  store [2 x i64] %6, ptr %abi_pair_storage, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !707
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %0, i64 16, i1 false), !dbg !707
  %7 = getelementptr inbounds nuw { ptr, i64 }, ptr %5, i32 0, i32 1, !dbg !708
  %8 = load i64, ptr %7, align 8, !dbg !708
  %9 = icmp eq i64 %8, 0, !dbg !708
  %10 = zext i1 %9 to i8, !dbg !708
  %11 = trunc i8 %10 to i1, !dbg !709
  br i1 %11, label %arm_cons, label %arm_cons1, !dbg !709

arm_cons:                                         ; preds = %entry
  store i8 0, ptr %3, align 1, !dbg !710
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %3, i64 16, i1 false), !dbg !709
  br label %match_end, !dbg !709

arm_cons1:                                        ; preds = %entry
  store i8 1, ptr %2, align 1, !dbg !711
  %12 = getelementptr inbounds nuw { i8, { i64 } }, ptr %2, i32 0, i32 1, !dbg !711
  %13 = getelementptr inbounds nuw { ptr, i64 }, ptr %5, i32 0, i32 1, !dbg !712
  %14 = load i64, ptr %13, align 8, !dbg !712
  %15 = sub i64 %14, 1, !dbg !712
  %16 = load [2 x i64], ptr %5, align 8, !dbg !713
  call fastcc void @core.buffer.check-bounds_1827([2 x i64] %16, i64 %15), !dbg !713
  %17 = load ptr, ptr %5, align 8, !dbg !713
  %18 = getelementptr inbounds ptr, ptr %17, i64 %15, !dbg !713
  store ptr %18, ptr %1, align 8, !dbg !713
  %19 = load ptr, ptr %1, align 8, !dbg !713
  %20 = load ptr, ptr %19, align 8, !dbg !713
  store ptr %20, ptr %12, align 8, !dbg !713
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %2, i64 16, i1 false), !dbg !709
  br label %match_end, !dbg !709

match_end:                                        ; preds = %arm_cons, %arm_cons1
  %21 = load [2 x i64], ptr %4, align 8, !dbg !709
  ret [2 x i64] %21, !dbg !709
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.impl_23as-span.as-span_for_t3148_1802(ptr %self) #0 !dbg !714 {
prelude:
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  %0 = alloca { { ptr, i64 } }, align 8
  %1 = alloca { ptr, i64 }, align 8
  %2 = alloca { { ptr, i64 } }, align 8
  %3 = alloca { ptr, i64 }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %4 = call fastcc [2 x i64] @core.impl_22as-buffer.as-buffer_for_t3189_1832(ptr %self), !dbg !724
  store [2 x i64] %4, ptr %abi_pair_storage, align 8
  %5 = getelementptr inbounds nuw { [64 x ptr], i32 }, ptr %self, i32 0, i32 1, !dbg !724
  %6 = load i32, ptr %5, align 4, !dbg !724
  %7 = zext i32 %6 to i64, !dbg !724
  %8 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !724
  %9 = call fastcc [2 x i64] @core.buffer.slice_1843([2 x i64] %8, i64 0, i64 %7), !dbg !724
  store [2 x i64] %9, ptr %abi_pair_storage1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %abi_pair_storage1, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %1, i64 16, i1 false), !dbg !724
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %3, i64 16, i1 false), !dbg !725
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %2, i64 16, i1 false), !dbg !725
  %10 = load [2 x i64], ptr %0, align 8, !dbg !725
  ret [2 x i64] %10, !dbg !725
}

; Function Attrs: nounwind
define internal fastcc void @core.buffer.check-bounds_1827([2 x i64] %self, i64 %index) #0 !dbg !726 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage3 = alloca [2 x i64], align 8
  %abi_pair_storage2 = alloca [2 x i64], align 8
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %0 = alloca { { { ptr, i64 }, i64 } }, align 8
  %1 = alloca { { ptr, i64 }, i64 }, align 8
  %2 = alloca { { ptr, i64 }, i64 }, align 8
  %3 = alloca { ptr, i64 }, align 8
  %4 = alloca { ptr, i64 }, align 8
  %5 = alloca { { { ptr, i64 }, i64 } }, align 8
  %6 = alloca { { ptr, i64 }, i64 }, align 8
  %7 = alloca { { { ptr, i64 } } }, align 8
  %8 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %9 = alloca { { ptr, i64 } }, align 8
  %10 = alloca { { { ptr, i64 } } }, align 8
  %11 = alloca { ptr, i64 }, align 8
  %12 = alloca { ptr, i64 }, align 8
  %13 = alloca { { ptr, i64 } }, align 8
  %14 = alloca { { ptr, i64 } }, align 8
  %15 = alloca { { { ptr, i64 } } }, align 8
  %16 = alloca { ptr, i64 }, align 8
  %17 = alloca { { ptr, i64 } }, align 8
  %18 = alloca { ptr, i64 }, align 8
  %19 = alloca { ptr, i64 }, align 8
  %20 = alloca { { ptr, i64 } }, align 8
  %21 = alloca { { { ptr, i64 } } }, align 8
  %22 = alloca { ptr, i64 }, align 8
  %23 = alloca { ptr, i64 }, align 8
  %24 = alloca { { ptr, i64 } }, align 8
  %25 = alloca { ptr, i64 }, align 8
  %26 = alloca { ptr, i64 }, align 8
  %27 = alloca { ptr, i64 }, align 8
  %28 = alloca { { { ptr, i64 } } }, align 8
  %29 = alloca { { { ptr, i64 } } }, align 8
  %30 = alloca { { ptr, i64 } }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !730
  br label %entry

entry:                                            ; preds = %prelude
  %31 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !731
  %32 = load i64, ptr %31, align 8, !dbg !731
  %33 = icmp uge i64 %index, %32, !dbg !732
  %34 = zext i1 %33 to i8, !dbg !732
  %35 = trunc i8 %34 to i1, !dbg !733
  br i1 %35, label %arm_cons, label %match_end, !dbg !733

arm_cons:                                         ; preds = %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %8, ptr align 8 @static_126, i64 16, i1 false), !dbg !734
  %36 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %8, i32 0, i32 1, !dbg !734
  store i64 170, ptr %36, align 8, !dbg !734
  %37 = call fastcc ptr @core.mem.current-arena_924(), !dbg !735
  %38 = call fastcc [2 x i64] @core.buffer.allocate-in_1786(ptr %37, i64 0), !dbg !735
  store [2 x i64] %38, ptr %abi_pair_storage1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %abi_pair_storage1, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %4, i64 16, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %3, i64 16, i1 false), !dbg !735
  %39 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %2, i32 0, i32 1, !dbg !735
  store i64 0, ptr %39, align 8, !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %2, i64 24, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %1, i64 24, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %0, i64 24, i1 false), !dbg !735
  %40 = load [2 x i64], ptr @static_133, align 8, !dbg !735
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %5, [2 x i64] %40), !dbg !735
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %index, ptr %5), !dbg !736
  %41 = load [2 x i64], ptr @static_134, align 8, !dbg !735
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %5, [2 x i64] %41), !dbg !735
  %42 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !737
  %43 = load i64, ptr %42, align 8, !dbg !737
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %43, ptr %5), !dbg !737
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %5, i64 24, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %11, ptr align 8 %6, i64 16, i1 false), !dbg !735
  %44 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %6, i32 0, i32 1, !dbg !735
  %45 = load i64, ptr %44, align 8, !dbg !735
  %46 = load [2 x i64], ptr %11, align 8, !dbg !735
  %47 = call fastcc [2 x i64] @core.buffer.slice_1776([2 x i64] %46, i64 0, i64 %45), !dbg !735
  store [2 x i64] %47, ptr %abi_pair_storage2, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %12, ptr align 8 %abi_pair_storage2, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %22, ptr align 8 %12, i64 16, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %13, ptr align 8 %22, i64 16, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %9, ptr align 8 %13, i64 16, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %14, ptr align 8 %9, i64 16, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %16, ptr align 8 %14, i64 16, i1 false), !dbg !735
  %48 = load ptr, ptr %16, align 8, !dbg !735
  %49 = getelementptr inbounds nuw { ptr, i64 }, ptr %16, i32 0, i32 1, !dbg !735
  %50 = load i64, ptr %49, align 8, !dbg !735
  store ptr %48, ptr %19, align 8, !dbg !735
  %51 = getelementptr inbounds nuw { ptr, i64 }, ptr %19, i32 0, i32 1, !dbg !735
  store i64 %50, ptr %51, align 8, !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %18, ptr align 8 %19, i64 16, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %17, ptr align 8 %18, i64 16, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %15, ptr align 8 %17, i64 16, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %29, ptr align 8 %15, i64 16, i1 false), !dbg !735
  %52 = call fastcc ptr @core.mem.tmp_926(), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %20, ptr align 8 %29, i64 16, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %23, ptr align 8 %20, i64 16, i1 false), !dbg !735
  %53 = getelementptr inbounds nuw { ptr, i64 }, ptr %23, i32 0, i32 1, !dbg !735
  %54 = load i64, ptr %53, align 8, !dbg !735
  %55 = call fastcc [2 x i64] @core.buffer.allocate-in_1861(ptr %52, i64 %54), !dbg !735
  store [2 x i64] %55, ptr %abi_pair_storage3, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %27, ptr align 8 %abi_pair_storage3, i64 16, i1 false)
  %56 = load [2 x i64], ptr %23, align 8, !dbg !735
  %57 = load [2 x i64], ptr %27, align 8, !dbg !735
  call fastcc void @core.buffer.copy_1862([2 x i64] %56, [2 x i64] %57), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %26, ptr align 8 %27, i64 16, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %25, ptr align 8 %26, i64 16, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %24, ptr align 8 %25, i64 16, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %30, ptr align 8 %24, i64 16, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %28, ptr align 8 %30, i64 16, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %21, ptr align 8 %28, i64 16, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 8 %21, i64 16, i1 false), !dbg !735
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %10, ptr align 8 %7, i64 16, i1 false), !dbg !735
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %8, i64 24, i1 false)
  %58 = load [2 x i64], ptr %10, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %58), !dbg !734
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !734
  unreachable, !dbg !734

match_end:                                        ; preds = %entry
  ret void, !dbg !733
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.impl_22as-buffer.as-buffer_for_t3189_1832(ptr %self) #0 !dbg !738 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %0 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %1 = alloca { ptr, i64 }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  br i1 true, label %match_end, label %arm_cons, !dbg !741

arm_cons:                                         ; preds = %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @static_26, i64 16, i1 false), !dbg !741
  %2 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %0, i32 0, i32 1, !dbg !741
  store i64 985, ptr %2, align 8, !dbg !741
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %0, i64 24, i1 false)
  %3 = load [2 x i64], ptr @static_53, align 8
  call fastcc void @core.crash-bounds_336(ptr %abi_caller_copy, i64 64, i64 0, [2 x i64] %3), !dbg !741
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !741
  unreachable, !dbg !741

match_end:                                        ; preds = %entry
  %4 = getelementptr inbounds ptr, ptr %self, i64 0, !dbg !741
  store ptr %4, ptr %1, align 8, !dbg !741
  %5 = getelementptr inbounds nuw { ptr, i64 }, ptr %1, i32 0, i32 1, !dbg !742
  store i64 64, ptr %5, align 8, !dbg !743
  %6 = load [2 x i64], ptr %1, align 8, !dbg !742
  ret [2 x i64] %6, !dbg !742
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.buffer.slice_1843([2 x i64] %self, i64 %start, i64 %end) #0 !dbg !744 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage13 = alloca [2 x i64], align 8
  %abi_pair_storage12 = alloca [2 x i64], align 8
  %abi_pair_storage11 = alloca [2 x i64], align 8
  %0 = alloca { { { ptr, i64 }, i64 } }, align 8
  %1 = alloca { { ptr, i64 }, i64 }, align 8
  %2 = alloca { { ptr, i64 }, i64 }, align 8
  %3 = alloca { ptr, i64 }, align 8
  %4 = alloca { ptr, i64 }, align 8
  %5 = alloca ptr, align 8
  %6 = alloca { { ptr, i64 }, i64 }, align 8
  %7 = alloca { { { ptr, i64 } } }, align 8
  %8 = alloca i64, align 8
  %9 = alloca { { ptr, i64 } }, align 8
  %10 = alloca { ptr, i64 }, align 8
  %11 = alloca { ptr, i64 }, align 8
  %12 = alloca { ptr, i64 }, align 8
  %13 = alloca { { ptr, i64 } }, align 8
  %14 = alloca i64, align 8
  %15 = alloca { { { ptr, i64 } } }, align 8
  %16 = alloca { ptr, i64 }, align 8
  %17 = alloca { { ptr, i64 } }, align 8
  %18 = alloca { ptr, i64 }, align 8
  %19 = alloca { ptr, i64 }, align 8
  %20 = alloca { { ptr, i64 } }, align 8
  %21 = alloca { { { ptr, i64 } } }, align 8
  %22 = alloca { { { ptr, i64 }, i64 } }, align 8
  %23 = alloca { ptr, i64 }, align 8
  %24 = alloca { { ptr, i64 } }, align 8
  %25 = alloca { ptr, i64 }, align 8
  %26 = alloca { ptr, i64 }, align 8
  %27 = alloca { ptr, i64 }, align 8
  %28 = alloca { { { ptr, i64 } } }, align 8
  %29 = alloca { ptr, i64 }, align 8
  %30 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %31 = alloca { { { ptr, i64 } } }, align 8
  %32 = alloca { { ptr, i64 } }, align 8
  %33 = alloca { ptr, i64 }, align 8
  %34 = alloca { { { ptr, i64 } } }, align 8
  %35 = alloca { { ptr, i64 } }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !747
  br label %entry

entry:                                            ; preds = %prelude
  %36 = icmp slt i64 %start, 0, !dbg !748
  %37 = zext i1 %36 to i8, !dbg !748
  %38 = trunc i8 %37 to i1, !dbg !748
  br i1 %38, label %arm_cons1, label %arm_cons2, !dbg !748

arm_cons:                                         ; preds = %match_end
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %30, ptr align 8 @static_126, i64 16, i1 false), !dbg !749
  %39 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %30, i32 0, i32 1, !dbg !749
  store i64 199, ptr %39, align 8, !dbg !749
  %40 = call fastcc ptr @core.mem.current-arena_924(), !dbg !750
  %41 = call fastcc [2 x i64] @core.buffer.allocate-in_1786(ptr %40, i64 0), !dbg !750
  store [2 x i64] %41, ptr %abi_pair_storage11, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %abi_pair_storage11, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %4, i64 16, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %3, i64 16, i1 false), !dbg !750
  %42 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %2, i32 0, i32 1, !dbg !750
  store i64 0, ptr %42, align 8, !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %2, i64 24, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %1, i64 24, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %22, ptr align 8 %0, i64 24, i1 false), !dbg !750
  %43 = load [2 x i64], ptr @static_136, align 8, !dbg !750
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %22, [2 x i64] %43), !dbg !750
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %start, ptr %22), !dbg !751
  %44 = load [2 x i64], ptr @static_137, align 8, !dbg !750
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %22, [2 x i64] %44), !dbg !750
  %45 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !752
  %46 = load i64, ptr %45, align 8, !dbg !752
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %46, ptr %22), !dbg !752
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %22, i64 24, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %11, ptr align 8 %6, i64 16, i1 false), !dbg !750
  %47 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %6, i32 0, i32 1, !dbg !750
  %48 = load i64, ptr %47, align 8, !dbg !750
  %49 = load [2 x i64], ptr %11, align 8, !dbg !750
  %50 = call fastcc [2 x i64] @core.buffer.slice_1776([2 x i64] %49, i64 0, i64 %48), !dbg !750
  store [2 x i64] %50, ptr %abi_pair_storage12, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %12, ptr align 8 %abi_pair_storage12, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %33, ptr align 8 %12, i64 16, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %13, ptr align 8 %33, i64 16, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %9, ptr align 8 %13, i64 16, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %32, ptr align 8 %9, i64 16, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %16, ptr align 8 %32, i64 16, i1 false), !dbg !750
  %51 = load ptr, ptr %16, align 8, !dbg !750
  %52 = getelementptr inbounds nuw { ptr, i64 }, ptr %16, i32 0, i32 1, !dbg !750
  %53 = load i64, ptr %52, align 8, !dbg !750
  store ptr %51, ptr %19, align 8, !dbg !750
  %54 = getelementptr inbounds nuw { ptr, i64 }, ptr %19, i32 0, i32 1, !dbg !750
  store i64 %53, ptr %54, align 8, !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %18, ptr align 8 %19, i64 16, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %17, ptr align 8 %18, i64 16, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %15, ptr align 8 %17, i64 16, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %34, ptr align 8 %15, i64 16, i1 false), !dbg !750
  %55 = call fastcc ptr @core.mem.tmp_926(), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %20, ptr align 8 %34, i64 16, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %23, ptr align 8 %20, i64 16, i1 false), !dbg !750
  %56 = getelementptr inbounds nuw { ptr, i64 }, ptr %23, i32 0, i32 1, !dbg !750
  %57 = load i64, ptr %56, align 8, !dbg !750
  %58 = call fastcc [2 x i64] @core.buffer.allocate-in_1861(ptr %55, i64 %57), !dbg !750
  store [2 x i64] %58, ptr %abi_pair_storage13, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %27, ptr align 8 %abi_pair_storage13, i64 16, i1 false)
  %59 = load [2 x i64], ptr %23, align 8, !dbg !750
  %60 = load [2 x i64], ptr %27, align 8, !dbg !750
  call fastcc void @core.buffer.copy_1862([2 x i64] %59, [2 x i64] %60), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %26, ptr align 8 %27, i64 16, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %25, ptr align 8 %26, i64 16, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %24, ptr align 8 %25, i64 16, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %35, ptr align 8 %24, i64 16, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %28, ptr align 8 %35, i64 16, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %21, ptr align 8 %28, i64 16, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 8 %21, i64 16, i1 false), !dbg !750
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %31, ptr align 8 %7, i64 16, i1 false), !dbg !750
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %30, i64 24, i1 false)
  %61 = load [2 x i64], ptr %31, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %61), !dbg !749
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !749
  unreachable, !dbg !749

arm_cons1:                                        ; preds = %entry
  br label %match_end, !dbg !748

arm_cons2:                                        ; preds = %entry
  %62 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !753
  %63 = load i64, ptr %62, align 8, !dbg !753
  %64 = icmp sgt i64 %start, %63, !dbg !754
  %65 = zext i1 %64 to i8, !dbg !754
  br label %match_end, !dbg !748

match_end:                                        ; preds = %arm_cons1, %arm_cons2
  %66 = phi i8 [ 1, %arm_cons1 ], [ %65, %arm_cons2 ], !dbg !748
  %67 = trunc i8 %66 to i1, !dbg !755
  br i1 %67, label %arm_cons, label %arm_cond, !dbg !755

arm_cond:                                         ; preds = %match_end
  %68 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !756
  %69 = load i64, ptr %68, align 8, !dbg !756
  %70 = icmp sgt i64 %end, %69, !dbg !757
  %71 = zext i1 %70 to i8, !dbg !757
  %72 = trunc i8 %71 to i1, !dbg !758
  br i1 %72, label %arm_cons3, label %arm_cons4, !dbg !758

arm_cons3:                                        ; preds = %arm_cond
  %73 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !759
  %74 = load i64, ptr %73, align 8, !dbg !759
  store i64 %74, ptr %14, align 8, !dbg !758
  br label %match_end5, !dbg !758

arm_cons4:                                        ; preds = %arm_cond
  store i64 %end, ptr %14, align 8, !dbg !758
  br label %match_end5, !dbg !758

match_end5:                                       ; preds = %arm_cons3, %arm_cons4
  %75 = load i64, ptr %14, align 8, !dbg !760
  %76 = icmp sle i64 %75, %start, !dbg !760
  %77 = zext i1 %76 to i8, !dbg !760
  %78 = trunc i8 %77 to i1, !dbg !760
  br i1 %78, label %arm_cons8, label %arm_cons9, !dbg !760

entry6:                                           ; preds = %match_end10
  %79 = load ptr, ptr @_root__core__ptr__null, align 8, !dbg !761
  store ptr %79, ptr %29, align 8, !dbg !761
  %80 = getelementptr inbounds nuw { ptr, i64 }, ptr %29, i32 0, i32 1, !dbg !761
  store i64 0, ptr %80, align 8, !dbg !761
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %10, ptr align 8 %29, i64 16, i1 false), !dbg !761
  %81 = load [2 x i64], ptr %10, align 8, !dbg !762
  ret [2 x i64] %81, !dbg !762

match_end7:                                       ; preds = %match_end10
  %82 = load i64, ptr %14, align 8, !dbg !763
  %83 = sub i64 %82, %start, !dbg !763
  store i64 %83, ptr %8, align 8, !dbg !763
  %84 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !764
  call fastcc void @core.buffer.check-bounds_1827([2 x i64] %84, i64 %start), !dbg !764
  %85 = load ptr, ptr %abi_pair_storage, align 8, !dbg !764
  %86 = getelementptr inbounds ptr, ptr %85, i64 %start, !dbg !764
  store ptr %86, ptr %5, align 8, !dbg !764
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %10, ptr align 8 %5, i64 8, i1 false), !dbg !765
  %87 = getelementptr inbounds nuw { ptr, i64 }, ptr %10, i32 0, i32 1, !dbg !766
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %87, ptr align 8 %8, i64 8, i1 false), !dbg !767
  %88 = load [2 x i64], ptr %10, align 8, !dbg !766
  ret [2 x i64] %88, !dbg !766

arm_cons8:                                        ; preds = %match_end5
  br label %match_end10, !dbg !760

arm_cons9:                                        ; preds = %match_end5
  %89 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !768
  %90 = load i64, ptr %89, align 8, !dbg !768
  %91 = icmp eq i64 %90, 0, !dbg !768
  %92 = zext i1 %91 to i8, !dbg !768
  br label %match_end10, !dbg !760

match_end10:                                      ; preds = %arm_cons8, %arm_cons9
  %93 = phi i8 [ 1, %arm_cons8 ], [ %92, %arm_cons9 ], !dbg !760
  %94 = trunc i8 %93 to i1, !dbg !769
  br i1 %94, label %entry6, label %match_end7, !dbg !769
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.span.as-char-span_1174([2 x i64] %self) #0 !dbg !770 {
prelude:
  %0 = alloca { ptr, i64 }, align 8
  %1 = alloca { ptr, i64 }, align 8
  %2 = alloca { ptr, i64 }, align 8
  %3 = alloca { { ptr, i64 } }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !773
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !774
  %4 = load ptr, ptr %2, align 8, !dbg !774
  %5 = getelementptr inbounds nuw { ptr, i64 }, ptr %2, i32 0, i32 1, !dbg !774
  %6 = load i64, ptr %5, align 8, !dbg !774
  store ptr %4, ptr %0, align 8, !dbg !774
  %7 = getelementptr inbounds nuw { ptr, i64 }, ptr %0, i32 0, i32 1, !dbg !774
  store i64 %6, ptr %7, align 8, !dbg !774
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %0, i64 16, i1 false), !dbg !774
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %1, i64 16, i1 false), !dbg !774
  %8 = load [2 x i64], ptr %3, align 8, !dbg !775
  ret [2 x i64] %8, !dbg !775
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.string.cloned-in_1774([2 x i64] %self, ptr %alloc) #0 !dbg !776 {
prelude:
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %0 = alloca { { ptr, i64 } }, align 8
  %1 = alloca { ptr, i64 }, align 8
  %2 = alloca { ptr, i64 }, align 8
  %3 = alloca { ptr, i64 }, align 8
  %4 = alloca { ptr, i64 }, align 8
  %5 = alloca { { ptr, i64 } }, align 8
  %6 = alloca { { { ptr, i64 } } }, align 8
  %7 = alloca { { { ptr, i64 } } }, align 8
  %8 = alloca { { ptr, i64 } }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !780
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !781
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %0, i64 16, i1 false), !dbg !781
  %9 = getelementptr inbounds nuw { ptr, i64 }, ptr %4, i32 0, i32 1, !dbg !781
  %10 = load i64, ptr %9, align 8, !dbg !781
  %11 = call fastcc [2 x i64] @core.buffer.allocate-in_1861(ptr %alloc, i64 %10), !dbg !781
  store [2 x i64] %11, ptr %abi_pair_storage1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %abi_pair_storage1, i64 16, i1 false)
  %12 = load [2 x i64], ptr %4, align 8, !dbg !781
  %13 = load [2 x i64], ptr %2, align 8, !dbg !781
  call fastcc void @core.buffer.copy_1862([2 x i64] %12, [2 x i64] %13), !dbg !781
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %2, i64 16, i1 false), !dbg !781
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %1, i64 16, i1 false), !dbg !781
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %3, i64 16, i1 false), !dbg !781
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %8, ptr align 8 %5, i64 16, i1 false), !dbg !781
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %8, i64 16, i1 false), !dbg !782
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 8 %6, i64 16, i1 false), !dbg !782
  %14 = load [2 x i64], ptr %7, align 8, !dbg !782
  ret [2 x i64] %14, !dbg !782
}

; Function Attrs: nounwind
define internal fastcc void @core.list.push-in_1941(ptr %self, ptr %alloc, i8 %elem) #0 !dbg !783 {
prelude:
  %0 = alloca { { ptr, i64 }, i64 }, align 8
  %1 = alloca i64, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %2 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %self, i32 0, i32 1, !dbg !788
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %2, i64 8, i1 false), !dbg !788
  %3 = load i64, ptr %1, align 8, !dbg !789
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %self, i64 24, i1 false), !dbg !790
  %4 = getelementptr inbounds nuw { ptr, i64 }, ptr %0, i32 0, i32 1, !dbg !790
  %5 = load i64, ptr %4, align 8, !dbg !790
  %6 = icmp eq i64 %3, %5, !dbg !789
  %7 = zext i1 %6 to i8, !dbg !789
  %8 = trunc i8 %7 to i1, !dbg !791
  br i1 %8, label %arm_cons, label %match_end, !dbg !791

arm_cons:                                         ; preds = %entry
  call fastcc void @core.list._grow_1942(ptr %self, ptr %alloc), !dbg !792
  br label %match_end, !dbg !791

match_end:                                        ; preds = %arm_cons, %entry
  %9 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %self, i32 0, i32 1, !dbg !793
  %10 = load i64, ptr %1, align 8, !dbg !794
  %11 = add i64 %10, 1, !dbg !794
  store i64 %11, ptr %9, align 8, !dbg !793
  %12 = load i64, ptr %1, align 8, !dbg !795
  call fastcc void @core.impl_22as-buffer.set_for_t1665_1944(ptr %self, i64 %12, i8 %elem), !dbg !796
  ret void, !dbg !796
}

; Function Attrs: nounwind
define internal fastcc void @core.format-uint_1879(ptr %w, i64 %value, i64 %base) #0 !dbg !797 {
prelude:
  %abi_pair_storage9 = alloca [2 x i64], align 8
  %abi_pair_storage8 = alloca [2 x i64], align 8
  %abi_pair_storage7 = alloca [2 x i64], align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  %0 = alloca { [64 x i8], i32 }, align 4
  %1 = alloca { ptr, i64 }, align 8
  %2 = alloca { ptr, i64 }, align 8
  %3 = alloca { ptr, i64 }, align 8
  %4 = alloca { ptr, i64 }, align 8
  %5 = alloca i8, align 1
  %6 = alloca i64, align 8
  %7 = alloca { [64 x i8], i32 }, align 4
  %8 = alloca i64, align 8
  %9 = alloca i8, align 1
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i64, align 8
  %13 = alloca i8, align 1
  br label %entry

entry:                                            ; preds = %prelude
  %14 = icmp eq i64 %value, 0, !dbg !800
  %15 = zext i1 %14 to i8, !dbg !800
  %16 = trunc i8 %15 to i1, !dbg !801
  br i1 %16, label %entry1, label %arm_cond, !dbg !801

entry1:                                           ; preds = %entry
  %17 = call fastcc ptr @core.mem.current-arena_924(), !dbg !802
  call fastcc void @core.list.push-in_1941(ptr %w, ptr %17, i8 48), !dbg !802
  ret void, !dbg !803

arm_cond:                                         ; preds = %entry
  %18 = icmp ult i64 %value, %base, !dbg !804
  %19 = zext i1 %18 to i8, !dbg !804
  %20 = trunc i8 %19 to i1, !dbg !805
  br i1 %20, label %arm_cons, label %arm_cond2, !dbg !805

arm_cons:                                         ; preds = %arm_cond
  %21 = trunc i64 %value to i8, !dbg !806
  %22 = call fastcc i8 @core.u8.to-ascii-digit_644(i8 %21), !dbg !807
  store i8 %22, ptr %13, align 1, !dbg !807
  %23 = load i8, ptr %13, align 1, !dbg !808
  %24 = call fastcc ptr @core.mem.current-arena_924(), !dbg !809
  call fastcc void @core.list.push-in_1941(ptr %w, ptr %24, i8 %23), !dbg !809
  br label %match_end, !dbg !805

match_end:                                        ; preds = %arm_cons, %match_end5
  ret void, !dbg !801

arm_cond2:                                        ; preds = %arm_cond
  %25 = mul i64 %base, 2, !dbg !810
  %26 = icmp ult i64 %value, %25, !dbg !811
  %27 = zext i1 %26 to i8, !dbg !811
  %28 = trunc i8 %27 to i1, !dbg !812
  br i1 %28, label %arm_cons3, label %arm_cons4, !dbg !812

arm_cons3:                                        ; preds = %arm_cond2
  %29 = udiv i64 %value, %base, !dbg !813
  store i64 %29, ptr %12, align 8, !dbg !813
  %30 = load i64, ptr %12, align 8, !dbg !814
  %31 = trunc i64 %30 to i8, !dbg !814
  %32 = call fastcc i8 @core.u8.to-ascii-digit_644(i8 %31), !dbg !815
  store i8 %32, ptr %11, align 1, !dbg !815
  %33 = urem i64 %value, %base, !dbg !816
  store i64 %33, ptr %10, align 8, !dbg !816
  %34 = load i64, ptr %10, align 8, !dbg !817
  %35 = trunc i64 %34 to i8, !dbg !817
  %36 = call fastcc i8 @core.u8.to-ascii-digit_644(i8 %35), !dbg !818
  store i8 %36, ptr %9, align 1, !dbg !818
  %37 = load i8, ptr %11, align 1, !dbg !819
  %38 = call fastcc ptr @core.mem.current-arena_924(), !dbg !820
  call fastcc void @core.list.push-in_1941(ptr %w, ptr %38, i8 %37), !dbg !820
  %39 = load i8, ptr %9, align 1, !dbg !821
  %40 = call fastcc ptr @core.mem.current-arena_924(), !dbg !822
  call fastcc void @core.list.push-in_1941(ptr %w, ptr %40, i8 %39), !dbg !822
  br label %match_end5, !dbg !812

arm_cons4:                                        ; preds = %arm_cond2
  store i64 %value, ptr %8, align 8, !dbg !823
  %41 = getelementptr inbounds nuw { [64 x i8], i32 }, ptr %0, i32 0, i32 1, !dbg !824
  store i32 0, ptr %41, align 4, !dbg !824
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %7, ptr align 4 %0, i64 68, i1 false), !dbg !824
  br label %while_loop_condition, !dbg !825

match_end5:                                       ; preds = %arm_cons3, %entry6
  br label %match_end, !dbg !805

while_loop_condition:                             ; preds = %while_loop_body, %arm_cons4
  %42 = load i64, ptr %8, align 8, !dbg !826
  %43 = icmp ugt i64 %42, 0, !dbg !826
  %44 = zext i1 %43 to i8, !dbg !826
  %45 = trunc i8 %44 to i1, !dbg !825
  br i1 %45, label %while_loop_body, label %entry6, !dbg !825

while_loop_body:                                  ; preds = %while_loop_condition
  %46 = load i64, ptr %8, align 8, !dbg !827
  %47 = urem i64 %46, %base, !dbg !827
  store i64 %47, ptr %6, align 8, !dbg !827
  %48 = load i64, ptr %6, align 8, !dbg !828
  %49 = trunc i64 %48 to i8, !dbg !828
  %50 = call fastcc i8 @core.u8.to-ascii-digit_644(i8 %49), !dbg !829
  store i8 %50, ptr %5, align 1, !dbg !829
  %51 = load i8, ptr %5, align 1, !dbg !830
  call fastcc void @core.fixlist.push_1882(ptr %7, i8 %51), !dbg !831
  %52 = load i64, ptr %8, align 8, !dbg !832
  %53 = udiv i64 %52, %base, !dbg !832
  store i64 %53, ptr %8, align 8, !dbg !833
  br label %while_loop_condition, !dbg !825

entry6:                                           ; preds = %while_loop_condition
  %54 = call fastcc [2 x i64] @core.impl_22as-buffer.as-buffer_for_t3265_1928(ptr %7), !dbg !834
  store [2 x i64] %54, ptr %abi_pair_storage, align 8
  %55 = getelementptr inbounds nuw { [64 x i8], i32 }, ptr %7, i32 0, i32 1, !dbg !834
  %56 = load i32, ptr %55, align 4, !dbg !834
  %57 = zext i32 %56 to i64, !dbg !834
  %58 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !834
  %59 = call fastcc [2 x i64] @core.buffer.slice_1938([2 x i64] %58, i64 0, i64 %57), !dbg !834
  store [2 x i64] %59, ptr %abi_pair_storage7, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %abi_pair_storage7, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %2, i64 16, i1 false), !dbg !834
  %60 = load [2 x i64], ptr %1, align 8, !dbg !834
  call fastcc void @core.buffer.reverse_1922([2 x i64] %60), !dbg !834
  %61 = call fastcc [2 x i64] @core.impl_23as-span.as-span_for_t3238_1893(ptr %7), !dbg !835
  store [2 x i64] %61, ptr %abi_pair_storage8, align 8
  %62 = load [2 x i64], ptr %abi_pair_storage8, align 8, !dbg !835
  %63 = call fastcc [2 x i64] @core.span.as-byte-span_1175([2 x i64] %62), !dbg !835
  store [2 x i64] %63, ptr %abi_pair_storage9, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %abi_pair_storage9, i64 16, i1 false), !dbg !836
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %4, i64 16, i1 false), !dbg !836
  %64 = call fastcc ptr @core.mem.current-arena_924(), !dbg !836
  %65 = load [2 x i64], ptr %3, align 8, !dbg !836
  call fastcc void @core.list.append-buffer-in_1873(ptr %w, ptr %64, [2 x i64] %65), !dbg !836
  br label %match_end5, !dbg !812
}

; Function Attrs: nounwind
define internal fastcc void @core.list.append-buffer-in_1873(ptr %self, ptr %alloc, [2 x i64] %other) #0 !dbg !837 {
prelude:
  %abi_pair_storage6 = alloca [2 x i64], align 8
  %0 = alloca { { ptr, i64 }, i64 }, align 8
  %1 = alloca { ptr, i64 }, align 8
  %2 = alloca { { ptr, i64 }, i64 }, align 8
  %3 = alloca { ptr, i64 }, align 8
  %4 = alloca { { ptr, i64 }, i64 }, align 8
  %5 = alloca i64, align 8
  %6 = alloca { { ptr, i64 }, i64 }, align 8
  %7 = alloca i64, align 8
  %8 = alloca { ptr, i64 }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %other, ptr %abi_pair_storage, align 8, !dbg !841
  br label %entry

entry:                                            ; preds = %prelude
  %9 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %self, i32 0, i32 1, !dbg !842
  %10 = load i64, ptr %9, align 8, !dbg !842
  %11 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !843
  %12 = load i64, ptr %11, align 8, !dbg !843
  %13 = add i64 %10, %12, !dbg !842
  store i64 %13, ptr %7, align 8, !dbg !842
  %14 = load i64, ptr %7, align 8, !dbg !844
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %self, i64 24, i1 false), !dbg !844
  %15 = getelementptr inbounds nuw { ptr, i64 }, ptr %6, i32 0, i32 1, !dbg !845
  %16 = load i64, ptr %15, align 8, !dbg !845
  %17 = icmp sgt i64 %14, %16, !dbg !844
  %18 = zext i1 %17 to i8, !dbg !844
  %19 = trunc i8 %18 to i1, !dbg !846
  br i1 %19, label %arm_cons, label %match_end, !dbg !846

arm_cons:                                         ; preds = %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %self, i64 24, i1 false), !dbg !846
  %20 = getelementptr inbounds nuw { ptr, i64 }, ptr %4, i32 0, i32 1, !dbg !847
  %21 = load i64, ptr %20, align 8, !dbg !847
  %22 = mul i64 %21, 2, !dbg !847
  store i64 %22, ptr %5, align 8, !dbg !847
  %23 = load i64, ptr %5, align 8, !dbg !848
  %24 = load i64, ptr %7, align 8, !dbg !849
  %25 = icmp sgt i64 %23, %24, !dbg !848
  %26 = zext i1 %25 to i8, !dbg !848
  %27 = trunc i8 %26 to i1, !dbg !850
  br i1 %27, label %arm_cons1, label %arm_cons2, !dbg !850

match_end:                                        ; preds = %match_end5, %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %self, i64 24, i1 false), !dbg !846
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %0, i64 16, i1 false), !dbg !851
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %8, ptr align 8 %1, i64 16, i1 false), !dbg !851
  %28 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %self, i32 0, i32 1, !dbg !852
  %29 = load i64, ptr %28, align 8, !dbg !852
  %30 = load i64, ptr %7, align 8, !dbg !853
  %31 = load [2 x i64], ptr %8, align 8, !dbg !851
  %32 = call fastcc [2 x i64] @core.buffer.slice_1776([2 x i64] %31, i64 %29, i64 %30), !dbg !851
  store [2 x i64] %32, ptr %abi_pair_storage6, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %abi_pair_storage6, i64 16, i1 false)
  %33 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !854
  %34 = load [2 x i64], ptr %3, align 8, !dbg !854
  call fastcc void @core.buffer.copy_1877([2 x i64] %33, [2 x i64] %34), !dbg !854
  %35 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %self, i32 0, i32 1, !dbg !855
  %36 = load i64, ptr %7, align 8, !dbg !856
  store i64 %36, ptr %35, align 8, !dbg !855
  ret void, !dbg !855

arm_cons1:                                        ; preds = %arm_cons
  %37 = load i64, ptr %5, align 8, !dbg !857
  br label %match_end3, !dbg !850

arm_cons2:                                        ; preds = %arm_cons
  %38 = load i64, ptr %7, align 8, !dbg !858
  br label %match_end3, !dbg !850

match_end3:                                       ; preds = %arm_cons1, %arm_cons2
  %39 = phi i64 [ %37, %arm_cons1 ], [ %38, %arm_cons2 ], !dbg !850
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %self, i64 24, i1 false), !dbg !859
  %40 = getelementptr inbounds nuw { ptr, i64 }, ptr %2, i32 0, i32 1, !dbg !859
  %41 = load i64, ptr %40, align 8, !dbg !859
  %42 = icmp sge i64 %41, %39, !dbg !859
  %43 = zext i1 %42 to i8, !dbg !859
  %44 = trunc i8 %43 to i1, !dbg !859
  br i1 %44, label %match_end5, label %match_end4, !dbg !859

match_end4:                                       ; preds = %match_end3
  call fastcc void @core.buffer._grow-to_1878(ptr %self, ptr %alloc, i64 %39), !dbg !859
  br label %match_end5, !dbg !859

match_end5:                                       ; preds = %match_end4, %match_end3
  br label %match_end, !dbg !846
}

; Function Attrs: nounwind
define internal fastcc void @core.buffer._grow-to_1878(ptr %self, ptr %alloc, i64 %new-count) #0 !dbg !860 {
prelude:
  %0 = alloca { ptr, i64 }, align 8
  %1 = alloca { ptr, i64 }, align 8
  %2 = alloca ptr, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %5 = mul i64 %new-count, 1, !dbg !864
  store i64 %5, ptr %4, align 8, !dbg !864
  %6 = getelementptr inbounds nuw { ptr, i64 }, ptr %self, i32 0, i32 1, !dbg !865
  %7 = load i64, ptr %6, align 8, !dbg !865
  %8 = mul i64 %7, 1, !dbg !866
  store i64 %8, ptr %3, align 8, !dbg !866
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %self, i64 16, i1 false), !dbg !866
  %9 = load ptr, ptr %1, align 8, !dbg !867
  %10 = load i64, ptr %3, align 8, !dbg !868
  %11 = load i64, ptr %4, align 8, !dbg !869
  %12 = call fastcc ptr @core.arena.try-realloc_357(ptr %alloc, ptr %9, i64 %10, i64 %11, i64 1), !dbg !870
  store ptr %12, ptr %2, align 8, !dbg !870
  %13 = load ptr, ptr %2, align 8, !dbg !871
  store ptr %13, ptr %0, align 8, !dbg !871
  %14 = getelementptr inbounds nuw { ptr, i64 }, ptr %0, i32 0, i32 1, !dbg !872
  store i64 %new-count, ptr %14, align 8, !dbg !873
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %0, i64 16, i1 false), !dbg !874
  ret void, !dbg !874
}

; Function Attrs: nounwind
define internal fastcc void @core.buffer.copy_1877([2 x i64] %src, [2 x i64] %dst) #0 !dbg !875 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage4 = alloca [2 x i64], align 8
  %abi_pair_storage3 = alloca [2 x i64], align 8
  %0 = alloca i64, align 8
  %1 = alloca { { ptr, i64 }, i64 }, align 8
  %2 = alloca { { { ptr, i64 } } }, align 8
  %3 = alloca { { { ptr, i64 }, i64 } }, align 8
  %4 = alloca { { ptr, i64 } }, align 8
  %5 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %6 = alloca { ptr, i64 }, align 8
  %7 = alloca { ptr, i64 }, align 8
  %8 = alloca { { ptr, i64 } }, align 8
  %9 = alloca { { { ptr, i64 } } }, align 8
  %10 = alloca { { { ptr, i64 } } }, align 8
  %11 = alloca { ptr, i64 }, align 8
  %12 = alloca { { ptr, i64 } }, align 8
  %13 = alloca { ptr, i64 }, align 8
  %14 = alloca { ptr, i64 }, align 8
  %15 = alloca { { ptr, i64 } }, align 8
  %16 = alloca { { { ptr, i64 } } }, align 8
  %17 = alloca { { ptr, i64 } }, align 8
  %18 = alloca { ptr, i64 }, align 8
  %19 = alloca { { ptr, i64 } }, align 8
  %20 = alloca { ptr, i64 }, align 8
  %21 = alloca { ptr, i64 }, align 8
  %22 = alloca { ptr, i64 }, align 8
  %23 = alloca { { { ptr, i64 } } }, align 8
  %24 = alloca { ptr, i64 }, align 8
  %25 = alloca { { { ptr, i64 } } }, align 8
  %26 = alloca { { ptr, i64 } }, align 8
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %src, ptr %abi_pair_storage, align 8, !dbg !878
  store [2 x i64] %dst, ptr %abi_pair_storage1, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %27 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !879
  %28 = load i64, ptr %27, align 8, !dbg !879
  %29 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage1, i32 0, i32 1, !dbg !880
  %30 = load i64, ptr %29, align 8, !dbg !880
  %31 = icmp sgt i64 %28, %30, !dbg !879
  %32 = zext i1 %31 to i8, !dbg !879
  %33 = trunc i8 %32 to i1, !dbg !881
  br i1 %33, label %arm_cons, label %arm_cond, !dbg !881

arm_cons:                                         ; preds = %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 @static_126, i64 16, i1 false), !dbg !882
  %34 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %5, i32 0, i32 1, !dbg !882
  store i64 124, ptr %34, align 8, !dbg !882
  call fastcc void @core.string-builder.new_1255(ptr noalias sret({ { { ptr, i64 }, i64 } }) align 8 dereferenceable(24) %3), !dbg !883
  %35 = load [2 x i64], ptr @static_129, align 8, !dbg !883
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %3, [2 x i64] %35), !dbg !883
  %36 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !884
  %37 = load i64, ptr %36, align 8, !dbg !884
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %37, ptr %3), !dbg !884
  %38 = load [2 x i64], ptr @static_130, align 8, !dbg !883
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %3, [2 x i64] %38), !dbg !883
  %39 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage1, i32 0, i32 1, !dbg !885
  %40 = load i64, ptr %39, align 8, !dbg !885
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %40, ptr %3), !dbg !885
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %3, i64 24, i1 false), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %1, i64 16, i1 false), !dbg !883
  %41 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %1, i32 0, i32 1, !dbg !883
  %42 = load i64, ptr %41, align 8, !dbg !883
  %43 = load [2 x i64], ptr %6, align 8, !dbg !883
  %44 = call fastcc [2 x i64] @core.buffer.slice_1776([2 x i64] %43, i64 0, i64 %42), !dbg !883
  store [2 x i64] %44, ptr %abi_pair_storage3, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 8 %abi_pair_storage3, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %24, ptr align 8 %7, i64 16, i1 false), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %8, ptr align 8 %24, i64 16, i1 false), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %8, i64 16, i1 false), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %17, ptr align 8 %4, i64 16, i1 false), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %11, ptr align 8 %17, i64 16, i1 false), !dbg !883
  %45 = load ptr, ptr %11, align 8, !dbg !883
  %46 = getelementptr inbounds nuw { ptr, i64 }, ptr %11, i32 0, i32 1, !dbg !883
  %47 = load i64, ptr %46, align 8, !dbg !883
  store ptr %45, ptr %14, align 8, !dbg !883
  %48 = getelementptr inbounds nuw { ptr, i64 }, ptr %14, i32 0, i32 1, !dbg !883
  store i64 %47, ptr %48, align 8, !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %13, ptr align 8 %14, i64 16, i1 false), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %12, ptr align 8 %13, i64 16, i1 false), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %10, ptr align 8 %12, i64 16, i1 false), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %25, ptr align 8 %10, i64 16, i1 false), !dbg !883
  %49 = call fastcc ptr @core.mem.tmp_926(), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %15, ptr align 8 %25, i64 16, i1 false), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %18, ptr align 8 %15, i64 16, i1 false), !dbg !883
  %50 = getelementptr inbounds nuw { ptr, i64 }, ptr %18, i32 0, i32 1, !dbg !883
  %51 = load i64, ptr %50, align 8, !dbg !883
  %52 = call fastcc [2 x i64] @core.buffer.allocate-in_1861(ptr %49, i64 %51), !dbg !883
  store [2 x i64] %52, ptr %abi_pair_storage4, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %22, ptr align 8 %abi_pair_storage4, i64 16, i1 false)
  %53 = load [2 x i64], ptr %18, align 8, !dbg !883
  %54 = load [2 x i64], ptr %22, align 8, !dbg !883
  call fastcc void @core.buffer.copy_1862([2 x i64] %53, [2 x i64] %54), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %21, ptr align 8 %22, i64 16, i1 false), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %20, ptr align 8 %21, i64 16, i1 false), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %19, ptr align 8 %20, i64 16, i1 false), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %26, ptr align 8 %19, i64 16, i1 false), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %23, ptr align 8 %26, i64 16, i1 false), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %16, ptr align 8 %23, i64 16, i1 false), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %16, i64 16, i1 false), !dbg !883
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %9, ptr align 8 %2, i64 16, i1 false), !dbg !883
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %5, i64 24, i1 false)
  %55 = load [2 x i64], ptr %9, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %55), !dbg !882
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !882
  unreachable, !dbg !882

arm_cond:                                         ; preds = %entry
  %56 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !886
  %57 = load i64, ptr %56, align 8, !dbg !886
  %58 = icmp eq i64 %57, 0, !dbg !886
  %59 = zext i1 %58 to i8, !dbg !886
  %60 = trunc i8 %59 to i1, !dbg !887
  br i1 %60, label %arm_cons2, label %match_end, !dbg !887

arm_cons2:                                        ; preds = %arm_cond
  ret void, !dbg !888

match_end:                                        ; preds = %arm_cond
  %61 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !889
  %62 = load i64, ptr %61, align 8, !dbg !889
  %63 = mul i64 %62, 1, !dbg !890
  store i64 %63, ptr %0, align 8, !dbg !890
  %64 = load ptr, ptr %abi_pair_storage1, align 8, !dbg !891
  %65 = load ptr, ptr %abi_pair_storage, align 8, !dbg !892
  %66 = load i64, ptr %0, align 8, !dbg !893
  call void @core.mem.copy_234(ptr %64, ptr %65, i64 %66), !dbg !894
  ret void, !dbg !894
}

; Function Attrs: nounwind
define internal fastcc ptr @core.arena.try-realloc_357(ptr %self, ptr %old-ptr, i64 %old-size, i64 %new-size, i64 %align) #0 !dbg !895 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %0 = alloca ptr, align 8
  %1 = alloca ptr, align 8
  %2 = alloca ptr, align 8
  %3 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 @static_104, i64 16, i1 false), !dbg !898
  %4 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %3, i32 0, i32 1, !dbg !898
  store i64 183, ptr %4, align 8, !dbg !898
  %5 = icmp sge i64 %new-size, %old-size, !dbg !899
  %6 = zext i1 %5 to i8, !dbg !899
  %7 = trunc i8 %6 to i1, !dbg !898
  %8 = xor i1 %7, true, !dbg !898
  %9 = zext i1 %8 to i8, !dbg !898
  %10 = trunc i8 %9 to i1, !dbg !898
  br i1 %10, label %arm_cons, label %entry1, !dbg !898

arm_cons:                                         ; preds = %entry
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %3, i64 24, i1 false)
  %11 = load [2 x i64], ptr @static_64, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %11), !dbg !898
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !898
  unreachable, !dbg !898

entry1:                                           ; preds = %entry
  %12 = getelementptr inbounds i8, ptr %old-ptr, i64 %old-size, !dbg !900
  %13 = ptrtoint ptr %12 to i64, !dbg !900
  %14 = sub i64 8, %13, !dbg !900
  %15 = and i64 %14, 7, !dbg !900
  %16 = getelementptr inbounds i8, ptr %12, i64 %15, !dbg !900
  store ptr %16, ptr %2, align 8, !dbg !900
  %17 = getelementptr inbounds i8, ptr %old-ptr, i64 %new-size, !dbg !901
  %18 = ptrtoint ptr %17 to i64, !dbg !901
  %19 = sub i64 8, %18, !dbg !901
  %20 = and i64 %19, 7, !dbg !901
  %21 = getelementptr inbounds i8, ptr %17, i64 %20, !dbg !901
  store ptr %21, ptr %1, align 8, !dbg !901
  %22 = load ptr, ptr %2, align 8, !dbg !902
  %23 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %self, i32 0, i32 1, !dbg !903
  %24 = load ptr, ptr %23, align 8, !dbg !903
  %25 = ptrtoint ptr %22 to i64, !dbg !902
  %26 = ptrtoint ptr %24 to i64, !dbg !902
  %27 = icmp eq i64 %25, %26, !dbg !902
  %28 = zext i1 %27 to i8, !dbg !902
  %29 = trunc i8 %28 to i1, !dbg !904
  br i1 %29, label %matching_cond_continue, label %arm_cond, !dbg !904

arm_cons2:                                        ; preds = %matching_cond_continue
  %30 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %self, i32 0, i32 1, !dbg !905
  %31 = load ptr, ptr %1, align 8, !dbg !906
  store ptr %31, ptr %30, align 8, !dbg !905
  br label %match_end, !dbg !904

arm_cond:                                         ; preds = %matching_cond_continue, %entry1
  %32 = call fastcc ptr @core.arena.alloc-layout_350(ptr %self, i64 %new-size, i64 %align), !dbg !907
  store ptr %32, ptr %0, align 8, !dbg !907
  %33 = load ptr, ptr %0, align 8, !dbg !908
  call void @core.mem.copy_234(ptr %33, ptr %old-ptr, i64 %old-size), !dbg !909
  %34 = load ptr, ptr %0, align 8, !dbg !910
  br label %match_end, !dbg !904

match_end:                                        ; preds = %arm_cons2, %arm_cond
  %35 = phi ptr [ %old-ptr, %arm_cons2 ], [ %34, %arm_cond ], !dbg !904
  ret ptr %35, !dbg !904

matching_cond_continue:                           ; preds = %entry1
  %36 = load ptr, ptr %1, align 8, !dbg !911
  %37 = getelementptr inbounds nuw { ptr, ptr, ptr, ptr, ptr, ptr, i8, { { { ptr, i64 } } } }, ptr %self, i32 0, i32 2, !dbg !912
  %38 = load ptr, ptr %37, align 8, !dbg !912
  %39 = ptrtoint ptr %36 to i64, !dbg !911
  %40 = ptrtoint ptr %38 to i64, !dbg !911
  %41 = icmp ule i64 %39, %40, !dbg !911
  %42 = zext i1 %41 to i8, !dbg !911
  %43 = trunc i8 %42 to i1, !dbg !904
  br i1 %43, label %arm_cons2, label %arm_cond, !dbg !904
}

; Function Attrs: nounwind
define internal fastcc void @core.list._grow_1942(ptr %self, ptr %alloc) #0 !dbg !913 {
prelude:
  %0 = alloca { { ptr, i64 }, i64 }, align 8
  %1 = alloca { { ptr, i64 }, i64 }, align 8
  %2 = alloca i64, align 8
  %3 = alloca { ptr, i64 }, align 8
  %4 = alloca i64, align 8
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %self, i64 16, i1 false), !dbg !917
  %5 = getelementptr inbounds nuw { ptr, i64 }, ptr %3, i32 0, i32 1, !dbg !917
  %6 = load i64, ptr %5, align 8, !dbg !917
  %7 = icmp eq i64 %6, 0, !dbg !917
  %8 = zext i1 %7 to i8, !dbg !917
  %9 = trunc i8 %8 to i1, !dbg !918
  br i1 %9, label %arm_cons, label %arm_cons1, !dbg !918

arm_cons:                                         ; preds = %entry
  store i64 1, ptr %2, align 8, !dbg !919
  %10 = load i64, ptr %2, align 8, !dbg !920
  %11 = icmp sge i64 %10, 1024, !dbg !920
  %12 = zext i1 %11 to i8, !dbg !920
  %13 = trunc i8 %12 to i1, !dbg !921
  br i1 %13, label %arm_cons4, label %arm_cons5, !dbg !921

arm_cons1:                                        ; preds = %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %self, i64 24, i1 false), !dbg !918
  %14 = getelementptr inbounds nuw { ptr, i64 }, ptr %1, i32 0, i32 1, !dbg !922
  %15 = load i64, ptr %14, align 8, !dbg !922
  %16 = mul i64 %15, 2, !dbg !922
  store i64 %16, ptr %4, align 8, !dbg !918
  br label %match_end, !dbg !918

match_end:                                        ; preds = %match_end6, %arm_cons1
  %17 = load i64, ptr %4, align 8, !dbg !923
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %self, i64 24, i1 false), !dbg !924
  %18 = getelementptr inbounds nuw { ptr, i64 }, ptr %0, i32 0, i32 1, !dbg !924
  %19 = load i64, ptr %18, align 8, !dbg !924
  %20 = icmp sge i64 %19, %17, !dbg !924
  %21 = zext i1 %20 to i8, !dbg !924
  %22 = trunc i8 %21 to i1, !dbg !924
  br i1 %22, label %match_end3, label %match_end2, !dbg !924

match_end2:                                       ; preds = %match_end
  call fastcc void @core.buffer._grow-to_1878(ptr %self, ptr %alloc, i64 %17), !dbg !924
  br label %match_end3, !dbg !924

match_end3:                                       ; preds = %match_end2, %match_end
  ret void, !dbg !924

arm_cons4:                                        ; preds = %arm_cons
  br label %match_end6, !dbg !921

arm_cons5:                                        ; preds = %arm_cons
  br label %match_end6, !dbg !921

match_end6:                                       ; preds = %arm_cons4, %arm_cons5
  %23 = phi i64 [ 1, %arm_cons4 ], [ 8, %arm_cons5 ], !dbg !921
  store i64 %23, ptr %4, align 8, !dbg !918
  br label %match_end, !dbg !918
}

; Function Attrs: nounwind
define internal fastcc void @core.impl_22as-buffer.set_for_t1665_1944(ptr %self, i64 %index, i8 %value) #0 !dbg !925 {
prelude:
  %abi_pair_storage = alloca [2 x i64], align 8
  %0 = alloca { ptr, i64 }, align 8
  %1 = alloca { ptr, i64 }, align 8
  %2 = alloca { ptr, i64 }, align 8
  %3 = alloca { { ptr, i64 }, i64 }, align 8
  %4 = alloca { ptr, i64 }, align 8
  %5 = alloca ptr, align 8
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %self, i64 24, i1 false), !dbg !928
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %3, i64 16, i1 false), !dbg !928
  %6 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %3, i32 0, i32 1, !dbg !928
  %7 = load i64, ptr %6, align 8, !dbg !928
  %8 = load [2 x i64], ptr %1, align 8, !dbg !928
  %9 = call fastcc [2 x i64] @core.buffer.slice_1776([2 x i64] %8, i64 0, i64 %7), !dbg !928
  store [2 x i64] %9, ptr %abi_pair_storage, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %abi_pair_storage, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %2, i64 16, i1 false), !dbg !928
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %4, i64 16, i1 false), !dbg !928
  %10 = load [2 x i64], ptr %0, align 8, !dbg !928
  call fastcc void @core.buffer.check-bounds_1868([2 x i64] %10, i64 %index), !dbg !928
  %11 = load ptr, ptr %0, align 8, !dbg !928
  %12 = getelementptr inbounds i8, ptr %11, i64 %index, !dbg !928
  store ptr %12, ptr %5, align 8, !dbg !928
  %13 = load ptr, ptr %5, align 8, !dbg !928
  store i8 %value, ptr %13, align 1, !dbg !928
  ret void, !dbg !928
}

; Function Attrs: nounwind
define internal fastcc void @core.io.stdout-buffer-bytes_584([2 x i64] %bytes) #0 !dbg !929 {
prelude:
  %0 = alloca { { ptr, i64 } }, align 8
  %1 = alloca { { ptr, i64 } }, align 8
  %2 = alloca { ptr, i64 }, align 8
  %3 = alloca { ptr, i64 }, align 8
  %4 = alloca { ptr, i64 }, align 8
  %5 = alloca { { ptr, i64 } }, align 8
  %6 = alloca { { ptr, i64 } }, align 8
  %7 = alloca { ptr, i64 }, align 8
  %8 = alloca { ptr, i64 }, align 8
  %9 = alloca { { ptr, i64 } }, align 8
  %10 = alloca { { ptr, i64 } }, align 8
  %11 = alloca { ptr, i64 }, align 8
  %12 = alloca { ptr, i64 }, align 8
  %13 = alloca i8, align 1
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %bytes, ptr %abi_pair_storage, align 8, !dbg !933
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !934
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %1, i64 16, i1 false), !dbg !934
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %0, i64 16, i1 false), !dbg !934
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %3, i64 16, i1 false), !dbg !934
  %14 = getelementptr inbounds nuw { ptr, i64 }, ptr %2, i32 0, i32 1, !dbg !934
  %15 = load i64, ptr %14, align 8, !dbg !934
  %16 = icmp sge i64 %15, 8192, !dbg !934
  %17 = zext i1 %16 to i8, !dbg !934
  %18 = trunc i8 %17 to i1, !dbg !935
  br i1 %18, label %arm_cons, label %entry1, !dbg !935

arm_cons:                                         ; preds = %entry
  call fastcc void @core.io.flush-stdout_583(), !dbg !936
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !937
  %19 = load ptr, ptr %4, align 8, !dbg !937
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !938
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %6, i64 16, i1 false), !dbg !938
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %8, ptr align 8 %5, i64 16, i1 false), !dbg !938
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 8 %8, i64 16, i1 false), !dbg !938
  %20 = getelementptr inbounds nuw { ptr, i64 }, ptr %7, i32 0, i32 1, !dbg !938
  %21 = load i64, ptr %20, align 8, !dbg !938
  call fastcc void @core.io.write-all_582(i32 1, ptr %19, i64 %21), !dbg !939
  ret void, !dbg !940

entry1:                                           ; preds = %entry
  %22 = load i32, ptr getelementptr inbounds nuw ({ [8192 x i8], i32 }, ptr @_root__core__io__stdout-buf, i32 0, i32 1), align 4, !dbg !941
  %23 = zext i32 %22 to i64, !dbg !941
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %10, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !942
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %9, ptr align 8 %10, i64 16, i1 false), !dbg !942
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %12, ptr align 8 %9, i64 16, i1 false), !dbg !942
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %11, ptr align 8 %12, i64 16, i1 false), !dbg !942
  %24 = getelementptr inbounds nuw { ptr, i64 }, ptr %11, i32 0, i32 1, !dbg !942
  %25 = load i64, ptr %24, align 8, !dbg !942
  %26 = add i64 %23, %25, !dbg !941
  %27 = icmp sgt i64 %26, 8192, !dbg !941
  %28 = zext i1 %27 to i8, !dbg !941
  %29 = trunc i8 %28 to i1, !dbg !943
  br i1 %29, label %arm_cons2, label %match_end, !dbg !943

arm_cons2:                                        ; preds = %entry1
  call fastcc void @core.io.flush-stdout_583(), !dbg !944
  br label %match_end, !dbg !943

match_end:                                        ; preds = %arm_cons2, %entry1
  %30 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !945
  call fastcc void @core.fixlist.push-n_2016(ptr @_root__core__io__stdout-buf, [2 x i64] %30), !dbg !945
  %31 = call fastcc i8 @core.io.stdout-buffering-mode_581(), !dbg !946
  store i8 %31, ptr %13, align 1, !dbg !946
  %32 = load i8, ptr %13, align 1, !dbg !946
  %33 = icmp eq i8 %32, 1, !dbg !947
  %34 = zext i1 %33 to i8, !dbg !947
  %35 = trunc i8 %34 to i1, !dbg !948
  br i1 %35, label %matching_cond_continue, label %arm_cond, !dbg !948

arm_cons3:                                        ; preds = %matching_cond_continue
  call fastcc void @core.io.flush-stdout_583(), !dbg !949
  br label %match_end4, !dbg !948

arm_cond:                                         ; preds = %matching_cond_continue, %match_end
  br label %match_end4, !dbg !948

match_end4:                                       ; preds = %arm_cons3, %arm_cond
  ret void, !dbg !948

matching_cond_continue:                           ; preds = %match_end
  %36 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !950
  %37 = call fastcc i8 @core.span.contains_2017([2 x i64] %36, i8 10), !dbg !950
  %38 = trunc i8 %37 to i1, !dbg !948
  br i1 %38, label %arm_cons3, label %arm_cond, !dbg !948
}

; Function Attrs: nounwind
define internal fastcc void @core.fixlist.push-n_2016(ptr %self, [2 x i64] %values) #0 !dbg !951 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %struct_in_integer_storage = alloca { i8, {} }, align 1
  %0 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %1 = alloca { i8, {} }, align 1
  %2 = alloca { i8, {} }, align 1
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %values, ptr %abi_pair_storage, align 8, !dbg !955
  br label %entry

entry:                                            ; preds = %prelude
  %3 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !956
  %4 = call fastcc i8 @core.fixlist.try-push-n_2018(ptr %self, [2 x i64] %3), !dbg !956
  store i8 %4, ptr %struct_in_integer_storage, align 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %2, ptr align 1 %struct_in_integer_storage, i64 1, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %1, ptr align 1 %2, i64 1, i1 false), !dbg !957
  %5 = load i8, ptr %1, align 1, !dbg !958
  %6 = icmp eq i8 %5, 1, !dbg !958
  %7 = zext i1 %6 to i8, !dbg !958
  %8 = trunc i8 %7 to i1, !dbg !959
  br i1 %8, label %matching_cond_continue, label %match_end, !dbg !959

match_end:                                        ; preds = %entry
  ret void, !dbg !959

matching_cond_continue:                           ; preds = %entry
  %9 = getelementptr inbounds nuw { i8, {} }, ptr %1, i32 0, i32 1, !dbg !958
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @static_195, i64 16, i1 false), !dbg !960
  %10 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %0, i32 0, i32 1, !dbg !960
  store i64 60, ptr %10, align 8, !dbg !960
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %0, i64 24, i1 false)
  %11 = load [2 x i64], ptr @static_225, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %11), !dbg !960
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !960
  unreachable, !dbg !960
}

; Function Attrs: nounwind
define internal fastcc i8 @core.span.contains_2017([2 x i64] %self, i8 %needle) #0 !dbg !961 {
prelude:
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %0 = alloca i64, align 8
  %1 = alloca { i8, { i64 } }, align 8
  %2 = alloca { ptr, i64 }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !964
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !965
  %3 = load [2 x i64], ptr %2, align 8, !dbg !965
  %4 = call fastcc [2 x i64] @core.buffer.position_2030([2 x i64] %3, i8 %needle), !dbg !965
  store [2 x i64] %4, ptr %abi_pair_storage1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %abi_pair_storage1, i64 16, i1 false), !dbg !965
  %5 = load i8, ptr %1, align 1, !dbg !965
  %6 = icmp eq i8 %5, 1, !dbg !965
  %7 = zext i1 %6 to i8, !dbg !965
  %8 = trunc i8 %7 to i1, !dbg !965
  br i1 %8, label %matching_cond_continue, label %arm_cons, !dbg !965

arm_cons:                                         ; preds = %entry
  br label %match_end, !dbg !965

match_end:                                        ; preds = %matching_cond_continue, %arm_cons
  %9 = phi i8 [ 1, %matching_cond_continue ], [ 0, %arm_cons ], !dbg !965
  ret i8 %9, !dbg !965

matching_cond_continue:                           ; preds = %entry
  %10 = getelementptr inbounds nuw { i8, { i64 } }, ptr %1, i32 0, i32 1, !dbg !965
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %10, i64 8, i1 false), !dbg !965
  br label %match_end, !dbg !965
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.buffer.position_2030([2 x i64] %self, i8 %needle) #0 !dbg !966 {
prelude:
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %0 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %1 = alloca ptr, align 8
  %2 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %3 = alloca { i8, { i64 } }, align 8
  %4 = alloca i64, align 8
  %5 = alloca i8, align 1
  %6 = alloca i8, align 1
  %7 = alloca i8, align 1
  %8 = alloca { i8, { i64 } }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !975
  br label %entry

entry:                                            ; preds = %prelude
  store i8 %needle, ptr %7, align 1, !dbg !976
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %6, ptr align 1 %7, i64 1, i1 false), !dbg !976
  %9 = load ptr, ptr %abi_pair_storage, align 8, !dbg !977
  %10 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !978
  %11 = load i64, ptr %10, align 8, !dbg !978
  %12 = load i8, ptr %6, align 1, !dbg !979
  %13 = call fastcc [2 x i64] @core.buffer.position-byte_473(ptr %9, i64 %11, i8 %12), !dbg !980
  store [2 x i64] %13, ptr %abi_pair_storage1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %8, ptr align 8 %abi_pair_storage1, i64 16, i1 false), !dbg !976
  %14 = load [2 x i64], ptr %8, align 8, !dbg !976
  ret [2 x i64] %14, !dbg !976
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.buffer.position-byte_473(ptr %base, i64 %len, i8 %needle) #0 !dbg !981 {
prelude:
  %0 = alloca { i8, { i64 } }, align 8
  %1 = alloca <16 x i8>, align 16
  %2 = alloca i64, align 8
  %3 = alloca <16 x i8>, align 16
  %4 = alloca <16 x i8>, align 16
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  br label %entry

entry:                                            ; preds = %prelude
  store i64 16, ptr %6, align 8, !dbg !984
  store i64 0, ptr %5, align 8, !dbg !985
  %7 = load i64, ptr %6, align 8, !dbg !986
  %8 = icmp sge i64 %len, %7, !dbg !987
  %9 = zext i1 %8 to i8, !dbg !987
  %10 = trunc i8 %9 to i1, !dbg !988
  br i1 %10, label %arm_cons, label %match_end, !dbg !988

arm_cons:                                         ; preds = %entry
  %11 = insertelement <16 x i8> undef, i8 %needle, i32 0, !dbg !989
  %12 = shufflevector <16 x i8> %11, <16 x i8> undef, <16 x i32> zeroinitializer, !dbg !989
  store <16 x i8> %12, ptr %4, align 16, !dbg !989
  br label %while_loop_condition, !dbg !990

match_end:                                        ; preds = %while_loop_condition, %entry
  br label %while_loop_condition3, !dbg !991

while_loop_condition:                             ; preds = %match_end2, %arm_cons
  %13 = load i64, ptr %5, align 8, !dbg !992
  %14 = load i64, ptr %6, align 8, !dbg !993
  %15 = add i64 %13, %14, !dbg !992
  %16 = icmp sle i64 %15, %len, !dbg !992
  %17 = zext i1 %16 to i8, !dbg !992
  %18 = trunc i8 %17 to i1, !dbg !990
  br i1 %18, label %while_loop_body, label %match_end, !dbg !990

while_loop_body:                                  ; preds = %while_loop_condition
  %19 = load i64, ptr %5, align 8, !dbg !994
  %20 = getelementptr inbounds i8, ptr %base, i64 %19, !dbg !995
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %3, ptr align 1 %20, i64 16, i1 false), !dbg !996
  %21 = load <16 x i8>, ptr %3, align 16, !dbg !997
  %22 = load <16 x i8>, ptr %4, align 16, !dbg !997
  %23 = icmp eq <16 x i8> %21, %22, !dbg !997
  %24 = sext <16 x i1> %23 to <16 x i8>, !dbg !997
  store <16 x i8> %24, ptr %1, align 16, !dbg !997
  %25 = load [2 x i64], ptr %1, align 8, !dbg !997
  %26 = call fastcc i64 @core.vector.first-true-lane_1553([2 x i64] %25), !dbg !997
  store i64 %26, ptr %2, align 8, !dbg !997
  %27 = load i64, ptr %2, align 8, !dbg !998
  %28 = icmp sge i64 %27, 0, !dbg !998
  %29 = zext i1 %28 to i8, !dbg !998
  %30 = trunc i8 %29 to i1, !dbg !999
  br i1 %30, label %arm_cons1, label %match_end2, !dbg !999

arm_cons1:                                        ; preds = %while_loop_body
  store i8 1, ptr %0, align 1, !dbg !1000
  %31 = getelementptr inbounds nuw { i8, { i64 } }, ptr %0, i32 0, i32 1, !dbg !1000
  %32 = load i64, ptr %5, align 8, !dbg !1001
  %33 = load i64, ptr %2, align 8, !dbg !1002
  %34 = add i64 %32, %33, !dbg !1001
  store i64 %34, ptr %31, align 8, !dbg !1001
  %35 = load [2 x i64], ptr %0, align 8, !dbg !1003
  ret [2 x i64] %35, !dbg !1003

match_end2:                                       ; preds = %while_loop_body
  %36 = load i64, ptr %5, align 8, !dbg !1004
  %37 = load i64, ptr %6, align 8, !dbg !1005
  %38 = add i64 %36, %37, !dbg !1004
  store i64 %38, ptr %5, align 8, !dbg !1006
  br label %while_loop_condition, !dbg !990

while_loop_condition3:                            ; preds = %match_end5, %match_end
  %39 = load i64, ptr %5, align 8, !dbg !1007
  %40 = icmp slt i64 %39, %len, !dbg !1007
  %41 = zext i1 %40 to i8, !dbg !1007
  %42 = trunc i8 %41 to i1, !dbg !991
  br i1 %42, label %arm_cond, label %while_loop_end, !dbg !991

while_loop_end:                                   ; preds = %while_loop_condition3
  store i8 0, ptr %0, align 1, !dbg !1008
  %43 = load [2 x i64], ptr %0, align 8, !dbg !1008
  ret [2 x i64] %43, !dbg !1008

arm_cond:                                         ; preds = %while_loop_condition3
  %44 = load i64, ptr %5, align 8, !dbg !1009
  %45 = getelementptr inbounds i8, ptr %base, i64 %44, !dbg !1010
  %46 = load i8, ptr %45, align 1, !dbg !1010
  %47 = icmp eq i8 %46, %needle, !dbg !1010
  %48 = zext i1 %47 to i8, !dbg !1010
  %49 = trunc i8 %48 to i1, !dbg !1011
  br i1 %49, label %arm_cons4, label %match_end5, !dbg !1011

arm_cons4:                                        ; preds = %arm_cond
  store i8 1, ptr %0, align 1, !dbg !1012
  %50 = getelementptr inbounds nuw { i8, { i64 } }, ptr %0, i32 0, i32 1, !dbg !1012
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %50, ptr align 8 %5, i64 8, i1 false), !dbg !1013
  %51 = load [2 x i64], ptr %0, align 8, !dbg !1014
  ret [2 x i64] %51, !dbg !1014

match_end5:                                       ; preds = %arm_cond
  %52 = load i64, ptr %5, align 8, !dbg !1015
  %53 = add i64 %52, 1, !dbg !1015
  store i64 %53, ptr %5, align 8, !dbg !1016
  br label %while_loop_condition3, !dbg !991
}

; Function Attrs: nounwind
define internal fastcc i64 @core.vector.first-true-lane_1553([2 x i64] %v) #0 !dbg !1017 {
prelude:
  %0 = alloca i64, align 8
  %abi_pair_storage = alloca [2 x i64], align 16
  store [2 x i64] %v, ptr %abi_pair_storage, align 8, !dbg !1022
  br label %entry

entry:                                            ; preds = %prelude
  %1 = load <16 x i8>, ptr %abi_pair_storage, align 16, !dbg !1023
  %2 = icmp slt <16 x i8> %1, zeroinitializer, !dbg !1023
  %3 = bitcast <16 x i1> %2 to i16, !dbg !1023
  %4 = zext i16 %3 to i64, !dbg !1023
  store i64 %4, ptr %0, align 8, !dbg !1023
  %5 = load i64, ptr %0, align 8, !dbg !1024
  %6 = icmp eq i64 %5, 0, !dbg !1024
  %7 = zext i1 %6 to i8, !dbg !1024
  %8 = trunc i8 %7 to i1, !dbg !1025
  br i1 %8, label %arm_cons, label %arm_cons1, !dbg !1025

arm_cons:                                         ; preds = %entry
  br label %match_end, !dbg !1025

arm_cons1:                                        ; preds = %entry
  %9 = load i64, ptr %0, align 8, !dbg !1026
  %10 = call i64 @llvm.cttz.i64(i64 %9, i1 false), !dbg !1026
  br label %match_end, !dbg !1025

match_end:                                        ; preds = %arm_cons, %arm_cons1
  %11 = phi i64 [ -1, %arm_cons ], [ %10, %arm_cons1 ], !dbg !1025
  ret i64 %11, !dbg !1025
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i64 @llvm.cttz.i64(i64, i1 immarg) #4

; Function Attrs: nounwind
define internal fastcc i8 @core.fixlist.try-push-n_2018(ptr %self, [2 x i64] %values) #0 !dbg !1027 {
prelude:
  %abi_struct_int = alloca i8, align 1
  %abi_pair_storage3 = alloca [2 x i64], align 8
  %abi_pair_storage2 = alloca [2 x i64], align 8
  %0 = alloca { { ptr, i64 } }, align 8
  %1 = alloca { { ptr, i64 } }, align 8
  %2 = alloca { ptr, i64 }, align 8
  %3 = alloca { ptr, i64 }, align 8
  %4 = alloca { i8, {} }, align 1
  %5 = alloca { ptr, i64 }, align 8
  %6 = alloca { i8, {} }, align 1
  %7 = alloca { ptr, i64 }, align 8
  %8 = alloca { i8, {} }, align 1
  %9 = alloca i64, align 8
  %10 = alloca { ptr, i64 }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %values, ptr %abi_pair_storage, align 8, !dbg !1035
  br label %entry

entry:                                            ; preds = %prelude
  %11 = getelementptr inbounds nuw { [8192 x i8], i32 }, ptr %self, i32 0, i32 1, !dbg !1036
  %12 = load i32, ptr %11, align 4, !dbg !1036
  %13 = zext i32 %12 to i64, !dbg !1036
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !1037
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %1, i64 16, i1 false), !dbg !1037
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %0, i64 16, i1 false), !dbg !1037
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %3, i64 16, i1 false), !dbg !1037
  %14 = getelementptr inbounds nuw { ptr, i64 }, ptr %2, i32 0, i32 1, !dbg !1037
  %15 = load i64, ptr %14, align 8, !dbg !1037
  %16 = add i64 %13, %15, !dbg !1036
  store i64 %16, ptr %9, align 8, !dbg !1036
  %17 = load i64, ptr %9, align 8, !dbg !1038
  %18 = icmp sle i64 %17, 8192, !dbg !1038
  %19 = zext i1 %18 to i8, !dbg !1038
  %20 = trunc i8 %19 to i1, !dbg !1039
  br i1 %20, label %arm_cons, label %arm_cons1, !dbg !1039

arm_cons:                                         ; preds = %entry
  %21 = call fastcc [2 x i64] @core.impl_22as-buffer.as-buffer_for_t3358_2019(ptr %self), !dbg !1040
  store [2 x i64] %21, ptr %abi_pair_storage2, align 8
  %22 = getelementptr inbounds nuw { [8192 x i8], i32 }, ptr %self, i32 0, i32 1, !dbg !1041
  %23 = load i32, ptr %22, align 4, !dbg !1041
  %24 = zext i32 %23 to i64, !dbg !1041
  %25 = load i64, ptr %9, align 8, !dbg !1042
  %26 = load [2 x i64], ptr %abi_pair_storage2, align 8, !dbg !1040
  %27 = call fastcc [2 x i64] @core.buffer.slice_1776([2 x i64] %26, i64 %24, i64 %25), !dbg !1040
  store [2 x i64] %27, ptr %abi_pair_storage3, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 8 %abi_pair_storage3, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !1043
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %10, ptr align 8 %5, i64 16, i1 false), !dbg !1043
  %28 = load [2 x i64], ptr %10, align 8, !dbg !1044
  %29 = load [2 x i64], ptr %7, align 8, !dbg !1044
  call fastcc void @core.buffer.copy_1877([2 x i64] %28, [2 x i64] %29), !dbg !1044
  %30 = getelementptr inbounds nuw { [8192 x i8], i32 }, ptr %self, i32 0, i32 1, !dbg !1045
  %31 = load i64, ptr %9, align 8, !dbg !1046
  %32 = trunc i64 %31 to i32, !dbg !1046
  store i32 %32, ptr %30, align 4, !dbg !1045
  store i8 0, ptr %6, align 1, !dbg !1047
  %33 = getelementptr inbounds nuw { i8, {} }, ptr %6, i32 0, i32 1, !dbg !1047
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %8, ptr align 1 %6, i64 1, i1 false), !dbg !1039
  br label %match_end, !dbg !1039

arm_cons1:                                        ; preds = %entry
  store i8 1, ptr %4, align 1, !dbg !1048
  %34 = getelementptr inbounds nuw { i8, {} }, ptr %4, i32 0, i32 1, !dbg !1048
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %8, ptr align 1 %4, i64 1, i1 false), !dbg !1039
  br label %match_end, !dbg !1039

match_end:                                        ; preds = %arm_cons, %arm_cons1
  call void @llvm.lifetime.start.p0(i64 1, ptr %abi_struct_int)
  store i8 0, ptr %abi_struct_int, align 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %abi_struct_int, ptr align 1 %8, i64 1, i1 false)
  %35 = load i8, ptr %abi_struct_int, align 1
  call void @llvm.lifetime.end.p0(i64 1, ptr %abi_struct_int)
  ret i8 %35
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.impl_22as-buffer.as-buffer_for_t3358_2019(ptr %self) #0 !dbg !1049 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %0 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %1 = alloca { ptr, i64 }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  br i1 true, label %match_end, label %arm_cons, !dbg !1052

arm_cons:                                         ; preds = %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @static_26, i64 16, i1 false), !dbg !1052
  %2 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %0, i32 0, i32 1, !dbg !1052
  store i64 985, ptr %2, align 8, !dbg !1052
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %0, i64 24, i1 false)
  %3 = load [2 x i64], ptr @static_53, align 8
  call fastcc void @core.crash-bounds_336(ptr %abi_caller_copy, i64 8192, i64 0, [2 x i64] %3), !dbg !1052
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !1052
  unreachable, !dbg !1052

match_end:                                        ; preds = %entry
  %4 = getelementptr inbounds i8, ptr %self, i64 0, !dbg !1052
  store ptr %4, ptr %1, align 8, !dbg !1052
  %5 = getelementptr inbounds nuw { ptr, i64 }, ptr %1, i32 0, i32 1, !dbg !1053
  store i64 8192, ptr %5, align 8, !dbg !1054
  %6 = load [2 x i64], ptr %1, align 8, !dbg !1053
  ret [2 x i64] %6, !dbg !1053
}

; Function Attrs: nounwind
define internal fastcc void @core.buffer.swap_1923([2 x i64] %self, i64 %index-a, i64 %index-b) #0 !dbg !1055 {
prelude:
  %0 = alloca i8, align 1
  %1 = alloca ptr, align 8
  %2 = alloca ptr, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !1059
  br label %entry

entry:                                            ; preds = %prelude
  %3 = icmp eq i64 %index-a, %index-b, !dbg !1060
  %4 = zext i1 %3 to i8, !dbg !1060
  %5 = trunc i8 %4 to i1, !dbg !1061
  br i1 %5, label %arm_cons, label %entry1, !dbg !1061

arm_cons:                                         ; preds = %entry
  ret void, !dbg !1062

entry1:                                           ; preds = %entry
  %6 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !1063
  call fastcc void @core.buffer.check-bounds_1925([2 x i64] %6, i64 %index-a), !dbg !1063
  %7 = load ptr, ptr %abi_pair_storage, align 8, !dbg !1063
  %8 = getelementptr inbounds i8, ptr %7, i64 %index-a, !dbg !1063
  store ptr %8, ptr %2, align 8, !dbg !1063
  %9 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !1064
  call fastcc void @core.buffer.check-bounds_1925([2 x i64] %9, i64 %index-b), !dbg !1064
  %10 = load ptr, ptr %abi_pair_storage, align 8, !dbg !1064
  %11 = getelementptr inbounds i8, ptr %10, i64 %index-b, !dbg !1064
  store ptr %11, ptr %1, align 8, !dbg !1064
  %12 = load ptr, ptr %1, align 8, !dbg !1065
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %0, ptr align 1 %12, i64 1, i1 false), !dbg !1065
  %13 = load ptr, ptr %1, align 8, !dbg !1066
  %14 = load ptr, ptr %2, align 8, !dbg !1067
  %15 = load i8, ptr %14, align 1, !dbg !1067
  store i8 %15, ptr %13, align 1, !dbg !1066
  %16 = load ptr, ptr %2, align 8, !dbg !1068
  %17 = load i8, ptr %0, align 1, !dbg !1069
  store i8 %17, ptr %16, align 1, !dbg !1068
  ret void, !dbg !1068
}

; Function Attrs: nounwind
define internal fastcc void @core.buffer.check-bounds_1925([2 x i64] %self, i64 %index) #0 !dbg !1070 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage2 = alloca [2 x i64], align 8
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %0 = alloca { { { ptr, i64 }, i64 } }, align 8
  %1 = alloca { { ptr, i64 }, i64 }, align 8
  %2 = alloca { { { ptr, i64 } } }, align 8
  %3 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %4 = alloca { { ptr, i64 } }, align 8
  %5 = alloca { { { ptr, i64 } } }, align 8
  %6 = alloca { ptr, i64 }, align 8
  %7 = alloca { ptr, i64 }, align 8
  %8 = alloca { { ptr, i64 } }, align 8
  %9 = alloca { { ptr, i64 } }, align 8
  %10 = alloca { { { ptr, i64 } } }, align 8
  %11 = alloca { ptr, i64 }, align 8
  %12 = alloca { { ptr, i64 } }, align 8
  %13 = alloca { ptr, i64 }, align 8
  %14 = alloca { ptr, i64 }, align 8
  %15 = alloca { { ptr, i64 } }, align 8
  %16 = alloca { { { ptr, i64 } } }, align 8
  %17 = alloca { ptr, i64 }, align 8
  %18 = alloca { ptr, i64 }, align 8
  %19 = alloca { { ptr, i64 } }, align 8
  %20 = alloca { ptr, i64 }, align 8
  %21 = alloca { ptr, i64 }, align 8
  %22 = alloca { ptr, i64 }, align 8
  %23 = alloca { { { ptr, i64 } } }, align 8
  %24 = alloca { { { ptr, i64 } } }, align 8
  %25 = alloca { { ptr, i64 } }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !1074
  br label %entry

entry:                                            ; preds = %prelude
  %26 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !1075
  %27 = load i64, ptr %26, align 8, !dbg !1075
  %28 = icmp uge i64 %index, %27, !dbg !1076
  %29 = zext i1 %28 to i8, !dbg !1076
  %30 = trunc i8 %29 to i1, !dbg !1077
  br i1 %30, label %arm_cons, label %match_end, !dbg !1077

arm_cons:                                         ; preds = %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 @static_126, i64 16, i1 false), !dbg !1078
  %31 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %3, i32 0, i32 1, !dbg !1078
  store i64 170, ptr %31, align 8, !dbg !1078
  call fastcc void @core.string-builder.new_1255(ptr noalias sret({ { { ptr, i64 }, i64 } }) align 8 dereferenceable(24) %0), !dbg !1079
  %32 = load [2 x i64], ptr @static_133, align 8, !dbg !1079
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %0, [2 x i64] %32), !dbg !1079
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %index, ptr %0), !dbg !1080
  %33 = load [2 x i64], ptr @static_134, align 8, !dbg !1079
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %0, [2 x i64] %33), !dbg !1079
  %34 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !1081
  %35 = load i64, ptr %34, align 8, !dbg !1081
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %35, ptr %0), !dbg !1081
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %0, i64 24, i1 false), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %1, i64 16, i1 false), !dbg !1079
  %36 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %1, i32 0, i32 1, !dbg !1079
  %37 = load i64, ptr %36, align 8, !dbg !1079
  %38 = load [2 x i64], ptr %6, align 8, !dbg !1079
  %39 = call fastcc [2 x i64] @core.buffer.slice_1776([2 x i64] %38, i64 0, i64 %37), !dbg !1079
  store [2 x i64] %39, ptr %abi_pair_storage1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 8 %abi_pair_storage1, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %17, ptr align 8 %7, i64 16, i1 false), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %8, ptr align 8 %17, i64 16, i1 false), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %8, i64 16, i1 false), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %9, ptr align 8 %4, i64 16, i1 false), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %11, ptr align 8 %9, i64 16, i1 false), !dbg !1079
  %40 = load ptr, ptr %11, align 8, !dbg !1079
  %41 = getelementptr inbounds nuw { ptr, i64 }, ptr %11, i32 0, i32 1, !dbg !1079
  %42 = load i64, ptr %41, align 8, !dbg !1079
  store ptr %40, ptr %14, align 8, !dbg !1079
  %43 = getelementptr inbounds nuw { ptr, i64 }, ptr %14, i32 0, i32 1, !dbg !1079
  store i64 %42, ptr %43, align 8, !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %13, ptr align 8 %14, i64 16, i1 false), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %12, ptr align 8 %13, i64 16, i1 false), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %10, ptr align 8 %12, i64 16, i1 false), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %24, ptr align 8 %10, i64 16, i1 false), !dbg !1079
  %44 = call fastcc ptr @core.mem.tmp_926(), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %15, ptr align 8 %24, i64 16, i1 false), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %18, ptr align 8 %15, i64 16, i1 false), !dbg !1079
  %45 = getelementptr inbounds nuw { ptr, i64 }, ptr %18, i32 0, i32 1, !dbg !1079
  %46 = load i64, ptr %45, align 8, !dbg !1079
  %47 = call fastcc [2 x i64] @core.buffer.allocate-in_1861(ptr %44, i64 %46), !dbg !1079
  store [2 x i64] %47, ptr %abi_pair_storage2, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %22, ptr align 8 %abi_pair_storage2, i64 16, i1 false)
  %48 = load [2 x i64], ptr %18, align 8, !dbg !1079
  %49 = load [2 x i64], ptr %22, align 8, !dbg !1079
  call fastcc void @core.buffer.copy_1862([2 x i64] %48, [2 x i64] %49), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %21, ptr align 8 %22, i64 16, i1 false), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %20, ptr align 8 %21, i64 16, i1 false), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %19, ptr align 8 %20, i64 16, i1 false), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %25, ptr align 8 %19, i64 16, i1 false), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %23, ptr align 8 %25, i64 16, i1 false), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %16, ptr align 8 %23, i64 16, i1 false), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %16, i64 16, i1 false), !dbg !1079
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %2, i64 16, i1 false), !dbg !1079
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %3, i64 24, i1 false)
  %50 = load [2 x i64], ptr %5, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %50), !dbg !1078
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !1078
  unreachable, !dbg !1078

match_end:                                        ; preds = %entry
  ret void, !dbg !1077
}

; Function Attrs: nounwind
declare !dbg !1082 i64 @write(i32, ptr, i64) #0

; Function Attrs: nounwind
declare !dbg !1085 ptr @__error() #0

; Function Attrs: nounwind
define internal fastcc void @core.span.to-array_6209(ptr noalias sret([5 x i64]) align 8 dereferenceable(40) %sret, [2 x i64] %self) #0 !dbg !1086 {
prelude:
  %0 = alloca { ptr, i64 }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !1097
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !1098
  %1 = load [2 x i64], ptr %0, align 8, !dbg !1099
  call fastcc void @core.array.from-buffer_6212(ptr noalias sret([5 x i64]) align 8 dereferenceable(40) %sret, [2 x i64] %1), !dbg !1099
  ret void, !dbg !1099
}

; Function Attrs: nounwind
define internal fastcc void @core.println_6211(ptr %t) #0 !dbg !1100 {
prelude:
  %abi_caller_copy = alloca [5 x i64], align 8
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.lifetime.start.p0(i64 40, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %t, i64 40, i1 false)
  call fastcc void @core.print_6214(ptr %abi_caller_copy), !dbg !1104
  call void @llvm.lifetime.end.p0(i64 40, ptr %abi_caller_copy), !dbg !1104
  %0 = load [2 x i64], ptr @static_72, align 8, !dbg !1105
  call fastcc void @core.print_2069([2 x i64] %0), !dbg !1105
  ret void, !dbg !1105
}

; Function Attrs: nounwind
define internal fastcc void @core.print_6214(ptr %t) #0 !dbg !1106 {
prelude:
  %abi_caller_copy3 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  %abi_caller_copy2 = alloca [5 x i64], align 8
  %abi_struct_int = alloca i64, align 8
  %abi_caller_copy = alloca [5 x i64], align 8
  %0 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %1 = alloca { { { ptr, i64 } } }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %2 = load i8, ptr @_root__core__k1__is-static, align 1, !dbg !1110
  %3 = trunc i8 %2 to i1, !dbg !1111
  br i1 %3, label %arm_cons, label %arm_cons1, !dbg !1111

arm_cons:                                         ; preds = %entry
  call void @llvm.lifetime.start.p0(i64 40, ptr %abi_caller_copy2)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy2, ptr align 8 %t, i64 40, i1 false)
  %4 = call fastcc [2 x i64] @core.impl_6show.show_for_t9365_6225(ptr %abi_caller_copy2), !dbg !1112
  call void @llvm.lifetime.end.p0(i64 40, ptr %abi_caller_copy2), !dbg !1112
  store [2 x i64] %4, ptr %abi_pair_storage, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %abi_pair_storage, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @static_190, i64 16, i1 false), !dbg !1113
  %5 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %0, i32 0, i32 1, !dbg !1113
  store i64 26, ptr %5, align 8, !dbg !1113
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy3)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy3, ptr align 8 %0, i64 24, i1 false)
  %6 = load [2 x i64], ptr %1, align 8
  call void @core.k1.emit-compiler-message_245(ptr %abi_caller_copy3, i8 0, [2 x i64] %6), !dbg !1114
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy3), !dbg !1114
  br label %match_end, !dbg !1111

arm_cons1:                                        ; preds = %entry
  call void @llvm.lifetime.start.p0(i64 40, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %t, i64 40, i1 false)
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int)
  store i64 0, ptr %abi_struct_int, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int, ptr align 4 @_root__core__io__stdout, i64 4, i1 false)
  %7 = load i64, ptr %abi_struct_int, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int)
  call fastcc void @core.impl_5print.print-to_for_t9365_6226(ptr %abi_caller_copy, i64 %7), !dbg !1115
  call void @llvm.lifetime.end.p0(i64 40, ptr %abi_caller_copy), !dbg !1115
  br label %match_end, !dbg !1111

match_end:                                        ; preds = %arm_cons, %arm_cons1
  ret void, !dbg !1111
}

; Function Attrs: nounwind
define internal fastcc void @core.print_2069([2 x i64] %t) #0 !dbg !1116 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage2 = alloca [2 x i64], align 8
  %abi_struct_int = alloca i64, align 8
  %0 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %1 = alloca { { { ptr, i64 } } }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %t, ptr %abi_pair_storage, align 8, !dbg !1119
  br label %entry

entry:                                            ; preds = %prelude
  %2 = load i8, ptr @_root__core__k1__is-static, align 1, !dbg !1120
  %3 = trunc i8 %2 to i1, !dbg !1121
  br i1 %3, label %arm_cons, label %arm_cons1, !dbg !1121

arm_cons:                                         ; preds = %entry
  %4 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !1122
  %5 = call fastcc [2 x i64] @core.impl_6show.show_for_t33_1347([2 x i64] %4), !dbg !1122
  store [2 x i64] %5, ptr %abi_pair_storage2, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %abi_pair_storage2, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @static_190, i64 16, i1 false), !dbg !1123
  %6 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %0, i32 0, i32 1, !dbg !1123
  store i64 26, ptr %6, align 8, !dbg !1123
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %0, i64 24, i1 false)
  %7 = load [2 x i64], ptr %1, align 8
  call void @core.k1.emit-compiler-message_245(ptr %abi_caller_copy, i8 0, [2 x i64] %7), !dbg !1124
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !1124
  br label %match_end, !dbg !1121

arm_cons1:                                        ; preds = %entry
  %8 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !1125
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int)
  store i64 0, ptr %abi_struct_int, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int, ptr align 4 @_root__core__io__stdout, i64 4, i1 false)
  %9 = load i64, ptr %abi_struct_int, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int)
  call fastcc void @core.impl_5print.print-to_for_t33_1957([2 x i64] %8, i64 %9), !dbg !1125
  br label %match_end, !dbg !1121

match_end:                                        ; preds = %arm_cons, %arm_cons1
  ret void, !dbg !1121
}

; Function Attrs: nounwind
define internal fastcc void @core.impl_5print.print-to_for_t33_1957([2 x i64] %self, i64 %w) #0 !dbg !1126 {
prelude:
  %abi_struct_int = alloca i64, align 8
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %0 = alloca { { ptr, i64 } }, align 8
  %1 = alloca { { ptr, i64 } }, align 8
  %2 = alloca { { ptr, i64 } }, align 8
  %3 = alloca { { ptr, i64 } }, align 8
  %struct_in_integer_storage = alloca { i32 }, align 4
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !1130
  %4 = trunc i64 %w to i32, !dbg !1130
  store i32 %4, ptr %struct_in_integer_storage, align 4
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !1131
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %2, i64 16, i1 false), !dbg !1131
  %5 = load [2 x i64], ptr %3, align 8, !dbg !1131
  %6 = call fastcc [2 x i64] @core.span.as-byte-span_1175([2 x i64] %5), !dbg !1131
  store [2 x i64] %6, ptr %abi_pair_storage1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %abi_pair_storage1, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %1, i64 16, i1 false), !dbg !1131
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int)
  store i64 0, ptr %abi_struct_int, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int, ptr align 4 %struct_in_integer_storage, i64 4, i1 false)
  %7 = load i64, ptr %abi_struct_int, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int)
  %8 = load [2 x i64], ptr %0, align 8
  call fastcc void @core.io.impl_4writer.write-bytes_for_t162_586(i64 %7, [2 x i64] %8), !dbg !1131
  ret void, !dbg !1131
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.impl_6show.show_for_t33_1347([2 x i64] %self) #0 !dbg !1132 {
prelude:
  %0 = alloca { { { ptr, i64 } } }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !1135
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !1136
  %1 = load [2 x i64], ptr %0, align 8, !dbg !1136
  ret [2 x i64] %1, !dbg !1136
}

; Function Attrs: nounwind
define internal fastcc void @core.impl_5print.print-to_for_t9365_6226(ptr %self, i64 %w) #0 !dbg !1137 {
prelude:
  %abi_struct_int5 = alloca i64, align 8
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_struct_int4 = alloca i64, align 8
  %abi_struct_int3 = alloca i64, align 8
  %abi_struct_int = alloca i64, align 8
  %0 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %struct_in_integer_storage = alloca { i32 }, align 4
  %3 = trunc i64 %w to i32, !dbg !1141
  store i32 %3, ptr %struct_in_integer_storage, align 4, !dbg !1141
  br label %entry

entry:                                            ; preds = %prelude
  store i64 5, ptr %2, align 8, !dbg !1142
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int)
  store i64 0, ptr %abi_struct_int, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int, ptr align 4 %struct_in_integer_storage, i64 4, i1 false)
  %4 = load i64, ptr %abi_struct_int, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int)
  call fastcc void @core.io.impl_4writer.write-char_for_t162_587(i64 %4, i8 91), !dbg !1143
  store i64 0, ptr %1, align 8, !dbg !1144
  br label %while_loop_condition, !dbg !1145

while_loop_condition:                             ; preds = %match_end2, %entry
  %5 = load i64, ptr %1, align 8, !dbg !1146
  %6 = load i64, ptr %2, align 8, !dbg !1147
  %7 = icmp slt i64 %5, %6, !dbg !1146
  %8 = zext i1 %7 to i8, !dbg !1146
  %9 = trunc i8 %8 to i1, !dbg !1145
  br i1 %9, label %arm_cond, label %while_loop_end, !dbg !1145

while_loop_end:                                   ; preds = %while_loop_condition
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int3)
  store i64 0, ptr %abi_struct_int3, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int3, ptr align 4 %struct_in_integer_storage, i64 4, i1 false)
  %10 = load i64, ptr %abi_struct_int3, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int3)
  call fastcc void @core.io.impl_4writer.write-char_for_t162_587(i64 %10, i8 93), !dbg !1148
  ret void, !dbg !1148

arm_cond:                                         ; preds = %while_loop_condition
  %11 = load i64, ptr %1, align 8, !dbg !1149
  %12 = icmp sgt i64 %11, 0, !dbg !1149
  %13 = zext i1 %12 to i8, !dbg !1149
  %14 = trunc i8 %13 to i1, !dbg !1150
  br i1 %14, label %arm_cons, label %match_end, !dbg !1150

arm_cons:                                         ; preds = %arm_cond
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int4)
  store i64 0, ptr %abi_struct_int4, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int4, ptr align 4 %struct_in_integer_storage, i64 4, i1 false)
  %15 = load i64, ptr %abi_struct_int4, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int4)
  call fastcc void @core.io.impl_4writer.write-char_for_t162_587(i64 %15, i8 44), !dbg !1151
  br label %match_end, !dbg !1150

match_end:                                        ; preds = %arm_cons, %arm_cond
  %16 = load i64, ptr %1, align 8, !dbg !1152
  %17 = icmp slt i64 %16, 5, !dbg !1152
  %18 = zext i1 %17 to i8, !dbg !1152
  %19 = trunc i8 %18 to i1, !dbg !1153
  br i1 %19, label %match_end2, label %arm_cons1, !dbg !1153

arm_cons1:                                        ; preds = %match_end
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @static_26, i64 16, i1 false), !dbg !1153
  %20 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %0, i32 0, i32 1, !dbg !1153
  store i64 1050, ptr %20, align 8, !dbg !1153
  %21 = load i64, ptr %1, align 8, !dbg !1152
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %0, i64 24, i1 false)
  %22 = load [2 x i64], ptr @static_53, align 8
  call fastcc void @core.crash-bounds_336(ptr %abi_caller_copy, i64 5, i64 %21, [2 x i64] %22), !dbg !1153
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !1153
  unreachable, !dbg !1153

match_end2:                                       ; preds = %match_end
  %23 = load i64, ptr %1, align 8, !dbg !1152
  %24 = getelementptr inbounds i64, ptr %self, i64 %23, !dbg !1153
  %25 = load i64, ptr %24, align 8, !dbg !1153
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int5)
  store i64 0, ptr %abi_struct_int5, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int5, ptr align 4 %struct_in_integer_storage, i64 4, i1 false)
  %26 = load i64, ptr %abi_struct_int5, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int5)
  call fastcc void @core.impl_5print.print-to_for_t9_6229(i64 %25, i64 %26), !dbg !1153
  %27 = load i64, ptr %1, align 8, !dbg !1154
  %28 = add i64 %27, 1, !dbg !1154
  store i64 %28, ptr %1, align 8, !dbg !1155
  br label %while_loop_condition, !dbg !1145
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.impl_6show.show_for_t9365_6225(ptr %self) #0 !dbg !1156 {
prelude:
  %abi_pair_storage = alloca [2 x i64], align 8
  %abi_caller_copy = alloca [5 x i64], align 8
  %0 = alloca { { { ptr, i64 } } }, align 8
  %1 = alloca { { { ptr, i64 }, i64 } }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  call fastcc void @core.string-builder.new_1255(ptr noalias sret({ { { ptr, i64 }, i64 } }) align 8 dereferenceable(24) %1), !dbg !1159
  call void @llvm.lifetime.start.p0(i64 40, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %self, i64 40, i1 false)
  call fastcc void @core.impl_5print.print-to_for_t9365_6228(ptr %abi_caller_copy, ptr %1), !dbg !1160
  call void @llvm.lifetime.end.p0(i64 40, ptr %abi_caller_copy), !dbg !1160
  %2 = call fastcc [2 x i64] @core.string-builder.build_1258(ptr %1), !dbg !1161
  store [2 x i64] %2, ptr %abi_pair_storage, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %abi_pair_storage, i64 16, i1 false)
  %3 = load [2 x i64], ptr %0, align 8, !dbg !1161
  ret [2 x i64] %3, !dbg !1161
}

; Function Attrs: nounwind
define internal fastcc void @core.impl_5print.print-to_for_t9365_6228(ptr %self, ptr %w) #0 !dbg !1162 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %0 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  br label %entry

entry:                                            ; preds = %prelude
  store i64 5, ptr %2, align 8, !dbg !1166
  call fastcc void @core.impl_4writer.write-char_for_t2342_1262(ptr %w, i8 91), !dbg !1167
  store i64 0, ptr %1, align 8, !dbg !1168
  br label %while_loop_condition, !dbg !1169

while_loop_condition:                             ; preds = %match_end2, %entry
  %3 = load i64, ptr %1, align 8, !dbg !1170
  %4 = load i64, ptr %2, align 8, !dbg !1171
  %5 = icmp slt i64 %3, %4, !dbg !1170
  %6 = zext i1 %5 to i8, !dbg !1170
  %7 = trunc i8 %6 to i1, !dbg !1169
  br i1 %7, label %arm_cond, label %while_loop_end, !dbg !1169

while_loop_end:                                   ; preds = %while_loop_condition
  call fastcc void @core.impl_4writer.write-char_for_t2342_1262(ptr %w, i8 93), !dbg !1172
  ret void, !dbg !1172

arm_cond:                                         ; preds = %while_loop_condition
  %8 = load i64, ptr %1, align 8, !dbg !1173
  %9 = icmp sgt i64 %8, 0, !dbg !1173
  %10 = zext i1 %9 to i8, !dbg !1173
  %11 = trunc i8 %10 to i1, !dbg !1174
  br i1 %11, label %arm_cons, label %match_end, !dbg !1174

arm_cons:                                         ; preds = %arm_cond
  call fastcc void @core.impl_4writer.write-char_for_t2342_1262(ptr %w, i8 44), !dbg !1175
  br label %match_end, !dbg !1174

match_end:                                        ; preds = %arm_cons, %arm_cond
  %12 = load i64, ptr %1, align 8, !dbg !1176
  %13 = icmp slt i64 %12, 5, !dbg !1176
  %14 = zext i1 %13 to i8, !dbg !1176
  %15 = trunc i8 %14 to i1, !dbg !1177
  br i1 %15, label %match_end2, label %arm_cons1, !dbg !1177

arm_cons1:                                        ; preds = %match_end
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @static_26, i64 16, i1 false), !dbg !1177
  %16 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %0, i32 0, i32 1, !dbg !1177
  store i64 1050, ptr %16, align 8, !dbg !1177
  %17 = load i64, ptr %1, align 8, !dbg !1176
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %0, i64 24, i1 false)
  %18 = load [2 x i64], ptr @static_53, align 8
  call fastcc void @core.crash-bounds_336(ptr %abi_caller_copy, i64 5, i64 %17, [2 x i64] %18), !dbg !1177
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !1177
  unreachable, !dbg !1177

match_end2:                                       ; preds = %match_end
  %19 = load i64, ptr %1, align 8, !dbg !1176
  %20 = getelementptr inbounds i64, ptr %self, i64 %19, !dbg !1177
  %21 = load i64, ptr %20, align 8, !dbg !1177
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %21, ptr %w), !dbg !1177
  %22 = load i64, ptr %1, align 8, !dbg !1178
  %23 = add i64 %22, 1, !dbg !1178
  store i64 %23, ptr %1, align 8, !dbg !1179
  br label %while_loop_condition, !dbg !1169
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.string-builder.build_1258(ptr %self) #0 !dbg !1180 {
prelude:
  %abi_pair_storage2 = alloca [2 x i64], align 8
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  %abi_caller_copy = alloca { { ptr, i64 }, i64 }, align 8
  %0 = alloca { { ptr, i64 }, i64 }, align 8
  %1 = alloca { { { ptr, i64 } } }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %self, i64 24, i1 false), !dbg !1181
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %0, i64 24, i1 false)
  %2 = call fastcc [2 x i64] @core.impl_23as-span.as-span_for_t160_1754(ptr %abi_caller_copy), !dbg !1181
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !1181
  store [2 x i64] %2, ptr %abi_pair_storage, align 8
  %3 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !1182
  %4 = call fastcc [2 x i64] @core.string.wrap-bytes_1274([2 x i64] %3), !dbg !1182
  store [2 x i64] %4, ptr %abi_pair_storage1, align 8
  %5 = load [2 x i64], ptr %abi_pair_storage1, align 8, !dbg !1182
  %6 = call fastcc [2 x i64] @core.string.cloned_1270([2 x i64] %5), !dbg !1182
  store [2 x i64] %6, ptr %abi_pair_storage2, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %abi_pair_storage2, i64 16, i1 false)
  %7 = load [2 x i64], ptr %1, align 8, !dbg !1182
  ret [2 x i64] %7, !dbg !1182
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.impl_23as-span.as-span_for_t160_1754(ptr %self) #0 !dbg !1183 {
prelude:
  %abi_pair_storage = alloca [2 x i64], align 8
  %0 = alloca { { ptr, i64 } }, align 8
  %1 = alloca { ptr, i64 }, align 8
  %2 = alloca { ptr, i64 }, align 8
  %3 = alloca { { ptr, i64 } }, align 8
  %4 = alloca { ptr, i64 }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %self, i64 16, i1 false), !dbg !1186
  %5 = getelementptr inbounds nuw { { ptr, i64 }, i64 }, ptr %self, i32 0, i32 1, !dbg !1186
  %6 = load i64, ptr %5, align 8, !dbg !1186
  %7 = load [2 x i64], ptr %1, align 8, !dbg !1186
  %8 = call fastcc [2 x i64] @core.buffer.slice_1776([2 x i64] %7, i64 0, i64 %6), !dbg !1186
  store [2 x i64] %8, ptr %abi_pair_storage, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %abi_pair_storage, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %2, i64 16, i1 false), !dbg !1186
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %4, i64 16, i1 false), !dbg !1187
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %3, i64 16, i1 false), !dbg !1187
  %9 = load [2 x i64], ptr %0, align 8, !dbg !1187
  ret [2 x i64] %9, !dbg !1187
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.string.wrap-bytes_1274([2 x i64] %span) #0 !dbg !1188 {
prelude:
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %0 = alloca { { { ptr, i64 } } }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %span, ptr %abi_pair_storage, align 8, !dbg !1191
  br label %entry

entry:                                            ; preds = %prelude
  %1 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !1192
  %2 = call fastcc [2 x i64] @core.span.as-char-span_1174([2 x i64] %1), !dbg !1192
  store [2 x i64] %2, ptr %abi_pair_storage1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %abi_pair_storage1, i64 16, i1 false)
  %3 = load [2 x i64], ptr %0, align 8, !dbg !1193
  ret [2 x i64] %3, !dbg !1193
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.string.cloned_1270([2 x i64] %self) #0 !dbg !1194 {
prelude:
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %0 = alloca { { { ptr, i64 } } }, align 8
  %1 = alloca { { ptr, i64 } }, align 8
  %2 = alloca { { { ptr, i64 } } }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !1195
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !1196
  %3 = load [2 x i64], ptr %1, align 8, !dbg !1196
  %4 = call fastcc [2 x i64] @core.span.cloned_2700([2 x i64] %3), !dbg !1196
  store [2 x i64] %4, ptr %abi_pair_storage1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %abi_pair_storage1, i64 16, i1 false), !dbg !1197
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %0, i64 16, i1 false), !dbg !1197
  %5 = load [2 x i64], ptr %2, align 8, !dbg !1197
  ret [2 x i64] %5, !dbg !1197
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.span.cloned_2700([2 x i64] %self) #0 !dbg !1198 {
prelude:
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %0 = alloca { ptr, i64 }, align 8
  %1 = alloca { ptr, i64 }, align 8
  %2 = alloca { ptr, i64 }, align 8
  %3 = alloca { ptr, i64 }, align 8
  %4 = alloca { ptr, i64 }, align 8
  %5 = alloca { ptr, i64 }, align 8
  %6 = alloca { { ptr, i64 } }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !1201
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %5, ptr align 8 %abi_pair_storage, i64 16, i1 false), !dbg !1202
  %7 = getelementptr inbounds nuw { ptr, i64 }, ptr %5, i32 0, i32 1, !dbg !1202
  %8 = load i64, ptr %7, align 8, !dbg !1202
  %9 = call fastcc ptr @core.mem.current-arena_924(), !dbg !1202
  %10 = call fastcc [2 x i64] @core.buffer.allocate-in_1861(ptr %9, i64 %8), !dbg !1202
  store [2 x i64] %10, ptr %abi_pair_storage1, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %1, ptr align 8 %abi_pair_storage1, i64 16, i1 false)
  %11 = load [2 x i64], ptr %5, align 8, !dbg !1202
  %12 = load [2 x i64], ptr %1, align 8, !dbg !1202
  call fastcc void @core.buffer.copy_1862([2 x i64] %11, [2 x i64] %12), !dbg !1202
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 %1, i64 16, i1 false), !dbg !1202
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 %0, i64 16, i1 false), !dbg !1202
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %4, ptr align 8 %2, i64 16, i1 false), !dbg !1202
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %3, ptr align 8 %4, i64 16, i1 false), !dbg !1202
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %6, ptr align 8 %3, i64 16, i1 false), !dbg !1202
  %13 = load [2 x i64], ptr %6, align 8, !dbg !1203
  ret [2 x i64] %13, !dbg !1203
}

; Function Attrs: nounwind
define internal fastcc void @core.impl_4writer.write-char_for_t2342_1262(ptr %self, i8 %value) #0 !dbg !1204 {
prelude:
  br label %entry

entry:                                            ; preds = %prelude
  %0 = call fastcc ptr @core.mem.current-arena_924(), !dbg !1208
  call fastcc void @core.list.push-in_1941(ptr %self, ptr %0, i8 %value), !dbg !1208
  ret void, !dbg !1208
}

; Function Attrs: nounwind
define internal fastcc void @core.io.impl_4writer.write-char_for_t162_587(i64 %self, i8 %value) #0 !dbg !1209 {
prelude:
  %abi_struct_int = alloca i64, align 8
  %struct_in_integer_storage = alloca { i32 }, align 4
  %0 = trunc i64 %self to i32, !dbg !1213
  store i32 %0, ptr %struct_in_integer_storage, align 4, !dbg !1213
  br label %entry

entry:                                            ; preds = %prelude
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int)
  store i64 0, ptr %abi_struct_int, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int, ptr align 4 %struct_in_integer_storage, i64 4, i1 false)
  %1 = load i64, ptr %abi_struct_int, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int)
  call fastcc void @core.io.impl_4writer.write-byte_for_t162_585(i64 %1, i8 %value), !dbg !1214
  ret void, !dbg !1214
}

; Function Attrs: nounwind
define internal fastcc void @core.impl_5print.print-to_for_t9_6229(i64 %self, i64 %w) #0 !dbg !1215 {
prelude:
  %abi_struct_int4 = alloca i64, align 8
  %abi_struct_int = alloca i64, align 8
  %0 = alloca i64, align 8
  %1 = alloca i8, align 1
  %struct_in_integer_storage = alloca { i32 }, align 4
  %2 = trunc i64 %w to i32, !dbg !1218
  store i32 %2, ptr %struct_in_integer_storage, align 4, !dbg !1218
  br label %entry

entry:                                            ; preds = %prelude
  %3 = icmp slt i64 %self, 0, !dbg !1219
  %4 = zext i1 %3 to i8, !dbg !1219
  store i8 %4, ptr %1, align 1, !dbg !1219
  %5 = load i8, ptr %1, align 1, !dbg !1220
  %6 = trunc i8 %5 to i1, !dbg !1221
  br i1 %6, label %arm_cons, label %match_end, !dbg !1221

arm_cons:                                         ; preds = %entry
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int)
  store i64 0, ptr %abi_struct_int, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int, ptr align 4 %struct_in_integer_storage, i64 4, i1 false)
  %7 = load i64, ptr %abi_struct_int, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int)
  call fastcc void @core.io.impl_4writer.write-char_for_t162_587(i64 %7, i8 45), !dbg !1222
  br label %match_end, !dbg !1221

match_end:                                        ; preds = %arm_cons, %entry
  %8 = load i8, ptr %1, align 1, !dbg !1223
  %9 = trunc i8 %8 to i1, !dbg !1224
  br i1 %9, label %arm_cons1, label %arm_cons2, !dbg !1224

arm_cons1:                                        ; preds = %match_end
  %10 = sub i64 0, %self, !dbg !1225
  store i64 %10, ptr %0, align 8, !dbg !1224
  br label %match_end3, !dbg !1224

arm_cons2:                                        ; preds = %match_end
  store i64 %self, ptr %0, align 8, !dbg !1224
  br label %match_end3, !dbg !1224

match_end3:                                       ; preds = %arm_cons1, %arm_cons2
  %11 = load i64, ptr %0, align 8, !dbg !1226
  %12 = call fastcc i64 @core.i64.unsigned_654(i64 %11), !dbg !1226
  call void @llvm.lifetime.start.p0(i64 8, ptr %abi_struct_int4)
  store i64 0, ptr %abi_struct_int4, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_struct_int4, ptr align 4 %struct_in_integer_storage, i64 4, i1 false)
  %13 = load i64, ptr %abi_struct_int4, align 8
  call void @llvm.lifetime.end.p0(i64 8, ptr %abi_struct_int4)
  call fastcc void @core.format-uint_1489(i64 %13, i64 %12, i64 10), !dbg !1227
  ret void, !dbg !1227
}

; Function Attrs: nounwind
define internal fastcc i64 @core.i64.unsigned_654(i64 %self) #0 !dbg !1228 {
prelude:
  br label %entry

entry:                                            ; preds = %prelude
  ret i64 %self, !dbg !1231
}

; Function Attrs: nounwind
define internal fastcc void @core.array.from-buffer_6212(ptr noalias sret([5 x i64]) align 8 dereferenceable(40) %sret, [2 x i64] %buffer) #0 !dbg !1232 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %0 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %buffer, ptr %abi_pair_storage, align 8, !dbg !1235
  br label %entry

entry:                                            ; preds = %prelude
  %1 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !1236
  %2 = call fastcc i64 @core.buffer.len_3306([2 x i64] %1), !dbg !1236
  %3 = icmp eq i64 %2, 5, !dbg !1236
  %4 = zext i1 %3 to i8, !dbg !1236
  %5 = trunc i8 %4 to i1, !dbg !1236
  %6 = xor i1 %5, true, !dbg !1236
  %7 = zext i1 %6 to i8, !dbg !1236
  %8 = trunc i8 %7 to i1, !dbg !1237
  br i1 %8, label %arm_cons, label %match_end, !dbg !1237

arm_cons:                                         ; preds = %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @static_26, i64 16, i1 false), !dbg !1238
  %9 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %0, i32 0, i32 1, !dbg !1238
  store i64 1022, ptr %9, align 8, !dbg !1238
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %0, i64 24, i1 false)
  %10 = load [2 x i64], ptr @static_59, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %10), !dbg !1238
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !1238
  unreachable, !dbg !1238

match_end:                                        ; preds = %entry
  %11 = call fastcc [2 x i64] @core.impl_22as-buffer.as-buffer_for_t9377_6215(ptr %sret), !dbg !1239
  store [2 x i64] %11, ptr %abi_pair_storage1, align 8
  %12 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !1240
  %13 = load [2 x i64], ptr %abi_pair_storage1, align 8, !dbg !1240
  call fastcc void @core.buffer.copy_3302([2 x i64] %12, [2 x i64] %13), !dbg !1240
  ret void, !dbg !1241
}

; Function Attrs: nounwind
define internal fastcc i64 @core.buffer.len_3306([2 x i64] %self) #0 !dbg !1242 {
prelude:
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !1245
  br label %entry

entry:                                            ; preds = %prelude
  %0 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !1246
  %1 = load i64, ptr %0, align 8, !dbg !1246
  ret i64 %1, !dbg !1246
}

; Function Attrs: nounwind
define internal fastcc [2 x i64] @core.impl_22as-buffer.as-buffer_for_t9377_6215(ptr %self) #0 !dbg !1247 {
prelude:
  %0 = alloca { ptr, i64 }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %1 = call fastcc ptr @core.array.get-base_6227(ptr %self), !dbg !1250
  store ptr %1, ptr %0, align 8, !dbg !1250
  %2 = getelementptr inbounds nuw { ptr, i64 }, ptr %0, i32 0, i32 1, !dbg !1251
  store i64 5, ptr %2, align 8, !dbg !1252
  %3 = load [2 x i64], ptr %0, align 8, !dbg !1251
  ret [2 x i64] %3, !dbg !1251
}

; Function Attrs: nounwind
define internal fastcc void @core.buffer.copy_3302([2 x i64] %src, [2 x i64] %dst) #0 !dbg !1253 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage3 = alloca [2 x i64], align 8
  %0 = alloca i64, align 8
  %1 = alloca { { { ptr, i64 }, i64 } }, align 8
  %2 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %abi_pair_storage1 = alloca [2 x i64], align 8
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %src, ptr %abi_pair_storage, align 8, !dbg !1257
  store [2 x i64] %dst, ptr %abi_pair_storage1, align 8
  br label %entry

entry:                                            ; preds = %prelude
  %3 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !1258
  %4 = load i64, ptr %3, align 8, !dbg !1258
  %5 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage1, i32 0, i32 1, !dbg !1259
  %6 = load i64, ptr %5, align 8, !dbg !1259
  %7 = icmp sgt i64 %4, %6, !dbg !1258
  %8 = zext i1 %7 to i8, !dbg !1258
  %9 = trunc i8 %8 to i1, !dbg !1260
  br i1 %9, label %arm_cons, label %arm_cond, !dbg !1260

arm_cons:                                         ; preds = %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 8 @static_126, i64 16, i1 false), !dbg !1261
  %10 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %2, i32 0, i32 1, !dbg !1261
  store i64 124, ptr %10, align 8, !dbg !1261
  call fastcc void @core.string-builder.new_1255(ptr noalias sret({ { { ptr, i64 }, i64 } }) align 8 dereferenceable(24) %1), !dbg !1262
  %11 = load [2 x i64], ptr @static_129, align 8, !dbg !1262
  call fastcc void @core.impl_5print.print-to_for_t33_1398([2 x i64] %11, ptr %1), !dbg !1262
  %12 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !1263
  %13 = load i64, ptr %12, align 8, !dbg !1263
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %13, ptr %1), !dbg !1263
  %14 = load [2 x i64], ptr @static_130, align 8, !dbg !1262
  call fastcc void @core.impl_5print.print-to_for_t33_1398([2 x i64] %14, ptr %1), !dbg !1262
  %15 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage1, i32 0, i32 1, !dbg !1264
  %16 = load i64, ptr %15, align 8, !dbg !1264
  call fastcc void @core.impl_5print.print-to_for_t9_1490(i64 %16, ptr %1), !dbg !1264
  %17 = call fastcc [2 x i64] @core.string-builder.build-tmp_1259(ptr %1), !dbg !1262
  store [2 x i64] %17, ptr %abi_pair_storage3, align 8
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %2, i64 24, i1 false)
  %18 = load [2 x i64], ptr %abi_pair_storage3, align 8
  call fastcc void @core.crash_335(ptr %abi_caller_copy, [2 x i64] %18), !dbg !1261
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !1261
  unreachable, !dbg !1261

arm_cond:                                         ; preds = %entry
  %19 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !1265
  %20 = load i64, ptr %19, align 8, !dbg !1265
  %21 = icmp eq i64 %20, 0, !dbg !1265
  %22 = zext i1 %21 to i8, !dbg !1265
  %23 = trunc i8 %22 to i1, !dbg !1266
  br i1 %23, label %arm_cons2, label %match_end, !dbg !1266

arm_cons2:                                        ; preds = %arm_cond
  ret void, !dbg !1267

match_end:                                        ; preds = %arm_cond
  %24 = getelementptr inbounds nuw { ptr, i64 }, ptr %abi_pair_storage, i32 0, i32 1, !dbg !1268
  %25 = load i64, ptr %24, align 8, !dbg !1268
  %26 = call fastcc i64 @core.buffer.buffer-size-bytes_3224(i64 %25), !dbg !1269
  store i64 %26, ptr %0, align 8, !dbg !1269
  %27 = load [2 x i64], ptr %abi_pair_storage1, align 8, !dbg !1270
  %28 = call fastcc ptr @core.buffer.data-ptr_1851([2 x i64] %27), !dbg !1270
  %29 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !1271
  %30 = call fastcc ptr @core.buffer.data-ptr_1851([2 x i64] %29), !dbg !1271
  %31 = load i64, ptr %0, align 8, !dbg !1272
  call void @core.mem.copy_234(ptr %28, ptr %30, i64 %31), !dbg !1273
  ret void, !dbg !1273
}

; Function Attrs: nounwind
define internal fastcc i64 @core.buffer.buffer-size-bytes_3224(i64 %count) #0 !dbg !1274 {
prelude:
  br label %entry

entry:                                            ; preds = %prelude
  %0 = mul i64 %count, 8, !dbg !1277
  ret i64 %0, !dbg !1277
}

; Function Attrs: nounwind
define internal fastcc ptr @core.buffer.data-ptr_1851([2 x i64] %self) #0 !dbg !1278 {
prelude:
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !1281
  br label %entry

entry:                                            ; preds = %prelude
  %0 = load ptr, ptr %abi_pair_storage, align 8, !dbg !1282
  ret ptr %0, !dbg !1282
}

; Function Attrs: nounwind
define internal fastcc void @core.impl_5print.print-to_for_t33_1398([2 x i64] %self, ptr %w) #0 !dbg !1283 {
prelude:
  %abi_pair_storage = alloca [2 x i64], align 8
  store [2 x i64] %self, ptr %abi_pair_storage, align 8, !dbg !1287
  br label %entry

entry:                                            ; preds = %prelude
  %0 = load [2 x i64], ptr %abi_pair_storage, align 8, !dbg !1288
  call fastcc void @core.impl_4writer.write-string_for_t2342_1263(ptr %w, [2 x i64] %0), !dbg !1288
  ret void, !dbg !1288
}

; Function Attrs: nounwind
define internal fastcc ptr @core.array.get-base_6227(ptr %array) #0 !dbg !1289 {
prelude:
  %abi_caller_copy = alloca { { { { ptr, i64 } } }, i64 }, align 8
  %0 = alloca { { { { ptr, i64 } } }, i64 }, align 8
  br label %entry

entry:                                            ; preds = %prelude
  br i1 true, label %match_end, label %arm_cons, !dbg !1292

arm_cons:                                         ; preds = %entry
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @static_26, i64 16, i1 false), !dbg !1292
  %1 = getelementptr inbounds nuw { { { { ptr, i64 } } }, i64 }, ptr %0, i32 0, i32 1, !dbg !1292
  store i64 985, ptr %1, align 8, !dbg !1292
  call void @llvm.lifetime.start.p0(i64 24, ptr %abi_caller_copy)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %abi_caller_copy, ptr align 8 %0, i64 24, i1 false)
  %2 = load [2 x i64], ptr @static_53, align 8
  call fastcc void @core.crash-bounds_336(ptr %abi_caller_copy, i64 5, i64 0, [2 x i64] %2), !dbg !1292
  call void @llvm.lifetime.end.p0(i64 24, ptr %abi_caller_copy), !dbg !1292
  unreachable, !dbg !1292

match_end:                                        ; preds = %entry
  %3 = getelementptr inbounds i64, ptr %array, i64 0, !dbg !1292
  ret ptr %3, !dbg !1292
}

define i32 @main() {
entry:
  %0 = call i32 @sandbox.main_6208()
  call fastcc void @core.k1.program-exit_244(i32 %0)
  unreachable
}

attributes #0 = { nounwind "frame-pointer"="non-leaf" }
attributes #1 = { noreturn nounwind "frame-pointer"="non-leaf" }
attributes #2 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #3 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #4 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3, !4, !5, !6}

!0 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: "k1_compiler", isOptimized: false, runtimeVersion: 0, emissionKind: LineTablesOnly, splitDebugInlining: false, sysroot: "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk", sdk: "MacOSX.sdk")
!1 = !DIFile(filename: "builtin.k1", directory: "/Users/knix/dev/k1/modules/core")
!2 = !{i32 2, !"SDK Version", [2 x i32] [i32 15, i32 0]}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"PIC Level", i32 2}
!6 = !{i32 1, !"PIE Level", i32 2}
!7 = distinct !DISubprogram(name: "sandbox.main_6208", linkageName: "sandbox.main_6208", scope: !8, file: !8, line: 2, type: !9, scopeLine: 2, spFlags: DISPFlagDefinition, unit: !0)
!8 = !DIFile(filename: "sandbox.k1", directory: "/Users/knix/dev/k1/sandbox")
!9 = !DISubroutineType(types: !10)
!10 = !{!11}
!11 = !DIBasicType(name: "i32", size: 32, encoding: DW_ATE_signed)
!12 = !DILocation(line: 17, column: 3, scope: !7)
!13 = !DILocation(line: 18, column: 3, scope: !7)
!14 = distinct !DISubprogram(name: "core.k1.program-exit_244", linkageName: "core.k1.program-exit_244", scope: !1, file: !1, line: 775, type: !15, scopeLine: 775, spFlags: DISPFlagDefinition, unit: !0)
!15 = !DISubroutineType(types: !16)
!16 = !{!17, !11}
!17 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", file: !1, line: 1, align: 1, elements: !18, identifier: "empty")
!18 = !{}
!19 = !DILocation(line: 776, column: 5, scope: !14)
!20 = distinct !DISubprogram(name: "core.sys.exit_255", linkageName: "core.sys.exit_255", scope: !1, file: !1, line: 899, type: !21, scopeLine: 899, spFlags: DISPFlagDefinition, unit: !0)
!21 = !DISubroutineType(types: !22)
!22 = !{!23, !11}
!23 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !14, file: !1, line: 1, align: 1, elements: !18, identifier: "empty")
!24 = !DILocation(line: 900, column: 5, scope: !20)
!25 = !DILocation(line: 901, column: 5, scope: !20)
!26 = distinct !DISubprogram(name: "core.io.flush-stdout_583", linkageName: "core.io.flush-stdout_583", scope: !27, file: !27, line: 82, type: !28, scopeLine: 82, spFlags: DISPFlagDefinition, unit: !0)
!27 = !DIFile(filename: "core.k1", directory: "/Users/knix/dev/k1/modules/core")
!28 = !DISubroutineType(types: !29)
!29 = !{!30}
!30 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !20, file: !1, line: 1, align: 1, elements: !18, identifier: "empty")
!31 = !DILocation(line: 83, column: 8, scope: !26)
!32 = !DILocation(line: 83, column: 5, scope: !26)
!33 = !DILocation(line: 84, column: 20, scope: !26)
!34 = !DILocation(line: 84, column: 53, scope: !26)
!35 = !DILocation(line: 84, column: 7, scope: !26)
!36 = !DILocation(line: 85, column: 7, scope: !26)
!37 = !DISubprogram(name: "exit", linkageName: "exit", scope: !38, file: !38, line: 475, type: !39, scopeLine: 475, spFlags: 0)
!38 = !DIFile(filename: "platform.k1", directory: "/Users/knix/dev/k1/modules/core")
!39 = !DISubroutineType(types: !40)
!40 = !{!30, !11}
!41 = distinct !DISubprogram(name: "core.k1.exit_243", linkageName: "core.k1.exit_243", scope: !1, file: !1, line: 773, type: !39, scopeLine: 773, spFlags: DISPFlagDefinition, unit: !0)
!42 = !DILocation(line: 773, column: 14, scope: !41)
!43 = distinct !DISubprogram(name: "core.impl_23as-span.as-span_for_t3313_1990", linkageName: "core.impl_23as-span.as-span_for_t3313_1990", scope: !1, file: !1, line: 621, type: !44, scopeLine: 621, spFlags: DISPFlagDefinition, unit: !0)
!44 = !DISubroutineType(types: !45)
!45 = !{!46, !52}
!46 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.span_of_u8_", scope: !26, file: !27, line: 82, size: 16, align: 8, elements: !47, identifier: "core.span_of_u8_")
!47 = !{!48}
!48 = !DIDerivedType(tag: DW_TAG_member, name: "buffer", scope: !26, file: !27, line: 82, baseType: !49, size: 128, align: 64)
!49 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.buffer_of_u8_", scope: !26, file: !27, line: 82, size: 16, align: 8, elements: !50, identifier: "core.buffer_of_u8_")
!50 = !{!51, !53}
!51 = !DIDerivedType(tag: DW_TAG_member, name: "data", scope: !26, file: !27, line: 82, baseType: !52, size: 64, align: 64)
!52 = !DIBasicType(name: "ptr", size: 64, encoding: DW_ATE_address)
!53 = !DIDerivedType(tag: DW_TAG_member, name: "len", scope: !26, file: !27, line: 82, baseType: !54, size: 64, align: 64, offset: 8)
!54 = !DIBasicType(name: "i64", size: 64, encoding: DW_ATE_signed)
!55 = !DILocation(line: 621, column: 48, scope: !43)
!56 = !DILocation(line: 621, column: 31, scope: !43)
!57 = distinct !DISubprogram(name: "core.io.write-all_582", linkageName: "core.io.write-all_582", scope: !27, file: !27, line: 66, type: !58, scopeLine: 66, spFlags: DISPFlagDefinition, unit: !0)
!58 = !DISubroutineType(types: !59)
!59 = !{!60, !11, !52, !54}
!60 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !26, file: !27, line: 1, align: 1, elements: !18, identifier: "empty")
!61 = !DILocation(line: 67, column: 25, scope: !57)
!62 = !DILocation(line: 68, column: 5, scope: !57)
!63 = !DILocation(line: 68, column: 11, scope: !57)
!64 = !DILocation(line: 70, column: 25, scope: !57)
!65 = !DILocation(line: 71, column: 31, scope: !57)
!66 = !DILocation(line: 71, column: 16, scope: !57)
!67 = !DILocation(line: 72, column: 25, scope: !57)
!68 = !DILocation(line: 72, column: 17, scope: !57)
!69 = !DILocation(line: 69, column: 15, scope: !57)
!70 = !DILocation(line: 74, column: 9, scope: !57)
!71 = !DILocation(line: 75, column: 9, scope: !57)
!72 = !DILocation(line: 74, column: 19, scope: !57)
!73 = !DILocation(line: 77, column: 10, scope: !57)
!74 = !DILocation(line: 77, column: 7, scope: !57)
!75 = !DILocation(line: 75, column: 20, scope: !57)
!76 = !DILocation(line: 77, column: 17, scope: !57)
!77 = !DILocation(line: 78, column: 17, scope: !57)
!78 = !DILocation(line: 78, column: 27, scope: !57)
!79 = !DILocation(line: 78, column: 7, scope: !57)
!80 = distinct !DISubprogram(name: "core.platform.port.posix.write_1043", linkageName: "core.platform.port.posix.write_1043", scope: !38, file: !38, line: 564, type: !81, scopeLine: 564, spFlags: DISPFlagDefinition, unit: !0)
!81 = !DISubroutineType(types: !82)
!82 = !{!54, !11, !52, !54}
!83 = !DILocation(line: 565, column: 17, scope: !80)
!84 = !DILocation(line: 566, column: 9, scope: !80)
!85 = !DILocation(line: 566, column: 15, scope: !80)
!86 = !DILocation(line: 567, column: 15, scope: !80)
!87 = !DILocation(line: 567, column: 11, scope: !80)
!88 = !DILocation(line: 569, column: 12, scope: !80)
!89 = !DILocation(line: 569, column: 9, scope: !80)
!90 = !DILocation(line: 566, column: 25, scope: !80)
!91 = !DILocation(line: 566, column: 42, scope: !80)
!92 = !DILocation(line: 569, column: 24, scope: !80)
!93 = !DILocation(line: 569, column: 20, scope: !80)
!94 = !DILocation(line: 569, column: 54, scope: !80)
!95 = distinct !DISubprogram(name: "core.platform.io.impl_16try.result_for_t185_962", linkageName: "core.platform.io.impl_16try.result_for_t185_962", scope: !1, file: !1, line: 432, type: !96, scopeLine: 432, spFlags: DISPFlagDefinition, unit: !0)
!96 = !DISubroutineType(types: !97)
!97 = !{!98, !105}
!98 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.result_of_i64__i32_", scope: !57, file: !27, line: 66, size: 16, align: 8, elements: !99, identifier: "core.result_of_i64__i32_")
!99 = !{!100, !102}
!100 = !DIDerivedType(tag: DW_TAG_member, name: "tag", scope: !57, file: !27, line: 66, baseType: !101, size: 8, align: 8)
!101 = !DIBasicType(name: "u8", size: 8, encoding: DW_ATE_unsigned)
!102 = !DIDerivedType(tag: DW_TAG_member, name: "payload", scope: !57, file: !27, line: 66, baseType: !103, size: 64, align: 64, offset: 8)
!103 = !DICompositeType(tag: DW_TAG_union_type, name: "core.result_of_i64__i32_", scope: !57, file: !27, line: 66, size: 64, align: 64, elements: !104, identifier: "core.result_of_i64__i32_")
!104 = !{!54, !11}
!105 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.platform.io.io-result", scope: !57, file: !27, line: 66, size: 8, align: 8, elements: !106, identifier: "core.platform.io.io-result")
!106 = !{!107}
!107 = !DIDerivedType(tag: DW_TAG_member, name: "raw", scope: !57, file: !27, line: 66, baseType: !54, size: 64, align: 64)
!108 = !DILocation(line: 432, column: 6, scope: !95)
!109 = !DILocation(line: 433, column: 8, scope: !95)
!110 = !DILocation(line: 433, column: 5, scope: !95)
!111 = !DILocation(line: 433, column: 21, scope: !95)
!112 = !DILocation(line: 433, column: 25, scope: !95)
!113 = !DILocation(line: 433, column: 48, scope: !95)
!114 = !DILocation(line: 433, column: 53, scope: !95)
!115 = distinct !DISubprogram(name: "core.crash_335", linkageName: "core.crash_335", scope: !1, file: !1, line: 1073, type: !116, scopeLine: 1073, spFlags: DISPFlagDefinition, unit: !0)
!116 = !DISubroutineType(types: !117)
!117 = !{!118, !119, !122}
!118 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !57, file: !27, line: 1, align: 1, elements: !18, identifier: "empty")
!119 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.k1.source-location", scope: !57, file: !27, line: 66, size: 24, align: 8, elements: !120, identifier: "core.k1.source-location")
!120 = !{!121, !132}
!121 = !DIDerivedType(tag: DW_TAG_member, name: "filename", scope: !57, file: !27, line: 66, baseType: !122, size: 128, align: 64)
!122 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.string", scope: !57, file: !27, line: 66, size: 16, align: 8, elements: !123, identifier: "core.string")
!123 = !{!124}
!124 = !DIDerivedType(tag: DW_TAG_member, name: "span", scope: !57, file: !27, line: 66, baseType: !125, size: 128, align: 64)
!125 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.span_of_char_", scope: !57, file: !27, line: 66, size: 16, align: 8, elements: !126, identifier: "core.span_of_char_")
!126 = !{!127}
!127 = !DIDerivedType(tag: DW_TAG_member, name: "buffer", scope: !57, file: !27, line: 66, baseType: !128, size: 128, align: 64)
!128 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.buffer_of_char_", scope: !57, file: !27, line: 66, size: 16, align: 8, elements: !129, identifier: "core.buffer_of_char_")
!129 = !{!130, !131}
!130 = !DIDerivedType(tag: DW_TAG_member, name: "data", scope: !57, file: !27, line: 66, baseType: !52, size: 64, align: 64)
!131 = !DIDerivedType(tag: DW_TAG_member, name: "len", scope: !57, file: !27, line: 66, baseType: !54, size: 64, align: 64, offset: 8)
!132 = !DIDerivedType(tag: DW_TAG_member, name: "line", scope: !57, file: !27, line: 66, baseType: !133, size: 64, align: 64, offset: 16)
!133 = !DIBasicType(name: "u64", size: 64, encoding: DW_ATE_unsigned)
!134 = !DILocation(line: 1073, column: 4, scope: !115)
!135 = !DILocation(line: 1082, column: 16, scope: !115)
!136 = !DILocation(line: 1082, column: 10, scope: !115)
!137 = !DILocation(line: 1083, column: 5, scope: !115)
!138 = !DILocation(line: 1084, column: 5, scope: !115)
!139 = !DILocation(line: 1086, column: 5, scope: !115)
!140 = !DILocation(line: 1087, column: 5, scope: !115)
!141 = !DILocation(line: 1090, column: 5, scope: !115)
!142 = !DILocation(line: 1091, column: 5, scope: !115)
!143 = !DILocation(line: 1092, column: 12, scope: !115)
!144 = !DILocation(line: 1092, column: 5, scope: !115)
!145 = !DILocation(line: 1093, column: 5, scope: !115)
!146 = !DILocation(line: 1094, column: 46, scope: !115)
!147 = !DILocation(line: 1094, column: 5, scope: !115)
!148 = !DILocation(line: 1095, column: 5, scope: !115)
!149 = !DILocation(line: 1097, column: 5, scope: !115)
!150 = !DISubprogram(name: "_k1_print_backtrace", linkageName: "_k1_print_backtrace", scope: !27, file: !27, line: 574, type: !151, scopeLine: 574, spFlags: 0)
!151 = !DISubroutineType(types: !152)
!152 = !{!153, !11}
!153 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !115, file: !1, line: 1, align: 1, elements: !18, identifier: "empty")
!154 = distinct !DISubprogram(name: "core.eprint_1488", linkageName: "core.eprint_1488", scope: !27, file: !27, line: 36, type: !155, scopeLine: 36, spFlags: DISPFlagDefinition, unit: !0)
!155 = !DISubroutineType(types: !156)
!156 = !{!153, !122}
!157 = !DILocation(line: 36, column: 4, scope: !154)
!158 = !DILocation(line: 37, column: 9, scope: !154)
!159 = !DILocation(line: 37, column: 3, scope: !154)
!160 = !DILocation(line: 38, column: 13, scope: !154)
!161 = !DILocation(line: 39, column: 30, scope: !154)
!162 = !DILocation(line: 39, column: 5, scope: !154)
!163 = !DILocation(line: 41, column: 5, scope: !154)
!164 = distinct !DISubprogram(name: "core.format-uint_1489", linkageName: "core.format-uint_1489", scope: !27, file: !27, line: 303, type: !165, scopeLine: 303, spFlags: DISPFlagDefinition, unit: !0)
!165 = !DISubroutineType(types: !166)
!166 = !{!153, !167, !133, !133}
!167 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.io.out-stream", scope: !115, file: !1, line: 1073, size: 4, align: 4, elements: !168, identifier: "core.io.out-stream")
!168 = !{!169}
!169 = !DIDerivedType(tag: DW_TAG_member, name: "file-no", scope: !115, file: !1, line: 1073, baseType: !11, size: 32, align: 32)
!170 = !DILocation(line: 303, column: 4, scope: !164)
!171 = !DILocation(line: 304, column: 6, scope: !164)
!172 = !DILocation(line: 304, column: 3, scope: !164)
!173 = !DILocation(line: 304, column: 19, scope: !164)
!174 = !DILocation(line: 304, column: 38, scope: !164)
!175 = !DILocation(line: 305, column: 11, scope: !164)
!176 = !DILocation(line: 305, column: 8, scope: !164)
!177 = !DILocation(line: 306, column: 35, scope: !164)
!178 = !DILocation(line: 306, column: 17, scope: !164)
!179 = !DILocation(line: 307, column: 18, scope: !164)
!180 = !DILocation(line: 307, column: 5, scope: !164)
!181 = !DILocation(line: 308, column: 21, scope: !164)
!182 = !DILocation(line: 308, column: 13, scope: !164)
!183 = !DILocation(line: 308, column: 10, scope: !164)
!184 = !DILocation(line: 309, column: 17, scope: !164)
!185 = !DILocation(line: 310, column: 41, scope: !164)
!186 = !DILocation(line: 310, column: 23, scope: !164)
!187 = !DILocation(line: 311, column: 18, scope: !164)
!188 = !DILocation(line: 312, column: 42, scope: !164)
!189 = !DILocation(line: 312, column: 24, scope: !164)
!190 = !DILocation(line: 313, column: 18, scope: !164)
!191 = !DILocation(line: 313, column: 5, scope: !164)
!192 = !DILocation(line: 314, column: 18, scope: !164)
!193 = !DILocation(line: 314, column: 5, scope: !164)
!194 = !DILocation(line: 316, column: 21, scope: !164)
!195 = !DILocation(line: 317, column: 18, scope: !164)
!196 = !DILocation(line: 318, column: 5, scope: !164)
!197 = !DILocation(line: 318, column: 11, scope: !164)
!198 = !DILocation(line: 319, column: 15, scope: !164)
!199 = !DILocation(line: 320, column: 33, scope: !164)
!200 = !DILocation(line: 320, column: 15, scope: !164)
!201 = !DILocation(line: 321, column: 19, scope: !164)
!202 = !DILocation(line: 321, column: 7, scope: !164)
!203 = !DILocation(line: 322, column: 19, scope: !164)
!204 = !DILocation(line: 322, column: 7, scope: !164)
!205 = !DILocation(line: 325, column: 5, scope: !164)
!206 = !DILocation(line: 327, column: 19, scope: !164)
!207 = !DILocation(line: 327, column: 5, scope: !164)
!208 = !DISubprogram(name: "abort", linkageName: "abort", scope: !38, file: !38, line: 476, type: !209, scopeLine: 476, spFlags: 0)
!209 = !DISubroutineType(types: !210)
!210 = !{!153}
!211 = distinct !DISubprogram(name: "core.k1.emit-compiler-message_245", linkageName: "core.k1.emit-compiler-message_245", scope: !1, file: !1, line: 779, type: !212, scopeLine: 779, spFlags: DISPFlagDefinition, unit: !0)
!212 = !DISubroutineType(types: !213)
!213 = !{!153, !119, !101, !122}
!214 = !DILocation(line: 779, column: 14, scope: !211)
!215 = distinct !DISubprogram(name: "core.impl_22as-buffer.as-buffer_for_t3265_1928", linkageName: "core.impl_22as-buffer.as-buffer_for_t3265_1928", scope: !1, file: !1, line: 1032, type: !216, scopeLine: 1032, spFlags: DISPFlagDefinition, unit: !0)
!216 = !DISubroutineType(types: !217)
!217 = !{!128, !52}
!218 = !DILocation(line: 1033, column: 33, scope: !215)
!219 = !DILocation(line: 1033, column: 5, scope: !215)
!220 = !DILocation(line: 1033, column: 14, scope: !215)
!221 = distinct !DISubprogram(name: "core.buffer.slice_1938", linkageName: "core.buffer.slice_1938", scope: !222, file: !222, line: 197, type: !223, scopeLine: 197, spFlags: DISPFlagDefinition, unit: !0)
!222 = !DIFile(filename: "buffer.k1", directory: "/Users/knix/dev/k1/modules/core")
!223 = !DISubroutineType(types: !224)
!224 = !{!128, !128, !54, !54}
!225 = !DILocation(line: 197, column: 6, scope: !221)
!226 = !DILocation(line: 198, column: 8, scope: !221)
!227 = !DILocation(line: 199, column: 7, scope: !221)
!228 = !DILocation(line: 199, column: 13, scope: !221)
!229 = !DILocation(line: 199, column: 48, scope: !221)
!230 = !DILocation(line: 199, column: 68, scope: !221)
!231 = !DILocation(line: 198, column: 29, scope: !221)
!232 = !DILocation(line: 198, column: 21, scope: !221)
!233 = !DILocation(line: 198, column: 5, scope: !221)
!234 = !DILocation(line: 202, column: 30, scope: !221)
!235 = !DILocation(line: 202, column: 24, scope: !221)
!236 = !DILocation(line: 202, column: 21, scope: !221)
!237 = !DILocation(line: 202, column: 39, scope: !221)
!238 = !DILocation(line: 204, column: 8, scope: !221)
!239 = !DILocation(line: 205, column: 14, scope: !221)
!240 = !DILocation(line: 205, column: 7, scope: !221)
!241 = !DILocation(line: 208, column: 19, scope: !221)
!242 = !DILocation(line: 209, column: 20, scope: !221)
!243 = !DILocation(line: 210, column: 30, scope: !221)
!244 = !DILocation(line: 210, column: 5, scope: !221)
!245 = !DILocation(line: 210, column: 14, scope: !221)
!246 = !DILocation(line: 204, column: 30, scope: !221)
!247 = !DILocation(line: 204, column: 5, scope: !221)
!248 = distinct !DISubprogram(name: "core.buffer.reverse_1922", linkageName: "core.buffer.reverse_1922", scope: !222, file: !222, line: 478, type: !249, scopeLine: 478, spFlags: DISPFlagDefinition, unit: !0)
!249 = !DISubroutineType(types: !250)
!250 = !{!251, !128}
!251 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !164, file: !27, line: 1, align: 1, elements: !18, identifier: "empty")
!252 = !DILocation(line: 478, column: 6, scope: !248)
!253 = !DILocation(line: 479, column: 13, scope: !248)
!254 = !DILocation(line: 480, column: 5, scope: !248)
!255 = !DILocation(line: 480, column: 12, scope: !248)
!256 = !DILocation(line: 480, column: 17, scope: !248)
!257 = !DILocation(line: 480, column: 16, scope: !248)
!258 = !DILocation(line: 480, column: 11, scope: !248)
!259 = !DILocation(line: 481, column: 15, scope: !248)
!260 = !DILocation(line: 481, column: 26, scope: !248)
!261 = !DILocation(line: 482, column: 17, scope: !248)
!262 = !DILocation(line: 482, column: 20, scope: !248)
!263 = !DILocation(line: 482, column: 7, scope: !248)
!264 = !DILocation(line: 483, column: 11, scope: !248)
!265 = !DILocation(line: 483, column: 7, scope: !248)
!266 = distinct !DISubprogram(name: "core.impl_23as-span.as-span_for_t3238_1893", linkageName: "core.impl_23as-span.as-span_for_t3238_1893", scope: !1, file: !1, line: 621, type: !267, scopeLine: 621, spFlags: DISPFlagDefinition, unit: !0)
!267 = !DISubroutineType(types: !268)
!268 = !{!125, !52}
!269 = !DILocation(line: 621, column: 48, scope: !266)
!270 = !DILocation(line: 621, column: 31, scope: !266)
!271 = distinct !DISubprogram(name: "core.span.as-byte-span_1175", linkageName: "core.span.as-byte-span_1175", scope: !272, file: !272, line: 122, type: !273, scopeLine: 122, spFlags: DISPFlagDefinition, unit: !0)
!272 = !DIFile(filename: "span.k1", directory: "/Users/knix/dev/k1/modules/core")
!273 = !DISubroutineType(types: !274)
!274 = !{!46, !125}
!275 = !DILocation(line: 122, column: 6, scope: !271)
!276 = !DILocation(line: 123, column: 17, scope: !271)
!277 = !DILocation(line: 123, column: 5, scope: !271)
!278 = distinct !DISubprogram(name: "core.io.impl_4writer.write-bytes_for_t162_586", linkageName: "core.io.impl_4writer.write-bytes_for_t162_586", scope: !27, file: !27, line: 116, type: !279, scopeLine: 116, spFlags: DISPFlagDefinition, unit: !0)
!279 = !DISubroutineType(types: !280)
!280 = !{!251, !167, !46}
!281 = !DILocation(line: 116, column: 8, scope: !278)
!282 = !DILocation(line: 117, column: 10, scope: !278)
!283 = !DILocation(line: 117, column: 7, scope: !278)
!284 = !DILocation(line: 118, column: 9, scope: !278)
!285 = !DILocation(line: 120, column: 19, scope: !278)
!286 = !DILocation(line: 120, column: 33, scope: !278)
!287 = !DILocation(line: 120, column: 51, scope: !278)
!288 = !DILocation(line: 120, column: 9, scope: !278)
!289 = !DILocation(line: 122, column: 7, scope: !278)
!290 = distinct !DISubprogram(name: "core.u8.to-ascii-digit_644", linkageName: "core.u8.to-ascii-digit_644", scope: !27, file: !27, line: 438, type: !291, scopeLine: 438, spFlags: DISPFlagDefinition, unit: !0)
!291 = !DISubroutineType(types: !292)
!292 = !{!293, !101}
!293 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_unsigned_char)
!294 = !DILocation(line: 439, column: 8, scope: !290)
!295 = !DILocation(line: 439, column: 5, scope: !290)
!296 = !DILocation(line: 440, column: 7, scope: !290)
!297 = !DILocation(line: 441, column: 15, scope: !290)
!298 = !DILocation(line: 441, column: 12, scope: !290)
!299 = !DILocation(line: 442, column: 7, scope: !290)
!300 = !DILocation(line: 443, column: 15, scope: !290)
!301 = !DILocation(line: 443, column: 12, scope: !290)
!302 = !DILocation(line: 444, column: 7, scope: !290)
!303 = !DILocation(line: 446, column: 7, scope: !290)
!304 = !DILocation(line: 446, column: 13, scope: !290)
!305 = !DILocation(line: 446, column: 40, scope: !290)
!306 = distinct !DISubprogram(name: "core.fixlist.push_1882", linkageName: "core.fixlist.push_1882", scope: !307, file: !307, line: 24, type: !308, scopeLine: 24, spFlags: DISPFlagDefinition, unit: !0)
!307 = !DIFile(filename: "fix-list.k1", directory: "/Users/knix/dev/k1/modules/core")
!308 = !DISubroutineType(types: !309)
!309 = !{!251, !52, !293}
!310 = !DILocation(line: 25, column: 22, scope: !306)
!311 = !DILocation(line: 26, column: 8, scope: !306)
!312 = !DILocation(line: 26, column: 5, scope: !306)
!313 = !DILocation(line: 27, column: 21, scope: !306)
!314 = !DILocation(line: 27, column: 7, scope: !306)
!315 = !DILocation(line: 28, column: 7, scope: !306)
!316 = !DILocation(line: 28, column: 19, scope: !306)
!317 = !DILocation(line: 30, column: 22, scope: !306)
!318 = !DILocation(line: 31, column: 7, scope: !306)
!319 = !DILocation(line: 31, column: 13, scope: !306)
!320 = !DILocation(line: 31, column: 15, scope: !306)
!321 = !DILocation(line: 31, column: 47, scope: !306)
!322 = !DILocation(line: 31, column: 61, scope: !306)
!323 = distinct !DISubprogram(name: "core.io.impl_4writer.write-byte_for_t162_585", linkageName: "core.io.impl_4writer.write-byte_for_t162_585", scope: !27, file: !27, line: 105, type: !324, scopeLine: 105, spFlags: DISPFlagDefinition, unit: !0)
!324 = !DISubroutineType(types: !325)
!325 = !{!251, !167, !101}
!326 = !DILocation(line: 105, column: 8, scope: !323)
!327 = !DILocation(line: 106, column: 10, scope: !323)
!328 = !DILocation(line: 106, column: 7, scope: !323)
!329 = !DILocation(line: 111, column: 26, scope: !323)
!330 = !DILocation(line: 112, column: 19, scope: !323)
!331 = !DILocation(line: 112, column: 9, scope: !323)
!332 = !DILocation(line: 114, column: 7, scope: !323)
!333 = !DILocation(line: 107, column: 12, scope: !323)
!334 = !DILocation(line: 107, column: 9, scope: !323)
!335 = !DILocation(line: 107, column: 49, scope: !323)
!336 = !DILocation(line: 108, column: 9, scope: !323)
!337 = !DILocation(line: 109, column: 12, scope: !323)
!338 = !DILocation(line: 109, column: 9, scope: !323)
!339 = !DILocation(line: 109, column: 63, scope: !323)
!340 = !DILocation(line: 109, column: 29, scope: !323)
!341 = !DILocation(line: 109, column: 56, scope: !323)
!342 = distinct !DISubprogram(name: "core.fixlist.push_2014", linkageName: "core.fixlist.push_2014", scope: !307, file: !307, line: 24, type: !343, scopeLine: 24, spFlags: DISPFlagDefinition, unit: !0)
!343 = !DISubroutineType(types: !344)
!344 = !{!345, !52, !101}
!345 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !323, file: !27, line: 1, align: 1, elements: !18, identifier: "empty")
!346 = !DILocation(line: 25, column: 22, scope: !342)
!347 = !DILocation(line: 26, column: 8, scope: !342)
!348 = !DILocation(line: 26, column: 5, scope: !342)
!349 = !DILocation(line: 27, column: 21, scope: !342)
!350 = !DILocation(line: 27, column: 7, scope: !342)
!351 = !DILocation(line: 28, column: 7, scope: !342)
!352 = !DILocation(line: 28, column: 19, scope: !342)
!353 = !DILocation(line: 30, column: 22, scope: !342)
!354 = !DILocation(line: 31, column: 7, scope: !342)
!355 = !DILocation(line: 31, column: 13, scope: !342)
!356 = !DILocation(line: 31, column: 15, scope: !342)
!357 = !DILocation(line: 31, column: 47, scope: !342)
!358 = !DILocation(line: 31, column: 61, scope: !342)
!359 = distinct !DISubprogram(name: "core.io.stdout-buffering-mode_581", linkageName: "core.io.stdout-buffering-mode_581", scope: !27, file: !27, line: 59, type: !360, scopeLine: 59, spFlags: DISPFlagDefinition, unit: !0)
!360 = !DISubroutineType(types: !361)
!361 = !{!101}
!362 = !DILocation(line: 60, column: 8, scope: !359)
!363 = !DILocation(line: 60, column: 23, scope: !359)
!364 = !DILocation(line: 60, column: 5, scope: !359)
!365 = !DILocation(line: 63, column: 5, scope: !359)
!366 = !DILocation(line: 61, column: 24, scope: !359)
!367 = !DILocation(line: 61, column: 21, scope: !359)
!368 = !DILocation(line: 61, column: 7, scope: !359)
!369 = distinct !DISubprogram(name: "core.platform.io.is-tty_965", linkageName: "core.platform.io.is-tty_965", scope: !38, file: !38, line: 128, type: !370, scopeLine: 128, spFlags: DISPFlagDefinition, unit: !0)
!370 = !DISubroutineType(types: !371)
!371 = !{!372, !373}
!372 = !DIBasicType(name: "bool", size: 8, encoding: DW_ATE_boolean)
!373 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.platform.io.file-handle", scope: !57, file: !27, line: 66, size: 4, align: 4, elements: !374, identifier: "core.platform.io.file-handle")
!374 = !{!375}
!375 = !DIDerivedType(tag: DW_TAG_member, name: "raw", scope: !57, file: !27, line: 66, baseType: !11, size: 32, align: 32)
!376 = !DILocation(line: 128, column: 8, scope: !369)
!377 = !DILocation(line: 129, column: 13, scope: !369)
!378 = !DILocation(line: 129, column: 7, scope: !369)
!379 = !DILocation(line: 129, column: 41, scope: !369)
!380 = !DILocation(line: 129, column: 23, scope: !369)
!381 = !DILocation(line: 131, column: 59, scope: !369)
!382 = !DILocation(line: 131, column: 41, scope: !369)
!383 = !DISubprogram(name: "isatty", linkageName: "isatty", scope: !38, file: !38, line: 471, type: !384, scopeLine: 471, spFlags: 0)
!384 = !DISubroutineType(types: !385)
!385 = !{!11, !11}
!386 = distinct !DISubprogram(name: "core.types.type-id.name_2", linkageName: "core.types.type-id.name_2", scope: !1, file: !1, line: 66, type: !387, scopeLine: 66, spFlags: DISPFlagDefinition, unit: !0)
!387 = !DISubroutineType(types: !388)
!388 = !{!122, !389}
!389 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.types.type-id", scope: !342, file: !307, line: 24, size: 8, align: 8, elements: !390, identifier: "core.types.type-id")
!390 = !{!391}
!391 = !DIDerivedType(tag: DW_TAG_member, name: "inner", scope: !342, file: !307, line: 24, baseType: !133, size: 64, align: 64)
!392 = !DILocation(line: 66, column: 16, scope: !386)
!393 = distinct !DISubprogram(name: "core.string-builder.new_1255", linkageName: "core.string-builder.new_1255", scope: !394, file: !394, line: 5, type: !395, scopeLine: 5, spFlags: DISPFlagDefinition, unit: !0)
!394 = !DIFile(filename: "string-builder.k1", directory: "/Users/knix/dev/k1/modules/core")
!395 = !DISubroutineType(types: !396)
!396 = !{!397}
!397 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.string-builder", scope: !342, file: !307, line: 24, size: 24, align: 8, elements: !398, identifier: "core.string-builder")
!398 = !{!399}
!399 = !DIDerivedType(tag: DW_TAG_member, name: "list", scope: !342, file: !307, line: 24, baseType: !400, size: 192, align: 64)
!400 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.list_of_u8_", scope: !342, file: !307, line: 24, size: 24, align: 8, elements: !401, identifier: "core.list_of_u8_")
!401 = !{!402, !403}
!402 = !DIDerivedType(tag: DW_TAG_member, name: "buffer", scope: !342, file: !307, line: 24, baseType: !49, size: 128, align: 64)
!403 = !DIDerivedType(tag: DW_TAG_member, name: "len", scope: !342, file: !307, line: 24, baseType: !54, size: 64, align: 64, offset: 16)
!404 = !DILocation(line: 6, column: 15, scope: !393)
!405 = !DILocation(line: 6, column: 5, scope: !393)
!406 = distinct !DISubprogram(name: "core.impl_4writer.write-string_for_t2342_1263", linkageName: "core.impl_4writer.write-string_for_t2342_1263", scope: !1, file: !1, line: 243, type: !407, scopeLine: 243, spFlags: DISPFlagDefinition, unit: !0)
!407 = !DISubroutineType(types: !408)
!408 = !{!409, !52, !122}
!409 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !342, file: !307, line: 1, align: 1, elements: !18, identifier: "empty")
!410 = !DILocation(line: 243, column: 6, scope: !406)
!411 = !DILocation(line: 243, column: 59, scope: !406)
!412 = !DILocation(line: 243, column: 42, scope: !406)
!413 = distinct !DISubprogram(name: "core.impl_5print.print-to_for_t9_1490", linkageName: "core.impl_5print.print-to_for_t9_1490", scope: !27, file: !27, line: 376, type: !414, scopeLine: 376, spFlags: DISPFlagDefinition, unit: !0)
!414 = !DISubroutineType(types: !415)
!415 = !{!409, !54, !52}
!416 = !DILocation(line: 377, column: 15, scope: !413)
!417 = !DILocation(line: 378, column: 8, scope: !413)
!418 = !DILocation(line: 378, column: 5, scope: !413)
!419 = !DILocation(line: 379, column: 7, scope: !413)
!420 = !DILocation(line: 382, column: 18, scope: !413)
!421 = !DILocation(line: 382, column: 15, scope: !413)
!422 = !DILocation(line: 382, column: 22, scope: !413)
!423 = !DILocation(line: 383, column: 28, scope: !413)
!424 = !DILocation(line: 383, column: 5, scope: !413)
!425 = distinct !DISubprogram(name: "core.string-builder.build-tmp_1259", linkageName: "core.string-builder.build-tmp_1259", scope: !394, file: !394, line: 21, type: !426, scopeLine: 21, spFlags: DISPFlagDefinition, unit: !0)
!426 = !DISubroutineType(types: !427)
!427 = !{!122, !52}
!428 = !DILocation(line: 22, column: 23, scope: !425)
!429 = !DILocation(line: 22, column: 5, scope: !425)
!430 = !DILocation(line: 22, column: 54, scope: !425)
!431 = distinct !DISubprogram(name: "core.crash-bounds_336", linkageName: "core.crash-bounds_336", scope: !1, file: !1, line: 1101, type: !432, scopeLine: 1101, spFlags: DISPFlagDefinition, unit: !0)
!432 = !DISubroutineType(types: !433)
!433 = !{!409, !119, !54, !54, !122}
!434 = !DILocation(line: 1101, column: 4, scope: !431)
!435 = !DILocation(line: 1102, column: 9, scope: !431)
!436 = !DILocation(line: 1102, column: 11, scope: !431)
!437 = !DILocation(line: 1102, column: 17, scope: !431)
!438 = !DILocation(line: 1102, column: 27, scope: !431)
!439 = !DILocation(line: 1102, column: 3, scope: !431)
!440 = distinct !DISubprogram(name: "core.mem.current-arena_924", linkageName: "core.mem.current-arena_924", scope: !441, file: !441, line: 78, type: !442, scopeLine: 78, spFlags: DISPFlagDefinition, unit: !0)
!441 = !DIFile(filename: "mem.k1", directory: "/Users/knix/dev/k1/modules/core")
!442 = !DISubroutineType(types: !443)
!443 = !{!52}
!444 = !DILocation(line: 79, column: 5, scope: !440)
!445 = !DILocation(line: 80, column: 7, scope: !440)
!446 = !DILocation(line: 81, column: 7, scope: !440)
!447 = !DILocation(line: 81, column: 16, scope: !440)
!448 = !DILocation(line: 80, column: 18, scope: !440)
!449 = distinct !DISubprogram(name: "core.buffer.allocate-in_1786", linkageName: "core.buffer.allocate-in_1786", scope: !222, file: !222, line: 82, type: !450, scopeLine: 82, spFlags: DISPFlagDefinition, unit: !0)
!450 = !DISubroutineType(types: !451)
!451 = !{!49, !52, !54}
!452 = !DILocation(line: 83, column: 18, scope: !449)
!453 = !DILocation(line: 83, column: 15, scope: !449)
!454 = !DILocation(line: 84, column: 11, scope: !449)
!455 = !DILocation(line: 86, column: 24, scope: !449)
!456 = !DILocation(line: 87, column: 19, scope: !449)
!457 = !DILocation(line: 87, column: 7, scope: !449)
!458 = !DILocation(line: 89, column: 16, scope: !449)
!459 = !DILocation(line: 90, column: 21, scope: !449)
!460 = !DILocation(line: 90, column: 5, scope: !449)
!461 = !DILocation(line: 90, column: 14, scope: !449)
!462 = distinct !DISubprogram(name: "core.buffer.slice_1776", linkageName: "core.buffer.slice_1776", scope: !222, file: !222, line: 197, type: !463, scopeLine: 197, spFlags: DISPFlagDefinition, unit: !0)
!463 = !DISubroutineType(types: !464)
!464 = !{!49, !49, !54, !54}
!465 = !DILocation(line: 197, column: 6, scope: !462)
!466 = !DILocation(line: 198, column: 8, scope: !462)
!467 = !DILocation(line: 199, column: 7, scope: !462)
!468 = !DILocation(line: 199, column: 13, scope: !462)
!469 = !DILocation(line: 199, column: 48, scope: !462)
!470 = !DILocation(line: 199, column: 68, scope: !462)
!471 = !DILocation(line: 198, column: 29, scope: !462)
!472 = !DILocation(line: 198, column: 21, scope: !462)
!473 = !DILocation(line: 198, column: 5, scope: !462)
!474 = !DILocation(line: 202, column: 30, scope: !462)
!475 = !DILocation(line: 202, column: 24, scope: !462)
!476 = !DILocation(line: 202, column: 21, scope: !462)
!477 = !DILocation(line: 202, column: 39, scope: !462)
!478 = !DILocation(line: 204, column: 8, scope: !462)
!479 = !DILocation(line: 205, column: 14, scope: !462)
!480 = !DILocation(line: 205, column: 7, scope: !462)
!481 = !DILocation(line: 208, column: 19, scope: !462)
!482 = !DILocation(line: 209, column: 20, scope: !462)
!483 = !DILocation(line: 210, column: 30, scope: !462)
!484 = !DILocation(line: 210, column: 5, scope: !462)
!485 = !DILocation(line: 210, column: 14, scope: !462)
!486 = !DILocation(line: 204, column: 30, scope: !462)
!487 = !DILocation(line: 204, column: 5, scope: !462)
!488 = distinct !DISubprogram(name: "core.mem.tmp_926", linkageName: "core.mem.tmp_926", scope: !441, file: !441, line: 90, type: !442, scopeLine: 90, spFlags: DISPFlagDefinition, unit: !0)
!489 = !DILocation(line: 91, column: 8, scope: !488)
!490 = !DILocation(line: 91, column: 21, scope: !488)
!491 = !DILocation(line: 91, column: 5, scope: !488)
!492 = !DILocation(line: 91, column: 28, scope: !488)
!493 = !DILocation(line: 92, column: 5, scope: !488)
!494 = distinct !DISubprogram(name: "core.buffer.allocate-in_1861", linkageName: "core.buffer.allocate-in_1861", scope: !222, file: !222, line: 82, type: !495, scopeLine: 82, spFlags: DISPFlagDefinition, unit: !0)
!495 = !DISubroutineType(types: !496)
!496 = !{!128, !52, !54}
!497 = !DILocation(line: 83, column: 18, scope: !494)
!498 = !DILocation(line: 83, column: 15, scope: !494)
!499 = !DILocation(line: 84, column: 11, scope: !494)
!500 = !DILocation(line: 86, column: 24, scope: !494)
!501 = !DILocation(line: 87, column: 19, scope: !494)
!502 = !DILocation(line: 87, column: 7, scope: !494)
!503 = !DILocation(line: 89, column: 16, scope: !494)
!504 = !DILocation(line: 90, column: 21, scope: !494)
!505 = !DILocation(line: 90, column: 5, scope: !494)
!506 = !DILocation(line: 90, column: 14, scope: !494)
!507 = distinct !DISubprogram(name: "core.buffer.copy_1862", linkageName: "core.buffer.copy_1862", scope: !222, file: !222, line: 122, type: !508, scopeLine: 122, spFlags: DISPFlagDefinition, unit: !0)
!508 = !DISubroutineType(types: !509)
!509 = !{!510, !128, !128}
!510 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !431, file: !1, line: 1, align: 1, elements: !18, identifier: "empty")
!511 = !DILocation(line: 122, column: 6, scope: !507)
!512 = !DILocation(line: 123, column: 8, scope: !507)
!513 = !DILocation(line: 123, column: 18, scope: !507)
!514 = !DILocation(line: 123, column: 5, scope: !507)
!515 = !DILocation(line: 124, column: 7, scope: !507)
!516 = !DILocation(line: 124, column: 13, scope: !507)
!517 = !DILocation(line: 124, column: 53, scope: !507)
!518 = !DILocation(line: 124, column: 66, scope: !507)
!519 = !DILocation(line: 126, column: 8, scope: !507)
!520 = !DILocation(line: 126, column: 5, scope: !507)
!521 = !DILocation(line: 126, column: 21, scope: !507)
!522 = !DILocation(line: 127, column: 42, scope: !507)
!523 = !DILocation(line: 127, column: 21, scope: !507)
!524 = !DILocation(line: 128, column: 20, scope: !507)
!525 = !DILocation(line: 128, column: 42, scope: !507)
!526 = !DILocation(line: 128, column: 66, scope: !507)
!527 = !DILocation(line: 128, column: 5, scope: !507)
!528 = distinct !DISubprogram(name: "core.mem.copy_234", linkageName: "core.mem.copy_234", scope: !1, file: !1, line: 713, type: !529, scopeLine: 713, spFlags: DISPFlagDefinition, unit: !0)
!529 = !DISubroutineType(types: !530)
!530 = !{!531, !52, !52, !54}
!531 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !507, file: !222, line: 1, align: 1, elements: !18, identifier: "empty")
!532 = !DILocation(line: 713, column: 14, scope: !528)
!533 = distinct !DISubprogram(name: "core.arena.alloc-layout_350", linkageName: "core.arena.alloc-layout_350", scope: !534, file: !534, line: 109, type: !535, scopeLine: 109, spFlags: DISPFlagDefinition, unit: !0)
!534 = !DIFile(filename: "arena.k1", directory: "/Users/knix/dev/k1/modules/core")
!535 = !DISubroutineType(types: !536)
!536 = !{!52, !52, !54, !54}
!537 = !DILocation(line: 110, column: 18, scope: !533)
!538 = !DILocation(line: 115, column: 10, scope: !533)
!539 = !DILocation(line: 115, column: 7, scope: !533)
!540 = !DILocation(line: 115, column: 49, scope: !533)
!541 = !DILocation(line: 115, column: 28, scope: !533)
!542 = !DILocation(line: 117, column: 27, scope: !533)
!543 = !DILocation(line: 117, column: 20, scope: !533)
!544 = !DILocation(line: 119, column: 21, scope: !533)
!545 = !DILocation(line: 119, column: 40, scope: !533)
!546 = !DILocation(line: 120, column: 8, scope: !533)
!547 = !DILocation(line: 120, column: 21, scope: !533)
!548 = !DILocation(line: 120, column: 5, scope: !533)
!549 = !DILocation(line: 122, column: 54, scope: !533)
!550 = !DILocation(line: 122, column: 33, scope: !533)
!551 = !DILocation(line: 123, column: 24, scope: !533)
!552 = !DILocation(line: 123, column: 35, scope: !533)
!553 = !DILocation(line: 124, column: 23, scope: !533)
!554 = !DILocation(line: 124, column: 40, scope: !533)
!555 = !DILocation(line: 125, column: 7, scope: !533)
!556 = !DILocation(line: 125, column: 23, scope: !533)
!557 = !DILocation(line: 125, column: 40, scope: !533)
!558 = !DILocation(line: 126, column: 7, scope: !533)
!559 = !DILocation(line: 128, column: 7, scope: !533)
!560 = !DILocation(line: 130, column: 7, scope: !533)
!561 = distinct !DISubprogram(name: "core.arena._grow_351", linkageName: "core.arena._grow_351", scope: !534, file: !534, line: 134, type: !562, scopeLine: 134, spFlags: DISPFlagDefinition, unit: !0)
!562 = !DISubroutineType(types: !563)
!563 = !{!564, !52, !54, !54}
!564 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !533, file: !534, line: 1, align: 1, elements: !18, identifier: "empty")
!565 = !DILocation(line: 137, column: 8, scope: !561)
!566 = !DILocation(line: 137, column: 5, scope: !561)
!567 = !DILocation(line: 137, column: 19, scope: !561)
!568 = !DILocation(line: 138, column: 20, scope: !561)
!569 = !DILocation(line: 139, column: 25, scope: !561)
!570 = !DILocation(line: 139, column: 18, scope: !561)
!571 = !DILocation(line: 140, column: 21, scope: !561)
!572 = !DILocation(line: 140, column: 41, scope: !561)
!573 = !DILocation(line: 141, column: 24, scope: !561)
!574 = !DILocation(line: 141, column: 33, scope: !561)
!575 = !DILocation(line: 141, column: 21, scope: !561)
!576 = !DILocation(line: 141, column: 43, scope: !561)
!577 = !DILocation(line: 141, column: 55, scope: !561)
!578 = !DILocation(line: 142, column: 42, scope: !561)
!579 = !DILocation(line: 142, column: 18, scope: !561)
!580 = !DILocation(line: 143, column: 36, scope: !561)
!581 = !DILocation(line: 144, column: 5, scope: !561)
!582 = !DILocation(line: 144, column: 19, scope: !561)
!583 = !DILocation(line: 144, column: 16, scope: !561)
!584 = !DILocation(line: 144, column: 34, scope: !561)
!585 = !DILocation(line: 145, column: 5, scope: !561)
!586 = !DILocation(line: 145, column: 25, scope: !561)
!587 = !DILocation(line: 146, column: 22, scope: !561)
!588 = !DILocation(line: 147, column: 21, scope: !561)
!589 = !DILocation(line: 147, column: 5, scope: !561)
!590 = !DILocation(line: 148, column: 5, scope: !561)
!591 = !DILocation(line: 148, column: 21, scope: !561)
!592 = !DILocation(line: 149, column: 5, scope: !561)
!593 = !DILocation(line: 149, column: 21, scope: !561)
!594 = !DILocation(line: 149, column: 43, scope: !561)
!595 = distinct !DISubprogram(name: "core.platform.memory.acquire_953", linkageName: "core.platform.memory.acquire_953", scope: !38, file: !38, line: 45, type: !596, scopeLine: 45, spFlags: DISPFlagDefinition, unit: !0)
!596 = !DISubroutineType(types: !597)
!597 = !{!598, !54}
!598 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.platform.memory.region", scope: !561, file: !534, line: 134, size: 16, align: 8, elements: !599, identifier: "core.platform.memory.region")
!599 = !{!600, !601}
!600 = !DIDerivedType(tag: DW_TAG_member, name: "base", scope: !561, file: !534, line: 134, baseType: !52, size: 64, align: 64)
!601 = !DIDerivedType(tag: DW_TAG_member, name: "len", scope: !561, file: !534, line: 134, baseType: !54, size: 64, align: 64, offset: 8)
!602 = !DILocation(line: 46, column: 15, scope: !595)
!603 = !DILocation(line: 47, column: 14, scope: !595)
!604 = !DILocation(line: 47, column: 22, scope: !595)
!605 = !DILocation(line: 47, column: 7, scope: !595)
!606 = !DILocation(line: 48, column: 7, scope: !595)
!607 = distinct !DISubprogram(name: "core.platform.port.posix.reserve_1037", linkageName: "core.platform.port.posix.reserve_1037", scope: !38, file: !38, line: 527, type: !596, scopeLine: 527, spFlags: DISPFlagDefinition, unit: !0)
!608 = !DILocation(line: 530, column: 17, scope: !607)
!609 = !DILocation(line: 530, column: 11, scope: !607)
!610 = !DILocation(line: 530, column: 126, scope: !607)
!611 = !DILocation(line: 531, column: 30, scope: !607)
!612 = !DILocation(line: 531, column: 44, scope: !607)
!613 = !DILocation(line: 531, column: 28, scope: !607)
!614 = !DILocation(line: 533, column: 22, scope: !607)
!615 = !DILocation(line: 535, column: 20, scope: !607)
!616 = !DILocation(line: 536, column: 19, scope: !607)
!617 = !DILocation(line: 532, column: 20, scope: !607)
!618 = !DILocation(line: 540, column: 12, scope: !607)
!619 = !DILocation(line: 540, column: 31, scope: !607)
!620 = !DILocation(line: 540, column: 9, scope: !607)
!621 = !DILocation(line: 541, column: 11, scope: !607)
!622 = !DILocation(line: 543, column: 19, scope: !607)
!623 = !DILocation(line: 543, column: 9, scope: !607)
!624 = !DILocation(line: 543, column: 31, scope: !607)
!625 = distinct !DISubprogram(name: "core.platform.port.posix.commit_1038", linkageName: "core.platform.port.posix.commit_1038", scope: !38, file: !38, line: 546, type: !626, scopeLine: 546, spFlags: DISPFlagDefinition, unit: !0)
!626 = !DISubroutineType(types: !627)
!627 = !{!628, !52, !54}
!628 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !595, file: !38, line: 1, align: 1, elements: !18, identifier: "empty")
!629 = !DILocation(line: 547, column: 20, scope: !625)
!630 = !DILocation(line: 548, column: 21, scope: !625)
!631 = !DILocation(line: 548, column: 38, scope: !625)
!632 = !DILocation(line: 548, column: 45, scope: !625)
!633 = !DILocation(line: 549, column: 20, scope: !625)
!634 = !DILocation(line: 549, column: 43, scope: !625)
!635 = !DILocation(line: 549, column: 19, scope: !625)
!636 = !DILocation(line: 549, column: 55, scope: !625)
!637 = !DILocation(line: 549, column: 62, scope: !625)
!638 = !DILocation(line: 550, column: 23, scope: !625)
!639 = !DILocation(line: 550, column: 39, scope: !625)
!640 = !DILocation(line: 550, column: 45, scope: !625)
!641 = !DILocation(line: 550, column: 54, scope: !625)
!642 = !DILocation(line: 550, column: 68, scope: !625)
!643 = !DILocation(line: 550, column: 52, scope: !625)
!644 = !DILocation(line: 550, column: 12, scope: !625)
!645 = !DILocation(line: 550, column: 9, scope: !625)
!646 = !DILocation(line: 551, column: 11, scope: !625)
!647 = !DISubprogram(name: "getpagesize", linkageName: "getpagesize", scope: !38, file: !38, line: 455, type: !9, scopeLine: 455, spFlags: 0)
!648 = !DISubprogram(name: "mprotect", linkageName: "mprotect", scope: !38, file: !38, line: 454, type: !649, scopeLine: 454, spFlags: 0)
!649 = !DISubroutineType(types: !650)
!650 = !{!11, !52, !54, !11}
!651 = !DISubprogram(name: "mmap", linkageName: "mmap", scope: !38, file: !38, line: 452, type: !652, scopeLine: 452, spFlags: 0)
!652 = !DISubroutineType(types: !653)
!653 = !{!52, !52, !54, !11, !11, !11, !54}
!654 = distinct !DISubprogram(name: "core.mem.init-tmp-arena_925", linkageName: "core.mem.init-tmp-arena_925", scope: !441, file: !441, line: 85, type: !655, scopeLine: 85, spFlags: DISPFlagDefinition, unit: !0)
!655 = !DISubroutineType(types: !656)
!656 = !{!657}
!657 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !488, file: !441, line: 1, align: 1, elements: !18, identifier: "empty")
!658 = !DILocation(line: 86, column: 13, scope: !654)
!659 = !DILocation(line: 86, column: 26, scope: !654)
!660 = !DILocation(line: 86, column: 12, scope: !654)
!661 = !DILocation(line: 86, column: 32, scope: !654)
!662 = !DILocation(line: 87, column: 17, scope: !654)
!663 = !DILocation(line: 87, column: 5, scope: !654)
!664 = !DILocation(line: 86, column: 8, scope: !654)
!665 = !DILocation(line: 86, column: 5, scope: !654)
!666 = distinct !DISubprogram(name: "core.arena.store-header_342", linkageName: "core.arena.store-header_342", scope: !534, file: !534, line: 39, type: !667, scopeLine: 39, spFlags: DISPFlagDefinition, unit: !0)
!667 = !DISubroutineType(types: !668)
!668 = !{!52, !598}
!669 = !DILocation(line: 39, column: 6, scope: !666)
!670 = !DILocation(line: 40, column: 5, scope: !666)
!671 = !DILocation(line: 40, column: 12, scope: !666)
!672 = !DILocation(line: 41, column: 24, scope: !666)
!673 = !DILocation(line: 42, column: 22, scope: !666)
!674 = !DILocation(line: 42, column: 44, scope: !666)
!675 = !DILocation(line: 43, column: 25, scope: !666)
!676 = !DILocation(line: 44, column: 5, scope: !666)
!677 = !DILocation(line: 45, column: 18, scope: !666)
!678 = !DILocation(line: 44, column: 15, scope: !666)
!679 = !DILocation(line: 46, column: 18, scope: !666)
!680 = !DILocation(line: 47, column: 18, scope: !666)
!681 = !DILocation(line: 48, column: 20, scope: !666)
!682 = !DILocation(line: 49, column: 19, scope: !666)
!683 = !DILocation(line: 50, column: 26, scope: !666)
!684 = !DILocation(line: 51, column: 15, scope: !666)
!685 = !DILocation(line: 52, column: 14, scope: !666)
!686 = !DILocation(line: 54, column: 5, scope: !666)
!687 = distinct !DISubprogram(name: "core.buffer.check-bounds_1868", linkageName: "core.buffer.check-bounds_1868", scope: !222, file: !222, line: 168, type: !688, scopeLine: 168, spFlags: DISPFlagDefinition, unit: !0)
!688 = !DISubroutineType(types: !689)
!689 = !{!690, !49, !54}
!690 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !462, file: !222, line: 1, align: 1, elements: !18, identifier: "empty")
!691 = !DILocation(line: 168, column: 6, scope: !687)
!692 = !DILocation(line: 169, column: 28, scope: !687)
!693 = !DILocation(line: 169, column: 8, scope: !687)
!694 = !DILocation(line: 169, column: 5, scope: !687)
!695 = !DILocation(line: 170, column: 7, scope: !687)
!696 = !DILocation(line: 170, column: 13, scope: !687)
!697 = !DILocation(line: 170, column: 36, scope: !687)
!698 = !DILocation(line: 170, column: 46, scope: !687)
!699 = distinct !DISubprogram(name: "core.impl_23as-span.last_for_t3148_1809", linkageName: "core.impl_23as-span.last_for_t3148_1809", scope: !1, file: !1, line: 575, type: !700, scopeLine: 575, spFlags: DISPFlagDefinition, unit: !0)
!700 = !DISubroutineType(types: !701)
!701 = !{!702, !52}
!702 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.opt_of_ptr_", scope: !440, file: !441, line: 78, size: 16, align: 8, elements: !703, identifier: "core.opt_of_ptr_")
!703 = !{!704, !705}
!704 = !DIDerivedType(tag: DW_TAG_member, name: "tag", scope: !440, file: !441, line: 78, baseType: !101, size: 8, align: 8)
!705 = !DIDerivedType(tag: DW_TAG_member, name: "payload", scope: !440, file: !441, line: 78, baseType: !706, size: 64, align: 64, offset: 8)
!706 = !DICompositeType(tag: DW_TAG_union_type, name: "core.opt_of_ptr_", scope: !440, file: !441, line: 78, size: 64, align: 64, elements: !443, identifier: "core.opt_of_ptr_")
!707 = !DILocation(line: 576, column: 13, scope: !699)
!708 = !DILocation(line: 577, column: 8, scope: !699)
!709 = !DILocation(line: 577, column: 5, scope: !699)
!710 = !DILocation(line: 577, column: 19, scope: !699)
!711 = !DILocation(line: 578, column: 10, scope: !699)
!712 = !DILocation(line: 578, column: 22, scope: !699)
!713 = !DILocation(line: 578, column: 16, scope: !699)
!714 = distinct !DISubprogram(name: "core.impl_23as-span.as-span_for_t3148_1802", linkageName: "core.impl_23as-span.as-span_for_t3148_1802", scope: !1, file: !1, line: 621, type: !715, scopeLine: 621, spFlags: DISPFlagDefinition, unit: !0)
!715 = !DISubroutineType(types: !716)
!716 = !{!717, !52}
!717 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.span_of_ptr_", scope: !699, file: !1, line: 575, size: 16, align: 8, elements: !718, identifier: "core.span_of_ptr_")
!718 = !{!719}
!719 = !DIDerivedType(tag: DW_TAG_member, name: "buffer", scope: !699, file: !1, line: 575, baseType: !720, size: 128, align: 64)
!720 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.buffer_of_ptr_", scope: !699, file: !1, line: 575, size: 16, align: 8, elements: !721, identifier: "core.buffer_of_ptr_")
!721 = !{!722, !723}
!722 = !DIDerivedType(tag: DW_TAG_member, name: "data", scope: !699, file: !1, line: 575, baseType: !52, size: 64, align: 64)
!723 = !DIDerivedType(tag: DW_TAG_member, name: "len", scope: !699, file: !1, line: 575, baseType: !54, size: 64, align: 64, offset: 8)
!724 = !DILocation(line: 621, column: 48, scope: !714)
!725 = !DILocation(line: 621, column: 31, scope: !714)
!726 = distinct !DISubprogram(name: "core.buffer.check-bounds_1827", linkageName: "core.buffer.check-bounds_1827", scope: !222, file: !222, line: 168, type: !727, scopeLine: 168, spFlags: DISPFlagDefinition, unit: !0)
!727 = !DISubroutineType(types: !728)
!728 = !{!729, !720, !54}
!729 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !699, file: !1, line: 1, align: 1, elements: !18, identifier: "empty")
!730 = !DILocation(line: 168, column: 6, scope: !726)
!731 = !DILocation(line: 169, column: 28, scope: !726)
!732 = !DILocation(line: 169, column: 8, scope: !726)
!733 = !DILocation(line: 169, column: 5, scope: !726)
!734 = !DILocation(line: 170, column: 7, scope: !726)
!735 = !DILocation(line: 170, column: 13, scope: !726)
!736 = !DILocation(line: 170, column: 36, scope: !726)
!737 = !DILocation(line: 170, column: 46, scope: !726)
!738 = distinct !DISubprogram(name: "core.impl_22as-buffer.as-buffer_for_t3189_1832", linkageName: "core.impl_22as-buffer.as-buffer_for_t3189_1832", scope: !1, file: !1, line: 1032, type: !739, scopeLine: 1032, spFlags: DISPFlagDefinition, unit: !0)
!739 = !DISubroutineType(types: !740)
!740 = !{!720, !52}
!741 = !DILocation(line: 1033, column: 33, scope: !738)
!742 = !DILocation(line: 1033, column: 5, scope: !738)
!743 = !DILocation(line: 1033, column: 14, scope: !738)
!744 = distinct !DISubprogram(name: "core.buffer.slice_1843", linkageName: "core.buffer.slice_1843", scope: !222, file: !222, line: 197, type: !745, scopeLine: 197, spFlags: DISPFlagDefinition, unit: !0)
!745 = !DISubroutineType(types: !746)
!746 = !{!720, !720, !54, !54}
!747 = !DILocation(line: 197, column: 6, scope: !744)
!748 = !DILocation(line: 198, column: 8, scope: !744)
!749 = !DILocation(line: 199, column: 7, scope: !744)
!750 = !DILocation(line: 199, column: 13, scope: !744)
!751 = !DILocation(line: 199, column: 48, scope: !744)
!752 = !DILocation(line: 199, column: 68, scope: !744)
!753 = !DILocation(line: 198, column: 29, scope: !744)
!754 = !DILocation(line: 198, column: 21, scope: !744)
!755 = !DILocation(line: 198, column: 5, scope: !744)
!756 = !DILocation(line: 202, column: 30, scope: !744)
!757 = !DILocation(line: 202, column: 24, scope: !744)
!758 = !DILocation(line: 202, column: 21, scope: !744)
!759 = !DILocation(line: 202, column: 39, scope: !744)
!760 = !DILocation(line: 204, column: 8, scope: !744)
!761 = !DILocation(line: 205, column: 14, scope: !744)
!762 = !DILocation(line: 205, column: 7, scope: !744)
!763 = !DILocation(line: 208, column: 19, scope: !744)
!764 = !DILocation(line: 209, column: 20, scope: !744)
!765 = !DILocation(line: 210, column: 30, scope: !744)
!766 = !DILocation(line: 210, column: 5, scope: !744)
!767 = !DILocation(line: 210, column: 14, scope: !744)
!768 = !DILocation(line: 204, column: 30, scope: !744)
!769 = !DILocation(line: 204, column: 5, scope: !744)
!770 = distinct !DISubprogram(name: "core.span.as-char-span_1174", linkageName: "core.span.as-char-span_1174", scope: !272, file: !272, line: 118, type: !771, scopeLine: 118, spFlags: DISPFlagDefinition, unit: !0)
!771 = !DISubroutineType(types: !772)
!772 = !{!125, !46}
!773 = !DILocation(line: 118, column: 6, scope: !770)
!774 = !DILocation(line: 119, column: 17, scope: !770)
!775 = !DILocation(line: 119, column: 5, scope: !770)
!776 = distinct !DISubprogram(name: "core.string.cloned-in_1774", linkageName: "core.string.cloned-in_1774", scope: !777, file: !777, line: 18, type: !778, scopeLine: 18, spFlags: DISPFlagDefinition, unit: !0)
!777 = !DIFile(filename: "string.k1", directory: "/Users/knix/dev/k1/modules/core")
!778 = !DISubroutineType(types: !779)
!779 = !{!122, !122, !52}
!780 = !DILocation(line: 18, column: 6, scope: !776)
!781 = !DILocation(line: 19, column: 15, scope: !776)
!782 = !DILocation(line: 19, column: 5, scope: !776)
!783 = distinct !DISubprogram(name: "core.list.push-in_1941", linkageName: "core.list.push-in_1941", scope: !784, file: !784, line: 99, type: !785, scopeLine: 99, spFlags: DISPFlagDefinition, unit: !0)
!784 = !DIFile(filename: "list.k1", directory: "/Users/knix/dev/k1/modules/core")
!785 = !DISubroutineType(types: !786)
!786 = !{!787, !52, !52, !101}
!787 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !413, file: !27, line: 1, align: 1, elements: !18, identifier: "empty")
!788 = !DILocation(line: 100, column: 24, scope: !783)
!789 = !DILocation(line: 101, column: 8, scope: !783)
!790 = !DILocation(line: 101, column: 24, scope: !783)
!791 = !DILocation(line: 101, column: 5, scope: !783)
!792 = !DILocation(line: 102, column: 7, scope: !783)
!793 = !DILocation(line: 104, column: 5, scope: !783)
!794 = !DILocation(line: 104, column: 16, scope: !783)
!795 = !DILocation(line: 105, column: 14, scope: !783)
!796 = !DILocation(line: 105, column: 5, scope: !783)
!797 = distinct !DISubprogram(name: "core.format-uint_1879", linkageName: "core.format-uint_1879", scope: !27, file: !27, line: 303, type: !798, scopeLine: 303, spFlags: DISPFlagDefinition, unit: !0)
!798 = !DISubroutineType(types: !799)
!799 = !{!787, !52, !133, !133}
!800 = !DILocation(line: 304, column: 6, scope: !797)
!801 = !DILocation(line: 304, column: 3, scope: !797)
!802 = !DILocation(line: 304, column: 19, scope: !797)
!803 = !DILocation(line: 304, column: 38, scope: !797)
!804 = !DILocation(line: 305, column: 11, scope: !797)
!805 = !DILocation(line: 305, column: 8, scope: !797)
!806 = !DILocation(line: 306, column: 35, scope: !797)
!807 = !DILocation(line: 306, column: 17, scope: !797)
!808 = !DILocation(line: 307, column: 18, scope: !797)
!809 = !DILocation(line: 307, column: 5, scope: !797)
!810 = !DILocation(line: 308, column: 21, scope: !797)
!811 = !DILocation(line: 308, column: 13, scope: !797)
!812 = !DILocation(line: 308, column: 10, scope: !797)
!813 = !DILocation(line: 309, column: 17, scope: !797)
!814 = !DILocation(line: 310, column: 41, scope: !797)
!815 = !DILocation(line: 310, column: 23, scope: !797)
!816 = !DILocation(line: 311, column: 18, scope: !797)
!817 = !DILocation(line: 312, column: 42, scope: !797)
!818 = !DILocation(line: 312, column: 24, scope: !797)
!819 = !DILocation(line: 313, column: 18, scope: !797)
!820 = !DILocation(line: 313, column: 5, scope: !797)
!821 = !DILocation(line: 314, column: 18, scope: !797)
!822 = !DILocation(line: 314, column: 5, scope: !797)
!823 = !DILocation(line: 316, column: 21, scope: !797)
!824 = !DILocation(line: 317, column: 18, scope: !797)
!825 = !DILocation(line: 318, column: 5, scope: !797)
!826 = !DILocation(line: 318, column: 11, scope: !797)
!827 = !DILocation(line: 319, column: 15, scope: !797)
!828 = !DILocation(line: 320, column: 33, scope: !797)
!829 = !DILocation(line: 320, column: 15, scope: !797)
!830 = !DILocation(line: 321, column: 19, scope: !797)
!831 = !DILocation(line: 321, column: 7, scope: !797)
!832 = !DILocation(line: 322, column: 19, scope: !797)
!833 = !DILocation(line: 322, column: 7, scope: !797)
!834 = !DILocation(line: 325, column: 5, scope: !797)
!835 = !DILocation(line: 327, column: 19, scope: !797)
!836 = !DILocation(line: 327, column: 5, scope: !797)
!837 = distinct !DISubprogram(name: "core.list.append-buffer-in_1873", linkageName: "core.list.append-buffer-in_1873", scope: !784, file: !784, line: 144, type: !838, scopeLine: 144, spFlags: DISPFlagDefinition, unit: !0)
!838 = !DISubroutineType(types: !839)
!839 = !{!840, !52, !52, !49}
!840 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !797, file: !27, line: 1, align: 1, elements: !18, identifier: "empty")
!841 = !DILocation(line: 144, column: 6, scope: !837)
!842 = !DILocation(line: 145, column: 18, scope: !837)
!843 = !DILocation(line: 145, column: 29, scope: !837)
!844 = !DILocation(line: 146, column: 8, scope: !837)
!845 = !DILocation(line: 146, column: 17, scope: !837)
!846 = !DILocation(line: 146, column: 5, scope: !837)
!847 = !DILocation(line: 147, column: 21, scope: !837)
!848 = !DILocation(line: 148, column: 33, scope: !837)
!849 = !DILocation(line: 148, column: 43, scope: !837)
!850 = !DILocation(line: 148, column: 30, scope: !837)
!851 = !DILocation(line: 150, column: 15, scope: !837)
!852 = !DILocation(line: 150, column: 49, scope: !837)
!853 = !DILocation(line: 150, column: 65, scope: !837)
!854 = !DILocation(line: 151, column: 5, scope: !837)
!855 = !DILocation(line: 152, column: 5, scope: !837)
!856 = !DILocation(line: 152, column: 16, scope: !837)
!857 = !DILocation(line: 148, column: 50, scope: !837)
!858 = !DILocation(line: 148, column: 63, scope: !837)
!859 = !DILocation(line: 148, column: 7, scope: !837)
!860 = distinct !DISubprogram(name: "core.buffer._grow-to_1878", linkageName: "core.buffer._grow-to_1878", scope: !222, file: !222, line: 100, type: !861, scopeLine: 100, spFlags: DISPFlagDefinition, unit: !0)
!861 = !DISubroutineType(types: !862)
!862 = !{!863, !52, !52, !54}
!863 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !837, file: !784, line: 1, align: 1, elements: !18, identifier: "empty")
!864 = !DILocation(line: 101, column: 20, scope: !860)
!865 = !DILocation(line: 102, column: 41, scope: !860)
!866 = !DILocation(line: 102, column: 20, scope: !860)
!867 = !DILocation(line: 104, column: 17, scope: !860)
!868 = !DILocation(line: 105, column: 18, scope: !860)
!869 = !DILocation(line: 107, column: 18, scope: !860)
!870 = !DILocation(line: 103, column: 19, scope: !860)
!871 = !DILocation(line: 109, column: 41, scope: !860)
!872 = !DILocation(line: 109, column: 14, scope: !860)
!873 = !DILocation(line: 109, column: 23, scope: !860)
!874 = !DILocation(line: 109, column: 5, scope: !860)
!875 = distinct !DISubprogram(name: "core.buffer.copy_1877", linkageName: "core.buffer.copy_1877", scope: !222, file: !222, line: 122, type: !876, scopeLine: 122, spFlags: DISPFlagDefinition, unit: !0)
!876 = !DISubroutineType(types: !877)
!877 = !{!863, !49, !49}
!878 = !DILocation(line: 122, column: 6, scope: !875)
!879 = !DILocation(line: 123, column: 8, scope: !875)
!880 = !DILocation(line: 123, column: 18, scope: !875)
!881 = !DILocation(line: 123, column: 5, scope: !875)
!882 = !DILocation(line: 124, column: 7, scope: !875)
!883 = !DILocation(line: 124, column: 13, scope: !875)
!884 = !DILocation(line: 124, column: 53, scope: !875)
!885 = !DILocation(line: 124, column: 66, scope: !875)
!886 = !DILocation(line: 126, column: 8, scope: !875)
!887 = !DILocation(line: 126, column: 5, scope: !875)
!888 = !DILocation(line: 126, column: 21, scope: !875)
!889 = !DILocation(line: 127, column: 42, scope: !875)
!890 = !DILocation(line: 127, column: 21, scope: !875)
!891 = !DILocation(line: 128, column: 20, scope: !875)
!892 = !DILocation(line: 128, column: 42, scope: !875)
!893 = !DILocation(line: 128, column: 66, scope: !875)
!894 = !DILocation(line: 128, column: 5, scope: !875)
!895 = distinct !DISubprogram(name: "core.arena.try-realloc_357", linkageName: "core.arena.try-realloc_357", scope: !534, file: !534, line: 182, type: !896, scopeLine: 182, spFlags: DISPFlagDefinition, unit: !0)
!896 = !DISubroutineType(types: !897)
!897 = !{!52, !52, !52, !54, !54, !54}
!898 = !DILocation(line: 183, column: 5, scope: !895)
!899 = !DILocation(line: 183, column: 12, scope: !895)
!900 = !DILocation(line: 184, column: 27, scope: !895)
!901 = !DILocation(line: 185, column: 27, scope: !895)
!902 = !DILocation(line: 186, column: 8, scope: !895)
!903 = !DILocation(line: 186, column: 27, scope: !895)
!904 = !DILocation(line: 186, column: 5, scope: !895)
!905 = !DILocation(line: 188, column: 7, scope: !895)
!906 = !DILocation(line: 188, column: 23, scope: !895)
!907 = !DILocation(line: 191, column: 28, scope: !895)
!908 = !DILocation(line: 192, column: 22, scope: !895)
!909 = !DILocation(line: 192, column: 7, scope: !895)
!910 = !DILocation(line: 193, column: 7, scope: !895)
!911 = !DILocation(line: 186, column: 45, scope: !895)
!912 = !DILocation(line: 186, column: 64, scope: !895)
!913 = distinct !DISubprogram(name: "core.list._grow_1942", linkageName: "core.list._grow_1942", scope: !784, file: !784, line: 80, type: !914, scopeLine: 80, spFlags: DISPFlagDefinition, unit: !0)
!914 = !DISubroutineType(types: !915)
!915 = !{!916, !52, !52}
!916 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !783, file: !784, line: 1, align: 1, elements: !18, identifier: "empty")
!917 = !DILocation(line: 81, column: 28, scope: !913)
!918 = !DILocation(line: 81, column: 25, scope: !913)
!919 = !DILocation(line: 83, column: 23, scope: !913)
!920 = !DILocation(line: 84, column: 10, scope: !913)
!921 = !DILocation(line: 84, column: 7, scope: !913)
!922 = !DILocation(line: 86, column: 7, scope: !913)
!923 = !DILocation(line: 88, column: 28, scope: !913)
!924 = !DILocation(line: 88, column: 5, scope: !913)
!925 = distinct !DISubprogram(name: "core.impl_22as-buffer.set_for_t1665_1944", linkageName: "core.impl_22as-buffer.set_for_t1665_1944", scope: !1, file: !1, line: 521, type: !926, scopeLine: 521, spFlags: DISPFlagDefinition, unit: !0)
!926 = !DISubroutineType(types: !927)
!927 = !{!916, !52, !54, !101}
!928 = !DILocation(line: 522, column: 5, scope: !925)
!929 = distinct !DISubprogram(name: "core.io.stdout-buffer-bytes_584", linkageName: "core.io.stdout-buffer-bytes_584", scope: !27, file: !27, line: 89, type: !930, scopeLine: 89, spFlags: DISPFlagDefinition, unit: !0)
!930 = !DISubroutineType(types: !931)
!931 = !{!932, !46}
!932 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !278, file: !27, line: 1, align: 1, elements: !18, identifier: "empty")
!933 = !DILocation(line: 89, column: 6, scope: !929)
!934 = !DILocation(line: 90, column: 8, scope: !929)
!935 = !DILocation(line: 90, column: 5, scope: !929)
!936 = !DILocation(line: 91, column: 7, scope: !929)
!937 = !DILocation(line: 92, column: 20, scope: !929)
!938 = !DILocation(line: 92, column: 38, scope: !929)
!939 = !DILocation(line: 92, column: 7, scope: !929)
!940 = !DILocation(line: 93, column: 7, scope: !929)
!941 = !DILocation(line: 95, column: 8, scope: !929)
!942 = !DILocation(line: 95, column: 27, scope: !929)
!943 = !DILocation(line: 95, column: 5, scope: !929)
!944 = !DILocation(line: 96, column: 7, scope: !929)
!945 = !DILocation(line: 98, column: 5, scope: !929)
!946 = !DILocation(line: 99, column: 8, scope: !929)
!947 = !DILocation(line: 99, column: 35, scope: !929)
!948 = !DILocation(line: 99, column: 5, scope: !929)
!949 = !DILocation(line: 100, column: 7, scope: !929)
!950 = !DILocation(line: 99, column: 45, scope: !929)
!951 = distinct !DISubprogram(name: "core.fixlist.push-n_2016", linkageName: "core.fixlist.push-n_2016", scope: !307, file: !307, line: 58, type: !952, scopeLine: 58, spFlags: DISPFlagDefinition, unit: !0)
!952 = !DISubroutineType(types: !953)
!953 = !{!954, !52, !46}
!954 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !929, file: !27, line: 1, align: 1, elements: !18, identifier: "empty")
!955 = !DILocation(line: 58, column: 6, scope: !951)
!956 = !DILocation(line: 59, column: 18, scope: !951)
!957 = !DILocation(line: 60, column: 8, scope: !951)
!958 = !DILocation(line: 60, column: 18, scope: !951)
!959 = !DILocation(line: 60, column: 5, scope: !951)
!960 = !DILocation(line: 60, column: 25, scope: !951)
!961 = distinct !DISubprogram(name: "core.span.contains_2017", linkageName: "core.span.contains_2017", scope: !272, file: !272, line: 91, type: !962, scopeLine: 91, spFlags: DISPFlagDefinition, unit: !0)
!962 = !DISubroutineType(types: !963)
!963 = !{!372, !46, !101}
!964 = !DILocation(line: 91, column: 6, scope: !961)
!965 = !DILocation(line: 92, column: 5, scope: !961)
!966 = distinct !DISubprogram(name: "core.buffer.position_2030", linkageName: "core.buffer.position_2030", scope: !222, file: !222, line: 285, type: !967, scopeLine: 285, spFlags: DISPFlagDefinition, unit: !0)
!967 = !DISubroutineType(types: !968)
!968 = !{!969, !49, !101}
!969 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.opt_of_i64_", scope: !961, file: !272, line: 91, size: 16, align: 8, elements: !970, identifier: "core.opt_of_i64_")
!970 = !{!971, !972}
!971 = !DIDerivedType(tag: DW_TAG_member, name: "tag", scope: !961, file: !272, line: 91, baseType: !101, size: 8, align: 8)
!972 = !DIDerivedType(tag: DW_TAG_member, name: "payload", scope: !961, file: !272, line: 91, baseType: !973, size: 64, align: 64, offset: 8)
!973 = !DICompositeType(tag: DW_TAG_union_type, name: "core.opt_of_i64_", scope: !961, file: !272, line: 91, size: 64, align: 64, elements: !974, identifier: "core.opt_of_i64_")
!974 = !{!54}
!975 = !DILocation(line: 285, column: 6, scope: !966)
!976 = !DILocation(line: 286, column: 5, scope: !966)
!977 = !DILocation(line: 287, column: 39, scope: !966)
!978 = !DILocation(line: 287, column: 56, scope: !966)
!979 = !DILocation(line: 287, column: 66, scope: !966)
!980 = !DILocation(line: 287, column: 25, scope: !966)
!981 = distinct !DISubprogram(name: "core.buffer.position-byte_473", linkageName: "core.buffer.position-byte_473", scope: !222, file: !222, line: 234, type: !982, scopeLine: 234, spFlags: DISPFlagDefinition, unit: !0)
!982 = !DISubroutineType(types: !983)
!983 = !{!969, !52, !54, !101}
!984 = !DILocation(line: 235, column: 22, scope: !981)
!985 = !DILocation(line: 236, column: 13, scope: !981)
!986 = !DILocation(line: 237, column: 15, scope: !981)
!987 = !DILocation(line: 237, column: 8, scope: !981)
!988 = !DILocation(line: 237, column: 5, scope: !981)
!989 = !DILocation(line: 238, column: 19, scope: !981)
!990 = !DILocation(line: 239, column: 7, scope: !981)
!991 = !DILocation(line: 246, column: 5, scope: !981)
!992 = !DILocation(line: 239, column: 13, scope: !981)
!993 = !DILocation(line: 239, column: 17, scope: !981)
!994 = !DILocation(line: 240, column: 78, scope: !981)
!995 = !DILocation(line: 240, column: 62, scope: !981)
!996 = !DILocation(line: 240, column: 21, scope: !981)
!997 = !DILocation(line: 241, column: 20, scope: !981)
!998 = !DILocation(line: 242, column: 12, scope: !981)
!999 = !DILocation(line: 242, column: 9, scope: !981)
!1000 = !DILocation(line: 242, column: 29, scope: !981)
!1001 = !DILocation(line: 242, column: 36, scope: !981)
!1002 = !DILocation(line: 242, column: 40, scope: !981)
!1003 = !DILocation(line: 242, column: 22, scope: !981)
!1004 = !DILocation(line: 243, column: 13, scope: !981)
!1005 = !DILocation(line: 243, column: 17, scope: !981)
!1006 = !DILocation(line: 243, column: 9, scope: !981)
!1007 = !DILocation(line: 246, column: 11, scope: !981)
!1008 = !DILocation(line: 250, column: 5, scope: !981)
!1009 = !DILocation(line: 247, column: 26, scope: !981)
!1010 = !DILocation(line: 247, column: 10, scope: !981)
!1011 = !DILocation(line: 247, column: 7, scope: !981)
!1012 = !DILocation(line: 247, column: 48, scope: !981)
!1013 = !DILocation(line: 247, column: 54, scope: !981)
!1014 = !DILocation(line: 247, column: 41, scope: !981)
!1015 = !DILocation(line: 248, column: 11, scope: !981)
!1016 = !DILocation(line: 248, column: 7, scope: !981)
!1017 = distinct !DISubprogram(name: "core.vector.first-true-lane_1553", linkageName: "core.vector.first-true-lane_1553", scope: !1018, file: !1018, line: 56, type: !1019, scopeLine: 56, spFlags: DISPFlagDefinition, unit: !0)
!1018 = !DIFile(filename: "vector.k1", directory: "/Users/knix/dev/k1/modules/core")
!1019 = !DISubroutineType(types: !1020)
!1020 = !{!54, !1021}
!1021 = !DICompositeType(tag: DW_TAG_array_type, baseType: !101, size: 128, align: 128, elements: !18)
!1022 = !DILocation(line: 56, column: 6, scope: !1017)
!1023 = !DILocation(line: 57, column: 16, scope: !1017)
!1024 = !DILocation(line: 58, column: 8, scope: !1017)
!1025 = !DILocation(line: 58, column: 5, scope: !1017)
!1026 = !DILocation(line: 58, column: 32, scope: !1017)
!1027 = distinct !DISubprogram(name: "core.fixlist.try-push-n_2018", linkageName: "core.fixlist.try-push-n_2018", scope: !307, file: !307, line: 46, type: !1028, scopeLine: 46, spFlags: DISPFlagDefinition, unit: !0)
!1028 = !DISubroutineType(types: !1029)
!1029 = !{!1030, !52, !46}
!1030 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.result_of_empty__empty_", scope: !951, file: !307, line: 58, size: 1, align: 1, elements: !1031, identifier: "core.result_of_empty__empty_")
!1031 = !{!1032, !1033}
!1032 = !DIDerivedType(tag: DW_TAG_member, name: "tag", scope: !951, file: !307, line: 58, baseType: !101, size: 8, align: 8)
!1033 = !DIDerivedType(tag: DW_TAG_member, name: "payload", scope: !951, file: !307, line: 58, baseType: !1034, align: 8, offset: 1)
!1034 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !951, file: !307, line: 1, align: 1, elements: !18, identifier: "empty")
!1035 = !DILocation(line: 46, column: 6, scope: !1027)
!1036 = !DILocation(line: 47, column: 18, scope: !1027)
!1037 = !DILocation(line: 47, column: 31, scope: !1027)
!1038 = !DILocation(line: 48, column: 8, scope: !1027)
!1039 = !DILocation(line: 48, column: 5, scope: !1027)
!1040 = !DILocation(line: 49, column: 23, scope: !1027)
!1041 = !DILocation(line: 49, column: 59, scope: !1027)
!1042 = !DILocation(line: 49, column: 77, scope: !1027)
!1043 = !DILocation(line: 50, column: 25, scope: !1027)
!1044 = !DILocation(line: 50, column: 7, scope: !1027)
!1045 = !DILocation(line: 51, column: 7, scope: !1027)
!1046 = !DILocation(line: 51, column: 19, scope: !1027)
!1047 = !DILocation(line: 52, column: 7, scope: !1027)
!1048 = !DILocation(line: 54, column: 7, scope: !1027)
!1049 = distinct !DISubprogram(name: "core.impl_22as-buffer.as-buffer_for_t3358_2019", linkageName: "core.impl_22as-buffer.as-buffer_for_t3358_2019", scope: !1, file: !1, line: 1032, type: !1050, scopeLine: 1032, spFlags: DISPFlagDefinition, unit: !0)
!1050 = !DISubroutineType(types: !1051)
!1051 = !{!49, !52}
!1052 = !DILocation(line: 1033, column: 33, scope: !1049)
!1053 = !DILocation(line: 1033, column: 5, scope: !1049)
!1054 = !DILocation(line: 1033, column: 14, scope: !1049)
!1055 = distinct !DISubprogram(name: "core.buffer.swap_1923", linkageName: "core.buffer.swap_1923", scope: !222, file: !222, line: 398, type: !1056, scopeLine: 398, spFlags: DISPFlagDefinition, unit: !0)
!1056 = !DISubroutineType(types: !1057)
!1057 = !{!1058, !128, !54, !54}
!1058 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !248, file: !222, line: 1, align: 1, elements: !18, identifier: "empty")
!1059 = !DILocation(line: 398, column: 6, scope: !1055)
!1060 = !DILocation(line: 399, column: 8, scope: !1055)
!1061 = !DILocation(line: 399, column: 5, scope: !1055)
!1062 = !DILocation(line: 399, column: 27, scope: !1055)
!1063 = !DILocation(line: 400, column: 17, scope: !1055)
!1064 = !DILocation(line: 401, column: 17, scope: !1055)
!1065 = !DILocation(line: 402, column: 19, scope: !1055)
!1066 = !DILocation(line: 403, column: 5, scope: !1055)
!1067 = !DILocation(line: 403, column: 15, scope: !1055)
!1068 = !DILocation(line: 404, column: 5, scope: !1055)
!1069 = !DILocation(line: 404, column: 15, scope: !1055)
!1070 = distinct !DISubprogram(name: "core.buffer.check-bounds_1925", linkageName: "core.buffer.check-bounds_1925", scope: !222, file: !222, line: 168, type: !1071, scopeLine: 168, spFlags: DISPFlagDefinition, unit: !0)
!1071 = !DISubroutineType(types: !1072)
!1072 = !{!1073, !128, !54}
!1073 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !1055, file: !222, line: 1, align: 1, elements: !18, identifier: "empty")
!1074 = !DILocation(line: 168, column: 6, scope: !1070)
!1075 = !DILocation(line: 169, column: 28, scope: !1070)
!1076 = !DILocation(line: 169, column: 8, scope: !1070)
!1077 = !DILocation(line: 169, column: 5, scope: !1070)
!1078 = !DILocation(line: 170, column: 7, scope: !1070)
!1079 = !DILocation(line: 170, column: 13, scope: !1070)
!1080 = !DILocation(line: 170, column: 36, scope: !1070)
!1081 = !DILocation(line: 170, column: 46, scope: !1070)
!1082 = !DISubprogram(name: "write", linkageName: "write", scope: !38, file: !38, line: 468, type: !1083, scopeLine: 468, spFlags: 0)
!1083 = !DISubroutineType(types: !1084)
!1084 = !{!54, !11, !52, !133}
!1085 = !DISubprogram(name: "__error", linkageName: "__error", scope: !38, file: !38, line: 516, type: !442, scopeLine: 516, spFlags: 0)
!1086 = distinct !DISubprogram(name: "core.span.to-array_6209", linkageName: "core.span.to-array_6209", scope: !272, file: !272, line: 63, type: !1087, scopeLine: 63, spFlags: DISPFlagDefinition, unit: !0)
!1087 = !DISubroutineType(types: !1088)
!1088 = !{!1089, !1090}
!1089 = !DICompositeType(tag: DW_TAG_array_type, baseType: !54, size: 320, align: 64, elements: !18)
!1090 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.span_of_i64_", scope: !7, file: !8, line: 2, size: 16, align: 8, elements: !1091, identifier: "core.span_of_i64_")
!1091 = !{!1092}
!1092 = !DIDerivedType(tag: DW_TAG_member, name: "buffer", scope: !7, file: !8, line: 2, baseType: !1093, size: 128, align: 64)
!1093 = !DICompositeType(tag: DW_TAG_structure_type, name: "core.buffer_of_i64_", scope: !7, file: !8, line: 2, size: 16, align: 8, elements: !1094, identifier: "core.buffer_of_i64_")
!1094 = !{!1095, !1096}
!1095 = !DIDerivedType(tag: DW_TAG_member, name: "data", scope: !7, file: !8, line: 2, baseType: !52, size: 64, align: 64)
!1096 = !DIDerivedType(tag: DW_TAG_member, name: "len", scope: !7, file: !8, line: 2, baseType: !54, size: 64, align: 64, offset: 8)
!1097 = !DILocation(line: 63, column: 6, scope: !1086)
!1098 = !DILocation(line: 64, column: 29, scope: !1086)
!1099 = !DILocation(line: 64, column: 5, scope: !1086)
!1100 = distinct !DISubprogram(name: "core.println_6211", linkageName: "core.println_6211", scope: !27, file: !27, line: 31, type: !1101, scopeLine: 31, spFlags: DISPFlagDefinition, unit: !0)
!1101 = !DISubroutineType(types: !1102)
!1102 = !{!1103, !1089}
!1103 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !7, file: !8, line: 1, align: 1, elements: !18, identifier: "empty")
!1104 = !DILocation(line: 32, column: 3, scope: !1100)
!1105 = !DILocation(line: 33, column: 3, scope: !1100)
!1106 = distinct !DISubprogram(name: "core.print_6214", linkageName: "core.print_6214", scope: !27, file: !27, line: 22, type: !1107, scopeLine: 22, spFlags: DISPFlagDefinition, unit: !0)
!1107 = !DISubroutineType(types: !1108)
!1108 = !{!1109, !1089}
!1109 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !1100, file: !27, line: 1, align: 1, elements: !18, identifier: "empty")
!1110 = !DILocation(line: 24, column: 9, scope: !1106)
!1111 = !DILocation(line: 24, column: 3, scope: !1106)
!1112 = !DILocation(line: 25, column: 13, scope: !1106)
!1113 = !DILocation(line: 26, column: 30, scope: !1106)
!1114 = !DILocation(line: 26, column: 5, scope: !1106)
!1115 = !DILocation(line: 28, column: 5, scope: !1106)
!1116 = distinct !DISubprogram(name: "core.print_2069", linkageName: "core.print_2069", scope: !27, file: !27, line: 22, type: !1117, scopeLine: 22, spFlags: DISPFlagDefinition, unit: !0)
!1117 = !DISubroutineType(types: !1118)
!1118 = !{!1109, !122}
!1119 = !DILocation(line: 22, column: 4, scope: !1116)
!1120 = !DILocation(line: 24, column: 9, scope: !1116)
!1121 = !DILocation(line: 24, column: 3, scope: !1116)
!1122 = !DILocation(line: 25, column: 13, scope: !1116)
!1123 = !DILocation(line: 26, column: 30, scope: !1116)
!1124 = !DILocation(line: 26, column: 5, scope: !1116)
!1125 = !DILocation(line: 28, column: 5, scope: !1116)
!1126 = distinct !DISubprogram(name: "core.impl_5print.print-to_for_t33_1957", linkageName: "core.impl_5print.print-to_for_t33_1957", scope: !1, file: !1, line: 698, type: !1127, scopeLine: 698, spFlags: DISPFlagDefinition, unit: !0)
!1127 = !DISubroutineType(types: !1128)
!1128 = !{!1129, !122, !167}
!1129 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !1116, file: !27, line: 1, align: 1, elements: !18, identifier: "empty")
!1130 = !DILocation(line: 698, column: 6, scope: !1126)
!1131 = !DILocation(line: 698, column: 45, scope: !1126)
!1132 = distinct !DISubprogram(name: "core.impl_6show.show_for_t33_1347", linkageName: "core.impl_6show.show_for_t33_1347", scope: !777, file: !777, line: 217, type: !1133, scopeLine: 217, spFlags: DISPFlagDefinition, unit: !0)
!1133 = !DISubroutineType(types: !1134)
!1134 = !{!122, !122}
!1135 = !DILocation(line: 217, column: 6, scope: !1132)
!1136 = !DILocation(line: 217, column: 35, scope: !1132)
!1137 = distinct !DISubprogram(name: "core.impl_5print.print-to_for_t9365_6226", linkageName: "core.impl_5print.print-to_for_t9365_6226", scope: !1, file: !1, line: 1044, type: !1138, scopeLine: 1044, spFlags: DISPFlagDefinition, unit: !0)
!1138 = !DISubroutineType(types: !1139)
!1139 = !{!1140, !1089, !167}
!1140 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !1106, file: !27, line: 1, align: 1, elements: !18, identifier: "empty")
!1141 = !DILocation(line: 1044, column: 6, scope: !1137)
!1142 = !DILocation(line: 1045, column: 23, scope: !1137)
!1143 = !DILocation(line: 1046, column: 5, scope: !1137)
!1144 = !DILocation(line: 1047, column: 19, scope: !1137)
!1145 = !DILocation(line: 1048, column: 5, scope: !1137)
!1146 = !DILocation(line: 1048, column: 11, scope: !1137)
!1147 = !DILocation(line: 1048, column: 15, scope: !1137)
!1148 = !DILocation(line: 1053, column: 5, scope: !1137)
!1149 = !DILocation(line: 1049, column: 10, scope: !1137)
!1150 = !DILocation(line: 1049, column: 7, scope: !1137)
!1151 = !DILocation(line: 1049, column: 18, scope: !1137)
!1152 = !DILocation(line: 1050, column: 16, scope: !1137)
!1153 = !DILocation(line: 1050, column: 7, scope: !1137)
!1154 = !DILocation(line: 1051, column: 11, scope: !1137)
!1155 = !DILocation(line: 1051, column: 7, scope: !1137)
!1156 = distinct !DISubprogram(name: "core.impl_6show.show_for_t9365_6225", linkageName: "core.impl_6show.show_for_t9365_6225", scope: !27, file: !27, line: 283, type: !1157, scopeLine: 283, spFlags: DISPFlagDefinition, unit: !0)
!1157 = !DISubroutineType(types: !1158)
!1158 = !{!122, !1089}
!1159 = !DILocation(line: 284, column: 14, scope: !1156)
!1160 = !DILocation(line: 285, column: 5, scope: !1156)
!1161 = !DILocation(line: 286, column: 5, scope: !1156)
!1162 = distinct !DISubprogram(name: "core.impl_5print.print-to_for_t9365_6228", linkageName: "core.impl_5print.print-to_for_t9365_6228", scope: !1, file: !1, line: 1044, type: !1163, scopeLine: 1044, spFlags: DISPFlagDefinition, unit: !0)
!1163 = !DISubroutineType(types: !1164)
!1164 = !{!1165, !1089, !52}
!1165 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !1156, file: !27, line: 1, align: 1, elements: !18, identifier: "empty")
!1166 = !DILocation(line: 1045, column: 23, scope: !1162)
!1167 = !DILocation(line: 1046, column: 5, scope: !1162)
!1168 = !DILocation(line: 1047, column: 19, scope: !1162)
!1169 = !DILocation(line: 1048, column: 5, scope: !1162)
!1170 = !DILocation(line: 1048, column: 11, scope: !1162)
!1171 = !DILocation(line: 1048, column: 15, scope: !1162)
!1172 = !DILocation(line: 1053, column: 5, scope: !1162)
!1173 = !DILocation(line: 1049, column: 10, scope: !1162)
!1174 = !DILocation(line: 1049, column: 7, scope: !1162)
!1175 = !DILocation(line: 1049, column: 18, scope: !1162)
!1176 = !DILocation(line: 1050, column: 16, scope: !1162)
!1177 = !DILocation(line: 1050, column: 7, scope: !1162)
!1178 = !DILocation(line: 1051, column: 11, scope: !1162)
!1179 = !DILocation(line: 1051, column: 7, scope: !1162)
!1180 = distinct !DISubprogram(name: "core.string-builder.build_1258", linkageName: "core.string-builder.build_1258", scope: !394, file: !394, line: 17, type: !426, scopeLine: 17, spFlags: DISPFlagDefinition, unit: !0)
!1181 = !DILocation(line: 18, column: 23, scope: !1180)
!1182 = !DILocation(line: 18, column: 5, scope: !1180)
!1183 = distinct !DISubprogram(name: "core.impl_23as-span.as-span_for_t160_1754", linkageName: "core.impl_23as-span.as-span_for_t160_1754", scope: !1, file: !1, line: 621, type: !1184, scopeLine: 621, spFlags: DISPFlagDefinition, unit: !0)
!1184 = !DISubroutineType(types: !1185)
!1185 = !{!46, !400}
!1186 = !DILocation(line: 621, column: 48, scope: !1183)
!1187 = !DILocation(line: 621, column: 31, scope: !1183)
!1188 = distinct !DISubprogram(name: "core.string.wrap-bytes_1274", linkageName: "core.string.wrap-bytes_1274", scope: !777, file: !777, line: 30, type: !1189, scopeLine: 30, spFlags: DISPFlagDefinition, unit: !0)
!1189 = !DISubroutineType(types: !1190)
!1190 = !{!122, !46}
!1191 = !DILocation(line: 30, column: 6, scope: !1188)
!1192 = !DILocation(line: 31, column: 15, scope: !1188)
!1193 = !DILocation(line: 31, column: 5, scope: !1188)
!1194 = distinct !DISubprogram(name: "core.string.cloned_1270", linkageName: "core.string.cloned_1270", scope: !777, file: !777, line: 14, type: !1133, scopeLine: 14, spFlags: DISPFlagDefinition, unit: !0)
!1195 = !DILocation(line: 14, column: 6, scope: !1194)
!1196 = !DILocation(line: 15, column: 15, scope: !1194)
!1197 = !DILocation(line: 15, column: 5, scope: !1194)
!1198 = distinct !DISubprogram(name: "core.span.cloned_2700", linkageName: "core.span.cloned_2700", scope: !272, file: !272, line: 55, type: !1199, scopeLine: 55, spFlags: DISPFlagDefinition, unit: !0)
!1199 = !DISubroutineType(types: !1200)
!1200 = !{!125, !125}
!1201 = !DILocation(line: 55, column: 6, scope: !1198)
!1202 = !DILocation(line: 56, column: 17, scope: !1198)
!1203 = !DILocation(line: 56, column: 5, scope: !1198)
!1204 = distinct !DISubprogram(name: "core.impl_4writer.write-char_for_t2342_1262", linkageName: "core.impl_4writer.write-char_for_t2342_1262", scope: !1, file: !1, line: 242, type: !1205, scopeLine: 242, spFlags: DISPFlagDefinition, unit: !0)
!1205 = !DISubroutineType(types: !1206)
!1206 = !{!1207, !52, !293}
!1207 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !1162, file: !1, line: 1, align: 1, elements: !18, identifier: "empty")
!1208 = !DILocation(line: 242, column: 38, scope: !1204)
!1209 = distinct !DISubprogram(name: "core.io.impl_4writer.write-char_for_t162_587", linkageName: "core.io.impl_4writer.write-char_for_t162_587", scope: !1, file: !1, line: 242, type: !1210, scopeLine: 242, spFlags: DISPFlagDefinition, unit: !0)
!1210 = !DISubroutineType(types: !1211)
!1211 = !{!1212, !167, !293}
!1212 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !1137, file: !1, line: 1, align: 1, elements: !18, identifier: "empty")
!1213 = !DILocation(line: 242, column: 6, scope: !1209)
!1214 = !DILocation(line: 242, column: 38, scope: !1209)
!1215 = distinct !DISubprogram(name: "core.impl_5print.print-to_for_t9_6229", linkageName: "core.impl_5print.print-to_for_t9_6229", scope: !27, file: !27, line: 376, type: !1216, scopeLine: 376, spFlags: DISPFlagDefinition, unit: !0)
!1216 = !DISubroutineType(types: !1217)
!1217 = !{!1212, !54, !167}
!1218 = !DILocation(line: 376, column: 6, scope: !1215)
!1219 = !DILocation(line: 377, column: 15, scope: !1215)
!1220 = !DILocation(line: 378, column: 8, scope: !1215)
!1221 = !DILocation(line: 378, column: 5, scope: !1215)
!1222 = !DILocation(line: 379, column: 7, scope: !1215)
!1223 = !DILocation(line: 382, column: 18, scope: !1215)
!1224 = !DILocation(line: 382, column: 15, scope: !1215)
!1225 = !DILocation(line: 382, column: 22, scope: !1215)
!1226 = !DILocation(line: 383, column: 28, scope: !1215)
!1227 = !DILocation(line: 383, column: 5, scope: !1215)
!1228 = distinct !DISubprogram(name: "core.i64.unsigned_654", linkageName: "core.i64.unsigned_654", scope: !27, file: !27, line: 486, type: !1229, scopeLine: 486, spFlags: DISPFlagDefinition, unit: !0)
!1229 = !DISubroutineType(types: !1230)
!1230 = !{!133, !54}
!1231 = !DILocation(line: 486, column: 33, scope: !1228)
!1232 = distinct !DISubprogram(name: "core.array.from-buffer_6212", linkageName: "core.array.from-buffer_6212", scope: !1, file: !1, line: 1020, type: !1233, scopeLine: 1020, spFlags: DISPFlagDefinition, unit: !0)
!1233 = !DISubroutineType(types: !1234)
!1234 = !{!1089, !1093}
!1235 = !DILocation(line: 1020, column: 6, scope: !1232)
!1236 = !DILocation(line: 1022, column: 8, scope: !1232)
!1237 = !DILocation(line: 1022, column: 5, scope: !1232)
!1238 = !DILocation(line: 1022, column: 36, scope: !1232)
!1239 = !DILocation(line: 1024, column: 37, scope: !1232)
!1240 = !DILocation(line: 1024, column: 5, scope: !1232)
!1241 = !DILocation(line: 1026, column: 5, scope: !1232)
!1242 = distinct !DISubprogram(name: "core.buffer.len_3306", linkageName: "core.buffer.len_3306", scope: !222, file: !222, line: 22, type: !1243, scopeLine: 22, spFlags: DISPFlagDefinition, unit: !0)
!1243 = !DISubroutineType(types: !1244)
!1244 = !{!54, !1093}
!1245 = !DILocation(line: 22, column: 6, scope: !1242)
!1246 = !DILocation(line: 22, column: 38, scope: !1242)
!1247 = distinct !DISubprogram(name: "core.impl_22as-buffer.as-buffer_for_t9377_6215", linkageName: "core.impl_22as-buffer.as-buffer_for_t9377_6215", scope: !1, file: !1, line: 1032, type: !1248, scopeLine: 1032, spFlags: DISPFlagDefinition, unit: !0)
!1248 = !DISubroutineType(types: !1249)
!1249 = !{!1093, !52}
!1250 = !DILocation(line: 1033, column: 33, scope: !1247)
!1251 = !DILocation(line: 1033, column: 5, scope: !1247)
!1252 = !DILocation(line: 1033, column: 14, scope: !1247)
!1253 = distinct !DISubprogram(name: "core.buffer.copy_3302", linkageName: "core.buffer.copy_3302", scope: !222, file: !222, line: 122, type: !1254, scopeLine: 122, spFlags: DISPFlagDefinition, unit: !0)
!1254 = !DISubroutineType(types: !1255)
!1255 = !{!1256, !1093, !1093}
!1256 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !1232, file: !1, line: 1, align: 1, elements: !18, identifier: "empty")
!1257 = !DILocation(line: 122, column: 6, scope: !1253)
!1258 = !DILocation(line: 123, column: 8, scope: !1253)
!1259 = !DILocation(line: 123, column: 18, scope: !1253)
!1260 = !DILocation(line: 123, column: 5, scope: !1253)
!1261 = !DILocation(line: 124, column: 7, scope: !1253)
!1262 = !DILocation(line: 124, column: 13, scope: !1253)
!1263 = !DILocation(line: 124, column: 53, scope: !1253)
!1264 = !DILocation(line: 124, column: 66, scope: !1253)
!1265 = !DILocation(line: 126, column: 8, scope: !1253)
!1266 = !DILocation(line: 126, column: 5, scope: !1253)
!1267 = !DILocation(line: 126, column: 21, scope: !1253)
!1268 = !DILocation(line: 127, column: 42, scope: !1253)
!1269 = !DILocation(line: 127, column: 21, scope: !1253)
!1270 = !DILocation(line: 128, column: 20, scope: !1253)
!1271 = !DILocation(line: 128, column: 42, scope: !1253)
!1272 = !DILocation(line: 128, column: 66, scope: !1253)
!1273 = !DILocation(line: 128, column: 5, scope: !1253)
!1274 = distinct !DISubprogram(name: "core.buffer.buffer-size-bytes_3224", linkageName: "core.buffer.buffer-size-bytes_3224", scope: !222, file: !222, line: 14, type: !1275, scopeLine: 14, spFlags: DISPFlagDefinition, unit: !0)
!1275 = !DISubroutineType(types: !1276)
!1276 = !{!54, !54}
!1277 = !DILocation(line: 15, column: 5, scope: !1274)
!1278 = distinct !DISubprogram(name: "core.buffer.data-ptr_1851", linkageName: "core.buffer.data-ptr_1851", scope: !222, file: !222, line: 20, type: !1279, scopeLine: 20, spFlags: DISPFlagDefinition, unit: !0)
!1279 = !DISubroutineType(types: !1280)
!1280 = !{!52, !1093}
!1281 = !DILocation(line: 20, column: 6, scope: !1278)
!1282 = !DILocation(line: 20, column: 42, scope: !1278)
!1283 = distinct !DISubprogram(name: "core.impl_5print.print-to_for_t33_1398", linkageName: "core.impl_5print.print-to_for_t33_1398", scope: !1, file: !1, line: 698, type: !1284, scopeLine: 698, spFlags: DISPFlagDefinition, unit: !0)
!1284 = !DISubroutineType(types: !1285)
!1285 = !{!1286, !122, !52}
!1286 = !DICompositeType(tag: DW_TAG_structure_type, name: "empty", scope: !1253, file: !222, line: 1, align: 1, elements: !18, identifier: "empty")
!1287 = !DILocation(line: 698, column: 6, scope: !1283)
!1288 = !DILocation(line: 698, column: 45, scope: !1283)
!1289 = distinct !DISubprogram(name: "core.array.get-base_6227", linkageName: "core.array.get-base_6227", scope: !1, file: !1, line: 984, type: !1290, scopeLine: 984, spFlags: DISPFlagDefinition, unit: !0)
!1290 = !DISubroutineType(types: !1291)
!1291 = !{!52, !52}
!1292 = !DILocation(line: 985, column: 5, scope: !1289)
