; ModuleID = 'scratch.c'
source_filename = "scratch.c"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx14.0.0"

@.str = private unnamed_addr constant [5 x i8] c"%lld\00", align 1
@.str.1 = private unnamed_addr constant [9 x i8] c"len: %d\0A\00", align 1
@.str.2 = private unnamed_addr constant [14 x i8] c"Buffer is: %s\00", align 1

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca ptr, align 8
  %3 = alloca i64, align 8
  %4 = alloca i32, align 4
  store i32 0, ptr %1, align 4
  %5 = call ptr @malloc(i64 noundef 1024) #4
  store ptr %5, ptr %2, align 8
  store i64 45, ptr %3, align 8
  %6 = call i64 @llvm.objectsize.i64.p0(ptr null, i1 false, i1 true, i1 false)
  %7 = load i64, ptr %3, align 8
  %8 = call i32 (ptr, i64, i32, i64, ptr, ...) @__snprintf_chk(ptr noundef null, i64 noundef 0, i32 noundef 0, i64 noundef %6, ptr noundef @.str, i64 noundef %7)
  %9 = add nsw i32 %8, 1
  store i32 %9, ptr %4, align 4
  %10 = load i32, ptr %4, align 4
  %11 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %10)
  %12 = load ptr, ptr %2, align 8
  %13 = load i32, ptr %4, align 4
  %14 = sext i32 %13 to i64
  %15 = load ptr, ptr %2, align 8
  %16 = call i64 @llvm.objectsize.i64.p0(ptr %15, i1 false, i1 true, i1 false)
  %17 = load i64, ptr %3, align 8
  %18 = call i32 (ptr, i64, i32, i64, ptr, ...) @__snprintf_chk(ptr noundef %12, i64 noundef %14, i32 noundef 0, i64 noundef %16, ptr noundef @.str, i64 noundef %17)
  %19 = load ptr, ptr %2, align 8
  %20 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, ptr noundef %19)
  ret i32 0
}

; Function Attrs: allocsize(0)
declare ptr @malloc(i64 noundef) #1

declare i32 @__snprintf_chk(ptr noundef, i64 noundef, i32 noundef, i64 noundef, ptr noundef, ...) #2

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.objectsize.i64.p0(ptr, i1 immarg, i1 immarg, i1 immarg) #3

declare i32 @printf(ptr noundef, ...) #2

attributes #0 = { noinline nounwind optnone ssp uwtable(sync) "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8.5a,+v8a,+zcm,+zcz" }
attributes #1 = { allocsize(0) "frame-pointer"="non-leaf" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8.5a,+v8a,+zcm,+zcz" }
attributes #2 = { "frame-pointer"="non-leaf" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8.5a,+v8a,+zcm,+zcz" }
attributes #3 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #4 = { allocsize(0) }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 14, i32 4]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 8, !"PIC Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 1}
!4 = !{i32 7, !"frame-pointer", i32 1}
!5 = !{!"Apple clang version 15.0.0 (clang-1500.3.9.4)"}
