; ModuleID = 'agg.c'
source_filename = "agg.c"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx14.4.0"

%struct.P3 = type { i64, i64, i64 }

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define i64 @one(ptr noundef %0) #0 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  %3 = getelementptr inbounds %struct.P3, ptr %0, i32 0, i32 0
  %4 = load i64, ptr %3, align 8
  ret i64 %4
}

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define void @structRet(ptr noalias sret(%struct.P3) align 8 %0) #0 {
  %2 = getelementptr inbounds %struct.P3, ptr %0, i32 0, i32 0
  store i64 1, ptr %2, align 8
  %3 = getelementptr inbounds %struct.P3, ptr %0, i32 0, i32 1
  store i64 2, ptr %3, align 8
  %4 = getelementptr inbounds %struct.P3, ptr %0, i32 0, i32 2
  store i64 3, ptr %4, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.P3, align 8
  %3 = alloca %struct.P3, align 8
  store i32 0, ptr %1, align 4
  %4 = getelementptr inbounds %struct.P3, ptr %2, i32 0, i32 0
  store i64 3, ptr %4, align 8
  %5 = getelementptr inbounds %struct.P3, ptr %2, i32 0, i32 1
  store i64 4, ptr %5, align 8
  %6 = getelementptr inbounds %struct.P3, ptr %2, i32 0, i32 2
  store i64 0, ptr %6, align 8
  call void @structRet(ptr sret(%struct.P3) align 8 %3)
  %7 = getelementptr inbounds %struct.P3, ptr %3, i32 0, i32 2
  %8 = load i64, ptr %7, align 8
  %9 = trunc i64 %8 to i32
  ret i32 %9
}

attributes #0 = { noinline nounwind optnone ssp uwtable(sync) "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8.5a,+v8a,+zcm,+zcz" }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 14, i32 4]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 8, !"PIC Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 1}
!4 = !{i32 7, !"frame-pointer", i32 1}
!5 = !{!"Apple clang version 15.0.0 (clang-1500.3.9.4)"}
