; ModuleID = '/Users/knix/Downloads/rosetta.c'
source_filename = "/Users/knix/Downloads/rosetta.c"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx13.0.0"

@__const.main.str = private unnamed_addr constant [6 x i8] c"mystr\00", align 1
@.str = private unnamed_addr constant [8 x i8] c"done:)\0A\00", align 1

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define i32 @returny_func(i32* noundef %0, i8 noundef signext %1, i16 noundef signext %2, i32 noundef %3) #0 {
  %5 = alloca i32*, align 8
  %6 = alloca i8, align 1
  %7 = alloca i16, align 2
  %8 = alloca i32, align 4
  store i32* %0, i32** %5, align 8
  store i8 %1, i8* %6, align 1
  store i16 %2, i16* %7, align 2
  store i32 %3, i32* %8, align 4
  %9 = load i8, i8* %6, align 1
  %10 = sext i8 %9 to i32
  %11 = load i16, i16* %7, align 2
  %12 = sext i16 %11 to i32
  %13 = add nsw i32 %10, %12
  ret i32 %13
}

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define i32 @main(i32 noundef %0, i8** noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i8**, align 8
  %6 = alloca i64, align 8
  %7 = alloca i32, align 4
  %8 = alloca [6 x i8], align 1
  %9 = alloca i32, align 4
  %10 = alloca i32, align 4
  store i32 0, i32* %3, align 4
  store i32 %0, i32* %4, align 4
  store i8** %1, i8*** %5, align 8
  store i64 -4990328140781978930, i64* %6, align 8
  store i32 -559026163, i32* %7, align 4
  %11 = bitcast [6 x i8]* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %11, i8* align 1 getelementptr inbounds ([6 x i8], [6 x i8]* @__const.main.str, i32 0, i32 0), i64 6, i1 false)
  store i32 1337, i32* %9, align 4
  br label %12

12:                                               ; preds = %15, %2
  %13 = load i32, i32* %9, align 4
  %14 = icmp ne i32 %13, 0
  br i1 %14, label %15, label %18

15:                                               ; preds = %12
  %16 = load i32, i32* %9, align 4
  %17 = add nsw i32 %16, -1
  store i32 %17, i32* %9, align 4
  br label %12, !llvm.loop !10

18:                                               ; preds = %12
  %19 = call i32 @returny_func(i32* noundef %9, i8 noundef signext 66, i16 noundef signext 105, i32 noundef 201527)
  store i32 %19, i32* %10, align 4
  %20 = call i32 (i32, ...) @syscall(i32 noundef 4, i32 noundef 1, i8* noundef getelementptr inbounds ([8 x i8], [8 x i8]* @.str, i64 0, i64 0), i32 noundef 7)
  ret i32 32
}

; Function Attrs: argmemonly nofree nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #1

declare i32 @syscall(i32 noundef, ...) #2

attributes #0 = { noinline nounwind optnone ssp uwtable(sync) "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" }
attributes #1 = { argmemonly nofree nounwind willreturn }
attributes #2 = { "frame-pointer"="non-leaf" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" }

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6, !7, !8}
!llvm.ident = !{!9}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 13, i32 3]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 8, !"branch-target-enforcement", i32 0}
!3 = !{i32 8, !"sign-return-address", i32 0}
!4 = !{i32 8, !"sign-return-address-all", i32 0}
!5 = !{i32 8, !"sign-return-address-with-bkey", i32 0}
!6 = !{i32 7, !"PIC Level", i32 2}
!7 = !{i32 7, !"uwtable", i32 1}
!8 = !{i32 7, !"frame-pointer", i32 1}
!9 = !{!"Apple clang version 14.0.3 (clang-1403.0.22.14.1)"}
!10 = distinct !{!10, !11}
!11 = !{!"llvm.loop.mustprogress"}
