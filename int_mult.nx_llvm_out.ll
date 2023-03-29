; ModuleID = 'int_mult.nx'
source_filename = "int_mult.nx"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-darwin22.3.0"

@formatString = unnamed_addr constant [3 x i8] c"%i\0A"

declare i32 @printf(i8*, ...)

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define i64 @mult(i64 %x, i64 %y) #0 {
entry:
  %mul = mul i64 %x, %y
  ret i64 %mul
}

define i64 @main(i64 %x) {
entry:
  %println_res = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([3 x i8], [3 x i8]* @formatString, i64 0, i64 0), i64 440)
  ret i64 0
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind readnone willreturn }
