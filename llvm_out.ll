; ModuleID = 'resources/test_src/codegen_fn.nx'
source_filename = "resources/test_src/codegen_fn.nx"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-darwin22.3.0"

@formatString = unnamed_addr constant [3 x i8] c"%i\0A"
@z = constant i64 1337

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define i64 @add(i64 %x, i64 %y) #0 {
entry:
  %add = add i64 %x, 1
  %add5 = add i64 %add, 1337
  %add9 = add i64 %add5, %y
  ret i64 %add9
}

define i64 @main(i64 %x) {
entry:
  %add = call i64 @add(i64 1, i64 2)
  %println_res = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @formatString, i32 0, i32 0), i64 %add)
  ret i64 0
}

declare i32 @printf(i8*, ...)

attributes #0 = { mustprogress nofree norecurse nosync nounwind readnone willreturn }
