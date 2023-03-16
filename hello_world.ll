
@formatString = unnamed_addr constant [2 x i8] c"%s"

define i32 @main() {
entry:
  %arg1 = alloca [9 x i8], align 1
  store [9 x i8] c"123456789", [9 x i8]* %arg1, align 1
  %value = bitcast [9 x i8]* %arg1 to i8*
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @formatString, i32 0, i32 0), i8* %value)
  ret i32 %printf
}

declare i32 @printf(i8*, ...)
