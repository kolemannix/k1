; ModuleID = 'resources/nexsample/codegen_fn.nx'
source_filename = "resources/nexsample/codegen_fn.nx"

@formatString = unnamed_addr constant [2 x i8] c"%s"

define i32 @main() {
entry:
  %a = alloca i32, align 4
  store i32 0, i32* %a, align 4
  %b = alloca i32, align 4
  store i32 1, i32* %b, align 4
  %arg1 = alloca [9 x i8], align 1
  store [9 x i8] c"123456789", [9 x i8]* %arg1, align 1
  %value = bitcast [9 x i8]* %arg1 to i8*
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @formatString, i32 0, i32 0), i8* %value)
  %ret = load i32, i32* %a, align 4
  ret i32 %ret
}

declare i32 @printf(i8*, ...)
