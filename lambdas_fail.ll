; ModuleID = 'lambdas'
source_filename = "builtin.k1"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-darwin24.3.0"

%"Opt[bool]" = type { %"Opt[bool].Some" }
%"Opt[bool].Some" = type { i8, i8 }
%"51" = type { %"51.String" }
%"51.String" = type { i64, { { i64, ptr } } }
%"Opt[enum Char(char) | String(string)]" = type { %"Opt[enum Char(char) | String(string)].Some" }
%"Opt[enum Char(char) | String(string)].Some" = type { i64, %"51" }
%"51.Char" = type { i64, i8 }
%"Opt[char]" = type { %"Opt[char].Some" }
%"Opt[char].Some" = type { i8, i8 }
%"Opt[enum Char(char) | String(string)].None" = type { i64 }
%"Opt[char].None" = type { i8 }
%"Opt[i64]" = type { %"Opt[i64].Some" }
%"Opt[i64].Some" = type { i64, i64 }
%"Opt[i64].None" = type { i64 }

@_root__K1_TEST = constant i8 0
@str_data = constant [5 x i8] c"macos"
@str = constant { { i64, ptr } } { { i64, ptr } { i64 5, ptr @str_data } }
@_root__K1_OS = constant ptr @str
@_root__K1_NO_STD = constant i8 0
@_root__files__unix__SEEK_END = constant i32 2
@_root__files__unix__SEEK_SET = constant i32 0
@_root__Arena__mb = constant i64 1048576
@_root__Arena__gb = constant i64 1073741824
@str_data.1 = constant [10 x i8] c"lambdas.k1"
@str.2 = constant { { i64, ptr } } { { i64, ptr } { i64 10, ptr @str_data.1 } }
@str_data.3 = constant [13 x i8] c"ASSERT FAILED"
@str.4 = constant { { i64, ptr } } { { i64, ptr } { i64 13, ptr @str_data.3 } }
@str_data.5 = constant [0 x i8] zeroinitializer
@str.6 = constant { { i64, ptr } } { { i64, ptr } { i64 0, ptr @str_data.5 } }
@str_data.7 = constant [9 x i8] c"buffer.k1"
@str.8 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.7 } }
@str_data.9 = constant [40 x i8] c"Buffer copy source index out of bounds: "
@str.10 = constant { { i64, ptr } } { { i64, ptr } { i64 40, ptr @str_data.9 } }
@str_data.11 = constant [1 x i8] c"0"
@str.12 = constant { { i64, ptr } } { { i64, ptr } { i64 1, ptr @str_data.11 } }
@str_data.13 = constant [9 x i8] c"buffer.k1"
@str.14 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.13 } }
@str_data.15 = constant [40 x i8] c"Buffer copy source index out of bounds: "
@str.16 = constant { { i64, ptr } } { { i64, ptr } { i64 40, ptr @str_data.15 } }
@str_data.17 = constant [9 x i8] c"buffer.k1"
@str.18 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.17 } }
@str_data.19 = constant [33 x i8] c"slice start index out of bounds: "
@str.20 = constant { { i64, ptr } } { { i64, ptr } { i64 33, ptr @str_data.19 } }
@str_data.21 = constant [9 x i8] c"buffer.k1"
@str.22 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.21 } }
@str_data.23 = constant [26 x i8] c"Buffer index out of bounds"
@str.24 = constant { { i64, ptr } } { { i64, ptr } { i64 26, ptr @str_data.23 } }
@str_data.25 = constant [6 x i8] c"opt.k1"
@str.26 = constant { { i64, ptr } } { { i64, ptr } { i64 6, ptr @str_data.25 } }
@str_data.27 = constant [11 x i8] c"Match Error"
@str.28 = constant { { i64, ptr } } { { i64, ptr } { i64 11, ptr @str_data.27 } }
@str_data.29 = constant [6 x i8] c"opt.k1"
@str.30 = constant { { i64, ptr } } { { i64, ptr } { i64 6, ptr @str_data.29 } }
@str_data.31 = constant [15 x i8] c"Opt.get on None"
@str.32 = constant { { i64, ptr } } { { i64, ptr } { i64 15, ptr @str_data.31 } }
@str_data.33 = constant [17 x i8] c"string_builder.k1"
@str.34 = constant { { i64, ptr } } { { i64, ptr } { i64 17, ptr @str_data.33 } }
@str_data.35 = constant [11 x i8] c"Match Error"
@str.36 = constant { { i64, ptr } } { { i64, ptr } { i64 11, ptr @str_data.35 } }
@str_data.37 = constant [9 x i8] c"buffer.k1"
@str.38 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.37 } }
@str_data.39 = constant [26 x i8] c"Buffer index out of bounds"
@str.40 = constant { { i64, ptr } } { { i64, ptr } { i64 26, ptr @str_data.39 } }
@str_data.41 = constant [6 x i8] c"opt.k1"
@str.42 = constant { { i64, ptr } } { { i64, ptr } { i64 6, ptr @str_data.41 } }
@str_data.43 = constant [11 x i8] c"Match Error"
@str.44 = constant { { i64, ptr } } { { i64, ptr } { i64 11, ptr @str_data.43 } }
@str_data.45 = constant [6 x i8] c"opt.k1"
@str.46 = constant { { i64, ptr } } { { i64, ptr } { i64 6, ptr @str_data.45 } }
@str_data.47 = constant [15 x i8] c"Opt.get on None"
@str.48 = constant { { i64, ptr } } { { i64, ptr } { i64 15, ptr @str_data.47 } }
@str_data.49 = constant [17 x i8] c"string_builder.k1"
@str.50 = constant { { i64, ptr } } { { i64, ptr } { i64 17, ptr @str_data.49 } }
@str_data.51 = constant [11 x i8] c"Match Error"
@str.52 = constant { { i64, ptr } } { { i64, ptr } { i64 11, ptr @str_data.51 } }
@str_data.53 = constant [9 x i8] c"buffer.k1"
@str.54 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.53 } }
@str_data.55 = constant [33 x i8] c"slice start index out of bounds: "
@str.56 = constant { { i64, ptr } } { { i64, ptr } { i64 33, ptr @str_data.55 } }
@str_data.57 = constant [9 x i8] c"buffer.k1"
@str.58 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.57 } }
@str_data.59 = constant [38 x i8] c"Buffer copy dest index out of bounds: "
@str.60 = constant { { i64, ptr } } { { i64, ptr } { i64 38, ptr @str_data.59 } }
@str_data.61 = constant [9 x i8] c"buffer.k1"
@str.62 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.61 } }
@str_data.63 = constant [38 x i8] c"Buffer copy dest index out of bounds: "
@str.64 = constant { { i64, ptr } } { { i64, ptr } { i64 38, ptr @str_data.63 } }
@str_data.65 = constant [4 x i8] c" at "
@str.66 = constant { { i64, ptr } } { { i64, ptr } { i64 4, ptr @str_data.65 } }
@str_data.67 = constant [1 x i8] c":"
@str.68 = constant { { i64, ptr } } { { i64, ptr } { i64 1, ptr @str_data.67 } }
@str_data.69 = constant [1 x i8] c"\0A"
@str.70 = constant { { i64, ptr } } { { i64, ptr } { i64 1, ptr @str_data.69 } }
@str_data.71 = constant [10 x i8] c"lambdas.k1"
@str.72 = constant { { i64, ptr } } { { i64, ptr } { i64 10, ptr @str_data.71 } }
@str_data.73 = constant [10 x i8] c"lambdas.k1"
@str.74 = constant { { i64, ptr } } { { i64, ptr } { i64 10, ptr @str_data.73 } }
@str_data.75 = constant [15 x i8] c"ASSERT FAILED: "
@str.76 = constant { { i64, ptr } } { { i64, ptr } { i64 15, ptr @str_data.75 } }
@str_data.77 = constant [1 x i8] c"0"
@str.78 = constant { { i64, ptr } } { { i64, ptr } { i64 1, ptr @str_data.77 } }
@str_data.79 = constant [4 x i8] c" != "
@str.80 = constant { { i64, ptr } } { { i64, ptr } { i64 4, ptr @str_data.79 } }
@str_data.81 = constant [10 x i8] c"lambdas.k1"
@str.82 = constant { { i64, ptr } } { { i64, ptr } { i64 10, ptr @str_data.81 } }
@str_data.83 = constant [10 x i8] c"lambdas.k1"
@str.84 = constant { { i64, ptr } } { { i64, ptr } { i64 10, ptr @str_data.83 } }
@str_data.85 = constant [10 x i8] c"lambdas.k1"
@str.86 = constant { { i64, ptr } } { { i64, ptr } { i64 10, ptr @str_data.85 } }
@str_data.87 = constant [10 x i8] c"lambdas.k1"
@str.88 = constant { { i64, ptr } } { { i64, ptr } { i64 10, ptr @str_data.87 } }
@str_data.89 = constant [6 x i8] c"opt.k1"
@str.90 = constant { { i64, ptr } } { { i64, ptr } { i64 6, ptr @str_data.89 } }
@str_data.91 = constant [11 x i8] c"Match Error"
@str.92 = constant { { i64, ptr } } { { i64, ptr } { i64 11, ptr @str_data.91 } }
@str_data.93 = constant [9 x i8] c"buffer.k1"
@str.94 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.93 } }
@str_data.95 = constant [40 x i8] c"Buffer copy source index out of bounds: "
@str.96 = constant { { i64, ptr } } { { i64, ptr } { i64 40, ptr @str_data.95 } }
@str_data.97 = constant [9 x i8] c"buffer.k1"
@str.98 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.97 } }
@str_data.99 = constant [38 x i8] c"Buffer copy dest index out of bounds: "
@str.100 = constant { { i64, ptr } } { { i64, ptr } { i64 38, ptr @str_data.99 } }
@str_data.101 = constant [9 x i8] c"buffer.k1"
@str.102 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.101 } }
@str_data.103 = constant [26 x i8] c"Buffer index out of bounds"
@str.104 = constant { { i64, ptr } } { { i64, ptr } { i64 26, ptr @str_data.103 } }
@str_data.105 = constant [9 x i8] c"buffer.k1"
@str.106 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.105 } }
@str_data.107 = constant [33 x i8] c"slice start index out of bounds: "
@str.108 = constant { { i64, ptr } } { { i64, ptr } { i64 33, ptr @str_data.107 } }
@str_data.109 = constant [6 x i8] c"opt.k1"
@str.110 = constant { { i64, ptr } } { { i64, ptr } { i64 6, ptr @str_data.109 } }
@str_data.111 = constant [11 x i8] c"Match Error"
@str.112 = constant { { i64, ptr } } { { i64, ptr } { i64 11, ptr @str_data.111 } }
@str_data.113 = constant [6 x i8] c"opt.k1"
@str.114 = constant { { i64, ptr } } { { i64, ptr } { i64 6, ptr @str_data.113 } }
@str_data.115 = constant [15 x i8] c"Opt.get on None"
@str.116 = constant { { i64, ptr } } { { i64, ptr } { i64 15, ptr @str_data.115 } }
@str_data.117 = constant [10 x i8] c"lambdas.k1"
@str.118 = constant { { i64, ptr } } { { i64, ptr } { i64 10, ptr @str_data.117 } }
@str_data.119 = constant [6 x i8] c"Thunk!"
@str.120 = constant { { i64, ptr } } { { i64, ptr } { i64 6, ptr @str_data.119 } }
@str_data.121 = constant [10 x i8] c"lambdas.k1"
@str.122 = constant { { i64, ptr } } { { i64, ptr } { i64 10, ptr @str_data.121 } }
@str_data.123 = constant [10 x i8] c"lambdas.k1"
@str.124 = constant { { i64, ptr } } { { i64, ptr } { i64 10, ptr @str_data.123 } }
@str_data.125 = constant [10 x i8] c"lambdas.k1"
@str.126 = constant { { i64, ptr } } { { i64, ptr } { i64 10, ptr @str_data.125 } }
@str_data.127 = constant [10 x i8] c"lambdas.k1"
@str.128 = constant { { i64, ptr } } { { i64, ptr } { i64 10, ptr @str_data.127 } }

define i64 @main() {
entry:
  %0 = call i8 @lambdas.test()
  ret i64 0
}

define i8 @lambdas.test() {
entry:
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.2, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 91, ptr %line_store_addr, align 8
  %0 = call i8 @Equals_Pointer_equals(ptr @lambdas.staticAddOne, ptr @lambdas.staticAddOne)
  %1 = call i8 @assert(ptr %struct_literal, i8 %0)
  %struct_literal1 = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr2 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal1, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr2, ptr align 8 @str.72, i64 16, i1 false)
  %line_store_addr3 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal1, i32 0, i32 1
  store i64 93, ptr %line_store_addr3, align 8
  %2 = call i8 @Equals_Pointer_equals(ptr @lambdas.staticAddOne, ptr @lambdas.staticAddTwo)
  %3 = trunc i8 %2 to i1
  %4 = xor i1 %3, true
  %5 = sext i1 %4 to i8
  %6 = call i8 @assert(ptr %struct_literal1, i8 %5)
  %staticAddOneVar = alloca ptr, align 8
  store ptr @lambdas.staticAddOne, ptr %staticAddOneVar, align 8
  %staticAddOneVar_loaded = load ptr, ptr %staticAddOneVar, align 8
  %7 = call i64 @lambdas.someFnParam2_spec__1(i64 5, ptr @lambdas.staticAddOne, ptr %staticAddOneVar_loaded)
  %staticResult = alloca i64, align 8
  store i64 %7, ptr %staticResult, align 8
  %struct_literal4 = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr5 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal4, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr5, ptr align 8 @str.74, i64 16, i1 false)
  %line_store_addr6 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal4, i32 0, i32 1
  store i64 97, ptr %line_store_addr6, align 8
  %staticResult_loaded = load i64, ptr %staticResult, align 8
  %8 = call i8 @assertEquals_spec_i64_1(ptr %struct_literal4, i64 %staticResult_loaded, i64 12)
  %9 = call i64 @lambdas.someFnParam1_spec__1(i64 5, ptr @lambdas.staticAddOne)
  %lam1ResultPtr = alloca i64, align 8
  store i64 %9, ptr %lam1ResultPtr, align 8
  %struct_literal7 = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr8 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal7, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr8, ptr align 8 @str.82, i64 16, i1 false)
  %line_store_addr9 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal7, i32 0, i32 1
  store i64 100, ptr %line_store_addr9, align 8
  %lam1ResultPtr_loaded = load i64, ptr %lam1ResultPtr, align 8
  %10 = call i8 @assertEquals_spec_i64_1(ptr %struct_literal7, i64 %lam1ResultPtr_loaded, i64 6)
  %fn2obj = alloca { ptr, ptr }, align 8
  %fn_ptr = getelementptr inbounds { ptr, ptr }, ptr %fn2obj, i32 0, i32 0
  store ptr @lambdas.staticAddOne__dyn, ptr %fn_ptr, align 8
  %nop_env = getelementptr inbounds { ptr, ptr }, ptr %fn2obj, i32 0, i32 1
  store ptr null, ptr %nop_env, align 8
  %11 = call i64 @lambdas.someFnParam1_spec__2(i64 5, ptr %fn2obj)
  %lam1ResultDyn = alloca i64, align 8
  store i64 %11, ptr %lam1ResultDyn, align 8
  %struct_literal10 = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr11 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal10, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr11, ptr align 8 @str.84, i64 16, i1 false)
  %line_store_addr12 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal10, i32 0, i32 1
  store i64 103, ptr %line_store_addr12, align 8
  %lam1ResultDyn_loaded = load i64, ptr %lam1ResultDyn, align 8
  %12 = call i8 @assertEquals_spec_i64_1(ptr %struct_literal10, i64 %lam1ResultDyn_loaded, i64 6)
  %struct_literal13 = alloca { i64 }, align 8
  %x_store_addr = getelementptr inbounds { i64 }, ptr %struct_literal13, i32 0, i32 0
  store i64 3, ptr %x_store_addr, align 8
  %c1 = alloca ptr, align 8
  store ptr %struct_literal13, ptr %c1, align 8
  %c2 = alloca i64, align 8
  store i64 2, ptr %c2, align 8
  %struct_literal14 = alloca { ptr }, align 8
  %c1_loaded = load ptr, ptr %c1, align 8
  %c1_store_addr = getelementptr inbounds { ptr }, ptr %struct_literal14, i32 0, i32 0
  store ptr %c1_loaded, ptr %c1_store_addr, align 8
  %struct_literal15 = alloca { i64 }, align 8
  %c2_loaded = load i64, ptr %c2, align 8
  %c2_store_addr = getelementptr inbounds { i64 }, ptr %struct_literal15, i32 0, i32 0
  store i64 %c2_loaded, ptr %c2_store_addr, align 8
  %13 = call i64 @lambdas.someFnParam2_spec__2(i64 5, ptr %struct_literal14, ptr %struct_literal15)
  %withClosures = alloca i64, align 8
  store i64 %13, ptr %withClosures, align 8
  %struct_literal16 = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr17 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal16, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr17, ptr align 8 @str.86, i64 16, i1 false)
  %line_store_addr18 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal16, i32 0, i32 1
  store i64 109, ptr %line_store_addr18, align 8
  %withClosures_loaded = load i64, ptr %withClosures, align 8
  %14 = call i8 @assertEquals_spec_i64_1(ptr %struct_literal16, i64 %withClosures_loaded, i64 18)
  %struct_literal19 = alloca {}, align 8
  %struct_literal20 = alloca {}, align 8
  %call_sret = alloca %"Opt[bool]", align 8
  call void @lambdas.someFnParamGen_spec_bool_1(ptr sret(%"Opt[bool]") %call_sret, i8 1, ptr %struct_literal19, ptr %struct_literal20)
  %withGenerics = alloca %"Opt[bool]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %withGenerics, ptr align 1 %call_sret, i64 2, i1 false)
  %struct_literal21 = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr22 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal21, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr22, ptr align 8 @str.88, i64 16, i1 false)
  %line_store_addr23 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal21, i32 0, i32 1
  store i64 112, ptr %line_store_addr23, align 8
  %call_sret24 = alloca %"Opt[bool]", align 8
  call void @some_spec_bool_4(ptr sret(%"Opt[bool]") %call_sret24, i8 0)
  %15 = call i8 @"Equals_Opt[bool]_equals"(ptr %withGenerics, ptr %call_sret24)
  %16 = call i8 @assert(ptr %struct_literal21, i8 %15)
  %call_sret25 = alloca { i64, { i64, ptr } }, align 8
  call void @List.withCapacity_spec_i64_10(ptr sret({ i64, { i64, ptr } }) %call_sret25, i64 7)
  %__list_literal_1200 = alloca ptr, align 8
  store ptr %call_sret25, ptr %__list_literal_1200, align 8
  %__list_literal_1200_loaded = load ptr, ptr %__list_literal_1200, align 8
  %17 = call i8 @List.push_spec_i64_11(ptr %__list_literal_1200_loaded, i64 1)
  %__list_literal_1200_loaded26 = load ptr, ptr %__list_literal_1200, align 8
  %18 = call i8 @List.push_spec_i64_11(ptr %__list_literal_1200_loaded26, i64 2)
  %__list_literal_1200_loaded27 = load ptr, ptr %__list_literal_1200, align 8
  %19 = call i8 @List.push_spec_i64_11(ptr %__list_literal_1200_loaded27, i64 3)
  %__list_literal_1200_loaded28 = load ptr, ptr %__list_literal_1200, align 8
  %20 = call i8 @List.push_spec_i64_11(ptr %__list_literal_1200_loaded28, i64 5)
  %__list_literal_1200_loaded29 = load ptr, ptr %__list_literal_1200, align 8
  %21 = call i8 @List.push_spec_i64_11(ptr %__list_literal_1200_loaded29, i64 6)
  %__list_literal_1200_loaded30 = load ptr, ptr %__list_literal_1200, align 8
  %22 = call i8 @List.push_spec_i64_11(ptr %__list_literal_1200_loaded30, i64 7)
  %__list_literal_1200_loaded31 = load ptr, ptr %__list_literal_1200, align 8
  %23 = call i8 @List.push_spec_i64_11(ptr %__list_literal_1200_loaded31, i64 8)
  %__list_literal_1200_loaded32 = load ptr, ptr %__list_literal_1200, align 8
  %struct_literal33 = alloca {}, align 8
  %call_sret34 = alloca { i64, { i64, ptr } }, align 8
  call void @lambdas.map_spec_i64i64_1(ptr sret({ i64, { i64, ptr } }) %call_sret34, ptr %__list_literal_1200_loaded32, ptr %struct_literal33)
  %struct_literal35 = alloca {}, align 8
  %call_sret36 = alloca { i64, { i64, ptr } }, align 8
  call void @lambdas.filter_spec_i64_1(ptr sret({ i64, { i64, ptr } }) %call_sret36, ptr %call_sret34, ptr %struct_literal35)
  %result = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %result, ptr align 8 %call_sret36, i64 24, i1 false)
  %struct_literal37 = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr38 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal37, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr38, ptr align 8 @str.118, i64 16, i1 false)
  %line_store_addr39 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal37, i32 0, i32 1
  store i64 115, ptr %line_store_addr39, align 8
  %call_sret40 = alloca { i64, { i64, ptr } }, align 8
  call void @List.withCapacity_spec_i64_10(ptr sret({ i64, { i64, ptr } }) %call_sret40, i64 4)
  %__list_literal_1206 = alloca ptr, align 8
  store ptr %call_sret40, ptr %__list_literal_1206, align 8
  %__list_literal_1206_loaded = load ptr, ptr %__list_literal_1206, align 8
  %24 = call i8 @List.push_spec_i64_11(ptr %__list_literal_1206_loaded, i64 2)
  %__list_literal_1206_loaded41 = load ptr, ptr %__list_literal_1206, align 8
  %25 = call i8 @List.push_spec_i64_11(ptr %__list_literal_1206_loaded41, i64 4)
  %__list_literal_1206_loaded42 = load ptr, ptr %__list_literal_1206, align 8
  %26 = call i8 @List.push_spec_i64_11(ptr %__list_literal_1206_loaded42, i64 6)
  %__list_literal_1206_loaded43 = load ptr, ptr %__list_literal_1206, align 8
  %27 = call i8 @List.push_spec_i64_11(ptr %__list_literal_1206_loaded43, i64 8)
  %__list_literal_1206_loaded44 = load ptr, ptr %__list_literal_1206, align 8
  %28 = call i8 @"Equals_List[i64]_equals"(ptr %result, ptr %__list_literal_1206_loaded44)
  %29 = call i8 @assert(ptr %struct_literal37, i8 %28)
  %struct_literal45 = alloca {}, align 8
  %env_ptr = alloca ptr, align 8
  store ptr %struct_literal45, ptr %env_ptr, align 8
  %30 = insertvalue { ptr, ptr } { ptr @"lambdas.lambdas__test_{lambda}_1214", ptr undef }, ptr %struct_literal45, 1
  %lam_obj_ptr = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %30, ptr %lam_obj_ptr, align 8
  %31 = call i8 @lambdas.simpleDyn(ptr %lam_obj_ptr)
  %32 = call i8 @lambdas.captures()
  %33 = call i8 @lambdas.captureMut()
  %34 = call i8 @lambdas.closureReturn()
  ret i8 0
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i64 @lambdas.staticAddOne(i64 %x_arg) {
entry:
  %x = alloca i64, align 8
  store i64 %x_arg, ptr %x, align 8
  %x_loaded = load i64, ptr %x, align 8
  %add = add i64 %x_loaded, 1
  ret i64 %add
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

define i8 @Equals_Pointer_equals(ptr %self_arg, ptr %other_arg) {
entry:
  %self = alloca ptr, align 8
  store ptr %self_arg, ptr %self, align 8
  %other = alloca ptr, align 8
  store ptr %other_arg, ptr %other, align 8
  %self_loaded = load ptr, ptr %self, align 8
  %ptrtoint_cast = ptrtoint ptr %self_loaded to i64
  %other_loaded = load ptr, ptr %other, align 8
  %ptrtoint_cast1 = ptrtoint ptr %other_loaded to i64
  %"==_i1" = icmp eq i64 %ptrtoint_cast, %ptrtoint_cast1
  %"==_res" = sext i1 %"==_i1" to i8
  ret i8 %"==_res"
}

define i8 @assert(ptr %locn_arg, i8 %value_arg) {
entry:
  %locn = alloca { { { i64, ptr } }, i64 }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %locn, ptr align 8 %locn_arg, i64 24, i1 false)
  %value = alloca i8, align 1
  store i8 %value_arg, ptr %value, align 1
  br label %arm_0

arm_0:                                            ; preds = %entry
  %value_loaded = load i8, ptr %value, align 1
  %0 = trunc i8 %value_loaded to i1
  %1 = xor i1 %0, true
  %2 = sext i1 %1 to i8
  %arm_pattern_i1 = trunc i8 %2 to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  call void @crash(ptr %locn, ptr @str.4)
  unreachable

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1
  %match_result = phi i8 [ 0, %arm_cons_1 ]
  ret i8 %match_result
}

define void @crash(ptr %locn_arg, ptr %msg_arg) {
entry:
  %locn = alloca { { { i64, ptr } }, i64 }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %locn, ptr align 8 %locn_arg, i64 24, i1 false)
  %msg = alloca { { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %msg, ptr align 8 %msg_arg, i64 16, i1 false)
  %struc.filename = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %locn, i32 0, i32 0
  %filename = alloca { { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename, ptr align 8 %struc.filename, i64 16, i1 false)
  %struc.line = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %locn, i32 0, i32 1
  %struc.line1 = load i64, ptr %struc.line, align 8
  %line = alloca i64, align 8
  store i64 %struc.line1, ptr %line, align 8
  %0 = call i8 @sys.printBacktrace()
  %call_sret = alloca { ptr }, align 8
  call void @StringBuilder.withCapacity(ptr sret({ ptr }) %call_sret, i64 7)
  %__sb_510 = alloca { ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__sb_510, ptr align 8 %call_sret, i64 8, i1 false)
  %1 = call i8 @StringBuilder.putString(ptr %__sb_510, ptr @str.6)
  %call_sret2 = alloca { { i64, ptr } }, align 8
  call void @Show_string_show(ptr sret({ { i64, ptr } }) %call_sret2, ptr %msg)
  %2 = call i8 @StringBuilder.putString(ptr %__sb_510, ptr %call_sret2)
  %3 = call i8 @StringBuilder.putString(ptr %__sb_510, ptr @str.66)
  %call_sret3 = alloca { { i64, ptr } }, align 8
  call void @Show_string_show(ptr sret({ { i64, ptr } }) %call_sret3, ptr %filename)
  %4 = call i8 @StringBuilder.putString(ptr %__sb_510, ptr %call_sret3)
  %5 = call i8 @StringBuilder.putString(ptr %__sb_510, ptr @str.68)
  %line_loaded = load i64, ptr %line, align 8
  %call_sret4 = alloca { { i64, ptr } }, align 8
  call void @Show_u64_show(ptr sret({ { i64, ptr } }) %call_sret4, i64 %line_loaded)
  %6 = call i8 @StringBuilder.putString(ptr %__sb_510, ptr %call_sret4)
  %7 = call i8 @StringBuilder.putString(ptr %__sb_510, ptr @str.70)
  %call_sret5 = alloca { { i64, ptr } }, align 8
  call void @StringBuilder.build(ptr sret({ { i64, ptr } }) %call_sret5, ptr %__sb_510)
  %s = alloca { { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %s, ptr align 8 %call_sret5, i64 16, i1 false)
  %8 = call i8 @eprint(ptr %s)
  call void @exit(i32 1)
  unreachable
}

define i8 @sys.printBacktrace() {
entry:
  %0 = call i8 @_k1_show_backtrace()
  ret i8 %0
}

declare i8 @_k1_show_backtrace()

define void @StringBuilder.withCapacity(ptr sret({ ptr }) %sret_ptr, i64 %cap_arg) {
entry:
  %cap = alloca i64, align 8
  store i64 %cap_arg, ptr %cap, align 8
  %cap_loaded = load i64, ptr %cap, align 8
  %call_sret = alloca { i64, { i64, ptr } }, align 8
  call void @"List.withCapacity_spec_enum Char(char) | String(string)_5"(ptr sret({ i64, { i64, ptr } }) %call_sret, i64 %cap_loaded)
  %0 = call ptr @"new_spec_List[enum Char(char) | String(string)]_1"(ptr %call_sret)
  %parts = alloca ptr, align 8
  store ptr %0, ptr %parts, align 8
  %struct_literal = alloca { ptr }, align 8
  %parts_loaded = load ptr, ptr %parts, align 8
  %parts_store_addr = getelementptr inbounds { ptr }, ptr %struct_literal, i32 0, i32 0
  store ptr %parts_loaded, ptr %parts_store_addr, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %struct_literal, i64 8, i1 false)
  ret void
}

define void @"List.withCapacity_spec_enum Char(char) | String(string)_5"(ptr sret({ i64, { i64, ptr } }) %sret_ptr, i64 %cap_arg) {
entry:
  %cap = alloca i64, align 8
  store i64 %cap_arg, ptr %cap, align 8
  %cap_loaded = load i64, ptr %cap, align 8
  %call_sret = alloca { i64, ptr }, align 8
  call void @"Buffer._allocate_spec_enum Char(char) | String(string)_8"(ptr sret({ i64, ptr }) %call_sret, i64 %cap_loaded)
  %buffer = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %buffer, ptr align 8 %call_sret, i64 16, i1 false)
  %struct_literal = alloca { i64, { i64, ptr } }, align 8
  %len_store_addr = getelementptr inbounds { i64, { i64, ptr } }, ptr %struct_literal, i32 0, i32 0
  store i64 0, ptr %len_store_addr, align 8
  %buffer_store_addr = getelementptr inbounds { i64, { i64, ptr } }, ptr %struct_literal, i32 0, i32 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %buffer_store_addr, ptr align 8 %buffer, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %struct_literal, i64 24, i1 false)
  ret void
}

define void @"Buffer._allocate_spec_enum Char(char) | String(string)_8"(ptr sret({ i64, ptr }) %sret_ptr, i64 %count_arg) {
entry:
  %count = alloca i64, align 8
  store i64 %count_arg, ptr %count, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %count_loaded = load i64, ptr %count, align 8
  %"==_i1" = icmp eq i64 %count_loaded, 0
  %"==_res" = sext i1 %"==_i1" to i8
  %arm_pattern_i1 = trunc i8 %"==_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %0 = call ptr @Pointer.null()
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  %count_loaded1 = load i64, ptr %count, align 8
  %1 = call i64 @"Buffer.bufferLenBytes_spec_enum Char(char) | String(string)_8"(i64 %count_loaded1)
  %sizeBytes = alloca i64, align 8
  store i64 %1, ptr %sizeBytes, align 8
  %sizeBytes_loaded = load i64, ptr %sizeBytes, align 8
  %2 = call ptr @malloc(i64 %sizeBytes_loaded)
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1, %arm_cons_0
  %match_result = phi ptr [ %0, %arm_cons_0 ], [ %2, %arm_cons_1 ]
  %ptr = alloca ptr, align 8
  store ptr %match_result, ptr %ptr, align 8
  %ptr_loaded = load ptr, ptr %ptr, align 8
  %data = alloca ptr, align 8
  store ptr %ptr_loaded, ptr %data, align 8
  %struct_literal = alloca { i64, ptr }, align 8
  %count_loaded2 = load i64, ptr %count, align 8
  %len_store_addr = getelementptr inbounds { i64, ptr }, ptr %struct_literal, i32 0, i32 0
  store i64 %count_loaded2, ptr %len_store_addr, align 8
  %data_loaded = load ptr, ptr %data, align 8
  %data_store_addr = getelementptr inbounds { i64, ptr }, ptr %struct_literal, i32 0, i32 1
  store ptr %data_loaded, ptr %data_store_addr, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %struct_literal, i64 16, i1 false)
  ret void
}

define ptr @Pointer.null() {
entry:
  ret ptr null
}

define i64 @"Buffer.bufferLenBytes_spec_enum Char(char) | String(string)_8"(i64 %count_arg) {
entry:
  %count = alloca i64, align 8
  store i64 %count_arg, ptr %count, align 8
  %count_loaded = load i64, ptr %count, align 8
  %mul = mul i64 %count_loaded, 24
  ret i64 %mul
}

declare ptr @malloc(i64)

define ptr @"new_spec_List[enum Char(char) | String(string)]_1"(ptr %value_arg) {
entry:
  %value = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %value, ptr align 8 %value_arg, i64 24, i1 false)
  %0 = call ptr @malloc(i64 24)
  %ptr = alloca ptr, align 8
  store ptr %0, ptr %ptr, align 8
  %ptr_loaded = load ptr, ptr %ptr, align 8
  %t = alloca ptr, align 8
  store ptr %ptr_loaded, ptr %t, align 8
  %t_loaded = load ptr, ptr %t, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %t_loaded, ptr align 8 %value, i64 24, i1 false)
  %t_loaded1 = load ptr, ptr %t, align 8
  ret ptr %t_loaded1
}

define i8 @StringBuilder.putString(ptr %self_arg, ptr %s_arg) {
entry:
  %self = alloca { ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 8, i1 false)
  %s = alloca { { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %s, ptr align 8 %s_arg, i64 16, i1 false)
  %struc.parts = getelementptr inbounds { ptr }, ptr %self, i32 0, i32 0
  %struc.parts1 = load ptr, ptr %struc.parts, align 8
  %enum_constr = alloca %"51", align 8
  %enum_tag_String = getelementptr inbounds %"51.String", ptr %enum_constr, i32 0, i32 0
  store i64 1, ptr %enum_tag_String, align 8
  %enum_payload_String = getelementptr inbounds %"51.String", ptr %enum_constr, i32 0, i32 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %enum_payload_String, ptr align 8 %s, i64 16, i1 false)
  %0 = call i8 @"List.push_spec_enum Char(char) | String(string)_6"(ptr %struc.parts1, ptr %enum_constr)
  ret i8 %0
}

define i8 @"List.push_spec_enum Char(char) | String(string)_6"(ptr %self_arg, ptr %elem_arg) {
entry:
  %self = alloca ptr, align 8
  store ptr %self_arg, ptr %self, align 8
  %elem = alloca %"51", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %elem, ptr align 8 %elem_arg, i64 24, i1 false)
  %self_loaded = load ptr, ptr %self, align 8
  %struc.len = getelementptr inbounds { i64, { i64, ptr } }, ptr %self_loaded, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %startLength = alloca i64, align 8
  store i64 %struc.len1, ptr %startLength, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %startLength_loaded = load i64, ptr %startLength, align 8
  %self_loaded2 = load ptr, ptr %self, align 8
  %0 = call i64 @"List.cap_spec_enum Char(char) | String(string)_7"(ptr %self_loaded2)
  %"==_i1" = icmp eq i64 %startLength_loaded, %0
  %"==_res" = sext i1 %"==_i1" to i8
  %arm_pattern_i1 = trunc i8 %"==_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %self_loaded3 = load ptr, ptr %self, align 8
  %1 = call i8 @"List._grow_spec_enum Char(char) | String(string)_5"(ptr %self_loaded3)
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1, %arm_cons_0
  %match_result = phi i8 [ %1, %arm_cons_0 ], [ 0, %arm_cons_1 ]
  %startLength_loaded4 = load i64, ptr %startLength, align 8
  %add = add i64 %startLength_loaded4, 1
  %self_loaded5 = load ptr, ptr %self, align 8
  %struc.len6 = getelementptr inbounds { i64, { i64, ptr } }, ptr %self_loaded5, i32 0, i32 0
  store i64 %add, ptr %struc.len6, align 8
  %self_loaded7 = load ptr, ptr %self, align 8
  %startLength_loaded8 = load i64, ptr %startLength, align 8
  %2 = call i8 @"List.set_spec_enum Char(char) | String(string)_6"(ptr %self_loaded7, i64 %startLength_loaded8, ptr %elem)
  ret i8 0
}

define i64 @"List.cap_spec_enum Char(char) | String(string)_7"(ptr %self_arg) {
entry:
  %self = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 24, i1 false)
  %struc.buffer = getelementptr inbounds { i64, { i64, ptr } }, ptr %self, i32 0, i32 1
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %struc.buffer, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  ret i64 %struc.len1
}

define i8 @"List._grow_spec_enum Char(char) | String(string)_5"(ptr %self_arg) {
entry:
  %self = alloca ptr, align 8
  store ptr %self_arg, ptr %self, align 8
  %elemSize = alloca i64, align 8
  store i64 24, ptr %elemSize, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %self_loaded = load ptr, ptr %self, align 8
  %struc.buffer = getelementptr inbounds { i64, { i64, ptr } }, ptr %self_loaded, i32 0, i32 1
  %0 = call i8 @"Buffer.isEmpty_spec_enum Char(char) | String(string)_7"(ptr %struc.buffer)
  %arm_pattern_i1 = trunc i8 %0 to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  br label %arm_01

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  %self_loaded9 = load ptr, ptr %self, align 8
  %1 = call i64 @"List.cap_spec_enum Char(char) | String(string)_7"(ptr %self_loaded9)
  %mul = mul i64 %1, 2
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1, %match_end6
  %match_result = phi i64 [ %match_result7, %match_end6 ], [ %mul, %arm_cons_1 ]
  %newCap = alloca i64, align 8
  store i64 %match_result, ptr %newCap, align 8
  %self_loaded10 = load ptr, ptr %self, align 8
  %newCap_loaded = load i64, ptr %newCap, align 8
  %2 = call i8 @"List.reserve_spec_enum Char(char) | String(string)_5"(ptr %self_loaded10, i64 %newCap_loaded)
  ret i8 0

arm_01:                                           ; preds = %arm_cons_0
  %elemSize_loaded = load i64, ptr %elemSize, align 8
  %">=_i1" = icmp sge i64 %elemSize_loaded, 1024
  %">=_res" = sext i1 %">=_i1" to i8
  %arm_pattern_i18 = trunc i8 %">=_res" to i1
  br i1 %arm_pattern_i18, label %arm_cons_02, label %arm_13

arm_cons_02:                                      ; preds = %arm_01
  br label %match_end6

arm_13:                                           ; preds = %arm_01
  br i1 true, label %arm_cons_14, label %match_fail5

arm_cons_14:                                      ; preds = %arm_13
  br label %match_end6

match_fail5:                                      ; preds = %arm_13
  unreachable

match_end6:                                       ; preds = %arm_cons_14, %arm_cons_02
  %match_result7 = phi i64 [ 1, %arm_cons_02 ], [ 8, %arm_cons_14 ]
  br label %match_end
}

define i8 @"Buffer.isEmpty_spec_enum Char(char) | String(string)_7"(ptr %self_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %"==_i1" = icmp eq i64 %struc.len1, 0
  %"==_res" = sext i1 %"==_i1" to i8
  ret i8 %"==_res"
}

define i8 @"List.reserve_spec_enum Char(char) | String(string)_5"(ptr %self_arg, i64 %count_arg) {
entry:
  %self = alloca ptr, align 8
  store ptr %self_arg, ptr %self, align 8
  %count = alloca i64, align 8
  store i64 %count_arg, ptr %count, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %self_loaded = load ptr, ptr %self, align 8
  %0 = call i64 @"List.cap_spec_enum Char(char) | String(string)_7"(ptr %self_loaded)
  %count_loaded = load i64, ptr %count, align 8
  %">=_i1" = icmp sge i64 %0, %count_loaded
  %">=_res" = sext i1 %">=_i1" to i8
  %arm_pattern_i1 = trunc i8 %">=_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  ret i8 0

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1
  %match_result = phi i8 [ 0, %arm_cons_1 ]
  %self_loaded1 = load ptr, ptr %self, align 8
  %struc.buffer = getelementptr inbounds { i64, { i64, ptr } }, ptr %self_loaded1, i32 0, i32 1
  %count_loaded2 = load i64, ptr %count, align 8
  %call_sret = alloca { i64, ptr }, align 8
  call void @"Buffer._enlargedClone_spec_enum Char(char) | String(string)_6"(ptr sret({ i64, ptr }) %call_sret, ptr %struc.buffer, i64 %count_loaded2)
  %newBuffer = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %newBuffer, ptr align 8 %call_sret, i64 16, i1 false)
  %self_loaded3 = load ptr, ptr %self, align 8
  %struc.buffer4 = getelementptr inbounds { i64, { i64, ptr } }, ptr %self_loaded3, i32 0, i32 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %struc.buffer4, ptr align 8 %newBuffer, i64 16, i1 false)
  ret i8 0
}

define void @"Buffer._enlargedClone_spec_enum Char(char) | String(string)_6"(ptr sret({ i64, ptr }) %sret_ptr, ptr %self_arg, i64 %newCount_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %newCount = alloca i64, align 8
  store i64 %newCount_arg, ptr %newCount, align 8
  %newCount_loaded = load i64, ptr %newCount, align 8
  %call_sret = alloca { i64, ptr }, align 8
  call void @"Buffer._allocate_spec_enum Char(char) | String(string)_8"(ptr sret({ i64, ptr }) %call_sret, i64 %newCount_loaded)
  %newBuffer = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %newBuffer, ptr align 8 %call_sret, i64 16, i1 false)
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %0 = call i8 @"Buffer._copyFrom_spec_enum Char(char) | String(string)_6"(ptr %self, ptr %newBuffer, i64 %struc.len1)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %newBuffer, i64 16, i1 false)
  ret void
}

define i8 @"Buffer._copyFrom_spec_enum Char(char) | String(string)_6"(ptr %source_arg, ptr %dest_arg, i64 %count_arg) {
entry:
  %source = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %source, ptr align 8 %source_arg, i64 16, i1 false)
  %dest = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %dest, ptr align 8 %dest_arg, i64 16, i1 false)
  %count = alloca i64, align 8
  store i64 %count_arg, ptr %count, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %count_loaded = load i64, ptr %count, align 8
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %source, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %">_i1" = icmp sgt i64 %count_loaded, %struc.len1
  %">_res" = sext i1 %">_i1" to i8
  %arm_pattern_i1 = trunc i8 %">_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.8, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 36, ptr %line_store_addr, align 8
  %call_sret = alloca { ptr }, align 8
  call void @StringBuilder.withCapacity(ptr sret({ ptr }) %call_sret, i64 2)
  %__sb_1754 = alloca { ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__sb_1754, ptr align 8 %call_sret, i64 8, i1 false)
  %0 = call i8 @StringBuilder.putString(ptr %__sb_1754, ptr @str.10)
  %count_loaded2 = load i64, ptr %count, align 8
  %call_sret3 = alloca { { i64, ptr } }, align 8
  call void @Show_u64_show(ptr sret({ { i64, ptr } }) %call_sret3, i64 %count_loaded2)
  %1 = call i8 @StringBuilder.putString(ptr %__sb_1754, ptr %call_sret3)
  %call_sret4 = alloca { { i64, ptr } }, align 8
  call void @StringBuilder.build(ptr sret({ { i64, ptr } }) %call_sret4, ptr %__sb_1754)
  call void @crash(ptr %struct_literal, ptr %call_sret4)
  unreachable

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1
  %match_result = phi i8 [ 0, %arm_cons_1 ]
  br label %arm_05

arm_05:                                           ; preds = %match_end
  %count_loaded12 = load i64, ptr %count, align 8
  %struc.len13 = getelementptr inbounds { i64, ptr }, ptr %dest, i32 0, i32 0
  %struc.len14 = load i64, ptr %struc.len13, align 8
  %">_i115" = icmp sgt i64 %count_loaded12, %struc.len14
  %">_res16" = sext i1 %">_i115" to i8
  %arm_pattern_i117 = trunc i8 %">_res16" to i1
  br i1 %arm_pattern_i117, label %arm_cons_06, label %arm_17

arm_cons_06:                                      ; preds = %arm_05
  %struct_literal18 = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr19 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal18, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr19, ptr align 8 @str.62, i64 16, i1 false)
  %line_store_addr20 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal18, i32 0, i32 1
  store i64 39, ptr %line_store_addr20, align 8
  %call_sret21 = alloca { ptr }, align 8
  call void @StringBuilder.withCapacity(ptr sret({ ptr }) %call_sret21, i64 2)
  %__sb_1755 = alloca { ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__sb_1755, ptr align 8 %call_sret21, i64 8, i1 false)
  %2 = call i8 @StringBuilder.putString(ptr %__sb_1755, ptr @str.64)
  %count_loaded22 = load i64, ptr %count, align 8
  %call_sret23 = alloca { { i64, ptr } }, align 8
  call void @Show_u64_show(ptr sret({ { i64, ptr } }) %call_sret23, i64 %count_loaded22)
  %3 = call i8 @StringBuilder.putString(ptr %__sb_1755, ptr %call_sret23)
  %call_sret24 = alloca { { i64, ptr } }, align 8
  call void @StringBuilder.build(ptr sret({ { i64, ptr } }) %call_sret24, ptr %__sb_1755)
  call void @crash(ptr %struct_literal18, ptr %call_sret24)
  unreachable

arm_17:                                           ; preds = %arm_05
  br i1 true, label %arm_cons_18, label %match_fail9

arm_cons_18:                                      ; preds = %arm_17
  br label %match_end10

match_fail9:                                      ; preds = %arm_17
  unreachable

match_end10:                                      ; preds = %arm_cons_18
  %match_result11 = phi i8 [ 0, %arm_cons_18 ]
  %count_loaded25 = load i64, ptr %count, align 8
  %4 = call i64 @"Buffer.bufferLenBytes_spec_enum Char(char) | String(string)_8"(i64 %count_loaded25)
  %numBytes = alloca i64, align 8
  store i64 %4, ptr %numBytes, align 8
  %5 = call ptr @"Buffer.dataPointer_spec_enum Char(char) | String(string)_11"(ptr %dest)
  %6 = call ptr @"Buffer.dataPointer_spec_enum Char(char) | String(string)_11"(ptr %source)
  %numBytes_loaded = load i64, ptr %numBytes, align 8
  %7 = call ptr @memcpy(ptr %5, ptr %6, i64 %numBytes_loaded)
  ret i8 0
}

define void @Show_u64_show(ptr sret({ { i64, ptr } }) %sret_ptr, i64 %self_arg) {
entry:
  %self = alloca i64, align 8
  store i64 %self_arg, ptr %self, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %self_loaded = load i64, ptr %self, align 8
  %"==_i1" = icmp eq i64 %self_loaded, 0
  %"==_res" = sext i1 %"==_i1" to i8
  %arm_pattern_i1 = trunc i8 %"==_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 @str.12, i64 16, i1 false)
  ret void

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1
  %match_result = phi i8 [ 0, %arm_cons_1 ]
  %self_loaded1 = load i64, ptr %self, align 8
  %remaining = alloca i64, align 8
  store i64 %self_loaded1, ptr %remaining, align 8
  %call_sret = alloca { i64, { i64, ptr } }, align 8
  call void @List.withCapacity_spec_char_2(ptr sret({ i64, { i64, ptr } }) %call_sret, i64 8)
  %buf = alloca ptr, align 8
  store ptr %call_sret, ptr %buf, align 8
  br label %while_cond

while_cond:                                       ; preds = %while_body, %match_end
  %remaining_loaded = load i64, ptr %remaining, align 8
  %">_i1" = icmp sgt i64 %remaining_loaded, 0
  %">_res" = sext i1 %">_i1" to i8
  %while_cond2 = trunc i8 %">_res" to i1
  br i1 %while_cond2, label %while_body, label %while_end

while_body:                                       ; preds = %while_cond
  %remaining_loaded3 = load i64, ptr %remaining, align 8
  %urem = urem i64 %remaining_loaded3, 10
  %d = alloca i64, align 8
  store i64 %urem, ptr %d, align 8
  %d_loaded = load i64, ptr %d, align 8
  %add = add i64 %d_loaded, 48
  %trunc_cast = trunc i64 %add to i8
  %c = alloca i8, align 1
  store i8 %trunc_cast, ptr %c, align 1
  %buf_loaded = load ptr, ptr %buf, align 8
  %c_loaded = load i8, ptr %c, align 1
  %0 = call i8 @List.push_spec_char_2(ptr %buf_loaded, i8 %c_loaded)
  %remaining_loaded4 = load i64, ptr %remaining, align 8
  %udiv = udiv i64 %remaining_loaded4, 10
  store i64 %udiv, ptr %remaining, align 8
  br label %while_cond

while_end:                                        ; preds = %while_cond
  %buf_loaded5 = load ptr, ptr %buf, align 8
  %1 = call i8 @List.reverse_spec_char_1(ptr %buf_loaded5)
  %buf_loaded6 = load ptr, ptr %buf, align 8
  %call_sret7 = alloca { { i64, ptr } }, align 8
  call void @string.wrapList(ptr sret({ { i64, ptr } }) %call_sret7, ptr %buf_loaded6)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret7, i64 16, i1 false)
  ret void
}

define void @List.withCapacity_spec_char_2(ptr sret({ i64, { i64, ptr } }) %sret_ptr, i64 %cap_arg) {
entry:
  %cap = alloca i64, align 8
  store i64 %cap_arg, ptr %cap, align 8
  %cap_loaded = load i64, ptr %cap, align 8
  %call_sret = alloca { i64, ptr }, align 8
  call void @Buffer._allocate_spec_char_7(ptr sret({ i64, ptr }) %call_sret, i64 %cap_loaded)
  %buffer = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %buffer, ptr align 8 %call_sret, i64 16, i1 false)
  %struct_literal = alloca { i64, { i64, ptr } }, align 8
  %len_store_addr = getelementptr inbounds { i64, { i64, ptr } }, ptr %struct_literal, i32 0, i32 0
  store i64 0, ptr %len_store_addr, align 8
  %buffer_store_addr = getelementptr inbounds { i64, { i64, ptr } }, ptr %struct_literal, i32 0, i32 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %buffer_store_addr, ptr align 8 %buffer, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %struct_literal, i64 24, i1 false)
  ret void
}

define void @Buffer._allocate_spec_char_7(ptr sret({ i64, ptr }) %sret_ptr, i64 %count_arg) {
entry:
  %count = alloca i64, align 8
  store i64 %count_arg, ptr %count, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %count_loaded = load i64, ptr %count, align 8
  %"==_i1" = icmp eq i64 %count_loaded, 0
  %"==_res" = sext i1 %"==_i1" to i8
  %arm_pattern_i1 = trunc i8 %"==_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %0 = call ptr @Pointer.null()
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  %count_loaded1 = load i64, ptr %count, align 8
  %1 = call i64 @Buffer.bufferLenBytes_spec_char_5(i64 %count_loaded1)
  %sizeBytes = alloca i64, align 8
  store i64 %1, ptr %sizeBytes, align 8
  %sizeBytes_loaded = load i64, ptr %sizeBytes, align 8
  %2 = call ptr @malloc(i64 %sizeBytes_loaded)
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1, %arm_cons_0
  %match_result = phi ptr [ %0, %arm_cons_0 ], [ %2, %arm_cons_1 ]
  %ptr = alloca ptr, align 8
  store ptr %match_result, ptr %ptr, align 8
  %ptr_loaded = load ptr, ptr %ptr, align 8
  %data = alloca ptr, align 8
  store ptr %ptr_loaded, ptr %data, align 8
  %struct_literal = alloca { i64, ptr }, align 8
  %count_loaded2 = load i64, ptr %count, align 8
  %len_store_addr = getelementptr inbounds { i64, ptr }, ptr %struct_literal, i32 0, i32 0
  store i64 %count_loaded2, ptr %len_store_addr, align 8
  %data_loaded = load ptr, ptr %data, align 8
  %data_store_addr = getelementptr inbounds { i64, ptr }, ptr %struct_literal, i32 0, i32 1
  store ptr %data_loaded, ptr %data_store_addr, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %struct_literal, i64 16, i1 false)
  ret void
}

define i64 @Buffer.bufferLenBytes_spec_char_5(i64 %count_arg) {
entry:
  %count = alloca i64, align 8
  store i64 %count_arg, ptr %count, align 8
  %count_loaded = load i64, ptr %count, align 8
  %mul = mul i64 %count_loaded, 1
  ret i64 %mul
}

define i8 @List.push_spec_char_2(ptr %self_arg, i8 %elem_arg) {
entry:
  %self = alloca ptr, align 8
  store ptr %self_arg, ptr %self, align 8
  %elem = alloca i8, align 1
  store i8 %elem_arg, ptr %elem, align 1
  %self_loaded = load ptr, ptr %self, align 8
  %struc.len = getelementptr inbounds { i64, { i64, ptr } }, ptr %self_loaded, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %startLength = alloca i64, align 8
  store i64 %struc.len1, ptr %startLength, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %startLength_loaded = load i64, ptr %startLength, align 8
  %self_loaded2 = load ptr, ptr %self, align 8
  %0 = call i64 @List.cap_spec_char_5(ptr %self_loaded2)
  %"==_i1" = icmp eq i64 %startLength_loaded, %0
  %"==_res" = sext i1 %"==_i1" to i8
  %arm_pattern_i1 = trunc i8 %"==_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %self_loaded3 = load ptr, ptr %self, align 8
  %1 = call i8 @List._grow_spec_char_3(ptr %self_loaded3)
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1, %arm_cons_0
  %match_result = phi i8 [ %1, %arm_cons_0 ], [ 0, %arm_cons_1 ]
  %startLength_loaded4 = load i64, ptr %startLength, align 8
  %add = add i64 %startLength_loaded4, 1
  %self_loaded5 = load ptr, ptr %self, align 8
  %struc.len6 = getelementptr inbounds { i64, { i64, ptr } }, ptr %self_loaded5, i32 0, i32 0
  store i64 %add, ptr %struc.len6, align 8
  %self_loaded7 = load ptr, ptr %self, align 8
  %startLength_loaded8 = load i64, ptr %startLength, align 8
  %elem_loaded = load i8, ptr %elem, align 1
  %2 = call i8 @List.set_spec_char_4(ptr %self_loaded7, i64 %startLength_loaded8, i8 %elem_loaded)
  ret i8 0
}

define i64 @List.cap_spec_char_5(ptr %self_arg) {
entry:
  %self = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 24, i1 false)
  %struc.buffer = getelementptr inbounds { i64, { i64, ptr } }, ptr %self, i32 0, i32 1
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %struc.buffer, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  ret i64 %struc.len1
}

define i8 @List._grow_spec_char_3(ptr %self_arg) {
entry:
  %self = alloca ptr, align 8
  store ptr %self_arg, ptr %self, align 8
  %elemSize = alloca i64, align 8
  store i64 1, ptr %elemSize, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %self_loaded = load ptr, ptr %self, align 8
  %struc.buffer = getelementptr inbounds { i64, { i64, ptr } }, ptr %self_loaded, i32 0, i32 1
  %0 = call i8 @Buffer.isEmpty_spec_char_5(ptr %struc.buffer)
  %arm_pattern_i1 = trunc i8 %0 to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  br label %arm_01

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  %self_loaded9 = load ptr, ptr %self, align 8
  %1 = call i64 @List.cap_spec_char_5(ptr %self_loaded9)
  %mul = mul i64 %1, 2
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1, %match_end6
  %match_result = phi i64 [ %match_result7, %match_end6 ], [ %mul, %arm_cons_1 ]
  %newCap = alloca i64, align 8
  store i64 %match_result, ptr %newCap, align 8
  %self_loaded10 = load ptr, ptr %self, align 8
  %newCap_loaded = load i64, ptr %newCap, align 8
  %2 = call i8 @List.reserve_spec_char_3(ptr %self_loaded10, i64 %newCap_loaded)
  ret i8 0

arm_01:                                           ; preds = %arm_cons_0
  %elemSize_loaded = load i64, ptr %elemSize, align 8
  %">=_i1" = icmp sge i64 %elemSize_loaded, 1024
  %">=_res" = sext i1 %">=_i1" to i8
  %arm_pattern_i18 = trunc i8 %">=_res" to i1
  br i1 %arm_pattern_i18, label %arm_cons_02, label %arm_13

arm_cons_02:                                      ; preds = %arm_01
  br label %match_end6

arm_13:                                           ; preds = %arm_01
  br i1 true, label %arm_cons_14, label %match_fail5

arm_cons_14:                                      ; preds = %arm_13
  br label %match_end6

match_fail5:                                      ; preds = %arm_13
  unreachable

match_end6:                                       ; preds = %arm_cons_14, %arm_cons_02
  %match_result7 = phi i64 [ 1, %arm_cons_02 ], [ 8, %arm_cons_14 ]
  br label %match_end
}

define i8 @Buffer.isEmpty_spec_char_5(ptr %self_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %"==_i1" = icmp eq i64 %struc.len1, 0
  %"==_res" = sext i1 %"==_i1" to i8
  ret i8 %"==_res"
}

define i8 @List.reserve_spec_char_3(ptr %self_arg, i64 %count_arg) {
entry:
  %self = alloca ptr, align 8
  store ptr %self_arg, ptr %self, align 8
  %count = alloca i64, align 8
  store i64 %count_arg, ptr %count, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %self_loaded = load ptr, ptr %self, align 8
  %0 = call i64 @List.cap_spec_char_5(ptr %self_loaded)
  %count_loaded = load i64, ptr %count, align 8
  %">=_i1" = icmp sge i64 %0, %count_loaded
  %">=_res" = sext i1 %">=_i1" to i8
  %arm_pattern_i1 = trunc i8 %">=_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  ret i8 0

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1
  %match_result = phi i8 [ 0, %arm_cons_1 ]
  %self_loaded1 = load ptr, ptr %self, align 8
  %struc.buffer = getelementptr inbounds { i64, { i64, ptr } }, ptr %self_loaded1, i32 0, i32 1
  %count_loaded2 = load i64, ptr %count, align 8
  %call_sret = alloca { i64, ptr }, align 8
  call void @Buffer._enlargedClone_spec_char_3(ptr sret({ i64, ptr }) %call_sret, ptr %struc.buffer, i64 %count_loaded2)
  %newBuffer = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %newBuffer, ptr align 8 %call_sret, i64 16, i1 false)
  %self_loaded3 = load ptr, ptr %self, align 8
  %struc.buffer4 = getelementptr inbounds { i64, { i64, ptr } }, ptr %self_loaded3, i32 0, i32 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %struc.buffer4, ptr align 8 %newBuffer, i64 16, i1 false)
  ret i8 0
}

define void @Buffer._enlargedClone_spec_char_3(ptr sret({ i64, ptr }) %sret_ptr, ptr %self_arg, i64 %newCount_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %newCount = alloca i64, align 8
  store i64 %newCount_arg, ptr %newCount, align 8
  %newCount_loaded = load i64, ptr %newCount, align 8
  %call_sret = alloca { i64, ptr }, align 8
  call void @Buffer._allocate_spec_char_7(ptr sret({ i64, ptr }) %call_sret, i64 %newCount_loaded)
  %newBuffer = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %newBuffer, ptr align 8 %call_sret, i64 16, i1 false)
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %0 = call i8 @Buffer._copyFrom_spec_char_3(ptr %self, ptr %newBuffer, i64 %struc.len1)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %newBuffer, i64 16, i1 false)
  ret void
}

define i8 @Buffer._copyFrom_spec_char_3(ptr %source_arg, ptr %dest_arg, i64 %count_arg) {
entry:
  %source = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %source, ptr align 8 %source_arg, i64 16, i1 false)
  %dest = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %dest, ptr align 8 %dest_arg, i64 16, i1 false)
  %count = alloca i64, align 8
  store i64 %count_arg, ptr %count, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %count_loaded = load i64, ptr %count, align 8
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %source, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %">_i1" = icmp sgt i64 %count_loaded, %struc.len1
  %">_res" = sext i1 %">_i1" to i8
  %arm_pattern_i1 = trunc i8 %">_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.14, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 36, ptr %line_store_addr, align 8
  %call_sret = alloca { ptr }, align 8
  call void @StringBuilder.withCapacity(ptr sret({ ptr }) %call_sret, i64 2)
  %__sb_1539 = alloca { ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__sb_1539, ptr align 8 %call_sret, i64 8, i1 false)
  %0 = call i8 @StringBuilder.putString(ptr %__sb_1539, ptr @str.16)
  %count_loaded2 = load i64, ptr %count, align 8
  %call_sret3 = alloca { { i64, ptr } }, align 8
  call void @Show_u64_show(ptr sret({ { i64, ptr } }) %call_sret3, i64 %count_loaded2)
  %1 = call i8 @StringBuilder.putString(ptr %__sb_1539, ptr %call_sret3)
  %call_sret4 = alloca { { i64, ptr } }, align 8
  call void @StringBuilder.build(ptr sret({ { i64, ptr } }) %call_sret4, ptr %__sb_1539)
  call void @crash(ptr %struct_literal, ptr %call_sret4)
  unreachable

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1
  %match_result = phi i8 [ 0, %arm_cons_1 ]
  br label %arm_05

arm_05:                                           ; preds = %match_end
  %count_loaded12 = load i64, ptr %count, align 8
  %struc.len13 = getelementptr inbounds { i64, ptr }, ptr %dest, i32 0, i32 0
  %struc.len14 = load i64, ptr %struc.len13, align 8
  %">_i115" = icmp sgt i64 %count_loaded12, %struc.len14
  %">_res16" = sext i1 %">_i115" to i8
  %arm_pattern_i117 = trunc i8 %">_res16" to i1
  br i1 %arm_pattern_i117, label %arm_cons_06, label %arm_17

arm_cons_06:                                      ; preds = %arm_05
  %struct_literal18 = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr19 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal18, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr19, ptr align 8 @str.58, i64 16, i1 false)
  %line_store_addr20 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal18, i32 0, i32 1
  store i64 39, ptr %line_store_addr20, align 8
  %call_sret21 = alloca { ptr }, align 8
  call void @StringBuilder.withCapacity(ptr sret({ ptr }) %call_sret21, i64 2)
  %__sb_1540 = alloca { ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__sb_1540, ptr align 8 %call_sret21, i64 8, i1 false)
  %2 = call i8 @StringBuilder.putString(ptr %__sb_1540, ptr @str.60)
  %count_loaded22 = load i64, ptr %count, align 8
  %call_sret23 = alloca { { i64, ptr } }, align 8
  call void @Show_u64_show(ptr sret({ { i64, ptr } }) %call_sret23, i64 %count_loaded22)
  %3 = call i8 @StringBuilder.putString(ptr %__sb_1540, ptr %call_sret23)
  %call_sret24 = alloca { { i64, ptr } }, align 8
  call void @StringBuilder.build(ptr sret({ { i64, ptr } }) %call_sret24, ptr %__sb_1540)
  call void @crash(ptr %struct_literal18, ptr %call_sret24)
  unreachable

arm_17:                                           ; preds = %arm_05
  br i1 true, label %arm_cons_18, label %match_fail9

arm_cons_18:                                      ; preds = %arm_17
  br label %match_end10

match_fail9:                                      ; preds = %arm_17
  unreachable

match_end10:                                      ; preds = %arm_cons_18
  %match_result11 = phi i8 [ 0, %arm_cons_18 ]
  %count_loaded25 = load i64, ptr %count, align 8
  %4 = call i64 @Buffer.bufferLenBytes_spec_char_5(i64 %count_loaded25)
  %numBytes = alloca i64, align 8
  store i64 %4, ptr %numBytes, align 8
  %5 = call ptr @Buffer.dataPointer_spec_char_5(ptr %dest)
  %6 = call ptr @Buffer.dataPointer_spec_char_5(ptr %source)
  %numBytes_loaded = load i64, ptr %numBytes, align 8
  %7 = call ptr @memcpy(ptr %5, ptr %6, i64 %numBytes_loaded)
  ret i8 0
}

define void @StringBuilder.build(ptr sret({ { i64, ptr } }) %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca { ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 8, i1 false)
  %0 = call i64 @StringBuilder.len(ptr %self)
  %call_sret = alloca { i64, { i64, ptr } }, align 8
  call void @List.withCapacity_spec_char_2(ptr sret({ i64, { i64, ptr } }) %call_sret, i64 %0)
  %bytes = alloca ptr, align 8
  store ptr %call_sret, ptr %bytes, align 8
  %itIndex = alloca i64, align 8
  store i64 0, ptr %itIndex, align 8
  %struc.parts = getelementptr inbounds { ptr }, ptr %self, i32 0, i32 0
  %struc.parts1 = load ptr, ptr %struc.parts, align 8
  %call_sret2 = alloca { { i64, ptr }, i64 }, align 8
  call void @"Iterable_List[enum Char(char) | String(string)]_iterator"(ptr sret({ { i64, ptr }, i64 }) %call_sret2, ptr %struc.parts1)
  %__iter_1000 = alloca ptr, align 8
  store ptr %call_sret2, ptr %__iter_1000, align 8
  %break = alloca i8, align 1
  br label %loop_body

loop_body:                                        ; preds = %if_merge, %entry
  %__iter_1000_loaded = load ptr, ptr %__iter_1000, align 8
  %call_sret3 = alloca %"Opt[enum Char(char) | String(string)]", align 8
  call void @"Iterator_BufferIterator[enum Char(char) | String(string)]_next"(ptr sret(%"Opt[enum Char(char) | String(string)]") %call_sret3, ptr %__iter_1000_loaded)
  %__next_1001 = alloca %"Opt[enum Char(char) | String(string)]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__next_1001, ptr align 8 %call_sret3, i64 32, i1 false)
  %1 = call i8 @"Opt.isSome_spec_enum Char(char) | String(string)_8"(ptr %__next_1001)
  %cond_i1 = trunc i8 %1 to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

loop_end:                                         ; preds = %if_alt
  %loop_value25 = load i8, ptr %break, align 1
  %bytes_loaded26 = load ptr, ptr %bytes, align 8
  %call_sret27 = alloca { { i64, ptr } }, align 8
  call void @string.wrapList(ptr sret({ { i64, ptr } }) %call_sret27, ptr %bytes_loaded26)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret27, i64 16, i1 false)
  ret void

if_cons:                                          ; preds = %loop_body
  %call_sret4 = alloca %"51", align 8
  call void @"Unwrap_Opt[enum Char(char) | String(string)]_unwrap"(ptr sret(%"51") %call_sret4, ptr %__next_1001)
  %part = alloca %"51", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %part, ptr align 8 %call_sret4, i64 24, i1 false)
  %__match_subject_1003 = alloca %"51", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__match_subject_1003, ptr align 8 %part, i64 24, i1 false)
  br label %arm_0

if_alt:                                           ; preds = %loop_body
  store i8 0, ptr %break, align 1
  br label %loop_end

if_merge:                                         ; preds = %match_end
  %if_phi22 = phi i8 [ 0, %match_end ]
  %itIndex_loaded23 = load i64, ptr %itIndex, align 8
  %add24 = add i64 %itIndex_loaded23, 1
  store i64 %add24, ptr %itIndex, align 8
  br label %loop_body

arm_0:                                            ; preds = %if_cons
  %get_payload_ptr = getelementptr inbounds %"51.Char", ptr %__match_subject_1003, i32 0, i32 1
  %payload_by_value = load i8, ptr %get_payload_ptr, align 1
  %__payload_Char_1004 = alloca i8, align 1
  store i8 %payload_by_value, ptr %__payload_Char_1004, align 1
  %tag = load i64, ptr %__match_subject_1003, align 8
  %is_variant_cmp = icmp eq i64 %tag, 0
  %is_variant_Char = sext i1 %is_variant_cmp to i8
  %arm_pattern_i1 = trunc i8 %is_variant_Char to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %__payload_Char_1004_loaded = load i8, ptr %__payload_Char_1004, align 1
  %c = alloca i8, align 1
  store i8 %__payload_Char_1004_loaded, ptr %c, align 1
  %bytes_loaded = load ptr, ptr %bytes, align 8
  %c_loaded = load i8, ptr %c, align 1
  %2 = call i8 @List.push_spec_char_2(ptr %bytes_loaded, i8 %c_loaded)
  br label %match_end

arm_1:                                            ; preds = %arm_0
  %get_payload_ptr5 = getelementptr inbounds %"51.String", ptr %__match_subject_1003, i32 0, i32 1
  %__payload_String_1006 = alloca { { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__payload_String_1006, ptr align 8 %get_payload_ptr5, i64 16, i1 false)
  %tag6 = load i64, ptr %__match_subject_1003, align 8
  %is_variant_cmp7 = icmp eq i64 %tag6, 1
  %is_variant_String = sext i1 %is_variant_cmp7 to i8
  %arm_pattern_i18 = trunc i8 %is_variant_String to i1
  br i1 %arm_pattern_i18, label %arm_cons_1, label %arm_2

arm_cons_1:                                       ; preds = %arm_1
  %s = alloca { { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %s, ptr align 8 %__payload_String_1006, i64 16, i1 false)
  %itIndex9 = alloca i64, align 8
  store i64 0, ptr %itIndex9, align 8
  %call_sret10 = alloca { { i64, ptr }, i64 }, align 8
  call void @Iterable_string_iterator(ptr sret({ { i64, ptr }, i64 }) %call_sret10, ptr %s)
  %__iter_1009 = alloca ptr, align 8
  store ptr %call_sret10, ptr %__iter_1009, align 8
  %break13 = alloca i8, align 1
  br label %loop_body11

arm_2:                                            ; preds = %arm_1
  br i1 true, label %arm_cons_2, label %match_fail

arm_cons_2:                                       ; preds = %arm_2
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.50, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 34, ptr %line_store_addr, align 8
  call void @crash(ptr %struct_literal, ptr @str.52)
  unreachable

match_fail:                                       ; preds = %arm_2
  unreachable

match_end:                                        ; preds = %loop_end12, %arm_cons_0
  %match_result = phi i8 [ %2, %arm_cons_0 ], [ %loop_value, %loop_end12 ]
  %__block_expr_val_1013 = alloca i8, align 1
  store i8 %match_result, ptr %__block_expr_val_1013, align 1
  br label %if_merge

loop_body11:                                      ; preds = %if_merge17, %arm_cons_1
  %__iter_1009_loaded = load ptr, ptr %__iter_1009, align 8
  %call_sret14 = alloca %"Opt[char]", align 8
  call void @"Iterator_BufferIterator[char]_next"(ptr sret(%"Opt[char]") %call_sret14, ptr %__iter_1009_loaded)
  %__next_1010 = alloca %"Opt[char]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %__next_1010, ptr align 1 %call_sret14, i64 2, i1 false)
  %3 = call i8 @Opt.isSome_spec_char_6(ptr %__next_1010)
  %cond_i118 = trunc i8 %3 to i1
  br i1 %cond_i118, label %if_cons15, label %if_alt16

loop_end12:                                       ; preds = %if_alt16
  %loop_value = load i8, ptr %break13, align 1
  br label %match_end

if_cons15:                                        ; preds = %loop_body11
  %4 = call i8 @"Unwrap_Opt[char]_unwrap"(ptr %__next_1010)
  %c19 = alloca i8, align 1
  store i8 %4, ptr %c19, align 1
  %bytes_loaded20 = load ptr, ptr %bytes, align 8
  %c_loaded21 = load i8, ptr %c19, align 1
  %5 = call i8 @List.push_spec_char_2(ptr %bytes_loaded20, i8 %c_loaded21)
  %__block_expr_val_1012 = alloca i8, align 1
  store i8 %5, ptr %__block_expr_val_1012, align 1
  br label %if_merge17

if_alt16:                                         ; preds = %loop_body11
  store i8 0, ptr %break13, align 1
  br label %loop_end12

if_merge17:                                       ; preds = %if_cons15
  %if_phi = phi i8 [ 0, %if_cons15 ]
  %itIndex_loaded = load i64, ptr %itIndex9, align 8
  %add = add i64 %itIndex_loaded, 1
  store i64 %add, ptr %itIndex9, align 8
  br label %loop_body11
}

define i64 @StringBuilder.len(ptr %self_arg) {
entry:
  %self = alloca { ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 8, i1 false)
  %len = alloca i64, align 8
  store i64 0, ptr %len, align 8
  %itIndex = alloca i64, align 8
  store i64 0, ptr %itIndex, align 8
  %struc.parts = getelementptr inbounds { ptr }, ptr %self, i32 0, i32 0
  %struc.parts1 = load ptr, ptr %struc.parts, align 8
  %call_sret = alloca { { i64, ptr }, i64 }, align 8
  call void @"Iterable_List[enum Char(char) | String(string)]_iterator"(ptr sret({ { i64, ptr }, i64 }) %call_sret, ptr %struc.parts1)
  %__iter_979 = alloca ptr, align 8
  store ptr %call_sret, ptr %__iter_979, align 8
  %break = alloca i8, align 1
  br label %loop_body

loop_body:                                        ; preds = %if_merge, %entry
  %__iter_979_loaded = load ptr, ptr %__iter_979, align 8
  %call_sret2 = alloca %"Opt[enum Char(char) | String(string)]", align 8
  call void @"Iterator_BufferIterator[enum Char(char) | String(string)]_next"(ptr sret(%"Opt[enum Char(char) | String(string)]") %call_sret2, ptr %__iter_979_loaded)
  %__next_986 = alloca %"Opt[enum Char(char) | String(string)]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__next_986, ptr align 8 %call_sret2, i64 32, i1 false)
  %0 = call i8 @"Opt.isSome_spec_enum Char(char) | String(string)_8"(ptr %__next_986)
  %cond_i1 = trunc i8 %0 to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

loop_end:                                         ; preds = %if_alt
  %loop_value = load i8, ptr %break, align 1
  %len_loaded9 = load i64, ptr %len, align 8
  ret i64 %len_loaded9

if_cons:                                          ; preds = %loop_body
  %call_sret3 = alloca %"51", align 8
  call void @"Unwrap_Opt[enum Char(char) | String(string)]_unwrap"(ptr sret(%"51") %call_sret3, ptr %__next_986)
  %part = alloca %"51", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %part, ptr align 8 %call_sret3, i64 24, i1 false)
  %__match_subject_991 = alloca %"51", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__match_subject_991, ptr align 8 %part, i64 24, i1 false)
  br label %arm_0

if_alt:                                           ; preds = %loop_body
  store i8 0, ptr %break, align 1
  br label %loop_end

if_merge:                                         ; preds = %match_end
  %if_phi = phi i8 [ 0, %match_end ]
  %itIndex_loaded = load i64, ptr %itIndex, align 8
  %add8 = add i64 %itIndex_loaded, 1
  store i64 %add8, ptr %itIndex, align 8
  br label %loop_body

arm_0:                                            ; preds = %if_cons
  %get_payload_ptr = getelementptr inbounds %"51.Char", ptr %__match_subject_991, i32 0, i32 1
  %payload_by_value = load i8, ptr %get_payload_ptr, align 1
  %__payload_Char_992 = alloca i8, align 1
  store i8 %payload_by_value, ptr %__payload_Char_992, align 1
  %tag = load i64, ptr %__match_subject_991, align 8
  %is_variant_cmp = icmp eq i64 %tag, 0
  %is_variant_Char = sext i1 %is_variant_cmp to i8
  %arm_pattern_i1 = trunc i8 %is_variant_Char to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  br label %match_end

arm_1:                                            ; preds = %arm_0
  %get_payload_ptr4 = getelementptr inbounds %"51.String", ptr %__match_subject_991, i32 0, i32 1
  %__payload_String_993 = alloca { { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__payload_String_993, ptr align 8 %get_payload_ptr4, i64 16, i1 false)
  %tag5 = load i64, ptr %__match_subject_991, align 8
  %is_variant_cmp6 = icmp eq i64 %tag5, 1
  %is_variant_String = sext i1 %is_variant_cmp6 to i8
  %arm_pattern_i17 = trunc i8 %is_variant_String to i1
  br i1 %arm_pattern_i17, label %arm_cons_1, label %arm_2

arm_cons_1:                                       ; preds = %arm_1
  %s = alloca { { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %s, ptr align 8 %__payload_String_993, i64 16, i1 false)
  %1 = call i64 @string.len(ptr %s)
  br label %match_end

arm_2:                                            ; preds = %arm_1
  br i1 true, label %arm_cons_2, label %match_fail

arm_cons_2:                                       ; preds = %arm_2
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.34, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 26, ptr %line_store_addr, align 8
  call void @crash(ptr %struct_literal, ptr @str.36)
  unreachable

match_fail:                                       ; preds = %arm_2
  unreachable

match_end:                                        ; preds = %arm_cons_1, %arm_cons_0
  %match_result = phi i64 [ 1, %arm_cons_0 ], [ %1, %arm_cons_1 ]
  %partLen = alloca i64, align 8
  store i64 %match_result, ptr %partLen, align 8
  %len_loaded = load i64, ptr %len, align 8
  %partLen_loaded = load i64, ptr %partLen, align 8
  %add = add i64 %len_loaded, %partLen_loaded
  store i64 %add, ptr %len, align 8
  %__block_expr_val_996 = alloca i8, align 1
  store i8 0, ptr %__block_expr_val_996, align 1
  br label %if_merge
}

define void @"Iterable_List[enum Char(char) | String(string)]_iterator"(ptr sret({ { i64, ptr }, i64 }) %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 24, i1 false)
  %call_sret = alloca { i64, ptr }, align 8
  call void @"List.toBuffer_spec_enum Char(char) | String(string)_12"(ptr sret({ i64, ptr }) %call_sret, ptr %self)
  %call_sret1 = alloca { { i64, ptr }, i64 }, align 8
  call void @"Iterable_Buffer[enum Char(char) | String(string)]_iterator"(ptr sret({ { i64, ptr }, i64 }) %call_sret1, ptr %call_sret)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret1, i64 24, i1 false)
  ret void
}

define void @"List.toBuffer_spec_enum Char(char) | String(string)_12"(ptr sret({ i64, ptr }) %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 24, i1 false)
  %struc.buffer = getelementptr inbounds { i64, { i64, ptr } }, ptr %self, i32 0, i32 1
  %struc.len = getelementptr inbounds { i64, { i64, ptr } }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %call_sret = alloca { i64, ptr }, align 8
  call void @"Buffer.slice_spec_enum Char(char) | String(string)_11"(ptr sret({ i64, ptr }) %call_sret, ptr %struc.buffer, i64 0, i64 %struc.len1)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret, i64 16, i1 false)
  ret void
}

define void @"Buffer.slice_spec_enum Char(char) | String(string)_11"(ptr sret({ i64, ptr }) %sret_ptr, ptr %self_arg, i64 %start_arg, i64 %end_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %start = alloca i64, align 8
  store i64 %start_arg, ptr %start, align 8
  %end = alloca i64, align 8
  store i64 %end_arg, ptr %end, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %end_loaded = load i64, ptr %end, align 8
  %start_loaded = load i64, ptr %start, align 8
  %"<_i1" = icmp slt i64 %end_loaded, %start_loaded
  %"<_res" = sext i1 %"<_i1" to i8
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %"==_i1" = icmp eq i64 %struc.len1, 0
  %"==_res" = sext i1 %"==_i1" to i8
  %bool_or = or i8 %"<_res", %"==_res"
  %arm_pattern_i1 = trunc i8 %bool_or to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %call_sret = alloca { i64, ptr }, align 8
  call void @"Buffer.empty_spec_enum Char(char) | String(string)_8"(ptr sret({ i64, ptr }) %call_sret)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret, i64 16, i1 false)
  ret void

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1
  %match_result = phi i8 [ 0, %arm_cons_1 ]
  br label %arm_02

arm_02:                                           ; preds = %match_end
  %start_loaded9 = load i64, ptr %start, align 8
  %struc.len10 = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len11 = load i64, ptr %struc.len10, align 8
  %">=_i1" = icmp sge i64 %start_loaded9, %struc.len11
  %">=_res" = sext i1 %">=_i1" to i8
  %start_loaded12 = load i64, ptr %start, align 8
  %"<_i113" = icmp slt i64 %start_loaded12, 0
  %"<_res14" = sext i1 %"<_i113" to i8
  %bool_or15 = or i8 %">=_res", %"<_res14"
  %arm_pattern_i116 = trunc i8 %bool_or15 to i1
  br i1 %arm_pattern_i116, label %arm_cons_03, label %arm_14

arm_cons_03:                                      ; preds = %arm_02
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.18, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 74, ptr %line_store_addr, align 8
  %call_sret17 = alloca { ptr }, align 8
  call void @StringBuilder.withCapacity(ptr sret({ ptr }) %call_sret17, i64 2)
  %__sb_1687 = alloca { ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__sb_1687, ptr align 8 %call_sret17, i64 8, i1 false)
  %0 = call i8 @StringBuilder.putString(ptr %__sb_1687, ptr @str.20)
  %start_loaded18 = load i64, ptr %start, align 8
  %call_sret19 = alloca { { i64, ptr } }, align 8
  call void @Show_u64_show(ptr sret({ { i64, ptr } }) %call_sret19, i64 %start_loaded18)
  %1 = call i8 @StringBuilder.putString(ptr %__sb_1687, ptr %call_sret19)
  %call_sret20 = alloca { { i64, ptr } }, align 8
  call void @StringBuilder.build(ptr sret({ { i64, ptr } }) %call_sret20, ptr %__sb_1687)
  call void @crash(ptr %struct_literal, ptr %call_sret20)
  unreachable

arm_14:                                           ; preds = %arm_02
  br i1 true, label %arm_cons_15, label %match_fail6

arm_cons_15:                                      ; preds = %arm_14
  br label %match_end7

match_fail6:                                      ; preds = %arm_14
  unreachable

match_end7:                                       ; preds = %arm_cons_15
  %match_result8 = phi i8 [ 0, %arm_cons_15 ]
  br label %arm_021

arm_021:                                          ; preds = %match_end7
  %end_loaded28 = load i64, ptr %end, align 8
  %struc.len29 = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len30 = load i64, ptr %struc.len29, align 8
  %">_i1" = icmp sgt i64 %end_loaded28, %struc.len30
  %">_res" = sext i1 %">_i1" to i8
  %arm_pattern_i131 = trunc i8 %">_res" to i1
  br i1 %arm_pattern_i131, label %arm_cons_022, label %arm_123

arm_cons_022:                                     ; preds = %arm_021
  %struc.len32 = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len33 = load i64, ptr %struc.len32, align 8
  br label %match_end26

arm_123:                                          ; preds = %arm_021
  br i1 true, label %arm_cons_124, label %match_fail25

arm_cons_124:                                     ; preds = %arm_123
  %end_loaded34 = load i64, ptr %end, align 8
  br label %match_end26

match_fail25:                                     ; preds = %arm_123
  unreachable

match_end26:                                      ; preds = %arm_cons_124, %arm_cons_022
  %match_result27 = phi i64 [ %struc.len33, %arm_cons_022 ], [ %end_loaded34, %arm_cons_124 ]
  %end35 = alloca i64, align 8
  store i64 %match_result27, ptr %end35, align 8
  %end_loaded36 = load i64, ptr %end35, align 8
  %start_loaded37 = load i64, ptr %start, align 8
  %sub = sub i64 %end_loaded36, %start_loaded37
  %newLen = alloca i64, align 8
  store i64 %sub, ptr %newLen, align 8
  %start_loaded38 = load i64, ptr %start, align 8
  %2 = call ptr @"Buffer.getRef_spec_enum Char(char) | String(string)_15"(ptr %self, i64 %start_loaded38)
  %newBase = alloca ptr, align 8
  store ptr %2, ptr %newBase, align 8
  %struct_literal39 = alloca { i64, ptr }, align 8
  %newLen_loaded = load i64, ptr %newLen, align 8
  %len_store_addr = getelementptr inbounds { i64, ptr }, ptr %struct_literal39, i32 0, i32 0
  store i64 %newLen_loaded, ptr %len_store_addr, align 8
  %newBase_loaded = load ptr, ptr %newBase, align 8
  %data_store_addr = getelementptr inbounds { i64, ptr }, ptr %struct_literal39, i32 0, i32 1
  store ptr %newBase_loaded, ptr %data_store_addr, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %struct_literal39, i64 16, i1 false)
  ret void
}

define void @"Buffer.empty_spec_enum Char(char) | String(string)_8"(ptr sret({ i64, ptr }) %sret_ptr) {
entry:
  %call_sret = alloca { i64, ptr }, align 8
  call void @"Buffer._allocate_spec_enum Char(char) | String(string)_8"(ptr sret({ i64, ptr }) %call_sret, i64 0)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret, i64 16, i1 false)
  ret void
}

define ptr @"Buffer.getRef_spec_enum Char(char) | String(string)_15"(ptr %self_arg, i64 %index_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %index = alloca i64, align 8
  store i64 %index_arg, ptr %index, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %index_loaded = load i64, ptr %index, align 8
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %">=_i1" = icmp sge i64 %index_loaded, %struc.len1
  %">=_res" = sext i1 %">=_i1" to i8
  %arm_pattern_i1 = trunc i8 %">=_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.22, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 87, ptr %line_store_addr, align 8
  call void @crash(ptr %struct_literal, ptr @str.24)
  unreachable

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1
  %match_result = phi i8 [ 0, %arm_cons_1 ]
  %0 = call ptr @"Buffer.dataPointer_spec_enum Char(char) | String(string)_11"(ptr %self)
  %index_loaded2 = load i64, ptr %index, align 8
  %refAtIndex = getelementptr inbounds %"51", ptr %0, i64 %index_loaded2
  ret ptr %refAtIndex
}

define ptr @"Buffer.dataPointer_spec_enum Char(char) | String(string)_11"(ptr %self_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %struc.data = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 1
  %struc.data1 = load ptr, ptr %struc.data, align 8
  ret ptr %struc.data1
}

define void @"Iterable_Buffer[enum Char(char) | String(string)]_iterator"(ptr sret({ { i64, ptr }, i64 }) %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %call_sret = alloca { { i64, ptr }, i64 }, align 8
  call void @"BufferIterator.fromBuffer_spec_enum Char(char) | String(string)_10"(ptr sret({ { i64, ptr }, i64 }) %call_sret, ptr %self)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret, i64 24, i1 false)
  ret void
}

define void @"BufferIterator.fromBuffer_spec_enum Char(char) | String(string)_10"(ptr sret({ { i64, ptr }, i64 }) %sret_ptr, ptr %buf_arg) {
entry:
  %buf = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %buf, ptr align 8 %buf_arg, i64 16, i1 false)
  %struct_literal = alloca { { i64, ptr }, i64 }, align 8
  %buf_store_addr = getelementptr inbounds { { i64, ptr }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %buf_store_addr, ptr align 8 %buf, i64 16, i1 false)
  %pos_store_addr = getelementptr inbounds { { i64, ptr }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 0, ptr %pos_store_addr, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %struct_literal, i64 24, i1 false)
  ret void
}

define void @"Iterator_BufferIterator[enum Char(char) | String(string)]_next"(ptr sret(%"Opt[enum Char(char) | String(string)]") %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca ptr, align 8
  store ptr %self_arg, ptr %self, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %self_loaded = load ptr, ptr %self, align 8
  %struc.pos = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded, i32 0, i32 1
  %struc.pos1 = load i64, ptr %struc.pos, align 8
  %self_loaded2 = load ptr, ptr %self, align 8
  %struc.buf = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded2, i32 0, i32 0
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %struc.buf, i32 0, i32 0
  %struc.len3 = load i64, ptr %struc.len, align 8
  %"<_i1" = icmp slt i64 %struc.pos1, %struc.len3
  %"<_res" = sext i1 %"<_i1" to i8
  %arm_pattern_i1 = trunc i8 %"<_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %self_loaded4 = load ptr, ptr %self, align 8
  %struc.buf5 = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded4, i32 0, i32 0
  %self_loaded6 = load ptr, ptr %self, align 8
  %struc.pos7 = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded6, i32 0, i32 1
  %struc.pos8 = load i64, ptr %struc.pos7, align 8
  %call_sret = alloca %"51", align 8
  call void @"Buffer.get_spec_enum Char(char) | String(string)_14"(ptr sret(%"51") %call_sret, ptr %struc.buf5, i64 %struc.pos8)
  %item = alloca %"51", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %item, ptr align 8 %call_sret, i64 24, i1 false)
  %self_loaded9 = load ptr, ptr %self, align 8
  %struc.pos10 = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded9, i32 0, i32 1
  %struc.pos11 = load i64, ptr %struc.pos10, align 8
  %add = add i64 %struc.pos11, 1
  %self_loaded12 = load ptr, ptr %self, align 8
  %struc.pos13 = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded12, i32 0, i32 1
  store i64 %add, ptr %struc.pos13, align 8
  %call_sret14 = alloca %"Opt[enum Char(char) | String(string)]", align 8
  call void @"some_spec_enum Char(char) | String(string)_14"(ptr sret(%"Opt[enum Char(char) | String(string)]") %call_sret14, ptr %item)
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  %enum_constr = alloca %"Opt[enum Char(char) | String(string)]", align 8
  %enum_tag_None = getelementptr inbounds %"Opt[enum Char(char) | String(string)].None", ptr %enum_constr, i32 0, i32 0
  store i64 0, ptr %enum_tag_None, align 8
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1, %arm_cons_0
  %match_result = phi ptr [ %call_sret14, %arm_cons_0 ], [ %enum_constr, %arm_cons_1 ]
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %match_result, i64 32, i1 false)
  ret void
}

define void @"Buffer.get_spec_enum Char(char) | String(string)_14"(ptr sret(%"51") %sret_ptr, ptr %self_arg, i64 %index_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %index = alloca i64, align 8
  store i64 %index_arg, ptr %index, align 8
  %index_loaded = load i64, ptr %index, align 8
  %0 = call ptr @"Buffer.getRef_spec_enum Char(char) | String(string)_15"(ptr %self, i64 %index_loaded)
  %tRef = alloca ptr, align 8
  store ptr %0, ptr %tRef, align 8
  %tRef_loaded = load ptr, ptr %tRef, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %tRef_loaded, i64 24, i1 false)
  ret void
}

define void @"some_spec_enum Char(char) | String(string)_14"(ptr sret(%"Opt[enum Char(char) | String(string)]") %sret_ptr, ptr %value_arg) {
entry:
  %value = alloca %"51", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %value, ptr align 8 %value_arg, i64 24, i1 false)
  %enum_constr = alloca %"Opt[enum Char(char) | String(string)]", align 8
  %enum_tag_Some = getelementptr inbounds %"Opt[enum Char(char) | String(string)].Some", ptr %enum_constr, i32 0, i32 0
  store i64 1, ptr %enum_tag_Some, align 8
  %enum_payload_Some = getelementptr inbounds %"Opt[enum Char(char) | String(string)].Some", ptr %enum_constr, i32 0, i32 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %enum_payload_Some, ptr align 8 %value, i64 24, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %enum_constr, i64 32, i1 false)
  ret void
}

define i8 @"Opt.isSome_spec_enum Char(char) | String(string)_8"(ptr %self_arg) {
entry:
  %self = alloca %"Opt[enum Char(char) | String(string)]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 32, i1 false)
  %__match_subject_1391 = alloca %"Opt[enum Char(char) | String(string)]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__match_subject_1391, ptr align 8 %self, i64 32, i1 false)
  br label %arm_0

arm_0:                                            ; preds = %entry
  %get_payload_ptr = getelementptr inbounds %"Opt[enum Char(char) | String(string)].Some", ptr %__match_subject_1391, i32 0, i32 1
  %__payload_Some_1392 = alloca %"51", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__payload_Some_1392, ptr align 8 %get_payload_ptr, i64 24, i1 false)
  %tag = load i64, ptr %__match_subject_1391, align 8
  %is_variant_cmp = icmp eq i64 %tag, 1
  %is_variant_Some = sext i1 %is_variant_cmp to i8
  %arm_pattern_i1 = trunc i8 %is_variant_Some to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %arm_2

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

arm_2:                                            ; preds = %arm_1
  br i1 true, label %arm_cons_2, label %match_fail

arm_cons_2:                                       ; preds = %arm_2
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.26, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 2, ptr %line_store_addr, align 8
  call void @crash(ptr %struct_literal, ptr @str.28)
  unreachable

match_fail:                                       ; preds = %arm_2
  unreachable

match_end:                                        ; preds = %arm_cons_1, %arm_cons_0
  %match_result = phi i8 [ 1, %arm_cons_0 ], [ 0, %arm_cons_1 ]
  ret i8 %match_result
}

define void @"Unwrap_Opt[enum Char(char) | String(string)]_unwrap"(ptr sret(%"51") %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca %"Opt[enum Char(char) | String(string)]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 32, i1 false)
  %call_sret = alloca %"51", align 8
  call void @"Opt.get_spec_enum Char(char) | String(string)_11"(ptr sret(%"51") %call_sret, ptr %self)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret, i64 24, i1 false)
  ret void
}

define void @"Opt.get_spec_enum Char(char) | String(string)_11"(ptr sret(%"51") %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca %"Opt[enum Char(char) | String(string)]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 32, i1 false)
  br label %arm_0

arm_0:                                            ; preds = %entry
  %__if_target_1584 = alloca %"Opt[enum Char(char) | String(string)]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__if_target_1584, ptr align 8 %self, i64 32, i1 false)
  %get_payload_ptr = getelementptr inbounds %"Opt[enum Char(char) | String(string)].Some", ptr %__if_target_1584, i32 0, i32 1
  %__payload_Some_1585 = alloca %"51", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__payload_Some_1585, ptr align 8 %get_payload_ptr, i64 24, i1 false)
  %tag = load i64, ptr %__if_target_1584, align 8
  %is_variant_cmp = icmp eq i64 %tag, 1
  %is_variant_Some = sext i1 %is_variant_cmp to i8
  %arm_pattern_i1 = trunc i8 %is_variant_Some to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %t = alloca %"51", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %t, ptr align 8 %__payload_Some_1585, i64 24, i1 false)
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.30, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 7, ptr %line_store_addr, align 8
  call void @crash(ptr %struct_literal, ptr @str.32)
  unreachable

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_0
  %match_result = phi ptr [ %t, %arm_cons_0 ]
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %match_result, i64 24, i1 false)
  ret void
}

define i64 @string.len(ptr %self_arg) {
entry:
  %self = alloca { { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %struc.buffer = getelementptr inbounds { { i64, ptr } }, ptr %self, i32 0, i32 0
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %struc.buffer, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  ret i64 %struc.len1
}

define void @Iterable_string_iterator(ptr sret({ { i64, ptr }, i64 }) %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca { { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %struc.buffer = getelementptr inbounds { { i64, ptr } }, ptr %self, i32 0, i32 0
  %call_sret = alloca { { i64, ptr }, i64 }, align 8
  call void @BufferIterator.fromBuffer_spec_char_2(ptr sret({ { i64, ptr }, i64 }) %call_sret, ptr %struc.buffer)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret, i64 24, i1 false)
  ret void
}

define void @BufferIterator.fromBuffer_spec_char_2(ptr sret({ { i64, ptr }, i64 }) %sret_ptr, ptr %buf_arg) {
entry:
  %buf = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %buf, ptr align 8 %buf_arg, i64 16, i1 false)
  %struct_literal = alloca { { i64, ptr }, i64 }, align 8
  %buf_store_addr = getelementptr inbounds { { i64, ptr }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %buf_store_addr, ptr align 8 %buf, i64 16, i1 false)
  %pos_store_addr = getelementptr inbounds { { i64, ptr }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 0, ptr %pos_store_addr, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %struct_literal, i64 24, i1 false)
  ret void
}

define void @"Iterator_BufferIterator[char]_next"(ptr sret(%"Opt[char]") %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca ptr, align 8
  store ptr %self_arg, ptr %self, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %self_loaded = load ptr, ptr %self, align 8
  %struc.pos = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded, i32 0, i32 1
  %struc.pos1 = load i64, ptr %struc.pos, align 8
  %self_loaded2 = load ptr, ptr %self, align 8
  %struc.buf = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded2, i32 0, i32 0
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %struc.buf, i32 0, i32 0
  %struc.len3 = load i64, ptr %struc.len, align 8
  %"<_i1" = icmp slt i64 %struc.pos1, %struc.len3
  %"<_res" = sext i1 %"<_i1" to i8
  %arm_pattern_i1 = trunc i8 %"<_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %self_loaded4 = load ptr, ptr %self, align 8
  %struc.buf5 = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded4, i32 0, i32 0
  %self_loaded6 = load ptr, ptr %self, align 8
  %struc.pos7 = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded6, i32 0, i32 1
  %struc.pos8 = load i64, ptr %struc.pos7, align 8
  %0 = call i8 @Buffer.get_spec_char_6(ptr %struc.buf5, i64 %struc.pos8)
  %item = alloca i8, align 1
  store i8 %0, ptr %item, align 1
  %self_loaded9 = load ptr, ptr %self, align 8
  %struc.pos10 = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded9, i32 0, i32 1
  %struc.pos11 = load i64, ptr %struc.pos10, align 8
  %add = add i64 %struc.pos11, 1
  %self_loaded12 = load ptr, ptr %self, align 8
  %struc.pos13 = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded12, i32 0, i32 1
  store i64 %add, ptr %struc.pos13, align 8
  %item_loaded = load i8, ptr %item, align 1
  %call_sret = alloca %"Opt[char]", align 8
  call void @some_spec_char_7(ptr sret(%"Opt[char]") %call_sret, i8 %item_loaded)
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  %enum_constr = alloca %"Opt[char]", align 8
  %enum_tag_None = getelementptr inbounds %"Opt[char].None", ptr %enum_constr, i32 0, i32 0
  store i8 0, ptr %enum_tag_None, align 1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1, %arm_cons_0
  %match_result = phi ptr [ %call_sret, %arm_cons_0 ], [ %enum_constr, %arm_cons_1 ]
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %sret_ptr, ptr align 1 %match_result, i64 2, i1 false)
  ret void
}

define i8 @Buffer.get_spec_char_6(ptr %self_arg, i64 %index_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %index = alloca i64, align 8
  store i64 %index_arg, ptr %index, align 8
  %index_loaded = load i64, ptr %index, align 8
  %0 = call ptr @Buffer.getRef_spec_char_10(ptr %self, i64 %index_loaded)
  %tRef = alloca ptr, align 8
  store ptr %0, ptr %tRef, align 8
  %tRef_loaded = load ptr, ptr %tRef, align 8
  %deref = load i8, ptr %tRef_loaded, align 1
  ret i8 %deref
}

define ptr @Buffer.getRef_spec_char_10(ptr %self_arg, i64 %index_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %index = alloca i64, align 8
  store i64 %index_arg, ptr %index, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %index_loaded = load i64, ptr %index, align 8
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %">=_i1" = icmp sge i64 %index_loaded, %struc.len1
  %">=_res" = sext i1 %">=_i1" to i8
  %arm_pattern_i1 = trunc i8 %">=_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.38, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 87, ptr %line_store_addr, align 8
  call void @crash(ptr %struct_literal, ptr @str.40)
  unreachable

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1
  %match_result = phi i8 [ 0, %arm_cons_1 ]
  %0 = call ptr @Buffer.dataPointer_spec_char_5(ptr %self)
  %index_loaded2 = load i64, ptr %index, align 8
  %refAtIndex = getelementptr inbounds i8, ptr %0, i64 %index_loaded2
  ret ptr %refAtIndex
}

define ptr @Buffer.dataPointer_spec_char_5(ptr %self_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %struc.data = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 1
  %struc.data1 = load ptr, ptr %struc.data, align 8
  ret ptr %struc.data1
}

define void @some_spec_char_7(ptr sret(%"Opt[char]") %sret_ptr, i8 %value_arg) {
entry:
  %value = alloca i8, align 1
  store i8 %value_arg, ptr %value, align 1
  %enum_constr = alloca %"Opt[char]", align 8
  %enum_tag_Some = getelementptr inbounds %"Opt[char].Some", ptr %enum_constr, i32 0, i32 0
  store i8 1, ptr %enum_tag_Some, align 1
  %value_loaded = load i8, ptr %value, align 1
  %enum_payload_Some = getelementptr inbounds %"Opt[char].Some", ptr %enum_constr, i32 0, i32 1
  store i8 %value_loaded, ptr %enum_payload_Some, align 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %sret_ptr, ptr align 1 %enum_constr, i64 2, i1 false)
  ret void
}

define i8 @Opt.isSome_spec_char_6(ptr %self_arg) {
entry:
  %self = alloca %"Opt[char]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self, ptr align 1 %self_arg, i64 2, i1 false)
  %__match_subject_1353 = alloca %"Opt[char]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %__match_subject_1353, ptr align 1 %self, i64 2, i1 false)
  br label %arm_0

arm_0:                                            ; preds = %entry
  %get_payload_ptr = getelementptr inbounds %"Opt[char].Some", ptr %__match_subject_1353, i32 0, i32 1
  %payload_by_value = load i8, ptr %get_payload_ptr, align 1
  %__payload_Some_1354 = alloca i8, align 1
  store i8 %payload_by_value, ptr %__payload_Some_1354, align 1
  %tag = load i8, ptr %__match_subject_1353, align 1
  %is_variant_cmp = icmp eq i8 %tag, 1
  %is_variant_Some = sext i1 %is_variant_cmp to i8
  %arm_pattern_i1 = trunc i8 %is_variant_Some to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %arm_2

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

arm_2:                                            ; preds = %arm_1
  br i1 true, label %arm_cons_2, label %match_fail

arm_cons_2:                                       ; preds = %arm_2
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.42, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 2, ptr %line_store_addr, align 8
  call void @crash(ptr %struct_literal, ptr @str.44)
  unreachable

match_fail:                                       ; preds = %arm_2
  unreachable

match_end:                                        ; preds = %arm_cons_1, %arm_cons_0
  %match_result = phi i8 [ 1, %arm_cons_0 ], [ 0, %arm_cons_1 ]
  ret i8 %match_result
}

define i8 @"Unwrap_Opt[char]_unwrap"(ptr %self_arg) {
entry:
  %self = alloca %"Opt[char]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self, ptr align 1 %self_arg, i64 2, i1 false)
  %0 = call i8 @Opt.get_spec_char_9(ptr %self)
  ret i8 %0
}

define i8 @Opt.get_spec_char_9(ptr %self_arg) {
entry:
  %self = alloca %"Opt[char]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self, ptr align 1 %self_arg, i64 2, i1 false)
  br label %arm_0

arm_0:                                            ; preds = %entry
  %__if_target_1544 = alloca %"Opt[char]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %__if_target_1544, ptr align 1 %self, i64 2, i1 false)
  %get_payload_ptr = getelementptr inbounds %"Opt[char].Some", ptr %__if_target_1544, i32 0, i32 1
  %payload_by_value = load i8, ptr %get_payload_ptr, align 1
  %__payload_Some_1545 = alloca i8, align 1
  store i8 %payload_by_value, ptr %__payload_Some_1545, align 1
  %tag = load i8, ptr %__if_target_1544, align 1
  %is_variant_cmp = icmp eq i8 %tag, 1
  %is_variant_Some = sext i1 %is_variant_cmp to i8
  %arm_pattern_i1 = trunc i8 %is_variant_Some to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %__payload_Some_1545_loaded = load i8, ptr %__payload_Some_1545, align 1
  %t = alloca i8, align 1
  store i8 %__payload_Some_1545_loaded, ptr %t, align 1
  %t_loaded = load i8, ptr %t, align 1
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.46, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 7, ptr %line_store_addr, align 8
  call void @crash(ptr %struct_literal, ptr @str.48)
  unreachable

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_0
  %match_result = phi i8 [ %t_loaded, %arm_cons_0 ]
  ret i8 %match_result
}

define void @string.wrapList(ptr sret({ { i64, ptr } }) %sret_ptr, ptr %list_arg) {
entry:
  %list = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %list, ptr align 8 %list_arg, i64 24, i1 false)
  %struct_literal = alloca { { i64, ptr } }, align 8
  %call_sret = alloca { i64, ptr }, align 8
  call void @List.toBuffer_spec_char_2(ptr sret({ i64, ptr }) %call_sret, ptr %list)
  %buffer_store_addr = getelementptr inbounds { { i64, ptr } }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %buffer_store_addr, ptr align 8 %call_sret, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %struct_literal, i64 16, i1 false)
  ret void
}

define void @List.toBuffer_spec_char_2(ptr sret({ i64, ptr }) %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 24, i1 false)
  %struc.buffer = getelementptr inbounds { i64, { i64, ptr } }, ptr %self, i32 0, i32 1
  %struc.len = getelementptr inbounds { i64, { i64, ptr } }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %call_sret = alloca { i64, ptr }, align 8
  call void @Buffer.slice_spec_char_6(ptr sret({ i64, ptr }) %call_sret, ptr %struc.buffer, i64 0, i64 %struc.len1)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret, i64 16, i1 false)
  ret void
}

define void @Buffer.slice_spec_char_6(ptr sret({ i64, ptr }) %sret_ptr, ptr %self_arg, i64 %start_arg, i64 %end_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %start = alloca i64, align 8
  store i64 %start_arg, ptr %start, align 8
  %end = alloca i64, align 8
  store i64 %end_arg, ptr %end, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %end_loaded = load i64, ptr %end, align 8
  %start_loaded = load i64, ptr %start, align 8
  %"<_i1" = icmp slt i64 %end_loaded, %start_loaded
  %"<_res" = sext i1 %"<_i1" to i8
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %"==_i1" = icmp eq i64 %struc.len1, 0
  %"==_res" = sext i1 %"==_i1" to i8
  %bool_or = or i8 %"<_res", %"==_res"
  %arm_pattern_i1 = trunc i8 %bool_or to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %call_sret = alloca { i64, ptr }, align 8
  call void @Buffer.empty_spec_char_3(ptr sret({ i64, ptr }) %call_sret)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret, i64 16, i1 false)
  ret void

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1
  %match_result = phi i8 [ 0, %arm_cons_1 ]
  br label %arm_02

arm_02:                                           ; preds = %match_end
  %start_loaded9 = load i64, ptr %start, align 8
  %struc.len10 = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len11 = load i64, ptr %struc.len10, align 8
  %">=_i1" = icmp sge i64 %start_loaded9, %struc.len11
  %">=_res" = sext i1 %">=_i1" to i8
  %start_loaded12 = load i64, ptr %start, align 8
  %"<_i113" = icmp slt i64 %start_loaded12, 0
  %"<_res14" = sext i1 %"<_i113" to i8
  %bool_or15 = or i8 %">=_res", %"<_res14"
  %arm_pattern_i116 = trunc i8 %bool_or15 to i1
  br i1 %arm_pattern_i116, label %arm_cons_03, label %arm_14

arm_cons_03:                                      ; preds = %arm_02
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.54, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 74, ptr %line_store_addr, align 8
  %call_sret17 = alloca { ptr }, align 8
  call void @StringBuilder.withCapacity(ptr sret({ ptr }) %call_sret17, i64 2)
  %__sb_1367 = alloca { ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__sb_1367, ptr align 8 %call_sret17, i64 8, i1 false)
  %0 = call i8 @StringBuilder.putString(ptr %__sb_1367, ptr @str.56)
  %start_loaded18 = load i64, ptr %start, align 8
  %call_sret19 = alloca { { i64, ptr } }, align 8
  call void @Show_u64_show(ptr sret({ { i64, ptr } }) %call_sret19, i64 %start_loaded18)
  %1 = call i8 @StringBuilder.putString(ptr %__sb_1367, ptr %call_sret19)
  %call_sret20 = alloca { { i64, ptr } }, align 8
  call void @StringBuilder.build(ptr sret({ { i64, ptr } }) %call_sret20, ptr %__sb_1367)
  call void @crash(ptr %struct_literal, ptr %call_sret20)
  unreachable

arm_14:                                           ; preds = %arm_02
  br i1 true, label %arm_cons_15, label %match_fail6

arm_cons_15:                                      ; preds = %arm_14
  br label %match_end7

match_fail6:                                      ; preds = %arm_14
  unreachable

match_end7:                                       ; preds = %arm_cons_15
  %match_result8 = phi i8 [ 0, %arm_cons_15 ]
  br label %arm_021

arm_021:                                          ; preds = %match_end7
  %end_loaded28 = load i64, ptr %end, align 8
  %struc.len29 = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len30 = load i64, ptr %struc.len29, align 8
  %">_i1" = icmp sgt i64 %end_loaded28, %struc.len30
  %">_res" = sext i1 %">_i1" to i8
  %arm_pattern_i131 = trunc i8 %">_res" to i1
  br i1 %arm_pattern_i131, label %arm_cons_022, label %arm_123

arm_cons_022:                                     ; preds = %arm_021
  %struc.len32 = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len33 = load i64, ptr %struc.len32, align 8
  br label %match_end26

arm_123:                                          ; preds = %arm_021
  br i1 true, label %arm_cons_124, label %match_fail25

arm_cons_124:                                     ; preds = %arm_123
  %end_loaded34 = load i64, ptr %end, align 8
  br label %match_end26

match_fail25:                                     ; preds = %arm_123
  unreachable

match_end26:                                      ; preds = %arm_cons_124, %arm_cons_022
  %match_result27 = phi i64 [ %struc.len33, %arm_cons_022 ], [ %end_loaded34, %arm_cons_124 ]
  %end35 = alloca i64, align 8
  store i64 %match_result27, ptr %end35, align 8
  %end_loaded36 = load i64, ptr %end35, align 8
  %start_loaded37 = load i64, ptr %start, align 8
  %sub = sub i64 %end_loaded36, %start_loaded37
  %newLen = alloca i64, align 8
  store i64 %sub, ptr %newLen, align 8
  %start_loaded38 = load i64, ptr %start, align 8
  %2 = call ptr @Buffer.getRef_spec_char_10(ptr %self, i64 %start_loaded38)
  %newBase = alloca ptr, align 8
  store ptr %2, ptr %newBase, align 8
  %struct_literal39 = alloca { i64, ptr }, align 8
  %newLen_loaded = load i64, ptr %newLen, align 8
  %len_store_addr = getelementptr inbounds { i64, ptr }, ptr %struct_literal39, i32 0, i32 0
  store i64 %newLen_loaded, ptr %len_store_addr, align 8
  %newBase_loaded = load ptr, ptr %newBase, align 8
  %data_store_addr = getelementptr inbounds { i64, ptr }, ptr %struct_literal39, i32 0, i32 1
  store ptr %newBase_loaded, ptr %data_store_addr, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %struct_literal39, i64 16, i1 false)
  ret void
}

define void @Buffer.empty_spec_char_3(ptr sret({ i64, ptr }) %sret_ptr) {
entry:
  %call_sret = alloca { i64, ptr }, align 8
  call void @Buffer._allocate_spec_char_7(ptr sret({ i64, ptr }) %call_sret, i64 0)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret, i64 16, i1 false)
  ret void
}

declare ptr @memcpy(ptr, ptr, i64)

define i8 @List.set_spec_char_4(ptr %self_arg, i64 %index_arg, i8 %elem_arg) {
entry:
  %self = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 24, i1 false)
  %index = alloca i64, align 8
  store i64 %index_arg, ptr %index, align 8
  %elem = alloca i8, align 1
  store i8 %elem_arg, ptr %elem, align 1
  %struc.buffer = getelementptr inbounds { i64, { i64, ptr } }, ptr %self, i32 0, i32 1
  %index_loaded = load i64, ptr %index, align 8
  %elem_loaded = load i8, ptr %elem, align 1
  %0 = call i8 @Buffer.set_spec_char_4(ptr %struc.buffer, i64 %index_loaded, i8 %elem_loaded)
  ret i8 %0
}

define i8 @Buffer.set_spec_char_4(ptr %self_arg, i64 %index_arg, i8 %elem_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %index = alloca i64, align 8
  store i64 %index_arg, ptr %index, align 8
  %elem = alloca i8, align 1
  store i8 %elem_arg, ptr %elem, align 1
  %index_loaded = load i64, ptr %index, align 8
  %0 = call ptr @Buffer.getRef_spec_char_10(ptr %self, i64 %index_loaded)
  %tRef = alloca ptr, align 8
  store ptr %0, ptr %tRef, align 8
  %elem_loaded = load i8, ptr %elem, align 1
  %tRef_loaded = load ptr, ptr %tRef, align 8
  store i8 %elem_loaded, ptr %tRef_loaded, align 1
  ret i8 0
}

define i8 @List.reverse_spec_char_1(ptr %self_arg) {
entry:
  %self = alloca ptr, align 8
  store ptr %self_arg, ptr %self, align 8
  %i = alloca i64, align 8
  store i64 0, ptr %i, align 8
  br label %while_cond

while_cond:                                       ; preds = %while_body, %entry
  %i_loaded = load i64, ptr %i, align 8
  %self_loaded = load ptr, ptr %self, align 8
  %struc.len = getelementptr inbounds { i64, { i64, ptr } }, ptr %self_loaded, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %udiv = udiv i64 %struc.len1, 2
  %"<_i1" = icmp slt i64 %i_loaded, %udiv
  %"<_res" = sext i1 %"<_i1" to i8
  %while_cond2 = trunc i8 %"<_res" to i1
  br i1 %while_cond2, label %while_body, label %while_end

while_body:                                       ; preds = %while_cond
  %self_loaded3 = load ptr, ptr %self, align 8
  %struc.len4 = getelementptr inbounds { i64, { i64, ptr } }, ptr %self_loaded3, i32 0, i32 0
  %struc.len5 = load i64, ptr %struc.len4, align 8
  %i_loaded6 = load i64, ptr %i, align 8
  %sub = sub i64 %struc.len5, %i_loaded6
  %sub7 = sub i64 %sub, 1
  %j = alloca i64, align 8
  store i64 %sub7, ptr %j, align 8
  %self_loaded8 = load ptr, ptr %self, align 8
  %i_loaded9 = load i64, ptr %i, align 8
  %j_loaded = load i64, ptr %j, align 8
  %0 = call i8 @List.swap_spec_char_2(ptr %self_loaded8, i64 %i_loaded9, i64 %j_loaded)
  %i_loaded10 = load i64, ptr %i, align 8
  %add = add i64 %i_loaded10, 1
  store i64 %add, ptr %i, align 8
  br label %while_cond

while_end:                                        ; preds = %while_cond
  ret i8 0
}

define i8 @List.swap_spec_char_2(ptr %self_arg, i64 %indexA_arg, i64 %indexB_arg) {
entry:
  %self = alloca ptr, align 8
  store ptr %self_arg, ptr %self, align 8
  %indexA = alloca i64, align 8
  store i64 %indexA_arg, ptr %indexA, align 8
  %indexB = alloca i64, align 8
  store i64 %indexB_arg, ptr %indexB, align 8
  %self_loaded = load ptr, ptr %self, align 8
  %indexA_loaded = load i64, ptr %indexA, align 8
  %0 = call ptr @List.getRef_spec_char_5(ptr %self_loaded, i64 %indexA_loaded)
  %aRef = alloca ptr, align 8
  store ptr %0, ptr %aRef, align 8
  %self_loaded1 = load ptr, ptr %self, align 8
  %indexB_loaded = load i64, ptr %indexB, align 8
  %1 = call ptr @List.getRef_spec_char_5(ptr %self_loaded1, i64 %indexB_loaded)
  %bRef = alloca ptr, align 8
  store ptr %1, ptr %bRef, align 8
  %bRef_loaded = load ptr, ptr %bRef, align 8
  %deref = load i8, ptr %bRef_loaded, align 1
  %bValue = alloca i8, align 1
  store i8 %deref, ptr %bValue, align 1
  %aRef_loaded = load ptr, ptr %aRef, align 8
  %deref2 = load i8, ptr %aRef_loaded, align 1
  %bRef_loaded3 = load ptr, ptr %bRef, align 8
  store i8 %deref2, ptr %bRef_loaded3, align 1
  %bValue_loaded = load i8, ptr %bValue, align 1
  %aRef_loaded4 = load ptr, ptr %aRef, align 8
  store i8 %bValue_loaded, ptr %aRef_loaded4, align 1
  ret i8 0
}

define ptr @List.getRef_spec_char_5(ptr %self_arg, i64 %index_arg) {
entry:
  %self = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 24, i1 false)
  %index = alloca i64, align 8
  store i64 %index_arg, ptr %index, align 8
  %struc.buffer = getelementptr inbounds { i64, { i64, ptr } }, ptr %self, i32 0, i32 1
  %index_loaded = load i64, ptr %index, align 8
  %0 = call ptr @Buffer.getRef_spec_char_10(ptr %struc.buffer, i64 %index_loaded)
  ret ptr %0
}

define i8 @"List.set_spec_enum Char(char) | String(string)_6"(ptr %self_arg, i64 %index_arg, ptr %elem_arg) {
entry:
  %self = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 24, i1 false)
  %index = alloca i64, align 8
  store i64 %index_arg, ptr %index, align 8
  %elem = alloca %"51", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %elem, ptr align 8 %elem_arg, i64 24, i1 false)
  %struc.buffer = getelementptr inbounds { i64, { i64, ptr } }, ptr %self, i32 0, i32 1
  %index_loaded = load i64, ptr %index, align 8
  %0 = call i8 @"Buffer.set_spec_enum Char(char) | String(string)_7"(ptr %struc.buffer, i64 %index_loaded, ptr %elem)
  ret i8 %0
}

define i8 @"Buffer.set_spec_enum Char(char) | String(string)_7"(ptr %self_arg, i64 %index_arg, ptr %elem_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %index = alloca i64, align 8
  store i64 %index_arg, ptr %index, align 8
  %elem = alloca %"51", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %elem, ptr align 8 %elem_arg, i64 24, i1 false)
  %index_loaded = load i64, ptr %index, align 8
  %0 = call ptr @"Buffer.getRef_spec_enum Char(char) | String(string)_15"(ptr %self, i64 %index_loaded)
  %tRef = alloca ptr, align 8
  store ptr %0, ptr %tRef, align 8
  %tRef_loaded = load ptr, ptr %tRef, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %tRef_loaded, ptr align 8 %elem, i64 24, i1 false)
  ret i8 0
}

define void @Show_string_show(ptr sret({ { i64, ptr } }) %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca { { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %self, i64 16, i1 false)
  ret void
}

define i8 @eprint(ptr %s_arg) {
entry:
  %s = alloca { { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %s, ptr align 8 %s_arg, i64 16, i1 false)
  %struc.buffer = getelementptr inbounds { { i64, ptr } }, ptr %s, i32 0, i32 0
  %struc.data = getelementptr inbounds { i64, ptr }, ptr %struc.buffer, i32 0, i32 1
  %struc.data1 = load ptr, ptr %struc.data, align 8
  %0 = call i64 @string.len(ptr %s)
  %1 = call i64 @write(i32 2, ptr %struc.data1, i64 %0, i64 0)
  ret i8 0
}

declare i64 @write(i32, ptr, i64, i64)

declare void @exit(i32)

define i64 @lambdas.staticAddTwo(i64 %x_arg) {
entry:
  %x = alloca i64, align 8
  store i64 %x_arg, ptr %x, align 8
  %x_loaded = load i64, ptr %x, align 8
  %add = add i64 %x_loaded, 2
  ret i64 %add
}

define i64 @lambdas.someFnParam2_spec__1(i64 %i_arg, ptr %f_arg, ptr %g_arg) {
entry:
  %i = alloca i64, align 8
  store i64 %i_arg, ptr %i, align 8
  %f = alloca ptr, align 8
  store ptr %f_arg, ptr %f, align 8
  %g = alloca ptr, align 8
  store ptr %g_arg, ptr %g, align 8
  %i_loaded = load i64, ptr %i, align 8
  %f_loaded = load ptr, ptr %f, align 8
  %0 = call i64 %f_loaded(i64 %i_loaded)
  %i_loaded1 = load i64, ptr %i, align 8
  %g_loaded = load ptr, ptr %g, align 8
  %1 = call i64 %g_loaded(i64 %i_loaded1)
  %add = add i64 %0, %1
  ret i64 %add
}

define i8 @assertEquals_spec_i64_1(ptr %locn_arg, i64 %a_arg, i64 %b_arg) {
entry:
  %locn = alloca { { { i64, ptr } }, i64 }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %locn, ptr align 8 %locn_arg, i64 24, i1 false)
  %a = alloca i64, align 8
  store i64 %a_arg, ptr %a, align 8
  %b = alloca i64, align 8
  store i64 %b_arg, ptr %b, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %a_loaded = load i64, ptr %a, align 8
  %b_loaded = load i64, ptr %b, align 8
  %"!=_i1" = icmp ne i64 %a_loaded, %b_loaded
  %"!=_res" = sext i1 %"!=_i1" to i8
  %arm_pattern_i1 = trunc i8 %"!=_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %call_sret = alloca { ptr }, align 8
  call void @StringBuilder.withCapacity(ptr sret({ ptr }) %call_sret, i64 4)
  %__sb_1425 = alloca { ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__sb_1425, ptr align 8 %call_sret, i64 8, i1 false)
  %0 = call i8 @StringBuilder.putString(ptr %__sb_1425, ptr @str.76)
  %a_loaded1 = load i64, ptr %a, align 8
  %call_sret2 = alloca { { i64, ptr } }, align 8
  call void @Show_i64_show(ptr sret({ { i64, ptr } }) %call_sret2, i64 %a_loaded1)
  %1 = call i8 @StringBuilder.putString(ptr %__sb_1425, ptr %call_sret2)
  %2 = call i8 @StringBuilder.putString(ptr %__sb_1425, ptr @str.80)
  %b_loaded3 = load i64, ptr %b, align 8
  %call_sret4 = alloca { { i64, ptr } }, align 8
  call void @Show_i64_show(ptr sret({ { i64, ptr } }) %call_sret4, i64 %b_loaded3)
  %3 = call i8 @StringBuilder.putString(ptr %__sb_1425, ptr %call_sret4)
  %call_sret5 = alloca { { i64, ptr } }, align 8
  call void @StringBuilder.build(ptr sret({ { i64, ptr } }) %call_sret5, ptr %__sb_1425)
  call void @crash(ptr %locn, ptr %call_sret5)
  unreachable

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1
  %match_result = phi i8 [ 0, %arm_cons_1 ]
  ret i8 %match_result
}

define void @Show_i64_show(ptr sret({ { i64, ptr } }) %sret_ptr, i64 %self_arg) {
entry:
  %self = alloca i64, align 8
  store i64 %self_arg, ptr %self, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %self_loaded = load i64, ptr %self, align 8
  %"==_i1" = icmp eq i64 %self_loaded, 0
  %"==_res" = sext i1 %"==_i1" to i8
  %arm_pattern_i1 = trunc i8 %"==_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 @str.78, i64 16, i1 false)
  ret void

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1
  %match_result = phi i8 [ 0, %arm_cons_1 ]
  %self_loaded1 = load i64, ptr %self, align 8
  %"<_i1" = icmp slt i64 %self_loaded1, 0
  %"<_res" = sext i1 %"<_i1" to i8
  %neg = alloca i8, align 1
  store i8 %"<_res", ptr %neg, align 1
  br label %arm_02

arm_02:                                           ; preds = %match_end
  %neg_loaded = load i8, ptr %neg, align 1
  %arm_pattern_i19 = trunc i8 %neg_loaded to i1
  br i1 %arm_pattern_i19, label %arm_cons_03, label %arm_14

arm_cons_03:                                      ; preds = %arm_02
  %self_loaded10 = load i64, ptr %self, align 8
  %mul = mul i64 %self_loaded10, -1
  br label %match_end7

arm_14:                                           ; preds = %arm_02
  br i1 true, label %arm_cons_15, label %match_fail6

arm_cons_15:                                      ; preds = %arm_14
  %self_loaded11 = load i64, ptr %self, align 8
  br label %match_end7

match_fail6:                                      ; preds = %arm_14
  unreachable

match_end7:                                       ; preds = %arm_cons_15, %arm_cons_03
  %match_result8 = phi i64 [ %mul, %arm_cons_03 ], [ %self_loaded11, %arm_cons_15 ]
  %remaining = alloca i64, align 8
  store i64 %match_result8, ptr %remaining, align 8
  %call_sret = alloca { i64, { i64, ptr } }, align 8
  call void @List.withCapacity_spec_char_2(ptr sret({ i64, { i64, ptr } }) %call_sret, i64 8)
  %buf = alloca ptr, align 8
  store ptr %call_sret, ptr %buf, align 8
  br label %while_cond

while_cond:                                       ; preds = %while_body, %match_end7
  %remaining_loaded = load i64, ptr %remaining, align 8
  %">_i1" = icmp sgt i64 %remaining_loaded, 0
  %">_res" = sext i1 %">_i1" to i8
  %while_cond12 = trunc i8 %">_res" to i1
  br i1 %while_cond12, label %while_body, label %while_end

while_body:                                       ; preds = %while_cond
  %remaining_loaded13 = load i64, ptr %remaining, align 8
  %srem = srem i64 %remaining_loaded13, 10
  %d = alloca i64, align 8
  store i64 %srem, ptr %d, align 8
  %d_loaded = load i64, ptr %d, align 8
  %add = add i64 %d_loaded, 48
  %trunc_cast = trunc i64 %add to i8
  %c = alloca i8, align 1
  store i8 %trunc_cast, ptr %c, align 1
  %buf_loaded = load ptr, ptr %buf, align 8
  %c_loaded = load i8, ptr %c, align 1
  %0 = call i8 @List.push_spec_char_2(ptr %buf_loaded, i8 %c_loaded)
  %remaining_loaded14 = load i64, ptr %remaining, align 8
  %sdiv = sdiv i64 %remaining_loaded14, 10
  store i64 %sdiv, ptr %remaining, align 8
  br label %while_cond

while_end:                                        ; preds = %while_cond
  br label %arm_015

arm_015:                                          ; preds = %while_end
  %neg_loaded22 = load i8, ptr %neg, align 1
  %arm_pattern_i123 = trunc i8 %neg_loaded22 to i1
  br i1 %arm_pattern_i123, label %arm_cons_016, label %arm_117

arm_cons_016:                                     ; preds = %arm_015
  %buf_loaded24 = load ptr, ptr %buf, align 8
  %1 = call i8 @List.push_spec_char_2(ptr %buf_loaded24, i8 45)
  br label %match_end20

arm_117:                                          ; preds = %arm_015
  br i1 true, label %arm_cons_118, label %match_fail19

arm_cons_118:                                     ; preds = %arm_117
  br label %match_end20

match_fail19:                                     ; preds = %arm_117
  unreachable

match_end20:                                      ; preds = %arm_cons_118, %arm_cons_016
  %match_result21 = phi i8 [ %1, %arm_cons_016 ], [ 0, %arm_cons_118 ]
  %buf_loaded25 = load ptr, ptr %buf, align 8
  %call_sret26 = alloca { i64, { i64, ptr } }, align 8
  call void @List.reversed_spec_char_1(ptr sret({ i64, { i64, ptr } }) %call_sret26, ptr %buf_loaded25)
  %rev = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %rev, ptr align 8 %call_sret26, i64 24, i1 false)
  %call_sret27 = alloca { { i64, ptr } }, align 8
  call void @string.wrapList(ptr sret({ { i64, ptr } }) %call_sret27, ptr %rev)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret27, i64 16, i1 false)
  ret void
}

define void @List.reversed_spec_char_1(ptr sret({ i64, { i64, ptr } }) %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 24, i1 false)
  %call_sret = alloca { i64, { i64, ptr } }, align 8
  call void @List.cloned_spec_char_3(ptr sret({ i64, { i64, ptr } }) %call_sret, ptr %self)
  %clone = alloca ptr, align 8
  store ptr %call_sret, ptr %clone, align 8
  %clone_loaded = load ptr, ptr %clone, align 8
  %0 = call i8 @List.reverse_spec_char_1(ptr %clone_loaded)
  %clone_loaded1 = load ptr, ptr %clone, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %clone_loaded1, i64 24, i1 false)
  ret void
}

define void @List.cloned_spec_char_3(ptr sret({ i64, { i64, ptr } }) %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 24, i1 false)
  %struc.buffer = getelementptr inbounds { i64, { i64, ptr } }, ptr %self, i32 0, i32 1
  %call_sret = alloca { i64, ptr }, align 8
  call void @Buffer.cloned_spec_char_2(ptr sret({ i64, ptr }) %call_sret, ptr %struc.buffer)
  %newBuffer = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %newBuffer, ptr align 8 %call_sret, i64 16, i1 false)
  %struct_literal = alloca { i64, { i64, ptr } }, align 8
  %struc.len = getelementptr inbounds { i64, { i64, ptr } }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %len_store_addr = getelementptr inbounds { i64, { i64, ptr } }, ptr %struct_literal, i32 0, i32 0
  store i64 %struc.len1, ptr %len_store_addr, align 8
  %buffer_store_addr = getelementptr inbounds { i64, { i64, ptr } }, ptr %struct_literal, i32 0, i32 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %buffer_store_addr, ptr align 8 %newBuffer, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %struct_literal, i64 24, i1 false)
  ret void
}

define void @Buffer.cloned_spec_char_2(ptr sret({ i64, ptr }) %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %0 = call i64 @Buffer.lenBytes_spec_char_3(ptr %self)
  %numBytes = alloca i64, align 8
  store i64 %0, ptr %numBytes, align 8
  %numBytes_loaded = load i64, ptr %numBytes, align 8
  %1 = call ptr @malloc(i64 %numBytes_loaded)
  %newBase = alloca ptr, align 8
  store ptr %1, ptr %newBase, align 8
  %newBase_loaded = load ptr, ptr %newBase, align 8
  %2 = call ptr @Buffer.dataPointer_spec_char_5(ptr %self)
  %numBytes_loaded1 = load i64, ptr %numBytes, align 8
  %3 = call ptr @memcpy(ptr %newBase_loaded, ptr %2, i64 %numBytes_loaded1)
  %struct_literal = alloca { i64, ptr }, align 8
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len2 = load i64, ptr %struc.len, align 8
  %len_store_addr = getelementptr inbounds { i64, ptr }, ptr %struct_literal, i32 0, i32 0
  store i64 %struc.len2, ptr %len_store_addr, align 8
  %newBase_loaded3 = load ptr, ptr %newBase, align 8
  %data_store_addr = getelementptr inbounds { i64, ptr }, ptr %struct_literal, i32 0, i32 1
  store ptr %newBase_loaded3, ptr %data_store_addr, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %struct_literal, i64 16, i1 false)
  ret void
}

define i64 @Buffer.lenBytes_spec_char_3(ptr %self_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %0 = call i64 @Buffer.bufferLenBytes_spec_char_5(i64 %struc.len1)
  ret i64 %0
}

define i64 @lambdas.someFnParam1_spec__1(i64 %i_arg, ptr %f_arg) {
entry:
  %i = alloca i64, align 8
  store i64 %i_arg, ptr %i, align 8
  %f = alloca ptr, align 8
  store ptr %f_arg, ptr %f, align 8
  %i_loaded = load i64, ptr %i, align 8
  %f_loaded = load ptr, ptr %f, align 8
  %0 = call i64 %f_loaded(i64 %i_loaded)
  ret i64 %0
}

define i64 @lambdas.staticAddOne__dyn(ptr %__lambda_env_arg, i64 %x_arg) {
entry:
  %__lambda_env = alloca ptr, align 8
  store ptr %__lambda_env_arg, ptr %__lambda_env, align 8
  %x = alloca i64, align 8
  store i64 %x_arg, ptr %x, align 8
  %x_loaded = load i64, ptr %x, align 8
  %add = add i64 %x_loaded, 1
  ret i64 %add
}

define i64 @lambdas.someFnParam1_spec__2(i64 %i_arg, ptr %f_arg) {
entry:
  %i = alloca i64, align 8
  store i64 %i_arg, ptr %i, align 8
  %f = alloca { ptr, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %f, ptr align 8 %f_arg, i64 16, i1 false)
  %i_loaded = load i64, ptr %i, align 8
  %fn_ptr_gep = getelementptr inbounds { ptr, ptr }, ptr %f, i32 0, i32 0
  %fn_ptr = load ptr, ptr %fn_ptr_gep, align 8
  %env_ptr_gep = getelementptr inbounds { ptr, ptr }, ptr %f, i32 0, i32 1
  %env_ptr = load ptr, ptr %env_ptr_gep, align 8
  %0 = call i64 %fn_ptr(ptr %env_ptr, i64 %i_loaded)
  ret i64 %0
}

define i64 @"lambdas.lambdas__test_{lambda}_1178"(ptr %__lambda_env_arg, i64 %x_arg) {
entry:
  %__lambda_env = alloca ptr, align 8
  store ptr %__lambda_env_arg, ptr %__lambda_env, align 8
  %x = alloca i64, align 8
  store i64 %x_arg, ptr %x, align 8
  %x_loaded = load i64, ptr %x, align 8
  %add = add i64 %x_loaded, 1
  %__lambda_env_loaded = load ptr, ptr %__lambda_env, align 8
  %struc.c1 = getelementptr inbounds { ptr }, ptr %__lambda_env_loaded, i32 0, i32 0
  %struc.c11 = load ptr, ptr %struc.c1, align 8
  %struc.x = getelementptr inbounds { i64 }, ptr %struc.c11, i32 0, i32 0
  %struc.x2 = load i64, ptr %struc.x, align 8
  %add3 = add i64 %add, %struc.x2
  ret i64 %add3
}

define i64 @"lambdas.lambdas__test_{lambda}_1179"(ptr %__lambda_env_arg, i64 %x_arg) {
entry:
  %__lambda_env = alloca ptr, align 8
  store ptr %__lambda_env_arg, ptr %__lambda_env, align 8
  %x = alloca i64, align 8
  store i64 %x_arg, ptr %x, align 8
  %x_loaded = load i64, ptr %x, align 8
  %add = add i64 %x_loaded, 2
  %__lambda_env_loaded = load ptr, ptr %__lambda_env, align 8
  %struc.c2 = getelementptr inbounds { i64 }, ptr %__lambda_env_loaded, i32 0, i32 0
  %struc.c21 = load i64, ptr %struc.c2, align 8
  %add2 = add i64 %add, %struc.c21
  ret i64 %add2
}

define i64 @lambdas.someFnParam2_spec__2(i64 %i_arg, ptr %f_arg, ptr %g_arg) {
entry:
  %i = alloca i64, align 8
  store i64 %i_arg, ptr %i, align 8
  %f = alloca { ptr, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %f, ptr align 8 %f_arg, i64 16, i1 false)
  %g = alloca { ptr, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %g, ptr align 8 %g_arg, i64 16, i1 false)
  %i_loaded = load i64, ptr %i, align 8
  %0 = call i64 @"lambdas.lambdas__test_{lambda}_1178"(ptr %f, i64 %i_loaded)
  %i_loaded1 = load i64, ptr %i, align 8
  %1 = call i64 @"lambdas.lambdas__test_{lambda}_1179"(ptr %g, i64 %i_loaded1)
  %add = add i64 %0, %1
  ret i64 %add
}

define i8 @"lambdas.lambdas__test_{lambda}_1185"(ptr %__lambda_env_arg, i8 %b_arg) {
entry:
  %__lambda_env = alloca ptr, align 8
  store ptr %__lambda_env_arg, ptr %__lambda_env, align 8
  %b = alloca i8, align 1
  store i8 %b_arg, ptr %b, align 1
  %b_loaded = load i8, ptr %b, align 1
  %0 = trunc i8 %b_loaded to i1
  %1 = xor i1 %0, true
  %2 = sext i1 %1 to i8
  ret i8 %2
}

define void @"lambdas.lambdas__test_{lambda}_1186"(ptr sret(%"Opt[bool]") %sret_ptr, ptr %__lambda_env_arg, i8 %b_arg) {
entry:
  %__lambda_env = alloca ptr, align 8
  store ptr %__lambda_env_arg, ptr %__lambda_env, align 8
  %b = alloca i8, align 1
  store i8 %b_arg, ptr %b, align 1
  %b_loaded = load i8, ptr %b, align 1
  %call_sret = alloca %"Opt[bool]", align 8
  call void @some_spec_bool_4(ptr sret(%"Opt[bool]") %call_sret, i8 %b_loaded)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %sret_ptr, ptr align 1 %call_sret, i64 2, i1 false)
  ret void
}

define void @some_spec_bool_4(ptr sret(%"Opt[bool]") %sret_ptr, i8 %value_arg) {
entry:
  %value = alloca i8, align 1
  store i8 %value_arg, ptr %value, align 1
  %enum_constr = alloca %"Opt[bool]", align 8
  %enum_tag_Some = getelementptr inbounds %"Opt[bool].Some", ptr %enum_constr, i32 0, i32 0
  store i8 1, ptr %enum_tag_Some, align 1
  %value_loaded = load i8, ptr %value, align 1
  %enum_payload_Some = getelementptr inbounds %"Opt[bool].Some", ptr %enum_constr, i32 0, i32 1
  store i8 %value_loaded, ptr %enum_payload_Some, align 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %sret_ptr, ptr align 1 %enum_constr, i64 2, i1 false)
  ret void
}

define void @lambdas.someFnParamGen_spec_bool_1(ptr sret(%"Opt[bool]") %sret_ptr, i8 %t_arg, ptr %f_arg, ptr %g_arg) {
entry:
  %t = alloca i8, align 1
  store i8 %t_arg, ptr %t, align 1
  %f = alloca { ptr, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %f, ptr align 8 %f_arg, i64 16, i1 false)
  %g = alloca { ptr, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %g, ptr align 8 %g_arg, i64 16, i1 false)
  %t_loaded = load i8, ptr %t, align 1
  %0 = call i8 @"lambdas.lambdas__test_{lambda}_1185"(ptr %f, i8 %t_loaded)
  %call_sret = alloca %"Opt[bool]", align 8
  call void @"lambdas.lambdas__test_{lambda}_1186"(ptr sret(%"Opt[bool]") %call_sret, ptr %g, i8 %0)
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %sret_ptr, ptr align 1 %call_sret, i64 2, i1 false)
  ret void
}

define i8 @"Equals_Opt[bool]_equals"(ptr %self_arg, ptr %other_arg) {
entry:
  %self = alloca %"Opt[bool]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %self, ptr align 1 %self_arg, i64 2, i1 false)
  %other = alloca %"Opt[bool]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %other, ptr align 1 %other_arg, i64 2, i1 false)
  %0 = call i8 @Opt.opt_equals_spec_bool_2(ptr %self, ptr %other)
  ret i8 %0
}

define i8 @Opt.opt_equals_spec_bool_2(ptr %a_arg, ptr %b_arg) {
entry:
  %a = alloca %"Opt[bool]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %a, ptr align 1 %a_arg, i64 2, i1 false)
  %b = alloca %"Opt[bool]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %b, ptr align 1 %b_arg, i64 2, i1 false)
  br label %arm_0

arm_0:                                            ; preds = %entry
  %__if_target_1630 = alloca %"Opt[bool]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %__if_target_1630, ptr align 1 %a, i64 2, i1 false)
  %get_payload_ptr = getelementptr inbounds %"Opt[bool].Some", ptr %__if_target_1630, i32 0, i32 1
  %payload_by_value = load i8, ptr %get_payload_ptr, align 1
  %__payload_Some_1631 = alloca i8, align 1
  store i8 %payload_by_value, ptr %__payload_Some_1631, align 1
  %tag = load i8, ptr %__if_target_1630, align 1
  %is_variant_cmp = icmp eq i8 %tag, 1
  %is_variant_Some = sext i1 %is_variant_cmp to i8
  %arm_pattern_i1 = trunc i8 %is_variant_Some to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %__payload_Some_1631_loaded = load i8, ptr %__payload_Some_1631, align 1
  %at = alloca i8, align 1
  store i8 %__payload_Some_1631_loaded, ptr %at, align 1
  br label %arm_01

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  %__match_subject_1636 = alloca %"Opt[bool]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %__match_subject_1636, ptr align 1 %b, i64 2, i1 false)
  br label %arm_014

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %match_end19, %match_end6
  %match_result = phi i8 [ %match_result7, %match_end6 ], [ %match_result20, %match_end19 ]
  ret i8 %match_result

arm_01:                                           ; preds = %arm_cons_0
  %__if_target_1633 = alloca %"Opt[bool]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %__if_target_1633, ptr align 1 %b, i64 2, i1 false)
  %get_payload_ptr8 = getelementptr inbounds %"Opt[bool].Some", ptr %__if_target_1633, i32 0, i32 1
  %payload_by_value9 = load i8, ptr %get_payload_ptr8, align 1
  %__payload_Some_1634 = alloca i8, align 1
  store i8 %payload_by_value9, ptr %__payload_Some_1634, align 1
  %tag10 = load i8, ptr %__if_target_1633, align 1
  %is_variant_cmp11 = icmp eq i8 %tag10, 1
  %is_variant_Some12 = sext i1 %is_variant_cmp11 to i8
  %arm_pattern_i113 = trunc i8 %is_variant_Some12 to i1
  br i1 %arm_pattern_i113, label %arm_cons_02, label %arm_13

arm_cons_02:                                      ; preds = %arm_01
  %__payload_Some_1634_loaded = load i8, ptr %__payload_Some_1634, align 1
  %bt = alloca i8, align 1
  store i8 %__payload_Some_1634_loaded, ptr %bt, align 1
  %at_loaded = load i8, ptr %at, align 1
  %bt_loaded = load i8, ptr %bt, align 1
  %"==_i1" = icmp eq i8 %at_loaded, %bt_loaded
  %"==_res" = sext i1 %"==_i1" to i8
  br label %match_end6

arm_13:                                           ; preds = %arm_01
  br i1 true, label %arm_cons_14, label %match_fail5

arm_cons_14:                                      ; preds = %arm_13
  br label %match_end6

match_fail5:                                      ; preds = %arm_13
  unreachable

match_end6:                                       ; preds = %arm_cons_14, %arm_cons_02
  %match_result7 = phi i8 [ %"==_res", %arm_cons_02 ], [ 0, %arm_cons_14 ]
  br label %match_end

arm_014:                                          ; preds = %arm_cons_1
  %tag21 = load i8, ptr %__match_subject_1636, align 1
  %is_variant_cmp22 = icmp eq i8 %tag21, 0
  %is_variant_None = sext i1 %is_variant_cmp22 to i8
  %arm_pattern_i123 = trunc i8 %is_variant_None to i1
  br i1 %arm_pattern_i123, label %arm_cons_015, label %arm_116

arm_cons_015:                                     ; preds = %arm_014
  br label %match_end19

arm_116:                                          ; preds = %arm_014
  br i1 true, label %arm_cons_117, label %arm_2

arm_cons_117:                                     ; preds = %arm_116
  br label %match_end19

arm_2:                                            ; preds = %arm_116
  br i1 true, label %arm_cons_2, label %match_fail18

arm_cons_2:                                       ; preds = %arm_2
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.90, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 17, ptr %line_store_addr, align 8
  call void @crash(ptr %struct_literal, ptr @str.92)
  unreachable

match_fail18:                                     ; preds = %arm_2
  unreachable

match_end19:                                      ; preds = %arm_cons_117, %arm_cons_015
  %match_result20 = phi i8 [ 1, %arm_cons_015 ], [ 0, %arm_cons_117 ]
  br label %match_end
}

define void @List.withCapacity_spec_i64_10(ptr sret({ i64, { i64, ptr } }) %sret_ptr, i64 %cap_arg) {
entry:
  %cap = alloca i64, align 8
  store i64 %cap_arg, ptr %cap, align 8
  %cap_loaded = load i64, ptr %cap, align 8
  %call_sret = alloca { i64, ptr }, align 8
  call void @Buffer._allocate_spec_i64_12(ptr sret({ i64, ptr }) %call_sret, i64 %cap_loaded)
  %buffer = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %buffer, ptr align 8 %call_sret, i64 16, i1 false)
  %struct_literal = alloca { i64, { i64, ptr } }, align 8
  %len_store_addr = getelementptr inbounds { i64, { i64, ptr } }, ptr %struct_literal, i32 0, i32 0
  store i64 0, ptr %len_store_addr, align 8
  %buffer_store_addr = getelementptr inbounds { i64, { i64, ptr } }, ptr %struct_literal, i32 0, i32 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %buffer_store_addr, ptr align 8 %buffer, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %struct_literal, i64 24, i1 false)
  ret void
}

define void @Buffer._allocate_spec_i64_12(ptr sret({ i64, ptr }) %sret_ptr, i64 %count_arg) {
entry:
  %count = alloca i64, align 8
  store i64 %count_arg, ptr %count, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %count_loaded = load i64, ptr %count, align 8
  %"==_i1" = icmp eq i64 %count_loaded, 0
  %"==_res" = sext i1 %"==_i1" to i8
  %arm_pattern_i1 = trunc i8 %"==_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %0 = call ptr @Pointer.null()
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  %count_loaded1 = load i64, ptr %count, align 8
  %1 = call i64 @Buffer.bufferLenBytes_spec_i64_6(i64 %count_loaded1)
  %sizeBytes = alloca i64, align 8
  store i64 %1, ptr %sizeBytes, align 8
  %sizeBytes_loaded = load i64, ptr %sizeBytes, align 8
  %2 = call ptr @malloc(i64 %sizeBytes_loaded)
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1, %arm_cons_0
  %match_result = phi ptr [ %0, %arm_cons_0 ], [ %2, %arm_cons_1 ]
  %ptr = alloca ptr, align 8
  store ptr %match_result, ptr %ptr, align 8
  %ptr_loaded = load ptr, ptr %ptr, align 8
  %data = alloca ptr, align 8
  store ptr %ptr_loaded, ptr %data, align 8
  %struct_literal = alloca { i64, ptr }, align 8
  %count_loaded2 = load i64, ptr %count, align 8
  %len_store_addr = getelementptr inbounds { i64, ptr }, ptr %struct_literal, i32 0, i32 0
  store i64 %count_loaded2, ptr %len_store_addr, align 8
  %data_loaded = load ptr, ptr %data, align 8
  %data_store_addr = getelementptr inbounds { i64, ptr }, ptr %struct_literal, i32 0, i32 1
  store ptr %data_loaded, ptr %data_store_addr, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %struct_literal, i64 16, i1 false)
  ret void
}

define i64 @Buffer.bufferLenBytes_spec_i64_6(i64 %count_arg) {
entry:
  %count = alloca i64, align 8
  store i64 %count_arg, ptr %count, align 8
  %count_loaded = load i64, ptr %count, align 8
  %mul = mul i64 %count_loaded, 8
  ret i64 %mul
}

define i8 @List.push_spec_i64_11(ptr %self_arg, i64 %elem_arg) {
entry:
  %self = alloca ptr, align 8
  store ptr %self_arg, ptr %self, align 8
  %elem = alloca i64, align 8
  store i64 %elem_arg, ptr %elem, align 8
  %self_loaded = load ptr, ptr %self, align 8
  %struc.len = getelementptr inbounds { i64, { i64, ptr } }, ptr %self_loaded, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %startLength = alloca i64, align 8
  store i64 %struc.len1, ptr %startLength, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %startLength_loaded = load i64, ptr %startLength, align 8
  %self_loaded2 = load ptr, ptr %self, align 8
  %0 = call i64 @List.cap_spec_i64_10(ptr %self_loaded2)
  %"==_i1" = icmp eq i64 %startLength_loaded, %0
  %"==_res" = sext i1 %"==_i1" to i8
  %arm_pattern_i1 = trunc i8 %"==_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %self_loaded3 = load ptr, ptr %self, align 8
  %1 = call i8 @List._grow_spec_i64_8(ptr %self_loaded3)
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1, %arm_cons_0
  %match_result = phi i8 [ %1, %arm_cons_0 ], [ 0, %arm_cons_1 ]
  %startLength_loaded4 = load i64, ptr %startLength, align 8
  %add = add i64 %startLength_loaded4, 1
  %self_loaded5 = load ptr, ptr %self, align 8
  %struc.len6 = getelementptr inbounds { i64, { i64, ptr } }, ptr %self_loaded5, i32 0, i32 0
  store i64 %add, ptr %struc.len6, align 8
  %self_loaded7 = load ptr, ptr %self, align 8
  %startLength_loaded8 = load i64, ptr %startLength, align 8
  %elem_loaded = load i64, ptr %elem, align 8
  %2 = call i8 @List.set_spec_i64_9(ptr %self_loaded7, i64 %startLength_loaded8, i64 %elem_loaded)
  ret i8 0
}

define i64 @List.cap_spec_i64_10(ptr %self_arg) {
entry:
  %self = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 24, i1 false)
  %struc.buffer = getelementptr inbounds { i64, { i64, ptr } }, ptr %self, i32 0, i32 1
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %struc.buffer, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  ret i64 %struc.len1
}

define i8 @List._grow_spec_i64_8(ptr %self_arg) {
entry:
  %self = alloca ptr, align 8
  store ptr %self_arg, ptr %self, align 8
  %elemSize = alloca i64, align 8
  store i64 8, ptr %elemSize, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %self_loaded = load ptr, ptr %self, align 8
  %struc.buffer = getelementptr inbounds { i64, { i64, ptr } }, ptr %self_loaded, i32 0, i32 1
  %0 = call i8 @Buffer.isEmpty_spec_i64_10(ptr %struc.buffer)
  %arm_pattern_i1 = trunc i8 %0 to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  br label %arm_01

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  %self_loaded9 = load ptr, ptr %self, align 8
  %1 = call i64 @List.cap_spec_i64_10(ptr %self_loaded9)
  %mul = mul i64 %1, 2
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1, %match_end6
  %match_result = phi i64 [ %match_result7, %match_end6 ], [ %mul, %arm_cons_1 ]
  %newCap = alloca i64, align 8
  store i64 %match_result, ptr %newCap, align 8
  %self_loaded10 = load ptr, ptr %self, align 8
  %newCap_loaded = load i64, ptr %newCap, align 8
  %2 = call i8 @List.reserve_spec_i64_8(ptr %self_loaded10, i64 %newCap_loaded)
  ret i8 0

arm_01:                                           ; preds = %arm_cons_0
  %elemSize_loaded = load i64, ptr %elemSize, align 8
  %">=_i1" = icmp sge i64 %elemSize_loaded, 1024
  %">=_res" = sext i1 %">=_i1" to i8
  %arm_pattern_i18 = trunc i8 %">=_res" to i1
  br i1 %arm_pattern_i18, label %arm_cons_02, label %arm_13

arm_cons_02:                                      ; preds = %arm_01
  br label %match_end6

arm_13:                                           ; preds = %arm_01
  br i1 true, label %arm_cons_14, label %match_fail5

arm_cons_14:                                      ; preds = %arm_13
  br label %match_end6

match_fail5:                                      ; preds = %arm_13
  unreachable

match_end6:                                       ; preds = %arm_cons_14, %arm_cons_02
  %match_result7 = phi i64 [ 1, %arm_cons_02 ], [ 8, %arm_cons_14 ]
  br label %match_end
}

define i8 @Buffer.isEmpty_spec_i64_10(ptr %self_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %"==_i1" = icmp eq i64 %struc.len1, 0
  %"==_res" = sext i1 %"==_i1" to i8
  ret i8 %"==_res"
}

define i8 @List.reserve_spec_i64_8(ptr %self_arg, i64 %count_arg) {
entry:
  %self = alloca ptr, align 8
  store ptr %self_arg, ptr %self, align 8
  %count = alloca i64, align 8
  store i64 %count_arg, ptr %count, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %self_loaded = load ptr, ptr %self, align 8
  %0 = call i64 @List.cap_spec_i64_10(ptr %self_loaded)
  %count_loaded = load i64, ptr %count, align 8
  %">=_i1" = icmp sge i64 %0, %count_loaded
  %">=_res" = sext i1 %">=_i1" to i8
  %arm_pattern_i1 = trunc i8 %">=_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  ret i8 0

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1
  %match_result = phi i8 [ 0, %arm_cons_1 ]
  %self_loaded1 = load ptr, ptr %self, align 8
  %struc.buffer = getelementptr inbounds { i64, { i64, ptr } }, ptr %self_loaded1, i32 0, i32 1
  %count_loaded2 = load i64, ptr %count, align 8
  %call_sret = alloca { i64, ptr }, align 8
  call void @Buffer._enlargedClone_spec_i64_9(ptr sret({ i64, ptr }) %call_sret, ptr %struc.buffer, i64 %count_loaded2)
  %newBuffer = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %newBuffer, ptr align 8 %call_sret, i64 16, i1 false)
  %self_loaded3 = load ptr, ptr %self, align 8
  %struc.buffer4 = getelementptr inbounds { i64, { i64, ptr } }, ptr %self_loaded3, i32 0, i32 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %struc.buffer4, ptr align 8 %newBuffer, i64 16, i1 false)
  ret i8 0
}

define void @Buffer._enlargedClone_spec_i64_9(ptr sret({ i64, ptr }) %sret_ptr, ptr %self_arg, i64 %newCount_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %newCount = alloca i64, align 8
  store i64 %newCount_arg, ptr %newCount, align 8
  %newCount_loaded = load i64, ptr %newCount, align 8
  %call_sret = alloca { i64, ptr }, align 8
  call void @Buffer._allocate_spec_i64_12(ptr sret({ i64, ptr }) %call_sret, i64 %newCount_loaded)
  %newBuffer = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %newBuffer, ptr align 8 %call_sret, i64 16, i1 false)
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %0 = call i8 @Buffer._copyFrom_spec_i64_9(ptr %self, ptr %newBuffer, i64 %struc.len1)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %newBuffer, i64 16, i1 false)
  ret void
}

define i8 @Buffer._copyFrom_spec_i64_9(ptr %source_arg, ptr %dest_arg, i64 %count_arg) {
entry:
  %source = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %source, ptr align 8 %source_arg, i64 16, i1 false)
  %dest = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %dest, ptr align 8 %dest_arg, i64 16, i1 false)
  %count = alloca i64, align 8
  store i64 %count_arg, ptr %count, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %count_loaded = load i64, ptr %count, align 8
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %source, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %">_i1" = icmp sgt i64 %count_loaded, %struc.len1
  %">_res" = sext i1 %">_i1" to i8
  %arm_pattern_i1 = trunc i8 %">_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.94, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 36, ptr %line_store_addr, align 8
  %call_sret = alloca { ptr }, align 8
  call void @StringBuilder.withCapacity(ptr sret({ ptr }) %call_sret, i64 2)
  %__sb_1767 = alloca { ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__sb_1767, ptr align 8 %call_sret, i64 8, i1 false)
  %0 = call i8 @StringBuilder.putString(ptr %__sb_1767, ptr @str.96)
  %count_loaded2 = load i64, ptr %count, align 8
  %call_sret3 = alloca { { i64, ptr } }, align 8
  call void @Show_u64_show(ptr sret({ { i64, ptr } }) %call_sret3, i64 %count_loaded2)
  %1 = call i8 @StringBuilder.putString(ptr %__sb_1767, ptr %call_sret3)
  %call_sret4 = alloca { { i64, ptr } }, align 8
  call void @StringBuilder.build(ptr sret({ { i64, ptr } }) %call_sret4, ptr %__sb_1767)
  call void @crash(ptr %struct_literal, ptr %call_sret4)
  unreachable

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1
  %match_result = phi i8 [ 0, %arm_cons_1 ]
  br label %arm_05

arm_05:                                           ; preds = %match_end
  %count_loaded12 = load i64, ptr %count, align 8
  %struc.len13 = getelementptr inbounds { i64, ptr }, ptr %dest, i32 0, i32 0
  %struc.len14 = load i64, ptr %struc.len13, align 8
  %">_i115" = icmp sgt i64 %count_loaded12, %struc.len14
  %">_res16" = sext i1 %">_i115" to i8
  %arm_pattern_i117 = trunc i8 %">_res16" to i1
  br i1 %arm_pattern_i117, label %arm_cons_06, label %arm_17

arm_cons_06:                                      ; preds = %arm_05
  %struct_literal18 = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr19 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal18, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr19, ptr align 8 @str.98, i64 16, i1 false)
  %line_store_addr20 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal18, i32 0, i32 1
  store i64 39, ptr %line_store_addr20, align 8
  %call_sret21 = alloca { ptr }, align 8
  call void @StringBuilder.withCapacity(ptr sret({ ptr }) %call_sret21, i64 2)
  %__sb_1768 = alloca { ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__sb_1768, ptr align 8 %call_sret21, i64 8, i1 false)
  %2 = call i8 @StringBuilder.putString(ptr %__sb_1768, ptr @str.100)
  %count_loaded22 = load i64, ptr %count, align 8
  %call_sret23 = alloca { { i64, ptr } }, align 8
  call void @Show_u64_show(ptr sret({ { i64, ptr } }) %call_sret23, i64 %count_loaded22)
  %3 = call i8 @StringBuilder.putString(ptr %__sb_1768, ptr %call_sret23)
  %call_sret24 = alloca { { i64, ptr } }, align 8
  call void @StringBuilder.build(ptr sret({ { i64, ptr } }) %call_sret24, ptr %__sb_1768)
  call void @crash(ptr %struct_literal18, ptr %call_sret24)
  unreachable

arm_17:                                           ; preds = %arm_05
  br i1 true, label %arm_cons_18, label %match_fail9

arm_cons_18:                                      ; preds = %arm_17
  br label %match_end10

match_fail9:                                      ; preds = %arm_17
  unreachable

match_end10:                                      ; preds = %arm_cons_18
  %match_result11 = phi i8 [ 0, %arm_cons_18 ]
  %count_loaded25 = load i64, ptr %count, align 8
  %4 = call i64 @Buffer.bufferLenBytes_spec_i64_6(i64 %count_loaded25)
  %numBytes = alloca i64, align 8
  store i64 %4, ptr %numBytes, align 8
  %5 = call ptr @Buffer.dataPointer_spec_i64_6(ptr %dest)
  %6 = call ptr @Buffer.dataPointer_spec_i64_6(ptr %source)
  %numBytes_loaded = load i64, ptr %numBytes, align 8
  %7 = call ptr @memcpy(ptr %5, ptr %6, i64 %numBytes_loaded)
  ret i8 0
}

define ptr @Buffer.dataPointer_spec_i64_6(ptr %self_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %struc.data = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 1
  %struc.data1 = load ptr, ptr %struc.data, align 8
  ret ptr %struc.data1
}

define i8 @List.set_spec_i64_9(ptr %self_arg, i64 %index_arg, i64 %elem_arg) {
entry:
  %self = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 24, i1 false)
  %index = alloca i64, align 8
  store i64 %index_arg, ptr %index, align 8
  %elem = alloca i64, align 8
  store i64 %elem_arg, ptr %elem, align 8
  %struc.buffer = getelementptr inbounds { i64, { i64, ptr } }, ptr %self, i32 0, i32 1
  %index_loaded = load i64, ptr %index, align 8
  %elem_loaded = load i64, ptr %elem, align 8
  %0 = call i8 @Buffer.set_spec_i64_10(ptr %struc.buffer, i64 %index_loaded, i64 %elem_loaded)
  ret i8 %0
}

define i8 @Buffer.set_spec_i64_10(ptr %self_arg, i64 %index_arg, i64 %elem_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %index = alloca i64, align 8
  store i64 %index_arg, ptr %index, align 8
  %elem = alloca i64, align 8
  store i64 %elem_arg, ptr %elem, align 8
  %index_loaded = load i64, ptr %index, align 8
  %0 = call ptr @Buffer.getRef_spec_i64_13(ptr %self, i64 %index_loaded)
  %tRef = alloca ptr, align 8
  store ptr %0, ptr %tRef, align 8
  %elem_loaded = load i64, ptr %elem, align 8
  %tRef_loaded = load ptr, ptr %tRef, align 8
  store i64 %elem_loaded, ptr %tRef_loaded, align 8
  ret i8 0
}

define ptr @Buffer.getRef_spec_i64_13(ptr %self_arg, i64 %index_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %index = alloca i64, align 8
  store i64 %index_arg, ptr %index, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %index_loaded = load i64, ptr %index, align 8
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %">=_i1" = icmp sge i64 %index_loaded, %struc.len1
  %">=_res" = sext i1 %">=_i1" to i8
  %arm_pattern_i1 = trunc i8 %">=_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.102, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 87, ptr %line_store_addr, align 8
  call void @crash(ptr %struct_literal, ptr @str.104)
  unreachable

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1
  %match_result = phi i8 [ 0, %arm_cons_1 ]
  %0 = call ptr @Buffer.dataPointer_spec_i64_6(ptr %self)
  %index_loaded2 = load i64, ptr %index, align 8
  %refAtIndex = getelementptr inbounds i64, ptr %0, i64 %index_loaded2
  ret ptr %refAtIndex
}

define i64 @"lambdas.lambdas__test_{lambda}_1199"(ptr %__lambda_env_arg, i64 %x_arg) {
entry:
  %__lambda_env = alloca ptr, align 8
  store ptr %__lambda_env_arg, ptr %__lambda_env, align 8
  %x = alloca i64, align 8
  store i64 %x_arg, ptr %x, align 8
  %x_loaded = load i64, ptr %x, align 8
  %add = add i64 %x_loaded, 1
  ret i64 %add
}

define void @lambdas.map_spec_i64i64_1(ptr sret({ i64, { i64, ptr } }) %sret_ptr, ptr %a_arg, ptr %f_arg) {
entry:
  %a = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %a, ptr align 8 %a_arg, i64 24, i1 false)
  %f = alloca { ptr, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %f, ptr align 8 %f_arg, i64 16, i1 false)
  %struc.len = getelementptr inbounds { i64, { i64, ptr } }, ptr %a, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %call_sret = alloca { i64, { i64, ptr } }, align 8
  call void @List.withCapacity_spec_i64_10(ptr sret({ i64, { i64, ptr } }) %call_sret, i64 %struc.len1)
  %us = alloca ptr, align 8
  store ptr %call_sret, ptr %us, align 8
  %itIndex = alloca i64, align 8
  store i64 0, ptr %itIndex, align 8
  %call_sret2 = alloca { { i64, ptr }, i64 }, align 8
  call void @"Iterable_List[i64]_iterator"(ptr sret({ { i64, ptr }, i64 }) %call_sret2, ptr %a)
  %__iter_1459 = alloca ptr, align 8
  store ptr %call_sret2, ptr %__iter_1459, align 8
  %break = alloca i8, align 1
  br label %loop_body

loop_body:                                        ; preds = %if_merge, %entry
  %__iter_1459_loaded = load ptr, ptr %__iter_1459, align 8
  %call_sret3 = alloca %"Opt[i64]", align 8
  call void @"Iterator_BufferIterator[i64]_next"(ptr sret(%"Opt[i64]") %call_sret3, ptr %__iter_1459_loaded)
  %__next_1466 = alloca %"Opt[i64]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__next_1466, ptr align 8 %call_sret3, i64 16, i1 false)
  %0 = call i8 @Opt.isSome_spec_i64_12(ptr %__next_1466)
  %cond_i1 = trunc i8 %0 to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

loop_end:                                         ; preds = %if_alt
  %loop_value = load i8, ptr %break, align 1
  %us_loaded4 = load ptr, ptr %us, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %us_loaded4, i64 24, i1 false)
  ret void

if_cons:                                          ; preds = %loop_body
  %1 = call i64 @"Unwrap_Opt[i64]_unwrap"(ptr %__next_1466)
  %i = alloca i64, align 8
  store i64 %1, ptr %i, align 8
  %us_loaded = load ptr, ptr %us, align 8
  %i_loaded = load i64, ptr %i, align 8
  %2 = call i64 @"lambdas.lambdas__test_{lambda}_1199"(ptr %f, i64 %i_loaded)
  %3 = call i8 @List.push_spec_i64_11(ptr %us_loaded, i64 %2)
  %__block_expr_val_1471 = alloca i8, align 1
  store i8 %3, ptr %__block_expr_val_1471, align 1
  br label %if_merge

if_alt:                                           ; preds = %loop_body
  store i8 0, ptr %break, align 1
  br label %loop_end

if_merge:                                         ; preds = %if_cons
  %if_phi = phi i8 [ 0, %if_cons ]
  %itIndex_loaded = load i64, ptr %itIndex, align 8
  %add = add i64 %itIndex_loaded, 1
  store i64 %add, ptr %itIndex, align 8
  br label %loop_body
}

define void @"Iterable_List[i64]_iterator"(ptr sret({ { i64, ptr }, i64 }) %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 24, i1 false)
  %call_sret = alloca { i64, ptr }, align 8
  call void @List.toBuffer_spec_i64_5(ptr sret({ i64, ptr }) %call_sret, ptr %self)
  %call_sret1 = alloca { { i64, ptr }, i64 }, align 8
  call void @"Iterable_Buffer[i64]_iterator"(ptr sret({ { i64, ptr }, i64 }) %call_sret1, ptr %call_sret)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret1, i64 24, i1 false)
  ret void
}

define void @List.toBuffer_spec_i64_5(ptr sret({ i64, ptr }) %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 24, i1 false)
  %struc.buffer = getelementptr inbounds { i64, { i64, ptr } }, ptr %self, i32 0, i32 1
  %struc.len = getelementptr inbounds { i64, { i64, ptr } }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %call_sret = alloca { i64, ptr }, align 8
  call void @Buffer.slice_spec_i64_9(ptr sret({ i64, ptr }) %call_sret, ptr %struc.buffer, i64 0, i64 %struc.len1)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret, i64 16, i1 false)
  ret void
}

define void @Buffer.slice_spec_i64_9(ptr sret({ i64, ptr }) %sret_ptr, ptr %self_arg, i64 %start_arg, i64 %end_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %start = alloca i64, align 8
  store i64 %start_arg, ptr %start, align 8
  %end = alloca i64, align 8
  store i64 %end_arg, ptr %end, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %end_loaded = load i64, ptr %end, align 8
  %start_loaded = load i64, ptr %start, align 8
  %"<_i1" = icmp slt i64 %end_loaded, %start_loaded
  %"<_res" = sext i1 %"<_i1" to i8
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %"==_i1" = icmp eq i64 %struc.len1, 0
  %"==_res" = sext i1 %"==_i1" to i8
  %bool_or = or i8 %"<_res", %"==_res"
  %arm_pattern_i1 = trunc i8 %bool_or to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %call_sret = alloca { i64, ptr }, align 8
  call void @Buffer.empty_spec_i64_6(ptr sret({ i64, ptr }) %call_sret)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret, i64 16, i1 false)
  ret void

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1
  %match_result = phi i8 [ 0, %arm_cons_1 ]
  br label %arm_02

arm_02:                                           ; preds = %match_end
  %start_loaded9 = load i64, ptr %start, align 8
  %struc.len10 = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len11 = load i64, ptr %struc.len10, align 8
  %">=_i1" = icmp sge i64 %start_loaded9, %struc.len11
  %">=_res" = sext i1 %">=_i1" to i8
  %start_loaded12 = load i64, ptr %start, align 8
  %"<_i113" = icmp slt i64 %start_loaded12, 0
  %"<_res14" = sext i1 %"<_i113" to i8
  %bool_or15 = or i8 %">=_res", %"<_res14"
  %arm_pattern_i116 = trunc i8 %bool_or15 to i1
  br i1 %arm_pattern_i116, label %arm_cons_03, label %arm_14

arm_cons_03:                                      ; preds = %arm_02
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.106, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 74, ptr %line_store_addr, align 8
  %call_sret17 = alloca { ptr }, align 8
  call void @StringBuilder.withCapacity(ptr sret({ ptr }) %call_sret17, i64 2)
  %__sb_1532 = alloca { ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__sb_1532, ptr align 8 %call_sret17, i64 8, i1 false)
  %0 = call i8 @StringBuilder.putString(ptr %__sb_1532, ptr @str.108)
  %start_loaded18 = load i64, ptr %start, align 8
  %call_sret19 = alloca { { i64, ptr } }, align 8
  call void @Show_u64_show(ptr sret({ { i64, ptr } }) %call_sret19, i64 %start_loaded18)
  %1 = call i8 @StringBuilder.putString(ptr %__sb_1532, ptr %call_sret19)
  %call_sret20 = alloca { { i64, ptr } }, align 8
  call void @StringBuilder.build(ptr sret({ { i64, ptr } }) %call_sret20, ptr %__sb_1532)
  call void @crash(ptr %struct_literal, ptr %call_sret20)
  unreachable

arm_14:                                           ; preds = %arm_02
  br i1 true, label %arm_cons_15, label %match_fail6

arm_cons_15:                                      ; preds = %arm_14
  br label %match_end7

match_fail6:                                      ; preds = %arm_14
  unreachable

match_end7:                                       ; preds = %arm_cons_15
  %match_result8 = phi i8 [ 0, %arm_cons_15 ]
  br label %arm_021

arm_021:                                          ; preds = %match_end7
  %end_loaded28 = load i64, ptr %end, align 8
  %struc.len29 = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len30 = load i64, ptr %struc.len29, align 8
  %">_i1" = icmp sgt i64 %end_loaded28, %struc.len30
  %">_res" = sext i1 %">_i1" to i8
  %arm_pattern_i131 = trunc i8 %">_res" to i1
  br i1 %arm_pattern_i131, label %arm_cons_022, label %arm_123

arm_cons_022:                                     ; preds = %arm_021
  %struc.len32 = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len33 = load i64, ptr %struc.len32, align 8
  br label %match_end26

arm_123:                                          ; preds = %arm_021
  br i1 true, label %arm_cons_124, label %match_fail25

arm_cons_124:                                     ; preds = %arm_123
  %end_loaded34 = load i64, ptr %end, align 8
  br label %match_end26

match_fail25:                                     ; preds = %arm_123
  unreachable

match_end26:                                      ; preds = %arm_cons_124, %arm_cons_022
  %match_result27 = phi i64 [ %struc.len33, %arm_cons_022 ], [ %end_loaded34, %arm_cons_124 ]
  %end35 = alloca i64, align 8
  store i64 %match_result27, ptr %end35, align 8
  %end_loaded36 = load i64, ptr %end35, align 8
  %start_loaded37 = load i64, ptr %start, align 8
  %sub = sub i64 %end_loaded36, %start_loaded37
  %newLen = alloca i64, align 8
  store i64 %sub, ptr %newLen, align 8
  %start_loaded38 = load i64, ptr %start, align 8
  %2 = call ptr @Buffer.getRef_spec_i64_13(ptr %self, i64 %start_loaded38)
  %newBase = alloca ptr, align 8
  store ptr %2, ptr %newBase, align 8
  %struct_literal39 = alloca { i64, ptr }, align 8
  %newLen_loaded = load i64, ptr %newLen, align 8
  %len_store_addr = getelementptr inbounds { i64, ptr }, ptr %struct_literal39, i32 0, i32 0
  store i64 %newLen_loaded, ptr %len_store_addr, align 8
  %newBase_loaded = load ptr, ptr %newBase, align 8
  %data_store_addr = getelementptr inbounds { i64, ptr }, ptr %struct_literal39, i32 0, i32 1
  store ptr %newBase_loaded, ptr %data_store_addr, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %struct_literal39, i64 16, i1 false)
  ret void
}

define void @Buffer.empty_spec_i64_6(ptr sret({ i64, ptr }) %sret_ptr) {
entry:
  %call_sret = alloca { i64, ptr }, align 8
  call void @Buffer._allocate_spec_i64_12(ptr sret({ i64, ptr }) %call_sret, i64 0)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret, i64 16, i1 false)
  ret void
}

define void @"Iterable_Buffer[i64]_iterator"(ptr sret({ { i64, ptr }, i64 }) %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %call_sret = alloca { { i64, ptr }, i64 }, align 8
  call void @BufferIterator.fromBuffer_spec_i64_14(ptr sret({ { i64, ptr }, i64 }) %call_sret, ptr %self)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret, i64 24, i1 false)
  ret void
}

define void @BufferIterator.fromBuffer_spec_i64_14(ptr sret({ { i64, ptr }, i64 }) %sret_ptr, ptr %buf_arg) {
entry:
  %buf = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %buf, ptr align 8 %buf_arg, i64 16, i1 false)
  %struct_literal = alloca { { i64, ptr }, i64 }, align 8
  %buf_store_addr = getelementptr inbounds { { i64, ptr }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %buf_store_addr, ptr align 8 %buf, i64 16, i1 false)
  %pos_store_addr = getelementptr inbounds { { i64, ptr }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 0, ptr %pos_store_addr, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %struct_literal, i64 24, i1 false)
  ret void
}

define void @"Iterator_BufferIterator[i64]_next"(ptr sret(%"Opt[i64]") %sret_ptr, ptr %self_arg) {
entry:
  %self = alloca ptr, align 8
  store ptr %self_arg, ptr %self, align 8
  br label %arm_0

arm_0:                                            ; preds = %entry
  %self_loaded = load ptr, ptr %self, align 8
  %struc.pos = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded, i32 0, i32 1
  %struc.pos1 = load i64, ptr %struc.pos, align 8
  %self_loaded2 = load ptr, ptr %self, align 8
  %struc.buf = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded2, i32 0, i32 0
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %struc.buf, i32 0, i32 0
  %struc.len3 = load i64, ptr %struc.len, align 8
  %"<_i1" = icmp slt i64 %struc.pos1, %struc.len3
  %"<_res" = sext i1 %"<_i1" to i8
  %arm_pattern_i1 = trunc i8 %"<_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %self_loaded4 = load ptr, ptr %self, align 8
  %struc.buf5 = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded4, i32 0, i32 0
  %self_loaded6 = load ptr, ptr %self, align 8
  %struc.pos7 = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded6, i32 0, i32 1
  %struc.pos8 = load i64, ptr %struc.pos7, align 8
  %0 = call i64 @Buffer.get_spec_i64_18(ptr %struc.buf5, i64 %struc.pos8)
  %item = alloca i64, align 8
  store i64 %0, ptr %item, align 8
  %self_loaded9 = load ptr, ptr %self, align 8
  %struc.pos10 = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded9, i32 0, i32 1
  %struc.pos11 = load i64, ptr %struc.pos10, align 8
  %add = add i64 %struc.pos11, 1
  %self_loaded12 = load ptr, ptr %self, align 8
  %struc.pos13 = getelementptr inbounds { { i64, ptr }, i64 }, ptr %self_loaded12, i32 0, i32 1
  store i64 %add, ptr %struc.pos13, align 8
  %item_loaded = load i64, ptr %item, align 8
  %call_sret = alloca %"Opt[i64]", align 8
  call void @some_spec_i64_3(ptr sret(%"Opt[i64]") %call_sret, i64 %item_loaded)
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  %enum_constr = alloca %"Opt[i64]", align 8
  %enum_tag_None = getelementptr inbounds %"Opt[i64].None", ptr %enum_constr, i32 0, i32 0
  store i64 0, ptr %enum_tag_None, align 8
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1, %arm_cons_0
  %match_result = phi ptr [ %call_sret, %arm_cons_0 ], [ %enum_constr, %arm_cons_1 ]
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %match_result, i64 16, i1 false)
  ret void
}

define i64 @Buffer.get_spec_i64_18(ptr %self_arg, i64 %index_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %index = alloca i64, align 8
  store i64 %index_arg, ptr %index, align 8
  %index_loaded = load i64, ptr %index, align 8
  %0 = call ptr @Buffer.getRef_spec_i64_13(ptr %self, i64 %index_loaded)
  %tRef = alloca ptr, align 8
  store ptr %0, ptr %tRef, align 8
  %tRef_loaded = load ptr, ptr %tRef, align 8
  %deref = load i64, ptr %tRef_loaded, align 8
  ret i64 %deref
}

define void @some_spec_i64_3(ptr sret(%"Opt[i64]") %sret_ptr, i64 %value_arg) {
entry:
  %value = alloca i64, align 8
  store i64 %value_arg, ptr %value, align 8
  %enum_constr = alloca %"Opt[i64]", align 8
  %enum_tag_Some = getelementptr inbounds %"Opt[i64].Some", ptr %enum_constr, i32 0, i32 0
  store i64 1, ptr %enum_tag_Some, align 8
  %value_loaded = load i64, ptr %value, align 8
  %enum_payload_Some = getelementptr inbounds %"Opt[i64].Some", ptr %enum_constr, i32 0, i32 1
  store i64 %value_loaded, ptr %enum_payload_Some, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %enum_constr, i64 16, i1 false)
  ret void
}

define i8 @Opt.isSome_spec_i64_12(ptr %self_arg) {
entry:
  %self = alloca %"Opt[i64]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %__match_subject_1656 = alloca %"Opt[i64]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__match_subject_1656, ptr align 8 %self, i64 16, i1 false)
  br label %arm_0

arm_0:                                            ; preds = %entry
  %get_payload_ptr = getelementptr inbounds %"Opt[i64].Some", ptr %__match_subject_1656, i32 0, i32 1
  %payload_by_value = load i64, ptr %get_payload_ptr, align 8
  %__payload_Some_1657 = alloca i64, align 8
  store i64 %payload_by_value, ptr %__payload_Some_1657, align 8
  %tag = load i64, ptr %__match_subject_1656, align 8
  %is_variant_cmp = icmp eq i64 %tag, 1
  %is_variant_Some = sext i1 %is_variant_cmp to i8
  %arm_pattern_i1 = trunc i8 %is_variant_Some to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %arm_2

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

arm_2:                                            ; preds = %arm_1
  br i1 true, label %arm_cons_2, label %match_fail

arm_cons_2:                                       ; preds = %arm_2
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.110, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 2, ptr %line_store_addr, align 8
  call void @crash(ptr %struct_literal, ptr @str.112)
  unreachable

match_fail:                                       ; preds = %arm_2
  unreachable

match_end:                                        ; preds = %arm_cons_1, %arm_cons_0
  %match_result = phi i8 [ 1, %arm_cons_0 ], [ 0, %arm_cons_1 ]
  ret i8 %match_result
}

define i64 @"Unwrap_Opt[i64]_unwrap"(ptr %self_arg) {
entry:
  %self = alloca %"Opt[i64]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %0 = call i64 @Opt.get_spec_i64_17(ptr %self)
  ret i64 %0
}

define i64 @Opt.get_spec_i64_17(ptr %self_arg) {
entry:
  %self = alloca %"Opt[i64]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  br label %arm_0

arm_0:                                            ; preds = %entry
  %__if_target_1717 = alloca %"Opt[i64]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__if_target_1717, ptr align 8 %self, i64 16, i1 false)
  %get_payload_ptr = getelementptr inbounds %"Opt[i64].Some", ptr %__if_target_1717, i32 0, i32 1
  %payload_by_value = load i64, ptr %get_payload_ptr, align 8
  %__payload_Some_1718 = alloca i64, align 8
  store i64 %payload_by_value, ptr %__payload_Some_1718, align 8
  %tag = load i64, ptr %__if_target_1717, align 8
  %is_variant_cmp = icmp eq i64 %tag, 1
  %is_variant_Some = sext i1 %is_variant_cmp to i8
  %arm_pattern_i1 = trunc i8 %is_variant_Some to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %__payload_Some_1718_loaded = load i64, ptr %__payload_Some_1718, align 8
  %t = alloca i64, align 8
  store i64 %__payload_Some_1718_loaded, ptr %t, align 8
  %t_loaded = load i64, ptr %t, align 8
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  %struct_literal = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.114, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal, i32 0, i32 1
  store i64 7, ptr %line_store_addr, align 8
  call void @crash(ptr %struct_literal, ptr @str.116)
  unreachable

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_0
  %match_result = phi i64 [ %t_loaded, %arm_cons_0 ]
  ret i64 %match_result
}

define i8 @"lambdas.lambdas__test_{lambda}_1204"(ptr %__lambda_env_arg, i64 %x_arg) {
entry:
  %__lambda_env = alloca ptr, align 8
  store ptr %__lambda_env_arg, ptr %__lambda_env, align 8
  %x = alloca i64, align 8
  store i64 %x_arg, ptr %x, align 8
  %x_loaded = load i64, ptr %x, align 8
  %srem = srem i64 %x_loaded, 2
  %"==_i1" = icmp eq i64 %srem, 0
  %"==_res" = sext i1 %"==_i1" to i8
  ret i8 %"==_res"
}

define void @lambdas.filter_spec_i64_1(ptr sret({ i64, { i64, ptr } }) %sret_ptr, ptr %a_arg, ptr %f_arg) {
entry:
  %a = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %a, ptr align 8 %a_arg, i64 24, i1 false)
  %f = alloca { ptr, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %f, ptr align 8 %f_arg, i64 16, i1 false)
  %call_sret = alloca { i64, { i64, ptr } }, align 8
  call void @List.empty_spec_i64_4(ptr sret({ i64, { i64, ptr } }) %call_sret)
  %keep = alloca ptr, align 8
  store ptr %call_sret, ptr %keep, align 8
  %itIndex = alloca i64, align 8
  store i64 0, ptr %itIndex, align 8
  %call_sret1 = alloca { { i64, ptr }, i64 }, align 8
  call void @"Iterable_List[i64]_iterator"(ptr sret({ { i64, ptr }, i64 }) %call_sret1, ptr %a)
  %__iter_1475 = alloca ptr, align 8
  store ptr %call_sret1, ptr %__iter_1475, align 8
  %break = alloca i8, align 1
  br label %loop_body

loop_body:                                        ; preds = %if_merge, %entry
  %__iter_1475_loaded = load ptr, ptr %__iter_1475, align 8
  %call_sret2 = alloca %"Opt[i64]", align 8
  call void @"Iterator_BufferIterator[i64]_next"(ptr sret(%"Opt[i64]") %call_sret2, ptr %__iter_1475_loaded)
  %__next_1476 = alloca %"Opt[i64]", align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %__next_1476, ptr align 8 %call_sret2, i64 16, i1 false)
  %0 = call i8 @Opt.isSome_spec_i64_12(ptr %__next_1476)
  %cond_i1 = trunc i8 %0 to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

loop_end:                                         ; preds = %if_alt
  %loop_value = load i8, ptr %break, align 1
  %keep_loaded4 = load ptr, ptr %keep, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %keep_loaded4, i64 24, i1 false)
  ret void

if_cons:                                          ; preds = %loop_body
  %1 = call i64 @"Unwrap_Opt[i64]_unwrap"(ptr %__next_1476)
  %i = alloca i64, align 8
  store i64 %1, ptr %i, align 8
  br label %arm_0

if_alt:                                           ; preds = %loop_body
  store i8 0, ptr %break, align 1
  br label %loop_end

if_merge:                                         ; preds = %match_end
  %if_phi = phi i8 [ 0, %match_end ]
  %itIndex_loaded = load i64, ptr %itIndex, align 8
  %add = add i64 %itIndex_loaded, 1
  store i64 %add, ptr %itIndex, align 8
  br label %loop_body

arm_0:                                            ; preds = %if_cons
  %i_loaded = load i64, ptr %i, align 8
  %2 = call i8 @"lambdas.lambdas__test_{lambda}_1204"(ptr %f, i64 %i_loaded)
  %arm_pattern_i1 = trunc i8 %2 to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  %keep_loaded = load ptr, ptr %keep, align 8
  %i_loaded3 = load i64, ptr %i, align 8
  %3 = call i8 @List.push_spec_i64_11(ptr %keep_loaded, i64 %i_loaded3)
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1, %arm_cons_0
  %match_result = phi i8 [ %3, %arm_cons_0 ], [ 0, %arm_cons_1 ]
  %__block_expr_val_1478 = alloca i8, align 1
  store i8 %match_result, ptr %__block_expr_val_1478, align 1
  br label %if_merge
}

define void @List.empty_spec_i64_4(ptr sret({ i64, { i64, ptr } }) %sret_ptr) {
entry:
  %call_sret = alloca { i64, { i64, ptr } }, align 8
  call void @List.withCapacity_spec_i64_10(ptr sret({ i64, { i64, ptr } }) %call_sret, i64 0)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret, i64 24, i1 false)
  ret void
}

define i8 @"Equals_List[i64]_equals"(ptr %self_arg, ptr %other_arg) {
entry:
  %self = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 24, i1 false)
  %other = alloca { i64, { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %other, ptr align 8 %other_arg, i64 24, i1 false)
  %call_sret = alloca { i64, ptr }, align 8
  call void @List.toBuffer_spec_i64_5(ptr sret({ i64, ptr }) %call_sret, ptr %self)
  %call_sret1 = alloca { i64, ptr }, align 8
  call void @List.toBuffer_spec_i64_5(ptr sret({ i64, ptr }) %call_sret1, ptr %other)
  %0 = call i8 @"Equals_Buffer[i64]_equals"(ptr %call_sret, ptr %call_sret1)
  ret i8 %0
}

define i8 @"Equals_Buffer[i64]_equals"(ptr %self_arg, ptr %other_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %other = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %other, ptr align 8 %other_arg, i64 16, i1 false)
  br label %arm_0

arm_0:                                            ; preds = %entry
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %struc.len2 = getelementptr inbounds { i64, ptr }, ptr %other, i32 0, i32 0
  %struc.len3 = load i64, ptr %struc.len2, align 8
  %"!=_i1" = icmp ne i64 %struc.len1, %struc.len3
  %"!=_res" = sext i1 %"!=_i1" to i8
  %arm_pattern_i1 = trunc i8 %"!=_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  %0 = call ptr @Buffer.dataPointer_spec_i64_6(ptr %self)
  %1 = call ptr @Buffer.dataPointer_spec_i64_6(ptr %other)
  %2 = call i64 @Buffer.lenBytes_spec_i64_4(ptr %self)
  %3 = call i32 @memcmp(ptr %0, ptr %1, i64 %2)
  %"==_i1" = icmp eq i32 %3, 0
  %"==_res" = sext i1 %"==_i1" to i8
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1, %arm_cons_0
  %match_result = phi i8 [ 0, %arm_cons_0 ], [ %"==_res", %arm_cons_1 ]
  ret i8 %match_result
}

define i64 @Buffer.lenBytes_spec_i64_4(ptr %self_arg) {
entry:
  %self = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %self, i32 0, i32 0
  %struc.len1 = load i64, ptr %struc.len, align 8
  %0 = call i64 @Buffer.bufferLenBytes_spec_i64_6(i64 %struc.len1)
  ret i64 %0
}

declare i32 @memcmp(ptr, ptr, i64)

define i8 @"lambdas.lambdas__test_{lambda}_1214"(ptr %__lambda_env_arg) {
entry:
  %__lambda_env = alloca ptr, align 8
  store ptr %__lambda_env_arg, ptr %__lambda_env, align 8
  %0 = call i8 @println(ptr @str.120)
  ret i8 %0
}

define i8 @println(ptr %s_arg) {
entry:
  %s = alloca { { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %s, ptr align 8 %s_arg, i64 16, i1 false)
  %call_sret = alloca { { i64, ptr } }, align 8
  call void @string.appendChar(ptr sret({ { i64, ptr } }) %call_sret, ptr %s, i8 10)
  %0 = call i8 @print(ptr %call_sret)
  ret i8 %0
}

define void @string.appendChar(ptr sret({ { i64, ptr } }) %sret_ptr, ptr %self_arg, i8 %c_arg) {
entry:
  %self = alloca { { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false)
  %c = alloca i8, align 1
  store i8 %c_arg, ptr %c, align 1
  %0 = call i64 @string.len(ptr %self)
  %add = add i64 %0, 1
  %newLength = alloca i64, align 8
  store i64 %add, ptr %newLength, align 8
  %struc.buffer = getelementptr inbounds { { i64, ptr } }, ptr %self, i32 0, i32 0
  %newLength_loaded = load i64, ptr %newLength, align 8
  %call_sret = alloca { i64, ptr }, align 8
  call void @Buffer._enlargedClone_spec_char_3(ptr sret({ i64, ptr }) %call_sret, ptr %struc.buffer, i64 %newLength_loaded)
  %newBuffer = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %newBuffer, ptr align 8 %call_sret, i64 16, i1 false)
  %1 = call i64 @string.len(ptr %self)
  %c_loaded = load i8, ptr %c, align 1
  %2 = call i8 @Buffer.set_spec_char_4(ptr %newBuffer, i64 %1, i8 %c_loaded)
  %call_sret1 = alloca { { i64, ptr } }, align 8
  call void @string.fromBuffer(ptr sret({ { i64, ptr } }) %call_sret1, ptr %newBuffer)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %call_sret1, i64 16, i1 false)
  ret void
}

define void @string.fromBuffer(ptr sret({ { i64, ptr } }) %sret_ptr, ptr %buffer_arg) {
entry:
  %buffer = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %buffer, ptr align 8 %buffer_arg, i64 16, i1 false)
  %call_sret = alloca { i64, ptr }, align 8
  call void @Buffer.cloned_spec_char_2(ptr sret({ i64, ptr }) %call_sret, ptr %buffer)
  %copied = alloca { i64, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %copied, ptr align 8 %call_sret, i64 16, i1 false)
  %struct_literal = alloca { { i64, ptr } }, align 8
  %buffer_store_addr = getelementptr inbounds { { i64, ptr } }, ptr %struct_literal, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %buffer_store_addr, ptr align 8 %copied, i64 16, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %struct_literal, i64 16, i1 false)
  ret void
}

define i8 @print(ptr %s_arg) {
entry:
  %s = alloca { { i64, ptr } }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %s, ptr align 8 %s_arg, i64 16, i1 false)
  %struc.buffer = getelementptr inbounds { { i64, ptr } }, ptr %s, i32 0, i32 0
  %struc.data = getelementptr inbounds { i64, ptr }, ptr %struc.buffer, i32 0, i32 1
  %struc.data1 = load ptr, ptr %struc.data, align 8
  %0 = call i64 @string.len(ptr %s)
  %1 = call i64 @write(i32 1, ptr %struc.data1, i64 %0, i64 0)
  ret i8 0
}

define i8 @lambdas.simpleDyn(ptr %thunk_arg) {
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

define i8 @lambdas.captures() {
entry:
  %closedOver = alloca i64, align 8
  store i64 3, ptr %closedOver, align 8
  %struct_literal = alloca { i64 }, align 8
  %closedOver_loaded = load i64, ptr %closedOver, align 8
  %closedOver_store_addr = getelementptr inbounds { i64 }, ptr %struct_literal, i32 0, i32 0
  store i64 %closedOver_loaded, ptr %closedOver_store_addr, align 8
  %add = alloca { ptr, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %add, ptr align 8 %struct_literal, i64 16, i1 false)
  %struct_literal1 = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal1, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.122, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal1, i32 0, i32 1
  store i64 32, ptr %line_store_addr, align 8
  %0 = call i64 @"lambdas.lambdas__captures_{lambda}_1143"(ptr %add, i64 3)
  %1 = call i8 @assertEquals_spec_i64_1(ptr %struct_literal1, i64 %0, i64 12)
  ret i8 %1
}

define i64 @"lambdas.lambdas__captures_{lambda}_1143"(ptr %__lambda_env_arg, i64 %x_arg) {
entry:
  %__lambda_env = alloca ptr, align 8
  store ptr %__lambda_env_arg, ptr %__lambda_env, align 8
  %x = alloca i64, align 8
  store i64 %x_arg, ptr %x, align 8
  %struct_literal = alloca {}, align 8
  %addOne = alloca { ptr, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %addOne, ptr align 8 %struct_literal, i64 16, i1 false)
  %x_loaded = load i64, ptr %x, align 8
  %0 = call i64 @"lambdas.lambdas__captures_{lambda}_1144"(ptr %addOne, i64 %x_loaded)
  %__lambda_env_loaded = load ptr, ptr %__lambda_env, align 8
  %struc.closedOver = getelementptr inbounds { i64 }, ptr %__lambda_env_loaded, i32 0, i32 0
  %struc.closedOver1 = load i64, ptr %struc.closedOver, align 8
  %add = add i64 %0, %struc.closedOver1
  %__lambda_env_loaded2 = load ptr, ptr %__lambda_env, align 8
  %struc.closedOver3 = getelementptr inbounds { i64 }, ptr %__lambda_env_loaded2, i32 0, i32 0
  %struc.closedOver4 = load i64, ptr %struc.closedOver3, align 8
  %1 = call i64 @"lambdas.lambdas__captures_{lambda}_1144"(ptr %addOne, i64 %struc.closedOver4)
  %add5 = add i64 %add, %1
  br label %arm_0

arm_0:                                            ; preds = %entry
  %__lambda_env_loaded6 = load ptr, ptr %__lambda_env, align 8
  %struc.closedOver7 = getelementptr inbounds { i64 }, ptr %__lambda_env_loaded6, i32 0, i32 0
  %struc.closedOver8 = load i64, ptr %struc.closedOver7, align 8
  %"==_i1" = icmp eq i64 %struc.closedOver8, 3
  %"==_res" = sext i1 %"==_i1" to i8
  %arm_pattern_i1 = trunc i8 %"==_res" to i1
  br i1 %arm_pattern_i1, label %arm_cons_0, label %arm_1

arm_cons_0:                                       ; preds = %arm_0
  br label %match_end

arm_1:                                            ; preds = %arm_0
  br i1 true, label %arm_cons_1, label %match_fail

arm_cons_1:                                       ; preds = %arm_1
  br label %match_end

match_fail:                                       ; preds = %arm_1
  unreachable

match_end:                                        ; preds = %arm_cons_1, %arm_cons_0
  %match_result = phi i64 [ 1, %arm_cons_0 ], [ 0, %arm_cons_1 ]
  %add9 = add i64 %add5, %match_result
  ret i64 %add9
}

define i64 @"lambdas.lambdas__captures_{lambda}_1144"(ptr %__lambda_env_arg, i64 %y_arg) {
entry:
  %__lambda_env = alloca ptr, align 8
  store ptr %__lambda_env_arg, ptr %__lambda_env, align 8
  %y = alloca i64, align 8
  store i64 %y_arg, ptr %y, align 8
  %y_loaded = load i64, ptr %y, align 8
  %add = add i64 %y_loaded, 1
  ret i64 %add
}

define i8 @lambdas.captureMut() {
entry:
  %counter = alloca ptr, align 8
  %0 = alloca i64, align 8
  store i64 3, ptr %0, align 8
  store ptr %0, ptr %counter, align 8
  %struct_literal = alloca { ptr }, align 8
  %counter_loaded = load ptr, ptr %counter, align 8
  %counter_store_addr = getelementptr inbounds { ptr }, ptr %struct_literal, i32 0, i32 0
  store ptr %counter_loaded, ptr %counter_store_addr, align 8
  %inc = alloca { ptr, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %inc, ptr align 8 %struct_literal, i64 16, i1 false)
  %1 = call i8 @"lambdas.lambdas__captureMut_{lambda}_1147"(ptr %inc, i64 0)
  %2 = call i8 @"lambdas.lambdas__captureMut_{lambda}_1147"(ptr %inc, i64 0)
  %3 = call i8 @"lambdas.lambdas__captureMut_{lambda}_1147"(ptr %inc, i64 0)
  %struct_literal1 = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal1, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.124, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal1, i32 0, i32 1
  store i64 46, ptr %line_store_addr, align 8
  %counter_loaded2 = load ptr, ptr %counter, align 8
  %deref = load i64, ptr %counter_loaded2, align 8
  %"==_i1" = icmp eq i64 %deref, 6
  %"==_res" = sext i1 %"==_i1" to i8
  %4 = call i8 @assert(ptr %struct_literal1, i8 %"==_res")
  ret i8 %4
}

define i8 @"lambdas.lambdas__captureMut_{lambda}_1147"(ptr %__lambda_env_arg, i64 %x_arg) {
entry:
  %__lambda_env = alloca ptr, align 8
  store ptr %__lambda_env_arg, ptr %__lambda_env, align 8
  %x = alloca i64, align 8
  store i64 %x_arg, ptr %x, align 8
  %__lambda_env_loaded = load ptr, ptr %__lambda_env, align 8
  %struc.counter = getelementptr inbounds { ptr }, ptr %__lambda_env_loaded, i32 0, i32 0
  %struc.counter1 = load ptr, ptr %struc.counter, align 8
  %deref = load i64, ptr %struc.counter1, align 8
  %add = add i64 %deref, 1
  %__lambda_env_loaded2 = load ptr, ptr %__lambda_env, align 8
  %struc.counter3 = getelementptr inbounds { ptr }, ptr %__lambda_env_loaded2, i32 0, i32 0
  %struc.counter4 = load ptr, ptr %struc.counter3, align 8
  store i64 %add, ptr %struc.counter4, align 8
  ret i8 0
}

define i8 @lambdas.closureReturn() {
entry:
  %struct_literal = alloca {}, align 8
  %getThree = alloca { ptr, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %getThree, ptr align 8 %struct_literal, i64 16, i1 false)
  %struct_literal1 = alloca {}, align 8
  %nullary = alloca { ptr, ptr }, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %nullary, ptr align 8 %struct_literal1, i64 16, i1 false)
  %struct_literal2 = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal2, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr, ptr align 8 @str.126, i64 16, i1 false)
  %line_store_addr = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal2, i32 0, i32 1
  store i64 52, ptr %line_store_addr, align 8
  %0 = call i64 @"lambdas.lambdas__closureReturn_{lambda}_1148"(ptr %getThree)
  %"==_i1" = icmp eq i64 %0, 3
  %"==_res" = sext i1 %"==_i1" to i8
  %1 = call i8 @assert(ptr %struct_literal2, i8 %"==_res")
  %struct_literal3 = alloca { { { i64, ptr } }, i64 }, align 8
  %filename_store_addr4 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal3, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %filename_store_addr4, ptr align 8 @str.128, i64 16, i1 false)
  %line_store_addr5 = getelementptr inbounds { { { i64, ptr } }, i64 }, ptr %struct_literal3, i32 0, i32 1
  store i64 53, ptr %line_store_addr5, align 8
  %2 = call i64 @"lambdas.lambdas__closureReturn_{lambda}_1149"(ptr %nullary)
  %"==_i16" = icmp eq i64 %2, 4
  %"==_res7" = sext i1 %"==_i16" to i8
  %3 = call i8 @assert(ptr %struct_literal3, i8 %"==_res7")
  ret i8 %3
}

define i64 @"lambdas.lambdas__closureReturn_{lambda}_1148"(ptr %__lambda_env_arg) {
entry:
  %__lambda_env = alloca ptr, align 8
  store ptr %__lambda_env_arg, ptr %__lambda_env, align 8
  ret i64 3
}

define i64 @"lambdas.lambdas__closureReturn_{lambda}_1149"(ptr %__lambda_env_arg) {
entry:
  %__lambda_env = alloca ptr, align 8
  store ptr %__lambda_env_arg, ptr %__lambda_env, align 8
  ret i64 4
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.module.flags = !{!0, !1, !2, !3}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 14, i32 0]}
!1 = !{i32 2, !"Dwarf Version", i32 4}
!2 = !{i32 2, !"Debug Info Version", i32 3}
!3 = !{i32 1, !"PIE Level", i32 2}
