; ModuleID = 'b'
source_filename = "builtin.k1"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx14.4.0"

%Bool.Yes = type { i64, i8 }
%Bool = type { %Bool.No }
%Bool.No = type { i64, i64 }
%"46.String" = type { i64, { { i64, ptr } } }
%"46" = type { %"46.String" }
%Opt = type { %Opt.Some }
%Opt.Some = type { i64, %"46.5" }
%"46.5" = type { %"46.String.4" }
%"46.String.4" = type { i64, { { i64, ptr } } }
%"46.Char.3" = type { i64, i8 }
%Opt.8 = type { %Opt.Some.7 }
%Opt.Some.7 = type { i8, i8 }
%"46.2" = type { %"46.String.1" }
%"46.String.1" = type { i64, { { i64, ptr } } }
%Opt.None = type { i64 }
%Opt.None.6 = type { i8 }
%Bool.11 = type { %Bool.No.10 }
%Bool.No.10 = type { i64, i64 }

@SEEK_END = constant i32 2
@SEEK_SET = constant i32 0
@mb = constant i64 1048576
@gb = constant i64 1073741824
@str_data = constant [9 x i8] c"buffer.k1"
@str = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data } }
@str_data.1 = constant [40 x i8] c"Buffer copy source index out of bounds: "
@str.2 = constant { { i64, ptr } } { { i64, ptr } { i64 40, ptr @str_data.1 } }
@str_data.3 = constant [9 x i8] c"buffer.k1"
@str.4 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.3 } }
@str_data.5 = constant [40 x i8] c"Buffer copy source index out of bounds: "
@str.6 = constant { { i64, ptr } } { { i64, ptr } { i64 40, ptr @str_data.5 } }
@str_data.7 = constant [1 x i8] c"0"
@str.8 = constant { { i64, ptr } } { { i64, ptr } { i64 1, ptr @str_data.7 } }
@str_data.9 = constant [9 x i8] c"buffer.k1"
@str.10 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.9 } }
@str_data.11 = constant [40 x i8] c"Buffer copy source index out of bounds: "
@str.12 = constant { { i64, ptr } } { { i64, ptr } { i64 40, ptr @str_data.11 } }
@str_data.13 = constant [0 x i8] zeroinitializer
@str.14 = constant { { i64, ptr } } { { i64, ptr } { i64 0, ptr @str_data.13 } }
@str_data.15 = constant [9 x i8] c"buffer.k1"
@str.16 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.15 } }
@str_data.17 = constant [33 x i8] c"slice start index out of bounds: "
@str.18 = constant { { i64, ptr } } { { i64, ptr } { i64 33, ptr @str_data.17 } }
@str_data.19 = constant [0 x i8] zeroinitializer
@str.20 = constant { { i64, ptr } } { { i64, ptr } { i64 0, ptr @str_data.19 } }
@str_data.21 = constant [0 x i8] zeroinitializer
@str.22 = constant { { i64, ptr } } { { i64, ptr } { i64 0, ptr @str_data.21 } }
@str_data.23 = constant [4 x i8] c" at "
@str.24 = constant { { i64, ptr } } { { i64, ptr } { i64 4, ptr @str_data.23 } }
@str_data.25 = constant [1 x i8] c":"
@str.26 = constant { { i64, ptr } } { { i64, ptr } { i64 1, ptr @str_data.25 } }
@str_data.27 = constant [1 x i8] c"\0A"
@str.28 = constant { { i64, ptr } } { { i64, ptr } { i64 1, ptr @str_data.27 } }
@str_data.29 = constant [9 x i8] c"buffer.k1"
@str.30 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.29 } }
@str_data.31 = constant [26 x i8] c"Buffer index out of bounds"
@str.32 = constant { { i64, ptr } } { { i64, ptr } { i64 26, ptr @str_data.31 } }
@str_data.33 = constant [6 x i8] c"opt.k1"
@str.34 = constant { { i64, ptr } } { { i64, ptr } { i64 6, ptr @str_data.33 } }
@str_data.35 = constant [15 x i8] c"Opt.get on None"
@str.36 = constant { { i64, ptr } } { { i64, ptr } { i64 15, ptr @str_data.35 } }
@str_data.37 = constant [17 x i8] c"string_builder.k1"
@str.38 = constant { { i64, ptr } } { { i64, ptr } { i64 17, ptr @str_data.37 } }
@str_data.39 = constant [11 x i8] c"Match Error"
@str.40 = constant { { i64, ptr } } { { i64, ptr } { i64 11, ptr @str_data.39 } }
@str_data.41 = constant [9 x i8] c"buffer.k1"
@str.42 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.41 } }
@str_data.43 = constant [26 x i8] c"Buffer index out of bounds"
@str.44 = constant { { i64, ptr } } { { i64, ptr } { i64 26, ptr @str_data.43 } }
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
@str_data.57 = constant [0 x i8] zeroinitializer
@str.58 = constant { { i64, ptr } } { { i64, ptr } { i64 0, ptr @str_data.57 } }
@str_data.59 = constant [9 x i8] c"buffer.k1"
@str.60 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.59 } }
@str_data.61 = constant [38 x i8] c"Buffer copy dest index out of bounds: "
@str.62 = constant { { i64, ptr } } { { i64, ptr } { i64 38, ptr @str_data.61 } }
@str_data.63 = constant [0 x i8] zeroinitializer
@str.64 = constant { { i64, ptr } } { { i64, ptr } { i64 0, ptr @str_data.63 } }
@str_data.65 = constant [0 x i8] zeroinitializer
@str.66 = constant { { i64, ptr } } { { i64, ptr } { i64 0, ptr @str_data.65 } }
@str_data.67 = constant [9 x i8] c"buffer.k1"
@str.68 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.67 } }
@str_data.69 = constant [38 x i8] c"Buffer copy dest index out of bounds: "
@str.70 = constant { { i64, ptr } } { { i64, ptr } { i64 38, ptr @str_data.69 } }
@str_data.71 = constant [0 x i8] zeroinitializer
@str.72 = constant { { i64, ptr } } { { i64, ptr } { i64 0, ptr @str_data.71 } }
@str_data.73 = constant [0 x i8] zeroinitializer
@str.74 = constant { { i64, ptr } } { { i64, ptr } { i64 0, ptr @str_data.73 } }
@str_data.75 = constant [9 x i8] c"buffer.k1"
@str.76 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.75 } }
@str_data.77 = constant [38 x i8] c"Buffer copy dest index out of bounds: "
@str.78 = constant { { i64, ptr } } { { i64, ptr } { i64 38, ptr @str_data.77 } }
@str_data.79 = constant [0 x i8] zeroinitializer
@str.80 = constant { { i64, ptr } } { { i64, ptr } { i64 0, ptr @str_data.79 } }
@str_data.81 = constant [9 x i8] c"buffer.k1"
@str.82 = constant { { i64, ptr } } { { i64, ptr } { i64 9, ptr @str_data.81 } }
@str_data.83 = constant [26 x i8] c"Buffer index out of bounds"
@str.84 = constant { { i64, ptr } } { { i64, ptr } { i64 26, ptr @str_data.83 } }

define i64 @main() {
entry:
  %0 = call { i64, { i64, ptr } } @List.empty_spec_Bool_3()
  %1 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %0, ptr %1, align 8
  %b = alloca ptr, align 8
  store ptr %1, ptr %b, align 8
  %loaded_value = load ptr, ptr %b, align 8
  %enum_constr = alloca %Bool.Yes, align 8
  %enum_tag_Yes = getelementptr inbounds %Bool.Yes, ptr %enum_constr, i32 0, i32 0
  store i64 0, ptr %enum_tag_Yes, align 8
  %enum_payload_Yes = getelementptr inbounds %Bool.Yes, ptr %enum_constr, i32 0, i32 1
  store i8 4, ptr %enum_payload_Yes, align 1
  %enum_value = load %Bool, ptr %enum_constr, align 8
  %2 = call i8 @List.push_spec_Bool_8(ptr %loaded_value, %Bool %enum_value)
  ret i64 0
}

define { i64, { i64, ptr } } @List.empty_spec_Bool_3() {
entry:
  %0 = call { i64, { i64, ptr } } @List.withCapacity_spec_Bool_9(i64 0)
  ret { i64, { i64, ptr } } %0
}

define { i64, { i64, ptr } } @List.withCapacity_spec_Bool_9(i64 %cap) {
entry:
  %cap1 = alloca i64, align 8
  store i64 %cap, ptr %cap1, align 8
  %loaded_value = load i64, ptr %cap1, align 8
  %0 = call { i64, ptr } @Buffer._allocate_spec_Bool_12(i64 %loaded_value)
  %buffer = alloca { i64, ptr }, align 8
  store { i64, ptr } %0, ptr %buffer, align 8
  %loaded_value2 = load { i64, ptr }, ptr %buffer, align 8
  %struct_init_1 = insertvalue { i64, { i64, ptr } } { i64 0, { i64, ptr } undef }, { i64, ptr } %loaded_value2, 1
  ret { i64, { i64, ptr } } %struct_init_1
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #0

define { i64, ptr } @Buffer._allocate_spec_Bool_12(i64 %count) {
entry:
  %count1 = alloca i64, align 8
  store i64 %count, ptr %count1, align 8
  %loaded_value = load i64, ptr %count1, align 8
  %"==_i1" = icmp eq i64 %loaded_value, 0
  %"==_res" = sext i1 %"==_i1" to i8
  %cond_i1 = trunc i8 %"==_res" to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %0 = call ptr @Pointer.null()
  br label %if_merge

if_alt:                                           ; preds = %entry
  %loaded_value2 = load i64, ptr %count1, align 8
  %1 = call i64 @Buffer.bufferLenBytes_spec_Bool_12(i64 %loaded_value2)
  %sizeBytes = alloca i64, align 8
  store i64 %1, ptr %sizeBytes, align 8
  %loaded_value3 = load i64, ptr %sizeBytes, align 8
  %2 = call ptr @malloc(i64 %loaded_value3)
  br label %if_merge

if_merge:                                         ; preds = %if_alt, %if_cons
  %if_phi = phi ptr [ %0, %if_cons ], [ %2, %if_alt ]
  %data = alloca ptr, align 8
  store ptr %if_phi, ptr %data, align 8
  %loaded_value4 = load i64, ptr %count1, align 8
  %struct_init_0 = insertvalue { i64, ptr } undef, i64 %loaded_value4, 0
  %loaded_value5 = load ptr, ptr %data, align 8
  %struct_init_1 = insertvalue { i64, ptr } %struct_init_0, ptr %loaded_value5, 1
  ret { i64, ptr } %struct_init_1
}

define ptr @Pointer.null() {
entry:
  ret ptr null
}

define i64 @Buffer.bufferLenBytes_spec_Bool_12(i64 %count) {
entry:
  %count1 = alloca i64, align 8
  store i64 %count, ptr %count1, align 8
  %loaded_value = load i64, ptr %count1, align 8
  %mul = mul i64 %loaded_value, 16
  ret i64 %mul
}

declare ptr @malloc(i64)

define i8 @List.push_spec_Bool_8(ptr %self, %Bool %elem) {
entry:
  %self1 = alloca ptr, align 8
  store ptr %self, ptr %self1, align 8
  %elem2 = alloca %Bool, align 8
  store %Bool %elem, ptr %elem2, align 8
  %loaded_value = load ptr, ptr %self1, align 8
  %deref = load { i64, { i64, ptr } }, ptr %loaded_value, align 8
  %struc.len = extractvalue { i64, { i64, ptr } } %deref, 0
  %startLength = alloca i64, align 8
  store i64 %struc.len, ptr %startLength, align 8
  %loaded_value3 = load i64, ptr %startLength, align 8
  %loaded_value4 = load ptr, ptr %self1, align 8
  %deref5 = load { i64, { i64, ptr } }, ptr %loaded_value4, align 8
  %0 = call i64 @List.cap_spec_Bool_8({ i64, { i64, ptr } } %deref5)
  %"==_i1" = icmp eq i64 %loaded_value3, %0
  %"==_res" = sext i1 %"==_i1" to i8
  %cond_i1 = trunc i8 %"==_res" to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %loaded_value6 = load ptr, ptr %self1, align 8
  %1 = call i8 @List._grow_spec_Bool_7(ptr %loaded_value6)
  br label %if_merge

if_alt:                                           ; preds = %entry
  br label %if_merge

if_merge:                                         ; preds = %if_alt, %if_cons
  %if_phi = phi i8 [ %1, %if_cons ], [ 0, %if_alt ]
  %loaded_value7 = load i64, ptr %startLength, align 8
  %add = add i64 %loaded_value7, 1
  %loaded_value8 = load ptr, ptr %self1, align 8
  %struc.len9 = getelementptr inbounds { i64, { i64, ptr } }, ptr %loaded_value8, i32 0, i32 0
  store i64 %add, ptr %struc.len9, align 8
  %loaded_value10 = load ptr, ptr %self1, align 8
  %deref11 = load { i64, { i64, ptr } }, ptr %loaded_value10, align 8
  %loaded_value12 = load i64, ptr %startLength, align 8
  %loaded_value13 = load %Bool, ptr %elem2, align 8
  %2 = call i8 @List.set_spec_Bool_8({ i64, { i64, ptr } } %deref11, i64 %loaded_value12, %Bool %loaded_value13)
  ret i8 0
}

define i64 @List.cap_spec_Bool_8({ i64, { i64, ptr } } %self) {
entry:
  %self1 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %self, ptr %self1, align 8
  %loaded_value = load { i64, { i64, ptr } }, ptr %self1, align 8
  %struc.buffer = extractvalue { i64, { i64, ptr } } %loaded_value, 1
  %struc.len = extractvalue { i64, ptr } %struc.buffer, 0
  ret i64 %struc.len
}

define i8 @List._grow_spec_Bool_7(ptr %self) {
entry:
  %self1 = alloca ptr, align 8
  store ptr %self, ptr %self1, align 8
  %elemSize = alloca i64, align 8
  store i64 16, ptr %elemSize, align 8
  %loaded_value = load ptr, ptr %self1, align 8
  %deref = load { i64, { i64, ptr } }, ptr %loaded_value, align 8
  %struc.buffer = extractvalue { i64, { i64, ptr } } %deref, 1
  %0 = call i8 @Buffer.isEmpty_spec_Bool_9({ i64, ptr } %struc.buffer)
  %cond_i1 = trunc i8 %0 to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %loaded_value5 = load i64, ptr %elemSize, align 8
  %">=_i1" = icmp sge i64 %loaded_value5, 1024
  %">=_res" = sext i1 %">=_i1" to i8
  %cond_i16 = trunc i8 %">=_res" to i1
  br i1 %cond_i16, label %if_cons2, label %if_alt3

if_alt:                                           ; preds = %entry
  %loaded_value7 = load ptr, ptr %self1, align 8
  %deref8 = load { i64, { i64, ptr } }, ptr %loaded_value7, align 8
  %1 = call i64 @List.cap_spec_Bool_8({ i64, { i64, ptr } } %deref8)
  %mul = mul i64 %1, 2
  br label %if_merge

if_merge:                                         ; preds = %if_alt, %if_merge4
  %if_phi9 = phi i64 [ %if_phi, %if_merge4 ], [ %mul, %if_alt ]
  %newCap = alloca i64, align 8
  store i64 %if_phi9, ptr %newCap, align 8
  %loaded_value10 = load ptr, ptr %self1, align 8
  %deref11 = load { i64, { i64, ptr } }, ptr %loaded_value10, align 8
  %struc.buffer12 = extractvalue { i64, { i64, ptr } } %deref11, 1
  %loaded_value13 = load i64, ptr %newCap, align 8
  %2 = call { i64, ptr } @Buffer._enlargedClone_spec_Bool_8({ i64, ptr } %struc.buffer12, i64 %loaded_value13)
  %newBuffer = alloca { i64, ptr }, align 8
  store { i64, ptr } %2, ptr %newBuffer, align 8
  %loaded_value14 = load { i64, ptr }, ptr %newBuffer, align 8
  %loaded_value15 = load ptr, ptr %self1, align 8
  %struc.buffer16 = getelementptr inbounds { i64, { i64, ptr } }, ptr %loaded_value15, i32 0, i32 1
  store { i64, ptr } %loaded_value14, ptr %struc.buffer16, align 8
  ret i8 0

if_cons2:                                         ; preds = %if_cons
  br label %if_merge4

if_alt3:                                          ; preds = %if_cons
  br label %if_merge4

if_merge4:                                        ; preds = %if_alt3, %if_cons2
  %if_phi = phi i64 [ 1, %if_cons2 ], [ 8, %if_alt3 ]
  br label %if_merge
}

define i8 @Buffer.isEmpty_spec_Bool_9({ i64, ptr } %self) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %loaded_value = load { i64, ptr }, ptr %self1, align 8
  %struc.len = extractvalue { i64, ptr } %loaded_value, 0
  %"==_i1" = icmp eq i64 %struc.len, 0
  %"==_res" = sext i1 %"==_i1" to i8
  ret i8 %"==_res"
}

define { i64, ptr } @Buffer._enlargedClone_spec_Bool_8({ i64, ptr } %self, i64 %newCount) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %newCount2 = alloca i64, align 8
  store i64 %newCount, ptr %newCount2, align 8
  %loaded_value = load i64, ptr %newCount2, align 8
  %0 = call { i64, ptr } @Buffer._allocate_spec_Bool_12(i64 %loaded_value)
  %newBuffer = alloca { i64, ptr }, align 8
  store { i64, ptr } %0, ptr %newBuffer, align 8
  %loaded_value3 = load { i64, ptr }, ptr %self1, align 8
  %loaded_value4 = load { i64, ptr }, ptr %newBuffer, align 8
  %loaded_value5 = load { i64, ptr }, ptr %self1, align 8
  %struc.len = extractvalue { i64, ptr } %loaded_value5, 0
  %1 = call i8 @Buffer._copyFrom_spec_Bool_8({ i64, ptr } %loaded_value3, { i64, ptr } %loaded_value4, i64 %struc.len)
  %loaded_value6 = load { i64, ptr }, ptr %newBuffer, align 8
  ret { i64, ptr } %loaded_value6
}

define i8 @Buffer._copyFrom_spec_Bool_8({ i64, ptr } %source, { i64, ptr } %dest, i64 %count) {
entry:
  %source1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %source, ptr %source1, align 8
  %dest2 = alloca { i64, ptr }, align 8
  store { i64, ptr } %dest, ptr %dest2, align 8
  %count3 = alloca i64, align 8
  store i64 %count, ptr %count3, align 8
  %loaded_value = load i64, ptr %count3, align 8
  %loaded_value4 = load { i64, ptr }, ptr %source1, align 8
  %struc.len = extractvalue { i64, ptr } %loaded_value4, 0
  %">_i1" = icmp sgt i64 %loaded_value, %struc.len
  %">_res" = sext i1 %">_i1" to i8
  %cond_i1 = trunc i8 %">_res" to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %0 = load { { i64, ptr } }, ptr @str, align 8
  %struct_init_0 = insertvalue { { { i64, ptr } }, i64 } undef, { { i64, ptr } } %0, 0
  %struct_init_1 = insertvalue { { { i64, ptr } }, i64 } %struct_init_0, i64 29, 1
  %1 = call { ptr } @StringBuilder.withCapacity(i64 3)
  %__sb_1289 = alloca { ptr }, align 8
  store { ptr } %1, ptr %__sb_1289, align 8
  %loaded_value5 = load { ptr }, ptr %__sb_1289, align 8
  %2 = load { { i64, ptr } }, ptr @str.2, align 8
  %3 = call i8 @StringBuilder.putString({ ptr } %loaded_value5, { { i64, ptr } } %2)
  %loaded_value6 = load { ptr }, ptr %__sb_1289, align 8
  %loaded_value7 = load i64, ptr %count3, align 8
  %4 = call { { i64, ptr } } @Show_u64_show(i64 %loaded_value7)
  %5 = call i8 @StringBuilder.putString({ ptr } %loaded_value6, { { i64, ptr } } %4)
  %loaded_value8 = load { ptr }, ptr %__sb_1289, align 8
  %6 = load { { i64, ptr } }, ptr @str.74, align 8
  %7 = call i8 @StringBuilder.putString({ ptr } %loaded_value8, { { i64, ptr } } %6)
  %loaded_value9 = load { ptr }, ptr %__sb_1289, align 8
  %8 = call { { i64, ptr } } @StringBuilder.build({ ptr } %loaded_value9)
  call void @crash({ { { i64, ptr } }, i64 } %struct_init_1, { { i64, ptr } } %8)
  unreachable

if_alt:                                           ; preds = %entry
  br label %if_merge

if_merge:                                         ; preds = %if_alt
  %if_phi = phi i8 [ 0, %if_alt ]
  %loaded_value13 = load i64, ptr %count3, align 8
  %loaded_value14 = load { i64, ptr }, ptr %dest2, align 8
  %struc.len15 = extractvalue { i64, ptr } %loaded_value14, 0
  %">_i116" = icmp sgt i64 %loaded_value13, %struc.len15
  %">_res17" = sext i1 %">_i116" to i8
  %cond_i118 = trunc i8 %">_res17" to i1
  br i1 %cond_i118, label %if_cons10, label %if_alt11

if_cons10:                                        ; preds = %if_merge
  %9 = load { { i64, ptr } }, ptr @str.76, align 8
  %struct_init_019 = insertvalue { { { i64, ptr } }, i64 } undef, { { i64, ptr } } %9, 0
  %struct_init_120 = insertvalue { { { i64, ptr } }, i64 } %struct_init_019, i64 32, 1
  %10 = call { ptr } @StringBuilder.withCapacity(i64 3)
  %__sb_1291 = alloca { ptr }, align 8
  store { ptr } %10, ptr %__sb_1291, align 8
  %loaded_value21 = load { ptr }, ptr %__sb_1291, align 8
  %11 = load { { i64, ptr } }, ptr @str.78, align 8
  %12 = call i8 @StringBuilder.putString({ ptr } %loaded_value21, { { i64, ptr } } %11)
  %loaded_value22 = load { ptr }, ptr %__sb_1291, align 8
  %loaded_value23 = load i64, ptr %count3, align 8
  %13 = call { { i64, ptr } } @Show_u64_show(i64 %loaded_value23)
  %14 = call i8 @StringBuilder.putString({ ptr } %loaded_value22, { { i64, ptr } } %13)
  %loaded_value24 = load { ptr }, ptr %__sb_1291, align 8
  %15 = load { { i64, ptr } }, ptr @str.80, align 8
  %16 = call i8 @StringBuilder.putString({ ptr } %loaded_value24, { { i64, ptr } } %15)
  %loaded_value25 = load { ptr }, ptr %__sb_1291, align 8
  %17 = call { { i64, ptr } } @StringBuilder.build({ ptr } %loaded_value25)
  call void @crash({ { { i64, ptr } }, i64 } %struct_init_120, { { i64, ptr } } %17)
  unreachable

if_alt11:                                         ; preds = %if_merge
  br label %if_merge12

if_merge12:                                       ; preds = %if_alt11
  %if_phi26 = phi i8 [ 0, %if_alt11 ]
  %loaded_value27 = load i64, ptr %count3, align 8
  %18 = call i64 @Buffer.bufferLenBytes_spec_Bool_12(i64 %loaded_value27)
  %numBytes = alloca i64, align 8
  store i64 %18, ptr %numBytes, align 8
  %loaded_value28 = load { i64, ptr }, ptr %dest2, align 8
  %struc.data = extractvalue { i64, ptr } %loaded_value28, 1
  %loaded_value29 = load { i64, ptr }, ptr %source1, align 8
  %struc.data30 = extractvalue { i64, ptr } %loaded_value29, 1
  %loaded_value31 = load i64, ptr %numBytes, align 8
  %19 = call ptr @memcpy(ptr %struc.data, ptr %struc.data30, i64 %loaded_value31)
  ret i8 0
}

define { ptr } @StringBuilder.withCapacity(i64 %cap) {
entry:
  %cap1 = alloca i64, align 8
  store i64 %cap, ptr %cap1, align 8
  %loaded_value = load i64, ptr %cap1, align 8
  %0 = call { i64, { i64, ptr } } @"List.withCapacity_spec_enum Char(char) | String(string)_5"(i64 %loaded_value)
  %1 = call ptr @"new_spec_List[enum Char(char) | String(string)]_1"({ i64, { i64, ptr } } %0)
  %parts = alloca ptr, align 8
  store ptr %1, ptr %parts, align 8
  %loaded_value2 = load ptr, ptr %parts, align 8
  %struct_init_0 = insertvalue { ptr } undef, ptr %loaded_value2, 0
  ret { ptr } %struct_init_0
}

define { i64, { i64, ptr } } @"List.withCapacity_spec_enum Char(char) | String(string)_5"(i64 %cap) {
entry:
  %cap1 = alloca i64, align 8
  store i64 %cap, ptr %cap1, align 8
  %loaded_value = load i64, ptr %cap1, align 8
  %0 = call { i64, ptr } @"Buffer._allocate_spec_enum Char(char) | String(string)_8"(i64 %loaded_value)
  %buffer = alloca { i64, ptr }, align 8
  store { i64, ptr } %0, ptr %buffer, align 8
  %loaded_value2 = load { i64, ptr }, ptr %buffer, align 8
  %struct_init_1 = insertvalue { i64, { i64, ptr } } { i64 0, { i64, ptr } undef }, { i64, ptr } %loaded_value2, 1
  ret { i64, { i64, ptr } } %struct_init_1
}

define { i64, ptr } @"Buffer._allocate_spec_enum Char(char) | String(string)_8"(i64 %count) {
entry:
  %count1 = alloca i64, align 8
  store i64 %count, ptr %count1, align 8
  %loaded_value = load i64, ptr %count1, align 8
  %"==_i1" = icmp eq i64 %loaded_value, 0
  %"==_res" = sext i1 %"==_i1" to i8
  %cond_i1 = trunc i8 %"==_res" to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %0 = call ptr @Pointer.null()
  br label %if_merge

if_alt:                                           ; preds = %entry
  %loaded_value2 = load i64, ptr %count1, align 8
  %1 = call i64 @"Buffer.bufferLenBytes_spec_enum Char(char) | String(string)_8"(i64 %loaded_value2)
  %sizeBytes = alloca i64, align 8
  store i64 %1, ptr %sizeBytes, align 8
  %loaded_value3 = load i64, ptr %sizeBytes, align 8
  %2 = call ptr @malloc(i64 %loaded_value3)
  br label %if_merge

if_merge:                                         ; preds = %if_alt, %if_cons
  %if_phi = phi ptr [ %0, %if_cons ], [ %2, %if_alt ]
  %data = alloca ptr, align 8
  store ptr %if_phi, ptr %data, align 8
  %loaded_value4 = load i64, ptr %count1, align 8
  %struct_init_0 = insertvalue { i64, ptr } undef, i64 %loaded_value4, 0
  %loaded_value5 = load ptr, ptr %data, align 8
  %struct_init_1 = insertvalue { i64, ptr } %struct_init_0, ptr %loaded_value5, 1
  ret { i64, ptr } %struct_init_1
}

define i64 @"Buffer.bufferLenBytes_spec_enum Char(char) | String(string)_8"(i64 %count) {
entry:
  %count1 = alloca i64, align 8
  store i64 %count, ptr %count1, align 8
  %loaded_value = load i64, ptr %count1, align 8
  %mul = mul i64 %loaded_value, 24
  ret i64 %mul
}

define ptr @"new_spec_List[enum Char(char) | String(string)]_1"({ i64, { i64, ptr } } %value) {
entry:
  %value1 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %value, ptr %value1, align 8
  %0 = call ptr @malloc(i64 24)
  %ptr = alloca ptr, align 8
  store ptr %0, ptr %ptr, align 8
  %loaded_value = load ptr, ptr %ptr, align 8
  %t = alloca ptr, align 8
  store ptr %loaded_value, ptr %t, align 8
  %loaded_value2 = load { i64, { i64, ptr } }, ptr %value1, align 8
  %loaded_value3 = load ptr, ptr %t, align 8
  store { i64, { i64, ptr } } %loaded_value2, ptr %loaded_value3, align 8
  %loaded_value4 = load ptr, ptr %t, align 8
  ret ptr %loaded_value4
}

define i8 @StringBuilder.putString({ ptr } %self, { { i64, ptr } } %s) {
entry:
  %self1 = alloca { ptr }, align 8
  store { ptr } %self, ptr %self1, align 8
  %s2 = alloca { { i64, ptr } }, align 8
  store { { i64, ptr } } %s, ptr %s2, align 8
  %loaded_value = load { ptr }, ptr %self1, align 8
  %struc.parts = extractvalue { ptr } %loaded_value, 0
  %enum_constr = alloca %"46.String", align 8
  %enum_tag_String = getelementptr inbounds %"46.String", ptr %enum_constr, i32 0, i32 0
  store i64 1, ptr %enum_tag_String, align 8
  %loaded_value3 = load { { i64, ptr } }, ptr %s2, align 8
  %enum_payload_String = getelementptr inbounds %"46.String", ptr %enum_constr, i32 0, i32 1
  store { { i64, ptr } } %loaded_value3, ptr %enum_payload_String, align 8
  %enum_value = load %"46", ptr %enum_constr, align 8
  %0 = call i8 @"List.push_spec_enum Char(char) | String(string)_6"(ptr %struc.parts, %"46" %enum_value)
  ret i8 %0
}

define i8 @"List.push_spec_enum Char(char) | String(string)_6"(ptr %self, %"46" %elem) {
entry:
  %self1 = alloca ptr, align 8
  store ptr %self, ptr %self1, align 8
  %elem2 = alloca %"46", align 8
  store %"46" %elem, ptr %elem2, align 8
  %loaded_value = load ptr, ptr %self1, align 8
  %deref = load { i64, { i64, ptr } }, ptr %loaded_value, align 8
  %struc.len = extractvalue { i64, { i64, ptr } } %deref, 0
  %startLength = alloca i64, align 8
  store i64 %struc.len, ptr %startLength, align 8
  %loaded_value3 = load i64, ptr %startLength, align 8
  %loaded_value4 = load ptr, ptr %self1, align 8
  %deref5 = load { i64, { i64, ptr } }, ptr %loaded_value4, align 8
  %0 = call i64 @"List.cap_spec_enum Char(char) | String(string)_6"({ i64, { i64, ptr } } %deref5)
  %"==_i1" = icmp eq i64 %loaded_value3, %0
  %"==_res" = sext i1 %"==_i1" to i8
  %cond_i1 = trunc i8 %"==_res" to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %loaded_value6 = load ptr, ptr %self1, align 8
  %1 = call i8 @"List._grow_spec_enum Char(char) | String(string)_5"(ptr %loaded_value6)
  br label %if_merge

if_alt:                                           ; preds = %entry
  br label %if_merge

if_merge:                                         ; preds = %if_alt, %if_cons
  %if_phi = phi i8 [ %1, %if_cons ], [ 0, %if_alt ]
  %loaded_value7 = load i64, ptr %startLength, align 8
  %add = add i64 %loaded_value7, 1
  %loaded_value8 = load ptr, ptr %self1, align 8
  %struc.len9 = getelementptr inbounds { i64, { i64, ptr } }, ptr %loaded_value8, i32 0, i32 0
  store i64 %add, ptr %struc.len9, align 8
  %loaded_value10 = load ptr, ptr %self1, align 8
  %deref11 = load { i64, { i64, ptr } }, ptr %loaded_value10, align 8
  %loaded_value12 = load i64, ptr %startLength, align 8
  %loaded_value13 = load %"46", ptr %elem2, align 8
  %2 = call i8 @"List.set_spec_enum Char(char) | String(string)_6"({ i64, { i64, ptr } } %deref11, i64 %loaded_value12, %"46" %loaded_value13)
  ret i8 0
}

define i64 @"List.cap_spec_enum Char(char) | String(string)_6"({ i64, { i64, ptr } } %self) {
entry:
  %self1 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %self, ptr %self1, align 8
  %loaded_value = load { i64, { i64, ptr } }, ptr %self1, align 8
  %struc.buffer = extractvalue { i64, { i64, ptr } } %loaded_value, 1
  %struc.len = extractvalue { i64, ptr } %struc.buffer, 0
  ret i64 %struc.len
}

define i8 @"List._grow_spec_enum Char(char) | String(string)_5"(ptr %self) {
entry:
  %self1 = alloca ptr, align 8
  store ptr %self, ptr %self1, align 8
  %elemSize = alloca i64, align 8
  store i64 24, ptr %elemSize, align 8
  %loaded_value = load ptr, ptr %self1, align 8
  %deref = load { i64, { i64, ptr } }, ptr %loaded_value, align 8
  %struc.buffer = extractvalue { i64, { i64, ptr } } %deref, 1
  %0 = call i8 @"Buffer.isEmpty_spec_enum Char(char) | String(string)_7"({ i64, ptr } %struc.buffer)
  %cond_i1 = trunc i8 %0 to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %loaded_value5 = load i64, ptr %elemSize, align 8
  %">=_i1" = icmp sge i64 %loaded_value5, 1024
  %">=_res" = sext i1 %">=_i1" to i8
  %cond_i16 = trunc i8 %">=_res" to i1
  br i1 %cond_i16, label %if_cons2, label %if_alt3

if_alt:                                           ; preds = %entry
  %loaded_value7 = load ptr, ptr %self1, align 8
  %deref8 = load { i64, { i64, ptr } }, ptr %loaded_value7, align 8
  %1 = call i64 @"List.cap_spec_enum Char(char) | String(string)_6"({ i64, { i64, ptr } } %deref8)
  %mul = mul i64 %1, 2
  br label %if_merge

if_merge:                                         ; preds = %if_alt, %if_merge4
  %if_phi9 = phi i64 [ %if_phi, %if_merge4 ], [ %mul, %if_alt ]
  %newCap = alloca i64, align 8
  store i64 %if_phi9, ptr %newCap, align 8
  %loaded_value10 = load ptr, ptr %self1, align 8
  %deref11 = load { i64, { i64, ptr } }, ptr %loaded_value10, align 8
  %struc.buffer12 = extractvalue { i64, { i64, ptr } } %deref11, 1
  %loaded_value13 = load i64, ptr %newCap, align 8
  %2 = call { i64, ptr } @"Buffer._enlargedClone_spec_enum Char(char) | String(string)_6"({ i64, ptr } %struc.buffer12, i64 %loaded_value13)
  %newBuffer = alloca { i64, ptr }, align 8
  store { i64, ptr } %2, ptr %newBuffer, align 8
  %loaded_value14 = load { i64, ptr }, ptr %newBuffer, align 8
  %loaded_value15 = load ptr, ptr %self1, align 8
  %struc.buffer16 = getelementptr inbounds { i64, { i64, ptr } }, ptr %loaded_value15, i32 0, i32 1
  store { i64, ptr } %loaded_value14, ptr %struc.buffer16, align 8
  ret i8 0

if_cons2:                                         ; preds = %if_cons
  br label %if_merge4

if_alt3:                                          ; preds = %if_cons
  br label %if_merge4

if_merge4:                                        ; preds = %if_alt3, %if_cons2
  %if_phi = phi i64 [ 1, %if_cons2 ], [ 8, %if_alt3 ]
  br label %if_merge
}

define i8 @"Buffer.isEmpty_spec_enum Char(char) | String(string)_7"({ i64, ptr } %self) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %loaded_value = load { i64, ptr }, ptr %self1, align 8
  %struc.len = extractvalue { i64, ptr } %loaded_value, 0
  %"==_i1" = icmp eq i64 %struc.len, 0
  %"==_res" = sext i1 %"==_i1" to i8
  ret i8 %"==_res"
}

define { i64, ptr } @"Buffer._enlargedClone_spec_enum Char(char) | String(string)_6"({ i64, ptr } %self, i64 %newCount) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %newCount2 = alloca i64, align 8
  store i64 %newCount, ptr %newCount2, align 8
  %loaded_value = load i64, ptr %newCount2, align 8
  %0 = call { i64, ptr } @"Buffer._allocate_spec_enum Char(char) | String(string)_8"(i64 %loaded_value)
  %newBuffer = alloca { i64, ptr }, align 8
  store { i64, ptr } %0, ptr %newBuffer, align 8
  %loaded_value3 = load { i64, ptr }, ptr %self1, align 8
  %loaded_value4 = load { i64, ptr }, ptr %newBuffer, align 8
  %loaded_value5 = load { i64, ptr }, ptr %self1, align 8
  %struc.len = extractvalue { i64, ptr } %loaded_value5, 0
  %1 = call i8 @"Buffer._copyFrom_spec_enum Char(char) | String(string)_6"({ i64, ptr } %loaded_value3, { i64, ptr } %loaded_value4, i64 %struc.len)
  %loaded_value6 = load { i64, ptr }, ptr %newBuffer, align 8
  ret { i64, ptr } %loaded_value6
}

define i8 @"Buffer._copyFrom_spec_enum Char(char) | String(string)_6"({ i64, ptr } %source, { i64, ptr } %dest, i64 %count) {
entry:
  %source1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %source, ptr %source1, align 8
  %dest2 = alloca { i64, ptr }, align 8
  store { i64, ptr } %dest, ptr %dest2, align 8
  %count3 = alloca i64, align 8
  store i64 %count, ptr %count3, align 8
  %loaded_value = load i64, ptr %count3, align 8
  %loaded_value4 = load { i64, ptr }, ptr %source1, align 8
  %struc.len = extractvalue { i64, ptr } %loaded_value4, 0
  %">_i1" = icmp sgt i64 %loaded_value, %struc.len
  %">_res" = sext i1 %">_i1" to i8
  %cond_i1 = trunc i8 %">_res" to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %0 = load { { i64, ptr } }, ptr @str.4, align 8
  %struct_init_0 = insertvalue { { { i64, ptr } }, i64 } undef, { { i64, ptr } } %0, 0
  %struct_init_1 = insertvalue { { { i64, ptr } }, i64 } %struct_init_0, i64 29, 1
  %1 = call { ptr } @StringBuilder.withCapacity(i64 3)
  %__sb_1280 = alloca { ptr }, align 8
  store { ptr } %1, ptr %__sb_1280, align 8
  %loaded_value5 = load { ptr }, ptr %__sb_1280, align 8
  %2 = load { { i64, ptr } }, ptr @str.6, align 8
  %3 = call i8 @StringBuilder.putString({ ptr } %loaded_value5, { { i64, ptr } } %2)
  %loaded_value6 = load { ptr }, ptr %__sb_1280, align 8
  %loaded_value7 = load i64, ptr %count3, align 8
  %4 = call { { i64, ptr } } @Show_u64_show(i64 %loaded_value7)
  %5 = call i8 @StringBuilder.putString({ ptr } %loaded_value6, { { i64, ptr } } %4)
  %loaded_value8 = load { ptr }, ptr %__sb_1280, align 8
  %6 = load { { i64, ptr } }, ptr @str.66, align 8
  %7 = call i8 @StringBuilder.putString({ ptr } %loaded_value8, { { i64, ptr } } %6)
  %loaded_value9 = load { ptr }, ptr %__sb_1280, align 8
  %8 = call { { i64, ptr } } @StringBuilder.build({ ptr } %loaded_value9)
  call void @crash({ { { i64, ptr } }, i64 } %struct_init_1, { { i64, ptr } } %8)
  unreachable

if_alt:                                           ; preds = %entry
  br label %if_merge

if_merge:                                         ; preds = %if_alt
  %if_phi = phi i8 [ 0, %if_alt ]
  %loaded_value13 = load i64, ptr %count3, align 8
  %loaded_value14 = load { i64, ptr }, ptr %dest2, align 8
  %struc.len15 = extractvalue { i64, ptr } %loaded_value14, 0
  %">_i116" = icmp sgt i64 %loaded_value13, %struc.len15
  %">_res17" = sext i1 %">_i116" to i8
  %cond_i118 = trunc i8 %">_res17" to i1
  br i1 %cond_i118, label %if_cons10, label %if_alt11

if_cons10:                                        ; preds = %if_merge
  %9 = load { { i64, ptr } }, ptr @str.68, align 8
  %struct_init_019 = insertvalue { { { i64, ptr } }, i64 } undef, { { i64, ptr } } %9, 0
  %struct_init_120 = insertvalue { { { i64, ptr } }, i64 } %struct_init_019, i64 32, 1
  %10 = call { ptr } @StringBuilder.withCapacity(i64 3)
  %__sb_1282 = alloca { ptr }, align 8
  store { ptr } %10, ptr %__sb_1282, align 8
  %loaded_value21 = load { ptr }, ptr %__sb_1282, align 8
  %11 = load { { i64, ptr } }, ptr @str.70, align 8
  %12 = call i8 @StringBuilder.putString({ ptr } %loaded_value21, { { i64, ptr } } %11)
  %loaded_value22 = load { ptr }, ptr %__sb_1282, align 8
  %loaded_value23 = load i64, ptr %count3, align 8
  %13 = call { { i64, ptr } } @Show_u64_show(i64 %loaded_value23)
  %14 = call i8 @StringBuilder.putString({ ptr } %loaded_value22, { { i64, ptr } } %13)
  %loaded_value24 = load { ptr }, ptr %__sb_1282, align 8
  %15 = load { { i64, ptr } }, ptr @str.72, align 8
  %16 = call i8 @StringBuilder.putString({ ptr } %loaded_value24, { { i64, ptr } } %15)
  %loaded_value25 = load { ptr }, ptr %__sb_1282, align 8
  %17 = call { { i64, ptr } } @StringBuilder.build({ ptr } %loaded_value25)
  call void @crash({ { { i64, ptr } }, i64 } %struct_init_120, { { i64, ptr } } %17)
  unreachable

if_alt11:                                         ; preds = %if_merge
  br label %if_merge12

if_merge12:                                       ; preds = %if_alt11
  %if_phi26 = phi i8 [ 0, %if_alt11 ]
  %loaded_value27 = load i64, ptr %count3, align 8
  %18 = call i64 @"Buffer.bufferLenBytes_spec_enum Char(char) | String(string)_8"(i64 %loaded_value27)
  %numBytes = alloca i64, align 8
  store i64 %18, ptr %numBytes, align 8
  %loaded_value28 = load { i64, ptr }, ptr %dest2, align 8
  %struc.data = extractvalue { i64, ptr } %loaded_value28, 1
  %loaded_value29 = load { i64, ptr }, ptr %source1, align 8
  %struc.data30 = extractvalue { i64, ptr } %loaded_value29, 1
  %loaded_value31 = load i64, ptr %numBytes, align 8
  %19 = call ptr @memcpy(ptr %struc.data, ptr %struc.data30, i64 %loaded_value31)
  ret i8 0
}

define { { i64, ptr } } @Show_u64_show(i64 %self) {
entry:
  %self1 = alloca i64, align 8
  store i64 %self, ptr %self1, align 8
  %loaded_value = load i64, ptr %self1, align 8
  %"==_i1" = icmp eq i64 %loaded_value, 0
  %"==_res" = sext i1 %"==_i1" to i8
  %cond_i1 = trunc i8 %"==_res" to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %0 = load { { i64, ptr } }, ptr @str.8, align 8
  ret { { i64, ptr } } %0

if_alt:                                           ; preds = %entry
  br label %if_merge

if_merge:                                         ; preds = %if_alt
  %if_phi = phi i8 [ 0, %if_alt ]
  %loaded_value2 = load i64, ptr %self1, align 8
  %remaining = alloca i64, align 8
  store i64 %loaded_value2, ptr %remaining, align 8
  %1 = call { i64, { i64, ptr } } @List.withCapacity_spec_char_2(i64 8)
  %2 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %1, ptr %2, align 8
  %buf = alloca ptr, align 8
  store ptr %2, ptr %buf, align 8
  br label %while_cond

while_cond:                                       ; preds = %while_body, %if_merge
  %loaded_value3 = load i64, ptr %remaining, align 8
  %">_i1" = icmp sgt i64 %loaded_value3, 0
  %">_res" = sext i1 %">_i1" to i8
  %while_cond4 = trunc i8 %">_res" to i1
  br i1 %while_cond4, label %while_body, label %while_end

while_body:                                       ; preds = %while_cond
  %loaded_value5 = load i64, ptr %remaining, align 8
  %urem = urem i64 %loaded_value5, 10
  %d = alloca i64, align 8
  store i64 %urem, ptr %d, align 8
  %loaded_value6 = load i64, ptr %d, align 8
  %add = add i64 %loaded_value6, 48
  %trunc_cast = trunc i64 %add to i8
  %c = alloca i8, align 1
  store i8 %trunc_cast, ptr %c, align 1
  %loaded_value7 = load ptr, ptr %buf, align 8
  %loaded_value8 = load i8, ptr %c, align 1
  %3 = call i8 @List.push_spec_char_2(ptr %loaded_value7, i8 %loaded_value8)
  %loaded_value9 = load i64, ptr %remaining, align 8
  %udiv = udiv i64 %loaded_value9, 10
  store i64 %udiv, ptr %remaining, align 8
  br label %while_cond

while_end:                                        ; preds = %while_cond
  %loaded_value10 = load ptr, ptr %buf, align 8
  %deref = load { i64, { i64, ptr } }, ptr %loaded_value10, align 8
  %4 = call { i64, { i64, ptr } } @List.reversed_spec_char_1({ i64, { i64, ptr } } %deref)
  %rev = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %4, ptr %rev, align 8
  %loaded_value11 = load { i64, { i64, ptr } }, ptr %rev, align 8
  %5 = call { { i64, ptr } } @string.wrapList({ i64, { i64, ptr } } %loaded_value11)
  ret { { i64, ptr } } %5
}

define { i64, { i64, ptr } } @List.withCapacity_spec_char_2(i64 %cap) {
entry:
  %cap1 = alloca i64, align 8
  store i64 %cap, ptr %cap1, align 8
  %loaded_value = load i64, ptr %cap1, align 8
  %0 = call { i64, ptr } @Buffer._allocate_spec_char_7(i64 %loaded_value)
  %buffer = alloca { i64, ptr }, align 8
  store { i64, ptr } %0, ptr %buffer, align 8
  %loaded_value2 = load { i64, ptr }, ptr %buffer, align 8
  %struct_init_1 = insertvalue { i64, { i64, ptr } } { i64 0, { i64, ptr } undef }, { i64, ptr } %loaded_value2, 1
  ret { i64, { i64, ptr } } %struct_init_1
}

define { i64, ptr } @Buffer._allocate_spec_char_7(i64 %count) {
entry:
  %count1 = alloca i64, align 8
  store i64 %count, ptr %count1, align 8
  %loaded_value = load i64, ptr %count1, align 8
  %"==_i1" = icmp eq i64 %loaded_value, 0
  %"==_res" = sext i1 %"==_i1" to i8
  %cond_i1 = trunc i8 %"==_res" to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %0 = call ptr @Pointer.null()
  br label %if_merge

if_alt:                                           ; preds = %entry
  %loaded_value2 = load i64, ptr %count1, align 8
  %1 = call i64 @Buffer.bufferLenBytes_spec_char_5(i64 %loaded_value2)
  %sizeBytes = alloca i64, align 8
  store i64 %1, ptr %sizeBytes, align 8
  %loaded_value3 = load i64, ptr %sizeBytes, align 8
  %2 = call ptr @malloc(i64 %loaded_value3)
  br label %if_merge

if_merge:                                         ; preds = %if_alt, %if_cons
  %if_phi = phi ptr [ %0, %if_cons ], [ %2, %if_alt ]
  %data = alloca ptr, align 8
  store ptr %if_phi, ptr %data, align 8
  %loaded_value4 = load i64, ptr %count1, align 8
  %struct_init_0 = insertvalue { i64, ptr } undef, i64 %loaded_value4, 0
  %loaded_value5 = load ptr, ptr %data, align 8
  %struct_init_1 = insertvalue { i64, ptr } %struct_init_0, ptr %loaded_value5, 1
  ret { i64, ptr } %struct_init_1
}

define i64 @Buffer.bufferLenBytes_spec_char_5(i64 %count) {
entry:
  %count1 = alloca i64, align 8
  store i64 %count, ptr %count1, align 8
  %loaded_value = load i64, ptr %count1, align 8
  %mul = mul i64 %loaded_value, 1
  ret i64 %mul
}

define i8 @List.push_spec_char_2(ptr %self, i8 %elem) {
entry:
  %self1 = alloca ptr, align 8
  store ptr %self, ptr %self1, align 8
  %elem2 = alloca i8, align 1
  store i8 %elem, ptr %elem2, align 1
  %loaded_value = load ptr, ptr %self1, align 8
  %deref = load { i64, { i64, ptr } }, ptr %loaded_value, align 8
  %struc.len = extractvalue { i64, { i64, ptr } } %deref, 0
  %startLength = alloca i64, align 8
  store i64 %struc.len, ptr %startLength, align 8
  %loaded_value3 = load i64, ptr %startLength, align 8
  %loaded_value4 = load ptr, ptr %self1, align 8
  %deref5 = load { i64, { i64, ptr } }, ptr %loaded_value4, align 8
  %0 = call i64 @List.cap_spec_char_4({ i64, { i64, ptr } } %deref5)
  %"==_i1" = icmp eq i64 %loaded_value3, %0
  %"==_res" = sext i1 %"==_i1" to i8
  %cond_i1 = trunc i8 %"==_res" to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %loaded_value6 = load ptr, ptr %self1, align 8
  %1 = call i8 @List._grow_spec_char_3(ptr %loaded_value6)
  br label %if_merge

if_alt:                                           ; preds = %entry
  br label %if_merge

if_merge:                                         ; preds = %if_alt, %if_cons
  %if_phi = phi i8 [ %1, %if_cons ], [ 0, %if_alt ]
  %loaded_value7 = load i64, ptr %startLength, align 8
  %add = add i64 %loaded_value7, 1
  %loaded_value8 = load ptr, ptr %self1, align 8
  %struc.len9 = getelementptr inbounds { i64, { i64, ptr } }, ptr %loaded_value8, i32 0, i32 0
  store i64 %add, ptr %struc.len9, align 8
  %loaded_value10 = load ptr, ptr %self1, align 8
  %deref11 = load { i64, { i64, ptr } }, ptr %loaded_value10, align 8
  %loaded_value12 = load i64, ptr %startLength, align 8
  %loaded_value13 = load i8, ptr %elem2, align 1
  %2 = call i8 @List.set_spec_char_4({ i64, { i64, ptr } } %deref11, i64 %loaded_value12, i8 %loaded_value13)
  ret i8 0
}

define i64 @List.cap_spec_char_4({ i64, { i64, ptr } } %self) {
entry:
  %self1 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %self, ptr %self1, align 8
  %loaded_value = load { i64, { i64, ptr } }, ptr %self1, align 8
  %struc.buffer = extractvalue { i64, { i64, ptr } } %loaded_value, 1
  %struc.len = extractvalue { i64, ptr } %struc.buffer, 0
  ret i64 %struc.len
}

define i8 @List._grow_spec_char_3(ptr %self) {
entry:
  %self1 = alloca ptr, align 8
  store ptr %self, ptr %self1, align 8
  %elemSize = alloca i64, align 8
  store i64 1, ptr %elemSize, align 8
  %loaded_value = load ptr, ptr %self1, align 8
  %deref = load { i64, { i64, ptr } }, ptr %loaded_value, align 8
  %struc.buffer = extractvalue { i64, { i64, ptr } } %deref, 1
  %0 = call i8 @Buffer.isEmpty_spec_char_5({ i64, ptr } %struc.buffer)
  %cond_i1 = trunc i8 %0 to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %loaded_value5 = load i64, ptr %elemSize, align 8
  %">=_i1" = icmp sge i64 %loaded_value5, 1024
  %">=_res" = sext i1 %">=_i1" to i8
  %cond_i16 = trunc i8 %">=_res" to i1
  br i1 %cond_i16, label %if_cons2, label %if_alt3

if_alt:                                           ; preds = %entry
  %loaded_value7 = load ptr, ptr %self1, align 8
  %deref8 = load { i64, { i64, ptr } }, ptr %loaded_value7, align 8
  %1 = call i64 @List.cap_spec_char_4({ i64, { i64, ptr } } %deref8)
  %mul = mul i64 %1, 2
  br label %if_merge

if_merge:                                         ; preds = %if_alt, %if_merge4
  %if_phi9 = phi i64 [ %if_phi, %if_merge4 ], [ %mul, %if_alt ]
  %newCap = alloca i64, align 8
  store i64 %if_phi9, ptr %newCap, align 8
  %loaded_value10 = load ptr, ptr %self1, align 8
  %deref11 = load { i64, { i64, ptr } }, ptr %loaded_value10, align 8
  %struc.buffer12 = extractvalue { i64, { i64, ptr } } %deref11, 1
  %loaded_value13 = load i64, ptr %newCap, align 8
  %2 = call { i64, ptr } @Buffer._enlargedClone_spec_char_3({ i64, ptr } %struc.buffer12, i64 %loaded_value13)
  %newBuffer = alloca { i64, ptr }, align 8
  store { i64, ptr } %2, ptr %newBuffer, align 8
  %loaded_value14 = load { i64, ptr }, ptr %newBuffer, align 8
  %loaded_value15 = load ptr, ptr %self1, align 8
  %struc.buffer16 = getelementptr inbounds { i64, { i64, ptr } }, ptr %loaded_value15, i32 0, i32 1
  store { i64, ptr } %loaded_value14, ptr %struc.buffer16, align 8
  ret i8 0

if_cons2:                                         ; preds = %if_cons
  br label %if_merge4

if_alt3:                                          ; preds = %if_cons
  br label %if_merge4

if_merge4:                                        ; preds = %if_alt3, %if_cons2
  %if_phi = phi i64 [ 1, %if_cons2 ], [ 8, %if_alt3 ]
  br label %if_merge
}

define i8 @Buffer.isEmpty_spec_char_5({ i64, ptr } %self) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %loaded_value = load { i64, ptr }, ptr %self1, align 8
  %struc.len = extractvalue { i64, ptr } %loaded_value, 0
  %"==_i1" = icmp eq i64 %struc.len, 0
  %"==_res" = sext i1 %"==_i1" to i8
  ret i8 %"==_res"
}

define { i64, ptr } @Buffer._enlargedClone_spec_char_3({ i64, ptr } %self, i64 %newCount) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %newCount2 = alloca i64, align 8
  store i64 %newCount, ptr %newCount2, align 8
  %loaded_value = load i64, ptr %newCount2, align 8
  %0 = call { i64, ptr } @Buffer._allocate_spec_char_7(i64 %loaded_value)
  %newBuffer = alloca { i64, ptr }, align 8
  store { i64, ptr } %0, ptr %newBuffer, align 8
  %loaded_value3 = load { i64, ptr }, ptr %self1, align 8
  %loaded_value4 = load { i64, ptr }, ptr %newBuffer, align 8
  %loaded_value5 = load { i64, ptr }, ptr %self1, align 8
  %struc.len = extractvalue { i64, ptr } %loaded_value5, 0
  %1 = call i8 @Buffer._copyFrom_spec_char_3({ i64, ptr } %loaded_value3, { i64, ptr } %loaded_value4, i64 %struc.len)
  %loaded_value6 = load { i64, ptr }, ptr %newBuffer, align 8
  ret { i64, ptr } %loaded_value6
}

define i8 @Buffer._copyFrom_spec_char_3({ i64, ptr } %source, { i64, ptr } %dest, i64 %count) {
entry:
  %source1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %source, ptr %source1, align 8
  %dest2 = alloca { i64, ptr }, align 8
  store { i64, ptr } %dest, ptr %dest2, align 8
  %count3 = alloca i64, align 8
  store i64 %count, ptr %count3, align 8
  %loaded_value = load i64, ptr %count3, align 8
  %loaded_value4 = load { i64, ptr }, ptr %source1, align 8
  %struc.len = extractvalue { i64, ptr } %loaded_value4, 0
  %">_i1" = icmp sgt i64 %loaded_value, %struc.len
  %">_res" = sext i1 %">_i1" to i8
  %cond_i1 = trunc i8 %">_res" to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %0 = load { { i64, ptr } }, ptr @str.10, align 8
  %struct_init_0 = insertvalue { { { i64, ptr } }, i64 } undef, { { i64, ptr } } %0, 0
  %struct_init_1 = insertvalue { { { i64, ptr } }, i64 } %struct_init_0, i64 29, 1
  %1 = call { ptr } @StringBuilder.withCapacity(i64 3)
  %__sb_1176 = alloca { ptr }, align 8
  store { ptr } %1, ptr %__sb_1176, align 8
  %loaded_value5 = load { ptr }, ptr %__sb_1176, align 8
  %2 = load { { i64, ptr } }, ptr @str.12, align 8
  %3 = call i8 @StringBuilder.putString({ ptr } %loaded_value5, { { i64, ptr } } %2)
  %loaded_value6 = load { ptr }, ptr %__sb_1176, align 8
  %loaded_value7 = load i64, ptr %count3, align 8
  %4 = call { { i64, ptr } } @Show_u64_show(i64 %loaded_value7)
  %5 = call i8 @StringBuilder.putString({ ptr } %loaded_value6, { { i64, ptr } } %4)
  %loaded_value8 = load { ptr }, ptr %__sb_1176, align 8
  %6 = load { { i64, ptr } }, ptr @str.14, align 8
  %7 = call i8 @StringBuilder.putString({ ptr } %loaded_value8, { { i64, ptr } } %6)
  %loaded_value9 = load { ptr }, ptr %__sb_1176, align 8
  %8 = call { { i64, ptr } } @StringBuilder.build({ ptr } %loaded_value9)
  call void @crash({ { { i64, ptr } }, i64 } %struct_init_1, { { i64, ptr } } %8)
  unreachable

if_alt:                                           ; preds = %entry
  br label %if_merge

if_merge:                                         ; preds = %if_alt
  %if_phi = phi i8 [ 0, %if_alt ]
  %loaded_value13 = load i64, ptr %count3, align 8
  %loaded_value14 = load { i64, ptr }, ptr %dest2, align 8
  %struc.len15 = extractvalue { i64, ptr } %loaded_value14, 0
  %">_i116" = icmp sgt i64 %loaded_value13, %struc.len15
  %">_res17" = sext i1 %">_i116" to i8
  %cond_i118 = trunc i8 %">_res17" to i1
  br i1 %cond_i118, label %if_cons10, label %if_alt11

if_cons10:                                        ; preds = %if_merge
  %9 = load { { i64, ptr } }, ptr @str.60, align 8
  %struct_init_019 = insertvalue { { { i64, ptr } }, i64 } undef, { { i64, ptr } } %9, 0
  %struct_init_120 = insertvalue { { { i64, ptr } }, i64 } %struct_init_019, i64 32, 1
  %10 = call { ptr } @StringBuilder.withCapacity(i64 3)
  %__sb_1178 = alloca { ptr }, align 8
  store { ptr } %10, ptr %__sb_1178, align 8
  %loaded_value21 = load { ptr }, ptr %__sb_1178, align 8
  %11 = load { { i64, ptr } }, ptr @str.62, align 8
  %12 = call i8 @StringBuilder.putString({ ptr } %loaded_value21, { { i64, ptr } } %11)
  %loaded_value22 = load { ptr }, ptr %__sb_1178, align 8
  %loaded_value23 = load i64, ptr %count3, align 8
  %13 = call { { i64, ptr } } @Show_u64_show(i64 %loaded_value23)
  %14 = call i8 @StringBuilder.putString({ ptr } %loaded_value22, { { i64, ptr } } %13)
  %loaded_value24 = load { ptr }, ptr %__sb_1178, align 8
  %15 = load { { i64, ptr } }, ptr @str.64, align 8
  %16 = call i8 @StringBuilder.putString({ ptr } %loaded_value24, { { i64, ptr } } %15)
  %loaded_value25 = load { ptr }, ptr %__sb_1178, align 8
  %17 = call { { i64, ptr } } @StringBuilder.build({ ptr } %loaded_value25)
  call void @crash({ { { i64, ptr } }, i64 } %struct_init_120, { { i64, ptr } } %17)
  unreachable

if_alt11:                                         ; preds = %if_merge
  br label %if_merge12

if_merge12:                                       ; preds = %if_alt11
  %if_phi26 = phi i8 [ 0, %if_alt11 ]
  %loaded_value27 = load i64, ptr %count3, align 8
  %18 = call i64 @Buffer.bufferLenBytes_spec_char_5(i64 %loaded_value27)
  %numBytes = alloca i64, align 8
  store i64 %18, ptr %numBytes, align 8
  %loaded_value28 = load { i64, ptr }, ptr %dest2, align 8
  %struc.data = extractvalue { i64, ptr } %loaded_value28, 1
  %loaded_value29 = load { i64, ptr }, ptr %source1, align 8
  %struc.data30 = extractvalue { i64, ptr } %loaded_value29, 1
  %loaded_value31 = load i64, ptr %numBytes, align 8
  %19 = call ptr @memcpy(ptr %struc.data, ptr %struc.data30, i64 %loaded_value31)
  ret i8 0
}

define { { i64, ptr } } @StringBuilder.build({ ptr } %self) {
entry:
  %self1 = alloca { ptr }, align 8
  store { ptr } %self, ptr %self1, align 8
  %loaded_value = load { ptr }, ptr %self1, align 8
  %0 = call i64 @StringBuilder.len({ ptr } %loaded_value)
  %1 = call { i64, { i64, ptr } } @List.withCapacity_spec_char_2(i64 %0)
  %2 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %1, ptr %2, align 8
  %bytes = alloca ptr, align 8
  store ptr %2, ptr %bytes, align 8
  %itIndex = alloca i64, align 8
  store i64 0, ptr %itIndex, align 8
  %loaded_value2 = load { ptr }, ptr %self1, align 8
  %struc.parts = extractvalue { ptr } %loaded_value2, 0
  %deref = load { i64, { i64, ptr } }, ptr %struc.parts, align 8
  %3 = call { { i64, ptr }, i64 } @"Iterable_List[enum Char(char) | String(string)]_iterator"({ i64, { i64, ptr } } %deref)
  %4 = alloca { { i64, ptr }, i64 }, align 8
  store { { i64, ptr }, i64 } %3, ptr %4, align 8
  %__iter_918 = alloca ptr, align 8
  store ptr %4, ptr %__iter_918, align 8
  %break = alloca i8, align 1
  br label %loop_body

loop_body:                                        ; preds = %if_merge, %entry
  %loaded_value3 = load ptr, ptr %__iter_918, align 8
  %5 = call %Opt @"Iterator_BufferIterator[enum Char(char) | String(string)]_next"(ptr %loaded_value3)
  %__next_919 = alloca %Opt, align 8
  store %Opt %5, ptr %__next_919, align 8
  %loaded_value4 = load %Opt, ptr %__next_919, align 8
  %6 = call i8 @"Opt.isSome_spec_enum Char(char) | String(string)_8"(%Opt %loaded_value4)
  %cond_i1 = trunc i8 %6 to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

loop_end:                                         ; preds = %if_alt
  %loop_value52 = load i8, ptr %break, align 1
  %loaded_value53 = load ptr, ptr %bytes, align 8
  %deref54 = load { i64, { i64, ptr } }, ptr %loaded_value53, align 8
  %7 = call { { i64, ptr } } @string.wrapList({ i64, { i64, ptr } } %deref54)
  ret { { i64, ptr } } %7

if_cons:                                          ; preds = %loop_body
  %loaded_value5 = load %Opt, ptr %__next_919, align 8
  %8 = call %"46.5" @"Unwrap_Opt[enum Char(char) | String(string)]_unwrap"(%Opt %loaded_value5)
  %part = alloca %"46.5", align 8
  store %"46.5" %8, ptr %part, align 8
  %loaded_value6 = load %"46.5", ptr %part, align 8
  %__match_target_920 = alloca %"46.5", align 8
  store %"46.5" %loaded_value6, ptr %__match_target_920, align 8
  %loaded_value7 = load %"46.5", ptr %__match_target_920, align 8
  %enum_ptr_for_payload = alloca %"46.Char.3", align 8
  store %"46.5" %loaded_value7, ptr %enum_ptr_for_payload, align 8
  %get_payload_ptr = getelementptr inbounds %"46.Char.3", ptr %enum_ptr_for_payload, i32 0, i32 1
  %get_payload = load i8, ptr %get_payload_ptr, align 1
  %__payload_922 = alloca i8, align 1
  store i8 %get_payload, ptr %__payload_922, align 1
  %loaded_value8 = load %"46.5", ptr %__match_target_920, align 8
  %enum_ptr_for_payload9 = alloca %"46.String.4", align 8
  store %"46.5" %loaded_value8, ptr %enum_ptr_for_payload9, align 8
  %get_payload_ptr10 = getelementptr inbounds %"46.String.4", ptr %enum_ptr_for_payload9, i32 0, i32 1
  %get_payload11 = load { { i64, ptr } }, ptr %get_payload_ptr10, align 8
  %__payload_923 = alloca { { i64, ptr } }, align 8
  store { { i64, ptr } } %get_payload11, ptr %__payload_923, align 8
  %loaded_value15 = load %"46.5", ptr %__match_target_920, align 8
  %get_tag = extractvalue %"46.5" %loaded_value15, 0
  %get_tag16 = extractvalue %"46.String.4" %get_tag, 0
  %is_variant_cmp = icmp eq i64 %get_tag16, 0
  %is_variant = sext i1 %is_variant_cmp to i8
  %cond_i117 = trunc i8 %is_variant to i1
  br i1 %cond_i117, label %if_cons12, label %if_alt13

if_alt:                                           ; preds = %loop_body
  store i8 0, ptr %break, align 1
  br label %loop_end

if_merge:                                         ; preds = %if_merge14
  %if_phi49 = phi i8 [ 0, %if_merge14 ]
  %loaded_value50 = load i64, ptr %itIndex, align 8
  %add51 = add i64 %loaded_value50, 1
  store i64 %add51, ptr %itIndex, align 8
  br label %loop_body

if_cons12:                                        ; preds = %if_cons
  %loaded_value18 = load i8, ptr %__payload_922, align 1
  %c = alloca i8, align 1
  store i8 %loaded_value18, ptr %c, align 1
  %loaded_value19 = load ptr, ptr %bytes, align 8
  %loaded_value20 = load i8, ptr %c, align 1
  %9 = call i8 @List.push_spec_char_2(ptr %loaded_value19, i8 %loaded_value20)
  br label %if_merge14

if_alt13:                                         ; preds = %if_cons
  %loaded_value24 = load %"46.5", ptr %__match_target_920, align 8
  %get_tag25 = extractvalue %"46.5" %loaded_value24, 0
  %get_tag26 = extractvalue %"46.String.4" %get_tag25, 0
  %is_variant_cmp27 = icmp eq i64 %get_tag26, 1
  %is_variant28 = sext i1 %is_variant_cmp27 to i8
  %cond_i129 = trunc i8 %is_variant28 to i1
  br i1 %cond_i129, label %if_cons21, label %if_alt22

if_merge14:                                       ; preds = %if_merge23, %if_cons12
  %if_phi48 = phi i8 [ %9, %if_cons12 ], [ %if_phi47, %if_merge23 ]
  %__block_expr_val_920 = alloca i8, align 1
  store i8 %if_phi48, ptr %__block_expr_val_920, align 1
  br label %if_merge

if_cons21:                                        ; preds = %if_alt13
  %loaded_value30 = load { { i64, ptr } }, ptr %__payload_923, align 8
  %s = alloca { { i64, ptr } }, align 8
  store { { i64, ptr } } %loaded_value30, ptr %s, align 8
  %itIndex31 = alloca i64, align 8
  store i64 0, ptr %itIndex31, align 8
  %loaded_value32 = load { { i64, ptr } }, ptr %s, align 8
  %10 = call { { i64, ptr }, i64 } @Iterable_string_iterator({ { i64, ptr } } %loaded_value32)
  %11 = alloca { { i64, ptr }, i64 }, align 8
  store { { i64, ptr }, i64 } %10, ptr %11, align 8
  %__iter_925 = alloca ptr, align 8
  store ptr %11, ptr %__iter_925, align 8
  %break35 = alloca i8, align 1
  br label %loop_body33

if_alt22:                                         ; preds = %if_alt13
  %12 = load { { i64, ptr } }, ptr @str.50, align 8
  %struct_init_0 = insertvalue { { { i64, ptr } }, i64 } undef, { { i64, ptr } } %12, 0
  %struct_init_1 = insertvalue { { { i64, ptr } }, i64 } %struct_init_0, i64 36, 1
  %13 = load { { i64, ptr } }, ptr @str.52, align 8
  call void @crash({ { { i64, ptr } }, i64 } %struct_init_1, { { i64, ptr } } %13)
  unreachable

if_merge23:                                       ; preds = %loop_end34
  %if_phi47 = phi i8 [ %loop_value, %loop_end34 ]
  br label %if_merge14

loop_body33:                                      ; preds = %if_merge39, %if_cons21
  %loaded_value36 = load ptr, ptr %__iter_925, align 8
  %14 = call %Opt.8 @"Iterator_BufferIterator[char]_next"(ptr %loaded_value36)
  %__next_926 = alloca %Opt.8, align 8
  store %Opt.8 %14, ptr %__next_926, align 1
  %loaded_value40 = load %Opt.8, ptr %__next_926, align 1
  %15 = call i8 @Opt.isSome_spec_char_6(%Opt.8 %loaded_value40)
  %cond_i141 = trunc i8 %15 to i1
  br i1 %cond_i141, label %if_cons37, label %if_alt38

loop_end34:                                       ; preds = %if_alt38
  %loop_value = load i8, ptr %break35, align 1
  br label %if_merge23

if_cons37:                                        ; preds = %loop_body33
  %loaded_value42 = load %Opt.8, ptr %__next_926, align 1
  %16 = call i8 @"Unwrap_Opt[char]_unwrap"(%Opt.8 %loaded_value42)
  %c43 = alloca i8, align 1
  store i8 %16, ptr %c43, align 1
  %loaded_value44 = load ptr, ptr %bytes, align 8
  %loaded_value45 = load i8, ptr %c43, align 1
  %17 = call i8 @List.push_spec_char_2(ptr %loaded_value44, i8 %loaded_value45)
  %__block_expr_val_927 = alloca i8, align 1
  store i8 %17, ptr %__block_expr_val_927, align 1
  br label %if_merge39

if_alt38:                                         ; preds = %loop_body33
  store i8 0, ptr %break35, align 1
  br label %loop_end34

if_merge39:                                       ; preds = %if_cons37
  %if_phi = phi i8 [ 0, %if_cons37 ]
  %loaded_value46 = load i64, ptr %itIndex31, align 8
  %add = add i64 %loaded_value46, 1
  store i64 %add, ptr %itIndex31, align 8
  br label %loop_body33
}

define i64 @StringBuilder.len({ ptr } %self) {
entry:
  %self1 = alloca { ptr }, align 8
  store { ptr } %self, ptr %self1, align 8
  %len = alloca i64, align 8
  store i64 0, ptr %len, align 8
  %itIndex = alloca i64, align 8
  store i64 0, ptr %itIndex, align 8
  %loaded_value = load { ptr }, ptr %self1, align 8
  %struc.parts = extractvalue { ptr } %loaded_value, 0
  %deref = load { i64, { i64, ptr } }, ptr %struc.parts, align 8
  %0 = call { { i64, ptr }, i64 } @"Iterable_List[enum Char(char) | String(string)]_iterator"({ i64, { i64, ptr } } %deref)
  %1 = alloca { { i64, ptr }, i64 }, align 8
  store { { i64, ptr }, i64 } %0, ptr %1, align 8
  %__iter_899 = alloca ptr, align 8
  store ptr %1, ptr %__iter_899, align 8
  %break = alloca i8, align 1
  br label %loop_body

loop_body:                                        ; preds = %if_merge, %entry
  %loaded_value2 = load ptr, ptr %__iter_899, align 8
  %2 = call %Opt @"Iterator_BufferIterator[enum Char(char) | String(string)]_next"(ptr %loaded_value2)
  %__next_900 = alloca %Opt, align 8
  store %Opt %2, ptr %__next_900, align 8
  %loaded_value3 = load %Opt, ptr %__next_900, align 8
  %3 = call i8 @"Opt.isSome_spec_enum Char(char) | String(string)_8"(%Opt %loaded_value3)
  %cond_i1 = trunc i8 %3 to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

loop_end:                                         ; preds = %if_alt
  %loop_value = load i8, ptr %break, align 1
  %loaded_value34 = load i64, ptr %len, align 8
  ret i64 %loaded_value34

if_cons:                                          ; preds = %loop_body
  %loaded_value4 = load %Opt, ptr %__next_900, align 8
  %4 = call %"46.5" @"Unwrap_Opt[enum Char(char) | String(string)]_unwrap"(%Opt %loaded_value4)
  %part = alloca %"46.5", align 8
  store %"46.5" %4, ptr %part, align 8
  %loaded_value5 = load %"46.5", ptr %part, align 8
  %__match_target_901 = alloca %"46.5", align 8
  store %"46.5" %loaded_value5, ptr %__match_target_901, align 8
  %loaded_value6 = load %"46.5", ptr %__match_target_901, align 8
  %enum_ptr_for_payload = alloca %"46.Char.3", align 8
  store %"46.5" %loaded_value6, ptr %enum_ptr_for_payload, align 8
  %get_payload_ptr = getelementptr inbounds %"46.Char.3", ptr %enum_ptr_for_payload, i32 0, i32 1
  %get_payload = load i8, ptr %get_payload_ptr, align 1
  %__payload_914 = alloca i8, align 1
  store i8 %get_payload, ptr %__payload_914, align 1
  %loaded_value7 = load %"46.5", ptr %__match_target_901, align 8
  %enum_ptr_for_payload8 = alloca %"46.String.4", align 8
  store %"46.5" %loaded_value7, ptr %enum_ptr_for_payload8, align 8
  %get_payload_ptr9 = getelementptr inbounds %"46.String.4", ptr %enum_ptr_for_payload8, i32 0, i32 1
  %get_payload10 = load { { i64, ptr } }, ptr %get_payload_ptr9, align 8
  %__payload_915 = alloca { { i64, ptr } }, align 8
  store { { i64, ptr } } %get_payload10, ptr %__payload_915, align 8
  %loaded_value14 = load %"46.5", ptr %__match_target_901, align 8
  %get_tag = extractvalue %"46.5" %loaded_value14, 0
  %get_tag15 = extractvalue %"46.String.4" %get_tag, 0
  %is_variant_cmp = icmp eq i64 %get_tag15, 0
  %is_variant = sext i1 %is_variant_cmp to i8
  %cond_i116 = trunc i8 %is_variant to i1
  br i1 %cond_i116, label %if_cons11, label %if_alt12

if_alt:                                           ; preds = %loop_body
  store i8 0, ptr %break, align 1
  br label %loop_end

if_merge:                                         ; preds = %if_merge13
  %if_phi31 = phi i8 [ 0, %if_merge13 ]
  %loaded_value32 = load i64, ptr %itIndex, align 8
  %add33 = add i64 %loaded_value32, 1
  store i64 %add33, ptr %itIndex, align 8
  br label %loop_body

if_cons11:                                        ; preds = %if_cons
  br label %if_merge13

if_alt12:                                         ; preds = %if_cons
  %loaded_value20 = load %"46.5", ptr %__match_target_901, align 8
  %get_tag21 = extractvalue %"46.5" %loaded_value20, 0
  %get_tag22 = extractvalue %"46.String.4" %get_tag21, 0
  %is_variant_cmp23 = icmp eq i64 %get_tag22, 1
  %is_variant24 = sext i1 %is_variant_cmp23 to i8
  %cond_i125 = trunc i8 %is_variant24 to i1
  br i1 %cond_i125, label %if_cons17, label %if_alt18

if_merge13:                                       ; preds = %if_merge19, %if_cons11
  %if_phi28 = phi i64 [ 1, %if_cons11 ], [ %if_phi, %if_merge19 ]
  %partLen = alloca i64, align 8
  store i64 %if_phi28, ptr %partLen, align 8
  %loaded_value29 = load i64, ptr %len, align 8
  %loaded_value30 = load i64, ptr %partLen, align 8
  %add = add i64 %loaded_value29, %loaded_value30
  store i64 %add, ptr %len, align 8
  %__block_expr_val_901 = alloca i8, align 1
  store i8 0, ptr %__block_expr_val_901, align 1
  br label %if_merge

if_cons17:                                        ; preds = %if_alt12
  %loaded_value26 = load { { i64, ptr } }, ptr %__payload_915, align 8
  %s = alloca { { i64, ptr } }, align 8
  store { { i64, ptr } } %loaded_value26, ptr %s, align 8
  %loaded_value27 = load { { i64, ptr } }, ptr %s, align 8
  %5 = call i64 @string.len({ { i64, ptr } } %loaded_value27)
  br label %if_merge19

if_alt18:                                         ; preds = %if_alt12
  %6 = load { { i64, ptr } }, ptr @str.38, align 8
  %struct_init_0 = insertvalue { { { i64, ptr } }, i64 } undef, { { i64, ptr } } %6, 0
  %struct_init_1 = insertvalue { { { i64, ptr } }, i64 } %struct_init_0, i64 26, 1
  %7 = load { { i64, ptr } }, ptr @str.40, align 8
  call void @crash({ { { i64, ptr } }, i64 } %struct_init_1, { { i64, ptr } } %7)
  unreachable

if_merge19:                                       ; preds = %if_cons17
  %if_phi = phi i64 [ %5, %if_cons17 ]
  br label %if_merge13
}

define { { i64, ptr }, i64 } @"Iterable_List[enum Char(char) | String(string)]_iterator"({ i64, { i64, ptr } } %self) {
entry:
  %self1 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %self, ptr %self1, align 8
  %loaded_value = load { i64, { i64, ptr } }, ptr %self1, align 8
  %0 = call { i64, ptr } @"List.toBuffer_spec_enum Char(char) | String(string)_12"({ i64, { i64, ptr } } %loaded_value)
  %1 = call { { i64, ptr }, i64 } @"Iterable_Buffer[enum Char(char) | String(string)]_iterator"({ i64, ptr } %0)
  ret { { i64, ptr }, i64 } %1
}

define { i64, ptr } @"List.toBuffer_spec_enum Char(char) | String(string)_12"({ i64, { i64, ptr } } %self) {
entry:
  %self1 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %self, ptr %self1, align 8
  %loaded_value = load { i64, { i64, ptr } }, ptr %self1, align 8
  %struc.buffer = extractvalue { i64, { i64, ptr } } %loaded_value, 1
  %loaded_value2 = load { i64, { i64, ptr } }, ptr %self1, align 8
  %struc.len = extractvalue { i64, { i64, ptr } } %loaded_value2, 0
  %0 = call { i64, ptr } @"Buffer.slice_spec_enum Char(char) | String(string)_11"({ i64, ptr } %struc.buffer, i64 0, i64 %struc.len)
  ret { i64, ptr } %0
}

define { i64, ptr } @"Buffer.slice_spec_enum Char(char) | String(string)_11"({ i64, ptr } %self, i64 %start, i64 %end) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %start2 = alloca i64, align 8
  store i64 %start, ptr %start2, align 8
  %end3 = alloca i64, align 8
  store i64 %end, ptr %end3, align 8
  %loaded_value = load i64, ptr %end3, align 8
  %loaded_value4 = load i64, ptr %start2, align 8
  %"<_i1" = icmp slt i64 %loaded_value, %loaded_value4
  %"<_res" = sext i1 %"<_i1" to i8
  %loaded_value5 = load { i64, ptr }, ptr %self1, align 8
  %struc.len = extractvalue { i64, ptr } %loaded_value5, 0
  %"==_i1" = icmp eq i64 %struc.len, 0
  %"==_res" = sext i1 %"==_i1" to i8
  %bool_or = or i8 %"<_res", %"==_res"
  %cond_i1 = trunc i8 %bool_or to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %0 = call { i64, ptr } @"Buffer.empty_spec_enum Char(char) | String(string)_8"()
  ret { i64, ptr } %0

if_alt:                                           ; preds = %entry
  br label %if_merge

if_merge:                                         ; preds = %if_alt
  %if_phi = phi i8 [ 0, %if_alt ]
  %loaded_value9 = load i64, ptr %start2, align 8
  %loaded_value10 = load { i64, ptr }, ptr %self1, align 8
  %struc.len11 = extractvalue { i64, ptr } %loaded_value10, 0
  %">=_i1" = icmp sge i64 %loaded_value9, %struc.len11
  %">=_res" = sext i1 %">=_i1" to i8
  %loaded_value12 = load i64, ptr %start2, align 8
  %"<_i113" = icmp slt i64 %loaded_value12, 0
  %"<_res14" = sext i1 %"<_i113" to i8
  %bool_or15 = or i8 %">=_res", %"<_res14"
  %cond_i116 = trunc i8 %bool_or15 to i1
  br i1 %cond_i116, label %if_cons6, label %if_alt7

if_cons6:                                         ; preds = %if_merge
  %1 = load { { i64, ptr } }, ptr @str.16, align 8
  %struct_init_0 = insertvalue { { { i64, ptr } }, i64 } undef, { { i64, ptr } } %1, 0
  %struct_init_1 = insertvalue { { { i64, ptr } }, i64 } %struct_init_0, i64 67, 1
  %2 = call { ptr } @StringBuilder.withCapacity(i64 3)
  %__sb_1257 = alloca { ptr }, align 8
  store { ptr } %2, ptr %__sb_1257, align 8
  %loaded_value17 = load { ptr }, ptr %__sb_1257, align 8
  %3 = load { { i64, ptr } }, ptr @str.18, align 8
  %4 = call i8 @StringBuilder.putString({ ptr } %loaded_value17, { { i64, ptr } } %3)
  %loaded_value18 = load { ptr }, ptr %__sb_1257, align 8
  %loaded_value19 = load i64, ptr %start2, align 8
  %5 = call { { i64, ptr } } @Show_u64_show(i64 %loaded_value19)
  %6 = call i8 @StringBuilder.putString({ ptr } %loaded_value18, { { i64, ptr } } %5)
  %loaded_value20 = load { ptr }, ptr %__sb_1257, align 8
  %7 = load { { i64, ptr } }, ptr @str.20, align 8
  %8 = call i8 @StringBuilder.putString({ ptr } %loaded_value20, { { i64, ptr } } %7)
  %loaded_value21 = load { ptr }, ptr %__sb_1257, align 8
  %9 = call { { i64, ptr } } @StringBuilder.build({ ptr } %loaded_value21)
  call void @crash({ { { i64, ptr } }, i64 } %struct_init_1, { { i64, ptr } } %9)
  unreachable

if_alt7:                                          ; preds = %if_merge
  br label %if_merge8

if_merge8:                                        ; preds = %if_alt7
  %if_phi22 = phi i8 [ 0, %if_alt7 ]
  %loaded_value26 = load i64, ptr %end3, align 8
  %loaded_value27 = load { i64, ptr }, ptr %self1, align 8
  %struc.len28 = extractvalue { i64, ptr } %loaded_value27, 0
  %">_i1" = icmp sgt i64 %loaded_value26, %struc.len28
  %">_res" = sext i1 %">_i1" to i8
  %cond_i129 = trunc i8 %">_res" to i1
  br i1 %cond_i129, label %if_cons23, label %if_alt24

if_cons23:                                        ; preds = %if_merge8
  %loaded_value30 = load { i64, ptr }, ptr %self1, align 8
  %struc.len31 = extractvalue { i64, ptr } %loaded_value30, 0
  br label %if_merge25

if_alt24:                                         ; preds = %if_merge8
  %loaded_value32 = load i64, ptr %end3, align 8
  br label %if_merge25

if_merge25:                                       ; preds = %if_alt24, %if_cons23
  %if_phi33 = phi i64 [ %struc.len31, %if_cons23 ], [ %loaded_value32, %if_alt24 ]
  %end34 = alloca i64, align 8
  store i64 %if_phi33, ptr %end34, align 8
  %loaded_value35 = load i64, ptr %end34, align 8
  %loaded_value36 = load i64, ptr %start2, align 8
  %sub = sub i64 %loaded_value35, %loaded_value36
  %newLen = alloca i64, align 8
  store i64 %sub, ptr %newLen, align 8
  %loaded_value37 = load { i64, ptr }, ptr %self1, align 8
  %loaded_value38 = load i64, ptr %start2, align 8
  %10 = call ptr @"Buffer.getRef_spec_enum Char(char) | String(string)_15"({ i64, ptr } %loaded_value37, i64 %loaded_value38)
  %newBase = alloca ptr, align 8
  store ptr %10, ptr %newBase, align 8
  %loaded_value39 = load i64, ptr %newLen, align 8
  %struct_init_040 = insertvalue { i64, ptr } undef, i64 %loaded_value39, 0
  %loaded_value41 = load ptr, ptr %newBase, align 8
  %struct_init_142 = insertvalue { i64, ptr } %struct_init_040, ptr %loaded_value41, 1
  ret { i64, ptr } %struct_init_142
}

define { i64, ptr } @"Buffer.empty_spec_enum Char(char) | String(string)_8"() {
entry:
  %0 = call { i64, ptr } @"Buffer._allocate_spec_enum Char(char) | String(string)_8"(i64 0)
  ret { i64, ptr } %0
}

define void @crash({ { { i64, ptr } }, i64 } %locn, { { i64, ptr } } %msg) {
entry:
  %locn1 = alloca { { { i64, ptr } }, i64 }, align 8
  store { { { i64, ptr } }, i64 } %locn, ptr %locn1, align 8
  %msg2 = alloca { { i64, ptr } }, align 8
  store { { i64, ptr } } %msg, ptr %msg2, align 8
  %loaded_value = load { { { i64, ptr } }, i64 }, ptr %locn1, align 8
  %struc.filename = extractvalue { { { i64, ptr } }, i64 } %loaded_value, 0
  %filename = alloca { { i64, ptr } }, align 8
  store { { i64, ptr } } %struc.filename, ptr %filename, align 8
  %loaded_value3 = load { { { i64, ptr } }, i64 }, ptr %locn1, align 8
  %struc.line = extractvalue { { { i64, ptr } }, i64 } %loaded_value3, 1
  %line = alloca i64, align 8
  store i64 %struc.line, ptr %line, align 8
  %0 = call { ptr } @StringBuilder.withCapacity(i64 7)
  %__sb_531 = alloca { ptr }, align 8
  store { ptr } %0, ptr %__sb_531, align 8
  %loaded_value4 = load { ptr }, ptr %__sb_531, align 8
  %1 = load { { i64, ptr } }, ptr @str.22, align 8
  %2 = call i8 @StringBuilder.putString({ ptr } %loaded_value4, { { i64, ptr } } %1)
  %loaded_value5 = load { ptr }, ptr %__sb_531, align 8
  %loaded_value6 = load { { i64, ptr } }, ptr %msg2, align 8
  %3 = call { { i64, ptr } } @Show_string_show({ { i64, ptr } } %loaded_value6)
  %4 = call i8 @StringBuilder.putString({ ptr } %loaded_value5, { { i64, ptr } } %3)
  %loaded_value7 = load { ptr }, ptr %__sb_531, align 8
  %5 = load { { i64, ptr } }, ptr @str.24, align 8
  %6 = call i8 @StringBuilder.putString({ ptr } %loaded_value7, { { i64, ptr } } %5)
  %loaded_value8 = load { ptr }, ptr %__sb_531, align 8
  %loaded_value9 = load { { i64, ptr } }, ptr %filename, align 8
  %7 = call { { i64, ptr } } @Show_string_show({ { i64, ptr } } %loaded_value9)
  %8 = call i8 @StringBuilder.putString({ ptr } %loaded_value8, { { i64, ptr } } %7)
  %loaded_value10 = load { ptr }, ptr %__sb_531, align 8
  %9 = load { { i64, ptr } }, ptr @str.26, align 8
  %10 = call i8 @StringBuilder.putString({ ptr } %loaded_value10, { { i64, ptr } } %9)
  %loaded_value11 = load { ptr }, ptr %__sb_531, align 8
  %loaded_value12 = load i64, ptr %line, align 8
  %11 = call { { i64, ptr } } @Show_u64_show(i64 %loaded_value12)
  %12 = call i8 @StringBuilder.putString({ ptr } %loaded_value11, { { i64, ptr } } %11)
  %loaded_value13 = load { ptr }, ptr %__sb_531, align 8
  %13 = load { { i64, ptr } }, ptr @str.28, align 8
  %14 = call i8 @StringBuilder.putString({ ptr } %loaded_value13, { { i64, ptr } } %13)
  %loaded_value14 = load { ptr }, ptr %__sb_531, align 8
  %15 = call { { i64, ptr } } @StringBuilder.build({ ptr } %loaded_value14)
  %s = alloca { { i64, ptr } }, align 8
  store { { i64, ptr } } %15, ptr %s, align 8
  %loaded_value15 = load { { i64, ptr } }, ptr %s, align 8
  %16 = call i8 @eprint({ { i64, ptr } } %loaded_value15)
  call void @abort()
  unreachable
}

define { { i64, ptr } } @Show_string_show({ { i64, ptr } } %self) {
entry:
  %self1 = alloca { { i64, ptr } }, align 8
  store { { i64, ptr } } %self, ptr %self1, align 8
  %loaded_value = load { { i64, ptr } }, ptr %self1, align 8
  ret { { i64, ptr } } %loaded_value
}

define i8 @eprint({ { i64, ptr } } %s) {
entry:
  %s1 = alloca { { i64, ptr } }, align 8
  store { { i64, ptr } } %s, ptr %s1, align 8
  %loaded_value = load { { i64, ptr } }, ptr %s1, align 8
  %struc.buffer = extractvalue { { i64, ptr } } %loaded_value, 0
  %struc.data = extractvalue { i64, ptr } %struc.buffer, 1
  %loaded_value2 = load { { i64, ptr } }, ptr %s1, align 8
  %0 = call i64 @string.len({ { i64, ptr } } %loaded_value2)
  %1 = call i64 @write(i32 2, ptr %struc.data, i64 %0, i64 0)
  ret i8 0
}

define i64 @string.len({ { i64, ptr } } %self) {
entry:
  %self1 = alloca { { i64, ptr } }, align 8
  store { { i64, ptr } } %self, ptr %self1, align 8
  %loaded_value = load { { i64, ptr } }, ptr %self1, align 8
  %struc.buffer = extractvalue { { i64, ptr } } %loaded_value, 0
  %struc.len = extractvalue { i64, ptr } %struc.buffer, 0
  ret i64 %struc.len
}

declare i64 @write(i32, ptr, i64, i64)

declare void @abort()

define ptr @"Buffer.getRef_spec_enum Char(char) | String(string)_15"({ i64, ptr } %self, i64 %index) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %index2 = alloca i64, align 8
  store i64 %index, ptr %index2, align 8
  %loaded_value = load i64, ptr %index2, align 8
  %loaded_value3 = load { i64, ptr }, ptr %self1, align 8
  %struc.len = extractvalue { i64, ptr } %loaded_value3, 0
  %">=_i1" = icmp sge i64 %loaded_value, %struc.len
  %">=_res" = sext i1 %">=_i1" to i8
  %cond_i1 = trunc i8 %">=_res" to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %0 = load { { i64, ptr } }, ptr @str.30, align 8
  %struct_init_0 = insertvalue { { { i64, ptr } }, i64 } undef, { { i64, ptr } } %0, 0
  %struct_init_1 = insertvalue { { { i64, ptr } }, i64 } %struct_init_0, i64 80, 1
  %1 = load { { i64, ptr } }, ptr @str.32, align 8
  call void @crash({ { { i64, ptr } }, i64 } %struct_init_1, { { i64, ptr } } %1)
  unreachable

if_alt:                                           ; preds = %entry
  br label %if_merge

if_merge:                                         ; preds = %if_alt
  %if_phi = phi i8 [ 0, %if_alt ]
  %loaded_value4 = load { i64, ptr }, ptr %self1, align 8
  %struc.data = extractvalue { i64, ptr } %loaded_value4, 1
  %loaded_value5 = load i64, ptr %index2, align 8
  %refAtIndex = getelementptr inbounds %"46.2", ptr %struc.data, i64 %loaded_value5
  ret ptr %refAtIndex
}

define { { i64, ptr }, i64 } @"Iterable_Buffer[enum Char(char) | String(string)]_iterator"({ i64, ptr } %self) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %loaded_value = load { i64, ptr }, ptr %self1, align 8
  %0 = call { { i64, ptr }, i64 } @"BufferIterator.fromBuffer_spec_enum Char(char) | String(string)_10"({ i64, ptr } %loaded_value)
  ret { { i64, ptr }, i64 } %0
}

define { { i64, ptr }, i64 } @"BufferIterator.fromBuffer_spec_enum Char(char) | String(string)_10"({ i64, ptr } %buf) {
entry:
  %buf1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %buf, ptr %buf1, align 8
  %loaded_value = load { i64, ptr }, ptr %buf1, align 8
  %struct_init_0 = insertvalue { { i64, ptr }, i64 } undef, { i64, ptr } %loaded_value, 0
  %struct_init_1 = insertvalue { { i64, ptr }, i64 } %struct_init_0, i64 0, 1
  ret { { i64, ptr }, i64 } %struct_init_1
}

define %Opt @"Iterator_BufferIterator[enum Char(char) | String(string)]_next"(ptr %self) {
entry:
  %self1 = alloca ptr, align 8
  store ptr %self, ptr %self1, align 8
  %loaded_value = load ptr, ptr %self1, align 8
  %deref = load { { i64, ptr }, i64 }, ptr %loaded_value, align 8
  %struc.pos = extractvalue { { i64, ptr }, i64 } %deref, 1
  %loaded_value2 = load ptr, ptr %self1, align 8
  %deref3 = load { { i64, ptr }, i64 }, ptr %loaded_value2, align 8
  %struc.buf = extractvalue { { i64, ptr }, i64 } %deref3, 0
  %struc.len = extractvalue { i64, ptr } %struc.buf, 0
  %"<_i1" = icmp slt i64 %struc.pos, %struc.len
  %"<_res" = sext i1 %"<_i1" to i8
  %cond_i1 = trunc i8 %"<_res" to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %loaded_value4 = load ptr, ptr %self1, align 8
  %deref5 = load { { i64, ptr }, i64 }, ptr %loaded_value4, align 8
  %struc.buf6 = extractvalue { { i64, ptr }, i64 } %deref5, 0
  %loaded_value7 = load ptr, ptr %self1, align 8
  %deref8 = load { { i64, ptr }, i64 }, ptr %loaded_value7, align 8
  %struc.pos9 = extractvalue { { i64, ptr }, i64 } %deref8, 1
  %0 = call %"46.5" @"Buffer.get_spec_enum Char(char) | String(string)_14"({ i64, ptr } %struc.buf6, i64 %struc.pos9)
  %1 = call %Opt @"some_spec_enum Char(char) | String(string)_13"(%"46.5" %0)
  %ret = alloca %Opt, align 8
  store %Opt %1, ptr %ret, align 8
  %loaded_value10 = load ptr, ptr %self1, align 8
  %deref11 = load { { i64, ptr }, i64 }, ptr %loaded_value10, align 8
  %struc.pos12 = extractvalue { { i64, ptr }, i64 } %deref11, 1
  %add = add i64 %struc.pos12, 1
  %loaded_value13 = load ptr, ptr %self1, align 8
  %struc.pos14 = getelementptr inbounds { { i64, ptr }, i64 }, ptr %loaded_value13, i32 0, i32 1
  store i64 %add, ptr %struc.pos14, align 8
  %loaded_value15 = load %Opt, ptr %ret, align 8
  br label %if_merge

if_alt:                                           ; preds = %entry
  %enum_constr = alloca %Opt.None, align 8
  %enum_tag_None = getelementptr inbounds %Opt.None, ptr %enum_constr, i32 0, i32 0
  store i64 0, ptr %enum_tag_None, align 8
  %enum_value = load %Opt, ptr %enum_constr, align 8
  br label %if_merge

if_merge:                                         ; preds = %if_alt, %if_cons
  %if_phi = phi %Opt [ %loaded_value15, %if_cons ], [ %enum_value, %if_alt ]
  ret %Opt %if_phi
}

define %"46.5" @"Buffer.get_spec_enum Char(char) | String(string)_14"({ i64, ptr } %self, i64 %index) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %index2 = alloca i64, align 8
  store i64 %index, ptr %index2, align 8
  %loaded_value = load { i64, ptr }, ptr %self1, align 8
  %loaded_value3 = load i64, ptr %index2, align 8
  %0 = call ptr @"Buffer.getRef_spec_enum Char(char) | String(string)_15"({ i64, ptr } %loaded_value, i64 %loaded_value3)
  %tRef = alloca ptr, align 8
  store ptr %0, ptr %tRef, align 8
  %loaded_value4 = load ptr, ptr %tRef, align 8
  %deref = load %"46.5", ptr %loaded_value4, align 8
  ret %"46.5" %deref
}

define %Opt @"some_spec_enum Char(char) | String(string)_13"(%"46.5" %value) {
entry:
  %value1 = alloca %"46.5", align 8
  store %"46.5" %value, ptr %value1, align 8
  %enum_constr = alloca %Opt.Some, align 8
  %enum_tag_Some = getelementptr inbounds %Opt.Some, ptr %enum_constr, i32 0, i32 0
  store i64 1, ptr %enum_tag_Some, align 8
  %loaded_value = load %"46.5", ptr %value1, align 8
  %enum_payload_Some = getelementptr inbounds %Opt.Some, ptr %enum_constr, i32 0, i32 1
  store %"46.5" %loaded_value, ptr %enum_payload_Some, align 8
  %enum_value = load %Opt, ptr %enum_constr, align 8
  ret %Opt %enum_value
}

define i8 @"Opt.isSome_spec_enum Char(char) | String(string)_8"(%Opt %self) {
entry:
  %self1 = alloca %Opt, align 8
  store %Opt %self, ptr %self1, align 8
  %loaded_value = load %Opt, ptr %self1, align 8
  %__match_target_916 = alloca %Opt, align 8
  store %Opt %loaded_value, ptr %__match_target_916, align 8
  %loaded_value2 = load %Opt, ptr %__match_target_916, align 8
  %enum_ptr_for_payload = alloca %Opt.Some, align 8
  store %Opt %loaded_value2, ptr %enum_ptr_for_payload, align 8
  %get_payload_ptr = getelementptr inbounds %Opt.Some, ptr %enum_ptr_for_payload, i32 0, i32 1
  %get_payload = load %"46.5", ptr %get_payload_ptr, align 8
  %__payload_1106 = alloca %"46.5", align 8
  store %"46.5" %get_payload, ptr %__payload_1106, align 8
  %loaded_value3 = load %Opt, ptr %__match_target_916, align 8
  %get_tag = extractvalue %Opt %loaded_value3, 0
  %get_tag4 = extractvalue %Opt.Some %get_tag, 0
  %is_variant_cmp = icmp eq i64 %get_tag4, 1
  %is_variant = sext i1 %is_variant_cmp to i8
  %cond_i1 = trunc i8 %is_variant to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  br label %if_merge

if_alt:                                           ; preds = %entry
  br label %if_merge

if_merge:                                         ; preds = %if_alt, %if_cons
  %if_phi = phi i8 [ 1, %if_cons ], [ 0, %if_alt ]
  ret i8 %if_phi
}

define %"46.5" @"Unwrap_Opt[enum Char(char) | String(string)]_unwrap"(%Opt %self) {
entry:
  %self1 = alloca %Opt, align 8
  store %Opt %self, ptr %self1, align 8
  %loaded_value = load %Opt, ptr %self1, align 8
  %0 = call %"46.5" @"Opt.get_spec_enum Char(char) | String(string)_11"(%Opt %loaded_value)
  ret %"46.5" %0
}

define %"46.5" @"Opt.get_spec_enum Char(char) | String(string)_11"(%Opt %self) {
entry:
  %self1 = alloca %Opt, align 8
  store %Opt %self, ptr %self1, align 8
  %loaded_value = load %Opt, ptr %self1, align 8
  %__match_target_1104 = alloca %Opt, align 8
  store %Opt %loaded_value, ptr %__match_target_1104, align 8
  %loaded_value2 = load %Opt, ptr %__match_target_1104, align 8
  %enum_ptr_for_payload = alloca %Opt.Some, align 8
  store %Opt %loaded_value2, ptr %enum_ptr_for_payload, align 8
  %get_payload_ptr = getelementptr inbounds %Opt.Some, ptr %enum_ptr_for_payload, i32 0, i32 1
  %get_payload = load %"46.5", ptr %get_payload_ptr, align 8
  %__payload_1210 = alloca %"46.5", align 8
  store %"46.5" %get_payload, ptr %__payload_1210, align 8
  %loaded_value3 = load %Opt, ptr %__match_target_1104, align 8
  %get_tag = extractvalue %Opt %loaded_value3, 0
  %get_tag4 = extractvalue %Opt.Some %get_tag, 0
  %is_variant_cmp = icmp eq i64 %get_tag4, 1
  %is_variant = sext i1 %is_variant_cmp to i8
  %cond_i1 = trunc i8 %is_variant to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %loaded_value5 = load %"46.5", ptr %__payload_1210, align 8
  %t = alloca %"46.5", align 8
  store %"46.5" %loaded_value5, ptr %t, align 8
  %loaded_value6 = load %"46.5", ptr %t, align 8
  br label %if_merge

if_alt:                                           ; preds = %entry
  %0 = load { { i64, ptr } }, ptr @str.34, align 8
  %struct_init_0 = insertvalue { { { i64, ptr } }, i64 } undef, { { i64, ptr } } %0, 0
  %struct_init_1 = insertvalue { { { i64, ptr } }, i64 } %struct_init_0, i64 7, 1
  %1 = load { { i64, ptr } }, ptr @str.36, align 8
  call void @crash({ { { i64, ptr } }, i64 } %struct_init_1, { { i64, ptr } } %1)
  unreachable

if_merge:                                         ; preds = %if_cons
  %if_phi = phi %"46.5" [ %loaded_value6, %if_cons ]
  ret %"46.5" %if_phi
}

define { { i64, ptr }, i64 } @Iterable_string_iterator({ { i64, ptr } } %self) {
entry:
  %self1 = alloca { { i64, ptr } }, align 8
  store { { i64, ptr } } %self, ptr %self1, align 8
  %loaded_value = load { { i64, ptr } }, ptr %self1, align 8
  %struc.buffer = extractvalue { { i64, ptr } } %loaded_value, 0
  %0 = call { { i64, ptr }, i64 } @BufferIterator.fromBuffer_spec_char_2({ i64, ptr } %struc.buffer)
  ret { { i64, ptr }, i64 } %0
}

define { { i64, ptr }, i64 } @BufferIterator.fromBuffer_spec_char_2({ i64, ptr } %buf) {
entry:
  %buf1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %buf, ptr %buf1, align 8
  %loaded_value = load { i64, ptr }, ptr %buf1, align 8
  %struct_init_0 = insertvalue { { i64, ptr }, i64 } undef, { i64, ptr } %loaded_value, 0
  %struct_init_1 = insertvalue { { i64, ptr }, i64 } %struct_init_0, i64 0, 1
  ret { { i64, ptr }, i64 } %struct_init_1
}

define %Opt.8 @"Iterator_BufferIterator[char]_next"(ptr %self) {
entry:
  %self1 = alloca ptr, align 8
  store ptr %self, ptr %self1, align 8
  %loaded_value = load ptr, ptr %self1, align 8
  %deref = load { { i64, ptr }, i64 }, ptr %loaded_value, align 8
  %struc.pos = extractvalue { { i64, ptr }, i64 } %deref, 1
  %loaded_value2 = load ptr, ptr %self1, align 8
  %deref3 = load { { i64, ptr }, i64 }, ptr %loaded_value2, align 8
  %struc.buf = extractvalue { { i64, ptr }, i64 } %deref3, 0
  %struc.len = extractvalue { i64, ptr } %struc.buf, 0
  %"<_i1" = icmp slt i64 %struc.pos, %struc.len
  %"<_res" = sext i1 %"<_i1" to i8
  %cond_i1 = trunc i8 %"<_res" to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %loaded_value4 = load ptr, ptr %self1, align 8
  %deref5 = load { { i64, ptr }, i64 }, ptr %loaded_value4, align 8
  %struc.buf6 = extractvalue { { i64, ptr }, i64 } %deref5, 0
  %loaded_value7 = load ptr, ptr %self1, align 8
  %deref8 = load { { i64, ptr }, i64 }, ptr %loaded_value7, align 8
  %struc.pos9 = extractvalue { { i64, ptr }, i64 } %deref8, 1
  %0 = call i8 @Buffer.get_spec_char_6({ i64, ptr } %struc.buf6, i64 %struc.pos9)
  %1 = call %Opt.8 @some_spec_char_6(i8 %0)
  %ret = alloca %Opt.8, align 8
  store %Opt.8 %1, ptr %ret, align 1
  %loaded_value10 = load ptr, ptr %self1, align 8
  %deref11 = load { { i64, ptr }, i64 }, ptr %loaded_value10, align 8
  %struc.pos12 = extractvalue { { i64, ptr }, i64 } %deref11, 1
  %add = add i64 %struc.pos12, 1
  %loaded_value13 = load ptr, ptr %self1, align 8
  %struc.pos14 = getelementptr inbounds { { i64, ptr }, i64 }, ptr %loaded_value13, i32 0, i32 1
  store i64 %add, ptr %struc.pos14, align 8
  %loaded_value15 = load %Opt.8, ptr %ret, align 1
  br label %if_merge

if_alt:                                           ; preds = %entry
  %enum_constr = alloca %Opt.None.6, align 8
  %enum_tag_None = getelementptr inbounds %Opt.None.6, ptr %enum_constr, i32 0, i32 0
  store i8 0, ptr %enum_tag_None, align 1
  %enum_value = load %Opt.8, ptr %enum_constr, align 1
  br label %if_merge

if_merge:                                         ; preds = %if_alt, %if_cons
  %if_phi = phi %Opt.8 [ %loaded_value15, %if_cons ], [ %enum_value, %if_alt ]
  ret %Opt.8 %if_phi
}

define i8 @Buffer.get_spec_char_6({ i64, ptr } %self, i64 %index) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %index2 = alloca i64, align 8
  store i64 %index, ptr %index2, align 8
  %loaded_value = load { i64, ptr }, ptr %self1, align 8
  %loaded_value3 = load i64, ptr %index2, align 8
  %0 = call ptr @Buffer.getRef_spec_char_10({ i64, ptr } %loaded_value, i64 %loaded_value3)
  %tRef = alloca ptr, align 8
  store ptr %0, ptr %tRef, align 8
  %loaded_value4 = load ptr, ptr %tRef, align 8
  %deref = load i8, ptr %loaded_value4, align 1
  ret i8 %deref
}

define ptr @Buffer.getRef_spec_char_10({ i64, ptr } %self, i64 %index) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %index2 = alloca i64, align 8
  store i64 %index, ptr %index2, align 8
  %loaded_value = load i64, ptr %index2, align 8
  %loaded_value3 = load { i64, ptr }, ptr %self1, align 8
  %struc.len = extractvalue { i64, ptr } %loaded_value3, 0
  %">=_i1" = icmp sge i64 %loaded_value, %struc.len
  %">=_res" = sext i1 %">=_i1" to i8
  %cond_i1 = trunc i8 %">=_res" to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %0 = load { { i64, ptr } }, ptr @str.42, align 8
  %struct_init_0 = insertvalue { { { i64, ptr } }, i64 } undef, { { i64, ptr } } %0, 0
  %struct_init_1 = insertvalue { { { i64, ptr } }, i64 } %struct_init_0, i64 80, 1
  %1 = load { { i64, ptr } }, ptr @str.44, align 8
  call void @crash({ { { i64, ptr } }, i64 } %struct_init_1, { { i64, ptr } } %1)
  unreachable

if_alt:                                           ; preds = %entry
  br label %if_merge

if_merge:                                         ; preds = %if_alt
  %if_phi = phi i8 [ 0, %if_alt ]
  %loaded_value4 = load { i64, ptr }, ptr %self1, align 8
  %struc.data = extractvalue { i64, ptr } %loaded_value4, 1
  %loaded_value5 = load i64, ptr %index2, align 8
  %refAtIndex = getelementptr inbounds i8, ptr %struc.data, i64 %loaded_value5
  ret ptr %refAtIndex
}

define %Opt.8 @some_spec_char_6(i8 %value) {
entry:
  %value1 = alloca i8, align 1
  store i8 %value, ptr %value1, align 1
  %enum_constr = alloca %Opt.Some.7, align 8
  %enum_tag_Some = getelementptr inbounds %Opt.Some.7, ptr %enum_constr, i32 0, i32 0
  store i8 1, ptr %enum_tag_Some, align 1
  %loaded_value = load i8, ptr %value1, align 1
  %enum_payload_Some = getelementptr inbounds %Opt.Some.7, ptr %enum_constr, i32 0, i32 1
  store i8 %loaded_value, ptr %enum_payload_Some, align 1
  %enum_value = load %Opt.8, ptr %enum_constr, align 1
  ret %Opt.8 %enum_value
}

define i8 @Opt.isSome_spec_char_6(%Opt.8 %self) {
entry:
  %self1 = alloca %Opt.8, align 8
  store %Opt.8 %self, ptr %self1, align 1
  %loaded_value = load %Opt.8, ptr %self1, align 1
  %__match_target_850 = alloca %Opt.8, align 8
  store %Opt.8 %loaded_value, ptr %__match_target_850, align 1
  %loaded_value2 = load %Opt.8, ptr %__match_target_850, align 1
  %enum_ptr_for_payload = alloca %Opt.Some.7, align 8
  store %Opt.8 %loaded_value2, ptr %enum_ptr_for_payload, align 1
  %get_payload_ptr = getelementptr inbounds %Opt.Some.7, ptr %enum_ptr_for_payload, i32 0, i32 1
  %get_payload = load i8, ptr %get_payload_ptr, align 1
  %__payload_1068 = alloca i8, align 1
  store i8 %get_payload, ptr %__payload_1068, align 1
  %loaded_value3 = load %Opt.8, ptr %__match_target_850, align 1
  %get_tag = extractvalue %Opt.8 %loaded_value3, 0
  %get_tag4 = extractvalue %Opt.Some.7 %get_tag, 0
  %is_variant_cmp = icmp eq i8 %get_tag4, 1
  %is_variant = sext i1 %is_variant_cmp to i8
  %cond_i1 = trunc i8 %is_variant to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  br label %if_merge

if_alt:                                           ; preds = %entry
  br label %if_merge

if_merge:                                         ; preds = %if_alt, %if_cons
  %if_phi = phi i8 [ 1, %if_cons ], [ 0, %if_alt ]
  ret i8 %if_phi
}

define i8 @"Unwrap_Opt[char]_unwrap"(%Opt.8 %self) {
entry:
  %self1 = alloca %Opt.8, align 8
  store %Opt.8 %self, ptr %self1, align 1
  %loaded_value = load %Opt.8, ptr %self1, align 1
  %0 = call i8 @Opt.get_spec_char_9(%Opt.8 %loaded_value)
  ret i8 %0
}

define i8 @Opt.get_spec_char_9(%Opt.8 %self) {
entry:
  %self1 = alloca %Opt.8, align 8
  store %Opt.8 %self, ptr %self1, align 1
  %loaded_value = load %Opt.8, ptr %self1, align 1
  %__match_target_1062 = alloca %Opt.8, align 8
  store %Opt.8 %loaded_value, ptr %__match_target_1062, align 1
  %loaded_value2 = load %Opt.8, ptr %__match_target_1062, align 1
  %enum_ptr_for_payload = alloca %Opt.Some.7, align 8
  store %Opt.8 %loaded_value2, ptr %enum_ptr_for_payload, align 1
  %get_payload_ptr = getelementptr inbounds %Opt.Some.7, ptr %enum_ptr_for_payload, i32 0, i32 1
  %get_payload = load i8, ptr %get_payload_ptr, align 1
  %__payload_1181 = alloca i8, align 1
  store i8 %get_payload, ptr %__payload_1181, align 1
  %loaded_value3 = load %Opt.8, ptr %__match_target_1062, align 1
  %get_tag = extractvalue %Opt.8 %loaded_value3, 0
  %get_tag4 = extractvalue %Opt.Some.7 %get_tag, 0
  %is_variant_cmp = icmp eq i8 %get_tag4, 1
  %is_variant = sext i1 %is_variant_cmp to i8
  %cond_i1 = trunc i8 %is_variant to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %loaded_value5 = load i8, ptr %__payload_1181, align 1
  %t = alloca i8, align 1
  store i8 %loaded_value5, ptr %t, align 1
  %loaded_value6 = load i8, ptr %t, align 1
  br label %if_merge

if_alt:                                           ; preds = %entry
  %0 = load { { i64, ptr } }, ptr @str.46, align 8
  %struct_init_0 = insertvalue { { { i64, ptr } }, i64 } undef, { { i64, ptr } } %0, 0
  %struct_init_1 = insertvalue { { { i64, ptr } }, i64 } %struct_init_0, i64 7, 1
  %1 = load { { i64, ptr } }, ptr @str.48, align 8
  call void @crash({ { { i64, ptr } }, i64 } %struct_init_1, { { i64, ptr } } %1)
  unreachable

if_merge:                                         ; preds = %if_cons
  %if_phi = phi i8 [ %loaded_value6, %if_cons ]
  ret i8 %if_phi
}

define { { i64, ptr } } @string.wrapList({ i64, { i64, ptr } } %list) {
entry:
  %list1 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %list, ptr %list1, align 8
  %loaded_value = load { i64, { i64, ptr } }, ptr %list1, align 8
  %0 = call { i64, ptr } @List.toBuffer_spec_char_6({ i64, { i64, ptr } } %loaded_value)
  %struct_init_0 = insertvalue { { i64, ptr } } undef, { i64, ptr } %0, 0
  ret { { i64, ptr } } %struct_init_0
}

define { i64, ptr } @List.toBuffer_spec_char_6({ i64, { i64, ptr } } %self) {
entry:
  %self1 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %self, ptr %self1, align 8
  %loaded_value = load { i64, { i64, ptr } }, ptr %self1, align 8
  %struc.buffer = extractvalue { i64, { i64, ptr } } %loaded_value, 1
  %loaded_value2 = load { i64, { i64, ptr } }, ptr %self1, align 8
  %struc.len = extractvalue { i64, { i64, ptr } } %loaded_value2, 0
  %0 = call { i64, ptr } @Buffer.slice_spec_char_6({ i64, ptr } %struc.buffer, i64 0, i64 %struc.len)
  ret { i64, ptr } %0
}

define { i64, ptr } @Buffer.slice_spec_char_6({ i64, ptr } %self, i64 %start, i64 %end) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %start2 = alloca i64, align 8
  store i64 %start, ptr %start2, align 8
  %end3 = alloca i64, align 8
  store i64 %end, ptr %end3, align 8
  %loaded_value = load i64, ptr %end3, align 8
  %loaded_value4 = load i64, ptr %start2, align 8
  %"<_i1" = icmp slt i64 %loaded_value, %loaded_value4
  %"<_res" = sext i1 %"<_i1" to i8
  %loaded_value5 = load { i64, ptr }, ptr %self1, align 8
  %struc.len = extractvalue { i64, ptr } %loaded_value5, 0
  %"==_i1" = icmp eq i64 %struc.len, 0
  %"==_res" = sext i1 %"==_i1" to i8
  %bool_or = or i8 %"<_res", %"==_res"
  %cond_i1 = trunc i8 %bool_or to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %0 = call { i64, ptr } @Buffer.empty_spec_char_3()
  ret { i64, ptr } %0

if_alt:                                           ; preds = %entry
  br label %if_merge

if_merge:                                         ; preds = %if_alt
  %if_phi = phi i8 [ 0, %if_alt ]
  %loaded_value9 = load i64, ptr %start2, align 8
  %loaded_value10 = load { i64, ptr }, ptr %self1, align 8
  %struc.len11 = extractvalue { i64, ptr } %loaded_value10, 0
  %">=_i1" = icmp sge i64 %loaded_value9, %struc.len11
  %">=_res" = sext i1 %">=_i1" to i8
  %loaded_value12 = load i64, ptr %start2, align 8
  %"<_i113" = icmp slt i64 %loaded_value12, 0
  %"<_res14" = sext i1 %"<_i113" to i8
  %bool_or15 = or i8 %">=_res", %"<_res14"
  %cond_i116 = trunc i8 %bool_or15 to i1
  br i1 %cond_i116, label %if_cons6, label %if_alt7

if_cons6:                                         ; preds = %if_merge
  %1 = load { { i64, ptr } }, ptr @str.54, align 8
  %struct_init_0 = insertvalue { { { i64, ptr } }, i64 } undef, { { i64, ptr } } %1, 0
  %struct_init_1 = insertvalue { { { i64, ptr } }, i64 } %struct_init_0, i64 67, 1
  %2 = call { ptr } @StringBuilder.withCapacity(i64 3)
  %__sb_1087 = alloca { ptr }, align 8
  store { ptr } %2, ptr %__sb_1087, align 8
  %loaded_value17 = load { ptr }, ptr %__sb_1087, align 8
  %3 = load { { i64, ptr } }, ptr @str.56, align 8
  %4 = call i8 @StringBuilder.putString({ ptr } %loaded_value17, { { i64, ptr } } %3)
  %loaded_value18 = load { ptr }, ptr %__sb_1087, align 8
  %loaded_value19 = load i64, ptr %start2, align 8
  %5 = call { { i64, ptr } } @Show_u64_show(i64 %loaded_value19)
  %6 = call i8 @StringBuilder.putString({ ptr } %loaded_value18, { { i64, ptr } } %5)
  %loaded_value20 = load { ptr }, ptr %__sb_1087, align 8
  %7 = load { { i64, ptr } }, ptr @str.58, align 8
  %8 = call i8 @StringBuilder.putString({ ptr } %loaded_value20, { { i64, ptr } } %7)
  %loaded_value21 = load { ptr }, ptr %__sb_1087, align 8
  %9 = call { { i64, ptr } } @StringBuilder.build({ ptr } %loaded_value21)
  call void @crash({ { { i64, ptr } }, i64 } %struct_init_1, { { i64, ptr } } %9)
  unreachable

if_alt7:                                          ; preds = %if_merge
  br label %if_merge8

if_merge8:                                        ; preds = %if_alt7
  %if_phi22 = phi i8 [ 0, %if_alt7 ]
  %loaded_value26 = load i64, ptr %end3, align 8
  %loaded_value27 = load { i64, ptr }, ptr %self1, align 8
  %struc.len28 = extractvalue { i64, ptr } %loaded_value27, 0
  %">_i1" = icmp sgt i64 %loaded_value26, %struc.len28
  %">_res" = sext i1 %">_i1" to i8
  %cond_i129 = trunc i8 %">_res" to i1
  br i1 %cond_i129, label %if_cons23, label %if_alt24

if_cons23:                                        ; preds = %if_merge8
  %loaded_value30 = load { i64, ptr }, ptr %self1, align 8
  %struc.len31 = extractvalue { i64, ptr } %loaded_value30, 0
  br label %if_merge25

if_alt24:                                         ; preds = %if_merge8
  %loaded_value32 = load i64, ptr %end3, align 8
  br label %if_merge25

if_merge25:                                       ; preds = %if_alt24, %if_cons23
  %if_phi33 = phi i64 [ %struc.len31, %if_cons23 ], [ %loaded_value32, %if_alt24 ]
  %end34 = alloca i64, align 8
  store i64 %if_phi33, ptr %end34, align 8
  %loaded_value35 = load i64, ptr %end34, align 8
  %loaded_value36 = load i64, ptr %start2, align 8
  %sub = sub i64 %loaded_value35, %loaded_value36
  %newLen = alloca i64, align 8
  store i64 %sub, ptr %newLen, align 8
  %loaded_value37 = load { i64, ptr }, ptr %self1, align 8
  %loaded_value38 = load i64, ptr %start2, align 8
  %10 = call ptr @Buffer.getRef_spec_char_10({ i64, ptr } %loaded_value37, i64 %loaded_value38)
  %newBase = alloca ptr, align 8
  store ptr %10, ptr %newBase, align 8
  %loaded_value39 = load i64, ptr %newLen, align 8
  %struct_init_040 = insertvalue { i64, ptr } undef, i64 %loaded_value39, 0
  %loaded_value41 = load ptr, ptr %newBase, align 8
  %struct_init_142 = insertvalue { i64, ptr } %struct_init_040, ptr %loaded_value41, 1
  ret { i64, ptr } %struct_init_142
}

define { i64, ptr } @Buffer.empty_spec_char_3() {
entry:
  %0 = call { i64, ptr } @Buffer._allocate_spec_char_7(i64 0)
  ret { i64, ptr } %0
}

declare ptr @memcpy(ptr, ptr, i64)

define i8 @List.set_spec_char_4({ i64, { i64, ptr } } %self, i64 %index, i8 %elem) {
entry:
  %self1 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %self, ptr %self1, align 8
  %index2 = alloca i64, align 8
  store i64 %index, ptr %index2, align 8
  %elem3 = alloca i8, align 1
  store i8 %elem, ptr %elem3, align 1
  %loaded_value = load { i64, { i64, ptr } }, ptr %self1, align 8
  %struc.buffer = extractvalue { i64, { i64, ptr } } %loaded_value, 1
  %loaded_value4 = load i64, ptr %index2, align 8
  %loaded_value5 = load i8, ptr %elem3, align 1
  %0 = call i8 @Buffer.set_spec_char_4({ i64, ptr } %struc.buffer, i64 %loaded_value4, i8 %loaded_value5)
  ret i8 %0
}

define i8 @Buffer.set_spec_char_4({ i64, ptr } %self, i64 %index, i8 %elem) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %index2 = alloca i64, align 8
  store i64 %index, ptr %index2, align 8
  %elem3 = alloca i8, align 1
  store i8 %elem, ptr %elem3, align 1
  %loaded_value = load { i64, ptr }, ptr %self1, align 8
  %loaded_value4 = load i64, ptr %index2, align 8
  %0 = call ptr @Buffer.getRef_spec_char_10({ i64, ptr } %loaded_value, i64 %loaded_value4)
  %tRef = alloca ptr, align 8
  store ptr %0, ptr %tRef, align 8
  %loaded_value5 = load i8, ptr %elem3, align 1
  %loaded_value6 = load ptr, ptr %tRef, align 8
  store i8 %loaded_value5, ptr %loaded_value6, align 1
  ret i8 0
}

define { i64, { i64, ptr } } @List.reversed_spec_char_1({ i64, { i64, ptr } } %self) {
entry:
  %self1 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %self, ptr %self1, align 8
  %loaded_value = load { i64, { i64, ptr } }, ptr %self1, align 8
  %0 = call { i64, { i64, ptr } } @List.cloned_spec_char_3({ i64, { i64, ptr } } %loaded_value)
  %1 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %0, ptr %1, align 8
  %clone = alloca ptr, align 8
  store ptr %1, ptr %clone, align 8
  %loaded_value2 = load ptr, ptr %clone, align 8
  %2 = call i8 @List.reverse_spec_char_2(ptr %loaded_value2)
  %loaded_value3 = load ptr, ptr %clone, align 8
  %deref = load { i64, { i64, ptr } }, ptr %loaded_value3, align 8
  ret { i64, { i64, ptr } } %deref
}

define { i64, { i64, ptr } } @List.cloned_spec_char_3({ i64, { i64, ptr } } %self) {
entry:
  %self1 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %self, ptr %self1, align 8
  %loaded_value = load { i64, { i64, ptr } }, ptr %self1, align 8
  %struc.buffer = extractvalue { i64, { i64, ptr } } %loaded_value, 1
  %0 = call { i64, ptr } @Buffer.cloned_spec_char_2({ i64, ptr } %struc.buffer)
  %newBuffer = alloca { i64, ptr }, align 8
  store { i64, ptr } %0, ptr %newBuffer, align 8
  %loaded_value2 = load { i64, { i64, ptr } }, ptr %self1, align 8
  %struc.len = extractvalue { i64, { i64, ptr } } %loaded_value2, 0
  %struct_init_0 = insertvalue { i64, { i64, ptr } } undef, i64 %struc.len, 0
  %loaded_value3 = load { i64, ptr }, ptr %newBuffer, align 8
  %struct_init_1 = insertvalue { i64, { i64, ptr } } %struct_init_0, { i64, ptr } %loaded_value3, 1
  ret { i64, { i64, ptr } } %struct_init_1
}

define { i64, ptr } @Buffer.cloned_spec_char_2({ i64, ptr } %self) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %loaded_value = load { i64, ptr }, ptr %self1, align 8
  %0 = call i64 @Buffer.lenBytes_spec_char_3({ i64, ptr } %loaded_value)
  %numBytes = alloca i64, align 8
  store i64 %0, ptr %numBytes, align 8
  %loaded_value2 = load i64, ptr %numBytes, align 8
  %1 = call ptr @malloc(i64 %loaded_value2)
  %newBuffer = alloca ptr, align 8
  store ptr %1, ptr %newBuffer, align 8
  %loaded_value3 = load ptr, ptr %newBuffer, align 8
  %loaded_value4 = load { i64, ptr }, ptr %self1, align 8
  %struc.data = extractvalue { i64, ptr } %loaded_value4, 1
  %loaded_value5 = load i64, ptr %numBytes, align 8
  %2 = call ptr @memcpy(ptr %loaded_value3, ptr %struc.data, i64 %loaded_value5)
  %loaded_value6 = load { i64, ptr }, ptr %self1, align 8
  %struc.len = extractvalue { i64, ptr } %loaded_value6, 0
  %struct_init_0 = insertvalue { i64, ptr } undef, i64 %struc.len, 0
  %loaded_value7 = load ptr, ptr %newBuffer, align 8
  %struct_init_1 = insertvalue { i64, ptr } %struct_init_0, ptr %loaded_value7, 1
  ret { i64, ptr } %struct_init_1
}

define i64 @Buffer.lenBytes_spec_char_3({ i64, ptr } %self) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %loaded_value = load { i64, ptr }, ptr %self1, align 8
  %struc.len = extractvalue { i64, ptr } %loaded_value, 0
  %0 = call i64 @Buffer.bufferLenBytes_spec_char_5(i64 %struc.len)
  ret i64 %0
}

define i8 @List.reverse_spec_char_2(ptr %self) {
entry:
  %self1 = alloca ptr, align 8
  store ptr %self, ptr %self1, align 8
  %i = alloca i64, align 8
  store i64 0, ptr %i, align 8
  br label %while_cond

while_cond:                                       ; preds = %while_body, %entry
  %loaded_value = load i64, ptr %i, align 8
  %loaded_value2 = load ptr, ptr %self1, align 8
  %deref = load { i64, { i64, ptr } }, ptr %loaded_value2, align 8
  %struc.len = extractvalue { i64, { i64, ptr } } %deref, 0
  %udiv = udiv i64 %struc.len, 2
  %"<_i1" = icmp slt i64 %loaded_value, %udiv
  %"<_res" = sext i1 %"<_i1" to i8
  %while_cond3 = trunc i8 %"<_res" to i1
  br i1 %while_cond3, label %while_body, label %while_end

while_body:                                       ; preds = %while_cond
  %loaded_value4 = load ptr, ptr %self1, align 8
  %deref5 = load { i64, { i64, ptr } }, ptr %loaded_value4, align 8
  %struc.len6 = extractvalue { i64, { i64, ptr } } %deref5, 0
  %loaded_value7 = load i64, ptr %i, align 8
  %sub = sub i64 %struc.len6, %loaded_value7
  %sub8 = sub i64 %sub, 1
  %j = alloca i64, align 8
  store i64 %sub8, ptr %j, align 8
  %loaded_value9 = load ptr, ptr %self1, align 8
  %loaded_value10 = load i64, ptr %i, align 8
  %loaded_value11 = load i64, ptr %j, align 8
  %0 = call i8 @List.swap_spec_char_2(ptr %loaded_value9, i64 %loaded_value10, i64 %loaded_value11)
  %loaded_value12 = load i64, ptr %i, align 8
  %add = add i64 %loaded_value12, 1
  store i64 %add, ptr %i, align 8
  br label %while_cond

while_end:                                        ; preds = %while_cond
  ret i8 0
}

define i8 @List.swap_spec_char_2(ptr %self, i64 %indexA, i64 %indexB) {
entry:
  %self1 = alloca ptr, align 8
  store ptr %self, ptr %self1, align 8
  %indexA2 = alloca i64, align 8
  store i64 %indexA, ptr %indexA2, align 8
  %indexB3 = alloca i64, align 8
  store i64 %indexB, ptr %indexB3, align 8
  %loaded_value = load ptr, ptr %self1, align 8
  %deref = load { i64, { i64, ptr } }, ptr %loaded_value, align 8
  %loaded_value4 = load i64, ptr %indexA2, align 8
  %0 = call ptr @List.getRef_spec_char_5({ i64, { i64, ptr } } %deref, i64 %loaded_value4)
  %aRef = alloca ptr, align 8
  store ptr %0, ptr %aRef, align 8
  %loaded_value5 = load ptr, ptr %self1, align 8
  %deref6 = load { i64, { i64, ptr } }, ptr %loaded_value5, align 8
  %loaded_value7 = load i64, ptr %indexB3, align 8
  %1 = call ptr @List.getRef_spec_char_5({ i64, { i64, ptr } } %deref6, i64 %loaded_value7)
  %bRef = alloca ptr, align 8
  store ptr %1, ptr %bRef, align 8
  %loaded_value8 = load ptr, ptr %bRef, align 8
  %deref9 = load i8, ptr %loaded_value8, align 1
  %bValue = alloca i8, align 1
  store i8 %deref9, ptr %bValue, align 1
  %loaded_value10 = load ptr, ptr %aRef, align 8
  %deref11 = load i8, ptr %loaded_value10, align 1
  %loaded_value12 = load ptr, ptr %bRef, align 8
  store i8 %deref11, ptr %loaded_value12, align 1
  %loaded_value13 = load i8, ptr %bValue, align 1
  %loaded_value14 = load ptr, ptr %aRef, align 8
  store i8 %loaded_value13, ptr %loaded_value14, align 1
  ret i8 0
}

define ptr @List.getRef_spec_char_5({ i64, { i64, ptr } } %self, i64 %index) {
entry:
  %self1 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %self, ptr %self1, align 8
  %index2 = alloca i64, align 8
  store i64 %index, ptr %index2, align 8
  %loaded_value = load { i64, { i64, ptr } }, ptr %self1, align 8
  %struc.buffer = extractvalue { i64, { i64, ptr } } %loaded_value, 1
  %loaded_value3 = load i64, ptr %index2, align 8
  %0 = call ptr @Buffer.getRef_spec_char_10({ i64, ptr } %struc.buffer, i64 %loaded_value3)
  ret ptr %0
}

define i8 @"List.set_spec_enum Char(char) | String(string)_6"({ i64, { i64, ptr } } %self, i64 %index, %"46.5" %elem) {
entry:
  %self1 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %self, ptr %self1, align 8
  %index2 = alloca i64, align 8
  store i64 %index, ptr %index2, align 8
  %elem3 = alloca %"46.5", align 8
  store %"46.5" %elem, ptr %elem3, align 8
  %loaded_value = load { i64, { i64, ptr } }, ptr %self1, align 8
  %struc.buffer = extractvalue { i64, { i64, ptr } } %loaded_value, 1
  %loaded_value4 = load i64, ptr %index2, align 8
  %loaded_value5 = load %"46.5", ptr %elem3, align 8
  %0 = call i8 @"Buffer.set_spec_enum Char(char) | String(string)_7"({ i64, ptr } %struc.buffer, i64 %loaded_value4, %"46.5" %loaded_value5)
  ret i8 %0
}

define i8 @"Buffer.set_spec_enum Char(char) | String(string)_7"({ i64, ptr } %self, i64 %index, %"46.5" %elem) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %index2 = alloca i64, align 8
  store i64 %index, ptr %index2, align 8
  %elem3 = alloca %"46.5", align 8
  store %"46.5" %elem, ptr %elem3, align 8
  %loaded_value = load { i64, ptr }, ptr %self1, align 8
  %loaded_value4 = load i64, ptr %index2, align 8
  %0 = call ptr @"Buffer.getRef_spec_enum Char(char) | String(string)_15"({ i64, ptr } %loaded_value, i64 %loaded_value4)
  %tRef = alloca ptr, align 8
  store ptr %0, ptr %tRef, align 8
  %loaded_value5 = load %"46.5", ptr %elem3, align 8
  %loaded_value6 = load ptr, ptr %tRef, align 8
  store %"46.5" %loaded_value5, ptr %loaded_value6, align 8
  ret i8 0
}

define i8 @List.set_spec_Bool_8({ i64, { i64, ptr } } %self, i64 %index, %Bool %elem) {
entry:
  %self1 = alloca { i64, { i64, ptr } }, align 8
  store { i64, { i64, ptr } } %self, ptr %self1, align 8
  %index2 = alloca i64, align 8
  store i64 %index, ptr %index2, align 8
  %elem3 = alloca %Bool, align 8
  store %Bool %elem, ptr %elem3, align 8
  %loaded_value = load { i64, { i64, ptr } }, ptr %self1, align 8
  %struc.buffer = extractvalue { i64, { i64, ptr } } %loaded_value, 1
  %loaded_value4 = load i64, ptr %index2, align 8
  %loaded_value5 = load %Bool, ptr %elem3, align 8
  %0 = call i8 @Buffer.set_spec_Bool_9({ i64, ptr } %struc.buffer, i64 %loaded_value4, %Bool %loaded_value5)
  ret i8 %0
}

define i8 @Buffer.set_spec_Bool_9({ i64, ptr } %self, i64 %index, %Bool %elem) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %index2 = alloca i64, align 8
  store i64 %index, ptr %index2, align 8
  %elem3 = alloca %Bool, align 8
  store %Bool %elem, ptr %elem3, align 8
  %loaded_value = load { i64, ptr }, ptr %self1, align 8
  %loaded_value4 = load i64, ptr %index2, align 8
  %0 = call ptr @Buffer.getRef_spec_Bool_16({ i64, ptr } %loaded_value, i64 %loaded_value4)
  %tRef = alloca ptr, align 8
  store ptr %0, ptr %tRef, align 8
  %loaded_value5 = load %Bool, ptr %elem3, align 8
  %loaded_value6 = load ptr, ptr %tRef, align 8
  store %Bool %loaded_value5, ptr %loaded_value6, align 8
  ret i8 0
}

define ptr @Buffer.getRef_spec_Bool_16({ i64, ptr } %self, i64 %index) {
entry:
  %self1 = alloca { i64, ptr }, align 8
  store { i64, ptr } %self, ptr %self1, align 8
  %index2 = alloca i64, align 8
  store i64 %index, ptr %index2, align 8
  %loaded_value = load i64, ptr %index2, align 8
  %loaded_value3 = load { i64, ptr }, ptr %self1, align 8
  %struc.len = extractvalue { i64, ptr } %loaded_value3, 0
  %">=_i1" = icmp sge i64 %loaded_value, %struc.len
  %">=_res" = sext i1 %">=_i1" to i8
  %cond_i1 = trunc i8 %">=_res" to i1
  br i1 %cond_i1, label %if_cons, label %if_alt

if_cons:                                          ; preds = %entry
  %0 = load { { i64, ptr } }, ptr @str.82, align 8
  %struct_init_0 = insertvalue { { { i64, ptr } }, i64 } undef, { { i64, ptr } } %0, 0
  %struct_init_1 = insertvalue { { { i64, ptr } }, i64 } %struct_init_0, i64 80, 1
  %1 = load { { i64, ptr } }, ptr @str.84, align 8
  call void @crash({ { { i64, ptr } }, i64 } %struct_init_1, { { i64, ptr } } %1)
  unreachable

if_alt:                                           ; preds = %entry
  br label %if_merge

if_merge:                                         ; preds = %if_alt
  %if_phi = phi i8 [ 0, %if_alt ]
  %loaded_value4 = load { i64, ptr }, ptr %self1, align 8
  %struc.data = extractvalue { i64, ptr } %loaded_value4, 1
  %loaded_value5 = load i64, ptr %index2, align 8
  %refAtIndex = getelementptr inbounds %Bool.11, ptr %struc.data, i64 %loaded_value5
  ret ptr %refAtIndex
}

attributes #0 = { nocallback nofree nosync nounwind readnone speculatable willreturn }

!llvm.module.flags = !{!0, !1, !2, !3}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 14, i32 0]}
!1 = !{i32 2, !"Dwarf Version", i32 4}
!2 = !{i32 2, !"Debug Info Version", i32 3}
!3 = !{i32 1, !"PIE Level", i32 2}
