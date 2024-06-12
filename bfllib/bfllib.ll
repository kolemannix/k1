; ModuleID = 'bfllib/bfllib.c'
source_filename = "bfllib/bfllib.c"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx14.0.0"

%struct.BflString = type { i64, ptr }

@.str = private unnamed_addr constant [2 x i8] c"r\00", align 1, !dbg !0

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define [2 x i64] @_bfl_charToString(i8 noundef signext %0) #0 !dbg !21 {
  %2 = alloca %struct.BflString, align 8
  %3 = alloca i8, align 1
  %4 = alloca ptr, align 8
  store i8 %0, ptr %3, align 1
  call void @llvm.dbg.declare(metadata ptr %3, metadata !34, metadata !DIExpression()), !dbg !35
  call void @llvm.dbg.declare(metadata ptr %4, metadata !36, metadata !DIExpression()), !dbg !37
  %5 = call ptr @malloc(i64 noundef 1) #4, !dbg !38
  store ptr %5, ptr %4, align 8, !dbg !37
  %6 = load i8, ptr %3, align 1, !dbg !39
  %7 = load ptr, ptr %4, align 8, !dbg !40
  %8 = getelementptr inbounds i8, ptr %7, i64 0, !dbg !40
  store i8 %6, ptr %8, align 1, !dbg !41
  call void @llvm.dbg.declare(metadata ptr %2, metadata !42, metadata !DIExpression()), !dbg !43
  %9 = getelementptr inbounds %struct.BflString, ptr %2, i32 0, i32 0, !dbg !44
  store i64 1, ptr %9, align 8, !dbg !44
  %10 = getelementptr inbounds %struct.BflString, ptr %2, i32 0, i32 1, !dbg !44
  %11 = load ptr, ptr %4, align 8, !dbg !45
  store ptr %11, ptr %10, align 8, !dbg !44
  %12 = load [2 x i64], ptr %2, align 8, !dbg !46
  ret [2 x i64] %12, !dbg !46
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: allocsize(0)
declare ptr @malloc(i64 noundef) #2

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define [2 x i64] @_bfl_readFileToString([2 x i64] %0) #0 !dbg !47 {
  %2 = alloca %struct.BflString, align 8
  %3 = alloca %struct.BflString, align 8
  %4 = alloca ptr, align 8
  %5 = alloca i64, align 8
  %6 = alloca ptr, align 8
  store [2 x i64] %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !50, metadata !DIExpression()), !dbg !51
  call void @llvm.dbg.declare(metadata ptr %4, metadata !52, metadata !DIExpression()), !dbg !114
  %7 = getelementptr inbounds %struct.BflString, ptr %3, i32 0, i32 1, !dbg !115
  %8 = load ptr, ptr %7, align 8, !dbg !115
  %9 = call ptr @"\01_fopen"(ptr noundef %8, ptr noundef @.str), !dbg !116
  store ptr %9, ptr %4, align 8, !dbg !114
  %10 = load ptr, ptr %4, align 8, !dbg !117
  %11 = call i32 @fseek(ptr noundef %10, i64 noundef 0, i32 noundef 2), !dbg !118
  call void @llvm.dbg.declare(metadata ptr %5, metadata !119, metadata !DIExpression()), !dbg !121
  %12 = load ptr, ptr %4, align 8, !dbg !122
  %13 = call i64 @ftell(ptr noundef %12), !dbg !123
  store i64 %13, ptr %5, align 8, !dbg !121
  call void @llvm.dbg.declare(metadata ptr %6, metadata !124, metadata !DIExpression()), !dbg !125
  %14 = load i64, ptr %5, align 8, !dbg !126
  %15 = call ptr @malloc(i64 noundef %14) #4, !dbg !127
  store ptr %15, ptr %6, align 8, !dbg !125
  %16 = load ptr, ptr %4, align 8, !dbg !128
  %17 = call i32 @fseek(ptr noundef %16, i64 noundef 0, i32 noundef 0), !dbg !129
  %18 = load ptr, ptr %6, align 8, !dbg !130
  %19 = load i64, ptr %5, align 8, !dbg !131
  %20 = load ptr, ptr %4, align 8, !dbg !132
  %21 = call i64 @fread(ptr noundef %18, i64 noundef %19, i64 noundef 1, ptr noundef %20), !dbg !133
  call void @llvm.dbg.declare(metadata ptr %2, metadata !134, metadata !DIExpression()), !dbg !135
  %22 = getelementptr inbounds %struct.BflString, ptr %2, i32 0, i32 0, !dbg !136
  %23 = load i64, ptr %5, align 8, !dbg !137
  store i64 %23, ptr %22, align 8, !dbg !136
  %24 = getelementptr inbounds %struct.BflString, ptr %2, i32 0, i32 1, !dbg !136
  %25 = load ptr, ptr %6, align 8, !dbg !138
  store ptr %25, ptr %24, align 8, !dbg !136
  %26 = load ptr, ptr %4, align 8, !dbg !139
  %27 = call i32 @fclose(ptr noundef %26), !dbg !140
  %28 = load [2 x i64], ptr %2, align 8, !dbg !141
  ret [2 x i64] %28, !dbg !141
}

declare ptr @"\01_fopen"(ptr noundef, ptr noundef) #3

declare i32 @fseek(ptr noundef, i64 noundef, i32 noundef) #3

declare i64 @ftell(ptr noundef) #3

declare i64 @fread(ptr noundef, i64 noundef, i64 noundef, ptr noundef) #3

declare i32 @fclose(ptr noundef) #3

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define i64 @_bfl_charToInt(i8 noundef signext %0) #0 !dbg !142 {
  %2 = alloca i8, align 1
  store i8 %0, ptr %2, align 1
  call void @llvm.dbg.declare(metadata ptr %2, metadata !144, metadata !DIExpression()), !dbg !145
  %3 = load i8, ptr %2, align 1, !dbg !146
  %4 = sext i8 %3 to i64, !dbg !147
  ret i64 %4, !dbg !148
}

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define signext i8 @_bfl_intToChar(i64 noundef %0) #0 !dbg !149 {
  %2 = alloca i64, align 8
  store i64 %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !152, metadata !DIExpression()), !dbg !153
  %3 = load i64, ptr %2, align 8, !dbg !154
  %4 = trunc i64 %3 to i8, !dbg !155
  ret i8 %4, !dbg !156
}

attributes #0 = { noinline nounwind optnone ssp uwtable(sync) "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8.5a,+v8a,+zcm,+zcz" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { allocsize(0) "frame-pointer"="non-leaf" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8.5a,+v8a,+zcm,+zcz" }
attributes #3 = { "frame-pointer"="non-leaf" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8.5a,+v8a,+zcm,+zcz" }
attributes #4 = { allocsize(0) }

!llvm.module.flags = !{!7, !8, !9, !10, !11, !12, !13}
!llvm.dbg.cu = !{!14}
!llvm.ident = !{!20}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 20, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "bfllib/bfllib.c", directory: "/Users/knix/dev/bfl")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 2)
!7 = !{i32 2, !"SDK Version", [2 x i32] [i32 14, i32 4]}
!8 = !{i32 7, !"Dwarf Version", i32 4}
!9 = !{i32 2, !"Debug Info Version", i32 3}
!10 = !{i32 1, !"wchar_size", i32 4}
!11 = !{i32 8, !"PIC Level", i32 2}
!12 = !{i32 7, !"uwtable", i32 1}
!13 = !{i32 7, !"frame-pointer", i32 1}
!14 = distinct !DICompileUnit(language: DW_LANG_C99, file: !2, producer: "Apple clang version 15.0.0 (clang-1500.3.9.4)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !15, globals: !19, splitDebugInlining: false, nameTableKind: None, sysroot: "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk", sdk: "MacOSX.sdk")
!15 = !{!16, !4}
!16 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !17, line: 30, baseType: !18)
!17 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_int64_t.h", directory: "")
!18 = !DIBasicType(name: "long long", size: 64, encoding: DW_ATE_signed)
!19 = !{!0}
!20 = !{!"Apple clang version 15.0.0 (clang-1500.3.9.4)"}
!21 = distinct !DISubprogram(name: "_bfl_charToString", scope: !2, file: !2, line: 9, type: !22, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !14, retainedNodes: !33)
!22 = !DISubroutineType(types: !23)
!23 = !{!24, !4}
!24 = !DIDerivedType(tag: DW_TAG_typedef, name: "BflString", file: !2, line: 7, baseType: !25)
!25 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !2, line: 4, size: 128, elements: !26)
!26 = !{!27, !31}
!27 = !DIDerivedType(tag: DW_TAG_member, name: "len", scope: !25, file: !2, line: 5, baseType: !28, size: 64)
!28 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint64_t", file: !29, line: 31, baseType: !30)
!29 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_types/_uint64_t.h", directory: "")
!30 = !DIBasicType(name: "unsigned long long", size: 64, encoding: DW_ATE_unsigned)
!31 = !DIDerivedType(tag: DW_TAG_member, name: "data", scope: !25, file: !2, line: 6, baseType: !32, size: 64, offset: 64)
!32 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!33 = !{}
!34 = !DILocalVariable(name: "c", arg: 1, scope: !21, file: !2, line: 9, type: !4)
!35 = !DILocation(line: 9, column: 34, scope: !21)
!36 = !DILocalVariable(name: "data", scope: !21, file: !2, line: 10, type: !32)
!37 = !DILocation(line: 10, column: 9, scope: !21)
!38 = !DILocation(line: 10, column: 16, scope: !21)
!39 = !DILocation(line: 11, column: 13, scope: !21)
!40 = !DILocation(line: 11, column: 3, scope: !21)
!41 = !DILocation(line: 11, column: 11, scope: !21)
!42 = !DILocalVariable(name: "string", scope: !21, file: !2, line: 12, type: !24)
!43 = !DILocation(line: 12, column: 13, scope: !21)
!44 = !DILocation(line: 12, column: 22, scope: !21)
!45 = !DILocation(line: 14, column: 15, scope: !21)
!46 = !DILocation(line: 16, column: 3, scope: !21)
!47 = distinct !DISubprogram(name: "_bfl_readFileToString", scope: !2, file: !2, line: 19, type: !48, scopeLine: 19, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !14, retainedNodes: !33)
!48 = !DISubroutineType(types: !49)
!49 = !{!24, !24}
!50 = !DILocalVariable(name: "filename", arg: 1, scope: !47, file: !2, line: 19, type: !24)
!51 = !DILocation(line: 19, column: 43, scope: !47)
!52 = !DILocalVariable(name: "file", scope: !47, file: !2, line: 20, type: !53)
!53 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !54, size: 64)
!54 = !DIDerivedType(tag: DW_TAG_typedef, name: "FILE", file: !55, line: 157, baseType: !56)
!55 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_stdio.h", directory: "")
!56 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__sFILE", file: !55, line: 126, size: 1216, elements: !57)
!57 = !{!58, !61, !63, !64, !66, !67, !72, !73, !75, !79, !83, !92, !98, !99, !102, !103, !107, !111, !112, !113}
!58 = !DIDerivedType(tag: DW_TAG_member, name: "_p", scope: !56, file: !55, line: 127, baseType: !59, size: 64)
!59 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !60, size: 64)
!60 = !DIBasicType(name: "unsigned char", size: 8, encoding: DW_ATE_unsigned_char)
!61 = !DIDerivedType(tag: DW_TAG_member, name: "_r", scope: !56, file: !55, line: 128, baseType: !62, size: 32, offset: 64)
!62 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!63 = !DIDerivedType(tag: DW_TAG_member, name: "_w", scope: !56, file: !55, line: 129, baseType: !62, size: 32, offset: 96)
!64 = !DIDerivedType(tag: DW_TAG_member, name: "_flags", scope: !56, file: !55, line: 130, baseType: !65, size: 16, offset: 128)
!65 = !DIBasicType(name: "short", size: 16, encoding: DW_ATE_signed)
!66 = !DIDerivedType(tag: DW_TAG_member, name: "_file", scope: !56, file: !55, line: 131, baseType: !65, size: 16, offset: 144)
!67 = !DIDerivedType(tag: DW_TAG_member, name: "_bf", scope: !56, file: !55, line: 132, baseType: !68, size: 128, offset: 192)
!68 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__sbuf", file: !55, line: 92, size: 128, elements: !69)
!69 = !{!70, !71}
!70 = !DIDerivedType(tag: DW_TAG_member, name: "_base", scope: !68, file: !55, line: 93, baseType: !59, size: 64)
!71 = !DIDerivedType(tag: DW_TAG_member, name: "_size", scope: !68, file: !55, line: 94, baseType: !62, size: 32, offset: 64)
!72 = !DIDerivedType(tag: DW_TAG_member, name: "_lbfsize", scope: !56, file: !55, line: 133, baseType: !62, size: 32, offset: 320)
!73 = !DIDerivedType(tag: DW_TAG_member, name: "_cookie", scope: !56, file: !55, line: 136, baseType: !74, size: 64, offset: 384)
!74 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!75 = !DIDerivedType(tag: DW_TAG_member, name: "_close", scope: !56, file: !55, line: 137, baseType: !76, size: 64, offset: 448)
!76 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !77, size: 64)
!77 = !DISubroutineType(types: !78)
!78 = !{!62, !74}
!79 = !DIDerivedType(tag: DW_TAG_member, name: "_read", scope: !56, file: !55, line: 138, baseType: !80, size: 64, offset: 512)
!80 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !81, size: 64)
!81 = !DISubroutineType(types: !82)
!82 = !{!62, !74, !32, !62}
!83 = !DIDerivedType(tag: DW_TAG_member, name: "_seek", scope: !56, file: !55, line: 139, baseType: !84, size: 64, offset: 576)
!84 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !85, size: 64)
!85 = !DISubroutineType(types: !86)
!86 = !{!87, !74, !87, !62}
!87 = !DIDerivedType(tag: DW_TAG_typedef, name: "fpos_t", file: !55, line: 81, baseType: !88)
!88 = !DIDerivedType(tag: DW_TAG_typedef, name: "__darwin_off_t", file: !89, line: 71, baseType: !90)
!89 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types.h", directory: "")
!90 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !91, line: 24, baseType: !18)
!91 = !DIFile(filename: "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/arm/_types.h", directory: "")
!92 = !DIDerivedType(tag: DW_TAG_member, name: "_write", scope: !56, file: !55, line: 140, baseType: !93, size: 64, offset: 640)
!93 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !94, size: 64)
!94 = !DISubroutineType(types: !95)
!95 = !{!62, !74, !96, !62}
!96 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !97, size: 64)
!97 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !4)
!98 = !DIDerivedType(tag: DW_TAG_member, name: "_ub", scope: !56, file: !55, line: 143, baseType: !68, size: 128, offset: 704)
!99 = !DIDerivedType(tag: DW_TAG_member, name: "_extra", scope: !56, file: !55, line: 144, baseType: !100, size: 64, offset: 832)
!100 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !101, size: 64)
!101 = !DICompositeType(tag: DW_TAG_structure_type, name: "__sFILEX", file: !55, line: 98, flags: DIFlagFwdDecl)
!102 = !DIDerivedType(tag: DW_TAG_member, name: "_ur", scope: !56, file: !55, line: 145, baseType: !62, size: 32, offset: 896)
!103 = !DIDerivedType(tag: DW_TAG_member, name: "_ubuf", scope: !56, file: !55, line: 148, baseType: !104, size: 24, offset: 928)
!104 = !DICompositeType(tag: DW_TAG_array_type, baseType: !60, size: 24, elements: !105)
!105 = !{!106}
!106 = !DISubrange(count: 3)
!107 = !DIDerivedType(tag: DW_TAG_member, name: "_nbuf", scope: !56, file: !55, line: 149, baseType: !108, size: 8, offset: 952)
!108 = !DICompositeType(tag: DW_TAG_array_type, baseType: !60, size: 8, elements: !109)
!109 = !{!110}
!110 = !DISubrange(count: 1)
!111 = !DIDerivedType(tag: DW_TAG_member, name: "_lb", scope: !56, file: !55, line: 152, baseType: !68, size: 128, offset: 960)
!112 = !DIDerivedType(tag: DW_TAG_member, name: "_blksize", scope: !56, file: !55, line: 155, baseType: !62, size: 32, offset: 1088)
!113 = !DIDerivedType(tag: DW_TAG_member, name: "_offset", scope: !56, file: !55, line: 156, baseType: !87, size: 64, offset: 1152)
!114 = !DILocation(line: 20, column: 11, scope: !47)
!115 = !DILocation(line: 20, column: 33, scope: !47)
!116 = !DILocation(line: 20, column: 18, scope: !47)
!117 = !DILocation(line: 21, column: 11, scope: !47)
!118 = !DILocation(line: 21, column: 5, scope: !47)
!119 = !DILocalVariable(name: "fsize", scope: !47, file: !2, line: 22, type: !120)
!120 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!121 = !DILocation(line: 22, column: 10, scope: !47)
!122 = !DILocation(line: 22, column: 24, scope: !47)
!123 = !DILocation(line: 22, column: 18, scope: !47)
!124 = !DILocalVariable(name: "buf", scope: !47, file: !2, line: 23, type: !32)
!125 = !DILocation(line: 23, column: 11, scope: !47)
!126 = !DILocation(line: 23, column: 24, scope: !47)
!127 = !DILocation(line: 23, column: 17, scope: !47)
!128 = !DILocation(line: 25, column: 11, scope: !47)
!129 = !DILocation(line: 25, column: 5, scope: !47)
!130 = !DILocation(line: 26, column: 11, scope: !47)
!131 = !DILocation(line: 26, column: 16, scope: !47)
!132 = !DILocation(line: 26, column: 26, scope: !47)
!133 = !DILocation(line: 26, column: 5, scope: !47)
!134 = !DILocalVariable(name: "string", scope: !47, file: !2, line: 27, type: !24)
!135 = !DILocation(line: 27, column: 15, scope: !47)
!136 = !DILocation(line: 27, column: 24, scope: !47)
!137 = !DILocation(line: 28, column: 16, scope: !47)
!138 = !DILocation(line: 29, column: 17, scope: !47)
!139 = !DILocation(line: 31, column: 12, scope: !47)
!140 = !DILocation(line: 31, column: 5, scope: !47)
!141 = !DILocation(line: 32, column: 5, scope: !47)
!142 = distinct !DISubprogram(name: "_bfl_charToInt", scope: !2, file: !2, line: 37, type: !143, scopeLine: 37, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !14, retainedNodes: !33)
!143 = !DISubroutineType(types: !15)
!144 = !DILocalVariable(name: "c", arg: 1, scope: !142, file: !2, line: 37, type: !4)
!145 = !DILocation(line: 37, column: 29, scope: !142)
!146 = !DILocation(line: 38, column: 21, scope: !142)
!147 = !DILocation(line: 38, column: 12, scope: !142)
!148 = !DILocation(line: 38, column: 5, scope: !142)
!149 = distinct !DISubprogram(name: "_bfl_intToChar", scope: !2, file: !2, line: 41, type: !150, scopeLine: 41, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !14, retainedNodes: !33)
!150 = !DISubroutineType(types: !151)
!151 = !{!4, !16}
!152 = !DILocalVariable(name: "i", arg: 1, scope: !149, file: !2, line: 41, type: !16)
!153 = !DILocation(line: 41, column: 29, scope: !149)
!154 = !DILocation(line: 42, column: 18, scope: !149)
!155 = !DILocation(line: 42, column: 12, scope: !149)
!156 = !DILocation(line: 42, column: 5, scope: !149)
