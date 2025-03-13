; ModuleID = 'b'
source_filename = "builtin.k1"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-darwin24.3.0"

@_root__K1_TEST = constant i8 0
@str_data = constant [5 x i8] c"macos"
@str = constant { { i64, ptr } } { { i64, ptr } { i64 5, ptr @str_data } }
@_root__K1_OS = constant { { i64, ptr } } @str
@_root__K1_NO_STD = constant i8 0
@_root__K1_MACOS = constant i8 1
@_root__files__unix__SEEK_END = constant i32 2
@_root__files__unix__SEEK_SET = constant i32 0
@_root__Arena__mb = global i64 1048576
@_root__Arena__gb = global i64 1073741824

define i64 @main() !dbg !6 {
entry:
  %s = alloca { { i64, ptr } }, align 8, !dbg !23
  call void @llvm.dbg.declare(metadata ptr %s, metadata !12, metadata !DIExpression()), !dbg !23
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %s, ptr align 8 @_root__K1_OS, i64 16, i1 false), !dbg !23
  %0 = call i8 @printIt_spec_string_1(ptr %s), !dbg !24
  ret i64 0, !dbg !25
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

define i8 @printIt_spec_string_1(ptr %value_arg) !dbg !26 {
entry:
  %value = alloca { { i64, ptr } }, align 8, !dbg !34
  call void @llvm.dbg.declare(metadata ptr %value, metadata !32, metadata !DIExpression()), !dbg !34
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %value, ptr align 8 %value_arg, i64 16, i1 false), !dbg !34
  %call_sret = alloca { { i64, ptr } }, align 8, !dbg !35
  call void @Show_string_show(ptr sret({ { i64, ptr } }) %call_sret, ptr %value), !dbg !35
  %s = alloca { { i64, ptr } }, align 8, !dbg !35
  call void @llvm.dbg.declare(metadata ptr %s, metadata !33, metadata !DIExpression()), !dbg !35
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %s, ptr align 8 %call_sret, i64 16, i1 false), !dbg !35
  %0 = call i8 @print(ptr %s), !dbg !36
  ret i8 %0, !dbg !36
}

define void @Show_string_show(ptr sret({ { i64, ptr } }) %sret_ptr, ptr %self_arg) !dbg !37 {
entry:
  %self = alloca { { i64, ptr } }, align 8, !dbg !43
  call void @llvm.dbg.declare(metadata ptr %self, metadata !42, metadata !DIExpression()), !dbg !43
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false), !dbg !43
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %sret_ptr, ptr align 8 %self, i64 16, i1 false), !dbg !44
  ret void, !dbg !44
}

define i8 @print(ptr %s_arg) !dbg !45 {
entry:
  %s = alloca { { i64, ptr } }, align 8, !dbg !48
  call void @llvm.dbg.declare(metadata ptr %s, metadata !47, metadata !DIExpression()), !dbg !48
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %s, ptr align 8 %s_arg, i64 16, i1 false), !dbg !48
  %struc.buffer = getelementptr inbounds { { i64, ptr } }, ptr %s, i32 0, i32 0, !dbg !49
  %struc.data = getelementptr inbounds { i64, ptr }, ptr %struc.buffer, i32 0, i32 1, !dbg !49
  %struc.data1 = load ptr, ptr %struc.data, align 8, !dbg !49
  %0 = call i64 @string.len(ptr %s), !dbg !50
  %1 = call i64 @write(i32 1, ptr %struc.data1, i64 %0, i64 0), !dbg !51
  ret i8 0, !dbg !52
}

define i64 @string.len(ptr %self_arg) !dbg !53 {
entry:
  %self = alloca { { i64, ptr } }, align 8, !dbg !58
  call void @llvm.dbg.declare(metadata ptr %self, metadata !57, metadata !DIExpression()), !dbg !58
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %self, ptr align 8 %self_arg, i64 16, i1 false), !dbg !58
  %struc.buffer = getelementptr inbounds { { i64, ptr } }, ptr %self, i32 0, i32 0, !dbg !59
  %struc.len = getelementptr inbounds { i64, ptr }, ptr %struc.buffer, i32 0, i32 0, !dbg !59
  %struc.len1 = load i64, ptr %struc.len, align 8, !dbg !59
  ret i64 %struc.len1, !dbg !59
}

declare i64 @write(i32, ptr, i64, i64)

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3, !4, !5}

!0 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: "k1_compiler", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, sysroot: "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk", sdk: "MacOSX.sdk")
!1 = !DIFile(filename: "builtin.k1", directory: "/Users/knix/dev/k1/stdlib")
!2 = !{i32 2, !"SDK Version", [2 x i32] [i32 14, i32 0]}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"PIE Level", i32 2}
!6 = distinct !DISubprogram(name: "main", linkageName: "main", scope: null, file: !7, line: 1, type: !8, scopeLine: 1, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !11)
!7 = !DIFile(filename: "b.k1", directory: "/Users/knix/dev/k1")
!8 = !DISubroutineType(types: !9)
!9 = !{!10}
!10 = !DIBasicType(name: "i64", size: 64, encoding: DW_ATE_signed)
!11 = !{!12}
!12 = !DILocalVariable(name: "s", scope: !6, file: !7, line: 2, type: !13, align: 64)
!13 = !DICompositeType(tag: DW_TAG_structure_type, name: "string", file: !1, line: 1, size: 128, align: 64, elements: !14, identifier: "string")
!14 = !{!15}
!15 = !DIDerivedType(tag: DW_TAG_member, name: "buffer", file: !1, line: 1, baseType: !16, size: 128, align: 64)
!16 = !DICompositeType(tag: DW_TAG_structure_type, name: "Buffer[char]", file: !1, line: 1, size: 128, align: 64, elements: !17, identifier: "Buffer[char]")
!17 = !{!18, !20}
!18 = !DIDerivedType(tag: DW_TAG_member, name: "len", file: !1, line: 1, baseType: !19, size: 64)
!19 = !DIBasicType(name: "u64", size: 64, encoding: DW_ATE_unsigned)
!20 = !DIDerivedType(tag: DW_TAG_member, name: "data", file: !1, line: 1, baseType: !21, size: 64, align: 64, offset: 64)
!21 = !DIDerivedType(tag: DW_TAG_pointer_type, name: "reference_25", baseType: !22, size: 64, align: 64, dwarfAddressSpace: 0)
!22 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!23 = !DILocation(line: 2, column: 10, scope: !6)
!24 = !DILocation(line: 3, column: 2, scope: !6)
!25 = !DILocation(line: 4, column: 2, scope: !6)
!26 = distinct !DISubprogram(name: "printIt_spec_string_1", linkageName: "printIt_spec_string_1", scope: !6, file: !27, line: 38, type: !28, scopeLine: 38, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !31)
!27 = !DIFile(filename: "core.k1", directory: "/Users/knix/dev/k1/stdlib")
!28 = !DISubroutineType(types: !29)
!29 = !{!30, !13}
!30 = !DIBasicType(name: "unit", size: 8, encoding: DW_ATE_boolean)
!31 = !{!32, !33}
!32 = !DILocalVariable(name: "value", scope: !26, file: !27, line: 41, type: !13)
!33 = !DILocalVariable(name: "s", scope: !26, file: !27, line: 39, type: !13, align: 64)
!34 = !DILocation(line: 38, column: 20, scope: !26)
!35 = !DILocation(line: 39, column: 10, scope: !26)
!36 = !DILocation(line: 40, column: 2, scope: !26)
!37 = distinct !DISubprogram(name: "Show_string_show", linkageName: "Show_string_show", scope: !26, file: !38, line: 179, type: !39, scopeLine: 179, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !41)
!38 = !DIFile(filename: "string.k1", directory: "/Users/knix/dev/k1/stdlib")
!39 = !DISubroutineType(types: !40)
!40 = !{!13, !13}
!41 = !{!42}
!42 = !DILocalVariable(name: "self", arg: 1, scope: !37, file: !38, line: 179, type: !13)
!43 = !DILocation(line: 18, column: 11, scope: !37)
!44 = !DILocation(line: 179, column: 34, scope: !37)
!45 = distinct !DISubprogram(name: "print", linkageName: "print", scope: !26, file: !27, line: 43, type: !28, scopeLine: 43, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !46)
!46 = !{!47}
!47 = !DILocalVariable(name: "s", scope: !45, file: !27, line: 46, type: !13)
!48 = !DILocation(line: 43, column: 9, scope: !45)
!49 = !DILocation(line: 44, column: 32, scope: !45)
!50 = !DILocation(line: 44, column: 66, scope: !45)
!51 = !DILocation(line: 44, column: 2, scope: !45)
!52 = !DILocation(line: 45, column: 2, scope: !45)
!53 = distinct !DISubprogram(name: "len", linkageName: "len", scope: !45, file: !38, line: 49, type: !54, scopeLine: 49, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !56)
!54 = !DISubroutineType(types: !55)
!55 = !{!19, !13}
!56 = !{!57}
!57 = !DILocalVariable(name: "self", scope: !53, file: !38, line: 49, type: !13)
!58 = !DILocation(line: 49, column: 9, scope: !53)
!59 = !DILocation(line: 49, column: 30, scope: !53)
