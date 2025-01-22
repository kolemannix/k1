; ModuleID = 'agg'
source_filename = "builtin.k1"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx11.0.0"

@SEEK_END = constant i32 2
@SEEK_SET = constant i32 0
@mb = constant i64 1048576
@gb = constant i64 1073741824

define i64 @main() !dbg !6 {
entry:
  %struct_literal = alloca { i64, i64, i64 }, align 8, !dbg !19
  %x_load = getelementptr inbounds { i64, i64, i64 }, ptr %struct_literal, i32 0, i32 0, !dbg !20
  store i64 1, ptr %x_load, align 8, !dbg !20
  %y_load = getelementptr inbounds { i64, i64, i64 }, ptr %struct_literal, i32 0, i32 1, !dbg !21
  store i64 2, ptr %y_load, align 8, !dbg !21
  %z_load = getelementptr inbounds { i64, i64, i64 }, ptr %struct_literal, i32 0, i32 2, !dbg !22
  store i64 3, ptr %z_load, align 8, !dbg !22
  %p1 = alloca ptr, align 8, !dbg !22
  call void @llvm.dbg.declare(metadata ptr %p1, metadata !12, metadata !DIExpression()), !dbg !22
  store ptr %struct_literal, ptr %p1, align 8, !dbg !22
  %p1_loaded = load ptr, ptr %p1, align 8, !dbg !23
  %0 = call i64 @one(ptr %p1_loaded), !dbg !24
  ret i64 %0, !dbg !24
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #0

define i64 @one({ i64, i64, i64 } %p) !dbg !25 {
entry:
  %p1 = alloca { i64, i64, i64 }, align 8, !dbg !30
  call void @llvm.dbg.declare(metadata ptr %p1, metadata !29, metadata !DIExpression()), !dbg !30
  store { i64, i64, i64 } %p, ptr %p1, align 8, !dbg !30
  %struc.x = getelementptr inbounds { i64, i64, i64 }, ptr %p1, i32 0, i32 0, !dbg !31
  %struc.x2 = load i64, ptr %struc.x, align 8, !dbg !31
  ret i64 %struc.x2, !dbg !31
}

attributes #0 = { nocallback nofree nosync nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3, !4, !5}

!0 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: "k1_compiler", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, sysroot: "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk", sdk: "MacOSX.sdk")
!1 = !DIFile(filename: "builtin.k1", directory: "/Users/knix/dev/k1/stdlib")
!2 = !{i32 2, !"SDK Version", [2 x i32] [i32 14, i32 0]}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"PIE Level", i32 2}
!6 = distinct !DISubprogram(name: "main", linkageName: "main", scope: null, file: !7, line: 40, type: !8, scopeLine: 40, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !11)
!7 = !DIFile(filename: "agg.k1", directory: "/Users/knix/dev/k1")
!8 = !DISubroutineType(types: !9)
!9 = !{!10}
!10 = !DIBasicType(name: "i64", size: 64, encoding: DW_ATE_signed)
!11 = !{!12}
!12 = !DILocalVariable(name: "p1", scope: !6, file: !7, line: 42, type: !13, align: 64)
!13 = !DIDerivedType(tag: DW_TAG_pointer_type, name: "reference_897", baseType: !14, size: 64, align: 64, dwarfAddressSpace: 0)
!14 = !DICompositeType(tag: DW_TAG_structure_type, name: "{x: i64, y: i64, z: i64}", scope: !6, file: !7, line: 35, size: 192, align: 64, elements: !15, identifier: "{x: i64, y: i64, z: i64}")
!15 = !{!16, !17, !18}
!16 = !DIDerivedType(tag: DW_TAG_member, name: "x", scope: !6, file: !7, line: 35, baseType: !10, size: 64)
!17 = !DIDerivedType(tag: DW_TAG_member, name: "y", scope: !6, file: !7, line: 35, baseType: !10, size: 64, offset: 64)
!18 = !DIDerivedType(tag: DW_TAG_member, name: "z", scope: !6, file: !7, line: 35, baseType: !10, size: 64, offset: 128)
!19 = !DILocation(line: 42, column: 14, scope: !6)
!20 = !DILocation(line: 42, column: 17, scope: !6)
!21 = !DILocation(line: 42, column: 23, scope: !6)
!22 = !DILocation(line: 42, column: 29, scope: !6)
!23 = !DILocation(line: 43, column: 6, scope: !6)
!24 = !DILocation(line: 43, column: 2, scope: !6)
!25 = distinct !DISubprogram(name: "one", linkageName: "one", scope: !6, file: !7, line: 36, type: !26, scopeLine: 36, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !28)
!26 = !DISubroutineType(types: !27)
!27 = !{!10, !14}
!28 = !{!29}
!29 = !DILocalVariable(name: "p", scope: !25, file: !7, line: 38, type: !14)
!30 = !DILocation(line: 36, column: 7, scope: !25)
!31 = !DILocation(line: 37, column: 2, scope: !25)
