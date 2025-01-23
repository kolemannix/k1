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
  %struct_literal = alloca { i64, i64, i64 }, align 8, !dbg !22
  %x_load = getelementptr inbounds { i64, i64, i64 }, ptr %struct_literal, i32 0, i32 0, !dbg !23
  store i64 1, ptr %x_load, align 8, !dbg !23
  %y_load = getelementptr inbounds { i64, i64, i64 }, ptr %struct_literal, i32 0, i32 1, !dbg !24
  store i64 2, ptr %y_load, align 8, !dbg !24
  %z_load = getelementptr inbounds { i64, i64, i64 }, ptr %struct_literal, i32 0, i32 2, !dbg !25
  store i64 3, ptr %z_load, align 8, !dbg !25
  %p1 = alloca ptr, align 8, !dbg !25
  call void @llvm.dbg.declare(metadata ptr %p1, metadata !12, metadata !DIExpression()), !dbg !25
  store ptr %struct_literal, ptr %p1, align 8, !dbg !25
  %p1_loaded = load ptr, ptr %p1, align 8, !dbg !26
  %0 = call i64 @one(ptr %p1_loaded), !dbg !27
  %o = alloca i64, align 8, !dbg !27
  call void @llvm.dbg.declare(metadata ptr %o, metadata !19, metadata !DIExpression()), !dbg !27
  store i64 %0, ptr %o, align 8, !dbg !27
  %p1_loaded1 = load ptr, ptr %p1, align 8, !dbg !28
  %1 = call i64 @two(ptr %p1_loaded1), !dbg !29
  %t = alloca i64, align 8, !dbg !29
  call void @llvm.dbg.declare(metadata ptr %t, metadata !20, metadata !DIExpression()), !dbg !29
  store i64 %1, ptr %t, align 8, !dbg !29
  %call_sret = alloca { i64, i64, i64 }, align 8, !dbg !30
  call void @retStruct(ptr %call_sret), !dbg !30
  %p3 = alloca { i64, i64, i64 }, align 8, !dbg !30
  call void @llvm.dbg.declare(metadata ptr %p3, metadata !21, metadata !DIExpression()), !dbg !30
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %p3, ptr align 8 %call_sret, i64 24, i1 false), !dbg !30
  %struc.y = getelementptr inbounds { i64, i64, i64 }, ptr %p3, i32 0, i32 1, !dbg !31
  %struc.y2 = load i64, ptr %struc.y, align 8, !dbg !31
  ret i64 %struc.y2, !dbg !31
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #0

define i64 @one(ptr %p_arg) !dbg !32 {
entry:
  %p = alloca { i64, i64, i64 }, align 8, !dbg !37
  call void @llvm.dbg.declare(metadata ptr %p, metadata !36, metadata !DIExpression()), !dbg !37
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %p, ptr align 8 %p_arg, i64 24, i1 false), !dbg !37
  %struc.z = getelementptr inbounds { i64, i64, i64 }, ptr %p, i32 0, i32 2, !dbg !38
  %struc.z1 = load i64, ptr %struc.z, align 8, !dbg !38
  ret i64 %struc.z1, !dbg !38
}

; Function Attrs: argmemonly nocallback nofree nounwind willreturn
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #1

define i64 @two(ptr %p_arg) !dbg !39 {
entry:
  %p = alloca ptr, align 8, !dbg !44
  call void @llvm.dbg.declare(metadata ptr %p, metadata !43, metadata !DIExpression()), !dbg !44
  store ptr %p_arg, ptr %p, align 8, !dbg !44
  %p_loaded = load ptr, ptr %p, align 8, !dbg !45
  %struc.z = getelementptr inbounds { i64, i64, i64 }, ptr %p_loaded, i32 0, i32 2, !dbg !45
  %struc.z1 = load i64, ptr %struc.z, align 8, !dbg !45
  ret i64 %struc.z1, !dbg !45
}

define void @retStruct(ptr sret({ i64, i64, i64 }) %0) !dbg !46 {
entry:
  %struct_literal = alloca { i64, i64, i64 }, align 8, !dbg !50
  %x_load = getelementptr inbounds { i64, i64, i64 }, ptr %struct_literal, i32 0, i32 0, !dbg !51
  store i64 1, ptr %x_load, align 8, !dbg !51
  %y_load = getelementptr inbounds { i64, i64, i64 }, ptr %struct_literal, i32 0, i32 1, !dbg !52
  store i64 2, ptr %y_load, align 8, !dbg !52
  %z_load = getelementptr inbounds { i64, i64, i64 }, ptr %struct_literal, i32 0, i32 2, !dbg !53
  store i64 3, ptr %z_load, align 8, !dbg !53
  ret ptr %struct_literal, !dbg !53
}

attributes #0 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #1 = { argmemonly nocallback nofree nounwind willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3, !4, !5}

!0 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: "k1_compiler", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, sysroot: "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk", sdk: "MacOSX.sdk")
!1 = !DIFile(filename: "builtin.k1", directory: "/Users/knix/dev/k1/stdlib")
!2 = !{i32 2, !"SDK Version", [2 x i32] [i32 14, i32 0]}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"PIE Level", i32 2}
!6 = distinct !DISubprogram(name: "main", linkageName: "main", scope: null, file: !7, line: 58, type: !8, scopeLine: 58, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !11)
!7 = !DIFile(filename: "agg.k1", directory: "/Users/knix/dev/k1")
!8 = !DISubroutineType(types: !9)
!9 = !{!10}
!10 = !DIBasicType(name: "i64", size: 64, encoding: DW_ATE_signed)
!11 = !{!12, !19, !20, !21}
!12 = !DILocalVariable(name: "p1", scope: !6, file: !7, line: 60, type: !13, align: 64)
!13 = !DIDerivedType(tag: DW_TAG_pointer_type, name: "reference_634", baseType: !14, size: 64, align: 64, dwarfAddressSpace: 0)
!14 = !DICompositeType(tag: DW_TAG_structure_type, name: "{x: i64, y: i64, z: i64}", scope: !6, file: !7, line: 35, size: 192, align: 64, elements: !15, identifier: "{x: i64, y: i64, z: i64}")
!15 = !{!16, !17, !18}
!16 = !DIDerivedType(tag: DW_TAG_member, name: "x", scope: !6, file: !7, line: 35, baseType: !10, size: 64)
!17 = !DIDerivedType(tag: DW_TAG_member, name: "y", scope: !6, file: !7, line: 35, baseType: !10, size: 64, offset: 64)
!18 = !DIDerivedType(tag: DW_TAG_member, name: "z", scope: !6, file: !7, line: 35, baseType: !10, size: 64, offset: 128)
!19 = !DILocalVariable(name: "o", scope: !6, file: !7, line: 61, type: !10, align: 64)
!20 = !DILocalVariable(name: "t", scope: !6, file: !7, line: 62, type: !10, align: 64)
!21 = !DILocalVariable(name: "p3", scope: !6, file: !7, line: 63, type: !14, align: 64)
!22 = !DILocation(line: 60, column: 14, scope: !6)
!23 = !DILocation(line: 60, column: 17, scope: !6)
!24 = !DILocation(line: 60, column: 23, scope: !6)
!25 = !DILocation(line: 60, column: 29, scope: !6)
!26 = !DILocation(line: 61, column: 14, scope: !6)
!27 = !DILocation(line: 61, column: 10, scope: !6)
!28 = !DILocation(line: 62, column: 14, scope: !6)
!29 = !DILocation(line: 62, column: 10, scope: !6)
!30 = !DILocation(line: 63, column: 11, scope: !6)
!31 = !DILocation(line: 64, column: 2, scope: !6)
!32 = distinct !DISubprogram(name: "one", linkageName: "one", scope: !6, file: !7, line: 36, type: !33, scopeLine: 36, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!33 = !DISubroutineType(types: !34)
!34 = !{!10, !14}
!35 = !{!36}
!36 = !DILocalVariable(name: "p", scope: !32, file: !7, line: 39, type: !14)
!37 = !DILocation(line: 36, column: 7, scope: !32)
!38 = !DILocation(line: 38, column: 2, scope: !32)
!39 = distinct !DISubprogram(name: "two", linkageName: "two", scope: !6, file: !7, line: 40, type: !40, scopeLine: 40, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !42)
!40 = !DISubroutineType(types: !41)
!41 = !{!10, !13}
!42 = !{!43}
!43 = !DILocalVariable(name: "p", scope: !39, file: !7, line: 43, type: !13)
!44 = !DILocation(line: 40, column: 7, scope: !39)
!45 = !DILocation(line: 42, column: 2, scope: !39)
!46 = distinct !DISubprogram(name: "retStruct", linkageName: "retStruct", scope: !6, file: !7, line: 54, type: !47, scopeLine: 54, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !49)
!47 = !DISubroutineType(types: !48)
!48 = !{!14}
!49 = !{}
!50 = !DILocation(line: 55, column: 4, scope: !46)
!51 = !DILocation(line: 55, column: 7, scope: !46)
!52 = !DILocation(line: 55, column: 13, scope: !46)
!53 = !DILocation(line: 55, column: 19, scope: !46)
