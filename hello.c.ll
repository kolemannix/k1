; ModuleID = 'hello.c'
source_filename = "hello.c"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx14.0.0"

%struct.Point = type { ptr, i64, i64 }

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define void @make_point(ptr noalias sret(%struct.Point) align 8 %0) #0 !dbg !10 {
  %2 = alloca i64, align 8
  call void @llvm.dbg.declare(metadata ptr %0, metadata !22, metadata !DIExpression()), !dbg !23
  call void @llvm.dbg.declare(metadata ptr %2, metadata !24, metadata !DIExpression()), !dbg !25
  store i64 42, ptr %2, align 8, !dbg !25
  %3 = getelementptr inbounds %struct.Point, ptr %0, i32 0, i32 0, !dbg !26
  store ptr %2, ptr %3, align 8, !dbg !27
  %4 = getelementptr inbounds %struct.Point, ptr %0, i32 0, i32 1, !dbg !28
  store i64 13, ptr %4, align 8, !dbg !29
  ret void, !dbg !30
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define i32 @main() #0 !dbg !31 {
  %1 = alloca i32, align 4
  store i32 0, ptr %1, align 4
  ret i32 0, !dbg !35
}

attributes #0 = { noinline nounwind optnone ssp uwtable(sync) "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8.5a,+v8a,+zcm,+zcz" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6}
!llvm.dbg.cu = !{!7}
!llvm.ident = !{!9}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 14, i32 2]}
!1 = !{i32 7, !"Dwarf Version", i32 4}
!2 = !{i32 2, !"Debug Info Version", i32 3}
!3 = !{i32 1, !"wchar_size", i32 4}
!4 = !{i32 8, !"PIC Level", i32 2}
!5 = !{i32 7, !"uwtable", i32 1}
!6 = !{i32 7, !"frame-pointer", i32 1}
!7 = distinct !DICompileUnit(language: DW_LANG_C99, file: !8, producer: "Apple clang version 15.0.0 (clang-1500.1.0.2.5)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None, sysroot: "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk", sdk: "MacOSX.sdk")
!8 = !DIFile(filename: "hello.c", directory: "/Users/knix/dev/nexlang")
!9 = !{!"Apple clang version 15.0.0 (clang-1500.1.0.2.5)"}
!10 = distinct !DISubprogram(name: "make_point", scope: !8, file: !8, line: 7, type: !11, scopeLine: 7, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !21)
!11 = !DISubroutineType(types: !12)
!12 = !{!13}
!13 = !DIDerivedType(tag: DW_TAG_typedef, name: "Point", file: !8, line: 5, baseType: !14)
!14 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !8, line: 1, size: 192, elements: !15)
!15 = !{!16, !19, !20}
!16 = !DIDerivedType(tag: DW_TAG_member, name: "x", scope: !14, file: !8, line: 2, baseType: !17, size: 64)
!17 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !18, size: 64)
!18 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!19 = !DIDerivedType(tag: DW_TAG_member, name: "y", scope: !14, file: !8, line: 3, baseType: !18, size: 64, offset: 64)
!20 = !DIDerivedType(tag: DW_TAG_member, name: "z", scope: !14, file: !8, line: 4, baseType: !18, size: 64, offset: 128)
!21 = !{}
!22 = !DILocalVariable(name: "p", scope: !10, file: !8, line: 8, type: !13)
!23 = !DILocation(line: 8, column: 9, scope: !10)
!24 = !DILocalVariable(name: "x", scope: !10, file: !8, line: 9, type: !18)
!25 = !DILocation(line: 9, column: 8, scope: !10)
!26 = !DILocation(line: 10, column: 5, scope: !10)
!27 = !DILocation(line: 10, column: 7, scope: !10)
!28 = !DILocation(line: 11, column: 5, scope: !10)
!29 = !DILocation(line: 11, column: 7, scope: !10)
!30 = !DILocation(line: 12, column: 3, scope: !10)
!31 = distinct !DISubprogram(name: "main", scope: !8, file: !8, line: 15, type: !32, scopeLine: 15, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !21)
!32 = !DISubroutineType(types: !33)
!33 = !{!34}
!34 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!35 = !DILocation(line: 16, column: 3, scope: !31)
