---
title: Compiler size
weight: 6
charts: [loc]
suites: [loc]
---
Lines of code in each compiler front end: no standard library, no tests, no build system, and no machine code generation or linking. k1, clang and rustc hand LLVM IR to LLVM and that glue stays in; Zig and Go emit machine code themselves, so their back ends and linkers are subtracted, which is most of Zig's tree (its x86_64 back end alone is 199,000 lines). k1's count is the whole `src/` tree: lexer, parser, typer, compile-time VM, IR optimizer, LLVM glue, language server and the server UI. Tests are excluded by file; the inline test blocks that remain are under three percent of any count. Which directories were subtracted, per compiler, is in the report.
