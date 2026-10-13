---
title: Compiler size
weight: 6
charts: [loc]
suites: [loc]
---

Lines of code in each compiler proper: no standard library, no tests, no build system. k1's count is the whole `src/` tree, which includes the lexer, parser, typer, the compile-time VM, the IR optimizer, the LLVM back end glue, the language server and the server UI. k1, clang and rustc all generate machine code through LLVM, so LLVM is its own row and is in nobody's count; Go and Zig carry their own back ends inside the counted tree. Tests are excluded by file; the inline test blocks that remain are under three percent of any count.
