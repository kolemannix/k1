# Compiler size: lines of code

Counted 2026-10-12 with `scc version 3.5.0` (code lines only: blank and comment lines are
scc's own classification and are listed separately). Each count is the compiler proper:
no standard library, no tests, no build system. k1, clang and rustc all generate machine code
through LLVM, so LLVM is its own row and is not included in any compiler's count; Go and Zig
carry their own back ends inside the counted tree. Nothing here reproduces unattended; the
command and the exact source revision are recorded per row.

Test exclusion is by file: `*_test.rs` and `src/vmtw` (not compiled) for k1, `tests/` directories and
`tests.rs` for rustc, `test.zig` files for zig, `_test.go` and `testdata` for Go. Inline test blocks
stay in the count and are small: about 1,950 raw lines of `#[cfg(test)]` items in k1 (2.7%), 3,550 in
rustc (0.6%), 5,700 lines of `test` declarations in zig (1.1%). k1's count also includes the
`k1_test` runner binary (413 lines).

| compiler | code lines | what is counted | source revision |
|---|---|---|---|
| k1 | 72,189 | src/ (lexer, parser, typer, static VM, IR, LLVM backend, linker shims, LSP, server), tests excluded | k1 0.1.0, working tree at acba9226 |
| Zig | 497,217 | src/ (the compiler with its own back ends and linkers), tests excluded | ziglang/zig tag 0.17.0 (7647ada) |
| Go | 547,231 | src/cmd/compile (gc compiler with its own SSA backend; cmd/link and cmd/asm not included), tests excluded | go version go1.27.1 darwin/arm64 |
| Rust | 630,574 | compiler/ (rustc and its crates, including the C++ LLVM wrapper; code generation is LLVM, counted separately), tests excluded | rust-lang/rust tag 1.98.1 (48a229c) |
| clang | 1,276,119 | clang/lib + clang/include (C, C++ and Objective-C front end; code generation is LLVM, counted separately) | llvmorg-23.1.2 (85ac56026) |
| LLVM | 2,395,042 | llvm/lib + llvm/include (the optimizer and code generator under k1, clang and rustc) | llvmorg-23.1.2 (85ac56026) |

## Per language

| compiler | language | files | code | comment | blank |
|---|---|---|---|---|---|
| k1 | Rust | 59 | 72,125 | 2,348 | 4,891 |
| k1 | C++ | 2 | 64 | 3 | 5 |
| Zig | Zig | 180 | 494,320 | 20,802 | 28,814 |
| Zig | C++ | 5 | 2,807 | 398 | 452 |
| Zig | C Header | 1 | 90 | 24 | 18 |
| Go | Go | 446 | 527,898 | 72,035 | 18,597 |
| Go | SSA rewrite rules (.rules) | 27 | 19,333 | 0 | 1,310 |
| Rust | Rust | 1987 | 627,442 | 124,758 | 74,432 |
| Rust | C++ | 5 | 3,085 | 402 | 421 |
| Rust | C Header | 2 | 47 | 5 | 18 |
| clang | C Header | 1372 | 340,611 | 163,685 | 86,743 |
| clang | C++ | 1109 | 935,508 | 171,346 | 151,654 |
| LLVM | C Header | 3153 | 427,871 | 188,488 | 111,669 |
| LLVM | C++ | 3098 | 1,957,516 | 384,524 | 313,227 |
| LLVM | C | 16 | 9,655 | 1,214 | 819 |

## Commands

- k1, in k1 repo: `scc --no-cocomo --exclude-dir vmtw --not-match '_test\.rs$' src`
- Zig, in ziglang/zig checkout: `scc --no-cocomo --not-match 'test\.zig$' src`
- Go, in $(go env GOROOT): `scc --no-cocomo --exclude-dir testdata --not-match '_test\.go$' src/cmd/compile`
- Rust, in rust-lang/rust checkout: `scc --no-cocomo --exclude-dir tests --not-match 'tests?\.rs$' compiler`
- clang, in llvm-project: `scc --no-cocomo clang/lib clang/include`
- LLVM, in llvm-project: `scc --no-cocomo llvm/lib llvm/include`

The rustc and zig trees were fetched as sparse, blobless, depth-1 checkouts of the tagged release
(`git clone --depth 1 --branch <tag> --filter=blob:none --sparse`, then `git sparse-checkout set compiler` or `src`).
