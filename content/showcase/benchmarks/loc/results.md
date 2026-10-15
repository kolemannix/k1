# Compiler size: lines of code

Counted 2026-10-13 with `scc version 3.5.0` (code lines only, by scc's own
classification). Each count is the compiler front end:
no standard library, no tests, no build system, and no machine code generation or linking. k1, clang
and rustc hand LLVM IR to LLVM (2,395,042 lines on its own, in nobody's count), and the glue that does
so stays in each count; Go and Zig emit machine code themselves, so their back ends and linkers are
subtracted, with the exact directories named per row. The whole-tree column is the same count before
that subtraction. Nothing here reproduces unattended; the
command and the exact source revision are recorded per row.

Test exclusion is by file: `*_test.rs` and `src/vmtw` (not compiled) for k1, `tests/` directories and
`tests.rs` for rustc, `test.zig` files for zig, `_test.go` and `testdata` for Go. Inline test blocks
stay in the count and are small: about 1,950 raw lines of `#[cfg(test)]` items in k1 (2.7%), 3,550 in
rustc (0.6%), 5,700 lines of `test` declarations in zig (1.1%). k1's count also includes the
`k1_test` runner binary (413 lines).

| compiler | front-end code lines | whole tree | what is counted | back end excluded | source revision |
|---|---|---|---|---|---|
| k1 | 72,204 | 72,204 | src/ (lexer, parser, typer, static VM, IR, LLVM backend, linker shims, LSP, server), tests excluded | nothing: k1 has no machine back end of its own | k1 0.1.0, working tree at 18199ac5 |
| Zig | 115,358 | 497,217 | src/ (the compiler with its own back ends and linkers), tests excluded | src/codegen/* except codegen/llvm (the x86_64, aarch64, arm, loongarch, mips, riscv64, s390x, sparc64, spirv, wasm and C back ends), src/link, register_manager.zig, zig_llvm-ar.cpp | ziglang/zig tag 0.17.0 (7647ada) |
| Go | 286,711 | 547,231 | src/cmd/compile (gc compiler with its own SSA backend; cmd/link and cmd/asm not included), tests excluded | internal/{amd64,arm,arm64,loong64,mips,mips64,ppc64,riscv64,s390x,wasm,x86}, and the per-architecture ssa/rewrite*.go and ssa/_gen/*.{go,rules}; the generic SSA passes and rules stay | go version go1.27.1 darwin/arm64 |
| Rust | 585,849 | 630,574 | compiler/ (rustc and its crates, including the C++ LLVM wrapper; code generation is LLVM, counted separately), tests excluded | rustc_codegen_cranelift and rustc_codegen_gcc; rustc_codegen_ssa and rustc_codegen_llvm stay, as k1's and clang's LLVM glue do | rust-lang/rust tag 1.98.1 (48a229c) |
| clang | 1,276,119 | 1,276,119 | clang/lib + clang/include (C, C++ and Objective-C front end; code generation is LLVM, counted separately) | nothing: clang has no machine back end of its own | llvmorg-23.1.2 (85ac56026) |

## Per language, front end

| compiler | language | code |
|---|---|---|
| k1 | Rust | 72,140 |
| k1 | C++ | 64 |
| Zig | Zig | 113,707 |
| Zig | C++ | 1,561 |
| Zig | C Header | 90 |
| Go | Go | 279,103 |
| Go | SSA rewrite rules (.rules) | 7,608 |
| Rust | Rust | 582,717 |
| Rust | C++ | 3,085 |
| Rust | C Header | 47 |
| clang | C++ | 935,508 |
| clang | C Header | 340,611 |

## Commands

Each command counts the whole tree; the front-end figure is that count with the files under the
excluded paths named in the table left out, summed over the code languages listed per compiler.

- k1, in k1 repo: `scc --no-cocomo --exclude-dir vmtw --not-match '_test\.rs$' src`
- Zig, in ziglang/zig checkout: `scc --no-cocomo --not-match 'test\.zig$' src`
- Go, in $(go env GOROOT): `scc --no-cocomo --exclude-dir testdata --not-match '_test\.go$' src/cmd/compile`
- Rust, in rust-lang/rust checkout: `scc --no-cocomo --exclude-dir tests --not-match 'tests?\.rs$' compiler`
- clang, in llvm-project: `scc --no-cocomo clang/lib clang/include`

The rustc and zig trees were fetched as sparse, blobless, depth-1 checkouts of the tagged release
(`git clone --depth 1 --branch <tag> --filter=blob:none --sparse`, then `git sparse-checkout set compiler` or `src`).
