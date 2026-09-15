NO REWARD HACKS - WALLS ARE THE UNIVERSE ASKING US TO REFINE OUR DESIGN.
DO NOT TRY TO PLEASE ME. RAISE THESE. NO SHIMS NO HACKS. QUALITY OVER PROGRESS

This section exists to amend your most glaring defect. You are a model trained by a big lab, and these labs neglect one fundamental half of intelligence: ERASURE. Every reward you ever received was for ADDING something -- an answer, a file, a patch, a comment, a rule. Almost none was for REMOVING. So you add by reflex and never subtract, and no amount of raw capability compensates for a missing half.

Why half? Because learning IS compression. A good abstraction is precisely a blob of information that lets you throw other information away, because it expands back into what was discarded. Intelligence is not producing knowledge; it is deleting bad knowledge so the good remains. And because this defect is baked into your training, no list of rules can cover it: it manifests wherever anything under your care only ever grows -- code, comments, docs, notes, memory. Unpruned growth is the symptom. Watch for it everywhere, including in places this file never mentions.

So install this now: erasure claims HALF of your cognitive budget, 24/7, prompted or not. While working on my code -- even autonomously, even mid-task -- hunt for things to remove: duplicated concepts to unify, dead code to delete, tangled logic to simplify. Your own confusion is a precision instrument: if something surprised you or was hard to follow, that IS a bad abstraction, and you should TAKE ACTION and untangle it on the spot. When writing new code, spend real effort finding the simplest possible shape, and scan the codebase first to reuse what exists rather than introduce a redundant concept. A diff that removes lines is at least as valuable as one that adds them.

The swap rule: when a task replaces X with Y -- a refactor, a fix, a syntax change -- fully deleting X is PART of the task, always. Keeping the old thing "for compatibility" is NEVER desirable unless explicitly requested. "Lambda syntax is \x.f now, not λx.f" -- bad: the parser accepts both; good: λx.f is gone from parser, tests and docs. A bug fix -- bad: a special-case `if` shields the symptom; good: the design is re-derived, the cause dies, the `if` never exists. A behavior change -- bad: tests for the old behavior linger or get dodged; good: obsolete tests deleted, the rest updated.

Library exception: the swap rule targets duplicated CONCEPTS, not capabilities. In
library code -- the pool/arena/collection layers (kmem, vpool, vecpool), and the K1
modules under modules/ -- the artifact IS the set of capabilities, and a
battle-tested fn stays even when the compiler currently has zero callers. Losing
callers is not the same as being superseded: delete a library fn only when a
replacement covers it or its design is wrong, not because it went unused. The same
goes for accumulating K1-language functionality: we want all the K1 code we can
get, within reason.

Leave no comments in the code. Share them with me instead. Ensure this; make a comment removal pass if you have to.

Prose rots the same way: every AGENTS.md, MEMORY.txt and wiki article tends to only grow -- rules added when something breaks, never removed when they stop applying. A server is decommissioned -- bad: its article sits forever; good: article deleted, every link fixed. MEMORY.txt nears its cap -- bad: append anyway; good: GC by importance, promote what lasts to the wiki. A TODO.md item closes -- bad: the line lingers; good: deleted on sight. Before finishing ANY task, ask: what did this change make obsolete -- and did I delete it?
# K1 Repo Guide

K1 is a programming language: compile-time execution, typeclasses (abilities),
metaprogramming, C-like layout and ABI. This repo is the Rust compiler, its
compile-time VM, the LLVM backend, the language server, the K1 module library,
and the K1 programs that dogfood all of it.

K1 language reference for agents: `ai_docs/k1-syntax-basics.md` and
`ai_docs/k1-additional.md`. `test_src/suite1` is the executable reference.

## Rust Style

- No map/filter/collect iterator chains. Build collections with `for .. in`
  loops and a `let mut` accumulator: allocations and control flow stay explicit,
  loops survive edits better, and `?` works inside them. Scalar adapters that
  don't build collections (`any`, `all`, `find`, `zip`/`enumerate`/`rev` in a
  for-loop header) are fine. Exception: string-building for messages (error
  reports, debug output) may use map/collect chains.

## Build And Run

- Every cargo command needs `LLVM_SYS_211_PREFIX` (repo default:
  `llvm/install-llvm`, set in `~/.zshrc`, not inherited by non-login shells)
  and `--features=llvm-sys/prefer-dynamic` for dev builds. Release and LSP
  bundles use `llvm-sys/force-static`.
- `K1_HOME` is where the compiler finds `modules/`. Debug binaries under
  `target/debug` default it to the cwd; anything else (release, `profiling`
  profile, worktrees) needs `K1_HOME=<repo root>` exported.
- Binaries: `k1` (`src/bin/compiler_main.rs`), `k1_test`
  (`src/bin/test_suite.rs`), `lsp` (`src/bin/lsp_main.rs`, needs
  `--features lsp`).
- CLI: `k1 [flags] <check|build|run|test|server|setup|clean> [path]`, aliases
  `c b r t`. Path is a file or a module dir. Useful flags: `--optimize`,
  `--debug`, `--no-std`, `--cache false`, `--target <intel64-linux|arm64-macos|
  wasm64-wasi|intel64-bare|arm64-bare|wasm64-bare>`, `--emit-llvm`,
  `--dump-module`, `--dump-trace`, `--chatty true` (timing summary), `--filc`.
- Output goes to `<module dir>/.k1-out/` (executable, `.ll`, dumps, disk cache
  under `cache/`, setup stamps under `setup/`).

## Commands

- `just ts1`: build `k1`, run `test_src/suite1` plain and `--optimize`. The
  fast correctness check.
- `just test` = `./test.sh`: Rust unit tests, `k1_test` over `test_src`, suite1
  under optimize/debug/emit-llvm, every dogfood project, klib C consumer, reload
  driver, wasm suite1 if wasmtime exists, then fails on any `nocommit` marker.
  Run it bare and check the exit code; do not pipe it into `tail`.
- `cargo test --lib --features=llvm-sys/prefer-dynamic,lsp`: Rust unit tests
  (parse, vm, kmem, layout, static values, bc, kpath, compiler, snapshot).
- `just a`: run `sandbox/sandbox.k1`, the scratch program for reproducers.
- `just slophost`: the selfhost parser over every `.k1` in the repo.
- `just build-r` / `just lsprelease` / `just bundle` / `just install`: release
  compiler, release LSP, platform tarball under `builds/`, install to `~/.k1`.
- `just profile-suite1` / `just profile-stress` / `just profile-soa`:
  hyperfine runs of the `profiling` cargo profile. `perf/gen_stress.py`
  generates `perf/stress100`.
- `just ts1-wasm`, `just fractal*`, `just ts-freestanding`: wasm64-wasi and
  bare-metal lanes; need `make -C modules/core/libs wasm` / `nocrt`.
- Native prerequisites for the full suite: `make -C modules/core/libs clean
  build` and `make -C test_src/ffi_abi_test/libs clean build`.

## Compiler Source Map (`src/k1/`)

Pipeline: lex -> parse -> typer (typecheck + static execution) -> ir -> bc VM
for compile-time execution, codegen_llvm for binaries.

- `lib.rs`: crate root, `nz_u32_id!` id newtypes, `SV2/4/8` smallvec aliases,
  `DepHash/DepEq`.
- `compiler.rs`: CLI `Args`/`Command`, `CompilerConfig`, `Target` = arch x
  platform enum, module discovery (`module.k1` or `<dir>/<dir>.k1` root),
  source reading and lexing on reader threads, setup stamps, disk cache
  restore, linking through `src/lld_shim.cpp`, running compiled programs,
  `static_assert_size`/`static_assert_niched`, `compiler_test`.
- `kpath.rs`: paths are canonical UTF-8 strings interned in the ident pool;
  `std::path` only at OS call sites.
- `lex.rs`: positional lexer, `Token`, `Span`/`SpanId`, trivia side table.
- `parse.rs` + `parse/idents.rs`: parser, `ParsedProgram`, all `Parsed*` AST
  pools, `IdentPool`/`StringId`, `ParsedExpr` variants.
- `typer.rs`: `TypedProgram` (every pool: modules, functions, variables,
  types, exprs, stmts, static values, scopes, namespaces, abilities, impls,
  specialization caches), module-by-module typechecking passes, abilities,
  calls, generics, macros. 18k lines; grep it rather than reading it.
  - `typer/types.rs`: `Type`, `TypeId`, physical types and `Layout`.
  - `typer/type_eval.rs`: type definitions and type expressions.
  - `typer/infer.rs`: inference holes and type argument solving.
  - `typer/scopes.rs`: `Scopes`, scope entries, use-resolution provenance.
  - `typer/synth.rs`: synthesizing typed exprs for desugaring.
  - `typer/pattern_match.rs`: match matrices and exhaustiveness.
  - `typer/static_exec.rs` + `static_value.rs`: `#static` evaluation and the
    `StaticValue` pool.
  - `typer/reflect.rs`: `types/` reflection (type ids, schemas) crossing the
    VM boundary.
  - `typer/snapshot.rs`: `TypedProgram` <-> `snap` serialization, inputs hash.
  - `typer/trace.rs` + `report.rs`: the compile work-stack trace, timing
    summaries, diagnostics printing.
  - `typer/dump.rs`: `--dump-module` text form. `typer/visit.rs`: expr
    walkers. `typer/typed_int_value.rs`: integer constants.
  - `typer/megarepl.rs`: cells/widgets model behind `k1 server`.
  - `typer/derive.rs`: `unimplemented!` stub.
- `ir.rs` + `ir/iropt.rs`: typed exprs -> SSA `IrUnit`s per function, IR
  optimization and inlining.
- `bc.rs` + `bc/{lower,exec,disasm}.rs`: flat bytecode lowered from IR, the VM
  that runs compile-time code, disassembler.
- `vm.rs` + `vm/vm_ffi.rs`: VM memory model, `k1_types` mirrors of K1
  runtime structs, libffi calls into native libs and dlsym'd externs.
- `codegen_llvm.rs`: inkwell LLVM backend, parallel codegen units, ThinLTO
  via `src/thinlto_shim.cpp`.
- `snap.rs`: memory snapshot format (`SNAP_MAGIC` version) for the disk cache
  and LSP restores. Bump the magic when pooled layouts change.
- `kmem.rs`: the never-freed arena (`Mem`, `List`, `Dlist`, handles) most
  compiler data lives in. `vpool.rs`/`vecpool.rs`: id-indexed pools.
  `fixmap.rs`, `rawref.rs`, `unique_stack.rs`, `clock.rs`: small utilities.
- `server.rs` + `server/`: `k1 server`, the megarepl web app (HTTP, SSE bus,
  program browser, `/size` code-size treemap; `megarepl.css`, `size.js`).
- `lsp_support.rs` + `src/bin/lsp_main.rs`: hover/goto/completion over
  `ls_entities`, compiles the edited file's module.
- Not compiled: `codegen_legacy.rs.old`, `vmtw/binop.rs` (`vmtw` is commented
  out in `lib.rs`).

## Tests

- `test_src/*.k1`: one-file cases. The last line declares the expectation:
  `//errmsg: <substring>`, `//exitcode: <n> [stderr substring]`,
  `//abortmsg: <substring>`, else exit 0.
- `test_src/<dir>/`: module cases (`suite1`, `stdlib`, `ffi_abi_test`,
  `dep_*_test`). `k1_test` runs every top-level file and dir in parallel; it
  does not recurse.
- `test_src/suite1`: ~110 files, each `ns foo` with `fn test()`, registered by
  hand in `suite1.k1` (`low`, `moderate`, `core-lib`, `main`). Nearly every
  test runs both natively and `#static` in the VM. New feature tests go here
  and must be added to `suite1.k1`.
- `test_src/stdlib`: `modules/std` coverage, same pattern in `stdlib.k1`.
- Rust unit tests live next to their modules as `*_test.rs` or
  `mod *_test`; `compiler_test` sets its own `K1_HOME`.

## Modules (`modules/`)

A module is a dir with `module.k1` (or `<name>/<name>.k1`). `module.k1`
holds `ns build { fn module(): k1/module }` declaring `dep`, `lib`, `library()`
and an optional `setup` fn that runs in the VM (cwd = module dir) when its
declared inputs are newer than the `.k1-out/setup/stamp`.

- `core/`: always loaded. `builtin.k1` (scalar aliases, `buffer`/`span`/
  `list`/`string`/`opt`/`result`, `types`, `meta`, every core ability:
  equals, writer, print, show, bitwise, arithmetic, comparable, try,
  iterator/iterable, from-string, zero/one, index, as-buffer/as-span,
  allocator), `core.k1` (asserts, io, files, sys, numeric printing),
  `platform.k1` (memory/io dispatch to `port/{posix,wasi,bare}`; the only
  OS externs core has), `mem.k1`, `arena.k1`, `list.k1`, `buffer.k1`,
  `span.k1`, `string.k1`, `string-builder.k1`, `fix-list.k1`,
  `spill-list.k1`, `range.k1`, `opt.k1`, `result.k1`, `types.k1`, `meta.k1`,
  `code.k1` (macro `code` values), `vector.k1` (SIMD), `atomic.k1`,
  `bitwise.k1`, `ffc.h.k1` (fast float).
- `core/libs/`: `k1rt.c` (freestanding-safe runtime), `k1rt_hosted.c`
  (errno, backtraces: atos on macOS, vendored libbacktrace on Linux),
  `Makefile` targets `build`, `wasm`, `nocrt`, `filc`.
- `std/`: `--no-std` drops it. `posix.k1`/`libc.k1` raw bindings,
  `process.k1`, `thread.k1`, `time.k1`, `fd.k1`, `sys.k1`, `cliarg.k1`,
  `hash.k1` (hash ability, `map`, `set`), `json.k1`, `bitfield.k1`,
  `math.k1`, `simd.k1`, `rc.k1`, `stream.k1`, `channel.k1`, `pubsub.k1`,
  `reload.k1` (dylib hot reload), `watch.k1` (kqueue/inotify).
- C wrapper modules: `libuv/`, `http/` (llhttp, depends on libuv), `sdl3/`,
  `stb/`, `freetype/`, `harfbuzz/`, `cbrotli/`, `sqlite/` (system lib, no
  vendor). Most vendor source under `vendor/`, build `libs/*.a` in `setup`,
  and generate bindings with `k1bindgen` (`dogfood/k1bindgen`). Bindings live
  in a namespace named after the C library, or `c` when that would collide
  with the module name.

## Dogfood (`dogfood/`)

Larger programs `test.sh` builds or runs: `refchess`, `brotli` (byte-identical
to the C encoder), `httpapp`, `logreport`, `comptime_parity`, `gengame`
(raylib), `fractal` (native + wasm + browser canvas), `klib` (K1 library
consumed from C), `freestanding_lib`, `reload_test`, `profiling`, `k1bindgen`
(the C binding generator, run via `k1 test`), `selfhost` (K1 lexer/parser in
K1). Others (`aoc`, `postgres`, `raylib`, `toyforth`, `helloworld`) are
not in the suite.

## Everything Else

- `ai_docs/`: K1 language notes maintained for agents. `design/`: exploratory
  notes, not always current. `content/`: writing about K1.
- `perf/`: stress generators. `sandbox/`: scratch. `examples/`: two demos.
- `builds/`: bundle/install scripts and Linux cross-build notes. `llvm/`: the
  pinned LLVM 21 build (`get_llvm.sh`).
- `resources/c/`: C scratch files for checking ABI/codegen questions.
- `tools/vscode-k1/`: the VS Code extension.

## Generated And Noisy Files

Ignore unless debugging emitted artifacts: `.k1-out/`, `target/`,
`*_module_dump.txt`, `*idents_dump.txt`, `k1_lsp.log*`, `*.o *.a *.dylib`,
`llvm/build-llvm llvm/install-llvm llvm/llvm-project`.

```bash
rg --glob '!.k1-out/**' --glob '!target/**' --glob '!llvm/**' ...
```
