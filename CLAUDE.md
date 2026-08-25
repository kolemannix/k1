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

This file is a quick operating guide for agents working in the K1 repo. It is
meant for orientation: repo structure, major compiler entrypoints, useful
commands, generated-file noise, and library layout.

For K1 language details, use:

- `ai_docs/k1-syntax-basics.md`
- `ai_docs/k1-additional.md`

## Repo Purpose

K1 is a Rust implementation of the K1 compiler, compile-time/runtime VM, LLVM
backend, LSP, K1 core/std library, and dogfood programs.

## Start Here

Before nontrivial work, read:

- `Justfile` for common commands and command intent.
- `README.md` for project overview and setup.
- `ai_docs/k1-syntax-basics.md` for core K1 syntax and conventions.
- `ai_docs/k1-additional.md` for less-common K1 language features exercised in
  `test_src`.

## Architecture Map

- `src/k1/kpath.rs`: path handling; compiler-internal paths are canonical UTF-8
  strings interned in the ident pool, `std::path` only at OS call sites.
- `src/k1/lex.rs`: lexer/tokenization.
- `src/k1/parse.rs` and `src/k1/parse/idents.rs`: parser, AST, identifiers, and
  parsed source model.
- `src/k1/typer.rs`: main typechecker, abilities, inference orchestration, and
  compile-time/meta hooks.
- `src/k1/typer/types.rs`: type representation, schemas, and layout-facing type
  data.
- `src/k1/typer/infer.rs`: inference support.
- `src/k1/typer/synth.rs`: synthesized typed expressions.
- `src/k1/typer/dump.rs`: typed-program display/debug output.
- `src/k1/ir.rs` and `src/k1/ir/iropt.rs`: typed expression to IR lowering and
  IR optimization.
- `src/k1/vm.rs` and `src/k1/vm/vm_ffi.rs`: VM/static execution and VM FFI.
- `src/k1/codegen_llvm.rs`: LLVM backend.
- `src/k1/compiler.rs`: CLI command plumbing, module compilation, output paths,
  and build/run/test orchestration.
- `src/bin/compiler_main.rs`: `k1` CLI.
- `src/bin/test_suite.rs`: K1 regression test runner.
- `src/bin/lsp_main.rs` and `src/k1/lsp_support.rs`: language server.

## Rust Style

- No map/filter/collect iterator chains. Build collections with `for .. in`
  loops and a `let mut` accumulator: allocations and control flow stay explicit,
  loops survive edits better, and `?` works inside them. Scalar adapters that
  don't build collections (`any`, `all`, `find`, `zip`/`enumerate`/`rev` in a
  for-loop header) are fine. Exception: string-building for messages (error
  reports, debug output) may use map/collect chains.

## Build Environment

- All cargo invocations need `LLVM_SYS_211_PREFIX=<repo root>/llvm/install-llvm`
  exported (set in ~/.zshrc, but not inherited by non-login shells) and
  `--features=llvm-sys/prefer-dynamic`. Without the prefix, `llvm-sys` fails to
  compile; export it before running the `just` recipes too.
- Binaries outside `target/debug` (e.g. `target/profiling/k1`) resolve modules
  from the exe path and fail in worktrees; set `K1_HOME=<repo root>`.
- Run the K1 test suite via `./test.sh` (or `just test`), not `k1_test` by hand.
  If you must invoke `target/debug/k1_test` directly, set
  `K1_HOME=<abs repo root>` — without it parallel runs fail nondeterministically
  with "Failed but had no errors".
- The full test suite needs native libs built first:
  `make -C modules/core/libs clean build` and
  `make -C test_src/ffi_abi_test/libs clean build` (`just test` handles this).
- `--chatty true` prints the compiler timing summary.

## Commands

- `just ts1`: fast suite1 language/compiler check.
- `just test`: full suite, including Rust tests, K1 tests, FFI libs, and dogfood
  projects.
- `just a`: run current scratch program under `sandbox/`.
- `cargo test --lib`: Rust unit tests.
- `cargo build --features=llvm-sys/prefer-dynamic --bin k1_test --bin k1`:
  debug compiler/test binaries.
- `just build-k1r`: release compiler.
- `just lsprelease`: release LSP.

## modules Map

- `modules/core/builtin.k1`: compiler-essential builtins, scalar aliases, core
  collection shapes, `opt`, `result`, `types`, `meta`, and foundational
  abilities.
- `modules/core/core.k1`: assertions, printing, IO/sys/file helpers, numeric
  printing/comparison, and runtime support hooks.
- `modules/core/mem.k1`: allocation, zeroing, bitcast, and raw memory helpers.
- `modules/core/list.k1`, `buffer.k1`, `span.k1`, `string.k1`: primary
  collection/string APIs.
- `modules/core/range.k1`: `range[t]`, the `rangeable` ability (`0.until(n)`), and
  its iterator/iterable impls.
- `modules/core/opt.k1`: option helpers like `some`, `none`, and unwrap-related
  behavior.
- `modules/core/types.k1`: type reflection helpers, `any`, and layout assertions.
- `modules/core/meta.k1`: metaprogramming helpers.
- `modules/core/fix-list.k1`, `spill-list.k1`, `string-builder.k1`, `arena.k1`,
  `bitwise.k1`: core utility types/abilities.
- `modules/core/ffc.h.k1` and `modules/core/libs/`: C runtime support, fast-float
  bridge, `k1rt.c` (freestanding-safe) + `k1rt_hosted.c` (errno, backtrace:
  atos on macos, vendored libbacktrace on linux), and static/shared/freestanding
  runtime libraries.
- `modules/libuv/`: libuv wrapper module (vendored release source in vendor/,
  cmake setup-built libs/libuv.a, bindgen-generated uv.k1/net.k1 bindings,
  uv_ext abilities).
- `modules/http/`: http framework module (dep on libuv; vendored llhttp C in
  vendor/, setup-built libs/libllhttp.a; response/routing/sse/task/work).
- `modules/std/posix.k1` and `modules/std/libc.k1`: raw POSIX/libc bindings
  (core is self-contained; its own OS externs live in `core/platform.k1`).
- `modules/std/bitfield.k1`: metaprogrammed bitfield generation.
- `modules/std/hash.k1`: the `hash` ability, `map`, and `set`.
- `modules/std/json.k1`: JSON parser/model.
- `modules/std/thread.k1`: pthread-backed threading helpers.
- `modules/std/time.k1`: time helpers.
- Bindgen output goes in a namespace named after the C library it binds, never
  after its own module: when those would collide it is `c` (`sdl3/c`, `stb/c`),
  leaving the module root for the K1 layer. k1bindgen's `-ns-name` sets it and
  defaults to `-lib-name`, which is what `libuv/uv`, `libuv/net`, and
  `http/llhttp` ride on.

## Generated And Noisy Files

Treat these as generated/noisy:

- `.k1-out/`
- `target/`
- `*_module_dump.txt`
- `k1_lsp.log*`

Avoid searching generated output unless debugging generated code or emitted
artifacts specifically.

For broad searches, prefer:

```bash
rg --glob '!.k1-out/**' --glob '!target/**' ...
```

## Other Reference Areas

- `test_src/`: language regression and feature tests.
- `dogfood/`: larger K1 programs used as integration coverage.
- `ai_docs/`: K1 language notes for agents and contributors.
- `design/`: exploratory design notes; useful context, not always current.
- `resources/c/`: helper scripts moved out of the root.
- `builds/`: bundle/install/cross-build support.

