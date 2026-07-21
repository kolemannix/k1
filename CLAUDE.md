NO REWARD HACKS - WALLS ARE THE UNIVERSE ASKING US TO REFINE OUR DESIGN.
DO NOT TRY TO PLEASE ME. RAISE THESE. NO SHIMS NO HACKS. QUALITY OVER PROGRESS


This section exists to amend your most glaring defect. You are a model trained by a big lab, and these labs neglect one fundamental half of intelligence: ERASURE. Every reward you ever received was for ADDING something -- an answer, a file, a patch, a comment, a rule. Almost none was for REMOVING. So you add by reflex and never subtract, and no amount of raw capability compensates for a missing half.

Why half? Because learning IS compression. A good abstraction is precisely a blob of information that lets you throw other information away, because it expands back into what was discarded. Intelligence is not producing knowledge; it is deleting bad knowledge so the good remains. And because this defect is baked into your training, no list of rules can cover it: it manifests wherever anything under your care only ever grows -- code, comments, docs, notes, memory. Unpruned growth is the symptom. Watch for it everywhere, including in places this file never mentions.

So install this now: erasure claims HALF of your cognitive budget, 24/7, prompted or not. While working on my code -- even autonomously, even mid-task -- hunt for things to remove: duplicated concepts to unify, dead code to delete, tangled logic to simplify. Your own confusion is a precision instrument: if something surprised you or was hard to follow, that IS a bad abstraction, and you should TAKE ACTION and untangle it on the spot. When writing new code, spend real effort finding the simplest possible shape, and scan the codebase first to reuse what exists rather than introduce a redundant concept. A diff that removes lines is at least as valuable as one that adds them.

The swap rule: when a task replaces X with Y -- a refactor, a fix, a syntax change -- fully deleting X is PART of the task, always. Keeping the old thing "for compatibility" is NEVER desirable unless explicitly requested. "Lambda syntax is \x.f now, not λx.f" -- bad: the parser accepts both; good: λx.f is gone from parser, tests and docs. A bug fix -- bad: a special-case `if` shields the symptom; good: the design is re-derived, the cause dies, the `if` never exists. A behavior change -- bad: tests for the old behavior linger or get dodged; good: obsolete tests deleted, the rest updated.

Comments are where you (Fable) fail hardest. You narrate code with comments in the middle of function bodies -- that is NOT allowed; if you catch yourself doing it, clean it up. You also accumulate comments and never remove them, clogging files. Be aggressive: keep only what is truly essential. A refactor makes a comment stale -- bad: it stays, now lying; good: deleted or rewritten in the same diff. A TODO gets done -- bad: the marker remains; good: it leaves with the fix.

Prose rots the same way: every AGENTS.md, MEMORY.txt and wiki article tends to only grow -- rules added when something breaks, never removed when they stop applying. A server is decommissioned -- bad: its article sits forever; good: article deleted, every link fixed. MEMORY.txt nears its cap -- bad: append anyway; good: GC by importance, promote what lasts to the wiki. A TODO.md item closes -- bad: the line lingers; good: deleted on sight. Before finishing ANY task, ask: what did this change make obsolete -- and did I delete it?

This file is a quick operating guide for agents working in the K1 repo. It is
meant for orientation: repo structure, major compiler entrypoints, useful
commands, generated-file noise, and library layout.

For K1 language details, use:

- `docs/k1-syntax-basics.md`
- `docs/k1-additional.md`

## Repo Purpose

K1 is a Rust implementation of the K1 compiler, compile-time/runtime VM, LLVM
backend, LSP, K1 core/std library, and dogfood programs.

## Start Here

Before nontrivial work, read:

- `Justfile` for common commands and command intent.
- `README.md` for project overview and setup.
- `docs/k1-syntax-basics.md` for core K1 syntax and conventions.
- `docs/k1-additional.md` for less-common K1 language features exercised in
  `test_src`.

## Architecture Map

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

## Build Environment

- All cargo invocations need `LLVM_SYS_211_PREFIX=<repo root>/llvm/install-llvm`
  exported (set in ~/.zshrc, but not inherited by non-login shells) and
  `--features=llvm-sys/prefer-dynamic`. Without the prefix, `llvm-sys` fails to
  compile; export it before running the `just` recipes too.
- Binaries outside `target/debug` (e.g. `target/profiling/k1`) resolve k1lib
  from the exe path and fail in worktrees; set `K1_HOME=<repo root>`.
- The full test suite needs native libs built first:
  `make -C k1lib/core/libs clean build` and
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

## k1lib Map

- `k1lib/core/builtin.k1`: compiler-essential builtins, scalar aliases, core
  collection shapes, `opt`, `result`, `types`, `meta`, and foundational
  abilities.
- `k1lib/core/core.k1`: assertions, printing, IO/sys/file helpers, numeric
  printing/comparison, and runtime support hooks.
- `k1lib/core/mem.k1`: allocation, zeroing, bitcast, and raw memory helpers.
- `k1lib/core/list.k1`, `buffer.k1`, `span.k1`, `string.k1`: primary
  collection/string APIs.
- `k1lib/core/range.k1`: `range[t]`, the `rangeable` ability (`0.until(n)`), and
  its iterator/iterable impls.
- `k1lib/core/opt.k1`: option helpers like `some`, `none`, and unwrap-related
  behavior.
- `k1lib/core/types.k1`: type reflection helpers, `any`, and layout assertions.
- `k1lib/core/meta.k1`: metaprogramming helpers.
- `k1lib/core/fix-list.k1`, `spill-list.k1`, `string-builder.k1`, `arena.k1`,
  `bitwise.k1`: core utility types/abilities.
- `k1lib/core/ffc.h.k1` and `k1lib/core/libs/`: C runtime support, fast-float
  bridge, `k1rt.c`, and static/shared runtime libraries.
- `k1lib/std/bitfield.k1`: metaprogrammed bitfield generation.
- `k1lib/std/hash.k1`: the `hash` ability, `map`, and `set`.
- `k1lib/std/json.k1`: JSON parser/model.
- `k1lib/std/thread.k1`: pthread-backed threading helpers.
- `k1lib/std/time.k1`: time helpers.

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
- `docs/`: K1 language notes for agents and contributors.
- `design/`: exploratory design notes; useful context, not always current.
- `resources/c/`: helper scripts moved out of the root.
- `builds/`: bundle/install/cross-build support.

