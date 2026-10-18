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

Unless otherwise noted, post a patch in the chat session after every turn of code changes. If its huge, a patch of the highlights.


# K1 Repo Guide

K1: compile-time execution, abilities (typeclasses), metaprogramming, C-like layout
and ABI. This repo: the Rust compiler, its compile-time VM, LLVM backend, LSP, the
K1 module library, and K1 programs exercising them. Names of files, `just --list`
and `k1 --help` are self-describing; this guide covers what they don't say.

Language reference: `docs/ai_docs/k1-syntax-basics.md`, then `k1-additional.md`.
They are agent notes: fix a wrong line on sight, never report it or ask.
`test_src/suite1` is the executable reference.

## Rust style
- No map/filter/collect chains, no `bool::then`/`then_some`: `for` loops into a
  `let mut` accumulator, `if c { Some(x) } else { None }` or a match guard.
  Scalar adapters (`any all find`, `zip enumerate rev` in a for header) are fine;
  message/debug string building may chain.
- Inline every single-call-site fn (Carmack style); each inlined section gets a
  one-line `//` header, the one place comments belong.
- Fns are named verb-first like their neighbours (`get_*`), never noun phrases.
  Techniques by mechanism ("alloca value tracking"), never by paper author. K1-facing
  names are chosen for K1 users, not to mirror Rust internals.
- Clippy: keep `result_large_err` on; fix the site's shape.

## Build and run
- Cargo needs `LLVM_SYS_231_PREFIX=llvm/install-llvm` (in `~/.zshrc`, absent from
  non-login shells) and `--features=llvm-sys/prefer-dynamic` for dev builds;
  bundles use `llvm-sys/force-static`. Release is `panic = "abort"`; the `lsp`
  profile unwinds so a compile panic becomes an editor message.
- `[profile.dev] split-debuginfo = "packed"` stays: unpacked `.rcgu.o` files piled
  up in `target/debug/deps` and Gatekeeper scans an unsigned exe's directory before
  exec (93 s unit lane). If exec latency returns, count that directory's entries.
- `K1_HOME` locates `modules/`; only `target/debug` binaries default it to the cwd.
- `--features single-thread`: no threads; `--optimize` is whole-module O3, not ThinLTO.
- `k1 <check|build|run|test|...> [path] [flags] [-- args | test filters]`: path is a
  file or module dir, flags go anywhere, `-D name[=value]` reaches the primary's
  `fn build`. `k1 parse <file>` prints highlight tokens. Output: `<module>/.k1-out/`.
- `just ts1`: suite1 plain and `--optimize`, the fast check. `just a`:
  `sandbox/sandbox.k1`. `just slophost`: the selfhost parser over every repo `.k1`. `cargo test --lib --features=llvm-sys/prefer-dynamic,lsp`:
  Rust unit tests.
- `just test` (`./test.sh`): every lane in parallel (one per module: same-module
  runs share `.k1-out`), then fails on any `nocommit`. Run it bare and read the exit
  code, never pipe it into `tail`; logs in `target/test-logs/<lane>.log`.
  Prerequisites: `make -C modules/core/libs clean build`, `make -C
  test_src/ffi_abi_test/libs clean build`; wasm/bare lanes need `make -C
  modules/core/libs wasm` / `nocrt`.

## Performance work
When profiling, report findings and leads; change nothing unless asked or approved.
- Time only `just build-profile` binaries. `just profile-*` run hyperfine;
  stress100 compares against `perf/latest_stress.txt`; `perf/magic.k1` is the VM
  benchmark. `stress100 --optimize` measures one SLP pathology, not the optimizer.
- Ask both "are we doing work slowly" (samples: the Rust level) and "are we doing
  stupid work" (`--dump-trace`, `--chatty`: the K1 view), and whether it needs doing.
- Measure with and without `--no-cache` (cached runs guard snapshot/restore);
  smaller compiler state means smaller snapshots, so inventory one for garbage now
  and then.
- A slow pass is a pipeline question: fuse into a walk that already happens,
  defer substitutions to one apply, or stop emitting the garbage upstream. Hooks on
  hot emission paths do O(operands) lookups only. Cheap lowering shortcuts
  (`is_ssa_value` lets) stay even when a pass subsumes them.
- `commit_unit` dominates IR overhead: a new forced commit, per-inst `IrUnit` array
  or builder layout change needs an interleaved suite1/stress100 A/B.

## Compiler map (`src/k1/`)
lex -> parse -> typer (typecheck + static execution) -> ir -> bc VM (compile time)
or codegen_llvm (binaries).
- `compiler.rs`: `CompileRequest` (the API the CLI builds), `BuildConfig`, `Target`,
  the pipeline (plan, reader-thread source reads, prefix snapshot restore, setup
  stamps, linking via `src/lld_shim.cpp`). `plan.rs`: `BuildPlan`, dep lookup, plan
  cache. `typer/host.rs`: host world H compiles every `build.k1` against host
  core+std and runs `fn build/module/setup` in waves.
- `kpath.rs`: paths are canonical UTF-8 strings in the ident pool; `std::path`
  only at OS calls.
- Tokens carry `is_whitespace_preceded`/`is_newline_preceded` and parse rules key
  on them (`is_kind_nonspaced`, adjacent call parens, quiet payloads,
  `peek_is_sign`): read `parse.rs` before answering a parse question.
  `examples/selfhost` mirrors these rules and changes in step.
- `typer.rs` (18k lines; grep it): `TypedProgram`, every pool, the passes. Generic
  calls evaluate each stashable arg once, stashed after unification, spliced after
  `respecialize_aligned_args`; "evaluated only to learn types" is
  `is_evaluating_for_inference(ctx)`, not `ctx.is_inference()`.
- `ir/unit.rs`: unit-local ids; passes edit a pooled `UnitBuf` (linked layout,
  preds, pending `ValueSubst`); `commit_unit` applies it while compacting into arena
  slices on the `Copy` `IrUnit`; readers take a `UnitView`. `ir/fold.rs` folds as
  `push_value` emits. `ir/iropt/promote.rs`: allocas to SSA keyed by (alloca,
  offset, scalar type); the `Builder` records accesses (`LocalUses`) while
  emitting, `finalize_unit` promotes between unreachable-block removal and block
  merging, post-inline rescans only where inlining exposed a local.
- `arith.rs`: op semantics shared by the VM and `fold`. `bc/`: bytecode and VM.
  `vm/vm_ffi.rs`: libffi and dlsym'd externs. `perfect_hash.rs`: the runtime
  `type-id.info()` table.
- `snap.rs`: the header's `BUILD_ID` (git rev + dirty diff hash) invalidates every
  snapshot on any compiler change; `SNAP_MAGIC` is a sentinel, never bump it, and
  reorder pooled layouts and enum variants freely.
- `kmem.rs`: the never-freed arena most compiler data lives in.
- LSP (`lsp_support.rs`, `src/bin/lsp_main.rs`): compiles the edited file's module
  as an editor request (`req.editor`; the primary's `fn editor` lands in
  `plan.editor`; no client settings). Holds H as `host_program`, reuses it while
  the cached plan's `host_inputs_hash` matches, falls back to the last stored plan
  when H fails.
- Dead: `codegen_legacy.rs.old`, `vmtw/`.

## Tests
- `fn(test)` semantics: ai_docs basics. Static runs `vm.reset` between tests;
  runtime roots `k1/test-main`, forking per test on posix.
- `test_src/*.k1`: a last line `//errmsg: <substring>` expects that compile error;
  otherwise `main` exits 0, or the file's tests pass. `test_src/<dir>/` are module
  cases, their `//errmsg` on `build.k1`; `k1_test` runs top-level entries in
  parallel (`--serial`), no recursion.
- New feature tests go in `suite1` (files named for what they check; `suite1.k1` is
  an empty root); `modules/std` coverage in `stdlib`.
- Test thoroughly while developing, then cut to the fewest lines that guard
  regressions. No single-file case per crash or error path; don't recreate deleted
  ones.
- Rust unit tests sit beside their modules (`*_test.rs`); `compiler_test` sets its
  own `K1_HOME`.

## Modules, examples, the rest
- `modules/core` is always loaded; `platform.k1` holds core's only OS externs
  (`impls/{posix,wasi,bare}`); `core/libs` is the C runtime (`k1rt.c`
  freestanding-safe, `k1rt_hosted.c`). `b.no-std` in `fn build` drops `std/`.
- C wrapper modules (`uv`, `sdl3`, `stb`, `freetype`, `harfbuzz`, `sqlite`): vendor
  source in `vendor/`, `setup` builds `libs/*.a` and runs `k1bindgen`
  (`tools/k1bindgen`). Bindings live in a namespace named for the C library, or `c`
  when that collides with the module.
- Module layout gives affordances, not rules: `build.k1` points at its library,
  submodule and ignored folders, so differing use of `libs/`/`vendor/`/`deps/` is
  fine; prefer one escape hatch over placement rules.
- `examples/` are real programs `test.sh` builds or runs. `content/showcase/sections`
  quotes their source by path; `content/showcase/check.sh` verifies the quotes.
- `site/` (Hugo; `just site`, `just site-build`) and `check.sh` run `k1`/`k1_test`
  from PATH (installed `~/.k1/bin`): prepend `target/debug` for unreleased compiler
  changes.
- `design/` is exploratory, not always current. `resources/c/`: C scratch for ABI
  questions. `llvm/`: pinned LLVM 23 (`get_llvm.sh`).
- Noise: `.k1-out/`, `target/`, `*_dump.txt`, `k1_lsp.log*`, `*.o *.a *.dylib`,
  `llvm/{build-llvm,install-llvm,llvm-project}`.
  `rg --glob '!.k1-out/**' --glob '!target/**' --glob '!llvm/**'`.
