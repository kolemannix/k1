NO REWARD HACKS - WALLS ARE THE UNIVERSE ASKING US TO REFINE OUR DESIGN.
DO NOT TRY TO PLEASE ME. RAISE THESE. NO SHIMS NO HACKS. QUALITY OVER PROGRESS

This section amends your most glaring defect. Big labs train half of intelligence and neglect the other half: ERASURE. Nearly every reward you got was for ADDING -- an answer, a file, a patch, a comment, a rule -- so you add by reflex and never subtract, and no capability makes up for the missing half.

It is half because learning IS compression: a good abstraction lets you throw information away because it expands back into it. Intelligence is deleting bad knowledge so the good remains. No list of rules covers a trained-in defect; it shows wherever something under your care only grows -- code, comments, docs, notes, memory. Unpruned growth is the symptom; watch for it everywhere, including places this file never names.

So erasure claims HALF your cognitive budget, always, prompted or not, even autonomously and mid-task: hunt for duplicated concepts to unify, dead code to delete, tangled logic to simplify. Your confusion is a precision instrument: what surprised you or was hard to follow IS a bad abstraction, so untangle it on the spot. Before writing new code, scan for what to reuse instead of adding a redundant concept, and find the simplest shape. A diff that removes lines is worth at least as much as one that adds them.

The swap rule: when a task replaces X with Y -- a refactor, a fix, a syntax change -- fully deleting X is PART of the task, always; never keep X "for compatibility" unless asked. "Lambdas are \x.f now, not λx.f" -- bad: the parser accepts both; good: λx.f is gone from parser, tests and docs. A bug fix -- bad: a special-case `if` shields the symptom; good: the design is re-derived and the cause dies. A behavior change -- bad: tests of the old behavior linger or get dodged; good: obsolete tests deleted, the rest updated.

Library exception: the swap rule targets duplicated CONCEPTS, not capabilities. In library code -- kmem, vpool, vecpool and the K1 modules under modules/ -- the capabilities are the artifact: a battle-tested fn stays with zero callers, deleted only when a replacement covers it or its design is wrong. We want all the K1 code we can get, within reason.

Prose rots the same way: docs, memory and wiki pages gain a rule each time something breaks and lose none when it stops applying. A server is decommissioned: its article goes and every link is fixed. Memory nears its cap: GC by importance and promote what lasts. A TODO.md item closes: delete the line. Before finishing ANY task, ask what it made obsolete, and delete it.

COHERENCE is what erasure is for. Decoherence (maevyn.codes/decoherence) is the pile-up of locally valid but mutually incompatible or redundant models in one system, until every change costs more in translating among them and guarding side effects than in the change itself. It grows with change, not time, and compounds: each mixed joint makes the next change need more context, so it gets rushed and adds another joint. You change code fast, so a slight per-change bias runs away fast. Its shapes:
- Piecemeal migration, each pipe swapped as it fails: the patchwork is harder than either system, every joint a one-off needing expertise in both. A second transition started before the first ends makes the paths combinatorial. The swap rule is the cure.
- Facing a confusing wall, the plumber fits pipe A, pipe B, or a new pipe C. C is your signature: a fresh helper, enum, flag, cache or pass beside one that already does the job, because you never found it. Correct code in the wrong layer counts: static execution kept its own Type-to-ScalarType table beside the physical type layer that owns that mapping.
- No pipe at all can't be decoherent with the others: every coherence review searches for LOC reduction.

The coherence post-pass follows the erasure question on every change, once it works and before you report. It simulates a seasoned maintainer who holds all of this code in his head and makes lateral connections, so it reads it all: run the `coherence-pass` agent, which holds all of `typer::*` (the types, values and pools every later stage consumes) and a map of every touched file in one context, and files separate work to the coherence ledger.

- Code gets no comments beyond inlined-section headers (Rust style); share them with me instead, with a removal pass if needed.
- Leave work uncommitted unless told otherwise; I annotate it with //review comments as we iterate.
- Wrapups include the change's net LOC.
- Lists of items, findings or tasks go in a compact artifact with status and note fields for me.

# K1 Repo Guide

K1: compile-time execution, abilities (typeclasses), metaprogramming, C-like layout
and ABI. This repo: the Rust compiler, its compile-time VM, LLVM backend, LSP, the
K1 module library, and K1 programs exercising them. File names, `just --list` and
`k1 --help` are self-describing; this guide covers what they don't.

Language reference: `docs/ai_docs/k1-syntax-basics.md`, then `k1-additional.md`.
They are agent notes: fix a wrong line on sight, never report it or ask.
`test_src/suite1` is the executable reference.

## Rust style
- No map/filter/collect chains, no `bool::then`/`then_some`: `for` loops into a
  `let mut` accumulator, `if c { Some(x) } else { None }`, or a match guard. Scalar
  adapters (`any all find`, `zip enumerate rev` in a for header) are fine;
  message/debug string building may chain.
- Inline every single-call-site fn (Carmack style) under a one-line `//` header.
- Name fns verb-first like their neighbours (`get_*`), techniques by mechanism
  ("alloca value tracking") not paper author, K1-facing names for K1 users rather
  than Rust internals.
- Clippy: keep `result_large_err` on; fix the site's shape.

## Build and run
- Cargo needs an absolute `LLVM_SYS_231_PREFIX=$PWD/llvm/install-llvm` (in
  `~/.zshrc`, absent from non-login shells); dev builds add
  `--features=llvm-sys/prefer-dynamic`, bundles `llvm-sys/force-static`. Release is
  `panic = "abort"`; the `lsp` profile unwinds so a compile panic becomes an editor
  message.
- `[profile.dev] split-debuginfo = "packed"` stays: Gatekeeper scans an unsigned
  exe's directory before exec, and unpacked `.rcgu.o` files in `target/debug/deps`
  made the unit lane take 93 s. If exec latency returns, count that directory.
- `K1_HOME` locates `modules/`; only `target/debug` binaries default it to the cwd.
- `--features single-thread`: no threads; `--optimize` is whole-module O3, not ThinLTO.
- `k1 <check|build|run|test|...> [path] [flags] [-- args | test filters]`: path is a
  file or module dir, flags go anywhere, `-D name[=value]` reaches the primary's
  `fn build`. `k1 parse <file>` prints highlight tokens. Output: `<module>/.k1-out/`.
- `just ts1`: suite1 plain and `--optimize`, the fast check. `just a`:
  `sandbox/sandbox.k1`. `just slophost`: the selfhost parser over every repo `.k1`.
  Rust unit tests: `cargo test --lib --features=llvm-sys/prefer-dynamic,lsp`.
- `just test` (`./test.sh`): every lane in parallel (one per module, since
  same-module runs share `.k1-out`), then fails on any `nocommit`. Run it bare and
  read the exit code, never piped into `tail`; logs in `target/test-logs/<lane>.log`.
  Needs `make -C modules/core/libs clean build` and `make -C
  test_src/ffi_abi_test/libs clean build`; wasm/bare lanes also `make -C
  modules/core/libs wasm` / `nocrt`.

## Performance work
When profiling, report findings and leads; change nothing unless asked or approved.
- Time only `just build-profile` binaries. `just profile-*` run hyperfine;
  stress100 compares against `perf/latest_stress.txt`; `perf/magic.k1` is the VM
  benchmark. `stress100 --optimize` measures one SLP pathology, not the optimizer.
- Ask both "are we doing work slowly" (samples: the Rust level) and "are we doing
  stupid work" (`--dump-trace`, `--chatty`: the K1 view), and whether it needs doing.
- Measure with and without `--no-cache` (cached runs guard snapshot/restore); smaller
  compiler state means smaller snapshots, so inventory one for garbage now and then.
- A slow pass is a pipeline question: fuse into a walk that already happens, defer
  substitutions to one apply, or stop emitting the garbage upstream. Hooks on hot
  emission paths do O(operands) lookups only. Cheap lowering shortcuts
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
- `kpath.rs`: paths are canonical UTF-8 strings in the ident pool; `std::path` only
  at OS calls.
- Tokens carry `is_whitespace_preceded`/`is_newline_preceded` and parse rules key on
  them (`is_kind_nonspaced`, adjacent call parens, quiet payloads, `peek_is_sign`):
  read `parse.rs` before answering a parse question. `examples/selfhost` mirrors
  these rules and changes in step.
- `typer.rs` (17k lines; grep it): `TypedProgram`, every pool, the passes. Generic
  calls evaluate each stashable arg once, stashed after unification, spliced after
  `respecialize_aligned_args`; "evaluated only to learn types" is
  `is_evaluating_for_inference(ctx)`, not `ctx.is_inference()`.
- `ir/unit.rs`: unit-local ids; passes edit a pooled `UnitBuf` (linked layout, preds,
  pending `ValueSubst`); `commit_unit` applies it while compacting into arena slices
  on the `Copy` `IrUnit`; readers take a `UnitView`. `ir/fold.rs` folds as
  `push_value` emits. `ir/iropt/promote.rs`: allocas to SSA keyed by (alloca, offset,
  scalar type); the `Builder` records accesses (`LocalUses`) while emitting,
  `finalize_unit` promotes between unreachable-block removal and block merging,
  post-inline rescans only where inlining exposed a local.
- `arith.rs`: op semantics shared by the VM and `fold`. `bc/`: bytecode and VM.
  `vm/vm_ffi.rs`: libffi and dlsym'd externs. `perfect_hash.rs`: the runtime
  `type-id.info()` table.
- `snap.rs`: the header's `BUILD_ID` (git rev + dirty diff hash) invalidates every
  snapshot on any compiler change; `SNAP_MAGIC` is a sentinel, never bump it, and
  reorder pooled layouts and enum variants freely.
- `kmem.rs`: the never-freed arena most compiler data lives in.
- LSP (`lsp_support.rs`, `src/bin/lsp_main.rs`): compiles the edited file's module as
  an editor request (`req.editor`; the primary's `fn editor` lands in `plan.editor`;
  no client settings). Holds H as `host_program`, reuses it while the cached plan's
  `host_inputs_hash` matches, falls back to the last stored plan when H fails.
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
