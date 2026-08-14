# K1 in 2026 — Highlights

Source list for tweets, demo videos, and website content. ★ = strongest
demo/promotion material. 304 commits Jan 1 – Aug 12.

Pace note: January (18 commits) and February (8) were quiet but deep —
foundations like ZST erasure and recursive types. The pace exploded from March
on (40–50/month, 91 in July).

Pre-2026 context (useful if framing content as "the last 12 months"): the big
story of late 2025 was the execution-pipeline rewrite — the typer-walking VM
replaced by an IR-based one. typer → llvm became typer → ir → llvm, with the
VM executing that same IR, for huge speedups in compile-time execution.
December 2025 was the LLVM codegen rewrite on top of the new IR + the
ABI/calling-convention system, plus AoC 2025 in K1 with a dev diary. 2026 then
took it a level lower still (see the bytecode VM below).

## Language design & syntax

- ★ **The great syntax overhaul (July 19–20)** — semicolons gone
  (newline-terminated statements), `=` as the universal assignment (`:=` and
  `<-` deleted), leading-dot struct literals `.{ x = 1 }`, quiet sum
  construction (paren-free variant payloads in exprs and patterns), quiet
  `return`/`break`/`continue`. One week where the language got its final look —
  perfect before/after screenshots.
- **String interpolation redesign (July 20)** — `$ident` / `${expr}`,
  p-strings removed, `\{` escape, block strings with dedent (July 21),
  multi-strings (April 1).
- **The great kebabification (Jan 31 + July 19)** — all types lowercased,
  lowerCamel → kebab-case across the language and stdlib.
- **Recursive types (Feb)** — full type-recursion rewrite; then **recursive
  generics** (July 25) with no new type-system machinery.
- **Enums rebuilt (Jan–May)** — rewritten in terms of unions and structs
  (Jan), int-represented enums + `enum` builtin ability (March),
  user-supplied enum/tag values (May).
- **ZST erasure (Jan)** — `unit` removed in favor of a single canonical empty
  struct.
- **Opaque types (June)** — `deftype` → `type`, nominal opaques, match on
  opaques. This is what unlocked bindgen.
- ★ **Closures with explicit capture lists (July 28)** — `fn` =
  non-capturing, `fn[a, b.&]` capture list, lambda = by-value env struct,
  allocation only at the dyn lift. "No hidden allocations" story.
- ★ **Ability objects (July 28)** — `dyn[ability[args]]` inline fn-ptr
  tables, explicit `to-dyn` only, interned by content. Pairs with closures as
  "dynamic dispatch without a runtime."
- **Context ability variables (July 28)** — `let(context(impl writer))`,
  ambient capability passing with strict conflict rules.
- **Ranges & iterators (July 20)** — `range[t]`, the `rangeable` ability
  (`0.until(n)`), next-primitive iterator protocol, no dedicated range syntax.
- **The place refactor (July 10)** — unified "place" semantics, `.&` postfix
  address-of, `let*` gone, address-of coercion.
- **`from-string` ability (July 27)** — infallible vs fallible encoded in the
  type (`never` error = infallible).
- ★ **Allocator redesign (Aug 8–10)** — `allocator` ability + `heap`, ambient
  allocation = TLS arena stack (`current-arena`/`with-arena`), four alloc
  builtins deleted, platform memory regions with arena chaining.
- **Declaration modifier syntax (Aug 11)** — modifiers as keyword-call lists:
  `let(mutable, tls)`, `fn(intern)`, `fn(extern("sym"), lib("pq"))`,
  namespace-level `lib`.
- **Pattern & control-flow niceties** — `type[T](pattern)` patterns (Jan),
  infallible-pattern detection in `require` with `else` elision (April),
  static `switch` (May), companion namespace syntax (May).

## Metaprogramming

- ★ **Macros (July 20–27)** — `macro` definitions, `code` as the currency with
  full span provenance (errors point into your source, not the expansion),
  macro bodies as bare templates, block-backtick code literals, `.fmt`
  replacing stringf. And the flex: **`for` loops are now a macro** — the
  language's own for-each is user-level code (July 23).
- ★ **Compile-time SIMD metaprograms (July 26)** — `vector[t,n]` up to
  512-bit, `intern("llvm.*")` intrinsic lane, `$simd` metaprograms, **52x
  speedup on string search**. That number is a tweet by itself.
- ★ **Routing megaprogram (July 27)** — `$pre/routing`: define an HTTP route
  enum once, get parsing + reverse-URL generation, with compile-time
  `#static` round-trip assertions.
- **std/bitfield** — metaprogrammed bitfield generation.
- **Metaprogramming shorthand syntax (April 30)** — `$` / `\` directives.

## Compiler engineering & performance

- ★ **The bytecode VM (April–July)** — the sequel to 2025's VM rewrite: having
  already replaced the typer-walking VM with one that executes the IR, 2026
  replaced *that* with a VM executing a lower bytecode. April 11's commit says
  it best: "Rename bytecode and bc to ir, because we about to go lower." The
  new `bc` layer landed through spring with a dual-exec parity harness
  (bytecode and IR execution checked against each other), and July's exec perf
  work (variable-length call instructions, fallthrough jumps, raw bit widths
  instead of physical types) all runs on it. Two generations of VM replaced in
  under a year — great "how fast can compile-time execution get" thread.
- ★ **Perf war, July edition (July 7–24)** — lexer rewritten (started March
  25), fast token peeking, 8-byte tokens with a trivia side-table, faster
  inlining, much faster megaprogram execution (on the new bytecode VM above).
  Thread material: "how I made my compiler N× faster."
- ★ **Superlinear-compile fix, 21x (Aug 10)** — 100× stress benchmark
  harness, hash-key collision fix, ability-scope walk hoisting, names-keyed
  method resolution. War-story post.
- **Parallel source file loading + typechecking read-ahead (Aug 2).**
- **IR overhaul (April 29)** — doubly-linked-list IR, function inlining, CFG
  simplification; earlier: RVO, out-params in bytecode, constant scalar
  global folding (July 11).
- **Type-system internals** — type params bound by id not name (July 25),
  specialization-cache binder sharing (July 29), handrolled string interner
  (July 30), one unified string pool (June 28).
- **ABI correctness grind (June)** — amd64 eightbyte collection, arm64 HFA
  detection, struct-in-integer fixes, 3-byte struct fix. Unsexy but
  credibility-building.
- **Debug info fixed (March 11), sanitize flag, optimized debug builds.**

## Tooling

- ★ **LSP, built up all year** — hover + goto definition for types (May),
  find references (May), semantic tokens (April), **completions via
  marker-splice, ~30ms per completion with zero partial-parsing hacks**
  (July 23), signature help + ability method completion (Aug 8), VS Code
  extension (April). The marker-splice completion approach is a genuinely
  novel technical post.
- ★ **k1bindgen (May–June)** — libclang-based C bindings generator with
  rename/prefix rules, recursive schemas. Proved by generating **unedited
  libuv bindings that just worked** (June 15).
- ★ **Module system (Aug 2–5)** — `module()` declarations, bare-name deps,
  valued params, `setup()` build steps run in the VM with per-target stamps.
  Modules can vendor and build C (cmake, cc, bindgen) as part of setup —
  libuv and llhttp modules prove it.
- ★ **Compile cache snapshots (Aug 6–7)** — `.k1-out/cache`,
  inputs-hash-named snapshots, deepest-restore. Fast rebuilds, LSP-ready.
- ★ **Hot reload (Aug 11–12)** — live code swap working: stubs, fn-address
  globals, dylib reload, `std/reload` loader. *The* demo video once polished.
- **Fil-C memory-safe target (July 19)** — K1 compiling through Fil-C's LLVM
  fork on Linux.
- **Distribution (March–June)** — macOS + Linux build/bundle/install scripts,
  test suite shipped in the bundle for cross-platform smoke tests.
- **std/process (Aug 2–6)** — posix_spawnp, checked-by-default exit codes,
  merged output capture.

## Real programs (dogfood — the proof it all works)

- ★ **Brotli encoder (Aug 11–12)** — quality 0 and 1 encoders
  **byte-identical to the C reference, at 0.97–1.3× C's speed**, verified
  against a vendored C oracle. The flagship "K1 is a real systems language"
  artifact.
- ★ **HTTP framework (June–July)** — libuv event loop + llhttp parsing,
  connection arenas, SSE subscriptions, streamed responses, pubsub topics,
  vectored zero-copy SSE writes.
- ★ **Megarepl webapp (July 16–18)** — live web REPL with SSE
  command/publish architecture and widgets. Very visual; good demo footage.
- **JSON parser (Feb), Postgres client (April), raylib demo (July), AoC
  solutions.**
- **std library growth** — hash ability + map/set (July 19), atomics +
  threads (July 22), time, arena fast paths, buffer/span/list
  standardization.

## Suggested framing

The year's arc: *January: the language got its identity (syntax, kebab,
recursion) → Spring: it got tooling (LSP, bindgen, builds) → Summer: it got
superpowers (macros, SIMD, allocators, modules, hot reload) → and it ships
real software (HTTP framework, brotli at C speed).*

Five strongest individual pieces for tweets/videos: hot reload demo, brotli
byte-identical-at-C-speed, 52x SIMD string search, `for`-is-a-macro, and the
30ms LSP completions story.
