# Walls collected from agents (for the final report to the user)

## palette (02)
- Literal-typed globals rejected: `let BG-HEX: "#16181d" = #static hex(BG)` at top level -> "Globals cannot be typed as 'static t'; you can use them in type position directly". Works only as a local.
- `//errmsg` cannot pin a compile-time crash message: crash text is Info-level (vm.rs:1603 report_execution_messages); only Error is generic "Static execution exited with code: 111" (bc/exec.rs:178). Every failing-static test has the same expectation.
- `static[string, ...]` has no `print`; error tail garbled: "...using expected type: '0". Wrong pin cascades into "No value 'bg-hex' is visible here".
- No hex formatter in core/std (format-uint has no zero padding).
- `.pow` on f32 needs `use std/math/pow`; error "Method 'pow' does not exist on type: 'f32'" doesn't hint the ability.
- f32 interpolation always prints six places; no precision control in a hole.

## general
- `for` was a macro Jul 23 - Sep 1 (removed in 39fc5981); 2026_highlight.md claim stale.
- k1_test name filter does not apply to dir modules (check.sh <filter> still runs every dir case).

## macros (04)
- (repeat) crash in static/macro context: message is info-level via k1/emit-compiler-message + sys/exit(111); the Error box is only "Static execution exited with code: 111" (vm.rs:1603, bc/exec.rs:178). //errmsg can't check the real message. CANDIDATE COMPILER FIX after agents finish.
- Definition-level `$` macro crash points at the macro definition line, not the `$` call site. No way to turn a code chunk's source span into k1/source-location from user code.
- `#debug` on a macro definition prints typed body/IR/bc, not the expansion text; ai_docs/k1-additional.md sentence is wrong. Workaround: `#static println(define-impl(...).text())`.
- `type-id.spell()` renders builtins as `(#type type-id/make(14))`: emitted code unreadable; scalars/nominals could spell by name.
- CLI: `--cache false` must precede the subcommand.
- 2026_highlight.md lines 82 and 174 ("for is a macro") are false: prototype for-each macro added 6911b70b (Jul 23), deleted 39fc5981 (Sep 1); typer.rs eval_for_expr comment "Project: Kill all this with the macro system".

## static (01)
- BUG: k1_test cwd race: compiler.rs:1360 CwdGuard::enter(home_dir) chdirs the whole process per module; k1_test compiles cases on parallel threads -> relative file reads in #static/globals race. No K1-side anchor for the module dir (core/k1/location().filename is a basename; no k1/module-dir). Repro: scratchpad/static_embed/ under k1_test with another case.
- BUG: `types/id[fifteen].name()` on a static literal type SIGTRAPs at runtime (signal 5, no diagnostic, stdout lost); in VM: "Missing type info: static[i64, 15]". Repro: scratchpad/exp/exp1c.k1, exp1f.k1, exp1h.k1.
- Literal-type annotation ignores expression int type: `let check: 0xCBF43926 = #static crc32()` (u32) typechecks but from-static() is i64; need `0xCBF43926u32`; `(lit: u32)` in type position doesn't parse. Repro exp/exp10b.k1.
- Cascade: failed static let -> "No value 'x' is visible here" at each use (2 errors for 1 mistake).
- (repeat) static[string,..] no print impl; globals can't carry literal types.
- `#static println` diagnostics point at core.k1:26, trailing newline becomes a second empty info message.
- test-compile in inferred position: "Cannot use test-compile when types are being inferred".
- Minor: `c-int` must be `core/c-int` in fn main; `if forward -1 else 1` parse error (- mid-line is binary); no string.to-int (i64/from-string).

## model (06)
- No niches: size[result[int, never]] = 16, size[?*int] = 16, size[?int] = 16.
- Dyn target not inferred through a generic param: `drain(sum.to-dyn(), [1,2,3])` with dyn[accumulator[t = t]] param -> "summer implements 'accumulator', but with i64, not '1".
- No integer -> enum conversion (`(2: i32).as[errno]` rejected; bitcast rejected). Repro p_enumcast.k1.
- Inconsistent core qualification: mem/new bare, but arena/init, type arena, io/stdout need core/.
- Impl fns with unused `self` warn "Parameter is never used: self" though the ability dictates it.
- (repeat) k1_test filter ignores dirs; CLI flag order.

## types (05)
- PANIC: `#if types/schema[t] is { :int _ -> ..., _ -> ... }` inside a generic fn: "thread 'main' panicked at src/k1/ir.rs:1602:17: ir: function should have a body I think". Non-generic: "Only enums are supported in static match for now". Repro scratchpad/k1/tp2.k1, tp5.k1.
- `string/from-string` "Function not found" though u64/from-string resolves; `from-string/from-string[t](s)` can't bind self (nz10.k1); `from-string@(t)/from-string(s)` works.
- `where t: from-string[e = e]` with sibling type param e: "Type 'e' not found" (nz8.k1); inline form works.
- `type(alias) patch[t] = #type ...`: "Alias types cannot have type parameters (yet)".
- `#for`'s `it` is not static: `#if it == 4` -> "No value 'it' is visible here"; `matrix[type-of(it)]` -> "Expected static[i64] but got i64" (tp8.k1).
- types/name of a synthesized either is "anon_enum_u16_7056" (struct prints structurally).
- Stdlib gap: no string.trim.
- `ptr.as[*t]` rejected with hint "Use .narrow instead" (undocumented); `.ref()` works.
- `x is :a or :b` parses as boolean or ("Not a sum or enum type: bool"); or-patterns only in match arms.
- ASSEMBLY NOTE: examples/static_params.k1 (01) overlaps types_static_params.k1 (05); dedupe.

## comptime-ffi (10)
- (repeat, 3rd report) k1_test parallel cwd race also breaks `fn setup`: CwdGuard::enter at compiler.rs:1360 and :768 use process-global set_current_dir; a sibling test finishing mid-setup restores cwd -> `cc: no such file 'atlas.c'`, and a stray `content/showcase/libs/` dir was created in the runner's cwd. Threatens every module setup under test.sh. Workaround: `cd "${ctx.module-dir}"` in the script. Repro: cd content/showcase && rm -rf examples/cffi_font_atlas/{libs,.k1-out} && k1_test --tests-dir examples cffi.
- Trailing-expression return type mismatch caret points at the fn header, not the expression.
- Warning wording: "This #static is immediately inside a static" should say global initializers already run at compile time.
- k1/setup-ctx exposes no target dylib extension (setup scripts hardcode .dylib/.so).
- No write-char on writer (used write-byte).

## reflection (03)
- No size/stride/align from a type-id (types/size takes a type param only; schema fields have offsets but no sizes; `types/layout` in builtin.k1 unused). Layout dumper must round-trip through #meta + spell().
- BUG-ish: #meta crash inside nested generic instantiation (holder { xs: list[int] } -> list -> buffer -> *i64) reports "Function fn count[t := i64](v: i64): i64 needs its own compiled ir while it is being compiled" instead of the crash text; `*i64` seems auto-derefed to t := i64. Repro: scratchpad/probe/nested_meta_crash3.k1 (source in agent report).
- (repeat) compile-time println reports [core.k1:26 info] + empty second message; k1/emit-compiler-message(k1/location(), :info, s) gives the right location.
- assert-layout failure: "[types.k1:42 error] ASSERT FAILED: 8 != 5" names neither type nor which of size/align, location inside types.k1.
- (repeat) test-compile inside string interpolation: "Cannot use test-compile when types are being inferred".
- Ability impls not reachable via the type's namespace (level/from-string after impl -> "Namespace not found: level").
- Derived print stops at references (opt[*person] field kills derived print) and doesn't quote strings.
- Doc note: `\n` inside a backtick template is a real newline in emitted code; need `\\n`.
- `--cache false` still logs "restored 2 modules from cache" (core/std).

## systems (07)
- Literal receiver ignores argument type: `0.until(N)` with N: u64 -> "Expected i64 but got u64 / Occurred in call parameter 'until.end'"; need 0u64.until(N). walls/wall_until_literal.k1.
- `u64.as[ptr]` rejected ("Cannot cast integer 'u64' to 'ptr'") though ai_docs says ptr/word casts use .as; `to-ptr()` exists (core.k1:517, fn(inline)). walls/wall_int_as_ptr.k1.
- (repeat) bare literal type doesn't take int kind from #static RHS (static[i64,4] vs static[u64,4]); cascade "No value 'x' is visible ... expected type: '0". walls/wall_static_literal_type.k1.
- No black-box for benchmarking: --optimize deletes dead reps (rep 0: 0 ns); workaround atomic/fence(:seq-cst) pairs + asserting results. walls/wall_bench_sink.k1.
- Every wasm64-wasi compile prints 4 warnings from ~/.k1/modules/std/watch.k1:202/212/220 "Parameter is never used: self" (wasi arm of #if k1/platform).
- Vector ops are bare fn(intern) compiler intrinsics, not intern("llvm.*"); only trailing-zeros rides llvm.cttz.i64 (2026_highlight.md phrasing "intern("llvm.*") intrinsic lane" is loose).
- NOTE: concurrent edits by the user in src/k1/typer.rs, typer/{infer,static_exec,trace}.rs and test_src/suite1/static_run.k1 during the session (not agents).

## regex (08)
- (repeat) macro crash diagnostics: VM prints "[file:29 error] <msg>" at the crash call, error box points at the macro definition, never at the `$define(...)` call site / pattern literal though execute_macro_call has the call span. Expression-level macros same (probe6.k1).
- (repeat) `#debug` on macro doesn't show expansion; emitted sources land in .k1-out/generated/ only when a diagnostic hits them (write_emitted_sources checks has_diagnostic). Ask: --dump-emitted or #debug printing emitted text.
- Parse wart: `if t is :some j :some(g.[j]) else :none` -> "Expected ) at '.'" (`j :some(` read as ascription). probe5.k1.
- Braceless else with assignment misparses: `if b > 5 { ... } else b = b + 1` -> "else branch type did not match" pointing at b. probe7.k1.
- Trailing CLI flags after subcommand silently swallowed for `run` (`k1 run f.k1 --cache false` runs with cache), error for `build`.

## generators (09)
- PERF: `some fn` params compile to function pointers (one body per t, indirect call, callee not inlined) rather than per-callee specialization as the docs imply. See examples/.k1-out/keywords_switch.ll after --optimize --emit-llvm.
- PERF: dynamic-length string `==` is a libc memcmp call; constant-length compares inline. A short-length fast path in core/buffer/equals-bits would help every runtime string compare. Repro scratchpad/exp/kw_table.k1.
- `size` is i64 (signed), `usize` u64; `>>` on size is arithmetic; naming surprise.
- Enum variants cannot be keywords (`either { if, fn }` -> "Expected <ident> at 'if'"). Repro exp/e2.k1.
- (repeat) #debug on macro; crash in macro -> generic diagnostic at macro def; CLI flag order.
- Good: dead code after return is a hard error; unused lambda captures warn; let inside ns for; it-index in named for loops.

## runtime benchmarks
- PERF (design): arena alloc pays a libc memset per allocation after reset(): arena.k1 `_claim` lazy re-zeroing emits llvm.memset with runtime size; mem/new then overwrites the same bytes (double write); mem/current-arena() reads TLS per call (_tlv_get_addr 4%). Whole gap to Zig on binary-trees (2.13 s vs 1.18 s; memset+bzero 30% of samples). Fix ideas: non-zeroing claim for push/new/alloc-t (caller fully initializes), or inline stores when size is a compile-time constant.
- `buffer[u8].contains` is the scalar iterable default (only span/list/string shadow it); buffer has SIMD `position` but no `contains`: 15 ms vs 5 ms on 256 MiB. Repro benchmarks/runtime/walls/buffer_contains_scalar.k1. Doc claim "implemented once in buffer" only true for position.
- Shift amount must be u32; i64 amount is a type error with no coercion ("Expected u32 but got i64"). Repro walls/shift_amount_i64.k1.
- `$std/simd/first-of` only scans a string, not span[u8].
- Env: zig 0.14.0 can't link on macOS 26 SDK by default (libSystem.tbd lacks arm64-macos target; undefined _sigaction); run.sh passes --sysroot of MacOSX15.4.sdk.
- Results (prelim, noisy): binary-trees k1 2.13s / zig 1.18 / go 2.88 / c 6.2 / rust 7.0; hashmap k1 0.197 / c 0.30 / rust 0.36 / zig 0.37 / go 0.52; byte-scan k1 0.367 / c 0.40 / go 0.81 / zig 0.89 / rust 1.52.

## brotli perf
- No restrict/noalias: `*bit-writer` forces `pos` through memory on every bit write; workaround = thread the bit-writer by value. Repro: git show HEAD:dogfood/brotli/bits.k1 + objdump compress-fragment-impl.
- (repeat) `some fn` param -> indirect call (`blr`) per comparison in sort-huffman-tree-items; no per-callee specialization.
- `--emit-llvm` builds a different program: one codegen unit, whole-module inlining (11 vs 33 brotli. symbols); the .ll is not the IR of the normal binary.
- Aggregate ABI: buffers cross non-inlined calls as [2 x i64] with pointer as i64 + inttoptr, losing provenance/pointer attrs.
- No fn(noinline); layout swings of 2-4% on the q0 hot loop from any size change.
- `.[i]` on a global array is bounds-checked in hot loops; no unchecked index for arrays short of .&.as-buffer().index-unchecked.
- HEAD had two "type is already u64" warnings in huffman.k1:610/625 (removed by the agent).
- Old bench harness overstated K1 on small/incompressible corpora by 10-30% (zeroed arena buffers per C stream, 0.25 ms per point); fixed in main.k1.
- Results (prelim): typer.rs q0 0.998x, q1 0.997x of C; random-300k q0 0.958x; compile 289 ms optimized / 119 ms plain vs cc -O2 1.07 s for the three C files; 2,020 K1 lines vs ~6,000 C lines.

## compile-time benchmarks
- PERF: ThinLTO at N=3600: 48 s wall, 109 s CPU. Not serial: the thin-link is 48 ms and LLVM's ThinLTOCodeGenerator runs the backends on its own 14-thread pool. The tail is one unit, the one holding `main`: the importer pulls all 3,600 `u<i>/test` callees into it, the inliner folds them into one function, and the per-function passes go superlinear on it (~7 cores for 11 s, then 1 core for 37 s). `-import-cutoff=1000` takes the phase to 8 s, but it is a process-global `static int ImportCount` in FunctionImport.cpp (first N imports across every module, visit-order dependent), not a per-module cap, and LLVM's inliner has no caller-growth limit either, so there is no landable LLVM dial. Real fix: cross-unit inlining decided in ir/iropt.rs before partitioning, with the LLVM importer off. Measurements in design/showcase-plans/thinlto-and-dev-pipeline.md.
- PERF: `snapshot store` on check's critical path: 107.55 ms of a 201 ms `check` at N=300 (cache on); restoring core+std saves ~15 ms; net the cache makes check 2x slower at this size.
- Disk cache never GC'd: 675 MB of .snap after ~30 nonced compiles at N=300 (34 MB each).
- PERF: the `passes` trace row was object emission (~58%) + `verify()` (~8%) + the Dev passes (~34%); it is now split into `passes` and `emit`. The Dev passes net ~3% of N=300 build CPU for a 29% smaller binary: mem2reg pays for itself in ISel, instcombine costs ~0.5 s for 6% of the binary, mergefunc is 15% of the binary for free; globaldce bought 2.3 KB and is gone. `verify()` now runs only in the debug compiler. Object emission at OptimizationLevel::None is where the next default-build win has to come from.
- `check` typechecks fewer specializations than `build` (10,476 vs 40,782 typecheck frames): can a post-monomorphization error pass check and fail build?
- (repeat) CLI global flags after subcommand rejected.
- perf/stress100/stress100.k1 in the tree is 3600 units, not 300 (name misleading).
- Env: Zig 0.14.0 needs -target aarch64-macos on macOS 26.
- Results (prelim, load 9-10): N=300 check K1 0.094 s vs C 0.477, C++ 11.0, Rust 2.51, Go 5.37 (build), Zig 0.69; debug build K1 0.81 (4.9 CPU) vs C 2.56, C++ 18.7, Rust 3.9, Go 5.4, Zig 8.6; optimized K1 2.0 (13.6 CPU) vs C 11.2, C++ 32.0, Rust 4.5, Go 5.4, Zig 23.8. Hello world K1 check 0.023 / debug 0.084 / opt 0.135.

## comptime benchmarks
- `#if FLAG (#static work()) else work()` -> "Expected expression at 'else'": parens parse as a call on FLAG; no hint. Repro scratchpad/walls/if_paren_call.k1.
- `--chatty true` prints "58.1M instrs (0.00us/instr)": per-instruction time rounds to zero; want ns/instr or Minstr/s.
- `for b in bytes` over list[u8] costs ~15% more VM instructions than an index while loop (5.6M vs 4.8M, crc32 64 KB). Repro scratchpad/probe/k1/crc32_while.k1.
- Results (prelim, throttled): VM ~370-410 M instr/s; sieve 1M K1 VM 0.14 s vs C++ 6.1 s, Rust 3.3 s, Zig 100k 48.7 s (1M died); crc32 8 MB K1 1.9 s vs C++ 54 s, Rust 51 s, Zig 64 KB 11 s (1 MB timeout); strbuild 1M K1 2.3 s vs Rust 42 s, C++ 100k 14 s, Zig 10k 7.6 s; reflect-serialize ms/type: Go 0.13 (runtime reflection), K1 0.57, Rust serde 0.58, Zig 3.0, C++26 P2996 4.7, C++23 manual 29.8.

## added 2026-09-16
- FIXED by design (eager zero on reset): per-allocation memset in arena _claim; binary-trees 2.07 s -> 0.98 s with explicit arena param.
- Ambient mem/new costs a TLS descriptor call per allocation on macOS arm64 (~6% on binary-trees); hot code should take the arena as a param; a context-param ambient allocator would remove it.
- buffer/position-byte and $std/simd/first-of process one 16-byte vector per loop iteration; .NET's Span.IndexOf unrolls 4 vectors and packs the IndexOfAny compares, and beats K1 0.27 s vs 0.34 s on byte-scan.
- The agents' reproducer files are session-scratchpad only (walls/, probe/, k1/, exp/) except benchmarks/runtime/walls/.
