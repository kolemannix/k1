# Showcase walls still standing

Rechecked 2026-09-23 against master. Repros are in `design/showcase-walls-repros/`
(named in brackets); run them with `K1_HOME=<repo> k1 run <file>`.

## Diagnostics
- Macro crash points at the macro body, never the `$`/call site, for expression
  and definition macros [macro_crash_call_site, macro_crash_call_site_defn].
  Plan: design/showcase-plans/macro-expansion-dump.md.
- The bc execution trace of a crash leads with `platform/process/exit`,
  `sys/exit` and `crash` frames before the user's frame, and ends with an empty
  line before the box's closing border.
- Errors inside call arguments and interpolation holes end with "Occurred while
  trying to determine type of argument for inference using expected type: '0";
  the hole prints as `'0`.
- A failed `let` cascades into "No value 'x' is visible here" at every later
  use [shift_amount].
- Trailing-expression return type mismatch puts the caret on the fn header, not
  the expression [return_caret].
- `.pow` on f32 without `use std/math/pow`: "Method 'pow' does not exist" gives
  no hint at the ability [pow_hint].
- A `#static` inside a global initializer warns "This #static is immediately
  inside a static"; it should say global initializers already run at compile
  time [static_in_static_warning].
- `assert-layout` failure reads "[types.k1:42 error] ASSERT FAILED: 8 != 5":
  no type, no size vs align, location inside types.k1 [assert_layout_message].
- Impl fns warn "Parameter is never used: self" though the ability dictates
  the signature [unused_self_warning]; every wasm64-wasi compile prints three
  of these from std/watch.k1's wasi arm.
- `k1` with a `K1_HOME` that has no `modules/` panics at parse.rs:1474 (index
  out of bounds) instead of reporting the missing core.

## Literals and inference
- Literal-typed globals rejected: `let BG-HEX: "#16181d" = #static hex()` at
  top level [literal_typed_global].
- A bare literal type takes i64, not the RHS's int kind: `let x: 4 = #static
  four()` with `four(): u64` fails static[i64,4] vs static[u64,4]
  [static_literal_int_kind]; `(0xCBF43926: u32)` does not parse in type position
  [literal_type_ascription].
- Literal receiver ignores the argument type: `0.until(N)` with `N: u64`
  [literal_receiver].
- Shift amount must be u32; an i64 amount has no coercion [shift_amount].
- `test-compile` in an inferred position (interpolation hole, call arg): "Cannot
  use test-compile when types are being inferred" [test_compile_inferred].
- `where t: from-string[e = e]` with sibling type param `e`: "Type 'e' not
  found"; the inline bound form works [where_sibling_param].
- `from-string/from-string[t](s)` cannot solve self; `from-string@(t)/
  from-string(s)` works [from_string_self].
- `#meta` walking a generic's fields: `count(v.data)` with `data: *i64`
  instantiates `t := i64`, not `*i64` [meta_generic_autoderef].
- `dyn[accumulator[t = t]]` with generic `t`: "dyn does not yet support generic
  ability-side arguments" [dyn_generic_ability_arg].
- A capturing closure's type cannot be named as a callable type argument:
  `*fn() -> t` rejects it, `st[some fn() -> t, t]` is not allowed, `[f: some
  fn() -> t]` does not parse. std/thread task/spawn works around it with a
  lambda inside the `some fn` function and `type-of(body)`.

## Names and scope
- Ability impls are not reachable through the type's namespace:
  `level/from-string` -> "Namespace not found: level" [impl_via_type_ns].
- Core qualification is inconsistent: `mem/new` resolves bare, but
  `format-uint`, `arena/init`, `io/stdout` and `c-int` need `core/`
  [c_int_scope, hex_zero_pad].

## Syntax
- Enum variants cannot be keywords: `either { if, fn }` [enum_variant_keyword].
- `x is :a or :b` parses as boolean `or`; or-patterns only exist in match arms
  [or_pattern_is].
- `if t is :some j :some(g.[j]) else :none` -> "Expected ) at '.'": `j :some(`
  reads as an ascription [is_binding_parse].

## Stdlib
- No zero padding in `format-uint`, so no fixed-width hex [hex_zero_pad].
- No `string.trim` [string_trim].
- Derived print stops at references (an `opt[*person]` field removes print from
  the struct) and does not quote strings [derived_print_refs].
- `type-id.spell()` renders builtins as `(#type type-id/make(9))`; scalars and
  nominals could spell by name [spell_builtin].
- No black-box: `--optimize` deletes benchmark reps; the workaround is
  `atomic/fence(:seq-cst)` pairs plus asserting results [bench_sink].

## Layout and codegen
- No niches in `opt`/`result`: `size[result[int, never]]`, `size[?*int]` and
  `size[?int]` are all 16; only `optref[t]` is one word [niches].
- No restrict/noalias on `*t`: a `*bit-writer` param forces `pos` through
  memory on every write in brotli; the workaround threads it by value.
- Buffers cross internal fastcc calls as `[2 x i64]` with the pointer as i64,
  losing provenance and pointer attributes.
- `.[i]` on a global array is bounds-checked in hot loops; the only unchecked
  path is `.&.as-buffer().index-unchecked`.
- `--emit-llvm` builds a different program: one codegen unit with whole-module
  inlining, so the .ll is not the IR of the normal binary.
- Ambient `mem/new` pays a TLS descriptor call per allocation on macOS arm64
  (~6% of binary-trees); a context-param ambient allocator would remove it.

## Compile time
- ThinLTO at N=3600 takes 48 s wall: the importer pulls every `u<i>/test` into
  `main`'s unit and the per-function passes go superlinear there. LLVM has no
  per-module import cap or caller-growth limit to set; the fix is cross-unit
  inlining decided in ir/iropt.rs before partitioning, with the importer off.
- `snapshot store` sits on `check`'s critical path: 107 ms of a 201 ms check at
  N=300, so the cache makes check 2x slower at that size.
- The disk cache is never GC'd: 675 MB of .snap after ~30 compiles at N=300.
- Object emission at OptimizationLevel::None is the next default-build win.
- `check` typechecks fewer specializations than `build` (10,476 vs 40,782
  frames): can a post-monomorphization error pass check and fail build?
- perf/stress100 is 3600 units, not 100 or 300.

## VM
- `for b in bytes` over list[u8] costs ~15% more VM instructions than an index
  while loop (5.6M vs 4.8M, crc32 64 KB) [vm_for_loop, vm_while_loop].
- `--chatty` prints "58.1M instrs (0.00us/instr)"; want ns/instr or Minstr/s.

## Tooling
- k1_test cwd race: `CwdGuard` (compiler.rs:491) chdirs the whole process for
  each compile and each `fn setup`, while k1_test compiles cases on parallel
  threads (test_suite.rs:347). Relative reads in `#static`, globals and setup
  scripts race; it has produced `cc: no such file 'atlas.c'` and a stray
  `content/showcase/libs/`.
- The k1_test name filter skips only single-file cases; every dir module still
  runs.
- content/showcase/check.sh runs `k1_test` from PATH (the last `just install`),
  not the tree's build.
