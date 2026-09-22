# SIMD byte scans: unroll `position-byte`, pack `first-of`'s compares

Wall source: `design/showcase-walls-2026-09-16.md`, "added 2026-09-16" and
"runtime benchmarks". Measured on this machine (M-series arm64, `--optimize`,
background Bash, 5 reps of a 256 MiB buffer, interleaved, ms totals for all 5).

## What the measurements say

Prototypes: `scratchpad/simd-scan/{scan,scan2,scan3,scan4}.k1` (session
scratchpad, not in the tree).

`position-byte` shape, absent needle / needle every 4093 B:

| unroll | absent | with hits |
|---|---|---|
| 1x (today) | 25 ms | 30-32 ms |
| 2x | 20 ms | - |
| 4x | 17-18 ms | 18-20 ms |
| 8x | 17-19 ms | - |

4x is the knee: **1.4x absent, 1.6x with hits**; 8x buys nothing (1.25 GiB /
17 ms = 79 GB/s, i.e. DRAM bound).

`first-of` with 3 needles, delimiter every ~1 MB:

| shape | time |
|---|---|
| today (k `to-mask` per 16 B block) | 105-111 ms |
| OR the `eq-lanes` **vectors**, one `any()` per block, no unroll | 39-40 ms |
| same + 4x unroll | 40 ms |

**2.7x, and all of it comes from the vector-form OR, none from the unroll.**

Why: `objdump` of the prototype shows `vector/to-mask` (the
`bitcast <16 x i1> -> i16` in `codegen_llvm.rs:3685`) lowering to a
`movi`/`and`/`addv.8h`/`umov` chain on arm64, while `.any()`
(`to-mask() != 0`) is folded by InstCombine into `vector.reduce.or` and comes
out as a single `umaxv.16b` + `cbz`. Today `first-of` pays the addv chain k
times per 16 bytes; `position-byte` already gets the good form because LLVM
splits `first-true-lane() >= 0` into `umaxv` for the test and the addv chain
only on the taken branch.

## Recommendation

1. **`modules/core/buffer.k1:151-168` `position-byte`: 4x unroll.**
2. **`modules/std/simd.k1:131-178` `first-of-impl`: OR in vector form, one
   `any()` per block; do NOT unroll it.**
3. **`first-of` scans `span[u8]`; `string` becomes a one-line wrapper.**
4. **No shared loop macro.** The two loops now differ in shape (one unrolled,
   one not) and in result (`?size` vs `{index, which}`), and `core` cannot
   reach a macro living in `std`. The only genuinely shared case is a
   single-needle `first-of`, which should just call `buffer/position-byte`
   (see 3b) — that is the duplication worth deleting.
5. **Item (2) of the brief, `buffer[u8].contains`, is a non-wall: drop it.**

### 1. `position-byte` (modules/core/buffer.k1:151)

Keep the `if len >= w` guard (it skips the splat for short buffers) and widen
it to hold both vector loops. Needs no new lane ops: `bit-or`, `any`,
`first-true-lane` all exist in `modules/core/vector.k1`.

```
fn position-byte(base: ptr, len: size, needle: u8): ?size {
  let w: size = k1/simd-bytes
  let i = 0: size
  if len >= w {
    let n = vector/splat[u8, k1/simd-bytes](needle)
    while i + 4 * w <= len {
      let e0 = vector/load-unchecked[u8, k1/simd-bytes](base.ref-at[u8](i).as[ptr]).eq-lanes(n)
      let e1 = vector/load-unchecked[u8, k1/simd-bytes](base.ref-at[u8](i + w).as[ptr]).eq-lanes(n)
      let e2 = vector/load-unchecked[u8, k1/simd-bytes](base.ref-at[u8](i + 2 * w).as[ptr]).eq-lanes(n)
      let e3 = vector/load-unchecked[u8, k1/simd-bytes](base.ref-at[u8](i + 3 * w).as[ptr]).eq-lanes(n)
      if e0.bit-or(e1).bit-or(e2).bit-or(e3).any() {
        let l0 = e0.first-true-lane()
        if l0 >= 0 return :some (i + l0)
        let l1 = e1.first-true-lane()
        if l1 >= 0 return :some (i + w + l1)
        let l2 = e2.first-true-lane()
        if l2 >= 0 return :some (i + 2 * w + l2)
        return :some (i + 3 * w + e3.first-true-lane())
      }
      i = i + 4 * w
    }
    while i + w <= len {
      let lane = vector/load-unchecked[u8, k1/simd-bytes](base.ref-at[u8](i).as[ptr]).eq-lanes(n).first-true-lane()
      if lane >= 0 return :some (i + lane)
      i = i + w
    }
  }
  while i < len {
    if base.ref-at[u8](i).* == needle return :some i
    i = i + 1
  }
  :none
}
```

Tail: three loops, 4w-block then w-block then scalar. Do not switch to an
overlapping final load — it would need a clamp against `i` and buys nothing
measurable. Unaligned `ldr q` is free on arm64 and the existing code does not
align either, so no alignment prologue.

This is the only `position-byte`; `buffer/position`, `span/position`,
`list/position`, `string/position`, `contains` and the `k == 1` arm of
`index-of-bytes` all land here, so one edit covers every byte scan in the tree.

### 2. `first-of-impl` (modules/std/simd.k1:145-166)

Replace the emitted body of the vector loop. Per block: one `eq-lanes` vector
per needle, OR them, one `.any()`. Only inside the taken branch materialize
the `to-mask`s and pick the needle.

```
  while i + $lanes <= len {
    let chunk = vector/load-unchecked[u8, $lanes](base.ref-at[u8](i).as[ptr])
    let e<j> = chunk.eq-lanes(needle<j>)            // one per needle
    if e0.bit-or(e1)...bit-or(e<k-1>).any() {
      let m<j> = e<j>.to-mask()                     // one per needle
      let lane = m0.bit-or(m1)...bit-or(m<k-1>).trailing-zeros()
      if m<j>.shift-right(lane.trunc[u32]).bit-and(1) == 1 {   // j < k-1
        return .{ index = i + lane.signed(), which = <j> }
      }
      return .{ index = i + lane.signed(), which = <k-1> }     // last needle
    }
    i = i + $lanes
  }
```

The last needle returns unconditionally, so `crash("unreachable: mask bit
vanished")` (simd.k1:163) is deleted with it.

### 3. `first-of` scans `span[u8]`

Emitted signature becomes

```
fn scan(data: span[u8], from: size): { index: i64, which: i64 }
fn scan-string(s: string, from: size): { index: i64, which: i64 } { scan(s.span-bytes(), from) }
```

`string/span-bytes` is `modules/core/string.k1:8`; `span/data-ptr` is
`span.k1:36`. In the body `let base = data.data-ptr()`, `let len = data.len()`,
and the scalar tail compares `data.[i]` (already `u8`, so `ch.as-u8()` at
simd.k1:170 goes away). Swap rule: the `string` overload of `scan` is gone, not
kept — update the two callers, `test_src/stdlib/simd_test.k1:43-56` and
`content/showcase/benchmarks/runtime/byte-scan/k1/byte_scan.k1:60`.

**3b.** When `needles.len() == 1`, emit a `scan` that delegates instead of a
second copy of the loop:

```
fn scan(data: span[u8], from: size): { index: i64, which: i64 } {
  if buffer/position-byte(data.data-ptr().ref-at[u8](from).as[ptr], data.len() - from, <N>) is :some p {
    .{ index = (from + p).signed(), which = 0 }
  } else .{ index = -1, which = -1 }
}
```

so the single-needle case inherits the 4x unroll and no loop is duplicated.

### 5. Delete the `buffer[u8].contains` wall

The claim (15 ms vs 5 ms, `contains` falling to the scalar iterable default) is
a first-touch artifact of the repro, which times one call each in order.
Re-measured with reps:

```
rep 0: buffer.contains false in 28 ms; span.contains false in 5 ms
rep 1: buffer.contains false in 5 ms;  span.contains false in 5 ms
rep 2: buffer.contains false in 5 ms;  span.contains false in 5 ms
rep 3: buffer.contains false in 5 ms;  span.contains false in 5 ms
```

and `buf.contains(0)`, `span/wrap-buffer(buf).contains(0)` and
`buf.position(0).is-some()` all cost 24-26 ms over 5 reps — identical, and at
51 GB/s, which no scalar byte loop reaches. The `contains` default at
`builtin.k1:476` calls `self.position`, and that resolves to the namespace
`position` (`buffer.k1:202`), not the ability default at `builtin.k1:469`. So
nothing shadows `contains` anywhere, nothing needs to: it is already SIMD.

Action: delete the bullet from `design/showcase-walls-2026-09-16.md`, delete
`content/showcase/benchmarks/runtime/walls/buffer_contains_scalar.k1`, and fix
the "implemented once in buffer / only true for position" line in the showcase
text it feeds.

## Tests

Vector ops need no VM work: `bc/lower.rs:780-940` lowers `Splat`, `EqLanes`,
`BitOr` and `ToMask` to per-lane scalar opcodes, and `test_src/stdlib/stdlib.k1`
already runs `simd_test/test()` under `#static`. The 4x unroll is neutral there
(4x the opcodes per iteration, 4x fewer iterations).

- `test_src/suite1/buffer_test.k1` (already registered at `suite1.k1:114-115`
  and run natively **and** `#static`): add `buffer-position-byte()`. Cover
  lengths 0,1,15,16,17,31,63,64,65,79,80,127,128,129 with the needle absent,
  and for each length place the needle at every index and assert the position —
  that is the only way to catch an off-by-`w` in the three-loop tail, and it
  exercises all four slots of the unrolled block plus the block/scalar tails.
  Keep the lengths small so `#static` stays cheap.
- `test_src/stdlib/simd_test.k1:42-57 test-first-of`: port to the `span[u8]`
  API, keep the existing five cases, and add (a) a delimiter past byte 64,
  (b) two delimiters in the same 16-byte block where the later-listed needle
  comes first (pins `which` when several masks are hot), (c) a single-needle
  `$std/simd/first-of(one, [','])` instantiation to cover the 3b delegation,
  (d) a `scan-string` call so the wrapper stays live.
- `just ts1` for the fast check, then bare `./test.sh` (exit code, not piped).

## Benchmark

`content/showcase/benchmarks/runtime/run.sh` with `BENCHES="byte-scan"`,
reading the ratio against the unchanged C#/C/go/zig/rust entries in
`results.md` (k1 0.367, .NET 0.27 prelim). Run it from background Bash only.
Do not read `--emit-llvm` output for perf conclusions; `objdump -d` the real
`.k1-out/byte_scan` binary and check the hot loop is
`ldr q / cmeq.16b x4 / orr.16b x3 / umaxv.16b / cbz` with no `addv.8h`.
Sanity: the prototypes predict ~1.5x on `count-newlines` and ~2.7x on
`scan-delims`, which is the bulk of that benchmark.

## Effort

~2 hours. `position-byte` ~20 lines, `first-of-impl` ~25 emitted lines plus the
signature change and 2 callers, tests ~60 lines, one benchmark run. No compiler
(Rust) changes, no new intrinsics, no VM changes.
