# Brotli q0/q1: the K1 port against google/brotli's C encoder

`dogfood/brotli` is a K1 port of the two fast brotli encoders, quality 0
(`compress_fragment.c`, one pass, adaptive command code) and quality 1
(`compress_fragment_two_pass.c`), plus the slice of `entropy_encode.c`,
`brotli_bit_stream.c` and `encode.c`'s streaming path they need. Its output
is byte-identical to the C library's: the harness checks every corpus at
both qualities against `modules/cbrotli` (the vendored google/brotli C tree)
before it times anything, and also round-trips a chunked `process`/`flush`
stream through the C decoder.

This page records how fast the port is relative to the C it was ported
from, what was changed to get there, and what is still in the way.

## Machine and toolchain

- Apple M3 Max (`sysctl -n machdep.cpu.brand_string`), 14 cores (10 P + 4 E),
  38 GB, macOS Darwin 25.6.0.
- K1 `k1 0.1.0`, built from this repo at the commit named in
  `brotli/results.md`. `--optimize` runs LLVM 21 at `O3`
  (`OptimizationLevel::Aggressive`) with the host CPU name and features
  (`TargetMachine::get_host_cpu_name()`, i.e. `-mcpu=native`), one codegen
  unit per group of functions, ThinLTO across the K1 units.
- C: `Apple clang version 21.0.0 (clang-2100.1.1.101)`, invoked as
  `cc -O2 -Ivendor/include -c` by `modules/cbrotli`'s setup step. No
  `-march`; Apple clang's default for arm64-apple-darwin is
  `-target-cpu apple-m1` (`cc -O2 -### -c x.c`). The resulting
  `libbrotli.a` is linked into the K1 binary as an ordinary static library,
  so it gets no LTO.

Both sides therefore run in the same process, on the same input buffers,
timed by the same clock.

## What exactly is compared

| | K1 encoder | C encoder |
|---|---|---|
| quality | 0 and 1 | `BROTLI_PARAM_QUALITY` 0 and 1 |
| window | lgwin 18 (fixed; the fragment size is `1 << 18`) | `BROTLI_PARAM_LGWIN` 18 |
| API | `encoder/new-with-quality-in`, then `reset()` + `compress(input, out)` per stream, one instance reused | `BrotliEncoderCreateInstance`, two `SetParameter`, one `BrotliEncoderCompressStream(BROTLI_OPERATION_FINISH)`, `DestroyInstance`, per stream |
| output buffer | a `list[u8]` with capacity `BrotliEncoderMaxCompressedSize(n) + 1024`, allocated once and cleared per stream | a `buffer[u8]` of the same size, allocated once |
| fragmenting | `1 << 18` bytes per fragment, last fragment is-last | identical (`block_size_limit = 1 << lgwin`) |

The C API has no reset, so an instance per stream is how it is used; the
instance and its 128 KB (q0) / 512 KB (q1) hash table come from `malloc`
inside the timed region. The K1 encoder allocates its arena, table and
storage once and `reset()` only reinitializes the stream header and the q0
command prefix code. Both sides zero the hash table per fragment.

Timing: `mach_absolute_time` around each call. One rep is one K1 stream
immediately followed by one C stream of the same input, so both sides see
the same machine state; a round is enough reps to take at least 200 ms; a
run is 7 rounds and reports the median and the best round for each side.
The `ratio` column is C time over K1 time from the median rounds, so above
1 means K1 is faster. Corpora:

| corpus | bytes | what it exercises |
|---|---|---|
| `cycle-251` | 100,000 | `i % 251`: long matches at distance 251, the copy path |
| `zeros-1mb` | 1,048,576 | one match per fragment, the match-length loop |
| `random-300k` | 300,000 | xorshift bytes: the skip heuristic, then the uncompressed-metablock rewrite; mostly memcpy and per-block Huffman work |
| `typer.rs` | ~750,000 | `src/k1/typer.rs`, mixed text: the literal-heavy path that decides real-world speed |

`typer.rs` is read from the working tree, so its size drifts as the file
is edited; each results record prints the size it used.

Note on the numbers below: they were taken while other agents kept the
machine at a load average of 6 to 30. Per-rep interleaving keeps the
K1/C ratio stable under that load (three consecutive runs agree to about
1%), but absolute MB/s should be reread from a `run.sh` pass on a quiet
machine (`content/showcase/benchmarks/brotli/results.md`). On a quiet
machine the median and best rounds agree within 2%; when a record shows
them far apart (the first `results.md` record has a `typer.rs` q1 round
with median 236 MB/s and best 415 MB/s on both sides), that run was
disturbed and the best column is the one to read.

## Results

"Before" is the encoder at the commit this work started from, "after" is
`dogfood/brotli` as it is now; both were built with `k1 --optimize
--cache false build` and run through the same harness, alternating the
two binaries three times. Each MB/s figure is the median of the three
runs' median rounds; K1/C is the median ratio. C is the same code in both
binaries, so its column is the median over all six runs.

| corpus | q | K1 before MB/s | K1 after MB/s | C MB/s | K1/C before | K1/C after | bytes in -> out |
|---|---|---|---|---|---|---|---|
| cycle-251 | 0 | 9359 | 9335 | 8932 | 1.044 | 1.049 | 100000 -> 344 |
| zeros-1mb | 0 | 5314 | 5096 | 4976 | 1.070 | 1.032 | 1048576 -> 724 |
| random-300k | 0 | 7816 | 7477 | 7774 | 1.005 | 0.958 | 300000 -> 300008 |
| typer.rs | 0 | 628 | 648 | 646 | 0.976 | 0.998 | 749637 -> 187825 |
| cycle-251 | 1 | 8391 | 8403 | 8137 | 1.037 | 1.029 | 100000 -> 285 |
| zeros-1mb | 1 | 18665 | 18572 | 18718 | 0.997 | 0.995 | 1048576 -> 183 |
| random-300k | 1 | 2413 | 2429 | 2421 | 1.000 | 1.003 | 300000 -> 300008 |
| typer.rs | 1 | 396 | 412 | 411 | 0.971 | 0.997 | 749637 -> 175352 |

The three per-run ratios behind the `typer.rs` rows: q0 0.975/0.977/0.976
before, 1.011/0.998/0.998 after; q1 0.966/0.971/0.985 before,
0.997/0.997/0.997 after. `zeros-1mb` q0 swings between 0.96 and 1.10
from run to run in both versions (one match per fragment, so the time is
a handful of long `find-match-length` calls and the block bookkeeping)
and its before/after difference is inside that band. `random-300k` q0
is a real loss and is discussed below.

Unoptimized K1 (`k1 build`, LLVM `O0`, C side still `-O2`) for scale:

| corpus | q | K1 MB/s | K1/C |
|---|---|---|---|
| cycle-251 | 0 | 1322 | 0.148 |
| zeros-1mb | 0 | 1135 | 0.221 |
| random-300k | 0 | 2223 | 0.284 |
| typer.rs | 0 | 213 | 0.324 |
| cycle-251 | 1 | 1457 | 0.180 |
| zeros-1mb | 1 | 2402 | 0.131 |
| random-300k | 1 | 680 | 0.284 |
| typer.rs | 1 | 154 | 0.382 |

`content/showcase/benchmarks/brotli/run.sh` rebuilds the module, runs
the bench (`RUNS=n` times), times the compiles, and appends everything to
`results.md` next to it.

## What changed

### 1. The bit writer became a value

The bit stream writer was `type bit-writer = { storage: ptr, pos: size }`
passed everywhere as `*bit-writer`. That is the natural port of the C,
whose `BrotliWriteBits(n, bits, size_t* BROTLI_RESTRICT pos, uint8_t*
BROTLI_RESTRICT array)` does exactly one thing: load `*pos`, or the bits
into an unaligned 64-bit store at `array + (*pos >> 3)`, add to `*pos`.
The `restrict` on both pointers is what lets clang keep `*pos` in a
register across the whole literal loop; the C literal loop is 13
instructions with one store of `*pos` and no reload.

K1 has no `restrict`. `bw` is a pointer whose address escapes into the
Huffman-building calls, so after every store through `bw.storage` LLVM has
to assume `bw.pos` may have changed and reloads it. Every `write-bits`
became load-pos, load-storage, or, store, store-pos, and the next one
started by reloading pos through store-to-load forwarding. In the
`--emit-llvm` output of the old code:

```text
  store i64 %19, ptr %15, align 1        ; the 8-byte bit store
  %20 = load i64, ptr %12, align 8       ; bw.pos reloaded
  %21 = add i64 %20, %6
  store i64 %21, ptr %12, align 8
```

The q0 literal loop of the old binary was 15 instructions per literal,
three of them through `bw` (x20): reload `storage`, reload `pos`, store
`pos`:

```text
100004c0c: ldrb  w9, [x24], #0x1
100004c10: ldrb  w10, [x19, x9]
100004c14: ldrh  w9, [x14, x9, lsl #1]
100004c18: ldr   x11, [x20]                ; bw.storage
100004c1c: asr   x12, x8, #3
100004c20: ldrb  w13, [x11, x12]
100004c24: and   x8, x8, #0x7
100004c28: lsl   x8, x9, x8
100004c2c: orr   x8, x8, x13
100004c30: str   x8, [x11, x12]
100004c34: ldr   x8, [x20, #0x8]           ; bw.pos
100004c38: add   x8, x8, x10
100004c3c: str   x8, [x20, #0x8]           ; bw.pos
100004c40: subs  x25, x25, #0x1
100004c44: b.ne  0x100004c0c
```

The fix is to thread the writer by value: `write-bits` takes `self:
bit-writer` and returns the advanced writer, every emitter takes and
returns a `bit-writer`, and the one function that also returns a number
returns `{ bw, literal-ratio }`.

```k1 path=dogfood/brotli/bits.k1
  fn(inline) write-bits(self: bit-writer, n-bits: size, bits: u64): bit-writer {
    let p = self.storage.add-bytes(self.pos >> 3)
    store64-le(p, p.ref[u8].*.widen[u64] | (bits << (self.pos & 7).trunc[u32]))
    self | .{ pos = self.pos + n-bits }
  }
```

```k1 path=dogfood/brotli/encoder.k1
fn(inline) emit-literals(input: ptr, len: size, depth: buffer[u8], bits: buffer[u16], bw: bit-writer): bit-writer {
  let bw = bw
  let j: size = 0
  while j < len {
    let lit = input.add-bytes(j).ref[u8].*
    bw = bw.write-bits(depth.index-unchecked(lit).*.widen[size], bits.index-unchecked(lit).*.widen[u64])
    j = j + 1
  }
  bw
}
```

A 16-byte struct travels in two registers, so the writer is now SSA all
the way through the inlined emitters. The q0 literal loop after:

```text
100004364: ldrb  w17, [x21], #0x1          ; next literal
100004368: ldrb  w0, [x19, x17]            ; depth[lit]
10000436c: ldrh  w17, [x5, x17, lsl #1]    ; bits[lit]
100004370: asr   x1, x16, #3               ; pos >> 3
100004374: ldrb  w2, [x25, x1]             ; storage[pos >> 3]
100004378: and   x3, x16, #0x7
10000437c: lsl   x17, x17, x3
100004380: orr   x17, x17, x2
100004384: str   x17, [x25, x1]            ; 8-byte store
100004388: add   x16, x16, x0              ; pos += depth, in a register
10000438c: subs  x15, x15, #0x1
100004390: b.ne  0x100004364
```

Twelve instructions, versus the C's thirteen (clang still stores `*pos`
each iteration). Measured effect (HEAD encoder vs this change plus the
inlining below, same harness, three interleaved runs each): `typer.rs`
q0 ratio 0.971/0.978/0.983 to 1.000/1.001/1.007, q1 0.957/0.966/0.964 to
0.981/0.977/0.976.

### 2. Inlining parity with the C

The C marks every hot helper `BROTLI_INLINE` (`__always_inline__`):
`Hash`, `IsMatch`, `FindMatchLengthWithLimit`, all the `Emit*` functions,
`BrotliWriteBits`, and in the two-pass file `CreateCommands` itself. In
the old K1 binary the q0 hot function still called `emit-insert-len`
(twice), `emit-copy-len` and `emit-copy-len-last-distance` out of line,
each call passing three buffers and the writer pointer, and the two-pass
scan called `emit-insert-len-2p`; `create-commands` was a separate
function, which meant the `cmd-sink` it filled lived in memory and the
q1 literal loop reloaded the literal pointer from the stack on every
iteration. `fn(inline)` on the same set the C force-inlines removed all
of that; the q1 literal loop is now the same 12 instructions as q0's, and
the q0 hot function's only remaining calls are the two per-block Huffman
builders, exactly as in the C's `Impl15`.

The five `emit-*` functions each repeated "write depth[code]/bits[code],
bump histo[code]" in every branch; that became one `emit-code` they all
call, which is also where `fn(inline)` earns its keep.

### 3. Harness

`main.k1`'s `bench` previously allocated a fresh zeroed output buffer from
the arena for every C stream and a fresh `list` for every K1 stream, ran a
fixed 20 reps (0.25 ms of work on the 100 KB corpus), and timed the two
sides in separate blocks. It now preallocates both outputs once, calibrates
reps to 200 ms rounds, interleaves the two encoders per rep, and reports
median and best of 7 rounds. The old numbers overstated K1 on the small
and incompressible corpora by 10-30% (the C side was paying for the arena
allocation); the new ones are what the tables above show.

### 4. `min-match` as a static parameter (q1)

The two-pass encoder's minimum match length is 4 or 6 depending on the
hash table size, and the C gets it for free as a constant because it
bakes `table_bits` into ten `Impl##B` copies. The K1 port carried
`min-match` as a runtime argument, so `is-match-2p`, the hash functions
and the table update all branched on it inside the scan loop (`cmp x7,
#0x4; b.ne` twice per candidate). `create-commands[mm: static size]`
with two instances, chosen once per fragment, removes those branches:

```k1 path=dogfood/brotli/twopass.k1
    if table-bits <= 15 {
      create-commands[4](input, block-size, input-size, base-ip, table, table-bits, sink.&)
    } else {
      create-commands[6](input, block-size, input-size, base-ip, table, table-bits, sink.&)
    }
```

Measured: `typer.rs` q1 ratio 0.977/0.973/0.977 to 0.998/0.996/0.988,
`random-300k` q1 0.991 to 1.007, `cycle-251` q1 1.025 to 1.044; q0 is
untouched by this change and stayed within its noise band. Two inlined
copies of a 2 KB scan loop did not trigger the layout penalty that four
copies of the 6 KB q0 function did (next section).

### Tried and reverted

- Baking `table-bits` into the q0 hot function with a static parameter
  (`fn compress-fragment-impl[tb: static size]`, four instances dispatched
  by a match), the exact shape of the C's `BAKE_METHOD_PARAM_`. It folds
  the hash shift to an immediate as intended, but measured 2-4% slower on
  every q0 corpus, and the loop body was otherwise instruction-identical:
  four copies of a 6 KB function change layout and alignment, and this
  loop is sensitive to that.
- `fn(inline)` on `sort-huffman-tree-items` so its `some fn` comparator
  stops being an indirect `blr` per comparison. Neutral on q1 and random,
  and 2-3% slower on `typer.rs` q0 for the same layout reason. The
  indirect call remains a real inefficiency, see below.

## Compile time

`hyperfine --warmup 1 --runs 5`, from `run.sh`, on the same loaded
machine (each K1 build lexes, parses, typechecks and codegens the module
with `core` and `std` from source, links, and writes the executable; the
`cc` line compiles the three C sources the port covers to object files
and does not link):

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `k1 --optimize --cache false build dogfood/brotli` | 288.7 ± 14.4 | 267.9 | 305.7 | 2.43 ± 0.39 |
| `k1 --cache false build dogfood/brotli` | 118.6 ± 18.3 | 92.3 | 139.7 | 1.00 |
| `cc -O2 -c compress_fragment.c compress_fragment_two_pass.c encode.c` | 1073.0 ± 66.3 | 983.9 | 1144.7 | 9.05 ± 1.50 |

A direct `time` of the optimized build: 0.22 s wall, 0.79 s user (LLVM
`O3` on the parallel codegen units); the binary's mtime changes, so this
is a full build, not a cache hit. The 7 s a first `k1 --optimize` build
took at the start of this session was the machine, not the compiler: 10%
CPU, the rest waiting.

Lines of code: the K1 module is 2,020 lines of encoder (`bits.k1`,
`encoder.k1`, `huffman.k1`, `twopass.k1`, `tables.k1`) plus the 207-line
parity/bench harness and the 180-line manifest whose setup step
regenerates `tables.k1` from the vendored C. The C it ports is
`compress_fragment.c` (790), `compress_fragment_two_pass.c` (647) and the
parts of `entropy_encode.c` (501), `brotli_bit_stream.c` (1,365),
`encode.c` (2,046), `write_bits.h`, `find_match_length.h`, `fast_log.c`
and `entropy_encode_static.h` (548, the static tables) they use.

## Where the remaining gap is

After these changes K1 is at parity with `cc -O2` on the text corpus at
both qualities and ahead on the synthetic ones, except `random-300k` q0
(about 0.96). What is left, from the machine code:

**q0 scan loop spills.** The hot loop of `compress-fragment-impl` (find
the next 5-byte match) is instruction-for-instruction the C's loop plus
six instructions: `shift`, `table` and `base-ip` are reloaded from the
stack every iteration and the folded hash multiplier `0x1E35A7BD << 24`
is rebuilt with `mov`+`movk`+`movk`. The C's `Impl15` keeps all of them
in registers.

```text
100003f68: mov   x26, x23                  ; ip = next-ip
100003f6c: mov   x23, x13                  ; next-ip += skip >> 5
100003f70: mov   x13, #0xbd000000          ; hash multiplier, rebuilt every iteration
100003f74: movk  x13, #0x35a7, lsl #32
100003f78: movk  x13, #0x1e, lsl #48
100003f7c: mul   x13, x14, x13
100003f80: ldr   x14, [sp, #0xa0]          ; shift, from the stack
100003f84: lsr   x13, x13, x14
100003f88: ldr   x14, [x23]                ; next 8 bytes
100003f8c: add   x17, x26, x15             ; ip - last-distance
100003f90: ldr   w0, [x26]
100003f94: ldr   w1, [x17]
100003f98: cmp   w0, w1
100003f9c: b.ne  0x100003fb4
100003fa0: tbz   x15, #0x3f, 0x100003fb4
100003fa4: ldrb  w0, [x26, #0x4]
100003fa8: ldrb  w1, [x17, #0x4]
100003fac: cmp   w0, w1
100003fb0: b.eq  0x100003f34
100003fb4: ldp   x1, x0, [sp, #0xa8]       ; table, base-ip, from the stack
100003fb8: ldrsw x17, [x1, x13, lsl #2]    ; candidate = base-ip + table[hash]
100003fbc: add   x17, x0, x17
100003fc0: sub   w0, w26, w0
100003fc4: str   w0, [x1, x13, lsl #2]     ; table[hash] = ip - base-ip
100003fc8: ldr   w13, [x26]
100003fcc: ldr   w0, [x17]
100003fd0: cmp   w13, w0
100003fd4: b.ne  0x100003f54
```

The function has the same live values as the C's (the arena's five
prefix-code arrays, the block bookkeeping, the writer, the scan state),
so this is LLVM's register allocation on a 6 KB function, not extra work
in the K1 source. It is also why every layout-changing experiment moved
q0 by 2-4% in either direction. The by-value writer freed the stack slots
that used to hold `pos`; the next win is getting these three out of the
loop.

**`random-300k` q0.** The skip heuristic makes the scan loop cheap here
(about 4,400 iterations per 300 KB); the time is memcpy for the
uncompressed-metablock rewrite, the hash-table memset, and the per-block
Huffman work: histogram, `sort-huffman-tree-items` on up to 256 symbols,
tree build, `store-huffman-tree` over the 704-symbol command alphabet. In
that sort the K1 comparator is a `some fn` parameter that compiles to an
indirect `blr` per comparison, while clang inlines
`SortHuffmanTreeItems` into each caller and then the comparator into it.
Forcing the sort inline fixed the asm but did not measure as a win here
(see "Tried and reverted"), so the 4% on this corpus is unexplained at
the instruction level; `sample` cannot attribute below function
granularity and `xctrace` needs a full Xcode install.

**Bounds checks that the C does not do.** `k-num-extra-bits.[code]` and
`k-insert-offset.[code]` in the q1 command loop are indexed through the
checked `.[i]` on a global array (`cmp w21, #0x80; b.hs` per command).
Two instructions per command, kept for the safety; `index-unchecked`
would remove them.

### Compiler asks

Things that the language author could do in the compiler; each was
worked around in `dogfood/brotli`.

1. **No way to say `restrict`.** A `*bit-writer` whose address
   escapes into any out-of-line call forces a reload of `bw.pos` after
   every store through `bw.storage`; the IR pattern is in section 1
   above. The C encoder is written entirely against `BROTLI_RESTRICT`
   pointers. Value threading works but costs a `let bw = bw` shadow, `bw
   = bw.write-bits(...)` at every emit, and a struct return where a
   function also produces a number. Either `noalias` on reference parameters
   under a declared rule, or a `unique`/`restrict` qualifier the codegen
   turns into `noalias`, would let the pointer form perform.
2. **`--emit-llvm` builds a different program.** With `--emit-llvm` the
   whole module is one codegen unit: the emitted `.ll` has 31 defines and
   the binary 11 `brotli.` symbols, versus 33 K1 symbols and a separate
   `compress-fragment-impl` in the normal build. Reading the IR of the
   thing you are not running cost an hour here. Emitting the per-unit IR
   of the normal build, or a note in `--help`, would fix it.
3. **Aggregates cross calls as `[2 x i64]` with the pointer half as an
   integer.** `define ... @build-and-store-huffman-tree-fast([2 x i64]
   %tree, [2 x i64] %histogram, ...)` then `%6 = inttoptr i64
   %histogram.fca.0.extract to ptr`. Inside an inlined body InstCombine
   folds it away, but across a real call LLVM cannot attach `noalias`,
   `nonnull` or `dereferenceable` to a buffer's data pointer and treats
   the `inttoptr` result as aliasing everything. Passing `{ptr, i64}`
   keeps provenance and enables the attributes.
4. **No `fn(noinline)`.** The C isolates each `Impl##B` behind
   `BROTLI_NOINLINE`; here inlining is LLVM's call, and every experiment
   that changed function sizes changed q0 by a few percent through
   layout alone. A `noinline` modifier (and an alignment hint for hot
   loops) would make such experiments controllable.
