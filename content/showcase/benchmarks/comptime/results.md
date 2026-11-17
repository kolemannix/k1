# Compile-time execution benchmarks

Generated 2026-11-16 13:18 by `run.sh`. Load average at start 4.0, at end 4.0 (a busy machine inflates every number).

## Machine and toolchains

- Apple M3 Max, 14 cores, 36 GB, macOS 27.2
- k1 0.1.0
- zig 0.17.0
- Apple clang version 21.0.0 (clang-2100.3.34.2)
- g++-16 (Homebrew GCC 16.2.0) 16.2.0
- rustc 1.98.1 (48a229cea 2026-09-01)
- cargo 1.98.1 (797e8a9bc 2026-08-05)
- hyperfine 1.19.0

## Methodology

- Every cell first does a full compile to an executable, runs it and compares its output with a
  Python reference. sieve, crc32 and strbuild time that full compile; reflect-serialize times a
  front-end-only compile of the same source (no machine code, no link), because its work is
  generating code from types and in a full build LLVM codegen and linking of the N serializers
  take over 90% of the time. The untimed first compile is the warm-up; timing is then
  `hyperfine --runs n` (mean ± standard deviation in seconds) with n = 5, reduced for slow cells
  so that n runs fit in about 45 s, down to a single run; cells with fewer than 5 runs are
  marked † and results.json records the count. A compile that exceeds 300 s is a timeout;
  larger sizes of that language are then not attempted.
- Full-build commands: k1 `k1 --no-cache build <file>`; Zig `zig build-exe -O Debug`
  with the local cache dir deleted before every run (the global cache keeps compiler_rt);
  C++ `clang++ -std=c++23 -O0 -fconstexpr-steps=2000000000`; Rust `rustc -C opt-level=0`
  (`#![allow(long_running_const_eval)]` in the source, `--cfg comptime` selects the comptime variant);
  Rust reflect `cargo build` with `CARGO_INCREMENTAL=0`, dependencies prebuilt and `src/main.rs` touched before every run;
  C++26 `g++-16 -std=c++26 -freflection -O0`.
- reflect-serialize front-end commands: k1 `k1 --no-cache check <file>` (typechecks every
  requested generic instance, which runs the serializers' `#meta` and the VM; no IR is built); Zig `zig build-exe -O Debug -fno-emit-bin` with the local cache
  dir deleted before every run (semantic analysis of everything reachable from `main`, including the
  `inline for` expansions, no codegen); C++26 `-fsyntax-only` (parsing, semantic analysis and
  template instantiation); Rust `cargo check` with `CARGO_INCREMENTAL=0`, dependencies prebuilt and
  `src/main.rs` touched before every run (macro expansion, type checking, MIR building and borrow
  checking, no codegen). Each cell's front-end command also ran once untimed to confirm it succeeds.
- Compiles run in their own process group and a timeout kills the whole group, so no compiler
  outlives the harness.
- Control: the same source with the work moved to runtime (k1 `#static` dropped, Zig `comptime`
  dropped, C++ `constexpr` dropped so clang cannot fold it, Rust the `const` item removed). The
  compile-time cost is the difference of the two means; throughput divides the benchmark's work
  by that difference. k1 also reports what the compiler measures itself (`--chatty`): the
  time spent running bytecode in the compile-time VM and the number of VM instructions executed.
- Every executable is run once (`ulimit -s 65520`, and `--stack 67092480` for Zig whose
  linked-in main-thread stack size ignores the rlimit, so the runtime controls can hold their
  arrays on the stack) and its output compared with a Python reference; mismatches are flagged.

## sieve

Sieve of Eratosthenes below N at compile time, collecting the primes into an array; the runtime prints the count and the last prime. Work = outer iterations plus composite marks. Zig keeps the composite flags bit-packed in u64 words (what std::bitset would be) because a comptime array element read costs time proportional to the array's length and indexing a local array by value copies it, which is what killed the byte-array version at 1,000,000; the loops and marks are the same as the other languages.

| N | language | comptime build (s) | runtime-control build (s) | comptime cost (s) | throughput | output |
|---|---|---|---|---|---|---|
| 10,000 | k1 | 0.070 ± 0.006 | 0.074 ± 0.010 | -0.004 | - | `1229 9973` |
| 10,000 | Zig | 1.553 ± 0.025 | 1.418 ± 0.021 | 0.135 | 199.6 k sieve steps/s | `1229 9973` |
| 10,000 | C++ | 0.160 ± 0.003 | 0.119 ± 0.006 | 0.041 | 656.4 k sieve steps/s | `1229 9973` |
| 10,000 | Rust | 0.153 ± 0.010 | 0.124 ± 0.004 | 0.029 | 923.1 k sieve steps/s | `1229 9973` |
| 100,000 | k1 | 0.076 ± 0.003 | 0.070 ± 0.003 | 0.006 | 49.4 M sieve steps/s | `9592 99991` |
| 100,000 | Zig | 2.634 ± 0.057 | 1.406 ± 0.039 | 1.229 | 238.5 k sieve steps/s | `9592 99991` |
| 100,000 | C++ | 0.643 ± 0.021 | 0.117 ± 0.002 | 0.525 | 557.8 k sieve steps/s | `9592 99991` |
| 100,000 | Rust | 0.415 ± 0.007 | 0.125 ± 0.005 | 0.290 | 1.0 M sieve steps/s | `9592 99991` |
| 1,000,000 | k1 | 0.131 ± 0.001 | 0.071 ± 0.004 | 0.060 | 52.0 M sieve steps/s | `78498 999983` |
| 1,000,000 | Zig | 28.340 ± 0.000 † | 1.686 ± 0.213 | 26.654 | 117.1 k sieve steps/s | `78498 999983` |
| 1,000,000 | C++ | 6.806 ± 1.205 | 0.114 ± 0.007 | 6.692 | 466.6 k sieve steps/s | `78498 999983` |
| 1,000,000 | Rust | 3.089 ± 0.014 | 0.110 ± 0.004 | 2.979 | 1.0 M sieve steps/s | `78498 999983` |

k1 compile-time VM, as reported by `k1 --chatty` for the comptime variant:

| N | VM run (ms) | VM instructions | VM instr/s | typecheck (ms) | IR optimization (ms) | whole compile (ms) |
|---|---|---|---|---|---|---|
| 10,000 | 0.9 | 0.4 M | 413.8 M instr/s | 3.2 | 0.3 | 57 |
| 100,000 | 6.6 | 3.1 M | 472.6 M instr/s | 3.3 | 0.4 | 66 |
| 1,000,000 | 63.4 | 31.1 M | 490.5 M instr/s | 3.2 | 0.3 | 123 |

## crc32

Generate the CRC-32 table (polynomial 0xEDB88320), generate B pseudo-random bytes with xorshift32, and checksum them, all at compile time; the runtime prints the checksum. Work = bytes. The Zig buffer is zero-initialised and the checksum loop indexes the array value rather than a slice: filling an `undefined` comptime array is quadratic and element reads through a slice cost the array's length.

| N | language | comptime build (s) | runtime-control build (s) | comptime cost (s) | throughput | output |
|---|---|---|---|---|---|---|
| 64 KB | k1 | 0.079 ± 0.018 | 0.069 ± 0.002 | 0.010 | 6.3 M bytes/s | `3373053292` |
| 64 KB | Zig | 2.010 ± 0.195 | 1.368 ± 0.025 | 0.642 | 102.0 k bytes/s | `3373053292` |
| 64 KB | C++ | 0.477 ± 0.006 | 0.115 ± 0.005 | 0.362 | 181.1 k bytes/s | `3373053292` |
| 64 KB | Rust | 0.519 ± 0.005 | 0.125 ± 0.007 | 0.393 | 166.7 k bytes/s | `3373053292` |
| 1 MB | k1 | 0.141 ± 0.005 | 0.071 ± 0.003 | 0.070 | 15.0 M bytes/s | `2058353098` |
| 1 MB | Zig | 10.906 ± 0.030 † | 2.010 ± 0.968 | 8.895 | 117.9 k bytes/s | `2058353098` |
| 1 MB | C++ | 6.565 ± 0.044 | 0.109 ± 0.005 | 6.456 | 162.4 k bytes/s | `2058353098` |
| 1 MB | Rust | 6.345 ± 0.056 | 0.119 ± 0.009 | 6.226 | 168.4 k bytes/s | `2058353098` |
| 8 MB | k1 | 0.623 ± 0.005 | 0.074 ± 0.006 | 0.549 | 15.3 M bytes/s | `1790965047` |
| 8 MB | Zig | compile error | 1.413 ± 0.026 | - | - | - |
| 8 MB | C++ | 55.677 ± 0.000 † | 0.107 ± 0.005 | 55.570 | 151.0 k bytes/s | `1790965047` |
| 8 MB | Rust | 49.694 ± 0.000 † | 0.122 ± 0.008 | 49.573 | 169.2 k bytes/s | `1790965047` |

k1 compile-time VM, as reported by `k1 --chatty` for the comptime variant:

| N | VM run (ms) | VM instructions | VM instr/s | typecheck (ms) | IR optimization (ms) | whole compile (ms) |
|---|---|---|---|---|---|---|
| 64 KB | 4.9 | 3.0 M | 611.0 M instr/s | 3.6 | 0.4 | 62 |
| 1 MB | 72.1 | 45.2 M | 627.0 M instr/s | 3.1 | 0.3 | 133 |
| 8 MB | 565.3 | 360.9 M | 638.5 M instr/s | 3.1 | 0.3 | 624 |

## strbuild

Format the integers 0..N in decimal with `,` separators into one string at compile time, then take its length and FNV-1a checksum; the runtime prints both. k1 uses `string-builder` and its `${}` formatter inside `#static`, Zig a fixed `[N * 8]u8` buffer and hand-written digits (`std.fmt.printInt` costs about three times the comptime memory), C++ a transient `constexpr std::string` with hand-written digit formatting (`std::to_string` is not constexpr), Rust a fixed `[u8; N * 8]` array with hand-written digits (no heap and no formatting machinery in const eval). Work = numbers formatted.

| N | language | comptime build (s) | runtime-control build (s) | comptime cost (s) | throughput | output |
|---|---|---|---|---|---|---|
| 10,000 | k1 | 0.073 ± 0.006 | 0.068 ± 0.004 | 0.005 | 1.9 M numbers/s | `48889 2155414189` |
| 10,000 | Zig | 1.642 ± 0.007 | 1.385 ± 0.040 | 0.257 | 39.0 k numbers/s | `48889 2155414189` |
| 10,000 | C++ | 1.267 ± 0.004 | 0.154 ± 0.004 | 1.113 | 9.0 k numbers/s | `48889 2155414189` |
| 10,000 | Rust | 0.421 ± 0.010 | 0.136 ± 0.002 | 0.284 | 35.2 k numbers/s | `48889 2155414189` |
| 100,000 | k1 | 0.122 ± 0.004 | 0.068 ± 0.003 | 0.054 | 1.8 M numbers/s | `588889 676220933` |
| 100,000 | Zig | 5.489 ± 0.102 | 1.343 ± 0.011 | 4.146 | 24.1 k numbers/s | `588889 676220933` |
| 100,000 | C++ | 14.981 ± 0.272 † | 0.152 ± 0.005 | 14.829 | 6.7 k numbers/s | `588889 676220933` |
| 100,000 | Rust | 3.744 ± 0.024 | 0.121 ± 0.006 | 3.623 | 27.6 k numbers/s | `588889 676220933` |
| 1,000,000 | k1 | 0.627 ± 0.008 | 0.070 ± 0.002 | 0.557 | 1.8 M numbers/s | `6888889 3396009285` |
| 1,000,000 | Zig | 52.568 ± 0.000 † | 1.564 ± 0.093 | 51.004 | 19.6 k numbers/s | `6888889 3396009285` |
| 1,000,000 | C++ | 193.613 ± 0.000 † | 0.167 ± 0.008 | 193.446 | 5.2 k numbers/s | `6888889 3396009285` |
| 1,000,000 | Rust | 40.587 ± 0.000 † | 0.121 ± 0.005 | 40.466 | 24.7 k numbers/s | `6888889 3396009285` |

k1 compile-time VM, as reported by `k1 --chatty` for the comptime variant:

| N | VM run (ms) | VM instructions | VM instr/s | typecheck (ms) | IR optimization (ms) | whole compile (ms) |
|---|---|---|---|---|---|---|
| 10,000 | 5.2 | 2.6 M | 498.1 M instr/s | 3.1 | 0.3 | 65 |
| 100,000 | 53.1 | 27.3 M | 514.5 M instr/s | 3.1 | 0.3 | 110 |
| 1,000,000 | 571.7 | 288.3 M | 504.3 M instr/s | 3.4 | 0.3 | 632 |

## reflect-serialize

N generated struct types with 8 fields (a rotating mix of i32, u8, bool, string, i64, u16, u32, i8) and a JSON serializer derived per type: k1 `#meta` walking `types/schema` and emitting the writer code, Zig `@typeInfo` with `inline for`, C++26 reflection (`nonstatic_data_members_of` + `template for`) on GCC 16, Rust `serde` derive through proc macros (cargo, dependencies prebuilt). Each derivation also computes a schema fingerprint at compile time, FNV-1a over every `name:kind,` of the type's fields, and the serializer writes it as a leading `"schema"` key (C++26 a `consteval` walk of the members, Rust a `macro_rules!` wrapper with a `const` item, since serde's derive has no hook). Each program serializes one value per type and prints the total output length; the full build that produces it is not timed. Front end only (see Methodology): N=0 is the baseline; per type = (t(N) - t(0)) / N; the slope column is (t(N) - t(previous N)) / (N - previous N), so a slope that rises with N is a superlinear front end.

| N types | language | front end (s) | per type (ms) | slope from previous N (ms/type) | output |
|---|---|---|---|---|---|
| 0 | k1 | 0.026 ± 0.004 | - | - | `0` |
| 0 | Zig | 0.192 ± 0.011 | - | - | `0` |
| 0 | C++26 reflection | 0.249 ± 0.005 | - | - | `0` |
| 0 | Rust | 0.089 ± 0.006 | - | - | `0` |
| 100 | k1 | 0.029 ± 0.003 | 0.03 | 0.03 | `9114` |
| 100 | Zig | 0.207 ± 0.006 | 0.14 | 0.14 | `9114` |
| 100 | C++26 reflection | 0.406 ± 0.009 | 1.58 | 1.58 | `9114` |
| 100 | Rust | 0.284 ± 0.006 | 1.95 | 1.95 | `9114` |
| 1000 | k1 | 0.050 ± 0.003 | 0.02 | 0.02 | `92145` |
| 1000 | Zig | 0.348 ± 0.012 | 0.16 | 0.16 | `92145` |
| 1000 | C++26 reflection | 1.948 ± 0.044 | 1.70 | 1.71 | `92145` |
| 1000 | Rust | 1.970 ± 0.023 | 1.88 | 1.87 | `92145` |
| 4000 | k1 | 0.127 ± 0.006 | 0.03 | 0.03 | `371918` |
| 4000 | Zig | 0.808 ± 0.019 | 0.15 | 0.15 | `371918` |
| 4000 | C++26 reflection | 7.732 ± 0.133 | 1.87 | 1.93 | `371918` |
| 4000 | Rust | 8.223 ± 0.399 | 2.03 | 2.08 | `371918` |
| 10000 | k1 | 0.293 ± 0.010 | 0.03 | 0.03 | `931454` |
| 10000 | Zig | 1.723 ± 0.008 | 0.15 | 0.15 | `931454` |
| 10000 | C++26 reflection | 20.948 ± 0.210 † | 2.07 | 2.20 | `931454` |
| 10000 | Rust | 19.482 ± 0.235 † | 1.94 | 1.88 | `931454` |

k1 `--chatty` rows for the timed front-end command (`meta` is the metaprogram expansion, `run` the VM time behind it, `lower` the lowering to k1 IR, `untracked` the time outside traced work, which is mostly writing the IR dump):

| N types | meta (ms) | VM run (ms) | VM instructions | typecheck (ms) | lower (ms) | untracked (ms) | whole compile (ms) |
|---|---|---|---|---|---|---|---|
| 0 | 0.0 | 0.3 | 0.09 M | 5.5 | 0.7 | 1.0 | 15 |
| 100 | 0.4 | 2.0 | 0.73 M | 5.3 | 0.8 | 1.1 | 18 |
| 1000 | 3.2 | 16.5 | 6.40 M | 6.5 | 1.0 | 0.9 | 40 |
| 4000 | 13.2 | 66.7 | 25.50 M | 11.5 | 2.1 | 1.2 | 124 |
| 10000 | 31.7 | 163.9 | 63.60 M | 19.1 | 3.6 | 2.0 | 278 |

## Output verification

Every executable that was built printed the reference value for its benchmark and size.

## Limits hit

### crc32 N=8,388,608 Zig (comptime)

`zig build-exe -O Debug --cache-dir /Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/crc32/8388608/zig/comptime/zc --global-cache-dir /Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/zig-global --stack 67092480 -femit-bin=/Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/crc32/8388608/zig/comptime/crc32 /Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/crc32/8388608/zig/comptime/crc32.zig`

Exited with code -11 after 65 s with no diagnostic output.

