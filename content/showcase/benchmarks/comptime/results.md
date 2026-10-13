# Compile-time execution benchmarks

Generated 2026-10-12 16:44 by `run.sh`. Load average at start 2.3, at end 1.8 (a busy machine inflates every number).

## Machine and toolchains

- Apple M3 Max, 14 cores, 36 GB, macOS 27.0
- k1 0.1.0
- zig 0.17.0
- Apple clang version 21.0.0 (clang-2100.3.34.2)
- clang version 21.0.0git (https://github.com/bloomberg/clang-p2996.git 0664c3f65dd5b198341412d7402b41f8e766b4b4)
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
  C++26 the Bloomberg clang-p2996 fork (`$CXX_P2996`, a wrapper baking
  in `-std=c++26 -freflection-latest -stdlib=libc++`) at `-O0`.
- reflect-serialize front-end commands: k1 `k1 --no-cache --no-optimize-ir --dump-ir check <file>`
  (`check` alone never instantiates the generic serializers, so their `#meta` never runs; `--dump-ir`
  makes it compile every function reachable from `main`, which typechecks the specializations, runs
  `#meta` and the VM and lowers to k1 IR, and then writes the IR dump, the one cost here that no
  other toolchain pays; `--no-optimize-ir` skips k1's inliner and IR passes, the middle end the
  other commands also stop before); Zig `zig build-exe -O Debug -fno-emit-bin` with the local cache
  dir deleted before every run (semantic analysis of everything reachable from `main`, including the
  `inline for` expansions, no codegen); C++ and C++26 `-fsyntax-only` (parsing, semantic analysis and
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

N generated struct types with 8 fields (a rotating mix of i32, u8, bool, string, i64, u16, u32, i8) and a JSON serializer derived per type: k1 `#meta` walking `types/schema` and emitting the writer code, Zig `@typeInfo` with `inline for`, C++ a generator-written `members<T>` specialization per struct whose `visit` calls a callback with each field name and value (clang 21 has no reflection, so this is the manual alternative; a class-template specialization rather than one `visit_members` overload per struct, which clang resolves against all N candidates at every call and so costs quadratic time: 96 s at N=4000 against 7 s), C++26 P2996 reflection (`nonstatic_data_members_of` + `template for`, no per-struct list) on the clang-p2996 fork, Rust `serde` derive through proc macros (cargo, dependencies prebuilt). Each program serializes one value per type and prints the total output length; the full build that produces it is not timed. Front end only (see Methodology): N=0 is the baseline; per type = (t(N) - t(0)) / N; the slope column is (t(N) - t(previous N)) / (N - previous N), so a slope that rises with N is a superlinear front end.

| N types | language | front end (s) | per type (ms) | slope from previous N (ms/type) | output |
|---|---|---|---|---|---|
| 0 | k1 | 0.021 ± 0.003 | - | - | `0` |
| 0 | Zig | 0.196 ± 0.001 | - | - | `0` |
| 0 | C++ | 0.111 ± 0.006 | - | - | `0` |
| 0 | C++26 (P2996) | 0.306 ± 0.012 | - | - | `0` |
| 0 | Rust | 0.076 ± 0.002 | - | - | `0` |
| 100 | k1 | 0.028 ± 0.002 | 0.07 | 0.07 | `7126` |
| 100 | Zig | 0.209 ± 0.006 | 0.12 | 0.12 | `7126` |
| 100 | C++ | 0.249 ± 0.004 | 1.38 | 1.38 | `7126` |
| 100 | C++26 (P2996) | 0.693 ± 0.037 | 3.87 | 3.87 | `7126` |
| 100 | Rust | 0.222 ± 0.008 | 1.46 | 1.46 | `7126` |
| 1000 | k1 | 0.105 ± 0.001 | 0.08 | 0.09 | `72270` |
| 1000 | Zig | 0.296 ± 0.003 | 0.10 | 0.10 | `72270` |
| 1000 | C++ | 1.466 ± 0.005 | 1.36 | 1.35 | `72270` |
| 1000 | C++26 (P2996) | 3.962 ± 0.043 | 3.66 | 3.63 | `72270` |
| 1000 | Rust | 1.404 ± 0.016 | 1.33 | 1.31 | `72270` |
| 4000 | k1 | 0.336 ± 0.003 | 0.08 | 0.08 | `292418` |
| 4000 | Zig | 0.592 ± 0.005 | 0.10 | 0.10 | `292418` |
| 4000 | C++ | 5.604 ± 0.063 | 1.37 | 1.38 | `292418` |
| 4000 | C++26 (P2996) | 14.883 ± 0.038 † | 3.64 | 3.64 | `292418` |
| 4000 | Rust | 5.534 ± 0.033 | 1.36 | 1.38 | `292418` |
| 10000 | k1 | 0.821 ± 0.010 | 0.08 | 0.08 | `732704` |
| 10000 | Zig | 1.191 ± 0.014 | 0.10 | 0.10 | `732704` |
| 10000 | C++ | 13.805 ± 0.147 † | 1.37 | 1.37 | `732704` |
| 10000 | C++26 (P2996) | 36.122 ± 0.000 † | 3.58 | 3.54 | `732704` |
| 10000 | Rust | 13.658 ± 0.049 † | 1.36 | 1.35 | `732704` |

k1 `--chatty` rows for the timed front-end command (`meta` is the metaprogram expansion, `run` the VM time behind it, `lower` the lowering to k1 IR, `untracked` the time outside traced work, which is mostly writing the IR dump):

| N types | meta (ms) | VM run (ms) | VM instructions | typecheck (ms) | lower (ms) | untracked (ms) | whole compile (ms) |
|---|---|---|---|---|---|---|---|
| 0 | 0.0 | 0.4 | 0.17 M | 3.0 | 0.7 | 1.8 | 11 |
| 100 | 1.0 | 2.8 | 1.30 M | 3.1 | 1.4 | 4.5 | 18 |
| 1000 | 9.5 | 24.7 | 11.00 M | 4.8 | 8.2 | 30.1 | 90 |
| 4000 | 37.9 | 96.9 | 43.70 M | 9.4 | 29.9 | 115.7 | 324 |
| 10000 | 95.3 | 244.9 | 109.00 M | 19.5 | 73.0 | 299.4 | 812 |

## Output verification

Every executable that was built printed the reference value for its benchmark and size.

## Limits hit

### crc32 N=8,388,608 Zig (comptime)

`zig build-exe -O Debug --cache-dir /Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/crc32/8388608/zig/comptime/zc --global-cache-dir /Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/zig-global --stack 67092480 -femit-bin=/Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/crc32/8388608/zig/comptime/crc32 /Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/crc32/8388608/zig/comptime/crc32.zig`

Exited with code -11 after 65 s with no diagnostic output.

