# Compile-time execution benchmarks

> **Fresh run with the IR optimizer working tree; provenance: ../run-info.json.**

Generated 2026-09-17 22:19 by `run.sh`. Load average at start 15.8, at end 7.2 (a busy machine inflates every number).

## Machine and toolchains

- Apple M3 Max, 14 cores, 36 GB, macOS 26.6.2
- k1 0.1.0
- zig 0.14.0
- Apple clang version 21.0.0 (clang-2100.1.1.101)
- clang version 21.0.0git (https://github.com/bloomberg/clang-p2996.git 0664c3f65dd5b198341412d7402b41f8e766b4b4)
- rustc 1.98.1 (48a229cea 2026-09-01)
- cargo 1.98.1 (797e8a9bc 2026-08-05)
- hyperfine 1.19.0

## Methodology

- Every cell is a full compile to an executable, timed with `hyperfine --warmup 1 --min-runs 5`
  (mean ± standard deviation in seconds). A cell whose first compile took more than 60 s is
  timed with `--runs 2` instead and marked †. A first compile that exceeds 300 s is a timeout;
  larger sizes of that language are then not attempted.
- Commands: K1 `k1 --cache false build <file>`; Zig `zig build-exe -O Debug -target aarch64-macos`
  with the local cache dir deleted before every run (the global cache keeps compiler_rt; the
  explicit target works around Zig 0.14.0 failing to find libSystem on this macOS);
  C++ `clang++ -std=c++23 -O0 -fconstexpr-steps=2000000000`; Rust `rustc -C opt-level=0`
  (`#![allow(long_running_const_eval)]` in the source, `--cfg comptime` selects the comptime variant);
  Rust reflect `cargo build` with dependencies prebuilt and `src/main.rs` touched before every run;
  C++26 the Bloomberg clang-p2996 fork (`$CXX_P2996`, a wrapper baking
  in `-std=c++26 -freflection-latest -stdlib=libc++`) at `-O0`.
- Compiles run in their own process group and a timeout kills the whole group, so no compiler
  outlives the harness.
- Control: the same source with the work moved to runtime (K1 `#static` dropped, Zig `comptime`
  dropped, C++ `constexpr` dropped so clang cannot fold it, Rust the `const` item removed). The
  compile-time cost is the difference of the two means; throughput divides the benchmark's work
  by that difference. K1 also reports what the compiler measures itself (`--chatty true`): the
  time spent running bytecode in the compile-time VM and the number of VM instructions executed.
- Every executable is run once (`ulimit -s 65520`, and `--stack 67092480` for Zig whose
  linked-in main-thread stack size ignores the rlimit, so the runtime controls can hold their
  arrays on the stack) and its output compared with a Python reference; mismatches are flagged.

## sieve

Sieve of Eratosthenes below N at compile time, collecting the primes into an array; the runtime prints the count and the last prime. Work = outer iterations plus composite marks.

| N | language | comptime build (s) | runtime-control build (s) | comptime cost (s) | throughput | output |
|---|---|---|---|---|---|---|
| 10,000 | K1 | 0.083 ± 0.003 | 0.083 ± 0.004 | 0.000 | 91.8 M sieve steps/s | `1229 9973` |
| 10,000 | Zig | 1.222 ± 0.009 | 0.714 ± 0.012 | 0.507 | 53.2 k sieve steps/s | `1229 9973` |
| 10,000 | C++ | 0.155 ± 0.009 | 0.112 ± 0.006 | 0.043 | 624.1 k sieve steps/s | `1229 9973` |
| 10,000 | Rust | 0.136 ± 0.008 | 0.105 ± 0.004 | 0.030 | 886.5 k sieve steps/s | `1229 9973` |
| 100,000 | K1 | 0.098 ± 0.005 | 0.086 ± 0.004 | 0.012 | 24.4 M sieve steps/s | `9592 99991` |
| 100,000 | Zig | 47.508 ± 0.188 | 0.696 ± 0.005 | 46.812 | 6.3 k sieve steps/s | `9592 99991` |
| 100,000 | C++ | 0.707 ± 0.236 | 0.116 ± 0.003 | 0.592 | 495.4 k sieve steps/s | `9592 99991` |
| 100,000 | Rust | 0.391 ± 0.007 | 0.104 ± 0.003 | 0.288 | 1.0 M sieve steps/s | `9592 99991` |
| 1,000,000 | K1 | 0.229 ± 0.003 | 0.083 ± 0.002 | 0.145 | 21.5 M sieve steps/s | `78498 999983` |
| 1,000,000 | Zig | compile error | 1.159 ± 0.266 | - | - | - |
| 1,000,000 | C++ | 6.050 ± 0.056 | 0.132 ± 0.026 | 5.919 | 527.5 k sieve steps/s | `78498 999983` |
| 1,000,000 | Rust | 3.188 ± 0.042 | 0.139 ± 0.020 | 3.049 | 1.0 M sieve steps/s | `78498 999983` |

K1 compile-time VM, as reported by `k1 --chatty true` for the comptime variant:

| N | VM run (ms) | VM instructions | VM instr/s | typecheck (ms) | IR optimization (ms) | whole compile (ms) |
|---|---|---|---|---|---|---|
| 10,000 | 1.5 | 0.5 M | 331.3 M instr/s | 2.7 | 4.1 | 73 |
| 100,000 | 14.5 | 5.1 M | 350.8 M instr/s | 2.6 | 4.1 | 86 |
| 1,000,000 | 147.6 | 52.7 M | 357.1 M instr/s | 2.7 | 4.1 | 225 |

## crc32

Generate the CRC-32 table (polynomial 0xEDB88320), generate B pseudo-random bytes with xorshift32, and checksum them, all at compile time; the runtime prints the checksum. Work = bytes.

| N | language | comptime build (s) | runtime-control build (s) | comptime cost (s) | throughput | output |
|---|---|---|---|---|---|---|
| 64 KB | K1 | 0.111 ± 0.038 | 0.091 ± 0.004 | 0.020 | 3.3 M bytes/s | `3373053292` |
| 64 KB | Zig | 10.980 ± 0.343 | 0.738 ± 0.002 | 10.242 | 6.4 k bytes/s | `3373053292` |
| 64 KB | C++ | 0.469 ± 0.006 | 0.127 ± 0.002 | 0.342 | 191.7 k bytes/s | `3373053292` |
| 64 KB | Rust | 0.528 ± 0.003 | 0.116 ± 0.007 | 0.411 | 159.3 k bytes/s | `3373053292` |
| 1 MB | K1 | 0.307 ± 0.003 | 0.086 ± 0.002 | 0.221 | 4.8 M bytes/s | `2058353098` |
| 1 MB | Zig | timed out (> 300 s) | 0.697 ± 0.006 | - | - | - |
| 1 MB | C++ | 6.152 ± 0.115 | 0.126 ± 0.004 | 6.026 | 174.0 k bytes/s | `2058353098` |
| 1 MB | Rust | 6.308 ± 0.044 | 0.115 ± 0.006 | 6.193 | 169.3 k bytes/s | `2058353098` |
| 8 MB | K1 | 1.818 ± 0.015 | 0.083 ± 0.002 | 1.735 | 4.8 M bytes/s | `1790965047` |
| 8 MB | Zig | not attempted | 0.699 ± 0.007 | - | - | - |
| 8 MB | C++ | 52.301 ± 2.591 | 0.128 ± 0.004 | 52.173 | 160.8 k bytes/s | `1790965047` |
| 8 MB | Rust | 50.497 ± 0.774 | 0.120 ± 0.005 | 50.376 | 166.5 k bytes/s | `1790965047` |

K1 compile-time VM, as reported by `k1 --chatty true` for the comptime variant:

| N | VM run (ms) | VM instructions | VM instr/s | typecheck (ms) | IR optimization (ms) | whole compile (ms) |
|---|---|---|---|---|---|---|
| 64 KB | 14.3 | 5.0 M | 348.7 M instr/s | 2.5 | 4.1 | 86 |
| 1 MB | 224.2 | 78.7 M | 351.0 M instr/s | 3.2 | 4.1 | 295 |
| 8 MB | 1716.1 | 629.2 M | 366.6 M instr/s | 2.5 | 3.9 | 1784 |

## strbuild

Format the integers 0..N in decimal with `,` separators into one string at compile time, then take its length and FNV-1a checksum; the runtime prints both. K1 uses `string-builder` and its `${}` formatter inside `#static`, Zig a fixed `[N * 8]u8` buffer and `std.fmt.formatIntBuf`, C++ a transient `constexpr std::string` with hand-written digit formatting (`std::to_string` is not constexpr), Rust a fixed `[u8; N * 8]` array with hand-written digits (no heap and no formatting machinery in const eval). Work = numbers formatted.

| N | language | comptime build (s) | runtime-control build (s) | comptime cost (s) | throughput | output |
|---|---|---|---|---|---|---|
| 10,000 | K1 | 0.108 ± 0.002 | 0.084 ± 0.001 | 0.024 | 421.3 k numbers/s | `48889 2155414189` |
| 10,000 | Zig | 7.367 ± 0.071 | 0.750 ± 0.010 | 6.617 | 1.5 k numbers/s | `48889 2155414189` |
| 10,000 | C++ | 1.215 ± 0.004 | 0.187 ± 0.010 | 1.028 | 9.7 k numbers/s | `48889 2155414189` |
| 10,000 | Rust | 0.420 ± 0.009 | 0.126 ± 0.005 | 0.294 | 34.0 k numbers/s | `48889 2155414189` |
| 100,000 | K1 | 0.322 ± 0.003 | 0.087 ± 0.002 | 0.236 | 424.0 k numbers/s | `588889 676220933` |
| 100,000 | Zig | timed out (> 300 s) | 0.767 ± 0.010 | - | - | - |
| 100,000 | C++ | 13.808 ± 0.135 | 0.182 ± 0.005 | 13.626 | 7.3 k numbers/s | `588889 676220933` |
| 100,000 | Rust | 3.740 ± 0.020 | 0.120 ± 0.007 | 3.619 | 27.6 k numbers/s | `588889 676220933` |
| 1,000,000 | K1 | 2.724 ± 0.018 | 0.091 ± 0.012 | 2.633 | 379.8 k numbers/s | `6888889 3396009285` |
| 1,000,000 | Zig | not attempted | 0.763 ± 0.012 | - | - | - |
| 1,000,000 | C++ | 178.802 ± 2.997 † | 0.444 ± 0.266 | 178.358 | 5.6 k numbers/s | `6888889 3396009285` |
| 1,000,000 | Rust | 42.337 ± 0.557 | 0.267 ± 0.127 | 42.071 | 23.8 k numbers/s | `6888889 3396009285` |

K1 compile-time VM, as reported by `k1 --chatty true` for the comptime variant:

| N | VM run (ms) | VM instructions | VM instr/s | typecheck (ms) | IR optimization (ms) | whole compile (ms) |
|---|---|---|---|---|---|---|
| 10,000 | 21.7 | 6.7 M | 308.5 M instr/s | 3.1 | 3.9 | 92 |
| 100,000 | 239.8 | 73.3 M | 305.7 M instr/s | 3.2 | 3.8 | 312 |
| 1,000,000 | 2650.7 | 816.7 M | 308.1 M instr/s | 3.1 | 3.9 | 2728 |

## reflect-serialize

N generated struct types with 8 fields (a rotating mix of i32, u8, bool, string, i64, u16, u32, i8) and a JSON serializer derived per type: K1 `#meta` walking `types/schema` and emitting the writer code, Zig `@typeInfo` with `inline for`, C++ a template over a generator-written member list (clang 21 has no reflection, so this is the manual alternative), C++26 P2996 reflection (`nonstatic_data_members_of` + `template for`, no per-struct list) on the clang-p2996 fork, Rust `serde` derive through proc macros (cargo, dependencies prebuilt). Each program serializes one value per type and prints the total output length. N=0 is the baseline; per type = (build(N) - build(0)) / N.

| N types | language | build (s) | per type (ms) | output |
|---|---|---|---|---|
| 0 | K1 | 0.078 ± 0.003 | - | `0` |
| 0 | Zig | 0.730 ± 0.008 | - | `0` |
| 0 | C++ | 0.167 ± 0.006 | - | `0` |
| 0 | C++26 (P2996) | 0.429 ± 0.013 | - | `0` |
| 0 | Rust | 0.142 ± 0.003 | - | `0` |
| 10 | K1 | 0.089 ± 0.003 | 1.10 | `703` |
| 10 | Zig | 0.774 ± 0.015 | 4.46 | `703` |
| 10 | C++ | 0.407 ± 0.022 | 23.98 | `703` |
| 10 | C++26 (P2996) | 0.470 ± 0.013 | 4.09 | `703` |
| 10 | Rust | 0.156 ± 0.005 | 1.42 | `703` |
| 100 | K1 | 0.154 ± 0.012 | 0.76 | `7126` |
| 100 | Zig | 1.028 ± 0.022 | 2.98 | `7126` |
| 100 | C++ | 2.824 ± 0.171 | 26.56 | `7126` |
| 100 | C++26 (P2996) | 0.873 ± 0.018 | 4.44 | `7126` |
| 100 | Rust | 0.207 ± 0.004 | 0.65 | `7126` |
| 500 | K1 | 1.290 ± 0.228 | 2.42 | `36079` |
| 500 | Zig | 2.333 ± 0.019 | 3.21 | `36079` |
| 500 | C++ | 16.334 ± 0.329 | 32.33 | `36079` |
| 500 | C++26 (P2996) | 2.815 ± 0.018 | 4.77 | `36079` |
| 500 | Rust | 0.581 ± 0.134 | 0.88 | `36079` |

K1 `--chatty true` rows for the comptime work of reflect-serialize (`meta` is the metaprogram expansion, `run` the VM time behind it):

| N types | meta (ms) | VM run (ms) | VM instructions | typecheck (ms) | IR optimization (ms) | codegen (ms) | LLVM passes (ms) | link (ms) | whole compile (ms) |
|---|---|---|---|---|---|---|---|---|---|
| 0 | 0.0 | 0.1 | 0.00 M | 2.6 | 3.7 | 1.9 | 3.6 | 40.0 | 67 |
| 10 | 0.2 | 0.6 | 0.15 M | 2.5 | 6.9 | 3.1 | 8.6 | 43.3 | 78 |
| 100 | 1.0 | 5.4 | 1.50 M | 3.3 | 23.6 | 29.7 | 66.4 | 43.6 | 142 |
| 500 | 5.5 | 26.6 | 7.60 M | 4.4 | 121.8 | 114.3 | 553.0 | 421.9 | 1174 |

## Output verification

Every executable that was built printed the reference value for its benchmark and size.

## Limits hit

### sieve N=1,000,000 Zig (comptime)

`zig build-exe -O Debug -target aarch64-macos --stack 67092480 --cache-dir /Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/sieve/1000000/zig/comptime/zc --global-cache-dir /Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/zig-global -femit-bin=/Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/sieve/1000000/zig/comptime/sieve /Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/sieve/1000000/zig/comptime/sieve.zig`

Exited with code -9 after 227 s with no diagnostic output.

### crc32 N=1,048,576 Zig (comptime)

`zig build-exe -O Debug -target aarch64-macos --stack 67092480 --cache-dir /Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/crc32/1048576/zig/comptime/zc --global-cache-dir /Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/zig-global -femit-bin=/Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/crc32/1048576/zig/comptime/crc32 /Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/crc32/1048576/zig/comptime/crc32.zig`

Killed after 300 s.

### strbuild N=100,000 Zig (comptime)

`zig build-exe -O Debug -target aarch64-macos --stack 67092480 --cache-dir /Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/strbuild/100000/zig/comptime/zc --global-cache-dir /Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/zig-global -femit-bin=/Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/strbuild/100000/zig/comptime/strbuild /Users/knix/dev/k1/content/showcase/benchmarks/comptime/gen/strbuild/100000/zig/comptime/strbuild.zig`

Killed after 300 s.

