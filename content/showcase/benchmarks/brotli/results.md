# Brotli encoder benchmark

Measured 2026-09-18T02:19:16Z.

Fresh run with the IR optimizer working tree; provenance: ../run-info.json.


- commit: ff40f0bf
- machine: Apple M3 Max, 14 cores, 38 GB
- k1: k1 0.1.0; cc: Apple clang version 21.0.0 (clang-2100.1.1.101)
- load average at start: 7.17 6.46 6.47

Each run checks byte identity with C and streaming round trips. K1 and C are interleaved per repetition; throughput is the median of seven rounds, with the best round in parentheses.

### bench run 1

| corpus | q | K1 MB/s (best) | C MB/s (best) | K1/C | bytes in -> out | reps x rounds |
|---|---|---|---|---|---|---|
| cycle-251 | q0 | 8661.9 (8718.8) | 8960.1 (9020.2) | 0.967 | 100000 -> 344 | 3397 x 7 |
| zeros-1mb | q0 | 4940.0 (5068.6) | 5196.6 (5350.7) | 0.951 | 1048576 -> 724 | 472 x 7 |
| random-300k | q0 | 7020.6 (7038.2) | 7736.1 (7799.8) | 0.908 | 300000 -> 300008 | 1094 x 7 |
| typer.rs | q0 | 645.4 (655.8) | 653.2 (662.3) | 0.988 | 751806 -> 188053 | 82 x 7 |
| cycle-251 | q1 | 7677.1 (7724.2) | 8114.2 (8184.0) | 0.946 | 100000 -> 285 | 2739 x 7 |
| zeros-1mb | q1 | 17691.0 (18046.3) | 18452.2 (18715.2) | 0.959 | 1048576 -> 183 | 1178 x 7 |
| random-300k | q1 | 2355.8 (2387.9) | 2416.9 (2444.4) | 0.975 | 300000 -> 300008 | 484 x 7 |
| typer.rs | q1 | 406.5 (412.0) | 412.8 (421.5) | 0.985 | 751806 -> 175534 | 50 x 7 |

### bench run 2

| corpus | q | K1 MB/s (best) | C MB/s (best) | K1/C | bytes in -> out | reps x rounds |
|---|---|---|---|---|---|---|
| cycle-251 | q0 | 8594.5 (8598.8) | 8917.2 (8962.3) | 0.964 | 100000 -> 344 | 4532 x 7 |
| zeros-1mb | q0 | 4524.8 (4570.3) | 5263.3 (5306.9) | 0.860 | 1048576 -> 724 | 468 x 7 |
| random-300k | q0 | 7005.5 (7091.9) | 7587.0 (7706.1) | 0.923 | 300000 -> 300008 | 1140 x 7 |
| typer.rs | q0 | 651.6 (667.9) | 658.5 (668.3) | 0.990 | 751806 -> 188053 | 78 x 7 |
| cycle-251 | q1 | 7716.9 (7773.8) | 8207.9 (8233.1) | 0.940 | 100000 -> 285 | 1577 x 7 |
| zeros-1mb | q1 | 17799.5 (17811.9) | 18588.5 (18654.2) | 0.958 | 1048576 -> 183 | 1303 x 7 |
| random-300k | q1 | 2347.2 (2361.2) | 2392.8 (2410.4) | 0.981 | 300000 -> 300008 | 462 x 7 |
| typer.rs | q1 | 414.3 (418.4) | 420.9 (424.2) | 0.984 | 751806 -> 175534 | 51 x 7 |

### bench run 3

| corpus | q | K1 MB/s (best) | C MB/s (best) | K1/C | bytes in -> out | reps x rounds |
|---|---|---|---|---|---|---|
| cycle-251 | q0 | 8561.7 (8618.2) | 8906.9 (9008.3) | 0.961 | 100000 -> 344 | 5223 x 7 |
| zeros-1mb | q0 | 4914.6 (4925.2) | 4864.2 (4886.1) | 1.010 | 1048576 -> 724 | 438 x 7 |
| random-300k | q0 | 6992.6 (7011.9) | 7730.9 (7746.6) | 0.904 | 300000 -> 300008 | 1110 x 7 |
| typer.rs | q0 | 659.8 (669.0) | 671.8 (677.2) | 0.982 | 751806 -> 188053 | 78 x 7 |
| cycle-251 | q1 | 7656.5 (7712.0) | 8141.5 (8186.9) | 0.940 | 100000 -> 285 | 2054 x 7 |
| zeros-1mb | q1 | 17461.6 (17623.2) | 18200.5 (18418.6) | 0.959 | 1048576 -> 183 | 1181 x 7 |
| random-300k | q1 | 2345.3 (2384.7) | 2399.6 (2436.7) | 0.977 | 300000 -> 300008 | 508 x 7 |
| typer.rs | q1 | 406.4 (423.1) | 413.0 (430.9) | 0.984 | 751806 -> 175534 | 51 x 7 |

### compile time

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `k1 --optimize build dogfood/brotli` | 319.5 ± 144.9 | 247.7 | 578.3 | 2.51 ± 1.14 |
| `k1 build dogfood/brotli` | 127.2 ± 1.4 | 125.1 | 129.0 | 1.00 |
| `cc -O2 -c compress_fragment.c compress_fragment_two_pass.c encode.c` | 826.9 ± 39.0 | 784.0 | 867.2 | 6.50 ± 0.32 |

