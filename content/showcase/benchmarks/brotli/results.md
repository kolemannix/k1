# Brotli encoder benchmark

Measured 2026-10-13T18:20:30Z.


- commit: 18199ac5
- machine: Apple M3 Max, 14 cores, 38 GB
- k1: k1 0.1.0; cc: Apple clang version 21.0.0 (clang-2100.3.34.2)
- load average at start: 2.45 2.20 2.48

Each run checks byte identity with C and streaming round trips. k1 and C are interleaved per repetition; throughput is the median of seven rounds, with the best round in parentheses.

### bench run 1

| corpus | q | k1 MB/s (best) | C MB/s (best) | k1/C | bytes in -> out | reps x rounds |
|---|---|---|---|---|---|---|
| cycle-251 | q0 | k1 (best) | c (best) | ratio | 1.041 -> -> | bytes x x |
| zeros-1mb | q0 | k1 (best) | c (best) | ratio | 0.997 -> -> | bytes x x |
| random-300k | q0 | k1 (best) | c (best) | ratio | 0.951 -> -> | bytes x x |
| typer.rs | q0 | k1 (best) | c (best) | ratio | 1.006 -> -> | bytes x x |
| cycle-251 | q1 | k1 (best) | c (best) | ratio | 1.030 -> -> | bytes x x |
| zeros-1mb | q1 | k1 (best) | c (best) | ratio | 0.995 -> -> | bytes x x |
| random-300k | q1 | k1 (best) | c (best) | ratio | 0.943 -> -> | bytes x x |
| typer.rs | q1 | k1 (best) | c (best) | ratio | 1.009 -> -> | bytes x x |
| cycle-251 | q2 | k1 (best) | c (best) | ratio | 1.790 -> -> | bytes x x |
| zeros-1mb | q2 | k1 (best) | c (best) | ratio | 2.411 -> -> | bytes x x |
| random-300k | q2 | k1 (best) | c (best) | ratio | 0.510 -> -> | bytes x x |
| typer.rs | q2 | k1 (best) | c (best) | ratio | 0.824 -> -> | bytes x x |
| cycle-251 | q3 | k1 (best) | c (best) | ratio | 3.098 -> -> | bytes x x |
| zeros-1mb | q3 | k1 (best) | c (best) | ratio | 3.251 -> -> | bytes x x |
| random-300k | q3 | k1 (best) | c (best) | ratio | 0.488 -> -> | bytes x x |
| typer.rs | q3 | k1 (best) | c (best) | ratio | 0.765 -> -> | bytes x x |
| cycle-251 | q4 | k1 (best) | c (best) | ratio | 2.077 -> -> | bytes x x |
| zeros-1mb | q4 | k1 (best) | c (best) | ratio | 1.405 -> -> | bytes x x |
| random-300k | q4 | k1 (best) | c (best) | ratio | 0.638 -> -> | bytes x x |
| typer.rs | q4 | k1 (best) | c (best) | ratio | 0.891 -> -> | bytes x x |
| cycle-251 | q5 | k1 (best) | c (best) | ratio | 1.265 -> -> | bytes x x |
| zeros-1mb | q5 | k1 (best) | c (best) | ratio | 3.422 -> -> | bytes x x |
| random-300k | q5 | k1 (best) | c (best) | ratio | 0.639 -> -> | bytes x x |
| typer.rs | q5 | k1 (best) | c (best) | ratio | 1.081 -> -> | bytes x x |
| cycle-251 | q6 | k1 (best) | c (best) | ratio | 1.116 -> -> | bytes x x |
| zeros-1mb | q6 | k1 (best) | c (best) | ratio | 3.419 -> -> | bytes x x |
| random-300k | q6 | k1 (best) | c (best) | ratio | 0.687 -> -> | bytes x x |
| typer.rs | q6 | k1 (best) | c (best) | ratio | 1.100 -> -> | bytes x x |
| cycle-251 | q7 | k1 (best) | c (best) | ratio | 0.364 -> -> | bytes x x |
| zeros-1mb | q7 | k1 (best) | c (best) | ratio | 1.714 -> -> | bytes x x |
| random-300k | q7 | k1 (best) | c (best) | ratio | 0.404 -> -> | bytes x x |
| typer.rs | q7 | k1 (best) | c (best) | ratio | 0.906 -> -> | bytes x x |
| cycle-251 | q8 | k1 (best) | c (best) | ratio | 0.333 -> -> | bytes x x |
| zeros-1mb | q8 | k1 (best) | c (best) | ratio | 1.718 -> -> | bytes x x |
| random-300k | q8 | k1 (best) | c (best) | ratio | 0.322 -> -> | bytes x x |
| typer.rs | q8 | k1 (best) | c (best) | ratio | 0.888 -> -> | bytes x x |
| cycle-251 | q9 | k1 (best) | c (best) | ratio | 0.138 -> -> | bytes x x |
| zeros-1mb | q9 | k1 (best) | c (best) | ratio | 2.053 -> -> | bytes x x |
| random-300k | q9 | k1 (best) | c (best) | ratio | 0.237 -> -> | bytes x x |
| typer.rs | q9 | k1 (best) | c (best) | ratio | 0.844 -> -> | bytes x x |
| cycle-251 | q10 | k1 (best) | c (best) | ratio | 0.946 -> -> | bytes x x |
| zeros-1mb | q10 | k1 (best) | c (best) | ratio | 0.936 -> -> | bytes x x |
| random-300k | q10 | k1 (best) | c (best) | ratio | 1.086 -> -> | bytes x x |
| typer.rs | q10 | k1 (best) | c (best) | ratio | 0.983 -> -> | bytes x x |
| cycle-251 | q11 | k1 (best) | c (best) | ratio | 0.935 -> -> | bytes x x |
| zeros-1mb | q11 | k1 (best) | c (best) | ratio | 0.956 -> -> | bytes x x |
| random-300k | q11 | k1 (best) | c (best) | ratio | 1.102 -> -> | bytes x x |
| typer.rs | q11 | k1 (best) | c (best) | ratio | 1.121 -> -> | bytes x x |
| cycle-251 | q5 | k1 (best) | c (best) | ratio | 1.506 -> -> | bytes x x |
| zeros-1mb | q5 | k1 (best) | c (best) | ratio | 1.639 -> -> | bytes x x |
| random-300k | q5 | k1 (best) | c (best) | ratio | 0.630 -> -> | bytes x x |
| typer.rs | q5 | k1 (best) | c (best) | ratio | 0.877 -> -> | bytes x x |
| cycle-251 | q7 | k1 (best) | c (best) | ratio | 1.323 -> -> | bytes x x |
| zeros-1mb | q7 | k1 (best) | c (best) | ratio | 1.373 -> -> | bytes x x |
| random-300k | q7 | k1 (best) | c (best) | ratio | 0.644 -> -> | bytes x x |
| typer.rs | q7 | k1 (best) | c (best) | ratio | 0.914 -> -> | bytes x x |
| cycle-251 | q9 | k1 (best) | c (best) | ratio | 1.287 -> -> | bytes x x |
| zeros-1mb | q9 | k1 (best) | c (best) | ratio | 1.682 -> -> | bytes x x |
| random-300k | q9 | k1 (best) | c (best) | ratio | 0.707 -> -> | bytes x x |
| typer.rs | q9 | k1 (best) | c (best) | ratio | 0.953 -> -> | bytes x x |
| cycle-251 | q6 | k1 (best) | c (best) | ratio | 0.406 -> -> | bytes x x |
| zeros-1mb | q6 | k1 (best) | c (best) | ratio | 3.325 -> -> | bytes x x |
| random-300k | q6 | k1 (best) | c (best) | ratio | 0.539 -> -> | bytes x x |
| typer.rs | q6 | k1 (best) | c (best) | ratio | 1.059 -> -> | bytes x x |

### compile time

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `k1 --optimize build examples/brotli` | 1.782 ± 0.012 | 1.762 | 1.792 | 2.40 ± 0.07 |
| `k1 build examples/brotli` | 1.429 ± 0.015 | 1.414 | 1.449 | 1.93 ± 0.06 |
| `cc -O2 -c compress_fragment.c compress_fragment_two_pass.c encode.c` | 0.743 ± 0.021 | 0.716 | 0.765 | 1.00 |

