---
title: Native speed
weight: 1
charts: [runtime, brotli]
suites: [runtime, brotli]
---

Three small programs in eight languages, each written the way that language's own programmers would write it, and a brotli encoder against the C it was ported from.

**hashmap** inserts five million keys into each language's standard table and looks them all up. **byte-scan** searches 256 MiB for single bytes and for a set of delimiters; k1's unoptimized build still beats Rust here, because the scan is written with k1's SIMD vectors and needs no autovectorizer to find them. **binary-trees** is the benchmarks-game allocator test; k1 builds each tree in an arena and resets it, where C and Rust free node by node. The C bump-arena row does what k1 does, so the gap between the two C rows is the allocator choice and the gap between C bump and k1 is the language. The faded rows are k1 without `--optimize`.

**brotli** is `examples/brotli`, a k1 port of the quality 0 and 1 encoders, timed in one process against the C library it was ported from. The output is byte-identical, and the k1 port runs at nearly the C encoder's speed on every corpus.
