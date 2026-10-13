---
title: Compile-time execution
weight: 3
charts: [comptime]
suites: [comptime]
---

The same work done at compile time by each language's evaluator: formatting a million numbers into one string, a sieve, CRC32 over a buffer, and a serializer generated per type from reflection. The cost is the compile-time build minus a control build that does the same work at runtime, so the compiler's own time cancels out.

k1 runs `#static` on the same bytecode VM that runs every compile-time call, so it gets through tens of millions of loop steps per second where the other evaluators manage thousands to a million, and the larger sizes that time out or fail elsewhere finish in seconds.
