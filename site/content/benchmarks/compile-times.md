---
title: Compile times
weight: 2
charts: [compile-times]
suites: [compile-times]
---

The same generated, application-shaped program in each language, typechecked by each compiler. k1 rebuilds core and std from source on every run; the warm-cache row restores them from the module cache instead. The hello-world rows measure each toolchain's floor. The brotli rows typecheck a real program, the whole encoder: k1 as one module, and the C encoder's four translation units either through one clang process or four in parallel, which is what a build system would do.
