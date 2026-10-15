# k1 by example

This directory contains various examples and benchmark results designed to show what k1 is capable of.

This is not a tutorial, formal documentation, or a language walkthrough. It's more of a recorded test drive.

The examples are deliberately fun, provocative, and a little ridiculous, designed to push and reveal the limits.

## Topics

1. [`#static` execution](sections/01-static.md): literal types, a CRC32 table
   checked by its type, a palette designed in OKLCH and shipped as bytes.
3. [Calling C at compile time](sections/03-comptime-ffi.md): a libc build
   stamp, and a font atlas rasterized by stb_truetype inside the compiler,
   baked into a binary that never links stb.
4. [Reflection](sections/04-reflection.md): `types/schema[t]` is a plain value.
   JSON, `show`, enum parsing and padding warnings, all as ordinary library code.
5. [Macros](sections/05-macros.md): arguments arrive as `code` with spans, so
   errors land on your source. State machines, bitfields, struct-of-arrays, HTML.
6. [A regex engine at compile time](sections/06-regex.md): NFA, DFA,
   minimization, and a matcher whose states are match arms.
7. [Generators](sections/07-generators.md): gperf as a macro, and a Brainfuck
   compiler checked against a literal type.
8. [Building types at compile time](sections/08-types.md): types synthesized from schemas,
   static parameters, predicate bounds, `never` and zero-sized types.
9. [The runtime model](sections/09-model.md): abilities, closures that capture
   only what you name, context parameters, `.try` for C status codes, arenas,
   exhaustive matching.
10. [Systems programming](sections/10-systems.md): SIMD, threads, atomics, FFI
    without a bindings file, a k1 library called from C, hot reload, wasm and
    bare metal.

## Performance

Every suite has an unattended `run.sh` that regenerates its `results.md`
(machine, toolchains, methodology, numbers). [run-info.json](benchmarks/run-info.json) records the compiler revision and
binary hash.

`python3 content/showcase/benchmarks/chart.py` redraws each suite's `chart.svg`
from those, in the site's theme; the site inlines them above each results page.

- [Runtime classics](benchmarks/runtime/results.md): binary-trees, hashmap,
  byte-scan. Whole-process timings include JVM/CLR startup; the report has
  steady-state numbers, peak RSS, and the allocator and library choices each
  language made.
- [Compile times](benchmarks/compile-times/results.md): generated
  application-shaped programs at 10, 100 and 300 units, hello world, and
  Brotli. k1 rebuilds core and std from source (`--no-cache`); cached rows are
  separate.
- [Compile-time execution](benchmarks/comptime/results.md): sieve, CRC32,
  formatting, and generated JSON serializers, against other languages'
  compile-time evaluators. Timeouts and failures stay in the graph.
- [Brotli](benchmarks/brotli/results.md): the k1 encoder (`modules/brotli`, quality 0 to 11)
  against the C encoder in one process, byte-identical output.
  ([earlier investigation](benchmarks/brotli.md))
- [Integer to string](benchmarks/int-strings/): Daniel Lemire's
  strings-per-second loop against clang's `std::to_string`; the journey from the
  naive `show()` to `spill-string` is a [blog post](../../site/content/blog/417-million-strings-per-second-in-k1.md).

Compiler time, emitted-program runtime and compile-time VM execution are
separate measurements; a win in one says nothing about the others.

## Provenance

Every example runs, and every code block in `sections/` is checked verbatim
against its source file. Compiler output was captured from those runs on an
Apple M3 Max, macOS 26.6.

```bash
./content/showcase/check.sh
```
