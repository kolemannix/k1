import json
import os
import re
import sys

gen = sys.argv[1]
env = open(os.path.join(gen, "env.txt")).read().strip()

NATIVE = ["k1", "c", "rust", "go", "zig"]

BENCHES = [
    (
        "binary-trees",
        NATIVE + ["java", "csharp", "python"],
        "Benchmarks-game binary-trees, depth 20, single-threaded: a stretch tree of depth 21, a long-lived tree of depth 20, then for each depth 4..20 step 2 build and check 2^(24-depth) trees. Prints the standard output lines; every language prints identical text.",
        {
            "k1": "An explicit `*arena` parameter and `arena.push` for every node. The long-lived tree uses the ambient arena; short-lived trees use a private arena, reset after each tree. `reset()` zeroes the used region in bulk; allocations bump the cursor. Leaves are `node.0`, null children.",
            "c": "`malloc`/`free` per node, recursive `destroy`.",
            "rust": "`Option<Box<Node>>` per node, dropped recursively.",
            "go": "`&Node{}` per node, garbage collected (the collector runs on other cores, so user time exceeds wall time).",
            "zig": "`std.heap.ArenaAllocator` over `page_allocator`, `reset(.retain_capacity)` after each tree: the arena is Zig's natural fast choice here, as it is K1's.",
            "java": "a plain `static final class Node` with `left`/`right` fields, `new Node()` per node, collected by G1 (the default GC). This is the benchmarks-game shape minus its per-depth thread pool, since every row here is single-threaded.",
            "csharp": "a `sealed class Node` with `Left`/`Right` fields, `new Node()` per node, collected by the default workstation GC.",
            "python": "nodes are `(left, right)` tuples and leaves are `(None, None)`, the benchmarks-game Python shape; reference counting frees each short-lived tree as soon as `check` returns. A `__slots__` class with an `__init__` is the more common way to write a node and took about five times as long in a side test (114 s).",
        },
    ),
    (
        "hashmap",
        NATIVE + ["java", "java-prim", "csharp", "python"],
        "Insert 5,000,000 distinct u64 keys from xorshift64 (value = insertion index), then look up 5,000,000 keys, alternating a key that was inserted with one from a different xorshift stream (a miss), summing the found values. Prints `inserted`, `found` (2,500,000) and the sum. No capacity hints anywhere: every table grows from empty.",
        {
            "k1": "`std/map[u64, u64]` (a swiss table: one control byte per bucket, 8-lane NEON group probes on arm64, murmur3 finalizer on the key). `insert`/`get` allocate in the ambient arena, where `free` is a no-op, so tables abandoned by growth are not reused; peak memory is about twice the final table.",
            "c": "A small open-addressing table written inline for this benchmark: power-of-two capacity, fibonacci hashing, linear probing, doubling at 1/2 load, parallel key and value arrays from `calloc`/`malloc`, old arrays freed on growth. It is not a library table, so it is a reference point rather than a peer.",
            "rust": "`std::collections::HashMap<u64, u64>` with the default SipHash-1-3 hasher (hashbrown underneath). FxHash or ahash would be faster; no crates are used.",
            "go": "`map[uint64]uint64` (Go 1.24+ swiss table).",
            "zig": "`std.AutoHashMap(u64, u64)` over `page_allocator`.",
            "java": "`HashMap<Long, Long>`, which is what people write. Java has no generics over primitives, so every key and every value is boxed into a heap `Long`, every entry is a separate `HashMap.Node` object holding two references and a cached hash, and every probe dereferences a pointer to compare keys. The `java (primitive table)` row separates that cost from the JIT's.",
            "java-prim": "the C row's table, written inline in Java over `long[]` keys and values: same fibonacci hash, same linear probing, same doubling at 1/2 load, same growth-by-reinsert. Nothing is boxed and nothing but the two arrays is allocated, so the gap to the `java` row is what `HashMap<Long, Long>` costs and the gap to the `c` row is HotSpot versus `clang -O2` on the same algorithm. This is not what people write; it is the control.",
            "csharp": "`Dictionary<ulong, ulong>`. The CLR specializes generics over value types, so keys and values sit unboxed in the entry array and no object is allocated per entry: the C# row is the same source shape as the Java row without the boxing.",
            "python": "a `dict`. Python integers are unbounded, so the xorshift masks each left shift to 64 bits, and keys this large are heap objects: like the Java row, every key and value is boxed, though the dict stores them in a compact array rather than one node per entry. An int's hash is its value mod 2^61 - 1. Misses are `dict.get` returning `None`, since a found value can be 0.",
        },
    ),
    (
        "byte-scan",
        NATIVE + ["java", "csharp", "python"],
        "A 256 MiB buffer of pseudo-random bytes in the range 0x40..0x5f, with a `\\n` planted every 4093 bytes and one of `,` `:` `\"` every 1,000,003 bytes. Eight repetitions of three scans: (1) `contains` of a byte that never occurs, one full pass; (2) count the newlines by repeated first-position search from the previous hit; (3) find each of the three delimiters in turn by repeated first-of-set search. Prints the counts and position sums. Fill time is included in every language.",
        {
            "k1": "(1)(2) `span[u8].contains`/`position`: `buffer/position-byte`, a `k1/simd-bytes`-wide `vector` compare loop, 16 lanes on arm64. (3) `$std/simd/first-of(delims, [',', ':', '\"'])`, a macro that generates a namespace with a `scan(s, from)` fn: one 16-lane pass computing three equality masks per chunk.",
            "c": "(1)(2) `memchr` from libSystem (hand-written NEON). (3) three `memchr` calls per segment, one per delimiter, taking the nearest hit: with delimiters this sparse each segment is scanned several times over, but still at NEON speed. `strpbrk` over a NUL-terminated copy is the C-string idiom; it is a byte-at-a-time table loop and was an order of magnitude slower in a side test.",
            "rust": "(1) `<[u8]>::contains`, which std routes to `core::slice::memchr` (a SWAR word-at-a-time loop, no SIMD). (2)(3) `iter().position(..)`, a scalar loop; the `memchr` crate would give SIMD, no crates are used.",
            "go": "(1)(2) `bytes.IndexByte` (NEON assembly in the runtime). (3) `bytes.IndexAny`, a scalar loop over an ASCII bitset.",
            "zig": "(1)(2) `std.mem.indexOfScalar`/`indexOfScalarPos`, a `@Vector` loop. (3) `std.mem.indexOfAnyPos`, a scalar nested loop.",
            "java": "hand-written `byte[]` loops for all three scans. Java's standard library has no memchr for byte arrays: `String.indexOf` is for text, `Arrays` has no search-for-value, and the Vector API is still an incubator module that needs `--add-modules jdk.incubator.vector`, so a plain loop is the honest idiom. HotSpot does not auto-vectorize a loop that exits early, so this is one byte per iteration. The fill writes 64-bit words through a little-endian `LongBuffer` view, matching the other ports.",
            "csharp": "(1)(2) `Span<byte>.IndexOf(byte)` and (3) `Span<byte>.IndexOfAny(byte, byte, byte)`. Both are idiomatic single calls and both are vectorized inside the runtime (128-bit NEON here), which is why this is the one managed row that keeps up with the native SIMD rows. The fill writes through `MemoryMarshal.Cast<byte, ulong>`.",
            "python": "a `bytearray`, filled through a `memoryview` cast to `Q` (native 64-bit words): 32 million interpreted xorshift steps, more than half of this row's time. (1) `0 in buf` and (2) `bytearray.find` run as C searches (memchr for a single byte), so they cost almost nothing next to the fill. (3) `re.compile(rb'[,:\"]').finditer`, the regex engine's character-set scan, a byte-at-a-time loop in C. Three `find` calls per segment, the C row's approach, was about 20 times faster on this scan in a side test, because the delimiters are sparse; the regex is what people write for a first-of-set search.",
        },
    ),
]

DISPLAY = {"java-prim": "java (primitive table)", "csharp": "c#"}


def load(bench):
    results = {}
    with open(os.path.join(gen, f"{bench}.json")) as f:
        for r in json.load(f)["results"]:
            results[r["command"]] = r
    with open(os.path.join(gen, f"{bench}-noopt.json")) as f:
        noopt = json.load(f)["results"][0]
    return results, noopt


def sizes(bench):
    out = {}
    for line in open(os.path.join(gen, f"sizes-{bench}.txt")):
        tag, n = line.split()
        out[tag] = int(n) / 1024
    return out


def rss(bench, tag):
    text = open(os.path.join(gen, f"rss-{bench}-{tag}.txt")).read()
    return int(re.search(r"(\d+)\s+maximum resident set size", text).group(1)) / 1024 / 1024


def steady(bench, tag):
    path = os.path.join(gen, f"steady-{bench}-{tag}.txt")
    if not os.path.exists(path):
        return "—"
    ms = int(re.search(r"steady-state ms: (\d+)", open(path).read()).group(1))
    return f"{ms / 1000:.3f} s"


out = []
out.append("# K1 runtime benchmarks\n")
out.append("Generated by `run.sh`; do not edit by hand.\n")
out.append("```text\n" + env + "\n```\n")
out.append("## Methodology\n")
out.append(
    "Each program is one source file with the parameters hard-coded, prints a checksum line, and `run.sh` "
    "refuses to time anything whose output differs from the C program's. Timing is whole-process wall time "
    "from `hyperfine -N --warmup 2 --min-runs 5` (no shell), so process startup, page faults and any setup "
    "work are included; user/sys are hyperfine's per-run averages. `vs fastest` is mean divided by the "
    "best mean in the table.\n"
)
out.append(
    "For Java and C# that whole-process number also includes JVM/CLR startup and JIT warm-up, which is what "
    "a command-line program actually pays. The `steady` column is the other view: the same program run with "
    "the argument `steady` executes the identical workload three times inside one process and prints the "
    "third iteration's elapsed milliseconds to stderr, so the code is compiled and the heap is sized by then. "
    "It is a single measurement, not a hyperfine mean. Native rows have no `steady` column because for them "
    "the two numbers are the same thing, and neither does Python: CPython interprets its bytecode with no JIT, "
    "so there is no warm-up to separate out. Steady-state is not automatically faster: where the workload "
    "allocates heavily, three iterations in one process leave the heap larger and the collector busier than "
    "a fresh process does, and the third iteration can come out slower than the cold one.\n"
)
out.append(
    "`peak RSS` is `maximum resident set size` from `/usr/bin/time -l` on one ordinary run (not the steady "
    "run). `binary` is the linked executable for the native rows (Rust and Go link their runtimes statically; "
    "K1, C and Zig link only libSystem), the sum of the `.class` files for Java, the IL assembly "
    "(`bench.dll`) for C#, and the source file for Python, which CPython compiles to bytecode on every run. For "
    "the managed and Python rows that is the program only: the JVM, the .NET shared runtime and the Python "
    "interpreter are installed separately and are not counted, while every native row carries everything it "
    "needs.\n"
)
out.append("Build flags:\n")
out.append("```text")
out.append("k1     k1 --optimize --cache false build <file>.k1   (the default-build row is plain `k1 --cache false build`)")
out.append("c      clang -O2")
out.append("rust   rustc --edition 2021 -C opt-level=3")
out.append("go     go build")
out.append("zig    zig build-exe -O ReleaseFast")
out.append("java   javac -d <dir> Main.java    then  java -cp <dir> Main")
out.append("c#     dotnet publish -c Release   then  ./bench")
out.append("python python3 <file>.py   (no build step)")
out.append("```\n")
out.append(
    "The managed toolchains are used at their defaults: no `-Xmx` (the machine's ergonomic default heap is "
    "in the header above and was large enough for depth 20), no GC or JIT flags, no `-XX:+UseSerialGC`, and "
    "on the .NET side no ReadyToRun and no Native AOT, so the C# rows are JIT-compiled from IL exactly like "
    "the Java rows. `dotnet publish` produces a framework-dependent apphost, which hyperfine runs directly; "
    "the `dotnet` muxer is not in the measurement. Python is stock CPython (the version is in the header) with "
    "the standard library only: no PyPy, no numpy, no C extensions.\n"
)
out.append(
    "All programs are single-threaded; Python runs with the GIL. The Go, Java and .NET garbage collectors and the JVM's JIT compiler "
    "threads run on other cores, so their user time can exceed wall time.\n"
)

for bench, langs, what, notes in BENCHES:
    results, noopt = load(bench)
    sz = sizes(bench)
    fastest = min(results[l]["mean"] for l in langs)
    out.append(f"## {bench}\n")
    out.append(what + "\n")
    out.append("| language | mean ± σ | min … max | user | sys | vs fastest | steady | peak RSS | binary |")
    out.append("|---|---|---|---|---|---|---|---|---|")
    for l in langs:
        r = results[l]
        out.append(
            f"| {DISPLAY.get(l, l)} | {r['mean']:.3f} s ± {r['stddev']:.3f} | {r['min']:.3f} … {r['max']:.3f} | "
            f"{r['user']:.3f} | {r['system']:.3f} | {r['mean'] / fastest:.2f}x | {steady(bench, l)} | "
            f"{rss(bench, l):.0f} MB | {sz[l]:.0f} KB |"
        )
    r = noopt
    out.append(
        f"| k1, no --optimize (1 run) | {r['mean']:.3f} s | | {r['user']:.3f} | {r['system']:.3f} | "
        f"{r['mean'] / fastest:.2f}x | — | {rss(bench, 'k1-noopt'):.0f} MB | {sz['k1-noopt']:.0f} KB |"
    )
    out.append("")
    out.append("Output:\n")
    out.append("```text\n" + open(os.path.join(gen, "out", f"{bench}-c.txt")).read().rstrip() + "\n```\n")
    out.append("What each language runs:\n")
    for l in langs:
        out.append(f"- {DISPLAY.get(l, l)}: {notes[l]}")
    out.append("")

print("\n".join(out))
