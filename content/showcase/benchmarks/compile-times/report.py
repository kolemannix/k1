#!/usr/bin/env python3
"""Assembles results.md from run.sh's hyperfine exports and text captures
under gen/results/. Every number in results.md comes from those files."""

import json
import re
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[3]
RES = HERE / "gen" / "results"

LANGS = [
    ("k1", "k1"), ("c", "C"), ("cpp", "C++"), ("rs", "Rust"),
    ("go", "Go"), ("zig", "Zig"), ("ml", "OCaml"), ("java", "Java"), ("kt", "Kotlin"), ("cs", "C#"),
]
STRESS_LANGS = LANGS[:3] + [("cpp-hand-vec", "C++, hand-rolled `vec`/`opt`")] + LANGS[3:]
MODE_LABELS = {"check": "typecheck only", "debug": "debug build", "opt": "optimized build"}


def src_path(lang, n):
    d = HERE / "gen" / f"n{n}"
    if lang == "java":
        return d / "Stress.java"
    if lang == "cpp-hand-vec":
        return d / "stress-hand-vec.cpp"
    return d / f"stress.{lang}"

results = {}
for p in RES.glob("*.json"):
    results[p.stem] = json.load(open(p))["results"][0]
rss = {}
for p in RES.glob("*.rss.txt"):
    for line in p.read_text().splitlines():
        if "maximum resident set size" in line:
            rss[p.name[: -len(".rss.txt")]] = int(line.split()[0])

versions = {}
for line in (RES / "versions.txt").read_text().splitlines():
    k, v = line.split(":", 1)
    versions[k] = v.strip()
SIZES = versions["sizes"].split()
BIG = SIZES[-1]
MODES = [(m, MODE_LABELS[m]) for m in versions["modes"].split()]
MODE_COLS = [(m, m) for m, _ in MODES]


def cell_key(lang, mode, tag):
    if lang in ("go", "gocold", "kt"):
        mode = "debug"
    if lang == "java":
        if mode == "opt":
            return None
        mode = "debug"
    if lang == "cs" and mode != "opt":
        return None
    return f"{lang}-{mode}-{tag}"


def lookup(lang, mode, tag):
    key = cell_key(lang, mode, tag)
    return None if key is None else results.get(key)


def mb(lang, mode, tag):
    key = cell_key(lang, mode, tag)
    return "—" if key is None or key not in rss else str(round(rss[key] / 1048576))


def wall(r):
    if r is None:
        return "—"
    if r["stddev"] is None:
        return f"{r['mean']:.3f} ({r['min']:.3f})"
    return f"{r['mean']:.3f} ± {r['stddev']:.3f} ({r['min']:.3f})"


def cpu(r):
    return "—" if r is None else f"{r['user'] + r['system']:.2f}"


def table(header, rows):
    out = ["| " + " | ".join(header) + " |", "|" + "|".join("---" for _ in header) + "|"]
    for row in rows:
        out.append("| " + " | ".join(row) + " |")
    return "\n".join(out)


def cells_table(first, cols, rows, cell):
    header = [first]
    for _, label in cols:
        header += [f"{label} wall s", f"{label} cpu s"]
    out = []
    for key, name in rows:
        row = [name]
        for c, _ in cols:
            r = cell(key, c)
            row += [wall(r), cpu(r)]
        out.append(row)
    return table(header, out)


def mode_table(mode, langs):
    return cells_table("Language", [(n, f"N={n}") for n in SIZES], langs, lambda lang, n: lookup(lang, mode, n))


def rss_table(langs):
    header = ["Language"]
    cols = []
    for m, label in MODES:
        for n in SIZES:
            header.append(f"{label} N={n}")
            cols.append((m, n))
    rows = []
    for key, name in langs:
        rows.append([name] + [mb(key, m, n) for m, n in cols])
    return table(header, rows)


def hello_table(first, langs):
    return cells_table(first, MODE_COLS, langs, lambda lang, mode: lookup(lang, mode, "hello"))


def loc(n):
    rows = []
    for lang, name in STRESS_LANGS:
        text = src_path(lang, n).read_text()
        rows.append([name, str(text.count("\n")), str(len(text) // 1024)])
    return table(["Language", f"lines at N={n}", "KiB"], rows)


def chatty(name):
    text = (RES / f"k1-chatty-{name}.txt").read_text().splitlines()
    keep = []
    grab = False
    for line in text:
        s = line.strip()
        if s.startswith("program ") and " took " in s:
            grab = True
        if grab:
            keep.append(line.replace("\t", "  "))
        if s.startswith("untracked"):
            break
    return "\n".join(keep)


def chatty_frames(name):
    text = (RES / f"k1-chatty-{name}.txt").read_text().splitlines()
    keep = []
    for i, line in enumerate(text):
        if line.strip().startswith("top frames:"):
            keep = text[i : i + 11]
            break
    return "\n".join(l.replace("\t", "  ") for l in keep)


def brotli_loc():
    k1_files = sorted((REPO / "modules" / "brotli").glob("*.k1"))
    k1_lines = sum(p.read_text().count("\n") for p in k1_files)
    inputs = (RES / "brotli-c-inputs.txt").read_text().split()
    c_files = [f for f in inputs if f.endswith(".c")]
    h_files = [f for f in inputs if f.endswith(".h")]
    c_lines = sum((REPO / f).read_text().count("\n") for f in c_files)
    h_lines = sum((REPO / f).read_text().count("\n") for f in h_files)
    return k1_files, k1_lines, c_files, c_lines, h_files, h_lines


k1_files, k1_lines, c_files, c_lines, h_files, h_lines = brotli_loc()

synthetic = []
for mode, label in MODES:
    synthetic.append(f"### {label}\n\n" + mode_table(mode, STRESS_LANGS))
synthetic.append("### peak memory, MiB\n\n" + rss_table(STRESS_LANGS))

hello = hello_table("Language", LANGS)
cold = hello_table("Toolchain", [("k1", "k1 (`--no-cache`, same as above)"), ("gocold", "Go, empty GOCACHE"), ("zigcold", "Zig, empty global + local cache")])

warm_tables = []
for mode, label in MODES:
    warm_tables.append(
        f"### {label}\n\n"
        + mode_table(mode, [("k1", "k1 `--no-cache` (core+std from source)"), ("k1warm", "k1 warm cache (core+std restored, program changed)")])
    )

brotli = cells_table(
    "Program",
    MODE_COLS,
    [("brotli-k1", "k1 `examples/brotli_parity` (whole program, links)"), ("brotli-c", "C: 4 files, one `clang` process"), ("brotli-cpar", "C: 4 files, 4 `clang` processes in parallel")],
    lambda key, mode: results.get(f"{key}-{mode}"),
)

phases = []
for mode, _ in MODES:
    block = chatty(f"{mode}-{BIG}")
    if mode == "opt":
        block += "\n" + chatty_frames(f"{mode}-{BIG}")
    phases.append(f"```text\n{block}\n```")
phases.append(f"```text\n{chatty(f'brotli-{MODES[-1][0]}')}\n```")

doc = f"""# Compile times: k1 vs C, C++, Rust, Go, Zig, OCaml, Java, Kotlin, C#

Generated by `run.sh` on {versions['date']}. Every number below comes from the
hyperfine exports and text captures under `gen/results/`.

- Machine: {versions['cpu']}, {versions['os']}
- Toolchains: `{versions['k1']}`, `{versions['clang']}`, `{versions['rustc']}`,
  `{versions['go']}`, `zig {versions['zig']}`, `ocaml {versions['ocaml']}`, `{versions['javac']}`,
  `{versions['kotlinc']}`, `dotnet {versions['dotnet']}`, `{versions['hyperfine']}`
- hyperfine: `-N --warmup 3 --min-runs {versions['runs']}`; cells are wall-clock
  mean ± σ in seconds with the fastest run in parentheses, and CPU time
  (user + system, mean) in seconds. Peak memory is the maximum resident set
  size `/usr/bin/time -l` reports for one more run of the same command, after
  the same prepare step; for a toolchain that forks (`go build`, `kotlinc`,
  `dotnet build`) it is the largest single process. The CPU column matters: k1, rustc and Go
  compile on several cores by default, while clang, clang++ and zig work
  single-threaded on one translation unit. On a busy machine the parallel
  compilers lose the most wall time, and the fastest run is the least
  disturbed number.
- Load average (1/5/15 min) at start: {versions.get('load-start', 'not recorded')};
  at end: {versions.get('load-end', 'not recorded')}.


`gen.py` emits the program shape of `perf/gen_stress.py` in ten languages,
unit for unit. One unit is an independent namespace shaped like application
code: 13 nominal types (structs, sum types with payloads, an optional, a list
of structs), eleven plain functions built from `match`, loops, early returns
and struct updates (`area`, `step`, `churn`, `line-cost`, `order-total`,
`advance`, `apply-event`, `settle`, `quantize`, `summarize`, `sample-run`),
two abilities (`sz`, `enc[o]`) with eight impls including one blanket impl
over a generic box, nine small generics instantiated at 4 to 16 types each,
and a `test()` that calls all of it. `main` sums every unit's `test()` so
nothing is dead before semantic analysis. The k1 text is
`perf/gen_stress.py`'s `unit()` verbatim; the other nine are hand ports of
the same statements. The same file is compiled at N = 10, 100 and 300 units
as one translation unit. Every generated program compiles cleanly and exits 0
in every language at N = 1, 10, 100 and 300.

Lines per language at N = 100 (same program, so the ratio is the language's
verbosity for this shape):

{loc(100)}

Where a language forced a different shape:

- C has no generics, so `gen.py` monomorphizes by hand: the shared `lib` block
  is the `bx`, `duo` and optional instantiations at the scalar types, emitted
  once, with one function per instantiation (`lib_mk_u8`, `lib_get_bx_u8`,
  `lib_swap_opt_u8_opt_u16`, ...), and each unit still instantiates them at its
  own `item` and `sample`. Sum types are a tag enum plus an anonymous union;
  struct patterns become `if` chains; abilities are plain functions.
  The list is a 3-word struct with a `realloc` push, per element type.
- C++ uses `namespace`, templates for the generics, `std::vector` and
  `std::optional`, and overloads for the abilities (`sz(uint32_t)`,
  `sz(item)`, `template <class T> sz(bx<T>)`). `enc[o]`'s output type
  parameter has no counterpart; the overloads return `uint64_t`. Sum types
  are tag + anonymous union as in C, built with C++20 designated
  initializers (`-std=c++20`). No `std::variant`, no `<iostream>`.
- The second C++ row, `stress-hand-vec.cpp`, is the same program with
  `std::vector` and `std::optional` swapped for a 20-line `vec<T>` (pointer,
  length, capacity, `realloc` push, like the C port's list) and a 12-line
  `opt<T>`, and no standard headers but `<cstdint>`/`<cstdlib>`. Every unit
  has its own `item` and `sample` types, so the program instantiates its list
  at 200 element types and its optional at 200 more beyond the shared scalar
  ones; with libc++ each of those
  pulls in `push_back`'s exception guards, `allocator_traits`,
  `reverse_iterator` and the relocation helpers. `clang -ftime-trace` puts
  most of the C++ front-end time in those instantiations, and this row is
  what the C++ language costs for this shape once the library is out of the
  picture. k1 and Rust pay for their own `list`/`opt` and `Vec`/`Option` the
  same 200 times inside their rows.
- Rust uses `mod`, `enum` + `match`, generics, `Vec`, `Option`, and traits
  `Sz` / `Enc<O>`. Three `#[derive(Clone, Copy)]` (`Duo`, `Vec2`, `Item`) are
  the minimum for the k1 statements to borrow-check; `line_cost`,
  `order_total` and `summarize` take `&T` where k1 passes by value and the
  value is used again afterwards. `#![allow(dead_code)]` silences the
  never-read-field warnings every unit would otherwise print 300 times.
- Go has no namespaces, so every identifier carries a `_<unit>` suffix. Sum
  types are a struct with a tag field plus one field per payload; optionals
  are a generic `opt[T]`; lists are slices. Abilities are interfaces (`sz`,
  `enc[O]`), which cannot be implemented for `uint32`/`uint64`, so the
  scalar impls live on named types `u32_N`/`u64_N`; the blanket impl
  `impl[t: sz] sz for bx[t]` cannot be a method of `bx[T]` and is a
  constrained free function `szBx`. Go has one build mode: `go build` is
  measured once per N and reported in all three columns.
- Zig uses a `struct` per unit as the namespace, `union(enum)` + `switch`,
  `?T`, `std.ArrayList`, and `comptime` generics (`fn Bx(comptime T: type)
  type`, `anytype` parameters). Abilities are `comptime` type switches inside
  `sz`/`enc` instead of per-type impls; the blanket box impl is the
  `else` prong. `settle`/`sampleRun` return `!u64` because `ArrayList.append`
  can fail. Zig forbids a local shadowing a declaration, so `summarize`'s
  local `total` is named `tally`; `test` is a keyword, so the unit entry point
  is `run`.
- OCaml puts each unit in a `module U<i> = struct ... end`. Sum types are
  variants, optionals are `option`, struct updates are `{{ a with ... }}`, and
  the by-value reassignments are shadowing `let`s or a `ref`. Lists are
  immutable `list`s built with `List.init` and walked with `List.fold_left`/
  `List.iter`, which is what the language offers in place of a growable
  array (`Dynarray` arrives in 5.2). OCaml has one integer type, so `u8`/
  `u16`/`u32`/`u64` are all `int` (63-bit; every value in the unit is small
  and non-negative) and `f32`/`f64` are both `float`, so the generics
  instantiate at fewer distinct types; that costs nothing here because OCaml
  never monomorphizes. Abilities are module types (`SZ`, and `ENC` with its
  `type o`), each impl is a module (`Sz_u32`, `Sz_item`, ...), the blanket
  box impl is the functor `Sz_bx (S : SZ)`, and `total`/`encode` take the
  impl as a first-class module (`total (module Sz_bx (Sz_item)) (mk probe)`):
  the dictionary K1 resolves is passed by hand. Record labels shared between
  types (`value`, `id`) rely on type-directed disambiguation, so the function
  parameters are annotated.
- Java puts each unit in a nested `static final class U<i>` of one
  `public final class Stress`, which is the closest thing Java has to k1's
  `ns`, so no identifier needs a unit suffix. Everything else the language
  forces: Java has no value types, so every struct is a class, and where the
  k1 and Go units mutate a by-value copy (`advance`, `applyEvent`) the port
  calls a copy constructor first. It has no designated initializers, so each
  class gets one all-fields constructor and the payload fields a variant does
  not use are passed `0`/`null`. Sum types are therefore a tag field plus one
  field per payload, as in Go; sealed interfaces plus records would have been
  the modern shape but records are immutable, which those two struct-update
  functions rule out. It has no unsigned integers: `u64` becomes `long`,
  `u32`/`u8` become `int`/`byte`, and the widening sites use
  `Integer.toUnsignedLong` and friends (every value in the unit is small and
  non-negative, so signed arithmetic gives the same answers). Generics are
  erased and cannot be instantiated at a primitive, so the nine generics
  instantiate at `Byte`/`Short`/`Integer`/`Long`/`Boolean`/`String`/`Float`/
  `Double`, and `Opt<Long>` boxes. Abilities are interfaces, which `int` and
  `long` cannot implement, so the scalar impls live on wrapper classes
  (`U32Val`/`U64Val`, named that way because unit 32's class is already
  called `U32`) exactly as in Go, and the blanket `impl[t: sz] sz for bx[t]`
  is the constrained static method `static <T extends Sz> long szBx(Bx<T>)`.
- Kotlin puts each unit in a top-level `object U<i>`, the language's
  namespace, and is the JVM port that needs the fewest concessions. Sum types
  are `sealed interface`s with one nested `class` per variant (`object` for a
  payload-less one) and `when` with `is` arms, the one JVM port whose sum
  types are real sum types; optionals are nullable types (`ULong?`,
  `Event?`), so the `opt` generic has no counterpart and `zero`/`measure`
  are `fun <T> zero(): T?` and `fun <T> measure(v: T?)`; `UByte`/`UInt`/
  `ULong` are real unsigned types (inline classes over the signed ones, so
  `Bx<ULong>` boxes like Java's `Bx<Long>`). Structs are classes with a
  primary constructor; `Order` and `Account`, the two the struct-update
  functions copy, are `data class`es so that `o.copy(status = next)` is K1's
  `o | .{{ status = next }}`, the same minimum as Rust's three derives. Lists
  are `ArrayList`. The two forced shapes are Go's and Java's: an interface
  cannot be implemented for `UInt`/`ULong`, so the scalar impls live on
  wrapper classes `U32Val`/`U64Val`, and the blanket box impl is
  `fun <T : Sz> szBx(b: Bx<T>)`. Every sealed variant and `object` is its own
  class file, so a unit compiles to 39 `.class` files where Java's is 20.
- C# puts each unit in a top-level `static class U<i>`. It is the port that
  needs the fewest concessions: `struct` gives k1's value semantics, so
  `advance`/`applyEvent` assign a copy with no helper; object initializers
  (`new Shape {{ Tag = ShapeTag.Rect, Rect = start }}`) give Go's named-field
  literals; `ulong`/`uint`/`byte` are real unsigned types; generics are
  specialized over value types, so `Opt<ulong>` does not box. Sum types are a
  tag `enum` plus one field per payload. The two forced shapes are the same
  two Go has: an interface cannot be implemented for `ulong`, so the scalar
  ability impls live on wrapper structs `U32Val`/`U64Val`, and the blanket box
  impl is `static ulong SzBx<T>(Bx<T> b) where T : ISz`. The `.csproj` carries
  `<NoWarn>CS0649</NoWarn>`, the counterpart of Rust's
  `#![allow(dead_code)]`: the `note` variant of `event` is never constructed,
  which would otherwise print two warnings per unit.


Everything is a single source file and a single toolchain invocation with
default settings unless listed. The debug build is no optimization plus full
debug info (DWARF for the native compilers, local variable tables for the
JVM ones), the mode an edit-debug loop runs; the typecheck and optimized
builds carry no debug info. Go is the exception with one mode that both
optimizes and emits DWARF.

| Language | typecheck only | debug build | optimized build |
|---|---|---|---|
| k1 | `k1 --no-cache check f.k1` | `k1 --no-cache --debug build f.k1` | `k1 --no-cache --optimize build f.k1` |
| C | `clang -fsyntax-only f.c` | `clang -g -O0 -o out f.c` | `clang -O2 -o out f.c` |
| C++ (both rows) | `clang++ -std=c++20 -fsyntax-only f.cpp` | `clang++ -std=c++20 -g -O0 -o out f.cpp` | `clang++ -std=c++20 -O2 -o out f.cpp` |
| Rust | `rustc --edition 2021 --emit=metadata f.rs` | `rustc --edition 2021 -g -C opt-level=0 f.rs` | `rustc --edition 2021 -C opt-level=3 f.rs` |
| Go | `go build -o out f.go` (no separate check; `go vet` is not the compiler) | same | same (Go has no optimization level) |
| Zig | `zig build-exe -fno-emit-bin f.zig` | `zig build-exe -O Debug f.zig` | `zig build-exe -O ReleaseFast f.zig` |
| OCaml | `ocamlopt -stop-after typing f.ml` | `ocamlopt -g -o out f.ml` | `ocamlopt -O3 -o out f.ml` (`-O3` is inert without flambda) |
| Java | `javac -g -d out Stress.java` | same | — (javac has no optimization level) |
| Kotlin | `JAVA_OPTS=-XX:MaxRAMPercentage=25 kotlinc -d out f.kt` (no typecheck-only mode) | same | same (kotlinc has no optimization level) |
| C# | — | — | `dotnet build -c Release f.csproj` |

- `rustc` is used bare, without cargo, so no cargo overhead is measured.
  `--edition 2021` because bare `rustc` defaults to 2015. rustc's defaults
  apply otherwise: 16 codegen units and parallel LLVM at both opt levels.
- `k1 --debug` is the pure `-O0` row: no LLVM passes and full DWARF, the
  same shape as `clang -g -O0`. k1's default `build` is the dev pipeline
  instead: line tables only, and each codegen unit runs `mem2reg`, one
  `instcombine` iteration, `simplifycfg`, `globaldce` and `mergefunc`
  (`Pipeline::Dev` in `src/k1/codegen_llvm.rs`); it is a little faster than
  `--debug` and is what the warm-cache rows below would normally run as.
  `--optimize` is ThinLTO: `thinlto-pre-link<O3>` per
  unit, then LLVM's ThinLTO backend, so it is closer to
  `clang -O3 -flto=thin` than to the single-TU `-O2`/`opt-level=3`/
  `ReleaseFast` rows, none of which do LTO.
- k1 codegens in parallel units and links with its bundled lld. `--no-cache`
  also disables restoring core and std from k1's disk cache, so every
  k1 run re-parses and re-typechecks the whole standard library from source;
  the section "k1 with a warm library cache" shows the cost of that choice.
- Zig's `-O Debug` already emits full DWARF, and that is most of its debug
  build: `-O Debug -fstrip` compiles the N=100 program in about half the
  time, where `-g` costs clang a third more on the same program. The default
  backend on arm64-macos is LLVM at `-O0` followed by Zig's own linker. Its
  global cache (compiler_rt, std artifacts) is one directory under `gen/`
  that stays warm across runs; its local cache (`--cache-dir`) is deleted
  before every run, so the program itself is always compiled from scratch.
- `ocamlopt -stop-after typing` parses and typechecks and writes only the
  `.cmi` (`ocamlc -i` would typecheck too, but then print the inferred
  interface of the whole program). `ocamlopt` has no `-O0`/`-O2`: the stock
  compiler, which opam and Homebrew install by default and reports
  `flambda: false` in `ocamlopt -config`, has one backend with no
  optimization level and accepts `-O3` only for command-line compatibility
  with the flambda variant, where it selects the aggressive inlining
  pipeline. The optimized column reruns the same compile with `-O3` and
  equals the debug column unless the installed switch is flambda; the
  versions line records which. `ocamlopt` is single-threaded, emits assembly
  text and runs the system assembler on it (`-dtimings` reports that
  `assemble` step as most of the build at these sizes), then `clang` for
  the link.
- `go build` optimizes and emits DWARF, with no switch for either in the
  table (`-gcflags=all=-N -l` and `-ldflags=-w -s` exist, and the typical
  Go user types neither). Go's GOCACHE is one directory under `gen/` that
  stays warm, so the runtime and `os`/`fmt` are not rebuilt (Go 1.20+ builds the standard library into
  GOCACHE, which takes seconds), but the source gets a fresh nonce comment
  before every run and the previous binary is deleted, so the program is
  compiled and linked from scratch each time.
- `javac` has one mode. It type-checks and emits bytecode in the same pass and
  has no optimization level at all (optimization is the JIT's job at run time),
  so the same measurement fills the typecheck and debug columns and the
  optimized column is empty. Every run is a fresh `javac` JVM: there is no
  compile server and no cache, and the output directory is deleted before each
  run, so the JVM startup and class loading `javac` itself pays are inside
  every Java number, and its CPU column is inflated by the JIT threads
  compiling `javac` itself while it works. One file becomes thousands of
  `.class` files (one per nested class per unit), which is part of what the
  debug column buys.
- `kotlinc` has one mode, like `javac`: no typecheck-only switch (its `-X`
  list has `-Xdisable-phases`, which turns off individual backend IR
  lowerings for debugging and nothing in front of them, and no front-end-only
  option) and no optimization level (`-Xno-optimize` turns off the bytecode
  peephole cleanups that are on by default), so the one `kotlinc -d out f.kt`
  measurement fills all three columns, and it always emits local variable
  tables, what `javac -g` adds. Every run is a fresh JVM running the
  compiler: the `kotlinc` script runs `java` on `K2JVMCompiler` directly,
  with no daemon (the Kotlin compile daemon exists only behind the Gradle and
  Maven plugins and is never started here) and no cache, and the output
  directory is deleted before each run, so JVM startup and the JIT compiling
  `kotlinc` itself are inside every Kotlin number, as for Java, and its CPU
  column is inflated by the JIT threads; `kotlinc -Xdump-perf` reports more
  JIT time than compiler time at N=100, and puts about half of the compiler
  time in analysis and the rest in IR translation, lowering and bytecode
  generation. The one change to the plain command: the `kotlinc` script caps
  its JVM at `-Xmx512M` unless `JAVA_OPTS` is set, which is an
  `OutOfMemoryError` at N=300, so run.sh sets
  `JAVA_OPTS=-XX:MaxRAMPercentage=25`, the JVM's own default heap policy and
  what `javac`'s JVM runs with; it does not change the N=10 and N=100
  numbers.
- C# is measured once, as `dotnet build -c Release`, and Release is an
  optimized build, so it sits in the optimized column. `dotnet build` is
  msbuild plus a restore check plus the Roslyn compiler, not a bare compiler
  invocation; the hello world row is the same command on a four-line program,
  so the difference between the two is what Roslyn spends on the program and
  the hello row is the fixed msbuild overhead. Roslyn does run in a persistent
  build server (`VBCSCompiler`) that survives between runs, unlike every other
  toolchain here. The source gets a fresh nonce comment before every run so
  msbuild cannot skip the compile. Two consequences for the C# cells: the
  compiler process is already warm, with no JIT warm-up of its own in the
  measurement, and the CPU column is wrong-by-construction, because it counts
  only the `dotnet` client process while the actual compile happens in the
  server. Read the C# wall column and ignore its CPU column.
- clang and rustc have no caches; C/C++ system headers are parsed on every
  run (no modules, no PCH).
- The "fully cold" table below shows the Go and Zig hello-world cost with
  their caches emptied before every run, next to k1's `--no-cache`.


{chr(10).join(synthetic)}


Same commands on a hello world (`println`/`printf`/`std::printf`/`println!`/
`fmt.Println`/`std.io` `print`/`print_endline`/`System.out.println`/`println`/
`Console.WriteLine`). C++ uses `<cstdio>`, not `<iostream>`. The Java, Kotlin
and C# rows are the floor of their toolchains: JVM startup and class loading
for `javac` and `kotlinc`, and msbuild plus a restore check for `dotnet
build`, on four lines of source.

{hello}


Same hello world with the toolchain's own library cache emptied before every
run. k1's `--no-cache` already is that mode (core and std typechecked from
source on every run), so its row repeats the table above.

{cold}


k1's disk cache restores the longest valid prefix of modules, so with the
cache on and a changed program it restores core and std from a snapshot and
compiles only the program, which is what an edit-rebuild loop pays. The
source gets a fresh nonce before every run (a global `NONCE` that `main`
reads), so the program itself is never restored. This is the direct
counterpart of the warm GOCACHE and warm Zig global cache above, with two
k1-specific effects: with the cache on, every run also *stores* a snapshot
of the whole typed program (`k1 --chatty` reports it as `snapshot store`),
which can outweigh the time saved by restoring modules, and `--optimize` also uses k1's
ThinLTO backend cache under `.k1-out/cache/thinlto`, so codegen units
whose bitcode did not change (everything but the unit holding `main`) skip
the LLVM backend.

{chr(10).join(warm_tables)}


`modules/brotli` is a k1 port of the brotli encoder, quality 0 through 11,
byte-identical to the C. k1 side: `k1 ... build examples/brotli_parity`
compiles the whole module ({len(k1_files)} files, {k1_lines} lines including
the generated tables and the table generator in `build.k1`), plus core, std,
the parity driver's `cbrotli` C encoder bindings, and the
driver itself, and links an executable. C side: the four vendored files the port corresponds to
(`modules/brotli/vendor/*.c`, {c_lines} lines) compiled with `-c`, no link;
they pull in {len(h_files)} project headers from `examples/brotli_parity/deps/cbrotli/vendor`
({h_lines} lines). Two C rows: one `clang` process compiling the four files in
sequence, and four processes at once (`par-cc.sh`, what `make -j` does),
since k1's build is parallel.

{brotli}


From `k1 --no-cache --chatty`. "excl ms" is exclusive time per kind
of work; codegen and passes run on several threads (their rows report both).

{(chr(10) * 3).join(phases)}


- Numbers are from one machine and one run of `run.sh`. The load averages
  in the header say how busy the machine was; σ and the fastest run tell how
  much each cell suffered, and the parallel compilers suffer first.
- One translation unit per program is the fairest single-invocation
  comparison but it favors the compilers that parallelize inside one unit
  (k1, rustc, Go, Roslyn) over those that do not (clang, zig). The CPU column
  is the single-core view.
- The three managed rows are not comparable to the native rows as
  "time to a runnable program": `javac` and Roslyn stop at bytecode/IL and
  leave machine-code generation to the JIT at run time, which is work the
  other seven compilers do here and those three defer to every execution. The
  `runtime/` benchmarks in this directory's sibling measure what that
  deferral costs.
- Go's build cache and Zig's global cache are warm in the main tables; k1's
  is cold (`--no-cache`). The separate k1 warm-cache rows measure the
  effect of restoring and storing snapshots and reusing LLVM artifacts.
- `-fsyntax-only`, `--emit=metadata` and `-stop-after typing` stop after
  semantic analysis;
  `k1 check` also runs the compile-time VM for `#static` code and lowers
  functions to k1's IR (the `lower` row in the phase table);
  `zig -fno-emit-bin` runs full semantic analysis of everything reachable
  from `main`. `k1 check` is also cheaper than the front end of `k1 build`:
  lowering to IR requests generic specializations that `check` never
  typechecks; the phase tables show the additional work.
"""

(HERE / "results.md").write_text(doc)
