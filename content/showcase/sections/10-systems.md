# Systems programming

K1 compiles to machine code through LLVM and speaks the C ABI, so the
systems-level surface is direct: SIMD lanes are a type, atomics are functions
over ordinary memory, an `extern` declaration is a binding, and a module's
build step is K1 that the compiler runs in its own VM. This section walks
through each, and ends with the same programs on wasm and bare metal.

## A SIMD scanner generated at compile time

One line asks the standard library for a delimiter scanner:

```k1
$std/simd/first-of(delims, [',', ':', '"'])
```

It expands into `ns delims { fn scan(s: string, from: size): { index: i64,
which: i64 } }`, and the program uses it like any other function:

```k1
fn scan-all-simd(text: string): u64 {
  let found: u64 = 0
  let from: size = 0
  loop {
    let hit = delims/scan(text, from)
    if hit.index < 0 break
    found = found + 1
    from = hit.index + 1
  }
  found
}
```

`first-of` is a macro in `modules/std/simd.k1`. Its body is an ordinary K1
function that writes K1 source into a string builder: a loop that loads a
vector of bytes, compares it against one splatted needle per delimiter, ORs
the lane masks together, and uses `trailing-zeros` on the combined mask to
find the first hit, followed by a scalar tail for the last partial vector.
This is the core of the generator:

```k1 path=modules/std/simd.k1
  code.writeln("    while i + $lanes <= len {")
  code.writeln("      let chunk = vector/load-unchecked[u8, $lanes](base.ref-at[u8](i).as[ptr])")
  for needles {
    code.writeln("      let m$it-index = chunk.eq-mask(needle$it-index)")
  }
  let all-masks = string-builder/new()
  for needles {
    if it-index > 0 { all-masks.write(".bit-or(m$it-index)") }
    else { all-masks.write("m0") }
  }
  code.writeln("      let all = ${all-masks.build()}")
  code.writeln("      if all != 0 {")
  code.writeln("        let lane = all.trailing-zeros()")
  for needles {
    code.writeln("        if m$it-index.shift-right(lane.trunc[u32]).bit-and(1) == 1 {")
    code.writeln("          return .{ index = i + lane.signed(), which = $it-index }")
    code.writeln("        }")
  }
  code.writeln("        crash(\"unreachable: mask bit vanished\")")
  code.writeln("      }")
  code.writeln("      i = i + $lanes")
  code.writeln("    }")
```

```k1 path=modules/std/simd.k1
macro first-of(name, needles: span[char]) {
  code/from-string(first-of-impl(name.text(), needles))
}
```

The lane count comes from `k1/simd-bytes`, the target's natural vector width
(16 on arm64-macos and on wasm64), so the same macro emits a 32-lane loop on
an AVX2 target without changing. The vector operations the generated code
calls are compiler intrinsics declared in `modules/core/vector.k1`:

```k1 path=modules/core/vector.k1
  // Lane-wise compare: each lane is all-ones on equal, all-zeros otherwise.
  fn(intern) eq-lanes[t, n: static size](a: vector[t, n], b: vector[t, n]): vector[t, n]

  // collapses lane MSBs into one bit per lane (movemask)
  fn(intern) to-mask[t, n: static size](v: vector[t, n]): u64
```

`eq-lanes` lowers to an LLVM vector `icmp eq` sign-extended back to the lane
width, and `to-mask` to a lane-MSB compare bitcast from `<16 x i1>` to `i16`
and zero-extended (`VecOpIr::ToMask` in `src/k1/codegen_llvm.rs`), leaving
the movemask idiom to LLVM's backend for each target. `trailing-zeros` is
not an intrinsic the compiler knows about; it is a named LLVM intrinsic
declared in K1 source, in `modules/core/core.k1`:

```k1 path=modules/core/core.k1
  fn(intern("llvm.cttz.i64")) _cttz(x: u64, zero-poison: bool): u64

  fn trailing-zeros(self: u64): u64 {
    _cttz(self, false)
  }
```

Any K1 file can declare one the same way (`fn(intern("llvm.ctpop.i64"))
popcount(x: u64): u64` compiles and runs from a user program; the declared
signature is trusted and the LLVM verifier is the backstop). The compile-time
VM emulates the ones it knows, cttz, ctlz and ctpop among them, and executes
vector operations on its own memory, so the generated scanner runs at compile
time too:

```k1
  let static-hits: 4u64 = #static scan-all-simd("{\"key\": [1, 2]}")
  println("compile-time scan found ${static-hits.from-static()} delimiters")
```

The `4u64` is a literal type: the compiler ran the SIMD scan in the VM while
typechecking and checked the answer. A first draft of this line said `3`, and
the compiler pointed out the miscount:

```text
│ ->  let static-hits: 3 = #static scan-all-simd("{\"key\": [1, 2]}")
│                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
│     println("compile-time scan found ${static-hits.from-static()} delimiters")
│   
├─────
│  Expected value type static[i64, 3] but got value type static[u64, 4]
```

There is no intrinsics header, no per-architecture `#ifdef`, and no second
language for the kernel: the generator is K1, the generated code is K1, and
the same text runs natively, under wasm, and in the compiler.

Using `vector[u8, 16]` by hand is the same set of operations. This counts
occurrences of a byte, adding one per matching lane into a vector
accumulator and flushing it to a scalar before the `u8` lanes can wrap:

```k1
fn count-byte-simd(bytes: span[u8], needle: u8): u64 {
  let base = bytes.data-ptr()
  let len = bytes.len()
  let needles = vector/splat[u8, 16](needle)
  let ones = vector/splat[u8, 16](1)
  let acc: vector[u8, 16] = .0
  let chunks-in-acc = 0
  let count: u64 = 0
  let i: size = 0
  while i + 16 <= len {
    let chunk = vector/load-unchecked[u8, 16](base.add-bytes(i))
    acc = acc + chunk.eq-lanes(needles).bit-and(ones)
    chunks-in-acc = chunks-in-acc + 1
    if chunks-in-acc == 255 {
      count = count + lane-sum(acc)
      acc = .0
      chunks-in-acc = 0
    }
    i = i + 16
  }
  count = count + lane-sum(acc)
  while i < len {
    if bytes.[i] == needle { count = count + 1 }
    i = i + 1
  }
  count
}
```

`examples/sys_simd.k1` times both kernels against plain scalar loops over an
8 MB buffer (best of three; each rep is offset by one byte and bracketed by
`atomic/fence`s so the pure kernels cannot be hoisted or sunk across the
timer reads; `std/time` supplies the counter). Unoptimized:

```text
compile-time scan found 4 delimiters
simd width: 16 bytes; buffer: 8 MB
scalar scan: 145794 found, 203 MB/s
first-of scan: 145794 found, 879 MB/s
scalar count ',': 137380 found, 451 MB/s
vector count ',': 137380 found, 3857 MB/s
```

With `--optimize`:

```text
compile-time scan found 4 delimiters
simd width: 16 bytes; buffer: 8 MB
scalar scan: 145794 found, 2965 MB/s
first-of scan: 145794 found, 5617 MB/s
scalar count ',': 137380 found, 6791 MB/s
vector count ',': 137380 found, 30467 MB/s
```

The optimized scalar byte count is LLVM's auto-vectorizer at work; the hand
accumulator still beats it four to one. The generated scanner pays for a
function call and a mask decode per delimiter (one every 61 bytes here), and
comes out half again to twice the scalar loop from run to run. Apple M3 Max, macOS 26.6.2,
`arm64-macos`, k1 0.1.0; the wasm numbers below are wasmtime 47.0.3.

## Threads, atomics and thread-locals

```k1
use std/thread

let N_THREADS: size = 4
let N_ITERS: u64 = 100000

type shared = { atomic-total: u64, lock: u64, locked-total: u64 }

let(mutable, tls) my-bumps: u64 = 0

fn worker(s: *shared): ptr {
  for 0u64.until(N_ITERS) {
    let _ = atomic/fetch-add(s.atomic-total.&, 1, :relaxed)
    my-bumps = my-bumps + 1
  }
  while not atomic/cmpxchg(s.lock.&, 0: u64, 1, :acquire, :relaxed).ok {}
  s.locked-total = s.locked-total + my-bumps
  atomic/store(s.lock.&, 0: u64, :release)
  mem/bitcast[u64, ptr](my-bumps)
}

fn main(): i32 {
  let s = mem/new(shared.0)
  let handles: array[thread/thread, 4] = .0
  for i in 0.until(N_THREADS) {
    thread/start(handles.[i].&, worker.&, s)
  }
  for i in 0.until(N_THREADS) {
    let result: ptr = ptr/null
    thread/join(handles.[i], result.&)
    let got = result.to-uint()
    println("thread $i bumped its thread-local $got times")
  }
  println("atomic total: ${s.atomic-total}")
  println("locked total: ${s.locked-total}")
  println("main's thread-local: $my-bumps")
  assert-equals(s.atomic-total, N_THREADS.unsigned() * N_ITERS)
  assert-equals(s.locked-total, s.atomic-total)
  0
}
```

```text
thread 0 bumped its thread-local 100000 times
thread 1 bumped its thread-local 100000 times
thread 2 bumped its thread-local 100000 times
thread 3 bumped its thread-local 100000 times
atomic total: 400000
locked total: 400000
main's thread-local: 0
```

Atomics are operations, not types: `atomic/fetch-add` takes a reference to an
ordinary `u64` field of an ordinary struct, and the `cmpxchg` spin lock guards
a plain field next to it. Orderings are literals, and the compiler checks
them at the call site; `test_src/suite1/atomics.k1` asserts the messages:

```k1 path=test_src/suite1/atomics.k1
  assert-equals[?string](
    test-compile(atomic/load(x, :release)),
    :some "atomic load cannot use `:release` ordering"
  )
```

`let(mutable, tls)` is the whole thread-local story: a global modifier, one
instance per thread, main's copy untouched by the workers. `std/thread` is a
thin layer over the platform: `start` writes the new thread's handle through
an out-param and takes any `*fn(*t) -> ptr` (`worker.&`, the function's
address) plus its argument; `join` writes the thread's raw `ptr` result into
the slot you hand it, here bitcast back to a `u64`.

## FFI with no bindings file

Three declarations, no header, no build step:

```k1
use core/c-int

fn(extern("strlen")) strlen(s: ptr): size
fn(extern("getpid")) getpid(): c-int
fn(extern("qsort")) qsort(base: ptr, count: size, width: size, compare: *fn(ptr, ptr) -> c-int)

fn compare-i32(a: ptr, b: ptr): c-int {
  let x = a.ref[i32].*
  let y = b.ref[i32].*
  if x < y { -1 } else if x > y { 1 } else { 0 }
}

fn main(): i32 {
  let greeting = "hello from libc".to-c-tmp()
  println("strlen: ${strlen(greeting.ptr)}")

  let xs: list[i32] = [40, 7, 23, 1, 99, 15]
  qsort(xs.as-span().data-ptr(), xs.len(), types/size[i32], compare-i32.&)
  println("qsort: $xs")

  println("getpid > 0: ${getpid() > 0}")
  0
}
```

```text
strlen: 15
qsort: [1,7,15,23,40,99]
getpid > 0: true
```

`fn(extern("sym"))` binds a symbol; the fn name is free. The address of a K1
function is a C function pointer, so `compare-i32.&` goes straight into
`qsort`, which sorts the `list[i32]`'s storage in place. `test_src/ffi_abi_test` pins the ABI
corners against a C library: 3- and 6-byte structs by value, homogeneous
float aggregates, packed structs, tagged unions, callbacks that take structs,
and extern globals including thread-locals:

```k1 path=test_src/ffi_abi_test/ffi_abi_test.k1
  type rgb3 = { r: u8, g: u8, b: u8 }
  fn(extern("rgb3")) rgb3(a: rgb3, b: rgb3): rgb3
```

```k1 path=test_src/ffi_abi_test/ffi_abi_test.k1
  let(extern("abi_readonly")) readonly: i32
  let(extern("abi_counter"), mutable) counter: i32
  let(extern("abi_tls_counter"), mutable, tls) tls-counter: i32
```

A library that is not libc is named once, in the manifest, and either on the
namespace (`ns(lib("foo")) foo { ... }`) or per function. `modules/cbrotli`
does the latter:

```k1 path=modules/cbrotli/cbrotli.k1
fn(extern("BrotliEncoderCompress"), lib("brotli")) encoder-compress(
  quality: c-int,
  lgwin: c-int,
  mode: mode,
  input-size: size,
  input: ptr,
  encoded-size: *size,
  encoded: ptr,
): c-int
```

For a large API the bindings are generated. `dogfood/k1bindgen` reads C
headers through libclang and emits K1; `modules/libuv/uv.k1` is its output
for libuv, and reads like the hand-written declarations above:

```k1 path=modules/libuv/uv.k1
fn(extern("uv_default_loop")) default_loop(): optref[uv-loop]
fn(extern("uv_loop_init")) loop_init(
  loop_c: *uv-loop,
): i32
fn(extern("uv_loop_close")) loop_close(
  loop_c: *uv-loop,
): i32
```

A `*t` in K1 is never null, so a C pointer that comes back from a call is
bound as `optref[uv-loop]`, one word that must be checked, while a pointer
argument the caller supplies stays `*uv-loop`. Every generated struct carries
the layout libclang reported, as a compile-time assertion, so a K1 type that drifts from its C definition fails
the build instead of corrupting memory:

```k1 path=modules/libuv/net.k1
type sockaddr_in = {
  sin_len: u8,
  sin_family: sa_family_t,
  sin_port: in_port_t,
  sin_addr: in_addr,
  sin_zero: array[u8, 8],
}
#static assert-layout[sockaddr_in](16, 4, [0, 1, 2, 4, 8, ])
```

## Build steps are K1 running inside the compiler

A module's `build.k1` holds its build description, compiled on the host
before any of the program: `fn module(b)` returns the manifest for the build
config `b`, and `fn setup(ctx)` is an optional build step. Both are plain K1.
`modules/cbrotli` compiles its vendored C sources:

```k1 path=modules/cbrotli/build.k1
use std/process

fn module(_b: k1/build-config): k1/module {
  let m = k1/module/new()
  m.lib("brotli", :static)
  m.setup(["libs/libbrotli.a"], ["vendor"])
  m
}

// Compiles the vendored C sources into libs/libbrotli.a. Runs in the
// compile-time VM with cwd = the module dir; needs cc on PATH.
fn setup(ctx: k1/setup-ctx) {
  let minver = if (ctx.build.target is :arm64-macos) "-mmacosx-version-min=15.0.0" else ""
  let script = `
    set -e
    mkdir -p libs .k1-out/cbrotli
    for f in vendor/common/*.c vendor/dec/*.c vendor/enc/*.c; do
      o=".k1-out/cbrotli/$(echo "\$f" | sed 's|vendor/||; s|/|_|g; s|\\.c\$|.o|')"
      cc -O2 ${minver} -Ivendor/include -c "\$f" -o "\$o"
    done
    ar rcs libs/libbrotli.a .k1-out/cbrotli/*.o
    `
  let _ = process/run-command("/bin/sh", ["-c", script], :inherit).!
}
```

`dogfood/brotli`, the K1 port of the encoder, needs the C encoder's static
tables. Its setup step parses them out of the vendored C sources and writes
`tables.k1`, one of the module's own source files:

```k1 path=dogfood/brotli/build.k1
fn module(_b: k1/build-config): k1/module {
  let m = k1/module/new()
  m.executable()
  m.dep("cbrotli")
  m.setup(["tables.k1"], ["vendor"])
  m
}

// Regenerates tables.k1 from vendor/
fn setup(_ctx: k1/setup-ctx) { tablegen/generate() }
```

```k1 path=dogfood/brotli/build.k1
  fn generate() {
    let ees = files/read-to-string("$VENDOR/entropy_encode_static.h")
    let enc = files/read-to-string("$VENDOR/encode.c")
    let cf = files/read-to-string("$VENDOR/compress_fragment.c")
    let cf2 = files/read-to-string("$VENDOR/compress_fragment_two_pass.c")
    let fl = files/read-to-string("$VENDOR/fast_log.c")

    let w = string-builder/new()
    w.writeln("// Static tables for the q0/q1 encoders, extracted by fn setup (build.k1)")
    w.writeln("// from this module's vendored google/brotli enc/ sources (vendor/). The reps")
    w.writeln("// tables are truncated to 257 entries: the fast literal-tree store only sees")
    w.writeln("// runs within a 256-symbol alphabet.")
    w.writeln("")
    emit-int-table(w.&, ees, "kCodeLengthDepth", "k-code-length-depth", "u8", 0, 18)
    emit-int-table(w.&, ees, "kCodeLengthBits", "k-code-length-bits", "u32", 0, 18)
    emit-int-table(w.&, ees, "kZeroRepsDepth", "k-zero-reps-depth", "u32", 257, 16)
    emit-int-table(w.&, ees, "kZeroRepsBits", "k-zero-reps-bits", "u64", 257, 8)
    emit-int-table(w.&, ees, "kNonZeroRepsDepth", "k-non-zero-reps-depth", "u32", 257, 16)
    emit-int-table(w.&, ees, "kNonZeroRepsBits", "k-non-zero-reps-bits", "u64", 257, 8)
    emit-int-table(w.&, enc, "kDefaultCommandDepths", "k-default-command-depths", "u8", 0, 16)
    emit-int-table(w.&, enc, "kDefaultCommandBits", "k-default-command-bits", "u16", 0, 16)
    emit-int-table(w.&, enc, "kDefaultCommandCode", "k-default-command-code", "u8", 0, 12)
    emit-int-table(w.&, cf, "kCmdHistoSeed", "k-cmd-histo-seed", "u32", 0, 24)
    emit-int-table(w.&, cf2, "kNumExtraBits", "k-num-extra-bits", "u32", 0, 16)
    emit-int-table(w.&, cf2, "kInsertOffset", "k-insert-offset", "u32", 0, 12)
    emit-float-table(w.&, fl, "kBrotliLog2Table", "k-log2-table", 6)

    files/write-entire-file("tables.k1", w.build())
  }
```

`modules/libuv` goes further: its setup builds libuv with cmake and then runs
`k1bindgen` to regenerate two of its own source files, so the manifest lists
three outputs:

```k1 path=modules/libuv/build.k1
fn module(_b: k1/build-config): k1/module {
  let m = k1/module/new()
  m.lib("uv", :static)
  m.setup(
    ["libs/libuv.a", "uv.k1", "net.k1"],
    ["net.c", "uv_bindgen_prelude.k1.txt"]
  )
  m
}
```

What the compiler does with this (`src/k1/typer/host.rs`, `run_setups`;
`src/k1/compiler.rs`, `start_setup` and `finish_setup`): every `build.k1` is
compiled and run on the host before any program source is read, and setups
run in dependency order, because setup is what produces those sources.
`m.setup(outputs, inputs)` declares the files. The stamp at
`.k1-out/setup/stamp` records a header (stamp format version, target, a
content hash of `build.k1`, the declared output and input lists) and then
every file under every declared input and output with its size, mtime and a
content hash. The step is fresh when the header matches and every file's hash
matches; a file whose size and mtime are unchanged keeps its recorded hash
rather than being reread. When it is stale the compiler deletes the stamp and
the declared outputs, takes an advisory lock in `.k1-out/setup/lock` so a
background LSP compile and a CLI build cannot both run it, switches the
process cwd to the module directory, executes `fn setup` in the compile-time
VM, then collects the outputs, and fails with `did not produce declared
output` if one is missing. A run that fails leaves no stamp, so the next
compile retries. `k1 setup --force <dir>` reruns it by hand. This is
`dogfood/brotli`'s stamp:

```text
k1-setup-stamp v6
target: arm64-macos
build: 9d178fc3b7048c0f
outputs: tables.k1
inputs: vendor
input-file: vendor/compress_fragment.c 32875 1786845398543775859 b59cd712a8ec002d
input-file: vendor/compress_fragment_two_pass.c 26806 1786845398544307991 0792bdd0dc01e128
input-file: vendor/encode.c 79374 1786845398544961792 726816900a3b3b04
input-file: vendor/entropy_encode_static.h 33144 1786845398545601634 73c55b0d24bc6a7e
input-file: vendor/fast_log.c 6046 1786845398546112724 3b53350a14dff339
output-file: tables.k1 15110 1790120636986915457 a33416ea8ec917f1
```

Forcing the step reruns the table extraction in the VM and rewrites
`tables.k1` byte-identically, in about 40 milliseconds:

```text
$ time k1 setup --force dogfood/brotli
Setting up module 'brotli' (running fn setup in /Users/knix/dev/k1/dogfood/brotli/build.k1)...
k1 setup --force dogfood/brotli 2>&1  0.03s user 0.00s system 0.04 total
$ diff dogfood/brotli/tables.k1 tables_before.k1 && echo byte-identical
byte-identical
```

Rust's `build.rs` and Zig's `build.zig` are also written in the host
language, but each is compiled to a separate host binary and run as a
process. Here the step is interpreted by the compiler that is already
running, with the same string, file and process APIs as the program, and the
staleness rule is content hashing over declared inputs and outputs rather
than a list of `rerun-if-changed` hints.

## Exporting a K1 library to C

A library module marks what it exports; everything else, the runtime's C
helpers included, is hidden:

```k1 path=dogfood/klib/build.k1
fn module(_b: k1/build-config): k1/module {
  let m = k1/module/new()
  m.library()
  m
}
```

```k1 path=dogfood/klib/module.k1
type pair = { a: i32, b: i32 }

fn(export) klib_add(a: i32, b: i32): i32 { a + b }

fn(export("klib_scale")) scale-by-two(x: i32): i32 { x * 2 }

fn(export) klib_pair_swap(p: pair): pair { .{ a = p.b, b = p.a } }

fn(export) klib_sum_to(n: i64): i64 {
  let items = list/empty[i64]()
  for i in 1.until(n + 1) { items.push(i) }
  let sum: i64 = 0
  for x in items { sum = sum + x }
  sum
}

let(export) klib_answer: i32 = 42
```

The C side declares the same five symbols and links either artifact:

```c
typedef struct {
  int32_t a;
  int32_t b;
} klib_pair;

extern int32_t klib_add(int32_t a, int32_t b);
extern int32_t klib_scale(int32_t x);
extern klib_pair klib_pair_swap(klib_pair p);
extern int64_t klib_sum_to(int64_t n);
extern int32_t klib_answer;
```

```make
build:
	clang consumer.c $(OUT)/libklib.a $(LIBS) -o consumer_static.a
	clang consumer.c -L$(OUT) -lklib -Wl,-rpath,$(abspath $(OUT)) -o consumer_dylib.$(DYLIB_EXT)
```

`k1 build` of a library module writes `.k1-out/libklib.dylib` (or `.so`) and
a fat `.k1-out/libklib.a`, a partial link of the K1 object with every static
library in the program, k1rt included, plus `klib.exports`, the symbol list
handed to the linker:

```text
_klib_add
_klib_answer
_klib_pair_swap
_klib_scale
_klib_sum_to
```

Built and run as `test.sh` does:

```text
$ k1 --cache false build dogfood/klib
$ make -C dogfood/klib/consumer clean run
rm -f consumer_static.a consumer_dylib.dylib out_static.txt out_dylib.txt
clang consumer.c ../.k1-out/libklib.a  -o consumer_static.a
clang consumer.c -L../.k1-out -lklib -Wl,-rpath,/Users/knix/dev/k1/dogfood/klib/.k1-out -o consumer_dylib.dylib
./consumer_static.a > out_static.txt
./consumer_dylib.dylib > out_dylib.txt
diff out_static.txt out_dylib.txt
diff out_static.txt expected.txt
$ cat dogfood/klib/consumer/out_static.txt
add=5
scale=42
swap=2,1
sum=55
answer=42
```

`klib_sum_to` allocates a list in K1's ambient arena from inside a C
program whose `main` makes no runtime init call.

## Hot reload

A namespace marked `ns(reload)` can be swapped in a running process:

```k1 path=dogfood/reload_test/app/scene.k1
ns(reload) scene {
  let(mutable) speed: i64 = 10
  fn greeting(): string { "one" }
  fn helper(x: i64): i64 { x + 1 }
}
```

The compiler gives every reloadable namespace three functions: `load()`,
`watch()` and `loaded-version()`. The app calls them like any other:

```k1 path=dogfood/reload_test/app/app.k1
    if line == "load" {
      if scene/load() is {
        :ok -> println("load ok"),
        :err e -> println("load err: ${e}"),
      }
    } else if line == "watch" {
      if scene/watch() is {
        :ok -> println("watch ok"),
        :err e -> println("watch err: ${e}"),
      }
    } else if line == "greet" {
      println("greet: ${scene/greeting()}")
    } else if line == "speed" {
      println("speed: ${scene/speed}")
```

Calls into a reloadable namespace go through a per-function address slot.
`k1 build` emits the namespace as its own dylib next to the executable,
together with a hash of the namespace's API (signatures and globals). The
runtime half is plain K1 in `modules/std/reload.k1`: `load` copies the
artifact to a unique temp path (loaders cache images by path), `dlopen`s it,
compares the API hash, resolves every symbol, and only then patches the
slots, atomically:

```k1 path=modules/std/reload.k1
    // Resolve every fn before patching anything, so a load fully succeeds or does not occur
    let entries = span/wrap-raw[entry](descriptor.entries, descriptor.entry-count.signed())
    let addrs = list/with-capacity[ptr](entries.len())
    for e in entries {
      let addr = if core/platform/dylib/sym(handle, string/wrap-c(e.symbol)) is {
        :some(a) -> a,
        :none -> return :err :symbol-missing .{
          symbol = heap-string/cloned(string/wrap-c(e.symbol)),
          path = heap-string/cloned(dylib-path),
        }
      }
      addrs.push(addr)
    }
    for e in entries {
      atomic/store(e.slot, addrs.[it-index], :release)
    }
    let version = atomic/fetch-add(descriptor.version, 1, :seq-cst) + 1
```

`watch` starts a thread on `std/watch` (kqueue or inotify) over the artifact
directory and loads whenever the file's content hash changes. An API change
is refused rather than half-applied. `dogfood/reload_test` drives all of
this across independent compiler runs: it writes a scene, builds the app,
talks to it over a pipe, rebuilds with new bodies, then with a changed
signature, then with an added global, then lets `watch` pick up a rebuild:

```k1 path=dogfood/reload_test/reload_test.k1
  // Signature change on a fn main never calls: hash drifts, load refuses,
  // nothing is patched
  build-app(k1-exe, "three", true, 20, false)
  send(app.&, "load")
  expect(app.&, "load err: restart required: api of app/scene changed")
  send(app.&, "greet")
  expect(app.&, "greet: two")
```

```text
$ K1_EXE=$(which k1) k1 run dogfood/reload_test
[INFO  k1] run executable: reload_test
reload: app/scene swapped to v1 (3 syms)
reload: app/scene swapped to v2 (3 syms)
reload: app/scene swapped to v3 (3 syms)
reload_test ok
```

## The same programs on wasm

`--target wasm64-wasi` builds a wasm64 module against `libk1rt-wasm.a`
(`make -C modules/core/libs wasm`; already built here, in the repo and in
the installed `~/.k1/modules/core/libs`), and `k1 run` executes it under
`wasmtime`. The SIMD example above, unchanged:

```text
$ k1 --optimize --cache false --target wasm64-wasi run content/showcase/examples/sys_simd.k1
compile-time scan found 4 delimiters
simd width: 16 bytes; buffer: 8 MB
scalar scan: 145794 found, 2916 MB/s
first-of scan: 145794 found, 4958 MB/s
scalar count ',': 137380 found, 1039 MB/s
vector count ',': 137380 found, 23657 MB/s
```

`k1/simd-bytes` is 16 on wasm too, so the same macro expansion runs on
wasm's 128-bit SIMD. The whole language test suite runs this way as part of
`test.sh`:

```text
$ k1 --optimize --cache false --target wasm64-wasi run test_src/suite1
[INFO  k1] run executable: suite1
[suite1] All tests passed!
```

`dogfood/fractal` is one program that is a terminal renderer natively and
under wasmtime, and a canvas renderer in the browser, from the same `.wasm`.
It exports two functions to JavaScript with the same `export` modifier the C
library used:

```k1 path=dogfood/fractal/fractal.k1
fn(export("k1_frame")) frame-address(width: i32, height: i32): u32 {
  ensure-frame(width, height)
  frame.data-ptr().to-size().trunc[u32]
}
```

```text
$ k1 --optimize --cache false --target wasm64-wasi run dogfood/fractal
[INFO  k1] run executable: fractal
k1 escape-time fractals, one binary for terminal and canvas
  mandelbrot at -0.65, 0.0 span 2.4
  julia at 0.0, 0.0 span 3.0
  burning ship at -1.755, -0.03 span 0.22
$ file dogfood/fractal/.k1-out/fractal
dogfood/fractal/.k1-out/fractal: WebAssembly (wasm) binary module version 0x1 (MVP)
```

`index.html` fetches that same file, supplies the handful of WASI imports it
needs, calls `_start` so `main` prints the montage into the page, then wraps
the framebuffer `k1_frame` returns in an `ImageData`. The `Justfile` recipes
are the whole workflow:

```text
fractal-wasi:
  make -C modules/core/libs wasm
  just run-frag --optimize --target wasm64-wasi run dogfood/fractal

fractal-web:
  make -C modules/core/libs wasm
  just run-frag -D web build dogfood/fractal
  python3 -m http.server -d dogfood/fractal 8088
```

## Bare metal

`dogfood/freestanding_lib` pins its own build: its `fn build` sets the target
to `intel64-bare` and drops `std`, so the OS is gone too. It is a K1 library
whose consumer is a freestanding C bootstrap with its own `_start` and raw
syscalls.

```k1 path=dogfood/freestanding_lib/build.k1
fn build(req: k1/build-request): k1/build-config {
  let b = req.default
  b.target = :intel64-bare
  b.no-std = true
  b
}

fn module(_b: k1/build-config): k1/module {
  let m = k1/module/new()
  m.library()
  m
}
```

```k1 path=dogfood/freestanding_lib/module.k1
fn(export) k1_add(a: i64, b: i64): i64 { a + b }

fn(export) k1_sum(data: ptr, count: size): i64 {
  let sum: i64 = 0
  for i in 0.until(count) {
    sum = sum + data.add-bytes(i * 8).ref[i64].*
  }
  sum
}

fn(export) k1_describe_sum(data: ptr, count: size): i64 {
  let total = k1_sum(data, count)
  println("k1 sum: ${total}")
  total
}
```

Cross-built from macOS, the object's undefined symbols are the entire
platform contract the library asks of its host:

```text
$ k1 --cache false build dogfood/freestanding_lib
$ llvm-nm --undefined-only dogfood/freestanding_lib/.k1-out/freestanding_lib.o
                 U k1_platform_io_is_tty
                 U k1_platform_io_write
                 U k1_platform_mem_commit
                 U k1_platform_mem_reserve
                 U k1_platform_process_exit
                 U memcpy
$ llvm-nm --defined-only --extern-only dogfood/freestanding_lib/.k1-out/freestanding_lib.o
0000000000001c00 T k1_add
0000000000001d20 T k1_describe_sum
0000000000001c10 T k1_sum
$ file dogfood/freestanding_lib/.k1-out/freestanding_lib.o
dogfood/freestanding_lib/.k1-out/freestanding_lib.o: ELF 64-bit LSB relocatable, x86-64, version 1 (SYSV), with debug_info, not stripped
```

`println` inside `k1_describe_sum` reaches the host through
`k1_platform_io_write`; the arena allocator through `mem_reserve` and
`mem_commit`. The consumer defines those five functions over Linux syscalls
in about forty lines of C:

```c
void *k1_platform_mem_reserve(unsigned long min_size) {
  void *p = sys_mmap(min_size);
  if ((long)p < 0)
    sys_exit(90);
  return p;
}
void k1_platform_mem_commit(void *base, unsigned long len) {
  (void)base;
  (void)len;
}
long k1_platform_io_write(int fd, const void *buf, unsigned long n) {
  return sys_write(fd, buf, n);
}
```

The `Justfile`'s `ts-freestanding` recipe asserts that undefined-symbol list
with `llvm-nm`, links the consumer with `ld.lld` against
`libk1rt-nocrt.a` (`make -C modules/core/libs nocrt`; present here), and runs
the binary in an `alpine` container, expecting `k1 sum: 100` from K1's
`println` and `103` from the C bootstrap's own writer:

```text
ts-freestanding:
  make -C modules/core/libs nocrt
  just run-frag --no-std --target intel64-bare --cache false build dogfood/freestanding_lib
  llvm/install-llvm/bin/llvm-nm --undefined-only dogfood/freestanding_lib/.k1-out/freestanding_lib.o | awk '$2 !~ /^k1_platform_/ && $2 !~ /^(memcpy|memmove|memset|memcmp|bcmp)$/' | awk 'END { exit NR != 0 }'
  llvm/install-llvm/bin/clang --target=x86_64-unknown-linux-gnu -ffreestanding -nostdinc -O2 -fno-stack-protector -c dogfood/freestanding_lib/consumer/consumer.c -o dogfood/freestanding_lib/.k1-out/consumer.o
  llvm/install-llvm/bin/ld.lld -z separate-loadable-segments dogfood/freestanding_lib/.k1-out/consumer.o dogfood/freestanding_lib/.k1-out/freestanding_lib.o modules/core/libs/libk1rt-nocrt.a -o dogfood/freestanding_lib/.k1-out/consumer_bin
  docker run --rm --platform linux/amd64 -v {{justfile_directory()}}/dogfood/freestanding_lib/.k1-out:/w alpine:latest /w/consumer_bin | grep -cxE 'k1 sum: 100|103' | grep -x 2
```

The build and the symbol check were run for this section; the docker step
was not.
