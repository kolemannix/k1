# Systems programming

k1 compiles to machine code through LLVM and speaks the C ABI. SIMD lanes are
a type, atomics are functions over ordinary memory, an `extern` is a binding,
and a module's build step is k1 running in the compiler's VM. Then the same
programs go to wasm and bare metal.

## A SIMD scanner generated at compile time

One line asks the standard library for a delimiter scanner:

```k1
$std/simd/first-of(delims, [',', ':', '"'])
```

That expands into a `delims/scan(data, from)` you call like anything else:

```k1
fn scan-all-simd(text: string): u64 {
  let found: u64 = 0
  let from: size = 0
  loop {
    let hit = delims/scan(text.span-bytes(), from)
    if hit.index < 0 break
    found = found + 1
    from = hit.index + 1
  }
  found
}
```

`first-of` is a macro in `std/simd`, and its body is ordinary k1 writing k1
source into a string builder:

```k1 path=modules/std/simd.k1
macro first-of(name: code, needles: span[char]) {
  code/from-string(first-of-impl(name.text, needles))
}
```

The heart of the generator: load a vector, compare against one splatted
needle per delimiter, OR them, and only on a hit decode the masks.

```k1 path=modules/std/simd.k1
  code.writeln("    while i + $lanes <= len {")
  code.writeln("      let chunk = vector/load-unchecked[u8, $lanes](base.ref-at[u8](i).as[ptr])")
  for needles {
    code.writeln("      let e$it-index = chunk.eq-lanes(needle$it-index)")
  }
  let any-lanes = string-builder/new()
  let any-masks = string-builder/new()
  for needles {
    if it-index > 0 {
      any-lanes.write(".bit-or(e$it-index)")
      any-masks.write(".bit-or(m$it-index)")
    } else {
      any-lanes.write("e0")
      any-masks.write("m0")
    }
  }
  code.writeln("      if ${any-lanes.build()}.any() {")
  for needles {
    code.writeln("        let m$it-index = e$it-index.to-mask()")
  }
  code.writeln("        let lane = ${any-masks.build()}.trailing-zeros()")
  for needles {
    if it-index < last {
      code.writeln("        if m$it-index.shift-right(lane.trunc[u32]).bit-and(1) == 1 {")
      code.writeln("          return .{ index = i + lane.signed(), which = $it-index }")
      code.writeln("        }")
    }
  }
  code.writeln("        return .{ index = i + lane.signed(), which = $last }")
  code.writeln("      }")
  code.writeln("      i = i + $lanes")
  code.writeln("    }")
```

`$lanes` is `k1/simd-bytes`, the target's natural vector width: 16 on
arm64-macos and wasm64, 32 on an AVX2 target, same macro.

The vector ops are compiler intrinsics, declared in core:

```k1 path=modules/core/vector.k1
  fn(intern) to-mask[t, n: static size](v: vector[t, n]): u64
```

`trailing-zeros` isn't even that. It's a raw LLVM intrinsic, declared in k1:

```k1 path=modules/core/core.k1
  fn(intern("llvm.cttz.i64")) _cttz(x: u64, zero-poison: bool): u64

  fn trailing-zeros(self: u64): u64 {
    _cttz(self, false)
  }
```

Any k1 file can do that. The VM emulates the common ones and runs vector ops
on its own memory, so the generated scanner runs at compile time too:

```k1
  let static-hits: 4u64 = #static scan-all-simd("{\"key\": [1, 2]}")
  println("compile-time scan found ${static-hits.from-static()} delimiters")
```

`4u64` is a literal type, so the compiler checks the answer while
typechecking. My first draft said `3`:

```text
│ ->  let static-hits: 3 = #static scan-all-simd("{\"key\": [1, 2]}")
│                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
│     println("compile-time scan found ${static-hits.from-static()} delimiters")
│   
├─────
│  Expected value type static[i64, 3] but got value type static[u64, 4]
```

No intrinsics header, no `#ifdef`, no second language: the generator is k1,
the output is k1, and it runs natively, on wasm, and inside the compiler.

`vector[u8, 16]` by hand is the same operations. Counting a byte, with a
vector accumulator flushed before the `u8` lanes wrap:

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

Both kernels against plain scalar loops over 8 MB, best of three, with
`atomic/fence`s around each rep so nothing gets hoisted past the timer:

```k1
    let start = time/read-cpu-counter()
    atomic/fence(:seq-cst)
    let r = run(rep)
    atomic/fence(:seq-cst)
    let nanos = time/elapsed-nanos(start)
```

Unoptimized:

```text
compile-time scan found 4 delimiters
simd width: 16 bytes; buffer: 8 MB
scalar scan: 145794 found, 208 MB/s
first-of scan: 145794 found, 1342 MB/s
scalar count ',': 137380 found, 465 MB/s
vector count ',': 137380 found, 3902 MB/s
```

With `--optimize`:

```text
compile-time scan found 4 delimiters
simd width: 16 bytes; buffer: 8 MB
scalar scan: 145794 found, 3016 MB/s
first-of scan: 145794 found, 6114 MB/s
scalar count ',': 137380 found, 7484 MB/s
vector count ',': 137380 found, 33847 MB/s
```

The optimized scalar count is LLVM auto-vectorizing; the hand-written one
still wins four to one. Apple M3 Max, macOS 26.6.2, `arm64-macos`.

## Threads, atomics and thread-locals

```k1
type shared = { atomic-total: u64, lock: u64, locked-total: u64 }

let(mutable, tls) my-bumps: u64 = 0

fn worker(s: *shared): u64 {
  for 0u64.until(N_ITERS) {
    atomic/fetch-add(s.atomic-total.&, 1, :relaxed)
    my-bumps = my-bumps + 1
  }
  while not atomic/cmpxchg(s.lock.&, 0: u64, 1, :acquire, :relaxed).ok {}
  s.locked-total = s.locked-total + my-bumps
  atomic/store(s.lock.&, 0: u64, :release)
  my-bumps
}
```

Atomics are operations, not types: `fetch-add` takes a plain `u64` field, and
a `cmpxchg` spin lock guards the plain field next to it. `let(mutable, tls)`
is the whole thread-local story.

```k1
  let s = mem/new(shared.0)
  let tasks: list[task[u64]] = []
  for 0.until(N_THREADS) {
    tasks.push(task/spawn(fn[s]. worker(s)))
  }
  for tasks {
    let got = it.join()
    println("thread $it-index bumped its thread-local $got times")
  }
  println("atomic total: ${s.atomic-total}")
  println("locked total: ${s.locked-total}")
  println("main's thread-local: $my-bumps")
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

`task` isn't built in. It's a few lines of `std/thread` over `thread/start`,
which only knows the C shape of a thread entry: one `*fn(*t) -> ptr`, one
argument.

```k1 path=modules/std/thread.k1
type task-state[f, t] = { body: f, result: *t }

type task[t] = { thread: thread, result: *t }

ns for task {

  fn spawn[t](body: some fn() -> t): task[t] {
    let result = heap.alloc-t[t]()
    let state = heap.new(task-state[type-of(body), t].{ body = body, result = result })
    let thread: thread = ptr/null
    start(thread.&, fn(s: *task-state[type-of(body), t] -> ptr) {
      s.result.* = (s.body)()
      heap.free-t(s)
      ptr/null
    }, state)
    .{ thread = thread, result = result }
  }
```

`some fn() -> t` specializes `spawn` per closure (see
[closures](09-model.md#closures)), so `type-of(body)` is one concrete type and
`task-state` holds the closure by value: no `dyn`, no vtable, no `void *`.
The entry lambda captures nothing, so it coerces to the bare function pointer
`start` wants. Under `--optimize`, `worker` inlines straight into the
function the new thread starts in.

## FFI with no bindings file

Three declarations, no header, no build step:

```k1
fn(extern("strlen")) strlen(s: *u8): size
fn(extern("getpid")) getpid(): c-int
fn(extern("qsort")) qsort(base: ptr, count: size, width: size, compare: *fn(ptr, ptr) -> c-int)
```

A k1 function's address is a C function pointer:

```k1
fn compare-i32(a: ptr, b: ptr): c-int {
  let x = a.ref[i32].*
  let y = b.ref[i32].*
  if x < y { -1 } else if x > y { 1 } else { 0 }
}
```

so `compare-i32.&` goes straight into `qsort`, sorting the list's storage in
place:

```k1
  let greeting = "hello from libc".to-c-tmp()
  println("strlen: ${strlen(greeting)}")

  let xs: list[i32] = [40, 7, 23, 1, 99, 15]
  qsort(xs.as-span().data-ptr(), xs.len(), types/size[i32], compare-i32.&)
  println("qsort: $xs")

  println("getpid > 0: ${getpid() > 0}")
```

```text
strlen: 15
qsort: [1,7,15,23,40,99]
getpid > 0: true
```

`test_src/ffi_abi_test` pins the ABI corners against a C library: odd-sized
structs by value, float aggregates, packed structs, tagged unions, callbacks
taking structs, extern globals and thread-locals.

```k1 path=test_src/ffi_abi_test/ffi_abi_test.k1
  type rgb3 = { r: u8, g: u8, b: u8 }
  fn(extern("rgb3")) rgb3(a: rgb3, b: rgb3): rgb3
```

```k1 path=test_src/ffi_abi_test/ffi_abi_test.k1
  let(extern("abi_readonly")) readonly: i32
  let(extern("abi_counter"), mutable) counter: i32
  let(extern("abi_tls_counter"), mutable, tls) tls-counter: i32
```

A library other than libc is named once, on the namespace or per function:

```k1 path=examples/brotli_parity/deps/cbrotli/cbrotli.k1
fn(extern("BrotliEncoderCompress"), lib("brotli")) encoder-compress(
  quality: c-int,
  lgwin: c-int,
  mode,
  input-size: size,
  input: ptr,
  encoded-size: *size,
  encoded: ptr,
): c-int
```

Big APIs get generated. `tools/k1bindgen` reads C headers through libclang
and writes k1. Its libuv output:

```k1 path=modules/uv/libuv.k1
fn(extern("uv_default_loop")) default_loop(): optref[uv-loop]
fn(extern("uv_loop_init")) loop_init(
  loop_c: *uv-loop,
): i32
fn(extern("uv_loop_close")) loop_close(
  loop_c: *uv-loop,
): i32
```

`*t` is never null in k1, so a returned C pointer comes back as
`optref[uv-loop]`, one word you have to check. Every generated struct carries
libclang's layout as a compile-time assertion, collected in one `#static`
block at the end of the file that compiles under `k1 test`, so drift from the
C definition fails the tests without costing every build:

```k1 path=modules/uv/net.k1
type sockaddr_in = {
  sin_len: u8,
  sin_family: sa_family_t,
  sin_port: in_port_t,
  sin_addr: in_addr,
  sin_zero: array[u8, 8],
}
```

```k1 path=modules/uv/net.k1
#if k1/test #static {
  assert-layout[in_addr_t](4, 4, [])
  assert-layout[in_port_t](2, 2, [])
  assert-layout[sa_family_t](1, 1, [])
  assert-layout[sockaddr](16, 1, [0, 1, 2, ])
  assert-layout[in_addr](4, 4, [0, ])
  assert-layout[sockaddr_in](16, 4, [0, 1, 2, 4, 8, ])
  assert-layout[in6_addr](16, 4, [0, ])
  assert-layout[sockaddr_in6](28, 4, [0, 1, 2, 4, 8, 24, ])
  assert-layout[addrinfo](48, 8, [0, 4, 8, 12, 16, 24, 32, 40, ])
}
```

## Build steps are k1 running inside the compiler

A module's `build.k1` is compiled on the host before any of the program.
`fn module(b)` returns the manifest; `fn setup(ctx)` is an optional build
step. The parity driver's `cbrotli` dep builds its vendored C encoder:

```k1 path=examples/brotli_parity/deps/cbrotli/build.k1
fn module(_b: k1/build-config): k1/module {
  let m = k1/module/new()
  m.lib("brotli", :static)
  m.setup(["libs/libbrotli.a"], ["vendor"])
  m
}
```

```k1 path=examples/brotli_parity/deps/cbrotli/build.k1
fn setup(ctx: k1/setup-ctx) {
  let minver = if (ctx.build.target is :arm64-macos) "-mmacosx-version-min=15.0.0" else ""
  let script = `
    set -e
    mkdir -p libs .k1-out/cbrotli
    for f in vendor/common/*.c vendor/enc/*.c; do
      o=".k1-out/cbrotli/$(echo "\$f" | sed 's|vendor/||; s|/|_|g; s|\\.c\$|.o|')"
      cc -O2 ${minver} -Ivendor/include -c "\$f" -o "\$o"
    done
    ar rcs libs/libbrotli.a .k1-out/cbrotli/*.o
    `
  let _ = process/run-command("/bin/sh", ["-c", script], :inherit).!
}
```

`modules/brotli`, the k1 port of the encoder and decoder, needs the C
library's static tables. Its setup parses them out of the C sources and writes one of its own
source files, `tables.k1`:

```k1 path=modules/brotli/build.k1
fn module(_b: k1/build-config): k1/module {
  let m = k1/module/new()
  m.setup(["tables.k1"], ["vendor"])
  m
}
```

```k1 path=modules/brotli/build.k1
    emit-int-table(w.&, dict-hash, "kStaticDictionaryHashWords", "k-dictionary-hash-words", "u16", 24)
    emit-int-table(w.&, dict-hash, "kStaticDictionaryHashLengths", "k-dictionary-hash-lengths", "u8", 32)
    emit-int-table(w.&, dict-lut, "kStaticDictionaryBuckets", "k-dict-lut-buckets", "u16", 24)
    emit-packed-table(w.&, dict-lut, "kStaticDictionaryWords", "k-dict-lut-words", "u32", 3,
      [.{ index = 0, shift = 0 }, .{ index = 1, shift = 8 }, .{ index = 2, shift = 16 }], 16)
    emit-int-table(w.&, dec, "kCodeLengthCodeOrder", "k-code-length-code-order", "u8", 18)
    emit-int-table(w.&, dec, "kCodeLengthPrefixLength", "k-code-length-prefix-length", "u8", 16)
    emit-int-table(w.&, dec, "kCodeLengthPrefixValue", "k-code-length-prefix-value", "u8", 16)
    emit-int-table(w.&, huff, "kReverseBits", "k-reverse-bits", "u8", 16)
    let prefix-suffix = string-values(transform, "kPrefixSuffix")
    w.writeln("let k-prefix-suffix: array[u8, ${prefix-suffix.len()}] = [")
    write-rows(w.&, prefix-suffix, 32)
    emit-int-table(w.&, transform, "kPrefixSuffixMap", "k-prefix-suffix-map", "u16", 25)
    emit-packed-table(w.&, transform, "kTransformsData", "k-transforms", "u32", 3,
      [.{ index = 0, shift = 0 }, .{ index = 1, shift = 8 }, .{ index = 2, shift = 16 }], 16)

    files/write-entire-file("tables.k1", w.build()).!
```

`modules/uv` builds libuv with cmake and then runs `k1bindgen` to
regenerate two of its own source files:

```k1 path=modules/uv/build.k1
fn module(_b: k1/build-config): k1/module {
  let m = k1/module/new()
  m.lib("uv", :static)
  m.setup(
    ["libs/libuv.a", "libuv.k1", "net.k1"],
    ["net.c", "uv_bindgen_prelude.k1.txt"]
  )
  m
}
```

Setups run in dependency order before any program source is read, since
they produce that source. `m.setup(outputs, inputs)` declares the files, and
staleness is a content hash over all of them, recorded in a stamp:

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

A stale step deletes its outputs, takes a lock (so the LSP and a CLI build
can't both run it), runs `fn setup` in the VM with cwd set to the module, and
fails if a declared output is missing. Forcing it reruns the table
extraction and rewrites `tables.k1` byte-identically in 40ms:

```text
$ time k1 setup --force modules/brotli
Setting up module 'brotli' (running fn setup in /Users/knix/dev/k1/modules/brotli/build.k1)...
k1 setup --force modules/brotli 2>&1  0.03s user 0.00s system 0.04 total
$ diff modules/brotli/tables.k1 tables_before.k1 && echo byte-identical
byte-identical
```

Rust's `build.rs` and Zig's `build.zig` are host-language too, but compiled to
a separate binary and run as a process, with `rerun-if-changed` hints. This
one runs in the compiler that's already running.

## Exporting a k1 library to C

Mark the module a library and mark what it exports. Everything else, the
runtime's C helpers included, stays hidden.

```k1 path=examples/klib/build.k1
fn module(_b: k1/build-config): k1/module {
  let m = k1/module/new()
  m.library()
  m
}
```

```k1 path=examples/klib/module.k1
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

The C side:

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

`k1 build` writes a dylib and a fat static archive (the k1 object partially
linked with every static lib, k1rt included), and C links either one:

```text
$ k1 --no-cache build examples/klib
$ make -C examples/klib/consumer clean run
rm -f consumer_static.a consumer_dylib.dylib out_static.txt out_dylib.txt
clang consumer.c ../.k1-out/libklib.a  -o consumer_static.a
clang consumer.c -L../.k1-out -lklib -Wl,-rpath,/Users/knix/dev/k1/examples/klib/.k1-out -o consumer_dylib.dylib
./consumer_static.a > out_static.txt
./consumer_dylib.dylib > out_dylib.txt
diff out_static.txt out_dylib.txt
diff out_static.txt expected.txt
$ cat examples/klib/consumer/out_static.txt
add=5
scale=42
swap=2,1
sum=55
answer=42
```

`klib_sum_to` allocates in k1's arena from a C `main` that never initializes
any runtime.

## Hot reload

Mark a namespace `ns(reload)` and its functions and globals can be swapped
under a live process:

```k1
ns(reload) handlers {
  let(mutable) HITS: i64 = 0

  fn handle-request(path: string): string {
    HITS = HITS + 1
    if (path == "/") "200 <h1>hello</h1>" else "404 $path"
  }
}
```

```k1
fn main() {
  handlers/watch().!
  for path in ["/", "/about", "/"] {
    println("${handlers/handle-request(path)}  hits=${handlers/HITS} v${handlers/loaded-version()}")
  }
}
```

```text
reload: sys_reload/handlers swapped to v1 (2 syms)
200 <h1>hello</h1>  hits=1 v1
404 /about  hits=2 v1
200 <h1>hello</h1>  hits=3 v1
```

Edit `handle-request`, run `k1 build`, and the next request runs the new
body, with `HITS` intact. The namespace also gets `load()` for a reload on
demand:

```k1 path=examples/reload_test/app/app.k1
    } else if line == "load" {
      if scene/load() is {
        :ok -> println("load ok"),
        :err e -> println("load err: ${e}"),
      }
    } else if line == "watch" {
      if scene/watch() is {
        :ok -> println("watch ok"),
        :err e -> println("watch err: ${e}"),
      }
```

`k1 build` emits the namespace as its own dylib with a hash of its API, and
calls into it go through per-function address slots. The runtime half is
plain k1 in `std/reload`: `dlopen`, check the hash, resolve every symbol,
and only then patch the slots:

```k1 path=modules/std/reload.k1
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

`watch` does the same on a kqueue/inotify thread whenever the file changes.
`examples/reload_test` rebuilds the app under a live process with new bodies,
an added global, and a changed signature, which gets refused:

```k1 path=examples/reload_test/reload_test.k1
  build-app(k1-exe, "three", true, 20, false)
  send(app.&, "load")
  expect(app.&, "load err: restart required: api of app/scene changed")
  send(app.&, "greet")
  expect(app.&, "greet: two")
```

```text
$ K1_EXE=$(which k1) k1 run examples/reload_test
[INFO  k1] run executable: reload_test
reload: app/scene swapped to v1 (3 syms)
reload: app/scene swapped to v2 (3 syms)
reload: app/scene swapped to v3 (3 syms)
reload_test ok
```

## The same programs on wasm

`--target wasm64-wasi`, and `k1 run` executes under wasmtime (47.0.3). The SIMD
example, unchanged:

```text
$ k1 --optimize --no-cache --target wasm64-wasi run content/showcase/examples/sys_simd.k1
compile-time scan found 4 delimiters
simd width: 16 bytes; buffer: 8 MB
scalar scan: 145794 found, 2874 MB/s
first-of scan: 145794 found, 5103 MB/s
scalar count ',': 137380 found, 1399 MB/s
vector count ',': 137380 found, 25006 MB/s
```

The whole language test suite runs this way in `test.sh`:

```text
$ k1 --optimize --no-cache --target wasm64-wasi run test_src/suite1
[INFO  k1] run executable: suite1
[suite1] All tests passed!
```

`examples/fractal` is one `.wasm` that renders to the terminal under wasmtime
and to a canvas in the browser. It exports to JavaScript with the same
`export` the C library used:

```k1 path=examples/fractal/fractal.k1
fn(export("k1_frame")) frame-address(width: i32, height: i32): u32 {
  ensure-frame(width, height)
  frame.data-ptr().to-size().trunc[u32]
}
```

```text
$ k1 --optimize --no-cache --target wasm64-wasi run examples/fractal
[INFO  k1] run executable: fractal
k1 escape-time fractals, one binary for terminal and canvas
  mandelbrot at -0.65, 0.0 span 2.4
  julia at 0.0, 0.0 span 3.0
  burning ship at -1.755, -0.03 span 0.22
$ file examples/fractal/.k1-out/fractal
examples/fractal/.k1-out/fractal: WebAssembly (wasm) binary module version 0x1 (MVP)
```

`index.html` loads that same file, stubs a handful of WASI imports, and
paints the framebuffer `k1_frame` returns.

## Bare metal

`examples/freestanding_lib` pins its own target and drops `std`:

```k1 path=examples/freestanding_lib/build.k1
fn build(req: k1/build-request): k1/build-config {
  let b = req.default
  b.target = :intel64-bare
  b.no-std = true
  b
}
```

```k1 path=examples/freestanding_lib/module.k1
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

Cross-built from macOS, the undefined symbols are the entire platform
contract:

```text
$ k1 --no-cache build examples/freestanding_lib
$ llvm-nm --undefined-only examples/freestanding_lib/.k1-out/freestanding_lib.o
                 U k1_platform_io_is_tty
                 U k1_platform_io_write
                 U k1_platform_mem_commit
                 U k1_platform_mem_reserve
                 U k1_platform_process_exit
                 U memcpy
$ llvm-nm --defined-only --extern-only examples/freestanding_lib/.k1-out/freestanding_lib.o
0000000000001c00 T k1_add
0000000000001d20 T k1_describe_sum
0000000000001c10 T k1_sum
$ file examples/freestanding_lib/.k1-out/freestanding_lib.o
examples/freestanding_lib/.k1-out/freestanding_lib.o: ELF 64-bit LSB relocatable, x86-64, version 1 (SYSV), with debug_info, not stripped
```

A freestanding C bootstrap with its own `_start` supplies them over raw
Linux syscalls in about forty lines:

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

`just ts-freestanding` checks that symbol list, links with `ld.lld` against
`libk1rt-nocrt.a`, and runs it in an alpine container, expecting `k1 sum: 100`
from k1's `println`.
