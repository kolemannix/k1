# The compile-time VM can call C

Every top-level `let` in K1 is a compile-time program: its initializer runs
in the compiler's VM and the result is baked into the binary as a constant.
The VM speaks the C ABI through libffi, so those initializers can call C. A
build stamp is the smallest possible demonstration: C gets it from
`__DATE__`, `__TIME__` and a `-DHOST=` flag the build system has to compute;
K1 just calls libc.

```k1
use std/libc

fn(extern("gethostname")) gethostname(name: ptr, len: size): i32
fn(extern("getpid")) getpid(): i32
fn(extern("time")) c-time(tloc: ptr): i64
fn(extern("localtime")) localtime(t: *i64): ptr
fn(extern("strftime")) strftime(s: ptr, max: size, format: ptr, tm: ptr): size

fn hostname(): string {
  let buf = buffer/allocate[u8](256)
  if gethostname(buf.data-ptr(), buf.len()) != 0 crash("gethostname failed")
  string/wrap-c(buf.data-ptr())
}

fn local-time-string(): string {
  let now = c-time(ptr/null)
  let out = buffer/allocate[u8](64)
  let n = strftime(out.data-ptr(), out.len(), "%Y-%m-%d %H:%M:%S %Z".to-c().ptr, localtime(now.&))
  if n == 0 crash("strftime failed")
  string/wrap-c(out.data-ptr())
}

fn env-or(name: string, fallback: string): string {
  let v = libc/getenv(name.to-c().ptr)
  if v is null fallback else string/wrap-c(v)
}

let BUILD_HOST: string = hostname()
let BUILD_USER: string = env-or("USER", "nobody")
let BUILD_TIME: string = local-time-string()
let BUILD_PID: i32 = getpid()

fn main(): i32 {
  println("built by $BUILD_USER@$BUILD_HOST at $BUILD_TIME")
  println("compiler pid $BUILD_PID, runtime pid ${getpid()}")
  0
}
```

`k1 run` compiles and runs it; running the produced executable again shows
what was decided at compile time and what was not:

```text
$ k1 run cffi_build_stamp.k1
built by knix@knix.local at 2026-09-17 17:04:16 EDT
compiler pid 3439, runtime pid 3446
$ ./.k1-out/cffi_build_stamp
built by knix@knix.local at 2026-09-17 17:04:16 EDT
compiler pid 3439, runtime pid 3449
$ ./.k1-out/cffi_build_stamp
built by knix@knix.local at 2026-09-17 17:04:16 EDT
compiler pid 3439, runtime pid 3450
```

The four globals are ordinary K1 functions calling ordinary `extern`
declarations. An extern with no `lib` is resolved with `dlsym` against the
compiler process itself, so anything in libc, libm or libSystem is callable
at compile time with no setup. The same declaration is also what the
generated code calls at runtime, which is why `getpid()` in `main` gives a
fresh pid. The binary imports only the symbol runtime code uses:

```text
$ nm .k1-out/cffi_build_stamp | grep -i 'gethostname\|getpid\|strftime\|localtime\|getenv'
                 U _getpid
```

`gethostname`, `time`, `localtime`, `strftime` and `getenv` were called once,
by the compiler, and never made it into the executable.

Because global initializers are always compile-time, `#static` inside one is
redundant, and the compiler says so:

```k1
fn(extern("getpid")) getpid(): i32

let COMPILER_PID: i32 = #static getpid()
```

```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/cffi_redundant_static.k1:3:24: warning
├─────
│   fn(extern("getpid")) getpid(): i32
│   
│ ->let COMPILER_PID: i32 = #static getpid()
│                           ^^^^^^^^^^^^^^^^
│   
│   fn main(): i32 {
├─────
│  This #static is immediately inside a static
└────────────────────────────────────────╴
Completed with 1 warnings
```

## Rasterizing a font atlas at compile time

Calling libc is a party trick. Calling your own C is the real capability: a
module can compile C in its build step, and the VM will `dlopen` the result
and call into it while the program is being compiled. This module bakes an
ASCII font atlas with stb_truetype at compile time and embeds the bitmap;
at runtime it prints glyphs from the embedded bytes.

The C side is two lines, a single translation unit around the vendored
header:

```c
#define STB_TRUETYPE_IMPLEMENTATION
#include "stb_truetype.h"
```

The module's `build.k1` compiles it to `libs/libatlas.dylib`. `fn setup`
runs on the host, before the program compiles, whenever its declared inputs
are newer than its outputs (the second compile costs a stamp check, not a `cc`
invocation). The script `cd`s to `k1/module-dir()`, the home dir of the
module whose source makes the call, so it does not care what directory the
compiler was invoked from. The VM is what loads the library, so its extension
comes from `k1/host-platform`, the platform the compiler runs on:

```k1 path=content/showcase/examples/cffi_font_atlas/build.k1
use std/process

fn module(_b: k1/build-config): k1/module {
  let m = k1/module/new()
  m.setup(outputs = ["libs/libatlas.${k1/host-platform.dylib-ext()}"], inputs = ["atlas.c"])
  m
}

fn setup(_ctx: k1/setup-ctx) {
  let _ = process/sh-verbose(`
    cd "${k1/module-dir()}"
    mkdir -p libs
    cc -O2 -shared -I../../../../modules/stb/vendor atlas.c -o libs/libatlas.${k1/host-platform.dylib-ext()}
    `).!
}
```

The K1 side declares the one stb function it needs, tagged with the library
it lives in, and bakes the atlas in a global initializer:

```k1
type baked-char = { x0: u16, y0: u16, x1: u16, y1: u16, xoff: f32, yoff: f32, xadvance: f32 }

fn(extern("stbtt_BakeFontBitmap"), lib("atlas")) bake-font-bitmap(
  data: ptr, offset: i32, pixel-height: f32,
  pixels: ptr, pw: i32, ph: i32,
  first-char: i32, num-chars: i32, chardata: *baked-char
): i32

let ATLAS_W: size = 256
let ATLAS_H: size = 128
let FIRST_CHAR: size = 32
let NUM_CHARS: size = 95

type atlas = { pixels: span[u8], glyphs: span[baked-char] }

fn bake(font-path: string, pixel-height: f32): atlas {
  let ttf = files/read-to-string(font-path)
  let pixels = buffer/allocate[u8](ATLAS_W * ATLAS_H)
  let glyphs = buffer/allocate[baked-char](NUM_CHARS)
  let rows-used = bake-font-bitmap(
    ttf.data-ptr(), 0, pixel-height,
    pixels.data-ptr(), ATLAS_W.trunc[i32], ATLAS_H.trunc[i32],
    FIRST_CHAR.trunc[i32], NUM_CHARS.trunc[i32], glyphs.data-ref())
  if rows-used <= 0 crash("font atlas does not fit: $rows-used")
  .{ pixels = pixels.as-span(), glyphs = glyphs.as-span() }
}

let ATLAS: atlas = bake("/System/Library/Fonts/Supplemental/Arial.ttf", 24.0)
```

`lib("atlas")` means two different things to the two consumers of the
declaration. To the VM it means: on first call, `dlopen` the module's
`libs/libatlas.dylib` (falling back to a system lookup by that name), `dlsym`
the symbol, and call it through libffi with the K1 arguments marshalled to
the C ABI, VM memory included: the `buffer`s are real host memory, so stb
writes its pixels straight into them. To the linker it would mean "link
this library", but only if the manifest declared it with `m.lib(...)`. This
module deliberately does not: stb is a compile-time dependency only. The
runtime half of the program just reads the constant:

```k1
fn shade(v: u8): char {
  if v < 32 ' ' else if v < 96 '.' else if v < 160 '+' else if v < 224 '*' else '#'
}

fn print-glyph(c: char) {
  let g = ATLAS.glyphs.[c.as-u8().widen[size] - FIRST_CHAR]
  for y in g.y0.widen[size].until(g.y1.widen[size]) {
    let line = string-builder/new()
    for x in g.x0.widen[size].until(g.x1.widen[size]) {
      line.write-byte(shade(ATLAS.pixels.[y * ATLAS_W + x]).as-u8())
    }
    println(line.build())
  }
}

fn main(): i32 {
  println("${ATLAS.pixels.len()} atlas bytes, ${ATLAS.glyphs.len()} glyphs")
  print-glyph('K')
  print-glyph('1')
  0
}
```

```text
$ k1 run cffi_font_atlas
Setting up module 'cffi_font_atlas' (running fn setup in /Users/knix/dev/k1/content/showcase/examples/cffi_font_atlas/cffi_font_atlas.k1)...
+ cd /Users/knix/dev/k1/content/showcase/examples/cffi_font_atlas
+ mkdir -p libs
+ cc -O2 -shared -I../../../../modules/stb/vendor atlas.c -o libs/libatlas.dylib
+ [902ms]
[INFO  k1] run executable: cffi_font_atlas
32768 atlas bytes, 95 glyphs
.+.       .+. 
+#+      +##. 
+#+     *##.  
+#+    *##.   
+#+   *#*.    
+#+  *#*      
+#+ *#*       
+#+*###       
+###**#*      
+##*  ##+     
+#*   .##.    
+#+    +##    
+#+     *#*   
+#+     .##+  
+#+      +##. 
+#+       *#* 
    .+ 
    ## 
   *## 
 .#### 
+##+## 
**. ## 
    ## 
    ## 
    ## 
    ## 
    ## 
    ## 
    ## 
    ## 
    ## 
    ## 
```

Three checks that the atlas is data, not a runtime computation. The
executable links nothing but libSystem, its undefined symbols contain no
`stbtt_` and no `dlopen`, and the bitmap is a 32 KiB constant in the emitted
IR:

```text
$ otool -L .k1-out/cffi_font_atlas
.k1-out/cffi_font_atlas:
	/usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1356.0.0)
$ nm .k1-out/cffi_font_atlas | grep -i 'stbtt\|dlopen'
$ grep -o '^@k1.static.[0-9]*.elems = internal unnamed_addr constant \[32768 x i8\]' .k1-out/cffi_font_atlas.ll
@k1.static.1375.elems = internal unnamed_addr constant [32768 x i8]
```

The whole compile, bake included, is 78 ms wall once `libatlas.dylib`
exists (`--chatty`: 21 VM runs totalling 0.9 ms, 50 ms of that wall in
the linker). The `glyphs` span survives the boundary too: it is a span of
20-byte structs mixing `u16` and `f32` fields, laid out exactly as
`stbtt_bakedchar`, and `print-glyph` indexes it at runtime.

What this replaces elsewhere: a font atlas in a C, C++, Rust or Zig project
is a build-time code generator, a separate host tool that writes a `.h` or a
`.bin` for `#embed`, `include_bytes!` or `@embedFile`, plus the build-system
glue to run it in the right order. Zig's comptime cannot call an `extern`
function; C++ `constexpr` cannot call a non-`constexpr` one, which rules out
every existing C library; Rust `const fn` cannot call FFI. K1's answer is
that compile time is a VM with a C ABI: the generator is the program itself,
written in the same language, in the same file, with its output typed as an
ordinary global.

This example is macOS-specific in one incidental way: the font path. The
mechanism is the same on Linux, where setup builds `libs/libatlas.so`.
