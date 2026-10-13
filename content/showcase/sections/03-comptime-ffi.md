# The compile-time VM can call C

Every top-level `let` runs at compile time, in the compiler's VM. The VM
speaks the C ABI through libffi, so those initializers can call C.

## A build stamp

Declare some libc functions. An extern with no `lib` is looked up with
`dlsym` in the compiler process itself, so libc, libm and libSystem are free:

```k1
fn(extern("gethostname")) gethostname(name: ptr, len: size): i32
fn(extern("getpid")) getpid(): i32
fn(extern("time")) c-time(tloc: ptr): i64
fn(extern("localtime")) localtime(t: *i64): ptr
fn(extern("strftime")) strftime(s: ptr, max: size, format: *u8, tm: ptr): size
```

Wrap them in plain k1:

```k1
fn hostname(): string {
  let buf = buffer/allocate[u8](256)
  if gethostname(buf.data-ptr(), buf.len()) != 0 crash("gethostname failed")
  string/wrap-c(buf.data-ref())
}

fn local-time-string(): string {
  let now = c-time(ptr/null)
  let out = buffer/allocate[u8](64)
  let n = strftime(out.data-ptr(), out.len(), "%Y-%m-%d %H:%M:%S %Z".to-c(), localtime(now.&))
  if n == 0 crash("strftime failed")
  string/wrap-c(out.data-ref())
}

fn env-or(name: string, fallback: string): string {
  let v = libc/getenv(name.to-c())
  if v.is-null() fallback else string/wrap-c(v.get())
}
```

And call them from globals:

```k1
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

No `__DATE__`, no `-DHOST=` computed by the build system. Run it, then run
the binary again:

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

The same `getpid` declaration serves both worlds: the VM calls it for
`BUILD_PID`, and generated code calls it in `main`. It's the only one that
makes it into the executable:

```text
$ nm .k1-out/cffi_build_stamp | grep -i 'gethostname\|getpid\|strftime\|localtime\|getenv'
                 U _getpid
```

Global initializers are already compile-time, so `#static` in one is
redundant, and the compiler tells you:

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

Calling libc is a party trick. Calling your own C is the real thing: a
module compiles C in its build step, and the VM `dlopen`s the result while
the program compiles. This one bakes an ASCII font atlas with stb_truetype
and embeds the bitmap.

The C side is the whole library in one translation unit:

```c
#define STB_TRUETYPE_IMPLEMENTATION
#include "stb_truetype.h"
```

`build.k1` compiles it. `fn setup` runs on the host before the program
compiles, and only when its inputs are newer than its outputs. The VM loads
the library, so the extension comes from `k1/host-platform`:

```k1 path=content/showcase/examples/cffi_font_atlas/build.k1
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

On the k1 side, one stb function, tagged with the library it lives in:

```k1
type baked-char = { x0: u16, y0: u16, x1: u16, y1: u16, xoff: f32, yoff: f32, xadvance: f32 }

fn(extern("stbtt_BakeFontBitmap"), lib("atlas")) bake-font-bitmap(
  data: ptr, offset: i32, pixel-height: f32,
  pixels: ptr, pw: i32, ph: i32,
  first-char: i32, num-chars: i32, chardata: *baked-char
): i32
```

`baked-char` is laid out exactly like `stbtt_bakedchar`. On first call the
VM `dlopen`s `libs/libatlas.dylib`, `dlsym`s the symbol, and marshals the
arguments through libffi. VM buffers are real host memory, so stb writes its
pixels straight into them:

```k1
let ATLAS_W: size = 256
let ATLAS_H: size = 128
let FIRST_CHAR: size = 32
let NUM_CHARS: size = 95

type atlas = { pixels: span[u8], glyphs: span[baked-char] }

fn bake(font-path: string, pixel-height: f32): atlas {
  let ttf = files/read-to-string(font-path).!
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

Runtime code just reads the constant:

```k1
fn shade(v: u8): char {
  if v < 32 ' ' else if v < 96 '.' else if v < 160 '+' else if v < 224 '*' else '#'
}

fn print-glyph(c: char) {
  let g = ATLAS.glyphs.[c.as-u8().as-size() - FIRST_CHAR]
  for y in g.y0.as-size().until(g.y1.as-size()) {
    let line = string-builder/new()
    for x in g.x0.as-size().until(g.x1.as-size()) {
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

`lib("atlas")` would also tell the linker to link the library, but only if
the manifest declared it with `m.lib(...)`. This one doesn't: stb is a
compile-time dependency only. The executable links nothing but libSystem,
has no `stbtt_` or `dlopen` symbols, and the bitmap is a 32 KiB constant:

```text
$ otool -L .k1-out/cffi_font_atlas
.k1-out/cffi_font_atlas:
	/usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1356.0.0)
$ nm .k1-out/cffi_font_atlas | grep -i 'stbtt\|dlopen'
$ grep -o '^@k1.static.[0-9]*.elems = internal unnamed_addr constant \[32768 x i8\]' .k1-out/cffi_font_atlas.ll
@k1.static.1542.elems = internal unnamed_addr constant [32768 x i8]
```

The only macOS-specific part is the font path. On Linux, setup builds
`libs/libatlas.so`.

## Elsewhere

A font atlas in C, C++, Rust or Zig means a separate generator tool, a
`.bin` for `#embed`, `include_bytes!` or `@embedFile`, and build glue to run
it in order. Here the generator is the program itself.

- Zig: comptime can't call an `extern` function.
- C++: `constexpr` can't call a non-`constexpr` function, so no existing C
  library.
- Rust: `const fn` can't call FFI.
