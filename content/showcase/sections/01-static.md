# Static execution

Running code at compile-time for great fun and profit

## Literal types

Every runtime type is actually a family of what k1 calls a `static` type, which is a
runtime type coupled together with a single instance of that type. Just as "hello" inhabits
the type `string`, so does `"hello"` inhabit the type `"hello"`, which we write as `static[family, instance]`.
So, `static[string, "hello"]` names a type that takes up zero bits at runtime, and fully specifies the
specific string "hello".

Some of these instances are spellable literally, like strings and integers. This is useful, specifically for
working with sizes at compile-time.

```k1
fn rot13(c: u8): u8 {
  if c >= 'a'.as-u8() and c <= 'z'.as-u8() {
    (c - 'a'.as-u8() + 13) % 26 + 'a'.as-u8()
  } else c
}

fn main(): i32 {
  let greeting: "hello world" = #static {
    let bytes: list[u8] = []
    for c in "uryyb jbeyq" {
      bytes.push(rot13(c.as-u8()))
    }
    string/wrap-bytes(bytes.as-span())
  }
  println(greeting.from-static())
  0
}
```

`greeting` itself does not actually exist on the stack, as it is a zero-sized type!
We can call `.from-static()` on it to materialize a real string from it, 'forgetting'
the type "hello" and remembering only the type "string".

`from-static` literally moves the information from the type into the runtime bits of the value.

The below fails like any other type error. Now that you understand static type families, you
could probably write the typechecker for them. `static[string, "hello w0rld"]` is clearly not an instance of
`static[string, "hello world"]`.

```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/static_greeting_wrong.k1:8:32: error
├─────
│   }
│   
│   fn main(): i32 {
│ ->  let greeting: "hello world" = #static {
│                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
│       let bytes: list[u8] = []
│       for c in "uryyb j0eyq" {
├─────
│  Different static values of same type family: hello world vs hello w0rld
└────────────────────────────────────────╴
```

That is a compiler type error, in the same category as 'expected int got string'.

Zig's `comptime` and C++20 `constexpr` can build the same string at compile
time, but the value never becomes a type. Rust's `const fn` cannot allocate on stable,
so a heap-built string is not available at all.

## A global's type is its value (if not mutable)

Since global initializers run at compile time, the compiler knows what they
are, and you can use the global's name as a type. `types/id[fifteen]` is `types/id[15]`, `types/id[name]` is
`types/id["k1"]`:

```k1
let fifteen: int = { 3 * 5 }

let name: string = {
  let base = "k"
  base.concat("1")
}

#static {
  assert-equals(types/id[fifteen], types/id[15])
  assert-equals(types/id[name], types/id["k1"])
}
```

```k1
  let x: fifteen = 15
  let y: name = "k1"
  println("${y.from-static()} ${x.from-static()}")
```

```text
k1 15
```

Anything else is rejected:

```k1
  let wrong-int: ?string = test-compile({ let z: fifteen = 16; z })
  let wrong-str: ?string = test-compile({ let z: name = "k2"; z })
  println(wrong-int ? "compiles")
  println(wrong-str ? "compiles")
```

```text
Static lift resulted in wrong value: Different static values of same type family: 15 vs 16
Different static values of same type family: k1 vs k2
```

A `#static` struct keeps every field value in its type:

```k1
  let point = #static .{ x = 3 * 4, y = 5 - 3 }
  (point.x: 12)
  (point.y: 2)
  println("point is ${point.x.from-static()},${point.y.from-static()}")
```

```text
point is 12,2
```

## Compile-time CRC32, checked by its type

The table is a global, so all 256 entries are computed by the compiler and land in the binary as data:

```k1
fn crc32-table(): array[u32, 256] {
  let table: array[u32, 256] = .0
  for n in 0.until(256) {
    let c = n.trunc[u32]
    for 0.until(8) {
      c = if (c & 1) == 1 0xEDB88320 ^ (c >> 1) else c >> 1
    }
    table.[n] = c
  }
  table
}

let CRC_TABLE: array[u32, 256] = crc32-table()

fn crc32(bytes: span[u8]): u32 {
  let c = 0xFFFFFFFF: u32
  for b in bytes {
    c = CRC_TABLE.[((c ^ b.widen[u32]) & 0xFF)] ^ (c >> 8)
  }
  c ^ 0xFFFFFFFF
}
```

The standard check value is the *type* of `check`. The same `crc32` runs in the
VM for that, and compiled for the command line:

```k1
fn main(argc: u32, argv: ptr): i32 {
  let check: 0xCBF43926u32 = #static crc32("123456789".span-bytes())
  println("check value 0x${hex(check.from-static())} verified while compiling")
  let args = std/sys/parse-main-args(argc, argv)
  for a in args.drop(1) {
    println("crc32(\"$a\") = 0x${hex(crc32(a.span-bytes()))}")
  }
  0
}
```

```text
$ k1 run static_crc32.k1 hello "The quick brown fox"
check value 0xcbf43926 verified while compiling
crc32("hello") = 0x3610a686
crc32("The quick brown fox") = 0xb74574de
```

Use the non-reflected polynomial by mistake:

```k1
      c = if (c & 1) == 1 0x04C11DB7 ^ (c >> 1) else c >> 1
```

```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/static_crc32_wrong.k1:30:29: error
├─────
│   }
│   
│   fn main(argc: u32, argv: ptr): i32 {
│ ->  let check: 0xCBF43926u32 = #static crc32("123456789".span-bytes())
│                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
│     println("check value 0x${hex(check.from-static())} verified while compiling")
│     let args = std/sys/parse-main-args(argc, argv)
├─────
│  Different static values of same type family: 3421780262 vs 4233047017
└────────────────────────────────────────╴
```

C++ and Zig can compute the table at compile time, but the self-test is a separate assertion, not the type of the result.

## A Brainfuck interpreter, run by the compiler

Nothing in here knows it is going to run inside the compiler:

```k1
fn jump(program: string, pc: size, forward: bool): size {
  let pc = pc
  let step = if forward 1 else -1
  let depth = 1
  while depth > 0 {
    pc = if forward pc + 1 else pc - 1
    if program.[pc] == '[' { depth = depth + step }
    if program.[pc] == ']' { depth = depth - step }
  }
  pc
}

fn bf(program: string): string {
  let tape: array[u8, 64] = .0
  let out = string-builder/new()
  let ptr: size = 0
  let pc: size = 0
  while pc < program.len() {
    if program.[pc] is {
      '>' -> { ptr = ptr + 1 },
      '<' -> { ptr = ptr - 1 },
      '+' -> { tape.[ptr] = tape.[ptr] + 1 },
      '-' -> { tape.[ptr] = tape.[ptr] - 1 },
      '.' -> { out.write-byte(tape.[ptr]) },
      '[' -> { if tape.[ptr] == 0 { pc = jump(program, pc, true) } },
      ']' -> { if tape.[ptr] != 0 { pc = jump(program, pc, false) } },
      _ -> {}
    }
    pc = pc + 1
  }
  out.build()
}
```

```k1
  let hello: "Hello World!\n" = #static bf("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
  print(hello.from-static())
```

```text
Hello World!
```

## A program that unit-tests its own type errors

`test-compile(expr)` typechecks `expr` and hands back the error as a `?string`:

```k1
fn report(label: string, outcome: ?string) {
  println("$label: ${outcome ? "compiles"}")
}

fn main(): i32 {
  report("1: 5", test-compile(1: 5))
  report("\"a\" + 1", test-compile("a" + 1))
  report("let s: string = 42", test-compile({ let s: string = 42; s }))
  report("1 + 1", test-compile(1 + 1))
  assert(test-compile(1 + 1).is-none())
  0
}
```

```text
1: 5: Expression did not conform to hint: Static lift resulted in wrong value: Different static values of same type family: 5 vs 1
"a" + 1: Call to add/add with self = string does not work
No matching implementations found
let s: string = 42: Expected string but got i64
1 + 1: compiles
```

Rust needs the `trybuild` crate and separate `compile_fail` files for this; a C++ `requires` expression never gets the message back.

## `#if`, and both worlds at once

`#if` picks a branch at compile time, and only that branch is typechecked:

```k1
  let platform = #if core/k1/platform-macos "macos" else "not macos"
  println("built for $platform")
```

```text
built for macos
```

`core/k1/is-static` is the one place the two worlds differ:

```k1
fn where-am-i(): string {
  if core/k1/is-static "compile time" else "run time"
}
```

```k1
  let baked: "compile time" = #static where-am-i()
  println("where-am-i said \"${baked.from-static()}\" then, and \"${where-am-i()}\" now")
```

```text
where-am-i said "compile time" then, and "run time" now
```

A `println` under `#static` prints from inside the compiler, tagged with its line:

```k1
  #static println("hello from inside the compiler")
```

```text
[static_both_worlds.k1:11 info] hello from inside the compiler
```

## OKLCH at compile-time

Design colors in a perceptual space, ship them as `u8` triples. The conversion
is trig, cubes, two matrix multiplies and a `pow` gamma curve. This is the color
module from a real UI workbench, unchanged:

```k1
fn oklch(l: f32, c: f32, h: f32): srgb8 {
  let rad: f32 = h * DEG-TO-RAD
  let a: f32 = c * rad.cos()
  let b: f32 = c * rad.sin()
  let lp: f32 = l + 0.3963377774 * a + 0.2158037573 * b
  let mp: f32 = l - 0.1055613458 * a - 0.0638541728 * b
  let sp: f32 = l - 0.0894841775 * a - 1.2914855480 * b
  let ll: f32 = lp * lp * lp
  let mm: f32 = mp * mp * mp
  let ss: f32 = sp * sp * sp
  .{
    r = srgb-from-linear( 4.0767416621 * ll - 3.3077115913 * mm + 0.2309699292 * ss),
    g = srgb-from-linear(-1.2684380046 * ll + 2.6097574011 * mm - 0.3413193965 * ss),
    b = srgb-from-linear(-0.0041960863 * ll - 0.7034186147 * mm + 1.7076147010 * ss),
  }
}
```

The palette is just globals, so it's computed by the compiler:

```k1
let UI-HUE: f32 = 267.0

let BG: srgb8     = oklch(0.209, 0.0104, UI-HUE)
let PANEL: srgb8  = oklch(0.252, 0.0140, UI-HUE)
let HILITE: srgb8 = oklch(0.309, 0.0210, UI-HUE)
let EDGE: srgb8   = oklch(0.375, 0.0253, UI-HUE)
let GRAY: srgb8   = oklch(0.657, 0.0279, UI-HUE)
let FG: srgb8     = oklch(0.942, 0.0075, UI-HUE)
let BLUE: srgb8   = oklch(0.714, 0.1351, 264.5)
let GREEN: srgb8  = oklch(0.792, 0.1385, 130.3)
let ORANGE: srgb8 = oklch(0.782, 0.1063, 74.8)
```

`cos` and `pow` call libm, and the VM calls the same `cosf` and `powf` the
binary links against, so the compiler's bytes are the program's bytes.

Pin the hex codes in the types, and retuning `BG` becomes a compile error:

```k1
  let bg-hex: "#16181d" = #static hex(BG)
  let fg-hex: "#e9ecf1" = #static hex(FG)
  println("${bg-hex.from-static()} on ${fg-hex.from-static()}")
```

```text
#16181d on #e9ecf1
```

### A design lint the compiler enforces

WCAG contrast:

```k1
fn luminance(c: srgb8): f32 {
  0.2126 * linear-from-srgb(c.r) + 0.7152 * linear-from-srgb(c.g) + 0.0722 * linear-from-srgb(c.b)
}

fn contrast(a: srgb8, b: srgb8): f32 {
  let la: f32 = luminance(a) + 0.05
  let lb: f32 = luminance(b) + 0.05
  if la > lb la / lb else lb / la
}
```

A table of text pairs, and a lint over it that runs in the compiler:

```k1
type text-pair = { name: string, fg: srgb8, bg: srgb8, min: f32 }

let TEXT-PAIRS: span[text-pair] = [
  .{ name = "fg on bg",     fg = FG,     bg = BG,    min = 7.0 },
  .{ name = "fg on panel",  fg = FG,     bg = PANEL, min = 7.0 },
  .{ name = "gray on bg",   fg = GRAY,   bg = BG,    min = 4.5 },
  .{ name = "blue on bg",   fg = BLUE,   bg = BG,    min = 4.5 },
  .{ name = "green on bg",  fg = GREEN,  bg = BG,    min = 4.5 },
  .{ name = "orange on bg", fg = ORANGE, bg = BG,    min = 4.5 },
]

#static for TEXT-PAIRS {
  let ratio = contrast(it.fg, it.bg)
  if ratio < it.min crash("${it.name} contrast is $ratio, need ${it.min}")
}
```

They all pass:

```k1
  for TEXT-PAIRS { println("${it.name}: ${contrast(it.fg, it.bg)}") }
```

```text
fg on bg: 14.996142
fg on panel: 13.443411
gray on bg: 5.6318216
blue on bg: 6.9090486
green on bg: 9.618753
orange on bg: 8.8115425
```

Now add a muted label that looks fine in a design tool:

```k1
let MUTED: srgb8  = oklch(0.500, 0.0279, UI-HUE)
```

```k1
  .{ name = "muted on panel", fg = MUTED, bg = PANEL, min = 4.5 },
```

```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/palette_contrast_fail.k1:84:20: error
├─────
│   
│   #static for TEXT-PAIRS {
│     let ratio = contrast(it.fg, it.bg)
│ ->  if ratio < it.min crash("${it.name} contrast is $ratio, need ${it.min}")
│                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
│   }
│   
├─────
│  muted on panel contrast is 2.6472688, need 4.5
bc Execution Trace
[00] core/platform/process/exit platform.k1:201
[01] core/sys/exit builtin.k1:964
[02] core/crash builtin.k1:1171
[03] expr palette_contrast_fail.k1:82 palette_contrast_fail.k1:84

└────────────────────────────────────────╴
Module palette_contrast_fail failed compiling with 1 errors
```

### Generated output, still zero cost

A CSS block, built with the ordinary `string-builder`:

```k1
let CSS: string = {
  let sb = string-builder/new()
  sb.writeln(":root {")
  for SWATCHES { sb.writeln("  --${it.name}: ${hex(it.color)};") }
  sb.writeln("}")
  sb.build()
}
```

```text
:root {
  --bg: #16181d;
  --panel: #1f2229;
  --hilite: #2b303b;
  --edge: #3b414f;
  --gray: #8a91a3;
  --fg: #e9ecf1;
  --blue: #78a0f7;
  --green: #9dcd6a;
  --orange: #e0ae67;
}
```

A lightness ramp, L from 0.2 to 0.84 in steps of 0.08:

```k1
let RAMP: array[srgb8, 9] = {
  let ramp: array[srgb8, 9] = .0
  for i in 0.until(9) {
    ramp.[i] = oklch(0.2 + 0.08 * i.as[f32], 0.02, UI-HUE)
  }
  ramp
}
```

```text
#12161f #242933 #383d48 #4d525e #646975 #7b808d #9398a5 #abb1be #c5cad8 
```

Evenly spaced grays, which is the whole point of OKLCH. Both are constants in
the binary, and `oklch`, `quantize` and the gamma curve never make it there:

```text
$ k1 build palette_oklch.k1 --emit-llvm && grep '^define.*palette_oklch' .k1-out/palette_oklch.ll
define internal fastcc [2 x i64] @palette_oklch.hex_4023(i64 %c) #0 !dbg !571 {
define internal fastcc float @palette_oklch.luminance_4024(i64 %c) #0 !dbg !596 {
define internal i32 @palette_oklch.main_4026() #0 !dbg !609 {
```

### Elsewhere

- Rust: `powf` and `cos` are not `const fn` on stable; this is a `build.rs`.
- C++: `std::pow` and `std::cos` are `constexpr` only as of C++26, and a
  `constexpr std::string` can't outlive the evaluation that built it.
- Zig: comptime float math uses builtins, not libm, so compile-time bytes
  aren't guaranteed to be runtime bytes.
