# A palette authored in OKLCH, shipped as bytes

UI colors want to be designed in a perceptual space, where equal steps in
lightness look like equal steps, and shipped as `u8` triples, because that is
what draw APIs and hex codes are. The conversion in between is trig, a cube,
two matrix multiplies and a gamma curve through `pow`. In K1 it runs once, in
the compiler, and the binary only ever sees the bytes.

This is the color module from a real UI workbench, ported unchanged. Every
global initializer in K1 is a compile-time program, so the palette is written
as calls:

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

`rad.cos()` and `.pow(2.4)` are `std/math`, which calls libm. The compiler's
VM calls the same `cosf` and `powf` the binary links against, so the bytes it
computes are the bytes the program would have computed at startup. Nothing
about `oklch` is written for compile time: it is `f32` arithmetic, a struct
return, `if` and `return` in `quantize`, and it is the same function the
workbench calls at runtime with a mouse-driven hue.

The proof is in the emitted IR (`k1 --emit-llvm build`, excerpted below). The
nine palette globals are folded into one constant blob, `#16181d` is in it as
three bytes, and the only K1 functions left in the module are `main`, `hex`,
`contrast` and `linear-from-srgb`. `oklch`, `quantize` and the gamma curve are
not there:

```text
@k1.static.1462.elems = internal unnamed_addr constant <{ ... i8 22, i8 24, i8 29 ... }>
@_root__palette_oklch__RAMP = internal unnamed_addr constant <{ ... }> <{ <{ i8 18, i8 22, i8 31 }>, <{ i8 36, i8 41, i8 51 }>, ... }>, align 1

palette_oklch.contrast_3717
palette_oklch.hex_3715
palette_oklch.linear-from-srgb_3712
palette_oklch.main_3718
```

## Pinning the result in the type

A `#static` expression can be given a literal type, so the hex code of `BG` is
not a comment that drifts, it is the type of the binding:

```k1
  let bg-hex: "#16181d" = #static hex(BG)
  let fg-hex: "#e9ecf1" = #static hex(FG)
```

Retune `BG` and the compiler tells you the new value on the way out:

```text
│ ->  let bg-hex: "#1a1a1e" = #static hex(BG)
│                             ^^^^^^^^^^^^^^^
├─────
│  Different static values of same type family: #1a1a1e vs #16181d
```

## A design lint the compiler enforces

WCAG contrast is relative luminance of the two colors, computed on the linear
channels. Both functions are ordinary; the table of text pairs is a global; the
lint is a `#static for` over it that crashes with the ratio it computed:

```k1
fn luminance(c: srgb8): f32 {
  0.2126 * linear-from-srgb(c.r) + 0.7152 * linear-from-srgb(c.g) + 0.0722 * linear-from-srgb(c.b)
}

fn contrast(a: srgb8, b: srgb8): f32 {
  let la: f32 = luminance(a) + 0.05
  let lb: f32 = luminance(b) + 0.05
  if la > lb la / lb else lb / la
}

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

`palette_contrast_fail.k1` is the conversion and the lint from above with one
more color, a muted label at L = 0.50 that looks fine in a design tool, and
one more row asking for it on the panel background:

```k1
let MUTED: srgb8  = oklch(0.500, 0.0279, UI-HUE)
```

```k1
  .{ name = "muted on panel", fg = MUTED, bg = PANEL, min = 4.5 },
```

The build stops. The message is the one the program wrote, with the ratio the
VM computed, which is the ratio the native build computes for the same pair
(2.6472688 both ways). The error is reported at the `crash` call itself, with
the VM's stack at that point:

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
[00] core/crash builtin.k1:1083
[01] expr palette_contrast_fail.k1:82 palette_contrast_fail.k1:84

└────────────────────────────────────────╴
Module palette_contrast_fail failed typechecking with 1 errors
```

A bare `#static assert(contrast(FG, BG) >= 7.0)` works the same way and
reports `ASSERT FAILED` at the assertion, one frame down.

## Generated output, still zero cost

The same palette feeds a CSS custom-properties block, built with the ordinary
`string-builder` in a global initializer, and a nine-step lightness ramp built
by a loop into an `array`. Both are constants in the binary; the CSS is a
183-byte string literal, the ramp is the 27 bytes shown above.

```k1
let CSS: string = {
  let sb = string-builder/new()
  sb.writeln(":root {")
  for SWATCHES { sb.writeln("  --${it.name}: ${hex(it.color)};") }
  sb.writeln("}")
  sb.build()
}

let RAMP: array[srgb8, 9] = {
  let ramp: array[srgb8, 9] = .0
  for i in 0.until(9) {
    ramp.[i] = oklch(0.2 + 0.08 * i.as[f32], 0.02, UI-HUE)
  }
  ramp
}
```

`k1 run palette_oklch.k1`:

```text
#16181d on #e9ecf1
bg: #16181d
panel: #1f2229
hilite: #2b303b
edge: #3b414f
gray: #8a91a3
fg: #e9ecf1
blue: #78a0f7
green: #9dcd6a
orange: #e0ae67

fg on bg: 14.996142
fg on panel: 13.443411
gray on bg: 5.6318216
blue on bg: 6.9090486
green on bg: 9.618753
orange on bg: 8.8115425

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

#12161f #242933 #383d48 #4d525e #646975 #7b808d #9398a5 #abb1be #c5cad8 
```

The ramp is the point of OKLCH: L steps of 0.08 from 0.2 to 0.84, and the
result reads as evenly spaced grays. Authoring that by hand in hex, or in HSL,
is guesswork.

## Why this is hard elsewhere

There is no compile-time subset here. The color code is the runtime color
code, the string builder is the runtime string builder, the loop is a loop.
Rust's `f32::powf` and `cos` are not `const fn` on stable, so this palette is
a `build.rs` or a proc macro. In C++, `std::pow` and `std::cos` only became
`constexpr` in C++26, and a `constexpr std::string` still cannot outlive the
evaluation that built it, so the CSS block is a fixed-size-buffer trick. Zig's
comptime can do the float math with its builtins but cannot call into libm,
so its compile-time bytes are not guaranteed to be its runtime bytes. K1 runs
the program's own functions against the program's own libm, and what comes
back is data.
