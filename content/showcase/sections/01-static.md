# `#static`: the compiler runs your program

`#static expr` runs `expr` in the compile-time VM and hands the result to the
rest of the program as a constant. It is the same language, the same standard
library and the same functions that run at runtime; nothing is annotated
`constexpr`, `comptime` or `const fn`. The twist is where the result goes:
values can live in types, so a compile-time result is checked against the type
you wrote, and a global's type is whatever its initializer computed.

## A string literal is a type

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

```text
hello world
```

`"hello world"` in type position is the type with exactly one value. The
`#static` block decodes a rot13 string byte by byte into a `list[u8]`, wraps it
as a `string`, and the compiler checks that value against the annotation. Change
one byte of the input (`"uryyb j0eyq"` in `static_greeting_wrong.k1`) and the
program no longer compiles:

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

Zig's `comptime` and C++20 `constexpr` can build the same string at compile
time, but the value never becomes a type: the check is a separate
`comptime assert` or `static_assert` you have to remember to write. Rust's
`const fn` cannot allocate on stable, so a heap-built string is not available
at all. `greeting` has type `static[string, "hello world"]`; `.from-static()`
turns it back into an ordinary runtime `string`.

## A global's type is its value

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

fn main(): i32 {
  let x: fifteen = 15
  let y: name = "k1"
  println("${y.from-static()} ${x.from-static()}")

  let wrong-int: ?string = test-compile({ let z: fifteen = 16; z })
  let wrong-str: ?string = test-compile({ let z: name = "k2"; z })
  println(wrong-int ? "compiles")
  println(wrong-str ? "compiles")

  let point = #static .{ x = 3 * 4, y = 5 - 3 }
  (point.x: 12)
  (point.y: 2)
  println("point is ${point.x.from-static()},${point.y.from-static()}")
  0
}
```

```text
k1 15
Static lift resulted in wrong value: Different static values of same type family: 15 vs 16
Different static values of same type family: k1 vs k2
point is 12,2
```

Global initializers run at compile time, and the compiler remembers what they
produced: `types/id[fifteen]` is `types/id[15]`, `types/id[name]` is
`types/id["k1"]`, and a global's name can be used directly as a type. The two
`test-compile` lines show the compiler rejecting `16` and `"k2"` for those
types. The same holds inside a function: `(point.x: 12)` is a type assertion
against a struct that was computed by `#static`, so the type of `point` records
both field values.

## Compile-time CRC32, checked by its type

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

fn hex(v: u32): string {
  let sb = string-builder/new()
  core/format-uint(context sb.&)(v.widen[u64], 16)
  sb.build()
}

fn main(argc: core/c-int, argv: ptr): i32 {
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

`CRC_TABLE` is an `array[u32, 256]` global, so its 256 entries are computed in
the VM and land in the binary as data. `crc32` is one ordinary function: the
`#static` call runs it in the VM over `"123456789"`, and the loop in `main`
runs the compiled version over the command line. The known check value
`0xCBF43926` is the *type* of `check`, and the `u32` suffix in the literal type
keeps `check.from-static()` a `u32`. Swap the polynomial for the non-reflected
form `0x04C11DB7` (`static_crc32_wrong.k1`) and the build fails before anything
runs:

```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/static_crc32_wrong.k1:30:29: error
├─────
│   }
│   
│   fn main(argc: core/c-int, argv: ptr): i32 {
│ ->  let check: 0xCBF43926u32 = #static crc32("123456789".span-bytes())
│                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
│     println("check value 0x${hex(check.from-static())} verified while compiling")
│     let args = std/sys/parse-main-args(argc, argv)
├─────
│  Different static values of same type family: 3421780262 vs 4233047017
└────────────────────────────────────────╴
```

In C the table is a generator script or a startup routine; C++ and Zig can
compute it at compile time, but the self-test is again a separate assertion
rather than the declared type of the result.

## A Brainfuck interpreter, run by the compiler

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

fn main(): i32 {
  let hello: "Hello World!\n" = #static bf("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
  print(hello.from-static())
  0
}
```

```text
Hello World!
```

Thirty lines of plain K1: an `array[u8, 64]` tape, a `string-builder` for
output, a `while` loop with a match on the current instruction, and a bracket
scanner. Nothing in it knows it is going to run inside the compiler. The
`#static` call interprets the classic hello-world program during typechecking,
and `"Hello World!\n"` is the declared type of the result, newline included.

## A program that unit-tests its own type errors

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

`test-compile(expr)` typechecks `expr` and returns the error message as a
`?string`, `:none` if it compiled. It is an ordinary expression, so the
messages can be printed, compared, or asserted on in the same test suite as
everything else. Rust reaches for the `trybuild` crate and separate
`compile_fail` files for this; C++ can ask whether an expression is well-formed
with a `requires` expression, but never gets the message back.

## `#if`, and both worlds at once

```k1
fn where-am-i(): string {
  if core/k1/is-static "compile time" else "run time"
}

fn main(): i32 {
  let platform = #if core/k1/platform-macos "macos" else "not macos"
  println("built for $platform")

  let baked: "compile time" = #static where-am-i()
  println("where-am-i said \"${baked.from-static()}\" then, and \"${where-am-i()}\" now")
  #static println("hello from inside the compiler")
  0
}
```

Compiler output while building, then the program:

```text
[static_both_worlds.k1:11 info] hello from inside the compiler

built for macos
where-am-i said "compile time" then, and "run time" now
```

`#if` picks a branch on a compile-time bool (`core/k1/platform-macos` here)
and only that branch is typechecked. `core/k1/is-static` is the one way the
two worlds differ: the same `where-am-i` returns `"compile time"` when the VM
runs it under `#static` and `"run time"` from the binary. A `println` inside
`#static` does not reach the program's stdout; the compiler surfaces it as an
info message while building, tagged with the line that printed it. Static
parameters (`fn pow[n: static int]`), `#for` unrolling and literal types get
their own treatment in the types section.
