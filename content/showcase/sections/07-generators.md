# Generators: a Brainfuck compiler

A classic code generator: a plain function that builds a `code` value, plus a
one-line `macro` that hands it to the compiler with `$`. Since the generator is
just a function, you can also call it at runtime and print what it emits.

[Section 1](01-static.md) interprets Brainfuck in `#static`. This one
compiles it: `define` emits `fn hello(): string` with native `while` loops,
and runs of `+`/`>` folded into single adds.

```k1
$bf/define(hello, "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
```

Call it at compile time and at runtime:

```k1
  let baked: "Hello World!\n" = #static hello()
  print(baked.from-static())
  print(hello())
```

```text
Hello World!
Hello World!
```

`baked` is a `static[string, "Hello World!\n"]`. A macro generated a function,
the VM ran it, and the type checker checked what it printed. Any other output
is a type error.

Here's what `++++++++[>++++++++<-]>+.` (it prints `A`) becomes:

```k1
  print(bf/transpile("letter-a", "++++++++[>++++++++<-]>+.").text)
```

```text
fn letter-a(): string {
  let tape: array[u8, 30000] = .0
  let p: size = 0
  let out = string-builder/new()
  tape.[p] = tape.[p] + 8
  while tape.[p] != 0 {
    p = p + 1
    tape.[p] = tape.[p] + 8
    p = p - 1
    tape.[p] = tape.[p] - 1
  }
  p = p + 1
  tape.[p] = tape.[p] + 1
  out.write-char(tape.[p].as-char())
  out.build()
}
```

The emitter is a recursive walk. `[` opens a `while` block whose body is the
recursive call, and that call reports whether it hit a `]`:

```k1
fn emit-body(cb: *code-builder, name: string, program: string, i: *size): bool {
  while i.* < program.len() {
    let c = program.[i.*]
    if c is {
      '+' or '-' or '>' or '<' -> {
        let n = run-length(program, i.*)
        i.* = i.* + n
        if c is {
          '+' -> cb.line("tape.[p] = tape.[p] + ${n % 256}"),
          '-' -> cb.line("tape.[p] = tape.[p] - ${n % 256}"),
          '>' -> cb.line("p = p + $n"),
          _ -> cb.line("p = p - $n"),
        }
      },
      '.' -> {
        cb.line("out.write-char(tape.[p].as-char())")
        i.* = i.* + 1
      },
      ',' -> crash("bf `$name`: input `,` is not supported (byte ${i.*})"),
      '[' -> {
        let open = i.*
        i.* = i.* + 1
        let closed = false
        cb.block("while tape.[p] != 0", fn[cb, name, program, i, closed.&]. {
          closed.* = emit-body(cb, name, program, i)
        })
        if not closed crash("bf `$name`: unmatched `[` at byte $open")
      },
      ']' -> {
        i.* = i.* + 1
        return true
      },
      _ -> { i.* = i.* + 1 },
    }
  }
  false
}
```

```k1
fn transpile(name: string, program: string): code {
  let cb-storage = code-builder/new()
  let cb = cb-storage.&
  cb.block("fn $name(): string", fn[cb, name, program]. {
    cb.line("let tape: array[u8, 30000] = .0")
    cb.line("let p: size = 0")
    cb.line("let out = string-builder/new()")
    let i = 0: size
    if emit-body(cb, name, program, i.&) crash("bf `$name`: unmatched `]` at byte ${i - 1}")
    cb.line("out.build()")
  })
  cb.build()
}

macro define(name: code, program: string) {
  transpile(name.text, program)
}
```

So unbalanced brackets are a compile error:

```k1 path=content/showcase/examples/bf_transpile_wrong.k1
$bf/define(broken, "++[>+<-")
```

```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/bf_transpile_wrong.k1:44:22: error
├─────
│           cb.block("while tape.[p] != 0", fn[cb, name, program, i, closed.&]. {
│             closed.* = emit-body(cb, name, program, i)
│           })
│ ->        if not closed crash("bf `$name`: unmatched `[` at byte $open")
│                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
│         },
│         ']' -> {
├─────
│  bf `broken`: unmatched `[` at byte 2
bc Execution Trace
[00] core/platform/process/abort platform.k1:295
[01] core/crash builtin.k1:1241
[02] bf_transpile_wrong/pre/bf/emit-body bf_transpile_wrong.k1:44
[03] bf_transpile_wrong/pre/bf/transpile_lam_7582 bf_transpile_wrong.k1:64
[04] core/code-builder/block code.k1:156
[05] bf_transpile_wrong/pre/bf/transpile bf_transpile_wrong.k1:67
[06] bf_transpile_wrong/pre/bf/define bf_transpile_wrong.k1:71

└────────────────────────────────────────╴
Module bf_transpile_wrong failed compiling with 1 errors
```

The trace stops at the macro body, not the `$bf/define(broken, ...)`
line. That one could be sharper.
