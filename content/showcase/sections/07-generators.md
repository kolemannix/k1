# Generators: a keyword recognizer and a Brainfuck compiler

Two classic code generators. Each is a plain function in `pre` that builds a
`code` value, plus a one-line `macro` that hands it to the compiler with `$`.
Since the generator is just a function, you can also call it at runtime and
print what it emits.

## A gperf in 140 lines

`gperf` takes a keyword list and spits out a C recognizer. This one is a K1
macro:

```k1
$pre/keywords/define(k1-keyword, [
  "fn", "let", "mut", "and", "or", "if", "else", "while", "loop", "ns", "intern",
  "for", "in", "ability", "impl", "is", "not", "builtin", "where", "context",
  "use", "require", "defer",
])
```

Here's what it generates for five words:

```k1
  print(pre/keywords/generate("tiny", ["if", "in", "is", "fn", "for"]).text())
```

```text
type tiny = either { kw-if, kw-in, kw-is, kw-fn, kw-for }
ns for tiny {
  fn lookup(s: string): ?tiny {
    let n = s.len()
    if n < 2 or n > 3 return :none
    let kl = s.[n - 1].as-u8().widen[size]
    let k0 = s.[0].as-u8().widen[size]
    let h = (n * 137 + kl * 59 + k0 * 125) & 15
    if h is {
      9 -> if s == "if" :some :kw-if else :none,
      1 -> if s == "in" :some :kw-in else :none,
      8 -> if s == "is" :some :kw-is else :none,
      10 -> if s == "fn" :some :kw-fn else :none,
      15 -> if s == "for" :some :kw-for else :none,
      _ -> :none,
    }
  }
  fn lookup-linear(s: string): ?tiny {
    if s is {
      "if" -> :some :kw-if,
      "in" -> :some :kw-in,
      "is" -> :some :kw-is,
      "fn" -> :some :kw-fn,
      "for" -> :some :kw-for,
      _ -> :none,
    }
  }
  fn words(): span[string] {
    ["if", "in", "is", "fn", "for"]
  }
}
#static {
  assert(tiny/lookup("if") is :some :kw-if)
  assert(tiny/lookup("in") is :some :kw-in)
  assert(tiny/lookup("is") is :some :kw-is)
  assert(tiny/lookup("fn") is :some :kw-fn)
  assert(tiny/lookup("for") is :some :kw-for)
}
```

First it picks one, two, then three byte positions until every
`(length, bytes)` signature is distinct. Then it hunts for multipliers that
put every keyword in its own slot:

```k1
fn find-hash(name: string, words: span[string], ps: span[pos]): hash {
  let slots = 1: size
  while slots < words.len() { slots = slots * 2 }
  let x = 88172645463325252: size
  while slots <= 4096 {
    for 0.until(1000) {
      let mults: list[size] = []
      for 0.until(ps.len() + 1) {
        x = x * 6364136223846793005 + 1442695040888963407
        mults.push(((x >> 56) & 255) | 1)
      }
      let h: hash = .{ slots, mults }
      let taken: list[size] = []
      let ok = true
      for w in words {
        let s = slot-of(w, ps, h)
        if taken.contains(s) { ok = false; break }
        taken.push(s)
      }
      if ok return h
    }
    slots = slots * 2
  }
  crash("keywords `$name`: no collision-free hash up to 4096 slots")
}
```

Emitting is a `code-builder` walk:

```k1
  let cb-storage = code-builder/new()
  let cb = cb-storage.&
  cb.line("type $name = either { ${string/join(variants, ", ")} }")
  cb.block("ns for $name", fn[cb, name, words, ps, h, terms, min-len, max-len, lits]. {
    cb.block("fn lookup(s: string): ?$name", fn[cb, words, ps, terms, min-len, max-len, h, lits]. {
      cb.line("let n = s.len()")
      cb.line("if n < $min-len or n > $max-len return :none")
      for p in ps {
        if p is {
          :at i if i < min-len -> cb.line("let k$i = s.[$i].as-u8().widen[size]"),
          :at i -> cb.line("let k$i: size = if n > $i s.[$i].as-u8().widen[size] else 0"),
          :last -> cb.line("let kl = s.[n - 1].as-u8().widen[size]"),
        }
      }
      cb.line("let h = (${terms.build()}) & ${h.slots - 1}")
      cb.block-close("if h is", "}", fn[cb, words, ps, h, lits]. {
        for w in words {
          cb.line("${slot-of(w, ps.as-span(), h)} -> if s == ${lits.[it-index]} :some :kw-$w else :none,")
        }
        cb.line("_ -> :none,")
      })
    })
    cb.block("fn lookup-linear(s: string): ?$name", fn[cb, words, lits]. {
      cb.block-close("if s is", "}", fn[cb, words, lits]. {
        for w in words { cb.line("${lits.[it-index]} -> :some :kw-$w,") }
        cb.line("_ -> :none,")
      })
    })
    cb.block("fn words(): span[string]", fn[cb, lits]. {
      cb.line("[${string/join(lits, ", ")}]")
    })
  })
  cb.block("#static", fn[cb, name, words, lits]. {
    for w in words { cb.line("assert($name/lookup(${lits.[it-index]}) is :some :kw-$w)") }
  })
  cb.build()
}
```

LLVM turns the dense `if h is` into a jump table, so a `tiny` lookup is two
byte loads, three multiplies, one indirect jump and one compare. Every keyword
plus 13 near-misses (`"fnn"`, `"whil"`, `"reinterpret_casts"`, ...), 100,000
times each, `--optimize`, Apple Silicon:

```text
23 k1 keywords, 36 probes
  perfect hash: 3.32 ns/lookup (2300000 hits)
  linear scan : 6.61 ns/lookup (2300000 hits)
92 c++ keywords, 105 probes
  perfect hash: 4.93 ns/lookup (9200000 hits)
  linear scan : 17.78 ns/lookup (9200000 hits)
```

At 23 keywords the plain `if s is` isn't far behind. The hash does one string
compare no matter how many keywords there are, so at 92 it's three and a half
times faster.

The whole search runs in the compile-time VM, during the build. No generator
tool, no generated file to check in.

## Brainfuck, compiled

[Section 1](01-static.md) interprets Brainfuck in `#static`. This one
compiles it: `define` emits `fn hello(): string` with native `while` loops,
and runs of `+`/`>` folded into single adds.

```k1
$pre/bf/define(hello, "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
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
  print(pre/bf/transpile("letter-a", "++++++++[>++++++++<-]>+.").text())
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
  transpile(name.text(), program)
}
```

So unbalanced brackets are a compile error:

```k1 path=content/showcase/examples/bf_transpile_wrong.k1
$pre/bf/define(broken, "++[>+<-")
```

```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/bf_transpile_wrong.k1:42:22: error
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
[00] core/platform/process/exit platform.k1:201
[01] core/sys/exit builtin.k1:964
[02] core/crash builtin.k1:1171
[03] bf_transpile_wrong/pre/bf/emit-body bf_transpile_wrong.k1:42
[04] bf_transpile_wrong/pre/bf/bf_transpile_wrong.pre.bf.transpile_lam_6684 bf_transpile_wrong.k1:62
[05] core/code-builder/block code.k1:85
[06] bf_transpile_wrong/pre/bf/transpile bf_transpile_wrong.k1:65
[07] bf_transpile_wrong/pre/bf/define bf_transpile_wrong.k1:69

└────────────────────────────────────────╴
Module bf_transpile_wrong failed compiling with 1 errors
```

The trace stops at the macro body, not the `$pre/bf/define(broken, ...)`
line. That one could be sharper.
