# Generators: a keyword recognizer and a Brainfuck compiler

Two classic code-generation exercises, done the same way: a plain K1 function
in the `pre` namespace builds a `code` value, and a one-line `macro` hands it
to the compiler at definition level with `$`. The generator runs in the
compile-time VM, its output is compiled in place, and the generator is also an
ordinary function you can call at runtime to look at what it emits.

## A gperf in 120 lines

`gperf` takes a keyword list and emits a C recognizer: pick a few byte
positions that tell the keywords apart, find hash constants that map them to
distinct slots, then compare once. Here the whole pipeline is a K1 macro. It
is invoked twice, on K1's 23 keywords and on the 92 C++ keywords:

```k1
$pre/keywords/define(k1-keyword, [
  "fn", "let", "mut", "and", "or", "if", "else", "while", "loop", "ns", "intern",
  "for", "in", "ability", "impl", "is", "not", "builtin", "where", "context",
  "use", "require", "defer",
])
```

Each invocation emits an enum (`k1-keyword:kw-while`), `lookup`, a naive
`lookup-linear` for comparison, `words()`, and a `#static` block that
round-trips every keyword through `lookup` at compile time. The position
search tries every set of one, two, then three positions (counting from the
front, plus the last byte) until the `(length, bytes)` signatures are all
distinct. The hash search then walks power-of-two table sizes and draws
multipliers from an LCG until no two keywords share a slot:

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

Emission is a `code-builder` walk. Templates in `cb.line` and `cb.block`
elaborate as `code`, so the literal parts keep their source spans; the hash
constants and the per-keyword arms are interpolated:

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

`main` prints `pre/keywords/generate("tiny", ["if", "in", "is", "fn", "for"]).text()`
before it benchmarks anything, so the generated K1 is visible without any
compiler flag. For those five words the search settles on the last byte plus
byte 0, and a 16-slot table:

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
```

The table is the `match`: LLVM lowers the dense integer match to a jump table
and each arm's `s == "if"` to an inlined constant-size compare, so a lookup is
two byte loads, three multiplies, one indirect jump and one compare. The
in-program benchmark runs every keyword plus 13 near-misses (`"fnn"`,
`"whil"`, `"context "`, `"reinterpret_casts"`, ...) 100,000 times through
both recognizers, `k1 --optimize`, Apple Silicon:

```text
23 k1 keywords, 36 probes
  perfect hash: 3.26 ns/lookup (2300000 hits)
  linear scan : 6.55 ns/lookup (2300000 hits)
92 c++ keywords, 105 probes
  perfect hash: 5.06 ns/lookup (9200000 hits)
  linear scan : 17.72 ns/lookup (9200000 hits)
```

The honest part: the naive `if s is { "fn" -> ..., "let" -> ... }` is not
far behind at 23 keywords, and how far depends on how `string ==` lowers.
With string equality as a `memcmp` intrinsic, LLVM turns that match into a
switch on the length followed by a short chain of inlined constant-size
compares, and with at most six keywords per length the chain wins outright
at 2.5 ns. With the current `mem/equals`, a K1 routine, LLVM inlines the
first few compares and then calls the rest, and the chain costs 6.5 ns. The
perfect hash does one compare either way and is flat in the keyword count,
so at 92 keywords it is three and a half times faster (unoptimized the gap
is 23 vs 77 ns and 35 vs 243 ns).
Everything above, search included, happens in the compile-time VM during the
build of the program that uses it: no generator tool, no generated file to
check in, and the `#static` self-check runs before the first native
instruction is emitted.

## Brainfuck, compiled

The sibling section interprets Brainfuck inside `#static`. This macro compiles
it: `$pre/bf/define(hello, "...")` emits `fn hello(): string` whose loops are
native `while` loops and whose runs of `+`/`>` are folded into single adds.

```k1
$pre/bf/define(hello, "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")

fn main(): i32 {
  let baked: "Hello World!\n" = #static hello()
  print(baked.from-static())
  print(hello())
  print(pre/bf/transpile("letter-a", "++++++++[>++++++++<-]>+.").text())
  0
}
```

The emitter is a recursive walk over the program string. `[` opens a
`cb.block("while tape.[p] != 0", ...)` whose body is the recursive call; the
call reports whether it stopped at a `]`, which is how unbalanced brackets are
caught at compile time:

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

macro define(name, program: string) {
  transpile(name.text(), program)
}
```

Running it prints the greeting twice and then the K1 that a small program
(`++++++++[>++++++++<-]>+.`, which prints `A`) turns into:

```text
Hello World!
Hello World!
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

The first `Hello World!` never ran at runtime. `let baked: "Hello World!\n" =
#static hello()` executes the compiled function in the compile-time VM and
checks the result against the literal type; `baked` is a
`static[string, "Hello World!\n"]` and any other output would be a type error
at the declaration. That is the whole chain in one line: a macro generates a
function, the VM runs it, the type system checks what it produced.

An unbalanced program is refused while the macro runs (`bf_transpile_wrong.k1`
defines `broken` from `"++[>+<-"`):

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
[00] core/crash builtin.k1:1083
[01] bf_transpile_wrong/pre/bf/emit-body bf_transpile_wrong.k1:42
[02] bf_transpile_wrong/pre/bf/bf_transpile_wrong.pre.bf.transpile_lam_6215 bf_transpile_wrong.k1:62
[03] core/code-builder/block code.k1:68
[04] bf_transpile_wrong/pre/bf/transpile bf_transpile_wrong.k1:65
[05] bf_transpile_wrong/pre/bf/define bf_transpile_wrong.k1:69

└────────────────────────────────────────╴
Module bf_transpile_wrong failed namespace declaration with 1 errors
```

The message names the program and the byte, the box carets the `crash` in
the emitter, and the trace shows the lambda passed to `cb.block` on the way.
The trace stops at the macro body rather than at the `$pre/bf/define(broken,
...)` line, which is the one thing here that could be sharper.
