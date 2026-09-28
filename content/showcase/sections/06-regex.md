# A regex engine that runs at compile time

`$pre/regex/define(name, "pattern")` compiles a regex while your program
compiles, and expands into three plain functions: `name-scan` (the DFA),
`name(s): bool` (anchored match) and `name-find(s)` (leftmost-longest search).
The engine is under 300 lines of ordinary K1 sitting in `ns pre`.

```k1
$pre/regex/define(ident, "[A-Za-z_][A-Za-z0-9_]*")
$pre/regex/define(number, "[0-9]+([.][0-9]+)?")
$pre/regex/define(email, "[a-z0-9._+-]+@[a-z0-9-]+([.][a-z0-9-]+)*[.][a-z][a-z]+")
```

```k1
  for s in ["foo_bar", "x9", "_", "9x", "", "a-b"] { report("ident ", ident(s), s) }
  for s in ["42", "3.14", "0.0", "3.", ".5", "1e5"] { report("number", number(s), s) }
  for s in ["a.b@c.io", "x+y@mail.example.org", "no-at.com", "a@b", "a@b.c"] { report("email ", email(s), s) }
```

```text
ident  match    "foo_bar"
ident  match    "x9"
ident  match    "_"
ident  no match "9x"
ident  no match ""
ident  no match "a-b"
number match    "42"
number match    "3.14"
number match    "0.0"
number no match "3."
number no match ".5"
number no match "1e5"
email  match    "a.b@c.io"
email  match    "x+y@mail.example.org"
email  no match "no-at.com"
email  no match "a@b"
email  no match "a@b.c"
```

Searching:

```k1
  let line = "total: 12.5 units, 3 left"
  if number-find(line) is :some m {
    println("first number in \"$line\": ${line.slice(m.start, m.end)} at ${m.start}..${m.end}")
  }
  println("ident-find in \"  42abc\": ${ident-find("  42abc")}")
  println("email-find in \"nothing here\": ${email-find("nothing here")}")
```

```text
first number in "total: 12.5 units, 3 left": 12.5 at 7..11
ident-find in "  42abc": :some .{ start = 4, end = 7 }
email-find in "nothing here": :none
```

## What the macro emitted

The macro logs its expansion, so compiling prints this for `number`:

```text
[regex_dfa.k1:289 info] fn number-scan(s: string): ?size {
  let state = 0
  let end: ?size = :none
  for c in s {
    state = if state is {
      0 -> if (c >= '0' and c <= '9') 1 else break,
      1 -> if c == '.' 2 else if (c >= '0' and c <= '9') 1 else break,
      2 -> if (c >= '0' and c <= '9') 3 else break,
      3 -> if (c >= '0' and c <= '9') 3 else break,
      _ -> break,
    }
    if state == 1 or state == 3 { end = :some(it-index + 1) }
  }
  end
}
fn number(s: string): bool {
  number-scan(s) is :some n and n == s.len()
}
fn number-find(s: string): ?{ start: size, end: size } {
  for start in 0.until(s.len() + 1) {
    if number-scan(s.drop(start)) is :some n { return :some .{ start, end = start + n } }
  }
  :none
}
```

No 256-entry transition table. The state count is known at compile time, so
the match arms *are* the automaton: 1 is the integer part, `.` moves to 2, 3
is the fraction.

The DFA is minimized, so `ident` is two states, not the eight that subset
construction produces from its three ranges:

```text
[regex_dfa.k1:289 info] fn ident-scan(s: string): ?size {
  let state = 0
  let end: ?size = :none
  for c in s {
    state = if state is {
      0 -> if (c >= 'A' and c <= 'Z') or c == '_' or (c >= 'a' and c <= 'z') 1 else break,
      1 -> if (c >= '0' and c <= '9') or (c >= 'A' and c <= 'Z') or c == '_' or (c >= 'a' and c <= 'z') 1 else break,
      _ -> break,
    }
    if state == 1 { end = :some(it-index + 1) }
  }
  end
}
```

With `--optimize`, the state dispatch in `email-scan` becomes a single LLVM
`switch`.

## The engine

Recursive descent, building a Thompson NFA as it parses. `*` `+` `?` just add
epsilon edges to the fragment it just parsed:

```k1
  fn repeat(self: *parser): frag {
    let f = self.atom()
    loop {
      if self.eat('*') {
        let s = self.state()
        self.eps(s, f.start)
        self.eps(f.end, s)
        f = .{ start = s, end = s }
      } else if self.eat('+') self.eps(f.end, f.start)
      else if self.eat('?') self.eps(f.start, f.end)
      else break
    }
    f
  }
```

```k1
  fn atom(self: *parser): frag {
    let c = self.next()
    if c is {
      '(' -> {
        let f = self.alt()
        if not self.eat(')') self.fail("expected `)`")
        f
      },
      '[' -> self.class(),
      '.' -> self.range(0, 255),
      '\\' -> { let e = self.next(); self.range(e.as-u8(), e.as-u8()) },
      '*' or '+' or '?' -> self.fail("nothing to repeat"),
      _ -> self.range(c.as-u8(), c.as-u8()),
    }
  }
```

Subset construction, one row of 256 optional targets per DFA state:

```k1
fn build(nfa: list[nstate], start: size, accept: size): dfa {
  let sets: list[list[size]] = [closure(nfa, [start])]
  let d: dfa = .{ rows = [], accept = [] }
  let i = 0
  while i < sets.len() {
    let set = sets.[i]
    d.accept.push(set.contains(accept))
    let row = list/with-capacity[?size](256)
    for b in 0.until(256) {
      let moved = list/empty[size]()
      for n in set {
        if nfa.[n].edge is :some e and b >= e.lo and b <= e.hi { moved.push(e.next) }
      }
      row.push(if moved.is-empty() :none else {
        let target = closure(nfa, moved)
        :some(sets.position(target) ? { sets.push(target); sets.len() - 1 })
      })
    }
    d.rows.push(row)
    i = i + 1
  }
  d
}
```

And the emitter just writes K1 source with `code-builder`. (`${name}-scan`
needs the braces: `$name-scan` is the kebab-case identifier `name-scan`.)

```k1
fn emit(name: string, d: dfa): code {
  let accepting = list/empty[string]()
  for a in d.accept { if a accepting.push("state == ${it-index}") }
  let init = if d.accept.[0] ":some 0" else ":none"
  let cb-storage = code-builder/new()
  let cb = cb-storage.&
  cb.block("fn ${name}-scan(s: string): ?size", fn[cb, d, accepting, init]. {
    cb.line("let state = 0")
    cb.line("let end: ?size = $init")
    cb.block("for c in s", fn[cb, d, accepting]. {
      cb.block-close("state = if state is", "}", fn[cb, d]. {
        for row in d.rows { cb.line("${it-index} -> ${arm(row)},") }
        cb.line("_ -> break,")
      })
      cb.line("if ${string/join(accepting, " or ")} { end = :some(it-index + 1) }")
    })
    cb.line("end")
  })
  cb.block("fn $name(s: string): bool", fn[cb, name]. {
    cb.line("${name}-scan(s) is :some n and n == s.len()")
  })
  cb.block("fn ${name}-find(s: string): ?{ start: size, end: size }", fn[cb, name]. {
    cb.block("for start in 0.until(s.len() + 1)", fn[cb, name]. {
      cb.line("if ${name}-scan(s.drop(start)) is :some n { return :some .{ start, end = start + n } }")
    })
    cb.line(":none")
  })
  cb.build()
}

macro define(name: code, pattern: string) {
  let generated = emit(name.text(), compile(pattern))
  k1/emit-compiler-message(k1/location(), :info, generated.text())
  generated
}
```

None of it is a comptime dialect. Same `list`, `?size`, `string-builder` and
`loop` as the runtime program; the engine would run unchanged at runtime.

## A malformed pattern

```k1
$pre/regex/define(broken, "(ab|cd*")
```

```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/regex_dfa_wrong.k1:29:4: error
├─────
│     }
│   
│     fn fail(self: parser, msg: string): never {
│ ->    crash("regex `${self.pat}` at offset ${self.pos}: $msg")
│       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
│     }
│   
├─────
│  regex `(ab|cd*` at offset 7: expected `)`
bc Execution Trace
[00] core/platform/process/exit platform.k1:201
[01] core/sys/exit builtin.k1:964
[02] core/crash builtin.k1:1171
[03] regex_dfa_wrong/pre/regex/parser/fail regex_dfa_wrong.k1:29
[04] regex_dfa_wrong/pre/regex/parser/atom regex_dfa_wrong.k1:92
[05] regex_dfa_wrong/pre/regex/parser/repeat regex_dfa_wrong.k1:75
[06] regex_dfa_wrong/pre/regex/parser/concat regex_dfa_wrong.k1:66
[07] regex_dfa_wrong/pre/regex/parser/alt regex_dfa_wrong.k1:48
[08] regex_dfa_wrong/pre/regex/compile regex_dfa_wrong.k1:192
[09] regex_dfa_wrong/pre/regex/define regex_dfa_wrong.k1:272

└────────────────────────────────────────╴
Module regex_dfa_wrong failed compiling with 1 errors
```

The engine's own `crash` is the diagnostic, and the VM trace walks the descent
back to the macro. It stops in `define`'s body, though, not at the
`define(broken, ...)` call. That's a compiler gap, not an engine one.

## Elsewhere

- C++ (CTRE): template metaprogramming parses the pattern into a type-level
  AST; a bad pattern is a template instantiation error.
- Rust: a proc-macro crate on `syn` and `quote`. The `regex` crate itself is a
  runtime engine.
- Zig: comptime can build the tables and `inline for` can unroll the loop, but
  there's no emitted function to read.
