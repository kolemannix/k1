# A regex engine that runs at compile time

`$pre/regex/define(name, "pattern")` compiles a regular expression while the
program compiles and expands into three ordinary functions: `name-scan`, the
DFA itself; `name(s): bool`, the anchored match; and `name-find(s)`, the
leftmost-longest search. The engine is under 270 lines of plain K1 in `ns pre`
(literals, `.`, `[a-z]` classes, `*` `+` `?`, `|`, grouping, `\` escapes), run
by the compile-time VM. Example: `examples/regex_dfa.k1`.

```k1
$pre/regex/define(ident, "[A-Za-z_][A-Za-z0-9_]*")
$pre/regex/define(number, "[0-9]+([.][0-9]+)?")
$pre/regex/define(email, "[a-z0-9._+-]+@[a-z0-9-]+([.][a-z0-9-]+)*[.][a-z][a-z]+")

fn report(name: string, hit: bool, s: string) {
  let verdict = if hit "match   " else "no match"
  println("$name $verdict \"$s\"")
}

fn main(): i32 {
  for s in ["foo_bar", "x9", "_", "9x", "", "a-b"] { report("ident ", ident(s), s) }
  for s in ["42", "3.14", "0.0", "3.", ".5", "1e5"] { report("number", number(s), s) }
  for s in ["a.b@c.io", "x+y@mail.example.org", "no-at.com", "a@b", "a@b.c"] { report("email ", email(s), s) }

  let line = "total: 12.5 units, 3 left"
  if number-find(line) is :some m {
    println("first number in \"$line\": ${line.slice(m.start, m.end)} at ${m.start}..${m.end}")
  }
  println("ident-find in \"  42abc\": ${ident-find("  42abc")}")
  println("email-find in \"nothing here\": ${email-find("nothing here")}")
  0
}
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
first number in "total: 12.5 units, 3 left": 12.5 at 7..11
ident-find in "  42abc": :some .{ start = 4, end = 7 }
email-find in "nothing here": :none
```

## What the macro emitted

The expansion for `number`, printed by temporarily adding `println(c.text())`
to the macro body (the VM's `println` runs during compilation and tags its
output with the printing line):

```text
[regex_dfa.k1:288 info] fn number-scan(s: string): ?size {
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

Each DFA state is one match arm, each arm a chain of range tests ordered by
target state, and the accepting states sit on one line. The alternative, a
baked `array[u8, states * 256]` table and a four-line loop, is what runtime
regex libraries do because the table is data; here the state count is known,
so the match reads as the automaton: state 1 is "in the integer part", `.`
moves to 2, 3 is "in the fraction". The DFA is minimized (Moore's partition
refinement after subset construction), so `[A-Za-z_][A-Za-z0-9_]*` is two
states, not the eight that subset construction produces from three ranges.
In the `--optimize --emit-llvm` output, `email-scan` dispatches the state with
a single `switch` instruction.

## The engine

A recursive-descent parser that builds a Thompson NFA as it goes. Each
fragment has one start and one end state; `*` `+` `?` add epsilon edges to the
fragment just parsed:

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

Subset construction with epsilon closure, one row of 256 optional targets per
DFA state:

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

The emitter writes K1 source with `code-builder`. Literal template parts carry
their spans into the expansion; the values interpolated in are the state
numbers and the range conditions. One gotcha is visible: `${name}-scan` needs
the braces, because `$name-scan` would read the kebab-case identifier
`name-scan`.

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
  emit(name.text(), compile(pattern))
}
```

Nothing in the engine is a comptime dialect: `list`, `?size`, `string-builder`
and `loop` are the same types and syntax the runtime program uses, and the
engine would run unchanged at runtime.

## A malformed pattern

`examples/regex_dfa_wrong.k1` asks for `(ab|cd*`:

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
[00] core/crash builtin.k1:1083
[01] regex_dfa_wrong/pre/regex/parser/fail regex_dfa_wrong.k1:29
[02] regex_dfa_wrong/pre/regex/parser/atom regex_dfa_wrong.k1:92
[03] regex_dfa_wrong/pre/regex/parser/repeat regex_dfa_wrong.k1:75
[04] regex_dfa_wrong/pre/regex/parser/concat regex_dfa_wrong.k1:66
[05] regex_dfa_wrong/pre/regex/parser/alt regex_dfa_wrong.k1:48
[06] regex_dfa_wrong/pre/regex/compile regex_dfa_wrong.k1:192
[07] regex_dfa_wrong/pre/regex/define regex_dfa_wrong.k1:272

└────────────────────────────────────────╴
Module regex_dfa_wrong failed namespace declaration with 1 errors
```

The parser's crash message carries the pattern, the offset and what was
expected; it is the diagnostic (the test harness matches on it), the box
carets the `crash` in the parser, and the VM's stack shows the descent that
got there, down to the macro. What is still missing is the last hop: the
trace ends in `define`'s body, not at the `$pre/regex/define(broken, ...)`
call or the pattern literal, even though the compiler knows that span. That
is a compiler gap, not an engine one.

## Elsewhere

In C++ this is CTRE: the pattern is parsed by template metaprogramming into a
type-level AST and the matcher is instantiated from it, so a malformed pattern
surfaces as a template instantiation error. In Rust it takes a proc-macro
crate, a separate compilation unit built on `syn` and `quote`; the `regex`
crate itself is a runtime engine. Zig's comptime can parse the pattern and
build the tables, and `inline for` unrolls the loops, but there is no emitted
function to read: the automaton exists only as whatever the comptime-unrolled
code lowers to. Here the expansion is source text, printable and readable, and
the engine that produced it is the same language as the program.
