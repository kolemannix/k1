# Macros and metaprogramming

A `macro` is a compile-time function. A `code` parameter gets the caller's
argument unevaluated: its source text and the span it came from. Any other
parameter is evaluated at compile time into an ordinary value. The macro
returns `code`, which is compiled in place of the call. Macro bodies run in
the same VM as `#static`, in plain k1: lists, string builders, pattern
matching and reflection all work.

## Arguments are code, parameters can be values

The argument is spliced, not evaluated:

```k1
let(mutable) calls: int = 0

fn next(): int {
  calls = calls + 1
  calls
}

macro twice(e: code) {
  "$e + $e"
}
```

```k1
  let x = twice(next())
  println("twice(next()) = $x after $calls calls")
```

```text
twice(next()) = 3 after 2 calls
```

`n` is a compile-time `int`, `body` is code:

```k1
macro repeat(n: int, body: code) {
  let cb = code-builder/new()
  for 0.until(n) {
    cb.line(body)
  }
  cb.build()
}
```

```k1
  repeat(3, println("hello"))
```

```text
hello
hello
hello
```

Return empty code and the argument is never compiled, `crash` included:

```k1
macro debug(e: code) {
  #if false "println($e)" else ""
}
```

```k1
  debug(crash("never compiled"))
```

Macros can be generic. `core/meta/baked-variable` turns any compile-time value
into an expression that rebuilds it, so this is a fully unrolled loop with
`it` as a real local:

```k1
macro static-for[t](items: span[t], body: code) {
  let cb = code-builder/new()
  for items {
    cb.write(core/meta/baked-variable("it", it))
    cb.line(body)
  }
  cb.build()
}
```

```k1
  static-for[int]([1, 2, 3], println("unrolled $it"))
```

```text
unrolled 1
unrolled 2
unrolled 3
```

`\expr` quotes into a `code` value: its text plus the source spans it came
from.

```k1
  let c = \next() * 2
  println("code: `${c.text}` spans=${c.spans.len()} sourced=${c.spans.[0].source != 0}")
```

```text
code: `next() * 2` spans=1 sourced=true
```

A string literal in `code` position gets its own span, and a `$c` hole keeps
`c`'s spans as they are:

```k1
  let wrapped: code = "let y = $c"
  println("${wrapped.text} with ${wrapped.spans.len()} spans")
```

```text
let y = next() * 2 with 2 spans
```

`.text` is the flat string; the spans survive every step until the compiler
consumes the result.

## Token macros: typechecked embedded HTML+k1 templating

`code` arguments must parse as k1. `\{ ... }` only has to lex: a
`span[code-token]` parameter gets the balanced group as tokens, each with its
own span. `std/html` is a template language built on that, in a few hundred
lines of k1:

```k1
fn todo-item[w: writer](context w)(t: todo) {
  html/write(\{
    <li class=${if t.done "done" else "open"} data-on:click="@post('/toggle')">
      ${t.title} #if t.done { <small>"(done)"</small> }
    </li>
  })
}
```

Tags and text fold into constant writes at compile time. Holes like
`${t.title}` are spliced back as `code`, typechecked in place, and HTML-escaped
as they're written. `#for`, `#if` and `#do { ... }` drop into k1. `markup` binds a `markup-writer` as its
`writer` context, so a component is just a function writing into that context,
and returns the result as a `dom/node`:

```k1
fn page(user: string, todos: span[todo]): html/dom/node {
  html/markup(\{
    <main id="todos">
      <h1>Hello, ${user}! ${todos.len()} things to do</h1>
      <ul>
        #for t in todos { #do { todo-item(t) } }
      </ul>
    </main>
  })
}
```

```k1
  let todos: span[todo] = [
    .{ title = "Write a <macro>", done = true },
    .{ title = "Watch TV & relax", done = false },
  ]
  let got = page("Ko", todos)
  println(got)
```

```text
<main id="todos"><h1>Hello, Ko! 2 things to do</h1><ul><li class="done" data-on:click="@post(&#39;/toggle&#39;)">Write a &lt;macro&gt; <small>(done)</small></li><li class="open" data-on:click="@post(&#39;/toggle&#39;)">Watch TV &amp; relax</li></ul></main>
```

The macro checks structure as it expands:

```k1
fn card(title: string): html/dom/node {
  html/markup(\{
    <section>
      <h2>$title</h2>
      <p>Cards close their tags.</div>
    </section>
  })
}
```

```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/macro_html_wrong.k1:7:34: error
├─────
│     html/markup(\{
│       <section>
│         <h2>$title</h2>
│ ->      <p>Cards close their tags.</div>
│                                     ^^^
│       </section>
│     })
├─────
│  </div> does not close <p>
```

That's one line in the emitter,
`close-name.fail("</${close-name.text}> does not close <$name>")`: `fail` on a
token reports at that token, in your file. `httpapp` renders its home page
this way.

## Typed HTTP routing

The biggest macro in the repo is `http/routing/define`. `httpapp`
declares its routes once, as a token stream: method, name, path pattern.

```k1 path=examples/httpapp/app.k1
$http/routing/define(route) {
  get    home         "/"
  get    events       "/events"
  get    counter      "/counter/events"
  get    big-file     "/static/big-file.json"
  get    starry       "/app/starry-night"
  get    visits       "/visits" ?{ who: ?string }
  post   login        "/login"
  get    game-events  "/game/{id: u64}/events"
  post   game-poke    "/game/{id: u64}/poke"
  delete delete-game  "/game/{id: u64}"
}
```

A row can also carry query params, written as the struct they parse into. A `?t`
field is optional:

```k1 path=test_src/http_test/http_test.k1
  get  grade-rate "/cmd/grade-rate/{who: string}" ?{ plan-id: int, item-id: u64, note: ?string }
```

`route` is an `either` with one variant per row, carrying the path and
query captures as a struct:

```text
type route = either {
  home, events, counter, big-file, starry,
  visits({ who: ?string }),
  login,
  game-events({ id: u64 }),
  game-poke({ id: u64 }),
  delete-game({ id: u64 }),
}
```

Out come:

- `type route`, one variant per row, with one payload struct for path and query
  captures (`game-events({ id: u64 })`); the query struct is pasted as written,
  and `#meta` fns over the payload's schema decode both kinds of capture and
  write the query string
- `route/parse(method, url)` and `route/parse-request(head)`: an allocation-free trie matcher that percent-decodes,
  answers `HEAD` for `GET` routes, parses captures through `from-string`, scans the
  query for its params without building a list, and returns `:bad-method` with
  the allowed list, `:missing-param`, or `:not-found`
- `impl print for route`, back to a percent-encoded URL: `"$r"` and `r.to-string()`

The macro reads the rows with a token cursor, then emits:

```k1 path=modules/http/routing.k1
macro define(name: code, body: span[code-token]) {
  let entries = parse-route-entries(body).as-span()
  validate-route-entries(entries)

  let cb-storage = code-builder/new()
  let(context) cb = cb-storage.&

  cb.write-code("type $name = either { ")
  cb.join-with(entries, ", ", fn[cb] en. {
    cb.write-token(en.name)
    if payload-type(en) is :some ty cb.write-code("($ty)")
  })
  cb.write(" }\n")

  cb.block("ns for $name", fn[cb, name, entries]. emit-parse-fn(name, entries))
  emit-print-impl(name, entries)

  cb.build()
}
```

Query types are re-emitted from the caller's tokens, so `?{ id: intt }` reports
`Type 'intt' not found` at `intt` in your file. The row parser rejects bad
identifiers, literal segments outside `-._~`, routes deeper than sixteen
segments, a misplaced `*rest`, double bindings, duplicate names, and same-shape
collisions, each at the offending token. The trie emitter also catches one a
hand-written matcher gets wrong silently: literal edges win, so a capture route
has to stay reachable under every literal that shares its prefix.

```k1 path=modules/http/routing.k1
  for t in lit-texts {
    for ci in caps {
      let cshape = shape-suffix(entries.[ci], d + 1)
      let covered = false
      for gi in group {
        let en = entries.[gi]
        if en.path.len() > d and en.path.[d] is :text lt and lt == t {
          if shape-suffix(en, d + 1) == cshape { covered = true }
        }
      }
      if not covered {
        entries.[ci].pattern-tok.fail("Route `${entries.[ci].pattern}` is unreachable for URLs whose segment $d is `$t`: add the same route shape under `$t`, or remove the overlap")
      }
    }
  }
```

Both directions come from the one row, and the test round-trips them:

```k1 path=test_src/http_test/http_test.k1
  let g = route:grade-rate .{ who = "a b", plan-id = -3, item-id = 9, note = :some "x&y=1+z" }
  assert-equals(g.to-string(), "/cmd/grade-rate/a%20b?plan-id=-3&item-id=9&note=x%26y%3D1%2Bz")
  assert-equals((route:grade-rate .{ who = "a/b", plan-id = 1, item-id = 2, note = :some "x y" }).to-string(), "/cmd/grade-rate/a%2Fb?plan-id=1&item-id=2&note=x+y")
  assert(route-from(:GET, g.to-string()) is :ok :grade-rate r and r.who == "a b" and r.plan-id == -3 and r.item-id == 9 and r.note is :some nt and nt == "x&y=1+z")
  assert(route-from(:GET, "/cmd/grade-rate/x?zz=1&note=a+b&item-id=2&&plan-id=1") is :ok :grade-rate r2 and r2.note is :some nn and nn == "a b")
  assert(route-from(:GET, "/cmd/grade-rate/x?item-id=2&plan-id=1") is :ok :grade-rate r3 and r3.note is :none)
  assert-equals((route:grade-rate .{ who = "x", plan-id = 1, item-id = 2, note = :none }).to-string(), "/cmd/grade-rate/x?plan-id=1&item-id=2")
  assert(route-from(:GET, "/cmd/grade-rate/x?plan-id=1") is :err :missing-param m and m.name == "item-id")
  assert(route-from(:GET, "/cmd/grade-rate/x?plan-id=one&item-id=2") is :err :bad-param b and b.got == "one")
  assert(route-from(:GET, "/cmd/grade-rate/x?plan-id=1&item-id=2&plan-id=1") is :err :bad-param b2 and b2.name == "plan-id")
```

## Errors point at your source, not the expansion

```k1
macro swap(a: code, b: code) {
  `
  let tmp = $a
  $a = $b
  $b = tmp
  `
}
```

```k1
  let xs: list[int] = [1, 2, 3]
  swap(xs.[0], xs.[2])
  println("${xs.[0]} ${xs.[1]} ${xs.[2]}")

  let lo = 10
  let hi = 20
  swap(lo, hi)
  println("lo=$lo hi=$hi")
```

```text
3 2 1
lo=20 hi=10
```

Now swap an `int` and a `string`:

```k1
fn main(): i32 {
  let count = 1
  let label = "one"
  swap(count, label)
  0
}
```

```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/macro_provenance_wrong.k1:12:14: error
├─────
│   fn main(): i32 {
│     let count = 1
│     let label = "one"
│ ->  swap(count, label)
│                 ^^^^^
│     0
│   }
├─────
│  Invalid type for assignment: Expected i64 but got string
└────────────────────────────────────────╴
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/macro_provenance_wrong.k1:5:7: error
├─────
│     `
│     let tmp = $a
│     $a = $b
│ ->  $b = tmp
│          ^^^
│     `
│   }
├─────
│  Invalid type for assignment: Expected string but got i64
└────────────────────────────────────────╴
Module macro_provenance_wrong failed compiling with 2 errors
```

Each caret lands on whoever wrote the bad text: `label` is yours, `tmp` is the
macro's. No generated file anywhere. Every chunk of `code` remembers where it
came from.

## A state machine from a table

Definition-level macros are invoked with `$` and emit declarations. This one
turns a transition table into two enums, a `step` method, and a `#static`
test of the generated code:

```k1
$fsm/define(door, {
  use fsm/on
  [
    on("closed", "open",   "opened"),
    on("opened", "close",  "closed"),
    on("closed", "lock",   "locked"),
    on("locked", "unlock", "closed"),
  ]
})
```

```k1
fn main(): i32 {
  let s: door/state = :closed
  let script: span[door/event] = [:lock, :open, :unlock, :open, :close]
  for e in script {
    if s.step(e) is {
      :some next -> {
        println("${s.enum-name()} --${e.enum-name()}--> ${next.enum-name()}")
        s = next
      },
      :none -> println("${s.enum-name()} --${e.enum-name()}--> rejected"),
    }
  }
  0
}
```

```text
closed --lock--> locked
locked --open--> rejected
locked --unlock--> closed
closed --open--> opened
opened --close--> closed
```

The table is a `span[transition]`, so that block is just evaluated at compile
time. The emitter:

```k1
type transition = { from: string, event: string, to: string }

fn on(from: string, event: string, to: string): transition { .{ from, event, to } }
```

`names` collects the states and events, then:

```k1
fn define-impl(name: string, table: span[transition]): code {
  if table.is-empty() crash("fsm `$name`: no transitions")
  let n = names(table)
  for s in n.states {
    let exits = false
    for tr in table { if tr.from == s { exits = true } }
    if not exits crash("fsm `$name`: state `$s` has no outgoing transition")
  }
  let cb-storage = code-builder/new()
  let cb = cb-storage.&
  cb.block("ns $name", fn[cb, n, table]. {
    cb.line("type state = either { ${string/join(n.states, ", ")} }")
    cb.line("type event = either { ${string/join(n.events, ", ")} }")
    cb.block("ns for state", fn[cb, table]. {
      cb.block("fn step(self: state, event: event): ?state", fn[cb, table]. {
        cb.block("if .{ s = self, e = event } is", fn[cb, table]. {
          for tr in table {
            cb.line(".{ s = :${tr.from}, e = :${tr.event} } -> :some :${tr.to},")
          }
          cb.line("_ -> :none,")
        })
      })
    })
  })
  cb.block("#static", fn[cb, name, table]. {
    for tr in table {
      cb.line("assert($name/state:${tr.from}.step(:${tr.event}) is :some :${tr.to})")
    }
  })
  cb.build()
}

macro define(name: code, table: span[transition]) {
  define-impl(name.text, table)
}
```

It expands to exactly what you'd write by hand:

```text
ns door {
  type state = either { closed, opened, locked }
  type event = either { open, close, lock, unlock }
  ns for state {
    fn step(self: state, event: event): ?state {
      if .{ s = self, e = event } is {
        .{ s = :closed, e = :open } -> :some :opened,
        .{ s = :opened, e = :close } -> :some :closed,
        .{ s = :closed, e = :lock } -> :some :locked,
        .{ s = :locked, e = :unlock } -> :some :closed,
        _ -> :none,
      }
    }
  }
}
#static {
  assert(door/state:closed.step(:open) is :some :opened)
  assert(door/state:opened.step(:close) is :some :closed)
  assert(door/state:closed.step(:lock) is :some :locked)
  assert(door/state:locked.step(:unlock) is :some :closed)
}
```

An emitter bug fails the build, not a test. Drop the `unlock` row and
`locked` becomes a dead end:

```k1
$fsm/define(door, {
  use fsm/on
  [
    on("closed", "open",  "opened"),
    on("opened", "close", "closed"),
    on("closed", "lock",  "locked"),
  ]
})
```

```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/macro_state_machine_wrong.k1:39:17: error
├─────
│     for s in n.states {
│       let exits = false
│       for tr in table { if tr.from == s { exits = true } }
│ ->    if not exits crash("fsm `$name`: state `$s` has no outgoing transition")
│                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
│     }
│     let cb-storage = code-builder/new()
├─────
│  fsm `door`: state `locked` has no outgoing transition
│  bc Execution Trace
│  [00] core/platform/process/abort platform.k1:137
│  [01] core/crash builtin.k1:1237
│  [02] macro_state_machine_wrong/pre/fsm/define-impl macro_state_machine_wrong.k1:39
│  [03] macro_state_machine_wrong/pre/fsm/define macro_state_machine_wrong.k1:66
└────────────────────────────────────────╴
Module macro_state_machine_wrong failed compiling with 1 errors
```

## Library megaprograms: bitfields and struct-of-arrays

`std/deftype/bitfield` is the same trick as a library. One `$` line gives you a
`u16` newtype, a mask per field, typed getters and `with-` setters, and `print`:

```k1
$std/deftype/bitfield(ship(u16)) { shielded, cloaked, damaged, sector = 5, hull = 8 }
```

```k1
fn main(): i32 {
  let s = ship.0.with-shielded(true).with-sector(17).with-hull(200)
  println("bits=${s.bits} $s")
  println("sector mask=${ship/sector-mask} hull mask=${ship/hull-mask}")
  let t = s.with-shielded(false).with-damaged(true)
  println("$t")
  0
}
```

```text
bits=51337 shielded=true,cloaked=false,damaged=false,sector=17,hull=200
sector mask=248 hull mask=65280
shielded=false,cloaked=false,damaged=true,sector=17,hull=200
```

The macro receives the name and the member list as tokens. A bare member is
one bit; `= n` is evaluated on the spot with `meta/eval`, and a field that
doesn't fit fails on its own name:

```k1 path=modules/std/deftype.k1
  for member in members {
    let width: size = if member.value is :some tokens meta/eval("$tokens") else 1
    if width < 1 or total + width > 64 {
      member.name.fail("`${member.name.text}` is $width bits wide at bit $total; a bitfield holds 64")
    }
    widths.push(width)
    total = total + width
  }
```

Each field's accessors are one quoted template with the computed mask, offset
and types spliced in:

```k1 path=modules/std/deftype.k1
    accessors.writeln(`
      let ${field}-mask: $backing = $mask
      fn get-$field(self): $field-type {
        let field = (self.bits >> $offset) & $low-mask
        $narrowed
      }
      fn with-$field(self, value: $field-type): $name {
        let field: $backing = $widened
        .{ bits = (self.bits & ${field}-mask.bit-not()) | ((field & $low-mask) << $offset) }
      }
    `)
```

Masks and shifts come out as constants, and the five-bit `sector` gets the
smallest unsigned type that holds it:

```text
type ship = { bits: u16 }
ns for ship {
  let sector-mask: u16 = 248
  fn get-sector(self): u8 {
    let field = (self.bits >> 3) & 31
    field.low[u8]
  }
  fn with-sector(self, value: u8): ship {
    let field: u16 = value
    .{ bits = (self.bits & sector-mask.bit-not()) | ((field & 31) << 3) }
  }
```

`types/define-soa` isn't even a macro: it's a plain function returning
`code`, run with `#meta` at definition level. Hand it a struct type, get a
struct-of-arrays:

```k1
#meta types/define-soa[{ x: f32, y: f32, mass: f32 }]("particles")
```

```k1
fn main(): i32 {
  let ps = particles/empty()
  ps.push(.{ x = 1.0, y = 2.0, mass = 0.5 })
  ps.push(.{ x = 3.0, y = 4.0, mass = 1.5 })
  ps.push(.{ x = 5.0, y = 6.0, mass = 2.0 })
  let total = 0.0: f32
  for m in ps.mass { total = total + m }
  let p = ps.row(1)
  println("row 1 = (${p.x}, ${p.y}) mass ${p.mass}; total mass $total")
  0
}
```

```text
row 1 = (3.0, 4.0) mass 1.5; total mass 4.0
```

The column struct comes from `map-struct`, which swaps each field's type for a
`list` of it. `type-id.spell()` writes a named type as its absolute path
(`/core/string`) and an anonymous one, as here, as `#type` over its id, so emitted code
means the same type in any scope:

```text
type particles = (#type /core/types/type-id/make(8741))
ns for particles {
  fn row(self: particles, index: size): (#type /core/types/type-id/make(8736)) { .{
    x = self.x.[index],
    y = self.y.[index],
    mass = self.mass.[index],
  } }
  fn empty(): particles { .0 }
  fn push(self: *particles, value: (#type /core/types/type-id/make(8736))) {
    self.x.push(value.x)
    self.y.push(value.y)
    self.mass.push(value.mass)
  }
}
```

## Tokens, then a checked type

`define-soa` wants its struct as a type argument. A macro can take the fields
as bare tokens and still reflect on a checked type: it re-spells the tokens as
a type literal and hands them to `meta/eval`, which evaluates code at
the call site while the macro runs, so `id` is the type the compiler built from
those tokens. The macro names it, bakes a `describe` string from its schema,
and writes a `write-row` that prints a missing optional as `-`:

```k1
  macro define-record(name: code, field-tokens: span[code-token]) {
    let id = meta/eval[types/type-id]("types/id[{ $field-tokens }]")
    let fields = id.schema().as-struct().!.fields
    let cb-storage = code-builder/new()
    let cb = cb-storage.&
    cb.line("type $name = #type types/type-id/make(${id.inner})")
    cb.block("ns for $name", fn[cb, fields, name]. {
      let parts = list/empty[string]()
      for f in fields { parts.push("${f.name}: ${f.type-id.name()}") }
      cb.line("fn describe(): string { ${meta/str-lit(string/join(parts, ", "))} }")
      cb.block("fn write-row[w: writer](self: $name, w)", fn[cb, fields]. {
        for f in fields {
          cb.line(`w.write(${meta/str-lit("${if it-index == 0 "" else " "}${f.name}=")})`)
          if f.type-id.is-instance-of[opt] {
            cb.line(`if self.${f.name} is :some v v.print-to(w) else w.write("-")`)
          } else {
            cb.line("self.${f.name}.print-to(w)")
          }
        }
      })
    })
    cb.build()
  }
```

It lives in `ns pre`, which compiles before the rest of the file, so a `$`
line at the top can call it:

```k1
$pre/define-record(point, \{ x: int, label: string, note: ?string })
```

```k1
  let p: point = .{ x = 1, label = "a", note = :none }
  let sb = string-builder/new()
  p.write-row(sb.&)
  println(point/describe())
  println(sb.peek())
```

```text
x: i64, label: string, note: opt[string]
x=1 label=a note=-
```

`int` comes back as `i64`: the names come from the checked type, not the
tokens.

## Small macros in the core library

Macros aren't only for megaprograms. `mem/with-arena` is one only because the
expression has to run between the push and the pop:

```k1 path=modules/core/mem.k1
  macro with-arena(arena: code, expr: code) {
    `
    let start-len = mem/arena-stack-len()
    mem/push-arena($arena)
    defer {
      mem/pop-arena()
      assert-equals[size](mem/arena-stack-len(), start-len)
    }
    ($expr)
    `
  }
```

No closure, no runtime cost, and a type error inside `with-arena(a, ...)`
still points at your code.

## Elsewhere

- Rust proc macros live in a separate crate, parse a `TokenStream`
  themselves, and give tokens they create `Span::call_site()` unless you
  thread `quote_spanned!` by hand.
- Zig's `comptime` can build the enums with `@Type` but can't emit
  declarations: no generated `step`, no namespace.
- C++ templates and `constexpr` can't either; P2996's `define_aggregate`
  adds data members, not functions.
