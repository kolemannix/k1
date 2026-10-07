# Macros and metaprogramming

A `macro` is a compile-time function. A `code` parameter gets the caller's
argument unevaluated: its source text and the span it came from. Any other
parameter is evaluated at compile time into an ordinary value. The macro
returns `code`, which is compiled in place of the call. Macro bodies run in
the same VM as `#static`, in plain K1: lists, string builders, pattern
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

`\expr` quotes into a `code` value: a chunk of text plus its source span.

```k1
  let c = \next() * 2
  println("chunk: `${c.chunks.[0].text}` sourced=${c.chunks.[0].source != 0}")
```

```text
chunk: `next() * 2` sourced=true
```

A string literal in `code` position gets its own span, and a `$c` hole keeps
`c`'s chunks as they are:

```k1
  let wrapped: code = "let y = $c"
  println("${wrapped.text()} in ${wrapped.chunks.len()} chunks")
```

```text
let y = next() * 2 in 2 chunks
```

Only `.text()` flattens chunks into a string, so spans survive every step
until the compiler consumes the result.

## Token macros: typechecked embedded HTML+k1 templating

`code` arguments must parse as K1. `\{ ... }` only has to lex: a
`span[code-token]` parameter gets the balanced group as tokens, each with its
own span. `std/html` is a template language built on that, in a few hundred
lines of K1:

```k1
fn todo-item(out: *string-builder, t: todo) {
  html/write(out, \{
    <li class={if t.done "done" else "open"} data-on:click="@post('/toggle')">
      {t.title} #if t.done { <small>"(done)"</small> }
    </li>
  })
}
```

Tags and text fold into constant writes at compile time. Holes like
`{t.title}` are spliced back as `code`, typechecked in place, and HTML-escaped
as they're written. `#for`, `#if` and `#{ ... }` drop into K1, so a component
is just a function writing into the same builder:

```k1
fn page(user: string, todos: span[todo]): string {
  html/render(\{
    <main id="todos">
      <h1>Hello, {user}! {todos.len()} things to do</h1>
      <ul>
        #for t in todos { #{ todo-item(html-sb.&, t) } }
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
fn card(title: string): string {
  html/render(\{
    <section>
      <h2>{title}</h2>
      <p>Cards close their tags.</div>
    </section>
  })
}
```

```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/macro_html_wrong.k1:7:34: error
├─────
│     html/render(\{
│       <section>
│         <h2>{title}</h2>
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
declares its routes once, as data:

```k1 path=dogfood/httpapp/app.k1
$http/routing/define(route, {
  use http/routing/get
  use http/routing/post
  use http/routing/delete
  use http/routing/seg
  let game: span[seg] = [:text "game", :u64 "id"]

  [
    get(   "home",                    []),
    get(   "events",                  [:text "events"]),
    get(   "counter",                 [:text "counter", :text "events"]),
    get(   "big-file",                [:text "static", :text "big-file.json"]),
    get(   "starry",                  [:text "app", :text "starry-night"]),
    get(   "game-events", game.concat([:text "events"])),
    post(  "game-poke",   game.concat([:text "poke"])),
    delete("delete-game", game),
  ]
})
```

Out come:

- `type route`, one variant per row, with a payload for captures
  (`game-events({ id: u64 })`)
- `route/parse(method, url)` and `route/from-header(header)`: an allocation-free trie matcher that percent-decodes,
  answers `HEAD` for `GET` routes, parses captures through `from-string`, and
  returns `:bad-method` with the allowed list or `:not-found`
- `route/url` and `route/url-string`, back to a percent-encoded URL

The top of the emitter:

```k1 path=modules/http/routing.k1
fn define-impl(base: string, entries: span[entry]): code {
  check-ident(base, "route type name")
  if entries.len() == 0 crash("routing: no routes given")
  validate(entries)

  let cb-storage = code-builder/new()
  let cb = cb-storage.&

  let variants = list/empty[string]()
  for en in entries {
    let caps = params(en)
    if caps.len() == 0 {
      variants.push(en.name)
    } else {
      let fields = list/empty[string]()
      for c in caps { fields.push("${c.name}: ${c.ty}") }
      variants.push("${en.name}({ ${string/join(fields, ", ")} })")
    }
  }
  cb.line("type $base = either { ${string/join(variants, ", ")} }")

  cb.block("ns for $base", fn[cb, base, entries]. {
    emit-parse-fn(cb, base, entries)
    emit-url-fn(cb, base, entries)
    emit-url-string-fn(cb, base)
  })

  cb.build()
}
```

`validate` rejects bad identifiers, routes deeper than sixteen segments, a
misplaced `rest`, double bindings, duplicate names, and same-shape collisions.
The trie emitter also catches one a hand-written matcher gets wrong silently:
literal edges win, so a capture route has to stay reachable under every
literal that shares its prefix.

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
        crash("routing: route `${render-pattern(entries.[ci].path)}` is unreachable for URLs whose segment $d is `$t`: add the same route shape under `$t`, or remove the overlap")
      }
    }
  }
```

And the whole table is round-trip tested in a `#static` block, every compile:

```k1 path=dogfood/httpapp/app.k1
  assert(route/url-string(:home) == "/")
  assert(route/url-string(:game-poke .{ id = 42 }) == "/game/42/poke")
  assert(route/url-string(:big-file) == "/static/big-file.json")

  assert(th(:GET, "/") is :ok :home)
  assert(th(:GET, "/events") is :ok :events)
  assert(th(:HEAD, "/events") is :ok :events)
  assert(th(:GET, "/events?foo=1") is :ok :events)
  assert(th(:GET, "/%65vents") is :ok :events)
  assert(th(:POST, "/game/42/poke") is :ok :game-poke p and p.id == 42)
  assert(th(:POST, "/game/42/poke/") is :ok :game-poke _)
  assert(th(:GET, "/game/42/poke") is :err :bad-method _)
  assert(th(:POST, "/game/abc/poke") is :err :bad-param b and b.got == "abc")
  assert(th(:POST, "/game/18446744073709551617/poke") is :err :bad-param _)
  assert(th(:GET, "/game/42/zzz") is :err :not-found)
  assert(th(:GET, "/nope") is :err :not-found)
  assert(th(:GET, "/game/%zz/poke") is :err :bad-encoding _)
  assert(th(:OPTIONS, "*") is :err :not-found)
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
$pre/fsm/define(door, {
  use pre/fsm/on
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
time. The emitter lives in `pre`, which compiles before the rest of the module:

```k1
ns pre { ns fsm {

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
  define-impl(name.text(), table)
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
$pre/fsm/define(door, {
  use pre/fsm/on
  [
    on("closed", "open",  "opened"),
    on("opened", "close", "closed"),
    on("closed", "lock",  "locked"),
  ]
})
```

```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/macro_state_machine_wrong.k1:37:17: error
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
bc Execution Trace
[00] core/platform/process/exit platform.k1:201
[01] core/sys/exit builtin.k1:964
[02] core/crash builtin.k1:1171
[03] macro_state_machine_wrong/pre/fsm/define-impl macro_state_machine_wrong.k1:37
[04] macro_state_machine_wrong/pre/fsm/define macro_state_machine_wrong.k1:64

└────────────────────────────────────────╴
Module macro_state_machine_wrong failed compiling with 1 errors
```

## Library megaprograms: bitfields and struct-of-arrays

`std/bitfield` is the same trick as a library. One `$` line gives you a `u16`
newtype, a mask per field, typed getters and setters, and `print`:

```k1
use std/bitfield/b1
use std/bitfield/bn

$std/bitfield/define[u16](ship, [b1(shielded), b1(cloaked), b1(damaged), .{ name = "sector", bits = 5 }, bn("hull", 8)])
```

```k1
fn main(): i32 {
  let s = ship/zero.set-shielded(true).set-sector(17).set-hull(200)
  println("bits=${s.bits} $s")
  println("sector mask=${ship/sector-mask} hull mask=${ship/hull-mask}")
  let t = s.set-shielded(false).set-damaged(true)
  println("$t")
  0
}
```

```text
bits=51337 shielded=true,cloaked=false,damaged=false,sector=17,hull=200
sector mask=248 hull mask=65280
shielded=false,cloaked=false,damaged=true,sector=17,hull=0
```

`b1(shielded)` is itself a macro, turning a bare identifier into a record;
`bn("hull", 8)` is a plain function. Either way the argument is a
compile-time `span[{ name: string, bits: size }]`:

```k1 path=modules/std/bitfield.k1
macro define[int-type](type-name: code, members: span[{ name: string, bits: size}]) {
  code/from-string(define-impl(types/id[int-type](), type-name.text(), members))
}
macro b1(name: code) { `.{ name = "$name", bits = 1 }` }
macro b8(name: code) { `bn("$name", 8)` }
```

`define-impl` learns the base width from `types/id[int-type]` and writes each
field's accessors:

```k1 path=modules/std/bitfield.k1
  for members {
    let info = member-info.[it-index]
    let is-bool = it.bits == 1
    let field-type = if is-bool "bool" else "u${info.type-width}"

    // Getter
    code.writeln("  fn get-${it.name}(self: $type-name): $field-type {")

    code.writeln("    let bits: $base-name = self.bits;")
    code.writeln("    let shifted: $base-name = bits >> ${info.offset};")
    code.writeln("    let cleared: $base-name = shifted & ${info.raw-value};")
    if is-bool {
      code.writeln("    cleared == 1")
    } else if field-type == base-name {
      code.writeln("    cleared")
    } else {
      code.writeln("    cleared.trunc[$field-type]")
    }

    code.writeln("  }") // End Getter

    // Setter
    code.writeln("  fn set-${it.name}(self: $type-name, value: $field-type): $type-name {")
    if is-bool {
      code.writeln("    let masked-value: $base-name = value.as-u8();")
    } else {
      code.writeln("    let masked-value: $base-name = value & ${info.raw-value};")
    }
    let clear-mask = info.inv-mask
    code.writeln("    let cleared-bits: $base-name = self.bits & $clear-mask;")
    code.writeln("    let inserted-bits: $base-name = cleared-bits | (masked-value << ${info.offset});")
    code.writeln("    .{ bits = inserted-bits }")
    code.writeln("  }") // End setter
  }
```

Masks and shifts come out as constants, and the five-bit `sector` gets the
smallest unsigned type that holds it:

```text
type ship = { bits: u16 }
ns for ship {
  let shielded-mask: u16 = 1
  let cloaked-mask: u16 = 2
  let damaged-mask: u16 = 4
  let sector-mask: u16 = 248
  let hull-mask: u16 = 65280
  let zero: ship = .{ bits = 0 }
```

```text
  fn get-sector(self: ship): u8 {
    let bits: u16 = self.bits;
    let shifted: u16 = bits >> 3;
    let cleared: u16 = shifted & 31;
    cleared.trunc[u8]
  }
  fn set-sector(self: ship, value: u8): ship {
    let masked-value: u16 = value & 31;
    let cleared-bits: u16 = self.bits & 7;
    let inserted-bits: u16 = cleared-bits | (masked-value << 3);
    .{ bits = inserted-bits }
  }
```

`core/meta/define-soa` isn't even a macro: it's a plain function returning
`code`, run with `#meta` at definition level. Hand it a struct type, get a
struct-of-arrays:

```k1
#meta core/meta/define-soa[{ x: f32, y: f32, mass: f32 }]("particles")
```

```k1
fn main(): i32 {
  let ps = particles/empty()
  ps.push(.{ x = 1.0, y = 2.0, mass = 0.5 })
  ps.push(.{ x = 3.0, y = 4.0, mass = 1.5 })
  ps.push(.{ x = 5.0, y = 6.0, mass = 2.0 })
  let total = 0.0: f32
  for m in ps.ARR_mass { total = total + m }
  let p = ps.row(1)
  println("row 1 = (${p.x}, ${p.y}) mass ${p.mass}; total mass $total")
  0
}
```

```text
row 1 = (3.0, 4.0) mass 1.5; total mass 4.0
```

`type-id.spell()` writes a type as `#type` over its id, so emitted code means
the same type in any scope:

```text
type particles = {
  ARR_x: list[(#type type-id/make(14))],
  ARR_y: list[(#type type-id/make(14))],
  ARR_mass: list[(#type type-id/make(14))],
}
ns for particles {
  fn row(self: particles, index: size): (#type type-id/make(7162)) {
    let(returned) ret: (#type type-id/make(7162)) = .0;
    ret.x = self.ARR_x.[index];
    ret.y = self.ARR_y.[index];
    ret.mass = self.ARR_mass.[index];
    ret
  }
  fn empty(): particles { .{
    ARR_x = list/empty(),
    ARR_y = list/empty(),
    ARR_mass = list/empty(),
  } }
  fn push(self: *particles, value: (#type type-id/make(7162))) {
    self.ARR_x.push(value.x);
    self.ARR_y.push(value.y);
    self.ARR_mass.push(value.mass);
  }
}
```

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
