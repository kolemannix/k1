# Macros and metaprogramming

A K1 `macro` is a compile-time function. Its bare parameters receive the
caller's argument as a `code` value: the source text plus the span it came
from. Annotated parameters (`n: int`, `table: span[transition]`) are evaluated
at compile time into ordinary values. The macro returns `code`, and the
compiler compiles that in place of the call. Macro bodies run in the same
bytecode VM as `#static`, in the same language as the rest of the program:
lists, string builders, pattern matching, and reflection are all available to
the emitter.

## Arguments are code, parameters can be values

```k1
let(mutable) calls: int = 0

fn next(): int {
  calls = calls + 1
  calls
}

macro twice(e) {
  "$e + $e"
}

macro repeat(n: int, body) {
  let cb = code-builder/new()
  for 0.until(n) {
    cb.line(body)
  }
  cb.build()
}

macro debug(e) {
  #if false "println($e)" else ""
}

macro static-for[t](items: span[t], body) {
  let cb = code-builder/new()
  for items {
    cb.write(core/meta/baked-variable("it", it))
    cb.line(body)
  }
  cb.build()
}

fn main(): i32 {
  let x = twice(next())
  println("twice(next()) = $x after $calls calls")

  repeat(3, println("hello"))

  debug(crash("never compiled"))

  static-for[int]([1, 2, 3], println("unrolled $it"))

  let c = \next() * 2
  println("chunk: `${c.chunks.[0].text}` sourced=${c.chunks.[0].source != 0}")
  let wrapped: code = "let y = $c"
  println("${wrapped.text()} in ${wrapped.chunks.len()} chunks")
  0
}
```

```text
twice(next()) = 3 after 2 calls
hello
hello
hello
unrolled 1
unrolled 2
unrolled 3
chunk: `next() * 2` sourced=true
let y = next() * 2 in 2 chunks
```

`twice(next())` expands to `next() + next()`: the argument is spliced twice,
so it runs twice, and the result is `1 + 2`. `repeat` mixes both parameter
kinds: `n` is a compile-time `int`, `body` is code, and `cb.line` appends it
three times. `debug` returns the empty string under `#if false`, so its
argument is never compiled, `crash` included. `static-for` is a generic macro:
for every element of a compile-time span it bakes the value into a `let it`
(`core/meta/baked-variable` serializes any compile-time value into an
expression that rebuilds it) and splices the body after it, so the loop is
fully unrolled with `it` bound as a real local.

The last lines show `code` itself. `\expr` quotes a statement into a `code`
value: one chunk holding the text and its source span. A string literal whose
expected type is `code` elaborates into chunks too: the literal part gets its
own span, and the `$c` hole appends `c`'s chunks unchanged. `.text()` flattens
the chunks into a string; nothing else ever does, so spans survive every
composition step until the compiler consumes the result.

## Errors point at your source, not the expansion

`swap` takes two places and emits a three-statement block around them:

```k1
macro swap(a, b) {
  `
  let tmp = $a
  $a = $b
  $b = tmp
  `
}

fn main(): i32 {
  let xs: list[int] = [1, 2, 3]
  swap(xs.[0], xs.[2])
  println("${xs.[0]} ${xs.[1]} ${xs.[2]}")

  let lo = 10
  let hi = 20
  swap(lo, hi)
  println("lo=$lo hi=$hi")
  0
}
```

```text
3 2 1
lo=20 hi=10
```

Now call it with an `int` and a `string`:

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
Module macro_provenance_wrong failed typechecking with 2 errors
```

The expansion contains two bad assignments, and each error box carets the
text that actually caused it. `count = label` fails on `label`, which the
caller wrote, so the caret sits on the caller's line. `label = tmp` fails on
`tmp`, which the macro author wrote, so that caret sits inside the macro body.
No generated file appears anywhere. This is the difference between `code` and
a string: every chunk remembers where it came from, and the compiler maps
byte offsets in the expansion back through those chunks. A Rust proc macro
keeps the spans of tokens it passes through, but tokens it creates get
`Span::call_site()`, the whole invocation, unless the author threads spans
through `quote_spanned!` by hand. Here both halves come for free: a string
literal in `code` position carries its own span, and a `\` quote carries the
argument's.

## A definition-level megaprogram: a state machine from a table

Definition-level macros are invoked with the `$` sentinel and their output is
compiled as declarations. This one takes a transition table and emits two
enums, a `step` method with a full match, and a `#static` block that checks
the generated code against the table at compile time:

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

The table argument is annotated `span[transition]`, so the block is evaluated
at compile time as a normal expression: it opens a `use`, calls a helper, and
yields a list. The emitter lives in the module's `pre` namespace, which is
compiled before the rest of the module so that definition-level macros can
run early:

```k1
ns pre { ns fsm {

type transition = { from: string, event: string, to: string }

fn on(from: string, event: string, to: string): transition { .{ from, event, to } }

fn names(table: span[transition]): { states: list[string], events: list[string] } {
  let states = list/empty[string]()
  let events = list/empty[string]()
  for tr in table {
    if not states.contains(tr.from) { states.push(tr.from) }
    if not states.contains(tr.to) { states.push(tr.to) }
    if not events.contains(tr.event) { events.push(tr.event) }
  }
  .{ states, events }
}

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

macro define(name, table: span[transition]) {
  define-impl(name.text(), table)
}

}}
```

`code-builder` handles the shape of the output: `block` writes a header, a
brace, an indented body, and the closing brace; `line` indents and appends a
`code` value, so the template literals at each call site keep their spans.
Nested blocks capture the builder reference at each level. The generated text
(obtained by printing `define-impl(...).text()` inside a `#static` block; the
`#debug` marker on a macro definition dumps the macro's own IR rather than its
expansion) is exactly what a person would have written:

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

The emitted `#static` block runs the generated `step` in the compile-time VM
on every row of the table, so a bug in the emitter fails the build rather than
a test. Validation runs in the emitter as well. Drop the `unlock` row so that
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
[00] core/crash builtin.k1:1083
[01] macro_state_machine_wrong/pre/fsm/define-impl macro_state_machine_wrong.k1:37
[02] macro_state_machine_wrong/pre/fsm/define macro_state_machine_wrong.k1:64

└────────────────────────────────────────╴
Module macro_state_machine_wrong failed namespace declaration with 1 errors
```

The error is reported at the emitter's `crash` call, with the VM's stack from
there back to the macro, and the build stops. Compare the alternatives: Zig's `comptime` can synthesize the two
enums with `@Type` but cannot emit declarations, so no generated `step`, no
namespace, and no compile-time test from the table; C++ templates and
`constexpr` cannot either, and P2996 reflection adds `define_aggregate` for
data members, not functions; a Rust proc macro can do all of it, but lives in
a separate crate, receives a `TokenStream` it has to parse itself, and cannot
call the program it is generating for. Here the emitter is forty lines in the
same file, its input is a typed value, and its output is checked by running
it.

## Library megaprograms: bitfields and struct-of-arrays

`std/bitfield` is the same technique packaged as a library. One `$` line
produces a newtype over `u16`, a mask constant per field, typed getters and
setters, and a `print` impl:

```k1
use std/bitfield/b1
use std/bitfield/bn

$std/bitfield/define[u16](ship, [b1(shielded), b1(cloaked), b1(damaged), .{ name = "sector", bits = 5 }, bn("hull", 8)])

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

The `members` argument is a compile-time `span[{ name: string, bits: size }]`.
`b1(shielded)` is itself a macro that turns a bare identifier into a record
literal, and `bn("hull", 8)` is a plain function; both are just expressions
evaluated to produce the argument. The layout work happens once, in the
emitter, using `types/id[int-type]` reflection to learn the base width:

```k1 path=modules/std/bitfield.k1
  // Now let's do a getter and setter for each bitfield. If the width is 1,
  // we'll use bool since that's what the people likely want. Note that we could configure
  // any of this behavior because this is just a regular function
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

```k1 path=modules/std/bitfield.k1
macro define[int-type](type-name, members: span[{ name: string, bits: size}]) {
  code/from-string(define-impl(types/id[int-type](), type-name.text(), members))
}
macro b1(name) { `.{ name = "$name", bits = 1 }` }
macro b8(name) { `bn("$name", 8)` }
```

An excerpt of the expansion for `ship` (printed from a `#static` block by
calling `define-impl` directly): masks and shifts are already constants, and
the five-bit `sector` field got the smallest unsigned type that holds it.

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

`core/meta/define-soa` is a `#meta` metaprogram rather than a macro: a plain
function that returns `code`, invoked with `#meta` at definition level. Given
a struct type it emits a struct-of-arrays container with `empty`, `push`, and
`row`, using the type's schema to enumerate the fields:

```k1
#meta core/meta/define-soa[{ x: f32, y: f32, mass: f32 }]("particles")

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

Its expansion shows a detail of emitting types: `type-id.spell()` renders a
type as a `#type` expression over its id, so the emitted code resolves to the
exact same type no matter which scope it is compiled in. `f32` comes out as
`(#type type-id/make(14))`, and the anonymous input struct likewise.

```text
type particles = {
  ARR_x: list[(#type type-id/make(14))],
  ARR_y: list[(#type type-id/make(14))],
  ARR_mass: list[(#type type-id/make(14))],
}
ns for particles {
  fn row(self: particles, index: size): (#type type-id/make(6750)) {
    let(returned) ret: (#type type-id/make(6750)) = .0;
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
  fn push(self: *particles, value: (#type type-id/make(6750))) {
    self.ARR_x.push(value.x);
    self.ARR_y.push(value.y);
    self.ARR_mass.push(value.mass);
}
}
```

## Typed HTTP routing

The largest macro in the repo is `http/pre/routing/define`, used by the
`httpapp` dogfood program. The application declares its routes once, as data:

```k1 path=dogfood/httpapp/app.k1
$http/pre/routing/define(route, {
  use http/pre/routing/get
  use http/pre/routing/post
  use http/pre/routing/delete
  use http/pre/routing/seg
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

From that table the macro generates three things. First, `type route`, an
enum with one variant per row, where rows with captures get a payload struct
(`game-events({ id: u64 })`). Second, `route/parse(header)`, which returns
`result[route, routing/route-error]`: it splits the path into up to sixteen
segment views without allocating, then descends a trie the emitter laid out
from the table, taking literal edges before capture edges at each depth,
percent-decoding segments as it goes; at a leaf it checks the method (a `GET`
route also answers `HEAD` unless the leaf declares its own `HEAD` route),
parses captures through the `from-string` ability, and otherwise answers
`:bad-method` with the allowed list, or `:not-found`. Third, `route/url` and `route/url-string`, the reverse mapping
from a `route` value back to a percent-encoded URL. The enum assembly and the
top of the emitter:

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
`rest` segment that is not last, a parameter bound twice, duplicate names, and
two routes with the same method and shape. The trie emitter adds a
reachability check that a hand-written matcher would silently get wrong:
because literal edges are committed to first, a capture route must remain
matchable under every literal that shares its prefix:

```k1 path=modules/http/routing.k1
  // Reachability: taking a literal edge must never lose a URL that the capture
  // edge could have matched, since the matcher commits to literals first
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

The application then does what the state machine example did: it runs the
generated parser and URL renderer in a `#static` block, so the route table is
round-trip tested every time the program compiles:

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

## Small macros in the core library

Macros are not reserved for megaprograms. `mem/with-arena` scopes the ambient
allocator around an expression, and needs a macro only because the expression
must be evaluated inside the push/pop pair:

```k1 path=modules/core/mem.k1
  macro with-arena(arena, expr) {
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

A closure would do the same job at the cost of a call and an environment; the
macro costs nothing at runtime and keeps the spliced expression's spans, so a
type error inside `with-arena(a, ...)` still points at the caller's code.
