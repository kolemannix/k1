# The runtime model

Compile-time execution gets the headlines; this section is the half of K1 you
touch on every line. Abilities are typeclasses with a one-indirection object
form. Closures capture only what you name. Context parameters make a writer,
an allocator, or the caller's source location ambient. Errors propagate through
an ability, not a keyword. Allocation defaults to an arena. Pattern matching is
exhaustive and can bind through references.

## Abilities

```k1
ability shape {
  fn area(self: *self): f64
  fn name(self: *self): string
  fn sides(): int
  fn describe(self: *self): string {
    "${self.name()} area=${self.area()} sides=${shape@(self)/sides()}"
  }
}
```

An `ability` declares functions over `self`; an `impl ... for` supplies them.
`describe` has a default body, so `circle` inherits it and `rect` overrides it.
`sides` mentions no `self` at all; the default method reaches it through the
qualified form `shape@(self)/sides()`, which picks the impl for whatever type
`self` is.

```k1
fn bigger[t: shape](a: *t, b: *t): *t {
  if a.area() > b.area() a else b
}

ability loud {
  fn shout(self: *self): string
}

impl[t: shape] loud for t {
  fn shout(self: *t): string { "${self.name()}!" }
}
```

`bigger` is a generic function with an ability bound; it is monomorphized per
`t`. `impl[t: shape] loud for t` is a blanket impl: every shape is `loud`
without another line of code per type, the way Rust's `impl<T: Display>
ToString for T` works.

Abilities take type parameters too. `accumulator[impl t]` says the item type is
chosen by the impl, so `summer` accumulates `int` and `joiner` accumulates
`string`:

```k1
ability accumulator[impl t] {
  fn add(self: *self, x: t)
  fn total(self: *self): t
}

type summer = { n: int }
impl accumulator[t = int] for summer {
  fn add(self: *summer, x: int) { self.n = self.n + x }
  fn total(self: *summer): int { self.n }
}

type joiner = { s: string }
impl accumulator[t = string] for joiner {
  fn add(self: *joiner, x: string) { self.s = self.s.concat(x) }
  fn total(self: *joiner): string { self.s }
}
```

## Ability objects

Any implementor erases to `dyn[ability[args]]`, an object you can put in a
list, return from a function, or pass through a generic signature:

```k1
fn drain[t](acc: dyn[accumulator[t = t]], items: list[t]): t {
  for x in items { acc.add(x) }
  acc.total()
}
```

```k1
  let shapes: list[dyn[shape]] = [c.&.to-dyn(), r.&.to-dyn()]
  for s in shapes { println("${s.describe()} (${s.sides()} sides)") }
  println("dyn[shape] is ${types/size[dyn[shape]]} bytes")

  let sum: *summer = mem/new(.{ n = 0 })
  let join: *joiner = mem/new(.{ s = "" })
  println(drain(sum.to-dyn[accumulator[t = int]](), [1, 2, 3]))
  println(drain(join.to-dyn[accumulator[t = string]](), ["a", "b", "c"]))
```
```text
circle area=3.14159 sides=0 (0 sides)
2.0x3.0 rect (4 sides)
dyn[shape] is 40 bytes
6
abc
```

`dyn[shape]` is 40 bytes: a state pointer plus one function pointer per
ability function, laid out inline. A call through it is a single indirect
call. A Rust `&dyn Trait` is 16 bytes but pays a second load through the
vtable pointer on every call; K1 trades object size for that load. Default
methods dispatch through the object (`describe` on the circle above is the
ability's default), and so do functions with no `self` like `sides()`: the
object acts as a type witness and no state is passed. Rust's
dyn-compatibility rules exclude such a method unless it opts out with
`where Self: Sized`.

## Closures

```k1
fn apply(x: int, f: some fn int -> int): int { f(x) }

fn apply-dyn(x: int, f: dyn[fn(int) -> int]): int { f(x) }

fn double(x: int): int { x * 2 }

fn make-adder(n: int): dyn[fn(int) -> int] {
  (fn[n](x: int) x + n).to-dyn()
}

fn main(): i32 {
  let inc = fn(x: int) x + 1
  println(inc(1))

  let base = 10
  let hits = 0
  let tally = fn[base, hits.&](x: int) {
    hits.* = hits.* + 1
    x + base
  }
  println(tally(1))
  println(tally(2))
  println("hits = $hits")

  println(apply(5, double))
  println(apply(5, inc))
  println(apply(5, tally))
  println(apply-dyn(5, tally.to-dyn()))

  let a = mem/current-arena()
  let before = a.used-bytes()
  let local = fn[base](x: int) x * base
  let mid = a.used-bytes()
  let add5 = make-adder(5)
  let after = a.used-bytes()
  println("${local(2)} ${add5(2)}")
  println("local closure: ${mid - before} bytes allocated; returned as dyn: ${after - mid} bytes")
  println("dyn[fn(int) -> int] is ${types/size[dyn[fn(int) -> int]]} bytes")

  let evens = [1, 2, 3, 4, 5, 6]
    || list/map(fn x. x * x)
    || list/filter(fn x. x % 2 == 0)
  println(evens)
  0
}
```
```text
2
11
12
hits = 2
10
6
15
15
20 7
local closure: 0 bytes allocated; returned as dyn: 8 bytes
dyn[fn(int) -> int] is 16 bytes
[4,16,36]
```

A lambda captures nothing unless you list it. `fn[base, hits.&]` copies `base`
into the closure and stores the address of `hits`, and the body sees `hits` as
a reference to store through, so the two flavors of capture are visible at the
definition instead of decided by an escape analysis. A named function or a
lambda is a zero-sized value of its own function type; `some fn int -> int`
accepts any of them and specializes the callee for each, like Rust's `impl
Fn`. `dyn[fn(int) -> int]` is the erased form, 16 bytes, an environment
pointer and a function pointer, and `.to-dyn()` is the one explicit step from
a closure to it (`.&` is the other conversion, to a bare `*fn` pointer).

The allocation story is measured, not asserted: the closure held in `local`
costs 0 bytes of arena because its environment is a by-value local, and
`.to-dyn()` on `fn[n](x: int) x + n` allocates exactly the 8-byte environment
in the ambient arena. Nothing else about a closure touches the allocator. The
pipe `||` feeds a value into the first argument of each call, which is where
the inferred-parameter form `fn x. x * x` reads best.

## Context parameters

```k1
fn greet[w: writer](context out: w)(name: string) {
  out.writeln("hello, $name")
}

fn report(context out: *string-builder)(items: list[string]) {
  for items { out.writeln("${it-index}: $it") }
}

fn check(context locn: k1/source-location)(ok: bool, what: string) {
  if not ok { println("$what failed at ${locn.filename}:${locn.line}") }
}

fn collect-words(n: int): list[string] {
  let words: list[string] = list/empty()
  for i in 0.until(n) { words.push("w$i") }
  words
}

fn main(): i32 {
  let sb = string-builder/new()
  let(context(impl writer)) out: *string-builder = sb.&
  greet("ada")
  report(["x", "y"])
  print(sb.build())
  greet(context core/io/stdout)("stdout")

  check(1 + 1 == 2, "arithmetic")
  check(1 + 1 == 3, "wishful thinking")

  let a = core/arena/init(64 * core/arena/kb)
  let words = mem/with-arena(a, collect-words(3))
  println("$words live in the scoped arena: ${a.used-bytes()} bytes")
  0
}
```
```text
hello, ada
0: x
1: y
hello, stdout
wishful thinking failed at model_context.k1:28
[w0,w1,w2] live in the scoped arena: 152 bytes
```

A function declares context parameters in a list before its ordinary ones. At
the call site they are filled from `let(context)` bindings in scope, or passed
explicitly with `(context value)`. `greet` is generic over any `writer`, and
`let(context(impl writer)) out: *string-builder` registers `sb` under the
ability, so `greet("ada")` solves `w = *string-builder` from the context
variable alone. The same binding also satisfies `report`, whose parameter is
the concrete type. `greet(context core/io/stdout)("stdout")` bypasses the
binding for one call.

`k1/source-location` is the same mechanism: `assert` and `crash` in the core
library take `context locn: k1/source-location`, and the compiler fills it
with the call site. That is what Rust needs `#[track_caller]` for, and what C
does with `__FILE__`/`__LINE__` macros threaded by hand.

The allocator is ambient by a different route: a thread-local arena stack.
`mem/with-arena(a, expr)` pushes `a` for the duration of `expr`, so
`collect-words` allocates its list in `a` without knowing `a` exists. Odin's
`context.allocator` is the nearest relative.

## Error handling

```k1
fn parse-digits(s: string): result[u64, parse-error] {
  require s.len() > 0 else { return :err :empty-input }
  let n: u64 = 0
  for c in s {
    require c.is-ascii-digit() else { return :err :bad-digit c }
    n = n * 10 + (c.as-u8() - '0'.as-u8()).widen[u64]
    require n < 100000 else { return :err :too-big }
  }
  :ok n
}

fn add-parsed(a: string, b: string): result[u64, parse-error] {
  let x = parse-digits(a).try
  let y = parse-digits(b).try
  :ok(x + y)
}

fn first-even(xs: list[int]): ?int {
  let first = xs.first().try
  if first % 2 == 0 :some first else :none
}
```

`result[t, e]` is an ordinary sum type, `either { ok(t), err(e) }`. `.try`
unwraps the `:ok` payload or returns the `:err` from the enclosing function,
like Rust's `?`. `require cond else { ... }` is the early-exit form: the
condition's bindings stay in scope after it, and the else block must diverge.

`.try` is not tied to `result`. It is the `try` ability, which `opt` also
implements with `e = empty`, so `xs.first().try` in a function returning `?int`
propagates `:none`. Rust's `Try` trait has been unstable for years; K1's is
what you implement for your own types:

```k1
type rc = either(i32) { ok = 0, noent = 2, acces = 13 }

ns pre {
  fn errors-of(t: types/type-id, ok-name: string): types/type-id {
    require t.schema() is :enum .{ int-kind, values } else crash("errors-of needs an enum")
    let variants: list[types/make-either.variants.t] = []
    for v in values {
      if v.name != ok-name { variants.push(.{ name = v.name, payload = :none, tag = :some v.value }) }
    }
    types/make-either(:some(int-kind.type-id()), variants.as-span())
  }
}

type errno = #type pre/errors-of(types/id[rc], "ok")

impl try[t = empty, e = errno] for rc {
  fn error(e: errno): rc { mem/bitcast(e) }
  fn value(_t: empty): rc { :ok }
  fn is-ok(self): bool { self is :ok }
  fn get-error(self): errno { mem/bitcast(self) }
  fn get-value(_self: self): empty { .{} }
}

fn c-open(path: string): rc {
  if path.starts-with("/root") :acces else if path.ends-with(".missing") :noent else :ok
}

fn touch(path: string): rc {
  c-open(path).try
  println("touched $path")
  :ok
}
```

`rc` is a C-style status code: an `i32` enum where zero is success. `errno` is
not written out. `pre/errors-of` reads `rc`'s schema at compile time and
builds a new enum from every variant except `ok`, keeping the tag type and
each tag value, so `errno` is `rc` minus `ok` and follows `rc` when a code is
added. Because the tags agree, crossing between the two is `mem/bitcast`, and
with `--optimize` a forwarding `r.try` followed by `:ok`, `get-error`, and
`error` each compile to a bare `ret` of their argument: `.try` on a C status
code costs one compare against zero. The derived type is nominal, named
`errno` in messages and in `types/name`, and a match on the error side needs
no `:ok` arm because there is none. The `?` operator is the fallback form and
`.!` is the assertion form; both are defined in terms of the same ability's
`is-ok` and `get-value`, not in terms of `result`.

```k1
fn main(): i32 {
  println(add-parsed("12", "30"))
  println(add-parsed("12", "3x"))
  println(add-parsed("", "1"))
  println(add-parsed("999999", "1"))
  println(parse-digits("7").!)
  println(parse-digits("seven") ? 0)

  println(first-even([]))
  println(first-even([3]))
  println(first-even([4]))

  for path in ["/tmp/a", "/root/x", "/tmp/b.missing"] {
    if touch(path).result() is {
      :ok _ -> println("$path ok"),
      :err :noent -> println("$path failed: no such file"),
      :err :acces -> println("$path failed: permission denied"),
    }
  }
  println("${types/name[errno]} ${types/size[errno]}")
  0
}
```
```text
:ok(42)
:err(bad digit 'x')
:err(empty input)
:err(too big)
7
0
:none
:none
:some 4
touched /tmp/a
/tmp/a ok
/root/x failed: permission denied
/tmp/b.missing failed: no such file
errno 4
```

## Arenas

```k1
fn render(id: int): string {
  let parts: list[string] = list/empty()
  for i in 0.until(3) { parts.push("item-${id}-${i}") }
  parts.join(",")
}

fn main(): i32 {
  let scratch = core/arena/init(64 * core/arena/kb)
  for id in 0.until(3) {
    let response = mem/with-arena(scratch, render(id))
    println("$response  used=${scratch.used-bytes()}")
    scratch.reset()
  }
  println("after reset: used=${scratch.used-bytes()}")

  let dirty = scratch.alloc-buffer[u64](4)
  dirty.fill(7)
  println(dirty)
  scratch.reset()
  println(scratch.alloc-buffer[u64](4))

  let m = scratch.mark()
  let tmp = scratch.push(99)
  println("${tmp.*} at ${scratch.used-bytes()}")
  scratch.pop-to-mark(m)
  println("popped: ${scratch.used-bytes()}")

  let durable: list[int] = list/with-capacity-in(heap, 4)
  durable.push-in(heap, 1)
  durable.push-in(heap, 2)
  println(durable)
  println(heap.alloc-t[u64]().*)
  0
}
```
```text
item-0-0,item-0-1,item-0-2  used=240
item-1-0,item-1-1,item-1-2  used=240
item-2-0,item-2-1,item-2-2  used=240
after reset: used=0
[7,7,7,7]
[0,0,0,0]
99 at 40
popped: 32
[1,2]
0
```

`render` never mentions an allocator: `list/empty`, `push`, and `join` all
allocate in the ambient arena, which `mem/with-arena` points at `scratch` for
the duration of the call. Each request builds its response in `scratch`,
the caller prints it, and `reset` returns the arena to empty. Three
requests, 240 bytes each, and the arena never grows.

Every allocation in K1 is zeroed, and the arena pays for it only where memory
was actually used: fresh pages arrive zeroed, and `reset` zeroes the bytes
handed out since the last reset, which is why the buffer filled with 7s comes
back as zeros. `mark`/`pop-to-mark` scopes the same trick to a
region. Long-lived data takes an explicit allocator through the `-in`
variants: `list/with-capacity-in(heap, 4)` and `push-in(heap, ...)` go to
the C heap, and `heap.alloc-t[u64]()` comes back zeroed too. Zig makes every allocation
name its allocator; K1 makes the transient case free to write and the durable
case explicit.

## Pattern matching

```k1
type shape = either { circle(f64), square(f64), tri({ a: f64, b: f64, c: f64 }) }

fn classify(s: shape): string {
  if s is {
    :circle r if r > 10.0 -> "big circle",
    :circle _ -> "circle",
    :tri .{ a, b, c } if a == b and b == c -> "equilateral",
    :square _ or :tri _ -> "polygon",
  }
}

fn quadrant(p: { x: int, y: int }): string {
  if p is {
    .{ x = 0, y = 0 } -> "origin",
    .{ x, y } if x > 0 and y > 0 -> "first",
    .{ x } if x < 0 -> "left half",
    _ -> "elsewhere",
  }
}

fn inspect[t](v: t): string {
  if v is {
    type[int](0) -> "zero",
    type[int](n) -> "int $n",
    type[string](s) -> "string of ${s.len()}",
    _ -> "something else",
  }
}

fn main(): i32 {
  println(classify(:circle 12.0))
  println(classify(:tri .{ a = 1.0, b = 1.0, c = 1.0 }))
  println(classify(:square 2.0))
  println(quadrant(.{ x = 0, y = 0 }))
  println(quadrant(.{ x = 3, y = 4 }))
  println(quadrant(.{ x = -1, y = 4 }))
  println(inspect(0))
  println(inspect(9))
  println(inspect("hello"))
  println(inspect(true))

  let r: result[int, string] = :ok 1
  let rr = r.&
  if rr is :ok(v)* { v.* = v.* + 41 }
  println(r)

  let deep: ***bool = mem/new(mem/new(mem/new(true)))
  println(deep is true***)

  require r is :ok n else { crash("unreachable") }
  println("n = $n")
  0
}
```
```text
big circle
equilateral
polygon
origin
first
left half
zero
int 9
string of 5
something else
:ok(42)
true
n = 42
```

`if x is { arms }` is the match expression. Arms take sum patterns with
payload patterns nested inside (`:tri .{ a, b, c }`), struct patterns that
name only the fields they care about, `if` guards, `or` alternatives, and
literals. `type[int](0)` matches on the type of a generic value and then on
its shape, so one `inspect[t]` handles every `t` it is instantiated with.

Matching on a reference binds values by default; a trailing `*` binds a
reference into the payload instead. `rr is :ok(v)*` hands back `*int`
pointing into `r`'s `:ok` slot, and `v.* = v.* + 41` updates `r` in place.
`true***` matches a `bool` behind three references. `require` is the same
pattern language as a statement: `n` is bound for the rest of the function.

Matches are checked for exhaustiveness, and guards do not count as coverage:

```k1
type shape = either { circle(f64), square(f64), tri({ a: f64, b: f64, c: f64 }) }

fn classify(s: shape): string {
  if s is {
    :circle r if r > 10.0 -> "big circle",
    :square _ -> "square",
  }
}

fn main(): i32 {
  println(classify(:circle 1.0))
  0
}
//errmsg: Non-exhaustive match
```
```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/model_matching_wrong.k1:4:5: error
├─────
│   type shape = either { circle(f64), square(f64), tri({ a: f64, b: f64, c: f64 }) }
│   
│   fn classify(s: shape): string {
│ ->  if s is {
│        ^
│       :circle r if r > 10.0 -> "big circle",
│       :square _ -> "square",
├─────
│  Non-exhaustive match; for example, this pattern is not covered:
- :circle(_)
└────────────────────────────────────────╴
Module model_matching_wrong failed typechecking with 1 errors
```

The compiler names a concrete pattern the arms miss. Here `:circle` looked
handled, but only behind a guard, so a small circle falls through. Add a
bare `:circle _` arm and the same error names `:tri(_)`.
