# The runtime model

Compile-time execution gets the headlines, but this is the half of k1 you
touch on every line: abilities, closures, context parameters, errors, arenas,
and pattern matching.

## Abilities

Typeclasses. Functions over `self`, optionally with a default body:

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

`sides` has no `self`, so the default reaches it with `shape@(self)/sides()`.
`circle` takes the default `describe`, `rect` overrides it:

```k1
impl shape for circle {
  fn area(self: *circle): f64 { 3.14159 * self.radius * self.radius }
  fn name(_self: *circle): string { "circle" }
  fn sides(): int { 0 }
}

impl shape for rect {
  fn area(self: *rect): f64 { self.w * self.h }
  fn name(_self: *rect): string { "rect" }
  fn sides(): int { 4 }
  fn describe(self: *rect): string { "${self.w}x${self.h} rect" }
}
```

```k1
  println(c.describe())
  println(r.describe())
```

```text
circle area=3.14159 sides=0
2.0x3.0 rect
```

Bounded generics, and blanket impls: every shape is `loud`.

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

```k1
  println(bigger(r.&, unit.&).describe())
  println(r.shout())
```

```text
2.0x3.0 rect
rect!
```

Operators are abilities. `+` is `add`, so implementing it is operator
overloading:

```k1
type vec2 = { x: f64, y: f64 }

impl add for vec2 {
  fn add(self: vec2, rhs: vec2): vec2 { .{ x = self.x + rhs.x, y = self.y + rhs.y } }
}
```

```k1
  let v = vec2.{ x = 1.0, y = 2.0 } + vec2.{ x = 0.5, y = 0.25 }
  println("${v.x} ${v.y}")
```

```text
1.5 2.25
```

Abilities take type parameters too. `[impl t]` means the impl picks `t`:

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

Any implementor erases to `dyn[...]`:

```k1
  let shapes: list[dyn[shape]] = [c.&.to-dyn(), r.&.to-dyn()]
  for s in shapes { println("${s.describe()} (${s.sides()} sides)") }
  println("dyn[shape] is ${types/size[dyn[shape]]} bytes")
```

```text
circle area=3.14159 sides=0 (0 sides)
2.0x3.0 rect (4 sides)
dyn[shape] is 40 bytes
```

40 bytes: a state pointer plus one function pointer per ability function,
inline. Every call is a single indirect call, with no vtable load. Even
`sides()`, which has no `self`, dispatches: the object is a type witness.
Rust's dyn-compatibility rules won't let you do that.

Generic abilities erase just as well:

```k1
fn drain[t](acc: dyn[accumulator[t = t]], items: list[t]): t {
  for x in items { acc.add(x) }
  acc.total()
}
```

```k1
  let sum: *summer = mem/new(.{ n = 0 })
  let join: *joiner = mem/new(.{ s = "" })
  println(drain(sum.to-dyn[accumulator[t = int]], [1, 2, 3]))
  println(drain(join.to-dyn[accumulator[t = string]], ["a", "b", "c"]))
```

```text
6
abc
```

## Closures

A lambda captures nothing unless you list it. `base` is copied, `hits` is
captured by reference:

```k1
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
```

```text
2
11
12
hits = 2
```

Every function and lambda is a zero-sized value of its own type. `some fn`
specializes per argument, like Rust's `impl Fn`; `dyn[fn]` erases:

```k1
fn apply(x: int, f: some fn int -> int): int { f(x) }

fn apply-dyn(x: int, f: dyn[fn(int) -> int]): int { f(x) }

fn double(x: int): int { x * 2 }

fn make-adder(n: int): dyn[fn(int) -> int] {
  (fn[n](x: int) x + n).to-dyn()
}
```

```k1
  println(apply(5, double))
  println(apply(5, inc))
  println(apply(5, tally))
  println(apply-dyn(5, tally.to-dyn()))
```

```text
10
6
15
15
```

What does a closure cost? Measure it:

```k1
  let a = mem/current-arena()
  let before = a.used-bytes()
  let local = fn[base](x: int) x * base
  let mid = a.used-bytes()
  let add5 = make-adder(5)
  let after = a.used-bytes()
  println("${local(2)} ${add5(2)}")
  println("local closure: ${mid - before} bytes allocated; returned as dyn: ${after - mid} bytes")
  println("dyn[fn(int) -> int] is ${types/size[dyn[fn(int) -> int]]} bytes")
```

```text
20 7
local closure: 0 bytes allocated; returned as dyn: 8 bytes
dyn[fn(int) -> int] is 16 bytes
```

`.to-dyn()` allocates exactly the environment, and nothing else about a
closure touches the allocator.

`||` pipes a value into the first argument:

```k1
  let evens = [1, 2, 3, 4, 5, 6]
    || list/map(fn x. x * x)
    || list/filter(fn x. x % 2 == 0)
  println(evens)
```

```text
[4,16,36]
```

## Context parameters

A function can take context parameters before its ordinary ones:

```k1
fn greet[w: writer](context out: w)(name: string) {
  out.writeln("hello, $name")
}

fn report(context out: *string-builder)(items: list[string]) {
  for items { out.writeln("${it-index}: $it") }
}
```

Callers fill them from `let(context)` bindings in scope, or pass one
explicitly:

```k1
  let sb = string-builder/new()
  let(context(impl writer)) out: *string-builder = sb.&
  greet("ada")
  report(["x", "y"])
  print(sb.build())
  greet(context core/io/stdout)("stdout")
```

```text
hello, ada
0: x
1: y
hello, stdout
```

`context(impl writer)` registers `sb` under the ability, so `greet` solves
`w` from it, and `report` still finds it by its concrete type.

The caller's source location works the same way. It's how `assert` and
`crash` report their call site, no `#[track_caller]` or `__LINE__` needed:

```k1
fn check(context locn: k1/source-location)(ok: bool, what: string) {
  if not ok { println("$what failed at ${locn.filename}:${locn.line}") }
}
```

```k1
  check(1 + 1 == 2, "arithmetic")
  check(1 + 1 == 3, "wishful thinking")
```

```text
wishful thinking failed at model_context.k1:28
```

The allocator is ambient too, through a thread-local arena stack. This
function has no idea where its list lives:

```k1
fn collect-words(n: int): list[string] {
  let words: list[string] = list/empty()
  for i in 0.until(n) { words.push("w$i") }
  words
}
```

```k1
  let a = core/arena/init(64 * core/arena/kb)
  let words = mem/with-arena(a, collect-words(3))
  println("$words live in the scoped arena: ${a.used-bytes()} bytes")
```

```text
[w0,w1,w2] live in the scoped arena: 152 bytes
```

## Error handling

`result[t, e]` is a plain `either { ok(t), err(e) }`. `.try` is Rust's `?`,
and `require ... else` exits early:

```k1
fn parse-digits(s: string): result[u64, parse-error] {
  require s.non-empty() else { return :err :empty-input }
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
```

`.!` asserts, `?` falls back:

```k1
  println(add-parsed("12", "30"))
  println(add-parsed("12", "3x"))
  println(add-parsed("", "1"))
  println(add-parsed("999999", "1"))
  println(parse-digits("7").!)
  println(parse-digits("seven") ? 0)
```

```text
:ok(42)
:err(bad digit 'x')
:err(empty input)
:err(too big)
7
0
```

`.try` isn't a `result` feature, it's the `try` ability. `opt` implements it:

```k1
fn first-even(xs: list[int]): ?int {
  let first = xs.first().try
  if first % 2 == 0 :some first else :none
}
```

```k1
  println(first-even([]))
  println(first-even([3]))
  println(first-even([4]))
```

```text
:none
:none
:some 4
```

So can your own types. Here's a C status code, where zero is success:

```k1
type rc = either(i32) { ok = 0, noent = 2, acces = 13 }
```

Its error type is `rc` minus `ok`, computed from `rc`'s schema at compile
time. Add a code to `rc` and `errno` follows:

```k1
  fn errors-of(t: types/type-id, ok-name: string): types/type-id {
    require t.schema() is :enum .{ int-kind, values } else crash("errors-of needs an enum")
    let variants: list[types/make-either.variants.t] = []
    for v in values {
      if v.name != ok-name { variants.push(.{ name = v.name, payload = :none, tag = :some v.value }) }
    }
    types/make-either(:some(int-kind.type-id()), variants.as-span())
  }
```

```k1
type errno = #type errors-of(types/id[rc], "ok")
```

The tags agree, so crossing between the two is a `mem/bitcast`, and `.try` on
an `rc` is one compare against zero:

```k1
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

The error side needs no `:ok` arm, because `errno` doesn't have one:

```k1
  for path in ["/tmp/a", "/root/x", "/tmp/b.missing"] {
    if touch(path).result() is {
      :ok _ -> println("$path ok"),
      :err :noent -> println("$path failed: no such file"),
      :err :acces -> println("$path failed: permission denied"),
    }
  }
  println("${types/name[errno]} ${types/size[errno]}")
```

```text
touched /tmp/a
/tmp/a ok
/root/x failed: permission denied
/tmp/b.missing failed: no such file
errno 4
```

## Arenas

`render` never mentions an allocator:

```k1
fn render(id: int): string {
  let parts: list[string] = list/empty()
  for i in 0.until(3) { parts.push("item-${id}-${i}") }
  string/join(parts, ",")
}
```

```k1
  let scratch = core/arena/init(64 * core/arena/kb)
  for id in 0.until(3) {
    let response = mem/with-arena(scratch, render(id))
    println("$response  used=${scratch.used-bytes()}")
    scratch.reset()
  }
  println("after reset: used=${scratch.used-bytes()}")
```

```text
item-0-0,item-0-1,item-0-2  used=240
item-1-0,item-1-1,item-1-2  used=240
item-2-0,item-2-1,item-2-2  used=240
after reset: used=0
```

Three requests, 240 bytes each, and the arena never grows.

Every allocation in k1 is zeroed. The arena only pays for that where memory
was actually used: `reset` zeroes what it handed out.

```k1
  let dirty = scratch.alloc-buffer[u64](4)
  dirty.fill(7)
  println(dirty)
  scratch.reset()
  println(scratch.alloc-buffer[u64](4))
```

```text
[7,7,7,7]
[0,0,0,0]
```

Same trick, scoped to a region:

```k1
  let m = scratch.mark()
  let tmp = scratch.push(99)
  println("${tmp.*} at ${scratch.used-bytes()}")
  scratch.pop-to-mark(m)
  println("popped: ${scratch.used-bytes()}")
```

```text
99 at 40
popped: 32
```

Long-lived data names its allocator with the `-in` variants:

```k1
  let durable: list[int] = list/with-capacity-in(heap, 4)
  durable.push-in(heap, 1)
  durable.push-in(heap, 2)
  println(durable)
  println(heap.alloc-t[u64].*)
```

```text
[1,2]
0
```

Zig makes every allocation name its allocator. k1 makes the transient case
free to write and the durable case explicit.

## Pattern matching

`if x is { ... }` is the match. Payload patterns nest, and arms take guards
and `or`:

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
```

```k1
  println(classify(:circle 12.0))
  println(classify(:tri .{ a = 1.0, b = 1.0, c = 1.0 }))
  println(classify(:square 2.0))
```

```text
big circle
equilateral
polygon
```

Struct patterns name only the fields they care about:

```k1
fn quadrant(p: { x: int, y: int }): string {
  if p is {
    .{ x = 0, y = 0 } -> "origin",
    .{ x, y } if x > 0 and y > 0 -> "first",
    .{ x } if x < 0 -> "left half",
    _ -> "elsewhere",
  }
}
```

```k1
  println(quadrant(.{ x = 0, y = 0 }))
  println(quadrant(.{ x = 3, y = 4 }))
  println(quadrant(.{ x = -1, y = 4 }))
```

```text
origin
first
left half
```

You can match on the type of a generic value, then on its shape:

```k1
fn inspect[t](v: t): string {
  if v is {
    type[int](0) -> "zero",
    type[int](n) -> "int $n",
    type[string](s) -> "string of ${s.len()}",
    _ -> "something else",
  }
}
```

```k1
  println(inspect(0))
  println(inspect(9))
  println(inspect("hello"))
  println(inspect(true))
```

```text
zero
int 9
string of 5
something else
```

A trailing `*` binds a reference into the payload, so you can write through
it:

```k1
  let r: result[int, string] = :ok 1
  let rr = r.&
  if rr is :ok(v)* { v.* = v.* + 41 }
  println(r)
```

```text
:ok(42)
```

`true***` is a `bool` behind three references:

```k1
  let deep: ***bool = mem/new(mem/new(mem/new(true)))
  println(deep is true***)
```

```text
true
```

`require` takes the same patterns, and its bindings live on past it. One
`require` can take a request apart: a struct pattern, a literal, a variant
with a struct payload, an optional, and guards that call functions on what
was just bound:

```k1
type auth = either {
  anonymous,
  bearer(string),
  basic({ user: string, pass: string }),
}
type request = {
  method: string,
  path: string,
  auth: auth,
  body: ?string,
}
```

```k1
fn known(user: string, pass: string): bool {
  user == "ada" and pass.len() >= 8
}

fn handle(r: request): string {
  require r is .{
      method = "POST",
      path,
      auth = :basic .{ user, pass },
      body = :some body,
    }
    and path.starts-with("/api/")
    and known(user, pass)
    else { return "401 for ${r.method} ${r.path}" }
  "${user} posted ${body.len()} bytes to $path"
}
```

```k1
  let ok: request = .{ method = "POST", path = "/api/notes", auth = :basic .{ user = "ada", pass = "difference engine" }, body = :some "hello" }
  println(handle(ok))
  println(handle(.{ method = "GET", path = "/api/notes", auth = :anonymous, body = :none }))
  println(handle(.{ method = "POST", path = "/api/notes", auth = :basic .{ user = "ada", pass = "short" }, body = :some "hello" }))
  println(handle(.{ method = "POST", path = "/login", auth = :bearer "t0k3n", body = :some "hello" }))
```

```text
ada posted 5 bytes to /api/notes
401 for GET /api/notes
401 for POST /api/notes
401 for POST /login
```

Matches are exhaustive, and guards don't count as coverage:

```k1
fn classify(s: shape): string {
  if s is {
    :circle r if r > 10.0 -> "big circle",
    :square _ -> "square",
  }
}
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
│  Non-exhaustive match; this pattern is not covered:
- :circle(_)
└────────────────────────────────────────╴
Module model_matching_wrong failed compiling with 1 errors
```

Add `:circle _` and it names `:tri(_)` next.
