# Language basics

Eleven small programs. Each one runs as written.

## Functions and variables

```k1
fn greeting(name: string, height-cm: int, formal: bool): string {
  let title = if formal "Dr. " else ""
  let inches = height-cm * 100 / 254
  "Hello, $title$name, you are ${inches / 12} ft ${inches % 12} in"
}

fn main() {
  let name = "Dennis"
  let line = greeting(name, height-cm = 180, formal = false)
  println(line)
  println(greeting("Ada", height-cm = 170, formal = true))
}
```

```text
Hello, Dennis, you are 5 ft 10 in
Hello, Dr. Ada, you are 5 ft 6 in
```

## list, span, and buffer

Name a better duo than `ptr` and `len`, I'll wait. `list` in `k1` is the growable one (a dynamic array), backed by a `buffer`.

`buffer[t]` is the raw construct that means 'this is memory that holds `t`s'. `span[t]` is your read-only, typically borrowed view of a buffer often called a slice, but I don't like how that implies cutting or partiality. Those closures disappear at runtime, by the way

```k1 path=modules/core/builtin.k1
type list[t] = { buffer: buffer[t], len: size }
```

```k1 path=modules/core/builtin.k1
type span[t] = { buffer: buffer[t] }
```

```k1 path=modules/core/builtin.k1
type string = { span: span[char] }
```

```k1
fn main() {
  let words = "the quick brown fox".split-by-char(' ')
  let last-two = words.slice(2, 4)
  println(last-two)
  let lengths = words.map(fn w. w.len())
  let long = words.filter(fn w. w.len() > 3)
  println("$lengths $long ${lengths.sum()}")
  let s = "hello world"
  println(s.slice(6, 11))
}
```

```text
[brown,fox]
[3,5,5,3] [quick,brown] 16
world
```

## Structs

Structs let us say 'and' at the type level. They're the basic building block of all data composition. `k1` has both named and anonymous structs, and understands their structure and lets you talk about it like typescript. More on that later

```k1
type point = { x: int, y: int }

ns for point {
  fn shifted(self, dx: int, dy: int): point {
    .{ x = self.x + dx, y = self.y + dy }
  }
}

fn main() {
  let p = point.{ x = 1, y = 2 }
  println(p.shifted(dx = 10, dy = 10))
  let origin = point.0
  let up = origin | .{ y = 5 }
  println("$origin $up")
}
```

```text
.{ x = 11, y = 12 }
.{ x = 0, y = 0 } .{ x = 0, y = 5 }
```

## either

`either`s let us say 'or' at the typelevel. Together with structs, we can 'and' and 'or' our way to extremely powerful constructs. This combination is often called "Algebraic Data Types". `k1` will construct a bare integer enum at runtime, or a tagged union, depending on whether any of your variants have payloads. Core types like `result` and `opt` will be familiar to Rust users, and are defined as simply as this. `?u8` is shorthand for `opt[u8]` and is a 2 byte struct at runtime

```k1 path=modules/core/builtin.k1
type opt[t] = either { none, some(t) }
```

```k1 path=modules/core/builtin.k1
type result[t, e] = either { ok(t), err(e) }
```

```k1
type level = either { low = 1, mid = 5, high = 10 }

type shape = either {
  circle(f64),
  rect({ w: f64, h: f64 }),
  dot,
}

fn area(s: shape): f64 {
  if s is {
    :circle r -> 3.14159 * r * r,
    :rect .{ w, h } -> w * h,
    :dot -> 0.0,
  }
}

fn main() {
  println("${level:high} is ${level:high.value}")
  println(area(:rect .{ w = 2.0, h = 3.0 }))
}
```

```text
high is 10
6.0
```

## Optionals and results

```k1
fn find-even(xs: list[int]): ?int {
  for x in xs { if x % 2 == 0 { return :some x } }
  :none
}

fn doubled-even(xs: list[int]): ?int {
  let x = find-even(xs).try
  :some(x * 2)
}

fn parse-age(s: string): result[int, string] {
  require int/from-string(s) is :ok n else { return :err "not a number: $s" }
  if n < 0 :err "negative" else :ok n
}

fn main() {
  println(doubled-even([1, 3, 4]))
  println(doubled-even([1, 3]) ? -1)
  println(parse-age("21"))
  println(parse-age("abc"))
}
```

```text
:some 8
-1
:ok(21)
:err(not a number: abc)
```

## Control flow

loops with value-breaks, a `never` type that models divergence, user-accessible `.try` for error returns, and my favorite, a _binding and matching_ early return construct, which can be chained, called `require`

```k1
fn first-square-over(limit: int): int {
  let i = 0
  loop {
    i = i + 1
    if i * i > limit { break i * i }
  }
}

fn describe-int(n: ?int): string {
  require n is :some x and x > 0 else { return "nothing positive" }
  let parity = if x % 2 == 0 "even" else "odd"
  "$x is $parity"
}

fn give-up(why: string): never {
  crash("giving up: $why")
}

fn half(n: int): ?int {
  if n % 2 == 0 :some(n / 2) else :none
}

fn quarter(n: int): ?int {
  let h = half(n).try
  half(h)
}

fn main() {
  println(describe-int(:some 7))
  println(describe-int(:none))
  let square = first-square-over(50)
  let side = if square == 64 8 else give-up("$square is not 64")
  println("$square $side")
  println("${quarter(12)} ${quarter(6)}")
}
```

```text
7 is odd
nothing positive
64 8
:some 3 :none
```

## First-class functions and lambdas

Lambdas compile to bare metal functions. `some fn int -> int`, shorthand for `some fn(int) -> int`, selects the physical representation for each argument: a direct call for a named function or a lambda, an indirect call through a `*fn`, a dynamic call through a `dyn[fn]`.

```k1
fn inc(x: int): int { x + 1 }

fn main() {
  let inc-ptr: *fn(int) -> int = inc.&
  let step = 5
  let add-step: dyn[fn(int) -> int] = (fn[step](x: int) x + step).to-dyn()
  println(apply-twice(1, inc))
  println(apply-twice(1, inc-ptr))
  println(apply-twice(1, add-step))
  println(apply-twice(1, fn x. x * 10))

  let total = 0
  let add = fn[step, total.&](x: int) { total.* = total.* + x + step }
  add(1)
  add(2)
  println(total)
}

fn apply-twice(x: int, f: some fn int -> int): int {
  f(f(x))
}
```

```text
3
3
11
100
13
```

## Abilities

k1 uses `ability` to model define and implement interfaces, and do all kinds of ad-hoc polymorphism, that leaves no runtime trace. `impl ours/greet for theirs/cat` is an independent 3rd party, neither the cat nor greet, teaching cat how to greet. This is an immensely powerful decoupling, contrast with inheritance-based implementation. An ability can have both ability-side and impl-side type parameters. `to[t]` is an ability-side parameter; a type can be `to` for as many `t`s as it can convert to; but `iterable[t]` is an impl-side parameter: a type can implement iterator _once_ and provide only one type that it iterates over. (similar to Rust traits' associated types)

```k1
ns ours {
  ability greet {
    fn greet(self: *self): string
  }
}

ns theirs {
  type cat = { name: string }
  type robot = { id: int }
}

impl ours/greet for theirs/cat {
  fn greet(self: *self): string { "meow, I am ${self.name}" }
}

impl ours/greet for theirs/robot {
  fn greet(self: *self): string { "unit ${self.id} online" }
}

fn introduce[t: ours/greet](x: *t) {
  println(x.greet())
}

fn main() {
  let c = theirs/cat.{ name = "Ada" }
  introduce(c.&)
  let r = theirs/robot.{ id = 7 }
  introduce(r.&)
}
```

```text
$ content/showcase/ir.sh examples/basics_abilities.k1 'greet'
define internal [2 x i64] @impl_43greet.greet_for_t8156(ptr %self)
define internal [2 x i64] @impl_43greet.greet_for_t8160(ptr %self)
define internal void @main() {
  ...
  %2 = call [2 x i64] @impl_43greet.greet_for_t8156(ptr %1)
  ...
  %3 = call [2 x i64] @impl_43greet.greet_for_t8160(ptr %0)
  ...
  ret void
}
```

```text
meow, I am Ada
unit 7 online
```

## Generics

Types and functions can accept types as parameters; this is your standard parametric polymorphism ('inputs over shapes') that allows writing code we call 'generic'. This happens entirely at compile-time, and understands abilities as type-bounds.

```k1
type pair[a, b] = { a, b }

ns for pair {
  fn swap[a, b](self: pair[a, b]): pair[b, a] {
    .{ a = self.b, b = self.a }
  }
}

fn largest[t: comparable](xs: list[t]): t {
  let best = xs.[0]
  for x in xs { if x.compare(best) is :greater { best = x } }
  best
}

fn main() {
  let p: pair[int, string] = .{ a = 1, b = "one" }
  println(p.swap())
  println(largest([3, 9, 2]))
  println(largest(["pear", "apple"]))
}
```

```text
.{ a = one, b = 1 }
9
pear
```

## References

These are really just pointers. They are nonnull, but it's not that hard to manufacture a null one; I'm not too interested in stopping you. The untyped one, your `void*`, is called `ptr`.

```k1
fn bump(n: *int) {
  n.* = n.* + 1
}

type point = { x: int, y: int }

fn main() {
  let x = 1
  bump(x.&)
  bump(x.&)
  println(x)

  let boxed: *int = mem/new(41)
  bump(boxed)
  println(boxed.*)

  let raw: ptr = boxed.as[ptr]
  let back: *int = raw.ref[int]
  println(back.*)

  let p = point.{ x = 1, y = 2 }
  let r = p.&
  bump(r.y.&)
  println("${r.x + r.y} $p")
}
```

```text
3
42
42
4 .{ x = 1, y = 3 }
```

## Early execution

`k1` can run any code at compile-time. It's really useful, really powerful, really fast, and really fun. Many more examples in Beyond

```k1
fn fib(n: int): int {
  if n < 2 n else fib(n - 1) + fib(n - 2)
}

let ANSWER: int = fib(30) * fib(20) + 1

fn main() {
  println(ANSWER)
  let table: list[int] = #static {
    let xs: list[int] = []
    for i in 0.until(10) { xs.push(fib(i)) }
    println("built ${xs.len()} entries while compiling")
    xs
  }
  println(table)
}
```

```text
$ content/showcase/ir.sh examples/basics_early.k1 'format-uint|memcpy|10 x i64]' fib
[basics_early.k1:12 info] built 10 entries while compiling
$ grep -c 'fib' .k1-out/basics_early.ll
0
define internal void @main() {
  ...
  call void @core.format-uint(i64 1, i64 5628750601, i64 10)
  ...
  call void @llvm.memcpy(ptr %0, ptr @k1.static.2552, i64 24, i1 false)
  ...
  ret void
}
@k1.static.2552.elems = constant [10 x i64] [i64 0, i64 1, i64 1, i64 2, i64 3, i64 5, i64 8, i64 13, i64 21, i64 34], align 8
```

```text
5628750601
[0,1,1,2,3,5,8,13,21,34]
```
