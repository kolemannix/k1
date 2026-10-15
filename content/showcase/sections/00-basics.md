# Language basics

Twelve small programs. Each one runs as written.

## Hello, world

A program is a file with a `main`.

```k1
fn main() {
  println("Hello, world")
}
```

```text
Hello, world
```

## Functions and variables

Arguments can be passed by name.

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

## Lists and spans

A list owns its elements and grows. A span is a view of memory someone else owns, and a string is a span of chars, with UTF-8 as library code you call when you need it.

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

A `ns for` block gives a type its methods.

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
}
```

```text
.{ x = 11, y = 12 }
```

## Either types

`either` is the sum type. A variant can carry a payload or just a tag, and `opt` and `result` are two of them.

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

`?t` is sugar for `opt[t]`. `?` is the else operator, and `.try` gives you the value or returns the error, on anything that implements `try`.

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

`if` is an expression, `loop` breaks with a value, and `require` binds a pattern for the rest of the block or leaves.

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

fn main() {
  println(describe-int(:some 7))
  println(describe-int(:none))
  println(first-square-over(50))
}
```

```text
7 is odd
nothing positive
64
```

## Lambdas

A lambda captures only what you list in brackets. The physical form of the function depends on what you need: inlined, a direct call, an indirect call, or a dynamic closure.

```k1
fn apply-twice(x: int, f: some fn int -> int): int {
  f(f(x))
}

fn inc(x: int): int { x + 1 }

fn main() {
  println(apply-twice(1, inc))
  println(apply-twice(1, fn x. x * 10))
  let inc-ptr: *fn(int) -> int = inc.&
  println(apply-twice(1, inc-ptr))

  let step = 5
  let total = 0
  let add = fn[step, total.&](x: int) { total.* = total.* + x + step }
  add(1)
  add(2)
  println(total)
}
```

```text
3
100
3
13
```

## Abilities

An ability is a set of functions a type can implement, sometimes called type classes or ad hoc polymorphism. It is the way to express a protocol without coupling your types to it. All of this compiles to plain function calls.

```k1
ability greet {
  fn greet(self: *self): string
}

type cat = { name: string }
type robot = { id: int }

impl greet for cat {
  fn greet(self: *cat): string { "meow, I am ${self.name}" }
}

impl greet for robot {
  fn greet(self: *robot): string { "unit ${self.id} online" }
}

fn introduce[t: greet](x: *t) {
  println(x.greet())
}

fn main() {
  let c = cat.{ name = "Ada" }
  introduce(c.&)
  let r = robot.{ id = 7 }
  introduce(r.&)
}
```

```text
$ content/showcase/ir.sh examples/basics_abilities.k1 'greet'
define internal [2 x i64] @impl_41greet.greet_for_t7270(ptr %self)
define internal [2 x i64] @impl_41greet.greet_for_t7274(ptr %self)
define internal void @main() {
  ...
  %2 = call [2 x i64] @impl_41greet.greet_for_t7270(ptr %1)
  ...
  %3 = call [2 x i64] @impl_41greet.greet_for_t7274(ptr %0)
  ...
  ret void
}
```

```text
meow, I am Ada
unit 7 online
```

## Generics

Type parameters go in square brackets, on types and on functions.

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

References are taken with `.&` and followed with `.*`. Every `*t` points at something, so there is nothing to null-check.

```k1
fn bump(n: *int) {
  n.* = n.* + 1
}

fn main() {
  let x = 1
  bump(x.&)
  bump(x.&)
  println(x)

  let boxed: *int = mem/new(41)
  bump(boxed)
  println(boxed.*)
}
```

```text
3
42
```

## Early execution

Globals are computed while compiling, `#static` runs any expression there, and only the results reach the binary.

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
