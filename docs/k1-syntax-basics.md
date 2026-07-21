# K1 Syntax And Basics

This guide is a practical map for reading and writing K1 in this repo. It is
based on examples from `test_src`, which is currently the best executable
language reference.

K1 is still moving. Prefer the patterns in newer tests when examples conflict.
Use kebab-case for new K1 names: namespaces, functions, types, fields,
variables, abilities, and methods.

## File Shape

A typical test file starts with a namespace, then declarations, then a `test`
function:

```rust
ns example-test

type point = { x: int, y: int }

fn origin(): point {
  .{ x = 0, y = 0 }
}

fn test() {
  let p = origin()
  assert-equals(p.x, 0)
}
```

A whole-file namespace is declared on its own line:

```rust
ns format-test
```

or with block form:

```rust
ns lambdas {
  fn test() {}
}
```

Call across namespaces with `/`:

```rust
format-test/test()
core/types/type-id[int]
```

See `test_src/suite1/main.k1`, `test_src/suite1/format.k1`, and
`test_src/suite1/lambdas.k1`.

## Naming

K1 code uses kebab-case for everything: namespaces, functions, types, fields,
and enum variants. Constants are SCREAMING_SNAKE.

```rust
ns allocator-test

type http-request = { status-code: int }

fn parse-request(): http-request {
  .{ status-code = 200 }
}
```

Prefer short, concrete names in tests. A file named `range_test.k1` may still use
`ns range-test` in new code.

## Functions

Functions use `fn`, typed parameters, and an optional return type after `:`.
The final expression is the return value when there is no explicit `return`.

```rust
fn add-one(x: int): int {
  x + 1
}

fn log-message(message: string) {
  println(message)
}
```

Use `return value` for early returns; the payload is a full expression, and a
bare `return` returns unit:

```rust
fn choose(flag: bool): int {
  if flag {
    return 1
  }
  0
}
```

Generic parameters go in square brackets:

```rust
fn unwrap-box[T](b: box[box[T]]): T {
  b.value.value
}
```

Use `_` at call sites when a type argument should be inferred:

```rust
takes-poly-au-pair[int, _](value)
```

See `test_src/suite1/control_flow.k1` and
`test_src/suite1/generic_struct.k1`.

## Blocks, Statements, And Values

Blocks are expressions. The last expression is the block value:

```rust
let answer = {
  let base = 40
  base + 2
}
```

A line break ends a statement. `;` also ends one, for putting several
statements on one line (`let a = 1; let b = 2`); it is never required at end
of line.

An expression continues across a line break when the next line starts with a
token that could not begin a new statement: a binary operator (`and`, `or`,
`||`, `==`, `+`, `?`, comparisons), `is`, `else`, or `.` followed by a name
(leading-dot method chains):

```rust
let result = [1, 2, 3]
  || map(fn x. x + 1)
  || filter(fn x. x % 2 == 0)

let n = "hello"
  .len()
  .as[int]
```

Tokens that can begin a statement end the previous one instead, even where a
binary reading exists: `-` (negative literal), `&` (address-of), `(`, `[`,
and string literals. To continue with one of those, put the operator at the
end of the previous line (`let x = a -` ... `b`). Call parens, type-arg
brackets, and juxtaposed string concatenation bind only on the same line.
Payloads never cross a line break: `return`, `break`, or a `:variant` at end
of line is bare.

A block with no meaningful value yields `.{}`, the empty/unit value (the unit
*type* is spelled `{}`):

```rust
let result: {} = if true {
  println("done")
}
assert(result == .{})
```

K1 code commonly ends test helpers with `.{}` when the intent is "return unit".

## Variables And Mutation

Use `let` for local bindings and `=` for assignment:

```rust
let count = 0
count = count + 1
```

`=` assigns to any place, not just variables: struct fields, array elements,
and dereferences are all valid destinations:

```rust
let item = .{ value = 41 }
item.value = 42
assert-equals(item.value, 42)
```

A bare variable on the left always means the variable itself: `r = v` rebinds
`r`. To store through a reference, dereference it explicitly on the left-hand
side with `.*`:

```rust
let counter = core/mem/new(0)
counter.* = counter.* + 1
assert-equals(counter.*, 1)
```

Use `.*` to read through a reference, and the postfix `.&` operator to take the
address of a place when you need a reference to it.

A statement that discards a non-unit expression result gets a warning; bind to
`_` to discard explicitly:

```rust
let _ = posix/madvise(base-ptr, len, advice = posix/MADV_SEQUENTIAL)
```

See `test_src/suite1/assign.k1`, `test_src/suite1/place_test.k1`,
`test_src/suite1/pointer.k1`, and `test_src/suite1/struct.k1`.

## Core Types

Common scalar types include:

```rust
bool
char
string
int
i8
i32
i64
u8
u32
u64
size
f32
f64
```

Integer literals can be annotated inline:

```rust
let byte = 10: u8
let signed = -3i8
```

Type assertions and casts use `:` and `.as[...]`:

```rust
let three = 3: i32
let raw = ptr-value.as[size]
```

## Structs

`type name = ...` creates a nominal type with a unique identity. Currently,
nominal types can be created for structs, enums, and opaque types:

```rust
type point = { x: int, y: int }

let p: point = .{ x = 1, y = 2 }
assert-equals(p.x, 1)
```

Struct *values* are marked by a leading `.{` and use `=` (`.{ x = 1 }`), while
struct *types* use bare braces and `:` (`{ x: int }`): `:` always means "type"
and `=` always means "value". The `.{` marker is what distinguishes a struct
literal from a block — a bare `{` in expression position is always a block, so
`{ cap }` is a block yielding `cap` and `.{ cap }` is the shorthand struct.
Fields use the same `name = value` shape as named call arguments.

`type alias name = ...` creates a plain alias for the type on the right-hand
side. This is how to name an existing scalar or structural type without creating
a new nominal identity:

```rust
type alias byte-count = u32
type alias point-like = { x: int, y: int }
```

Anonymous struct types are common:

```rust
fn area(rect: { width: int, height: int }): int {
  rect.width * rect.height
}
```

Field shorthand is supported when a local has the same name:

```rust
let width = 50
let rect = .{ width, height = 20 }
```

Use `.with(...)` to copy a struct with selected fields changed:

```rust
let next = current.with(.{ count = current.count + 1 })
```

Attach methods to a type with `ns for`:

```rust
type counter = { value: int }

ns for counter {
  fn inc(self: *mut counter) {
    self.value = self.value + 1
  }
}
```

See `test_src/suite1/struct.k1` and
`test_src/suite1/struct_composition.k1`.

## Field Access

`a.foo` always means "the field's value", whether `a` is a struct value or a
reference to one; K1 dereferences through the base as needed. When the access
chain denotes a place (it roots in a variable or a dereference), it can be
assigned with `=` and its address taken with `.&`:

```rust
let p = .{ x = 1, y = 2 }
let x: int = p.x
p.x = 2

let p-ref = p.&
let x-value: int = p-ref.x
let x-ref: *mut int = p-ref.x.&
p-ref.y = 20
```

The same applies to array elements: `arr.get(i)` yields the element value,
`arr.get(i) = value` stores into the element, and `arr.get(i).&` takes its
address. Sum patterns use trailing `*` to bind references to payload data; see
the next section.

See `test_src/suite1/assign.k1`, `test_src/suite1/place_test.k1`, and
`test_src/suite1/struct.k1`.

## Enums And Sums

K1 uses `either` for payload-less enum-like types and payload-carrying sum
types.

Payload-less enums expose their scalar value through `.value` and through the
`enum-value` ability method:

```rust
type color = either(u8) { red, green, blue }

let c: color = :red
assert(c is :red)
assert-equals(color:green.value, 1)
assert-equals(c.enum-value(), 0)
```

Enum tags may be explicit:

```rust
type status = either { ok = 10, missing = 20 }
assert-equals(status:missing.value, 20)
```

Payload-carrying sums expose their active tag through `.tag`, and generated
variant helpers such as `.as-ok()` return an optional payload:

```rust
type parse-result[T] = either { ok(T), err(string) }

let result: parse-result[int] = :ok(42)
assert-equals(result.tag, 0)

if result is :ok(value) {
  assert-equals(value, 42)
} else {
  crash("expected ok")
}

assert-equals(result.as-ok().!, 42)
```

Payload parentheses are optional in both constructors and patterns — the
'quiet' form. The payload must start on the same line, with an identifier,
literal, list literal, nonempty struct literal (`.{ x = 1 }`), or another
`:variant`; anything else (an operator, a lambda, an empty `.{}`, a line
break) needs the parenthesized form, so a trailing variant never captures a
following block or a bare unit:

```rust
let result: parse-result[int] = :ok 42

if result is :ok value {
  assert-equals(value, 42)
}

require result is :ok v else crash("expected ok")
```

A quiet payload binds one postfix expression: `:ok f(x).y` means
`:ok(f(x).y)`, while binary operators apply to the whole variant:
`:ok 1 == x` means `(:ok 1) == x`.

A payload of type `empty` can be elided entirely: constructing `:ok` for a
`result[empty, e]` means `:ok(.{})`, and the pattern `:ok` matches it (and
counts as exhaustive coverage). Spelling the payload out (`:ok(.{})`,
`:ok _`) remains valid. Variants with any other payload type still require
it.

When matching on a reference to a sum, plain patterns bind payload values; add
trailing `*` to the pattern to bind a reference to the variant's data instead,
then store through it with `.* =`:

```rust
let result: parse-result[int] = :ok 42
let result-ref = result.&

if result-ref is :ok(ok-ref)* {
  ok-ref.* = 100
} else {
  crash("expected ok")
}

assert-equals(result-ref.as-ok().!, 100)
```

This works in every pattern position — `if`/`while`/`require` conditions and
match arms:

```rust
result-ref is {
  :ok(value-ref)* -> { value-ref.* = value-ref.* + 1 },
  :err(message-ref)* -> { crash(message-ref.*) }
}
```

`is` with a brace block of arms is the match expression: exhaustive, and
usable anywhere an expression is. `x is <pattern>` (a bool) is just its
two-arm degenerate case.

```rust
fn show-result(result: parse-result[int]): string {
  result is {
    :ok(value) -> { "ok {value}" },
    :err(message) -> { "err {message}" }
  }
}
```

Generated helpers such as `.as-ok()`, `.is-some()`, `.enum-name()`,
`.sum-name()`, `.enum-value()`, and `.tag` show up throughout the tests.

See `test_src/suite1/enum_basic.k1`, `test_src/suite1/sum_basic.k1`,
`test_src/suite1/match_references.k1`, and `test_src/suite1/matching_if.k1`.

## Option And Result

Optional values use `?T`, with `:some(value)` and `:none`:

```rust
let maybe-name: ?string = :some("k1")

if maybe-name is :some(name) {
  assert-equals(name, "k1")
}
```

Examples may use either direct variants such as `:some(value)` or helper
constructors such as `some(value)`. Use whichever is clearer in the surrounding
code.

The postfix `.!` unwraps an optional-like value when the code expects it to be
present. It is good style when the invariant is clear:

```rust
assert-equals(maybe-name.!, "k1")
```

This syntax is user-accessible through abilities rather than hardcoded as a
one-off builtin. That is true of most special-looking K1 syntax: prefer looking
for the relevant ability before assuming a parser or compiler intrinsic.

The `?` operator provides a fallback for optionals:

```rust
let value = maybe-int ? 42
```

Results are sums with `:ok` and `:err`. The `.try` form propagates errors
through the active `try` ability:

```rust
fn run(): result[int, string] {
  let value = can-fail().try
  :ok(value + 1)
}
```

See `test_src/suite1/optionals.k1` and `test_src/suite1/try_test.k1`.

## Conditionals And Pattern Matching

`if` is an expression:

```rust
let n = if flag 1 else 2
```

Use `is` for pattern checks and bindings:

```rust
if value is :some(x) and x > 0 {
  assert(x > 0)
} else {
  assert(false)
}
```

Struct patterns work in `if`, `require`, and match arms. Arms support `or`
patterns, `if` guards, and a `_` default:

```rust
point is {
  .{ x = 0, y } -> y,
  .{ x, y } if x == y -> x,
  _ -> 0
}
```

Matching `if` chains bind names only when `is` expressions live at the top level
of an `and` chain. An `or` breaks that property.

`require` checks a condition and runs an error block otherwise:

```rust
require value is :some(x) else {
  crash("missing value")
}
assert(x > 0)
```

See `test_src/suite1/matching_if.k1`, `test_src/suite1/match_fails.k1`,
`test_src/suite1/match_references.k1`, and
`test_src/suite1/require_test.k1`.

## Loops

`while` loops are statement-like and usually yield `.{}`:

```rust
while i < 10 {
  i = i + 1
}
```

`loop` can yield a value through `break value`; a bare `break` exits with unit:

```rust
let found: int = loop {
  break 42
}
```

`for` loops iterate over iterable values. The loop body can use `it` and
`it-index`, or bind a name with `in`:

```rust
for values {
  println("{it-index}: {it}")
}
```

Numeric loops use ranges. `a.until(b)` (from the `rangeable` ability) builds the
half-open `range[t]` value `[a, b)`, which iterates by an implied step of one.
It works on any type with `add`, `scalar-cmp`, and `one` impls — all the
integer types and floats:

```rust
for i in 0.until(10) {
  println("{i}")
}
```

`defer` runs when leaving the current scope, including early returns:

```rust
defer cleanup()
```

See `test_src/suite1/while.k1`, `test_src/suite1/control_flow.k1`,
`test_src/suite1/for_yield.k1`, and `test_src/suite1/defer_test.k1`.

## Arrays, Buffers, Lists, And Spans

Array, buffer, list, and span types are written with square brackets:

```rust
let fixed: array[bool, 3] = [false, true, true]
let dynamic: list[bool] = [false, true, true]
let view: span[bool] = dynamic.as-span()
```

Arrays have a compile-time length in their type. Buffers, lists, and spans are
used heavily in the core library and dogfood programs.

Common helpers include `.len()`, `.get(index)`, `.set(index, value)`,
`.as-span()`, `buffer/wrap-array(...)`, and `list/filled-in(...)`.

Collection API naming follows a doctrine:

- `wrap-*` constructors are zero-copy views over existing memory (the dangerous
  ones, so they are explicitly named). `from-*` constructors make an owned copy.
- `as-*` accessors return views; `to-*` methods return owned copies.
- A bare operation allocates in the ambient `:current` alloc-mode; the `-in`
  variant takes an explicit `context alloc-mode` (e.g. `cloned`/`cloned-in`,
  `push`/`push-in`, `reserve`/`reserve-in`).
- Mutators take `*mut self` and reuse the verb (`sort`, `reverse`); functional
  variants get `-ed` (`sorted`, `reversed`).

See `test_src/suite1/array_test.k1`, `test_src/suite1/list_test.k1`,
`test_src/suite1/range_test.k1`, and `test_src/suite1/buffer_test.k1`.

## Lambdas And Function Values

Lambda forms include typed parameters:

```rust
let add-one = fn(x: int) x + 1
```

inferred parameters:

```rust
let add-one = fn x. x + 1
```

and nullary thunks:

```rust
let thunk = fn. { println("later") }
```

Function parameters can accept static function values, closure-like values, or
dynamic function objects depending on the type. Prefer `some fn ...` for
beginner-facing examples and ordinary function parameters; reach for
`dyn[fn ...]` when you specifically need a dynamic function object:

```rust
fn apply(i: int, f: some fn int -> int): int {
  f(i)
}

fn run-later(thunk: dyn[fn() -> {}]) {
  thunk()
}
```

The pipe operator `||` passes a value through functions:

```rust
let result = [1,2,3]
  || map(fn x. x + 1)
  || filter(fn x. x % 2 == 0)
```

See `test_src/suite1/lambdas.k1` and `test_src/suite1/pipe.k1`.

## Abilities And Impls

Abilities define behavior that types can implement:

```rust
ability printable-id {
  fn printable-id(self): string
}

type user = { id: int }

impl printable-id for user {
  fn printable-id(self): string {
    stringf("user-{id}", self)
  }
}
```

Ability bounds go on type parameters:

```rust
fn show[T: print](value: T): string {
  stringf("{}", value)
}
```

Qualified ability calls use `/`:

```rust
print/print-to(value, writer)
```

When multiple impls could apply, tests also use explicit ability/type selection:

```rust
some-ability@(some-type)/function-name()
```

See `test_src/suite1/ability.k1`,
`test_src/suite1/ability_constraint.k1`,
`test_src/suite1/ability_generic.k1`, and
`test_src/suite1/ability_complex.k1`.

## Imports

Use `use` to bring functions, constants, types, or namespaces into scope:

```rust
use core/types/type-id
use core/libc/files/SEEK_END as seek-end

fn test() {
  use core/string as str
  let hello: str = "hello"
}
```

Prefer local `use` statements when the alias is only needed in one function.

See `test_src/suite1/use_test.k1`.

## Compile-Time Execution

`#static` evaluates an expression at compile time:

```rust
let answer: 42 = #static {
  40 + 2
}
```

Static values can carry literal types:

```rust
let greeting: "hello" = #static "hello"
```

`#if` conditionally includes code at compile time:

```rust
#if false {
  crash("not compiled")
}
```

See `test_src/suite1/static_run.k1`,
`test_src/suite1/static_parameter.k1`, and
`test_src/suite1/test_comptime.k1`.

## Formatting And Strings

String interpolation is supported inside string literals and formatting helpers.
A hole is `$ident` for a bare identifier, or `${expr}` for any expression:

```rust
let name = "k1"
assert-equals("hello $name", "hello k1")
assert-equals("len ${name.len()}", "len 2")
```

`$ident` uses normal identifier rules, so it is greedy through kebab-case:
`"$n-th"` reads the identifier `n-th`; write `"${n}-th"` to stop at `n`. A `$`
followed by anything that cannot start an identifier is literal (`"$5.99"`), and
`\$` escapes a `$` that would otherwise open a hole. Braces are ordinary
characters in strings and need no escaping.

Use `writef` to write formatted text to any value that implements the `writer`
ability. The first argument is the writer, the second is the format string, and
the optional third argument supplies format values:

```rust
let w = string-builder/new()
writef(w, "hello ${}", 42)
writef(w, " $name", .{ name = "k1" })
```

Use `writelnf` the same way when you want a trailing newline:

```rust
writelnf(w, "status: ${}", 200)
```

Use `stringf` to build and return a formatted string. It takes the format string
first and an optional values argument second:

```rust
let s = stringf("hello $name")
let dated = stringf("$yyyy-${mm}-$dd", .{ yyyy = 2026, mm = 6, dd = 25 })
```

Bare `${}` placeholders consume the value argument directly. Named placeholders
such as `$name` read fields from the values struct. Interpolated expressions can
also refer to locals in scope:

```rust
let place = "Budapest"
assert-equals(stringf("hello $place"), "hello Budapest")
```

`writef`, `writelnf`, and `stringf` are special syntax hooks checked by the
typer, but they still rely on normal K1 abilities for the actual output. In
particular, `writef` and `writelnf` require a `writer`.

Raw strings use backticks:

```rust
let raw = `Hello,

  "world"`
```

See `test_src/suite1/format.k1` and
`test_src/suite1/string_interp.k1`.

## Tests

Language regression tests live under `test_src`.

Use this shape for a new positive test:

```rust
ns feature-test

fn test() {
  assert-equals(1 + 1, 2)
}
```

Then add it to `test_src/suite1/main.k1` if it belongs in suite1.

Use `test-compile(...)` when a snippet should fail to compile:

```rust
assert(test-compile(bad-expression()).is-some())
```

Use `assert`, `assert-equals`, and `core/assert-not-equals` for expectations.

## Open Questions For Idiom

These are places where tests show behavior, but the preferred style or long-term
design should be confirmed:

- Should `#static` be introduced early as a core language feature, or kept in an
  advanced guide?
