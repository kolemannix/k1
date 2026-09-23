# Types are values

In K1 a type is a value at compile time: `types/id[t]` is a plain struct, its
schema is a sum you pattern match, and `types/make-struct` / `types/make-either`
go the other way. `#type <expr>` runs any expression in the compile-time VM
and uses the type it returns. There is no separate template language; the code
that builds types is the same K1 that runs at runtime.

## A struct from a schema string

The parser is ordinary K1. It lives in `ns pre`, the part of a module compiled
before the rest so type definitions can call it.

```k1
  fn table(schema: string): types/type-id {
    let fields: list[{ name: string, type: types/type-id }] = []
    for column in schema.split-by-char(',') {
      let parts = column.split-by-char(':')
      fields.push(.{ name = trim(parts.[0]), type = column-type(trim(parts.[1])) })
    }
    types/make-struct(:struct, fields.as-span())
  }
}

type(alias) user = #type pre/table("id: u64, name: string, score: f64, active: bool")

fn main(): i32 {
  let u: user = .{ id = 7, name = "ada", score = 99.5, active = true }
  u.score = u.score + 0.5
  println("${u.id} ${u.name} ${u.score} ${u.active}")
  println("size ${types/size[user]} align ${types/align[user]}")
  println(types/name[user])
  assert(types/id[user] == types/id[{ id: u64, name: string, score: f64, active: bool }])
  0
}
```

```text
7 ada 100.0 true
size 33 align 8
{ id: u64, name: string, score: f64, active: bool }
```

`user` is not a lookalike: the final `assert` shows it is the same type id as
the struct written by hand, with the same layout, field access, literals and
assignment. Swapping the schema for a `CREATE TABLE` statement is a bigger
parser in the same `pre` block, nothing else changes. Zig gets here with
`@Type` on a `std.builtin.Type`, C++26 with `std::meta::define_aggregate`; a
Rust proc macro can emit the struct but only as tokens, without ever seeing a
type or a size.

## Variants from a table

`make-either` takes a tag type and a list of `{ name, payload, tag }`. Here
the tags come from a table of HTTP status codes.

```k1
  fn status-enum(): types/type-id {
    let variants: list[{ name: string, payload: ?types/type-id, tag: ?types/int-value }] = []
    for row in STATUS_TABLE {
      variants.push(.{ name = row.name, payload = :none, tag = :some(types/int-value:u16(row.code)) })
    }
    types/make-either(:some(types/id[u16]), variants.as-span())
  }
}

type(alias) status = #type pre/status-enum()

type(alias) event = #type types/make-either-default([
  types/variant-data[u16]("connected"),
  types/variant-data[string]("data"),
  types/variant-empty("closed"),
])

fn classify(s: status): string {
  if s is {
    :ok or :created -> "success",
    :not-found -> "client error",
    :teapot -> "short and stout",
    :internal-error -> "server error",
  }
}

fn main(): i32 {
  let s: status = :teapot
  println("${s.enum-name()} = ${s.value} -> ${classify(s)}")
  println("size ${types/size[status]} tag ${status:internal-error.value}")

  let events: list[event] = [:connected 8080, :data "hello", :data "world", :closed]
  let counts: array[u32, 3] = .0
  for e in events {
    let kind: event.tag-enum = if e is {
      :connected _ -> :connected,
      :data _ -> :data,
      :closed -> :closed,
    }
    counts.[kind.value.widen[size]] = counts.[kind.value.widen[size]] + 1
  }
  println("counts ${counts}")
  0
}
```

```text
teapot = 418 -> short and stout
size 2 tag 500
counts [1,2,1]
```

The synthesized enum is two bytes because its tag type is `u16`, `.value` is
the code from the table, and `.enum-name()` is the name. `event.tag-enum` is
the payload-less enum of a sum's tags, derived from any sum type. Matching is
exhaustive on a synthesized type exactly as on a written one; drop the table
to three rows and forget one:

```k1
fn classify(s: status): string {
  if s is {
    :ok -> "success",
    :not-found -> "client error",
  }
}
```

```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/types_derive_variants_wrong.k1:20:5: error
├─────
│   type(alias) status = #type pre/status-enum()
│   
│   fn classify(s: status): string {
│ ->  if s is {
│        ^
│       :ok -> "success",
│       :not-found -> "client error",
├─────
│  Non-exhaustive match; for example, this pattern is not covered:
- :teapot
└────────────────────────────────────────╴
Module types_derive_variants_wrong failed typechecking with 1 errors
```

## Struct transforms

`_struct_remove` and `_struct_combine` are built in; anything else is a
function from `type-id` to `type-id`. `patch-of` wraps every field in `opt`.

```k1
type user = { id: u64, name: string, password: string, email: string }
type user-view = _struct_remove[user, { password: string }]
type audited = { created: u64, updated: u64 }
type user-row = _struct_combine[user, audited]

ns pre {
  fn patch-of(t: types/type-id): types/type-id {
    require t.schema() is :struct .{ fields } else crash("patch-of needs a struct")
    let out: list[{ name: string, type: types/type-id }] = []
    for f in fields {
      out.push(.{ name = f.name, type = types/make-instance(types/id[opt], [f.type-id]) })
    }
    types/make-struct(:struct, out.as-span())
  }
}

type(alias) user-patch = #type pre/patch-of(types/id[user])

fn view(u: user): user-view {
  .{ id = u.id, name = u.name, email = u.email }
}

fn apply(u: *user, p: user-patch) {
  if p.name is :some n { u.name = n }
  if p.email is :some e { u.email = e }
  if p.password is :some pw { u.password = pw }
}
```

```text
1 ada ada@k1.dev
ada created 100
ada lovelace
user 56 view 40 row 72 patch 88
user-view
{ id: opt[u64], name: opt[string], password: opt[string], email: opt[string] }
```

These are four distinct types with four layouts: the view drops one 16-byte
string, the row appends two `u64`s, and every `opt` in the patch pays for its
tag. `types/make-instance(types/id[opt], [f.type-id])` is `?t` built from a
type id, the same way `list[int]` would be. One limit today: a type alias
cannot take type parameters, so `patch-of` is applied per type rather than
written once as `patch[t]`.

## Static parameters and literal types

A generic parameter can be a value. `n: static size` is a size known at
compile time, and it flows into array lengths and layout.

```k1
fn add-static[i: static int](x: int): int {
  core/meta/static-type-to-value[_, i]() + x
}

type matrix[n: static size] = { rows: array[array[i64, n], n] }

ns for matrix {
  fn dim[n](_self: *matrix[n]): size { core/meta/static-type-to-value[_, n]() }

  fn identity[n: static size](): matrix[n] {
    let m: matrix[n] = .0
    for i in 0.until(m.dim()) { m.rows.[i].[i] = 1 }
    m
  }

  fn mul[n](a: *matrix[n], b: *matrix[n]): matrix[n] {
    let out: matrix[n] = .0
    let n = a.dim()
    for i in 0.until(n) {
      for j in 0.until(n) {
        let acc = 0
        for k in 0.until(n) { acc = acc + a.rows.[i].[k] * b.rows.[k].[j] }
        out.rows.[i].[j] = acc
      }
    }
    out
  }
}

fn main(): i32 {
  println("${add-static[3](4)} ${add-static[40](2)}")
  let five: static int = 5
  println("${add-static[type-of(five)](10)}")

  let a: matrix[2] = .{ rows = [[1, 2], [3, 4]] }
  let sq = a.&.mul(a.&)
  println("${sq.rows} dim ${sq.dim()} size ${types/size[matrix[2]]}")
  let i3 = matrix/identity[3]()
  println("${i3.rows} size ${types/size[matrix[3]]}")
  let mismatch: ?string = test-compile(a.&.mul(i3.&))
  println(mismatch.!)

  let total = 0
  #for pre/squares(4) {
    total = total + it
    println("unrolled step $it, total $total")
  }

  let c: 'c' = 'c'
  let pi: 3.14 = 3.14
  let tag: "showcase" = "showcase"
  println("${c.from-static()} ${pi.from-static()} ${tag.from-static()}")
  let wrong-literal: ?string = test-compile('d': 'c')
  println(wrong-literal.!)
  0
}
```

```text
7 42
15
[[7,10],[15,22]] dim 2 size 32
[[1,0,0],[0,1,0],[0,0,1]] size 72
Error in parameter 'b' in call to 'mul''
Expected matrix[static[i64, 2]] but got matrix[static[i64, 3]]: Param 'n' is incorrect: Different static values of same type family: 2 vs 3
unrolled step 1, total 1
unrolled step 4, total 5
unrolled step 9, total 14
unrolled step 16, total 30
c 3.14 showcase
Expression did not conform to hint: Static lift resulted in wrong value: Different static values of same type family: c vs d
```

`matrix[2]` and `matrix[3]` are different types with different sizes, and
multiplying one by the other is the compile error printed above
(`test-compile` returns a compile error as a string, so a program can show its
own diagnostics). A static argument is written as a literal (`add-static[3]`)
or as the type of a `static int` variable. `#for` evaluates its list in the VM
and emits the body once per element with `it` baked in: the emitted IR has
four `println` calls for the loop and no `squares` function at all. Literal types `'c'`, `3.14`, `"showcase"` carry the value
in the type; the mismatch `'d': 'c'` is the second error above. This is Zig's
`comptime n` and C++'s non-type template parameter, but the same mechanism
also types the result of every `#static` block.

## Predicate bounds

A bound can be a function. `pred pre/is-pod` calls `is-pod[t]()` at each
instantiation; the predicate is a recursive walk over the schema that rejects
anything containing a reference.

```k1
ns pre {
  fn is-int[t](): bool { types/schema[t] is :int _ }

  fn is-pod-id(t: types/type-id): bool {
    if t.schema() is {
      :int _ or :float _ or :bool or :char or :enum _ -> true,
      :struct .{ fields } or :union .{ fields } -> fields.find(fn f. not is-pod-id(f.type-id)) is :none,
      :array .{ element-type-id } -> is-pod-id(element-type-id),
      :either .{ variants } -> variants.find(fn v. v.payload is :some p and not is-pod-id(p.type-id)) is :none,
      _ -> false
    }
  }

  fn is-pod[t](): bool { is-pod-id(types/id[t]) }
}

type int-wrapper[t: pred pre/is-int] = { value: t }

fn clone-pod[t: pred pre/is-pod](src: *t): *t {
  let dst: *t = mem/tmp().alloc-layout(types/size[t], types/align[t]).ref()
  mem/copy(dst = dst.as[ptr], src = src.as[ptr], count = types/size[t])
  dst
}

fn same-bytes[t: pred pre/is-pod](a: *t, b: *t): bool {
  mem/equals(a.as[ptr], b.as[ptr], types/size[t])
}

type vec3 = { x: f32, y: f32, z: f32 }
type shape = either { sphere({ center: vec3, radius: f32 }), box({ min: vec3, max: vec3 }) }

fn main(): i32 {
  let w: int-wrapper[u8] = .{ value = 200 }
  println("${w.value}")

  let s: shape = :sphere .{ center = .{ x = 1.0, y = 2.0, z = 3.0 }, radius = 0.5 }
  let copy = clone-pod(s.&)
  println("${same-bytes(s.&, copy)} ${copy.* is :sphere _}")
  println("${pre/is-pod[shape]()} ${pre/is-pod[string]()} ${pre/is-pod[array[vec3, 4]]()} ${pre/is-pod[?*vec3]()}")
  0
}
```

```text
200
true true
true false true false
```

A sum of structs of floats is plain old data, so `clone-pod` is a `memcpy`
and `same-bytes` a `memcmp`; `string` and `?*vec3` hold pointers and are
refused at compile time. Called on a struct with a `string` field:

```k1
type user = { id: u64, name: string }

fn main(): i32 {
  let u: user = .{ id = 1, name = "ada" }
  let _ = clone-pod(u.&)
  0
}
```

```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/types_predicates_wrong.k1:22:10: error
├─────
│   
│   fn main(): i32 {
│     let u: user = .{ id = 1, name = "ada" }
│ ->  let _ = clone-pod(u.&)
│             ^^^^^^^^^^^^^^
│     0
│   }
├─────
│  Predicate 'pre/is-pod' failed on type user. Therefore, cannot call function 'clone-pod' with given types: user({ id: u64, name: string({ span: span[char]({ buffer: buffer[char]({ data: ptr, len: i64 }) }) }) })
└────────────────────────────────────────╴
Module types_predicates_wrong failed typechecking with 1 errors
```

C++ ships `std::is_trivially_copyable` as a compiler builtin; here the walk
is user code, so "no pointers anywhere", "every field is under 64 bytes" or
"has a field named `id`" are equally one function. Stable Rust has no
user-defined structural bounds; the nearest thing, an auto trait, is
unstable.

## `never` and zero-sized types

`never` is the type of `crash` and `sys/exit`: uninhabited, so it unifies
with anything, and a variant carrying it cannot be matched because it cannot
exist. `from-string` encodes fallibility in its error type: `u64`'s impl fails
with a `string`, `string`'s own impl with `never`.

```k1
fn parse-all[e, t: from-string[e = e]](items: span[string]): result[list[t], e] {
  let out: list[t] = []
  for s in items {
    let parsed: result[t, e] = from-string/from-string(s)
    if parsed is {
      :ok v -> out.push(v),
      :err e -> return :err e
    }
  }
  :ok out
}

fn unwrap-infallible[t](r: result[t, never]): t {
  if r is { :ok v -> v }
}

fn checked-div(a: int, b: int): int {
  if b == 0 crash("division by zero") else a / b
}

fn main(): i32 {
  println("${parse-all[string, u64](["1", "2", "3"])}")
  println("${parse-all[string, u64](["1", "x", "3"])}")
  let names = unwrap-infallible(parse-all[never, string](["ada", "grace"]))
  println("$names ${checked-div(84, 2)}")

  println("result[u32, string] ${types/size[result[u32, string]]}, result[u32, never] ${types/size[result[u32, never]]}, result[u32, {}] ${types/size[result[u32, {}]]}")
  println("{} ${types/size[{}]}, empty-marker ${types/size[empty-marker]}, array[{}, 1000] ${types/size[array[{}, 1000]]}, ?{} ${types/size[?{}]}, result[{}, {}] ${types/size[result[{}, {}]]}")
  let markers: array[empty-marker, 4] = [.{}, .{}, .{}, .{}]
  println("${markers.len()} markers in ${types/size[array[empty-marker, 4]]} bytes")
  0
}
```

```text
:ok([1,2,3])
:err(not a digit)
[ada,grace] 42
result[u32, string] 24, result[u32, never] 8, result[u32, {}] 8
{} 0, empty-marker 0, array[{}, 1000] 0, ?{} 1, result[{}, {}] 1
4 markers in 0 bytes
```

One `parse-all` serves both: instantiated at `e = never` its `:err` arm is
dead code and the caller's single-arm match is exhaustive, no `unwrap`, no
`Infallible` conversion. A `never` payload takes no bytes (`result[u32,
never]` is a tag plus a `u32`, a third of `result[u32, string]`); a
zero-sized struct costs nothing anywhere, including a thousand of them in an
array, and an optional or result over `{}` is exactly its one-byte tag.

## Type patterns and static specialization

`#if` decides at instantiation and typechecks only the chosen arm. Its
subject can be any compile-time value, here the type's schema, matched with
the ordinary pattern syntax. `copy-into` takes a `memcpy` for scalars and an
element loop otherwise; `describe` proves the dead arm is never checked, since
`v + v` does not typecheck for a string and `v.len()` does not for an int.

```k1
fn copy-into[t](dst: buffer[t], src: span[t]): string {
  #if types/schema[t] is {
    :int _ or :float _ or :bool or :char -> {
      mem/copy(dst = dst.data, src = src.buffer.data, count = src.len() * types/stride[t])
      "memcpy ${src.len() * types/stride[t]} bytes"
    },
    _ -> {
      for src { dst.[it-index] = it }
      "element loop"
    }
  }
}

fn describe[t](v: t): string {
  #if types/schema[t] is {
    :int _ or :float _ -> "scalar ${v + v}",
    _ -> "${v.len()} elements"
  }
}

fn inspect[t](v: t): string {
  if v is {
    type[u8](b) -> "byte $b",
    type[string]("") -> "the empty string",
    type[string](s) -> "string of ${s.len()}",
    type[vec3](.{ x, y, z }) -> "vec3 ${x + y + z}",
    type[?int](:some n) -> "some $n",
    type[?int](:none) -> "none",
    _ -> "other: ${types/name[t]}"
  }
}
```

```text
memcpy 4 bytes [1,2,3,4]
element loop 6.0
element loop [ada,grace]
scalar 42 / scalar 2.5 / 5 elements / 3 elements
byte 7
the empty string
string of 5
vec3 6.0
some 42
none
other: f64
```

`inspect` is the runtime-value form: `type[u8](b)` tests the generic
parameter, not the value, so each instantiation keeps only the arms whose
type matches, and inside a matched arm the ordinary patterns nest (`""`,
struct destructuring, `:some n`). C++ has `if constexpr`, Zig has `comptime`
branches; stable Rust has neither, and specializing a generic on a concrete
type needs a trait per case.
