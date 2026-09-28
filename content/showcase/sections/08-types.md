# Types are values

At compile time a type is just a value. `types/make-struct` and
`types/make-either` build new ones, and `#type <expr>` runs any expression in
the compile-time VM and uses the type it returns. No template language, just K1.

## A struct from a schema string

The parser is ordinary K1, in `ns pre` so it compiles before anything that
needs it:

```k1
  fn table(schema: string): types/type-id {
    let fields: list[{ name: string, type: types/type-id }] = []
    for column in schema.split-by-char(',') {
      let parts = column.split-by-char(':')
      fields.push(.{ name = trim(parts.[0]), type = column-type(trim(parts.[1])) })
    }
    types/make-struct(:struct, fields.as-span())
  }
```

```k1
type(alias) user = #type pre/table("id: u64, name: string, score: f64, active: bool")
```

```k1
  let u: user = .{ id = 7, name = "ada", score = 99.5, active = true }
  u.score = u.score + 0.5
  println("${u.id} ${u.name} ${u.score} ${u.active}")
  println("size ${types/size[user]} align ${types/align[user]}")
  println(types/name[user])
```

```text
7 ada 100.0 true
size 33 align 8
{ id: u64, name: string, score: f64, active: bool }
```

Not a lookalike. It's the same type you'd write by hand:

```k1
  assert(types/id[user] == types/id[{ id: u64, name: string, score: f64, active: bool }])
```

Swap the schema string for a `CREATE TABLE` and nothing changes but the parser.

## Variants from a table

Sums work the same way. HTTP status codes, tags and all:

```k1
ns pre {
  let STATUS_TABLE: span[{ name: string, code: u16 }] = [
    .{ name = "ok", code = 200 },
    .{ name = "created", code = 201 },
    .{ name = "not-found", code = 404 },
    .{ name = "teapot", code = 418 },
    .{ name = "internal-error", code = 500 },
  ]

  fn status-enum(): types/type-id {
    let variants: list[{ name: string, payload: ?types/type-id, tag: ?types/int-value }] = []
    for row in STATUS_TABLE {
      variants.push(.{ name = row.name, payload = :none, tag = :some(types/int-value:u16(row.code)) })
    }
    types/make-either(:some(types/id[u16]), variants.as-span())
  }
}

type(alias) status = #type pre/status-enum()
```

```k1
fn classify(s: status): string {
  if s is {
    :ok or :created -> "success",
    :not-found -> "client error",
    :teapot -> "short and stout",
    :internal-error -> "server error",
  }
}
```

```k1
  let s: status = :teapot
  println("${s.enum-name()} = ${s.value} -> ${classify(s)}")
  println("size ${types/size[status]} tag ${status:internal-error.value}")
```

```text
teapot = 418 -> short and stout
size 2 tag 500
```

Two bytes, because the tag type is `u16`.

Variants with payloads:

```k1
type(alias) event = #type types/make-either-default([
  types/variant-data[u16]("connected"),
  types/variant-data[string]("data"),
  types/variant-empty("closed"),
])
```

Every sum also gets `.tag-enum`, the payload-less enum of its tags:

```k1
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
```

```text
counts [1,2,1]
```

Exhaustiveness checking doesn't care where a type came from. Cut the table to
three rows and forget one:

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
│  Non-exhaustive match; this pattern is not covered:
- :teapot
└────────────────────────────────────────╴
Module types_derive_variants_wrong failed compiling with 1 errors
```

## Struct transforms

Two are built in:

```k1
type user = { id: u64, name: string, password: string, email: string }
type user-view = _struct_remove[user, { password: string }]
type audited = { created: u64, updated: u64 }
type user-row = _struct_combine[user, audited]
```

```k1
fn view(u: user): user-view {
  .{ id = u.id, name = u.name, email = u.email }
}
```

```k1
  let u: user = .{ id = 1, name = "ada", password = "hunter2", email = "ada@k1.dev" }
  let v = view(u)
  println("${v.id} ${v.name} ${v.email}")

  let row: user-row = .{ id = 1, name = "ada", password = "hunter2", email = "ada@k1.dev", created = 100, updated = 200 }
  println("${row.name} created ${row.created}")
```

```text
1 ada ada@k1.dev
ada created 100
```

Anything else is a function from `type-id` to `type-id`. Here's every field
wrapped in `opt`:

```k1
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
```

```k1
fn apply(u: *user, p: user-patch) {
  if p.name is :some n { u.name = n }
  if p.email is :some e { u.email = e }
  if p.password is :some pw { u.password = pw }
}
```

```k1
  apply(u.&, .{ id = :none, name = :some "ada lovelace", password = :none, email = :none })
  println(u.name)

  println("user ${types/size[user]} view ${types/size[user-view]} row ${types/size[user-row]} patch ${types/size[user-patch]}")
  println(types/name[user-view])
  println(types/name[user-patch])
```

```text
ada lovelace
user 56 view 40 row 72 patch 88
user-view
{ id: opt[u64], name: opt[string], password: opt[string], email: opt[string] }
```

Four real types, four layouts. One wall today: a type alias can't take type
parameters, so there's no `patch[t]`, only `patch-of` per type.

## Static parameters and literal types

A generic parameter can be a value:

```k1
fn add-static[i: static int](x: int): int {
  core/meta/static-type-to-value[_, i]() + x
}
```

```k1
  println("${add-static[3](4)} ${add-static[40](2)}")
  let five: static int = 5
  println("${add-static[type-of(five)](10)}")
```

```text
7 42
15
```

It flows into array lengths and layout:

```k1
type matrix[n: static size] = { rows: array[array[i64, n], n] }
```

```k1
  let a: matrix[2] = .{ rows = [[1, 2], [3, 4]] }
  let sq = a.mul(a.&)
  println("${sq.rows} dim ${sq.dim()} size ${types/size[matrix[2]]}")
  let i3 = matrix/identity[3]()
  println("${i3.rows} size ${types/size[matrix[3]]}")
```

```text
[[7,10],[15,22]] dim 2 size 32
[[1,0,0],[0,1,0],[0,0,1]] size 72
```

`matrix[2]` times `matrix[3]` doesn't compile. `test-compile` hands the error
back as a string:

```k1
  let mismatch: ?string = test-compile(a.mul(i3.&))
  println(mismatch.!)
```

```text
Error in parameter 'b' in call to 'mul'
Expected matrix[static[i64, 2]] but got matrix[static[i64, 3]]: Param 'n' is incorrect: Different static values of same type family: 2 vs 3
```

`#for` runs its list in the VM and stamps out the body once per element:

```k1
  fn squares(n: int): span[int] {
    let out: list[int] = []
    for i in 1.until(n + 1) { out.push(i * i) }
    out.as-span()
  }
```

```k1
  let total = 0
  #for pre/squares(4) {
    total = total + it
    println("unrolled step $it, total $total")
  }
```

```text
unrolled step 1, total 1
unrolled step 4, total 5
unrolled step 9, total 14
unrolled step 16, total 30
```

The emitted IR has four `println` calls and no `squares` at all.

Literals are types too:

```k1
  let c: 'c' = 'c'
  let pi: 3.14 = 3.14
  let tag: "showcase" = "showcase"
  println("${c.from-static()} ${pi.from-static()} ${tag.from-static()}")
  let wrong-literal: ?string = test-compile('d': 'c')
  println(wrong-literal.!)
```

```text
c 3.14 showcase
Expression did not conform to hint: Static lift resulted in wrong value: Different static values of same type family: c vs d
```

That's the same mechanism that types the result of every `#static` block.

## Predicate bounds

A bound can be any function:

```k1
  fn is-int[t](): bool { types/schema[t] is :int _ }
```

```k1
type int-wrapper[t: pred pre/is-int] = { value: t }
```

Plain old data, as a recursive walk over the schema:

```k1
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
```

Then `memcpy` and `memcmp` are safe:

```k1
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
```

```k1
  let w: int-wrapper[u8] = .{ value = 200 }
  println("${w.value}")

  let s: shape = :sphere .{ center = .{ x = 1.0, y = 2.0, z = 3.0 }, radius = 0.5 }
  let copy = clone-pod(s.&)
  println("${same-bytes(s.&, copy)} ${copy.* is :sphere _}")
  println("${pre/is-pod[shape]()} ${pre/is-pod[string]()} ${pre/is-pod[array[vec3, 4]]()} ${pre/is-pod[?*vec3]()}")
```

```text
200
true true
true false true false
```

Anything holding a pointer gets refused:

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
Module types_predicates_wrong failed compiling with 1 errors
```

"Every field under 64 bytes" or "has a field named `id`" are just as easy.

## `never` and zero-sized types

`from-string` puts fallibility in its error type:

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
```

```k1
  println("${parse-all[string, u64](["1", "2", "3"])}")
  println("${parse-all[string, u64](["1", "x", "3"])}")
```

```text
:ok([1,2,3])
:err(not a digit)
```

`string` from a string can't fail, so its error type is `never`. A `never`
variant can't exist, so it needs no arm:

```k1
fn unwrap-infallible[t](r: result[t, never]): t {
  if r is { :ok v -> v }
}
```

`crash` returns `never`, which unifies with anything:

```k1
fn checked-div(a: int, b: int): int {
  if b == 0 crash("division by zero") else a / b
}
```

```k1
  let names = unwrap-infallible(parse-all[never, string](["ada", "grace"]))
  println("$names ${checked-div(84, 2)}")
```

```text
[ada,grace] 42
```

`never` takes no bytes, and neither does `{}`:

```k1
  println("result[u32, string] ${types/size[result[u32, string]]}, result[u32, never] ${types/size[result[u32, never]]}, result[u32, {}] ${types/size[result[u32, {}]]}")
  println("{} ${types/size[{}]}, empty-marker ${types/size[empty-marker]}, array[{}, 1000] ${types/size[array[{}, 1000]]}, ?{} ${types/size[?{}]}, result[{}, {}] ${types/size[result[{}, {}]]}")
  let markers: array[empty-marker, 4] = [.{}, .{}, .{}, .{}]
  println("${markers.len()} markers in ${types/size[array[empty-marker, 4]]} bytes")
```

```text
result[u32, string] 24, result[u32, never] 8, result[u32, {}] 8
{} 0, empty-marker 0, array[{}, 1000] 0, ?{} 1, result[{}, {}] 1
4 markers in 0 bytes
```

## Type patterns and static specialization

`#if` picks an arm per instantiation and only typechecks that one. The
subject can be any compile-time value, like a schema:

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
```

```k1
  let bytes: buffer[u8] = buffer/filled(4, 0)
  println("${copy-into(bytes, [1u8, 2, 3, 4])} ${bytes}")
  let vecs: buffer[vec3] = buffer/filled(2, .0)
  println("${copy-into(vecs, [.{ x = 1.0, y = 2.0, z = 3.0 }, .{ x = 4.0, y = 5.0, z = 6.0 }])} ${vecs.[1].z}")
  let names: buffer[string] = buffer/filled(2, "")
  println("${copy-into(names, ["ada", "grace"])} ${names}")
```

```text
memcpy 4 bytes [1,2,3,4]
element loop 6.0
element loop [ada,grace]
```

The dead arm really is never checked. `v + v` on a string or `v.len()` on an
int would be errors:

```k1
fn describe[t](v: t): string {
  #if types/schema[t] is {
    :int _ or :float _ -> "scalar ${v + v}",
    _ -> "${v.len()} elements"
  }
}
```

```k1
  println("${describe(21)} / ${describe(1.25)} / ${describe("hello")} / ${describe([1, 2, 3])}")
```

```text
scalar 42 / scalar 2.5 / 5 elements / 3 elements
```

`type[x](...)` patterns match on the type parameter, and ordinary patterns
nest inside:

```k1
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

```k1
  println(inspect(7u8))
  println(inspect(""))
  println(inspect("hello"))
  println(inspect(vec3.{ x = 1.0, y = 2.0, z = 3.0 }))
  let some: ?int = :some 42
  let none: ?int = :none
  println(inspect(some))
  println(inspect(none))
  println(inspect(3.5))
```

```text
byte 7
the empty string
string of 5
vec3 6.0
some 42
none
other: f64
```

## Elsewhere

- Zig: `@Type` on a `std.builtin.Type`, `comptime n`, `comptime` branches.
  Closest in spirit.
- C++26: `std::meta::define_aggregate`, non-type template parameters,
  `if constexpr`. `std::is_trivially_copyable` is a compiler builtin; here
  it's a user function.
- Rust: proc macros emit tokens and never see a type or a size. No
  user-defined structural bounds, no specializing a generic on a concrete
  type without a trait per case.
