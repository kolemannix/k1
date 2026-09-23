# Reflection: a type is a value you can look at

`types/schema[t]` returns an ordinary sum value describing `t`. It is not a
special syntax, a separate reflection language, or a token stream: it is a
`type-schema`, defined in `modules/core/builtin.k1`, and you take it apart with
the same `if x is { ... }` you use on any other sum. It works inside `#static`,
inside `#meta`, and in plain runtime code.

```k1 path=modules/core/builtin.k1
  type type-schema = either {
    char,
    bool,
    ptr,
    int(int-kind),
    float(float-kind),
    enum({
      int-kind,
      values: span[{
        name: string,
        value: int-value,
      }]
    }),
    reference({ inner-type-id: type-id }),
    array({
      element-type-id: type-id,
      size: ?int
    }),
    vector({
      element-type-id: type-id,
      size: ?int
    }),
    struct({
      fields: span[{
        name: string,
        type-id,
        offset: size
      }],
    }),
    union({
      fields: span[{
        name: string,
        type-id,
        offset: size
      }],
    }),
    either({
      tag-type: int-kind,
      payload-offset: size,
      variants: span[{
        name: string,
        tag: int-value,
        payload: ?{ type-id: type-id }
      }],
    }),
    string,
    never,
    function({
      params: span[{ name: string, type-id: type-id }],
      return-type-id: type-id,
    }),
    function-pointer({ function-type-id: type-id }),
    other(string)
  }
```

Alongside it: `types/id[t]` (a `type-id` with `.name()`, `.schema()`,
`.instance-info()`, `.spell()`), `types/size`, `types/stride`, `types/align`,
and `types/assert-layout`. Everything below is built from those.

## JSON for any type, generated per type

```k1
  fn write[t, w: writer](w: w, v: t) {
    #meta {
      let id = types/id[t]
      let cb = code-builder/new()
      if id.is-instance-of(types/id[opt]) {
        cb.line(`if v is { :some x -> write(w, x), :none -> w.write("null") }`)
      } else if id.is-instance-of(types/id[list]) or types/schema[t] is :array _ {
        cb.line(`w.write-char('[')`)
        cb.line(`for v { if it-index > 0 { w.write-char(',') }; write(w, it) }`)
        cb.line(`w.write-char(']')`)
      } else if types/schema[t] is {
        :bool -> cb.line(`v.print-to(w)`),
        :int _ -> cb.line(`v.print-to(w)`),
        :float _ -> cb.line(`v.print-to(w)`),
        :string -> cb.line(`write-escaped(w, v)`),
        :char -> cb.line(`write-escaped(w, v.to-string())`),
        :reference _ -> cb.line(`write(w, v.*)`),
        :enum _ -> cb.line(`write-escaped(w, v.enum-name())`),
        :struct .{ fields } -> {
          cb.line(`w.write-char('{')`)
          for f in fields {
            let key = if it-index == 0 `"${f.name}":` else `,"${f.name}":`
            cb.line(`w.write(${meta/str-lit(key)})`)
            cb.line(`write(w, v.${f.name})`)
          }
          cb.line(`w.write-char('}')`)
        },
        :either .{ variants } -> {
          cb.line("if v is {")
          cb.indent()
          for x in variants {
            if x.payload is :some _ {
              let key = meta/str-lit(`{"${x.name}":`)
              cb.line(`:${x.name} p -> { w.write($key); write(w, p); w.write-char('}') },`)
            } else {
              cb.line(`:${x.name} -> w.write(${meta/str-lit(`"${x.name}"`)}),`)
            }
          }
          cb.dedent()
          cb.line("}")
        },
        _ -> crash("json: no encoding for ${types/name[t]}")
      }
      let generated = cb.build()
      k1/emit-compiler-message(k1/location(), :info, "json/write[${types/name[t]}]\n${generated.text()}")
      generated
    }
  }
```

`write` is a generic function whose body is a `#meta` block. Every time the
compiler instantiates `write` at a new `t`, the block runs in the compile-time
VM, inspects `types/schema[t]`, and returns the `code` that becomes the body
for that `t`. The generated body calls `write` on each field, which instantiates
the next type, so the whole serializer for a nested type is assembled one
schema at a time. Optionals are recognized as instances of the `opt` generic
before the general `either` arm, so `?string` becomes `null` or the string
rather than `{"some": ...}`; enums become their variant name; sums with a
payload become a one-key object.

The last line of the block is the metaprogram logging its own expansion with
the compiler's message facility. Given

```k1
type role = either { admin, member, guest }
type contact = either { email(string), phone(string), unreachable }
type person = {
  name: string,
  initial: char,
  age: u32,
  score: f64,
  active: bool,
  role: role,
  nickname: ?string,
  contacts: list[contact],
  scores: array[i32, 3],
}
```

the compiler prints, while compiling `json/to-string(ada)`:

```text
[reflect_json.k1:65 info] json/write[person]
w.write-char('{')
w.write("\"name\":")
write(w, v.name)
w.write(",\"initial\":")
write(w, v.initial)
w.write(",\"age\":")
write(w, v.age)
w.write(",\"score\":")
write(w, v.score)
w.write(",\"active\":")
write(w, v.active)
w.write(",\"role\":")
write(w, v.role)
w.write(",\"nickname\":")
write(w, v.nickname)
w.write(",\"contacts\":")
write(w, v.contacts)
w.write(",\"scores\":")
write(w, v.scores)
w.write-char('}')

[reflect_json.k1:65 info] json/write[contact]
if v is {
  :email p -> { w.write("{\"email\":"); write(w, p); w.write-char('}') },
  :phone p -> { w.write("{\"phone\":"); write(w, p); w.write-char('}') },
  :unreachable -> w.write("\"unreachable\""),
}

[reflect_json.k1:65 info] json/write[opt[string]]
if v is { :some x -> write(w, x), :none -> w.write("null") }
```

and the program itself prints:

```text
{"name":"Ada \"Countess\" Lovelace","initial":"A","age":36,"score":99.5,"active":true,"role":"admin","nickname":null,"contacts":[{"email":"ada@example.com"},"unreachable"],"scores":[10,20,30]}
{"name":"Ada \"Countess\" Lovelace","initial":"A","age":36,"score":99.5,"active":true,"role":"admin","nickname":"Ada","contacts":[],"scores":[10,20,30]}
[1,2,3]
[{"email":"ada@example.com"},"unreachable"]
{"nested":{"deep":[{"ok":true}]},"tab":"a\tb\n"}
```

The last line serializes an anonymous struct literal that was never declared
anywhere; there is no type to attach a derive to, and none is needed. Nothing
in `person`, `role` or `contact` mentions JSON. Rust gets the same
per-type serializer from `#[derive(Serialize)]`, but the derive is a proc
macro reading the tokens of the type definition: it sees that a field is
spelled `Option<String>`, not that it is an optional, and it cannot see the
definition of `contact` at all. Here the walk is over resolved types, so a
field of type `?string` and a field of a type alias for it take the same arm.

## Derive without derive

```k1
fn show[t](v: t): string {
  let sb = string-builder/new()
  show-to(sb.&, v, 0)
  sb.build()
}

fn show-to[t, w: writer](w: w, v: t, depth: size) {
  #meta {
    let id = types/id[t]
    let cb = code-builder/new()
    if id.is-instance-of(types/id[opt]) {
      cb.line(`if v is { :some x -> show-to(w, x, depth), :none -> w.write("none") }`)
    } else if id.is-instance-of(types/id[list]) or types/schema[t] is :array _ {
      cb.line(`w.write("[")`)
      cb.line(`for v { if it-index > 0 { w.write(", ") }; show-to(w, it, depth) }`)
      cb.line(`w.write("]")`)
    } else if types/schema[t] is {
      :bool -> cb.line(`v.print-to(w)`),
      :int _ -> cb.line(`v.print-to(w)`),
      :float _ -> cb.line(`v.print-to(w)`),
      :char -> cb.line(`v.print-to(w)`),
      :string -> cb.line(`w.write(meta/str-lit(v))`),
      :reference _ -> cb.line(`w.write("&"); show-to(w, v.*, depth)`),
      :enum _ -> cb.line(`w.write-char(':'); w.write(v.enum-name())`),
      :struct .{ fields } -> {
        cb.line(`w.write(".{\\n")`)
        for f in fields {
          cb.line(`w.repeat("  ", depth + 1); w.write("${f.name} = "); show-to(w, v.${f.name}, depth + 1); w.write(",\\n")`)
        }
        cb.line(`w.repeat("  ", depth); w.write("}")`)
      },
      :either .{ variants } -> {
        cb.line("if v is {")
        cb.indent()
        for x in variants {
          if x.payload is :some _ {
            cb.line(`:${x.name} p -> { w.write(":${x.name} "); show-to(w, p, depth) },`)
          } else {
            cb.line(`:${x.name} -> w.write(":${x.name}"),`)
          }
        }
        cb.dedent()
        cb.line("}")
      },
      _ -> crash("show: no printer for ${types/name[t]}")
    }
    cb.build()
  }
}
```

The same shape as the JSON writer, producing K1 literal syntax with
indentation. `show` needs no `impl` on the user's types and no annotation:
it is a plain generic function, and the types it prints were written without
knowing it exists.

```k1
type role = either { admin, member }
type address = { city: string, zip: u32 }
type shape = either { circle(f64), rect({ w: f64, h: f64 }), dot }
type person = {
  name: string,
  age: u32,
  role: role,
  home: ?address,
  friends: list[string],
  favorite: shape,
  manager: ?*person,
}
```

```text
.{
  name = "Ada \"Countess\" Lovelace",
  age = 36,
  role = :member,
  home = .{
    city = "London",
    zip = 12345,
  },
  friends = ["Babbage", "Somerville"],
  favorite = :rect .{
    w = 2.0,
    h = 3.5,
  },
  manager = &.{
    name = "Grace",
    age = 45,
    role = :admin,
    home = none,
    friends = [],
    favorite = :dot,
    manager = none,
  },
}
[1, none]
[a, b, c]
:some .{ city = London, zip = 12345 }
```

The last line is the compiler's own derived `print` for `ada.home`, which
exists for every struct and sum automatically. It does not quote strings, and
it refuses `person` outright because `opt[*person]` has no `print` impl for
references; `show` follows the reference and prints what it finds. The point
is not that the built-in derive is weak but that it is not privileged: what
the compiler does for `print`, a library can do for anything else, with the
same information, in the same language.

## `enum_to_string`, both ways

Payload-less enums already carry their names: `c.enum-name()` is built in.
The generic version, the poster child of C++26's P2996 reflection paper, is a
few lines when the schema is data:

```k1
  fn names[t](): span[string] {
    #static {
      require types/schema[t] is :enum .{ values } else crash("${types/name[t]} is not an enum")
      let out = list/empty[string]()
      for v in values { out.push(v.name) }
      out.as-span()
    }
  }
```

`names[color]()` is a compile-time constant: the `#static` block runs once per
instantiation and the resulting span is baked into the binary. Baked values
have literal types, so the compiler can check the table's contents in a type
annotation:

```k1
  let count: 3 = #static enums/names[color]().len()
  let first: "red" = #static enums/names[color]().[0]
```

For the string-to-enum direction, and for a `to-string` that does not go
through a table at all, a `#meta` block emits the match directly:

```k1
  fn enum-to-string[t](v: t): string {
    #meta {
      require types/schema[t] is :enum .{ values } else crash("${types/name[t]} is not an enum")
      let cb = code-builder/new()
      cb.line("if v is {")
      cb.indent()
      for x in values { cb.line(`:${x.name} -> ${meta/str-lit(x.name)},`) }
      cb.dedent()
      cb.line("}")
      cb.build()
    }
  }

  fn enum-from-string[t](s: string): ?t {
    #meta {
      require types/schema[t] is :enum .{ values } else crash("${types/name[t]} is not an enum")
      let cb = code-builder/new()
      for x in values { cb.line(`if s == ${meta/str-lit(x.name)} return :some :${x.name}`) }
      cb.line(":none")
      cb.build()
    }
  }
```

The generic parser then plugs into the standard `from-string` ability, so
`level` parses like `u64` does, with an error message listing the valid names
from the same compile-time table:

```k1
type color = either { red, green, blue }
type level = either(u8) { debug = 10, info = 20, warn = 30, error = 40 }

impl from-string[e = string] for level {
  fn from-string(s: string): result[level, string] {
    if enums/enum-from-string[level](s) is {
      :some v -> :ok v,
      :none -> :err("expected one of ${enums/names[level]()}, got '$s'"),
    }
  }
}
```

```text
green
green
warn
[red,green,blue] [debug,info,warn,error]
3 red
color = either(u8) { red = 0u8, green = 1u8, blue = 2u8 }
level = either(u8) { debug = 10u8, info = 20u8, warn = 30u8, error = 40u8 }
:some blue
:none
:ok(warn)
:err(expected one of [debug,info,warn,error], got 'fatal')
```

The two `either(u8) { ... }` lines are `describe[t]`, which reconstructs the
declaration from `int-kind` and the tag values; `level`'s explicit tags come
back as `10u8` because `int-value` is itself a sum over the integer kinds. In
P2996 the equivalent is `template for (constexpr auto e :
std::meta::enumerators_of(^^E))` with `[:e:]` to splice each reflection back
into an expression and `std::meta::identifier_of(e)` for the name; Zig has
`@tagName` built in and `std.meta.stringToEnum` in its library. K1 has no
splice operator because there is nothing to splice: a variant name is a
`string`, and emitting `:${x.name}` into code is string interpolation.

## pahole in a screenful

Struct schemas carry field offsets, and `types/size`, `types/stride` and
`types/align` answer for any type, so a layout dumper is a walk over the fields
that keeps a cursor:

```k1
  fn members[t](): span[member] {
    #meta {
      let fields = if types/schema[t] is {
        :struct s -> s.fields,
        :union u -> u.fields,
        _ -> crash("${types/name[t]} has no members"),
      }
      let cb = code-builder/new()
      cb.write("[")
      for f in fields {
        let ty = f.type-id.spell()
        cb.line(`.{ name = ${meta/str-lit(f.name)}, type-name = ${meta/str-lit(f.type-id.name())}, offset = ${f.offset}, size = types/size[$ty], stride = types/stride[$ty], align = types/align[$ty] },`)
      }
      cb.write("]")
      cb.build()
    }
  }
```

```k1
  fn dump[t]() {
    let is-union = types/schema[t] is :union _
    let stride = types/stride[t]
    println("${types/name[t]}: size ${types/size[t]}, stride $stride, align ${types/align[t]}")
    let end: size = 0
    let holes: size = 0
    for m in members[t]() {
      if m.offset > end {
        println("        ~~ ${m.offset - end} byte hole")
        holes = holes + m.offset - end
      }
      let cell = padded("${m.name}: ${m.type-name}", 24)
      println("  ${padded("${m.offset}", 4)}$cell size ${m.size}, stride ${m.stride}, align ${m.align}")
      if is-union {
        if m.stride > end { end = m.stride }
      } else {
        end = m.offset + m.stride
      }
    }
    if stride > end {
      println("        ~~ ${stride - end} bytes tail padding")
      holes = holes + stride - end
    }
    println("  $holes of $stride bytes are padding\n")
  }
```

`members` is the one place a `#meta` is needed: `types/size` takes a type, not
a `type-id`, so each field's `type-id` is turned back into a type expression
with `.spell()` and emitted into a struct literal. Run over a badly ordered
struct, the same fields sorted by alignment, the packed version, a union, and
a struct nesting two of them:

```k1
type sloppy = { flag: bool, id: u64, kind: u16, count: u32, tail: u8 }
type tidy = { id: u64, count: u32, kind: u16, flag: bool, tail: u8 }
type wire = packed { flag: bool, id: u64, kind: u16, count: u32, tail: u8 }
type scalar = union { i: i64, f: f64, bytes: array[u8, 8], half: u16 }
type nested = { header: wire, body: sloppy }
```

```text
sloppy: size 25, stride 32, align 8
  0   flag: bool               size 1, stride 1, align 1
        ~~ 7 byte hole
  8   id: u64                  size 8, stride 8, align 8
  16  kind: u16                size 2, stride 2, align 2
        ~~ 2 byte hole
  20  count: u32               size 4, stride 4, align 4
  24  tail: u8                 size 1, stride 1, align 1
        ~~ 7 bytes tail padding
  16 of 32 bytes are padding

tidy: size 16, stride 16, align 8
  0   id: u64                  size 8, stride 8, align 8
  8   count: u32               size 4, stride 4, align 4
  12  kind: u16                size 2, stride 2, align 2
  14  flag: bool               size 1, stride 1, align 1
  15  tail: u8                 size 1, stride 1, align 1
  0 of 16 bytes are padding

wire: size 16, stride 16, align 1
  0   flag: bool               size 1, stride 1, align 1
  1   id: u64                  size 8, stride 8, align 8
  9   kind: u16                size 2, stride 2, align 2
  11  count: u32               size 4, stride 4, align 4
  15  tail: u8                 size 1, stride 1, align 1
  0 of 16 bytes are padding

scalar: size 8, stride 8, align 8
  0   i: i64                   size 8, stride 8, align 8
  0   f: f64                   size 8, stride 8, align 8
  0   bytes: array[u8, 8]      size 8, stride 8, align 1
  0   half: u16                size 2, stride 2, align 2
  0 of 8 bytes are padding

nested: size 48, stride 48, align 8
  0   header: wire             size 16, stride 16, align 1
  16  body: sloppy             size 25, stride 32, align 8
  0 of 48 bytes are padding
```

K1 distinguishes `size` (the bytes a value occupies) from `stride` (the bytes
between array elements, C's `sizeof`): `sloppy` is 25 bytes of data in a
32-byte slot, and as a field of `nested` it takes its stride. Packed structs
are an ordinary `:struct` schema; the packing is visible only in the offsets.

The same numbers make a compile-time guard. `types/assert-layout` is a
library function in `modules/core/types.k1`, run under `#static`:

```k1
  #static types/assert-layout[tidy](16, 8, [0, 8, 12, 14, 15])
  #static types/assert-layout[wire](16, 1, [0, 1, 9, 11, 15])
```

A wrong guard (`reflect_layout_fail.k1` claims a packed `{ tag: u8, len: u32 }`
is 8 bytes) is a compile error. The failing assertion is an ordinary
`assert-equals` inside the library function; the error is reported at that
line, and the VM's stack trace leads back to the `#static` in `main`:

```k1
type header = packed { tag: u8, len: u32 }

fn main(): i32 {
  #static types/assert-layout[header](8, 4, [0, 4])
  0
}
```

```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/modules/core/types.k1:42:2: error
├─────
│   fn assert-layout[t](size: size, align: size, struct-fields: span[size]) {
│     let actual-size = types/stride[t]
│     let actual-align = types/align[t]
│ ->  assert-equals(size, actual-size)
│     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
│     assert-equals(align, actual-align)
│     if struct-fields.non-empty() {
├─────
│  ASSERT FAILED: 8 != 5
bc Execution Trace
[00] core/crash builtin.k1:1083
[01] core/assert-equals core.k1:6
[02] core/types/assert-layout types.k1:43
[03] expr reflect_layout_fail.k1:4 reflect_layout_fail.k1:4

└────────────────────────────────────────╴
Module reflect_layout_fail failed typechecking with 1 errors
```

Zig has `@offsetOf` and `@sizeOf` as builtins and `@typeInfo(T).@"struct".fields`
for the walk; the pieces are equivalent, and this tool ports almost line for
line. C++ has `offsetof` and `sizeof` and, until P2996 ships, no standard way
to enumerate the members.

## Generics and functions are types too

A bare generic name is a type: `types/id[list]` is the id of `list[t]` itself,
and an instance knows its parent and arguments.

```k1
fn describe[t](): string {
  let id = types/id[t]
  if id.instance-info() is {
    :some .{ parent, args } -> {
      let arg-names = list/empty[string]()
      for a in args { arg-names.push(a.name()) }
      "${id.name()} = ${parent.name()} applied to [${string/join(arg-names, ", ")}]"
    },
    :none -> "${id.name()} is not a generic instance"
  }
}
```

That query can gate a generic. A predicate bound is a compile-time boolean
function over a type, declared in the module's `pre` namespace:

```k1
ns pre {
  fn is-list[t](): bool { types/id[t].is-instance-of(types/id[list]) }
}
```

```k1
fn element-type[t: pred pre/is-list](): string { types/instance-info[t].!.args.[0].name() }
```

Functions reflect as `:function` schemas with parameter names, and a function's
parameter and return types are addressable as `types/id[do-it.return]` and
`types/id[do-it.input]`. A bare function name is a zero-sized value of its own
function type; `do-it.&` is the pointer:

```k1
fn do-it(input: string, count: u32): bool { count > 0 and input.len() > 0 }
```

```k1
  println(types/id[do-it.return].name())
  println(types/id[do-it.input].name())
  require types/schema[do-it] is :function .{ params, return-type-id } else crash("do-it is a function")
  for p in params { println("  ${p.name}: ${p.type-id.name()}") }
  println("  -> ${return-type-id.name()}")
  #static assert(types/id[do-it.return] == types/id[bool])

  let f: *fn(string, u32) -> bool = do-it.&
  require types/schema[type-of(f)] is :function-pointer .{ function-type-id } else crash("f is a fn pointer")
  println(function-type-id.name())
```

```text
list[i64] = list[t] applied to [i64]
pair[string, opt[u8]] = pair[a, b] applied to [string, opt[u8]]
result[i64, string] = result[t, e] applied to [i64, string]
i64 is not a generic instance
*pair[i64, i64] is not a generic instance
list[t]
true false
string
Predicate 'pre/is-list' failed on type i64. Therefore, cannot call function 'element-type' with given types: i64
bool
string
  input: string
  count: u32
  -> bool
fn(input: string, count: u32) -> bool
```

The `Predicate 'pre/is-list' failed` line is `test-compile(element-type[int]())`
returning the compiler's error for the rejected instantiation as a string, at
compile time, to a program that then prints it. `?u8` shows up as `opt[u8]`
and `int` as `i64`: names come from the resolved type, not from how it was
spelled.

## Elsewhere

C++26's P2996 introduces `^^T` to produce a `std::meta::info`, `[: r :]` to
splice one back into code, and a `std::meta` library (`members_of`,
`nonstatic_data_members_of`, `enumerators_of`, `identifier_of`, `offset_of`)
usable in `constexpr` evaluation, with `template for` to iterate. It is
the closest design to K1's, and it arrives with two new operators and a
consteval-only value type; in K1 the reflection value is a plain `either` and
code generation is string interpolation into `code`. Zig's `@typeInfo` is the
same idea as `type-schema` (a tagged union you switch on at comptime) with
`inline for` and `@field(v, name)` for the walk; where K1 differs is that a
`#meta` block can emit any code, including new declarations, and the
`#static` table of names becomes a literal type the checker can see. Rust has
no reflection at either compile time or runtime; proc macros see the tokens
of one item and reconstruct what they can, which is why `serde_derive` treats
a field as optional only when its type is literally spelled `Option<...>`, and
why a derive on `person` cannot tell that `role` is an enum without `role`
also opting in.
