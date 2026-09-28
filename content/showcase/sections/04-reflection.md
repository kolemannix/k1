# Reflection: a type is a value you can look at

`types/schema[t]` returns an ordinary sum value describing `t`, taken apart
with the same `if x is { ... }` as any other sum, at compile time or runtime.

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
`.instance-info()`, `.size()`, `.stride()`, `.align()`, `.spell()`),
`types/size`, `types/stride`, `types/align`, and `types/assert-layout`.

## JSON for any type, generated per type

A generic function whose body is a `#meta` block: each new `t` runs the block
at compile time, and the `code` it returns becomes the body for that `t`.

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

None of these types mention JSON:

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

```k1
  println(json/to-string(ada))
  println(json/to-string(ada | .{ nickname = :some "Ada", contacts = [] }))
```

```text
{"name":"Ada \"Countess\" Lovelace","initial":"A","age":36,"score":99.5,"active":true,"role":"admin","nickname":null,"contacts":[{"email":"ada@example.com"},"unreachable"],"scores":[10,20,30]}
{"name":"Ada \"Countess\" Lovelace","initial":"A","age":36,"score":99.5,"active":true,"role":"admin","nickname":"Ada","contacts":[],"scores":[10,20,30]}
```

The block logs what it generated, so compiling that call also prints, among others:

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

Each field's `write` call instantiates the next type. No declaration is
needed at all:

```k1
  println(json/to-string(.{ nested = .{ deep = [.{ ok = true }] }, tab = "a\tb\n" }))
```

```text
{"nested":{"deep":[{"ok":true}]},"tab":"a\tb\n"}
```

Rust's `#[derive(Serialize)]` reads tokens: it sees `Option<String>` spelled
out, not an optional, and can't see `contact` at all. Here the walk is over
resolved types.

## Derive without derive

Same shape, printing K1 literal syntax. No `impl`, no annotation.

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

Types written without knowing `show` exists:

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

```k1
  println(show(ada))
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
```

## Enums: names out, names in

`enum_to_string`, the poster child of C++26's P2996, is built in:
`c.enum-name()`. The table of all names:

```k1
  fn names[t](): span[string] {
    #static {
      require types/schema[t] is :enum .{ values } else crash("${types/name[t]} is not an enum")
      values.map(fn v. v.name)
    }
  }
```

It is a compile-time value, and `#static(colors)` hands it back to
compile-time code:

```k1
  let colors = #static enums/names[color]
  #static(colors) assert-equals(colors.len(), 3)
  let first: "red" = #static(colors) colors.[0]
  println("$colors ${enums/names[level]} ${first.from-static()}")
```

```text
[red,green,blue] [debug,info,warn,error] red
```

The schema carries tag types and values, enough to rebuild the declaration:

```k1
  println(enums/describe[color])
  println(enums/describe[level])
```

```text
color = either(u8) { red = 0u8, green = 1u8, blue = 2u8 }
level = either(u8) { debug = 10u8, info = 20u8, warn = 30u8, error = 40u8 }
```

String to enum: a macro that writes the `from-string` impl for any enum.

```k1
  macro derive-from-string[t]() {
    require types/schema[t] is :enum .{ values } else crash("${types/name[t]} is not an enum")
    let names = values.map(fn v. v.name)
    let cb = code-builder/new()
    cb.line(`impl from-string[e = string] for ${types/name[t]} {`)
    cb.indent()
    cb.line(`fn from-string(s: string): result[${types/name[t]}, string] {`)
    cb.indent()
    for x in values { cb.line(`if s == ${meta/str-lit(x.name)} return :ok :${x.name}`) }
    cb.line(`:err("expected one of $names, got '\$s'")`)
    cb.dedent()
    cb.line("}")
    cb.dedent()
    cb.line("}")
    let generated = cb.build()
    k1/emit-compiler-message(k1/location(), :info, generated.text())
    generated
  }
```

```k1
$pre/derive-from-string[color]()
$pre/derive-from-string[level]()
```

For `level` it emits:

```text
[reflect_enum.k1:40 info] impl from-string[e = string] for level {
  fn from-string(s: string): result[level, string] {
    if s == "debug" return :ok :debug
    if s == "info" return :ok :info
    if s == "warn" return :ok :warn
    if s == "error" return :ok :error
    :err("expected one of [debug,info,warn,error], got '$s'")
  }
}
```

Both enums now parse like `u64` does:

```k1
  let parsed: result[color, string] = from-string/from-string("blue")
  println("$parsed")
  println("${from-string@(color)/from-string("purple")}")
  println("${from-string@(level)/from-string("warn")}")
  let bad: result[level, string] = from-string/from-string("fatal")
  println("$bad")
```

```text
:ok(blue)
:err(expected one of [red,green,blue], got 'purple')
:ok(warn)
:err(expected one of [debug,info,warn,error], got 'fatal')
```

P2996 needs `template for`, `^^E` and `[:e:]` splices for this. K1 has no
splice operator: a variant name is a `string`, and `:${x.name}` is
interpolation.

## pahole in a screenful

Schemas carry full, precise layout information too. Here's padding detection:

```k1
  fn dump[t]() {
    let is-union = types/schema[t] is :union _
    let fields = if types/schema[t] is {
      :struct s -> s.fields,
      :union u -> u.fields,
      _ -> crash("${types/name[t]} has no fields"),
    }
    let stride = types/stride[t]
    println("${types/name[t]}: size ${types/size[t]}, stride $stride, align ${types/align[t]}")
    let end: size = 0
    let holes: size = 0
    for f in fields {
      if f.offset > end {
        println("        ~~ ${f.offset - end} byte hole")
        holes = holes + f.offset - end
      }
      let ty = f.type-id
      let cell = padded("${f.name}: ${ty.name()}", 24)
      println("  ${padded("${f.offset}", 4)}$cell size ${ty.size()}, stride ${ty.stride()}, align ${ty.align()}")
      if is-union {
        if ty.stride() > end { end = ty.stride() }
      } else {
        end = f.offset + ty.stride()
      }
    }
    if stride > end {
      println("        ~~ ${stride - end} bytes tail padding")
      holes = holes + stride - end
    }
    println("  $holes of $stride bytes are padding\n")
  }
```

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

`size` is the bytes a value occupies; `stride` is C's `sizeof`.

You can even issue compiler warnings, at the caller's line:

```k1
  fn warn-padding[t](context locn: k1/source-location)() {
    require types/schema[t] is :struct .{ fields } else crash("${types/name[t]} is not a struct")
    let end: size = 0
    for f in fields {
      if f.offset > end {
        k1/emit-compiler-message(locn, :warn, "${types/name[t]}: ${f.offset - end} byte hole before '${f.name}'")
      }
      end = f.offset + f.type-id.stride()
    }
    if types/stride[t] > end {
      k1/emit-compiler-message(locn, :warn, "${types/name[t]}: ${types/stride[t] - end} bytes of tail padding")
    }
  }
```

```k1
  #static layout/warn-padding[tidy]()
  #static layout/warn-padding[sloppy]()
```

`tidy` passes; `sloppy` warns during compilation:

```text
[reflect_layout.k1:71 warn] sloppy: 7 byte hole before 'id'
[reflect_layout.k1:71 warn] sloppy: 2 byte hole before 'count'
[reflect_layout.k1:71 warn] sloppy: 7 bytes of tail padding
```

Or a hard guard, `types/assert-layout`, a plain library function:

```k1
  #static types/assert-layout[tidy](16, 8, [0, 8, 12, 14, 15])
  #static types/assert-layout[wire](16, 1, [0, 1, 9, 11, 15])
```

Get it wrong:

```k1
type header = packed { tag: u8, len: u32 }

fn main(): i32 {
  #static types/assert-layout[header](8, 4, [0, 4])
  0
}
```

The VM's stack trace leads back to `main`:

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
[00] core/platform/process/exit platform.k1:201
[01] core/sys/exit builtin.k1:964
[02] core/crash builtin.k1:1171
[03] core/assert-equals core.k1:8
[04] core/types/assert-layout types.k1:43
[05] expr reflect_layout_fail.k1:4 reflect_layout_fail.k1:4

└────────────────────────────────────────╴
Module reflect_layout_fail failed compiling with 1 errors
```

## Generics and functions are types too

A bare generic name is a type, and an instance knows its parent and arguments.

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

```k1
  println(describe[list[int]]())
  println(describe[pair[string, ?u8]]())
  println(describe[result[int, string]]())
  println(describe[int]())
  println(describe[*pair[int, int]]())
  println(types/id[list].name())
  println("${types/id[list[int]].is-instance-of(types/id[list])} ${types/id[list[int]].is-instance-of(types/id[pair])}")
```

```text
list[i64] = list[t] applied to [i64]
pair[string, opt[u8]] = pair[a, b] applied to [string, opt[u8]]
result[i64, string] = result[t, e] applied to [i64, string]
i64 is not a generic instance
*pair[i64, i64] is not a generic instance
list[t]
true false
```

That query can gate a generic, via a predicate in the `pre` namespace:

```k1
ns pre {
  fn is-list[t](): bool { types/id[t].is-instance-of(types/id[list]) }
}
```

```k1
fn element-type[t: pred pre/is-list](): string { types/instance-info[t].!.args.[0].name() }
```

`test-compile` returns the rejection as a string:

```k1
  println(element-type[list[string]]())
  let rejected = test-compile(element-type[int]())
  println(rejected.!)
```

```text
string
Predicate 'pre/is-list' failed on type i64. Therefore, cannot call function 'element-type' with given types: i64
```

Functions reflect too, down to parameter names:

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
bool
string
  input: string
  count: u32
  -> bool
fn(input: string, count: u32) -> bool
```

## Elsewhere

- C++26 P2996: `^^T`, `[: r :]` splices, `template for`, a consteval-only
  `std::meta::info`. The closest design; K1's reflection value is a plain
  `either` and codegen is string interpolation.
- Zig: `@typeInfo` is the same idea as `type-schema`. K1's `#meta` can also
  emit new declarations.
- Rust: no reflection; proc macros see one item's tokens.
