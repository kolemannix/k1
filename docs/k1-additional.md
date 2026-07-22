# K1 Additional Feature Map

This is a companion to `docs/k1-syntax-basics.md`. It lists language surfaces
that are exercised in `test_src` but are not yet fully explained in the basics
guide. Use it as a scan checklist before editing K1 code or expanding the docs.

## Context Parameters

K1 supports context parameter lists before ordinary parameters:

```rust
fn add-tracked(context hist: *mut list[string])(x: int, y: int): int {
  hist.push("{x} + {y}")
  x + y
}
```

Callers can pass context arguments explicitly:

```rust
add-tracked(context history)(1, 2)
```

or bind them locally for implicit passing:

```rust
let context history: *mut list[string] = core/mem/new([])
add-tracked(1, 2)
```

See `test_src/suite1/context_params.k1` and
`test_src/suite1/context_generic.k1`.

## Module Manifests And FFI

Executable modules can declare `MODULE_INFO` with dependencies, threading mode,
libraries, and linker args. FFI declarations use `extern("lib", "symbol")`.

```rust
let MODULE_INFO: k1/module-manifest = .{
  kind = :executable,
  deps = [],
  multithreading = false,
  libs = [.{ name = "foo", link-type = :static }],
  link-args = [],
}

extern("foo", "very_small")
fn very-small(x: very-small, y: very-small): very-small
```

See `test_src/ffi_abi_test/main.k1` and `test_src/threads.k1`.

## Function Pointers

Function pointer types are written with `*fn(...) -> ...`. Function names can be
used as values, stored in structs, reassigned, and called through the pointer.

```rust
fn add-one(x: int): int { x + 1 }

let f: *fn(int) -> int = add-one
assert-equals(f(10), 11)

type has-fn = { callback: *fn(x: int) -> int }
```

Null function pointers are represented via `ptr/null`.

See `test_src/suite1/function_pointer.k1`.

## Globals And Mutable Globals

Top-level `let` declarations create globals. Add `mutable` for globals that can
be assigned with `=` like any other place; immutable globals reject assignment.

```rust
let answer: int = 42
let mutable counter: int = 0
let mutable tls my-thread-local: i32 = 0

counter = counter + 1
```

See `test_src/suite1/globals.k1`, `test_src/suite1/ability_default_fns.k1`, and
`test_src/threads.k1`.

## Static Parameters And Literal Types

Static values can participate in types and generic arguments:

```rust
fn add-static[i: static int](x: int): int {
  let value: int = core/meta/static-type-to-value[_, i]()
  value + x
}

let three: static int = 3
add-static[type-of(three)](3)
add-static[3](3)
```

Literal types also appear directly:

```rust
let c: 'c' = 'c'
let pi: 3.14 = 3.14
let hello: "hello" = "hello"
```

Static values can be converted back to runtime values with `.from-static()`.

See `test_src/suite1/static_parameter.k1`,
`test_src/suite1/static_type_literals.k1`, and
`test_src/suite1/static_run.k1`.

## Metaprogramming

K1 supports source-generating metaprogramming with `#meta`. A metaprogram may
yield `code` — text plus a table of source spans — or a plain `string`, which
simply carries no source information. The compiler compiles what the
metaprogram returns in place of the invocation; errors inside compiled output
whose bytes came from a `code` span point back at the original source.

```rust
#meta "let n: u64 = 255"

#meta {
  let name = "point"
  "type $name = { x: int, y: int }"
}
```

The `pre` namespace is compiled early for metaprogramming support:

```rust
#meta pre/make-point("my-point")

ns pre {
  fn make-point(name: string): string {
    "type $name = { x: int, y: int }"
  }
}
```

Braces are plain characters in strings, so generated code reads like K1;
interpolation holes are `$ident`/`${expr}`, and a generated string that should
itself interpolate at runtime escapes its holes as `\$`. `code-writer` helpers
also appear in metaprograms.

### `code` and `code-builder`

`core/code.k1` defines `code = { chunks: list[code-chunk] }`, where each
`code-chunk = { text, source }` is a run of text plus the original span it came
from (`source` is a compiler span id widened to u64; 0 means no source).
Final text and byte offsets only materialize at the compile boundary, so
composing `code` values never copies or re-offsets text. `\ <stmt>` (or
`#code <stmt>`) produces a `code` value: one chunk holding the statement's
source text and span.

A string template in `code`-expected position elaborates as `code`: each
literal part becomes a chunk carrying its own span, a `code` part in a hole
appends its chunks, and every other part is printed as bytes into a sourceless
chunk. This applies to any string literal (double-quoted or backtick, holes
or not) whose expected type is `code` — a macro body's return position, a
`code`-typed let or parameter, and so on. Only literals elaborate; a
string-typed *value* in `code` position is still a type error, since a literal
carries its own spans while a value has none. `.fmt(values)` on a template
works here too: with expected type `code` it yields `code`. When no context
supplies the type, ascribe it: `` `1 + 2`: code `` or
`"let w = ${}".fmt(42): code`.

Building imperatively works by writing to a code-builder (`cb.write(...)`,
`cb.writeln(...)`) — the same hole rules apply; `cb.code(c)` appends another
`code` value's chunks, and `cb.build()` yields the `code`. `code/from-string(s)`
is the explicit sourceless wrap.

`code` does not implement `print`: interpolating one into an ordinary string is
an error, since it would drop the source info silently — `.text()` is the
explicit escape. There is no implicit conversion between `string` and `code` in
either direction.

## Macros

A `macro` is a compile-time function whose call sites pass code by source: bare
params receive the argument as a `code` value (source text plus its span),
annotated params are statically evaluated (any type annotation on a macro param
implies compile-time), and the returned `code` is compiled in place of the
call. The return type is implicit, and is `code`, so a string template in
return position elaborates as `code` with its spans intact. Callers write
plain expressions:

```rust
macro debug(e) {
  #if build-debug "log/debug($e)" else ""
}

macro repeat(n: int, body) {
  let cb = code-builder/new()
  for 0.until(n) {
    cb.code(body)
    cb.writeln("")
  }
  cb.build()
}

debug(expensive-report())   // expands to nothing when build-debug is off
repeat(3, counter.incr())
```

Because arguments arrive as `code`, a type error inside the expansion points at
the argument expression the caller wrote, not at the generated file. Error
spans in emitted bytes not covered by any span (e.g. text from
`code/from-string`) stay in the generated file, with a note naming the call the
code was compiled in place of.

At definition level, macros are invoked with the `$` sentinel — the callee must
be a macro (`$` binds tightly: no whitespace before the name), and its output is
compiled as definitions in place:

```rust
$pre/make-point(my-point)

ns pre {
  macro make-point(name) {
    "type $name = { x: i32, y: i32 }"
  }
}
```

Macros expand when the call is typechecked, so a macro must be defined in the
same module (any namespace) or an upstream module; a definition-level `$`
invocation runs early, so its macro must live in the module's `pre` namespace or
an upstream module. Self-recursive expansion is an error. Macros cannot be used
as values, and unused macro params get no warning (conditionally ignoring an
argument is the point of a debug macro).

See `test_src/suite1/test_macro.k1`.

See `test_src/suite1/test_meta.k1`, `test_src/suite1/meta_defns.k1`, and
`test_src/stdlib/metaprintf.k1`.

## Conditional Compilation

`#if` conditionally includes or evaluates code at compile time. It can be used
as a statement or expression, and supports `else`.

```rust
#if feature-enabled {
  run-enabled-code()
} else {
  run-disabled-code()
}

let value = #if false "foo" else "bar"
```

See `test_src/suite1/condcomp.k1` and `test_src/suite1/static_run.k1`.

## Type Reflection

The `types` APIs expose type ids, names, layout, and schema information:

```rust
types/type-id[int]
types/type-id-name(types/type-id[int])
types/schema[array[i32, 10]]
types/type-id-schema(type-id)
types/size[string]
types/stride[string]
types/align[string]
```

Function parameter and return member types can be addressed through type member
paths:

```rust
types/type-id[do-it.return]()
types/type-id[do-it.arg-name]()
```

Schemas are pattern matched as sums such as `:struct`, `:reference`, `:either`,
`:array`, `:function`, and `:union`.

See `test_src/suite1/test_type_schema.k1`,
`test_src/suite1/type_info.k1`, and `test_src/suite1/array_type_test.k1`.

## Predicate Bounds And Richer Constraints

Generic bounds can use abilities, `where` clauses, predicates, and compound
constraints.

```rust
fn add[t](a: t, b: t): i32 where t: num, t: show {
  a.num() + b.num()
}

fn is-int[t](): bool {
  types/schema[t] is :int(_)
}

type int-wrapper[t: pred is-int] = { value: t }
```

Ability parameters can also be constrained by predicates:

```rust
ability to-scalar[out: pred is-scalar] {
  fn to-scalar(self): out
}
```

See `test_src/suite1/ability_constraint.k1` and
`test_src/suite1/type_predicate.k1`.

## Advanced Abilities

The tests cover several ability features beyond the basics:

- Default ability methods.
- Impl methods overriding defaults.
- Blanket impls such as `impl[t: increment] incr-four for t`.
- Ability parameters with named assignments like `as-pair[aa=a, bb=b]`.
- Compound bounds such as `t: as-pair[aa=x, bb=y] and as-pair[aa=y, bb=x]`.
- Multiple abilities exposing methods with the same name and different
  signatures.

See `test_src/suite1/ability_default_fns.k1`,
`test_src/suite1/ability_complex.k1`,
`test_src/suite1/ability_overload_ish.k1`, and
`test_src/suite1/ability_generic.k1`.

## Opaque, Union, Uninit, Zeroed, And Bitcast

Opaque types carry explicit size and alignment:

```rust
type blob = opaque[16, 4]
```

Unions use overlapping fields:

```rust
type bits = union { as-u32: u32, as-f32: f32 }
```

`uninit` creates an uninitialized value. `zeroed()` creates a zeroed value.
`mem/bitcast` reinterprets bits between same-sized representations.

See `test_src/suite1/opaque_type.k1`, `test_src/suite1/union_basic.k1`,
`test_src/suite1/test_uninit.k1`, and `test_src/suite1/zero_test.k1`.

## Type-Level Struct Transforms

The tests use structural type combinators:

```rust
type text = _struct_combine[{ text: string }, positioned]
type user-view = _struct_remove[user, { password: string }]
```

See `test_src/suite1/struct_composition.k1`.

## Pattern Matching Extras

Beyond ordinary struct and sum patterns, tests use:

- Literal patterns for ints, floats, strings, chars, booleans, and `.{}`.
- Wildcard `_` and binding patterns.
- Type patterns such as `type[int](x)` and `type[string](s)`.
- `or` patterns in match arms.
- Guards with `if`.
- Reference-depth patterns such as `true***`.
- Reference-through payload patterns such as `:some(value-ref)*`.

See `test_src/suite1/is_patterns.k1`,
`test_src/suite1/type_pattern.k1`,
`test_src/suite1/matching_if.k1`, and
`test_src/suite1/match_references.k1`.

## Operators And Literals

The basics guide does not yet enumerate operators. Tests cover:

- Arithmetic: `+`, `-`, `*`, `/`, `%`.
- Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`.
- Boolean: `not`, `and`, `or`, with short-circuiting.
- Bitwise: `&`, `|`, `^`, `<<`, `>>`.
- Hex, binary, and underscore numeric literals.
- Integer suffixes such as `255u8`, `-128i8`, and `3u64`.

See `test_src/suite1/arith.k1`, `test_src/suite1/bits_test.k1`, and
`test_src/suite1/bool.k1`.

## Named Arguments

Calls can provide arguments by name and out of order:

```rust
add(y = 2, x = 1)
get-nothing(_t = pair)
```

See `test_src/suite1/arith.k1` and `test_src/suite1/ability_complex.k1`.

## Namespace Behavior

Namespaces can be reopened in multiple blocks, and `ns for type` extensions can
be added from those blocks. Nested namespaces and aliases participate in normal
scope resolution.

```rust
ns foo { type bar = { value: int } }
ns foo { fn make(): bar { .{ value = 1 } } }
```

See `test_src/suite1/ns_extend.k1`,
`test_src/suite1/ns_collision.k1`, and `test_src/suite1/scopes.k1`.

## Pointer And Address Syntax

The guide covers field references, but tests also use:

- `&x` / `&array` to take addresses.
- `ptr/null`.
- `.as[ptr]` and `.as[*t]` casts.
- `.r[t]` to read a value from a raw pointer.
- Multi-level references such as `***` in patterns.

See `test_src/suite1/pointer.k1`,
`test_src/suite1/recurse_basic.k1`, and
`test_src/suite1/match_references.k1`.

## String And Char Details

Tests cover string and char escapes:

```rust
'\n'
'\0'
'\t'
'\r'
'\''
'"'
'\\'
```

String tests also cover `\n`, `\0`, `\t`, `\r`, `\"`, `\\`, and `\$`, plus
nested interpolation and backtick strings.

See `test_src/suite1/char_test.k1`,
`test_src/suite1/string_test.k1`, and
`test_src/suite1/string_interp.k1`.

## RVO And Returned Locals

`let returned name = ...` appears in RVO tests and looks like a dedicated return
value optimization or return-slot feature.

```rust
let returned v = zeroed()
```

See `test_src/suite1/rvo_test.k1`.

## Threads And Runtime Module Behavior

Thread tests combine several runtime features:

- `MODULE_INFO` with `multithreading: true`.
- Thread-local mutable globals via `let mutable tls`.
- `std/thread/start` and `std/thread/join`.
- Arena-backed allocation for cross-thread values.

See `test_src/threads.k1`.

## Atomics

Atomics are operations, not types: `ns atomic` in `k1lib/core/atomic.k1`
exposes intrinsics over ordinary memory through references.

```
let x = mem/new(41u64)
atomic/fetch-add(x, 1, :seq-cst)      // returns the previous value
atomic/load(x, :acquire)
atomic/store(x, 10, :release)
let r = atomic/cmpxchg(x, 10, 20, :seq-cst, :seq-cst)  // { prev, ok }
atomic/fence(:seq-cst)
```

The full set: `load`, `store`, `xchg`, `fetch-add/sub/and/or/xor/min/max`,
`cmpxchg`, `cmpxchg-weak`, `fence`. Element types are integer-sized scalars;
the non-arithmetic ops also take `ptr`. Orderings
(`:relaxed`/`:acquire`/`:release`/`:acq-rel`/`:seq-cst`) must be literals at
the call site — to dispatch over a runtime ordering, match on it, as
`atomic/cell[t]` (the plain-K1 wrapper type in the same file) does.

See `test_src/suite1/atomics.k1` and `test_src/atomic_threads.k1`.

