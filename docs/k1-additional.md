# K1 Additional Feature Map

This is a companion to `docs/k1-syntax-basics.md`. It lists language surfaces
that are exercised in `test_src` but are not yet fully explained in the basics
guide. Use it as a scan checklist before editing K1 code or expanding the docs.

## Context Parameters

K1 supports context parameter lists before ordinary parameters:

```rust
fn add-tracked(context hist: *mut list[string])(x: int, y: int): int {
  hist.push("{x} + {y}");
  x + y
}
```

Callers can pass context arguments explicitly:

```rust
add-tracked(context history)(1, 2)
```

or bind them locally for implicit passing:

```rust
let context history: *mut list[string] = core/mem/new([]);
add-tracked(1, 2)
```

See `test_src/suite1/context_params.k1` and
`test_src/suite1/context_generic.k1`.

## Module Manifests And FFI

Executable modules can declare `MODULE_INFO` with dependencies, threading mode,
libraries, and linker args. FFI declarations use `extern("lib", "symbol")`.

```rust
let MODULE_INFO: k1/module-manifest = {
  kind: :executable,
  deps: [],
  multithreading: false,
  libs: [{ name: "foo", link-type: :static }],
  link-args: [],
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

let f: *fn(int) -> int = add-one;
assert-equals(f(10), 11);

type has-fn = { callback: *fn(x: int) -> int }
```

Null function pointers are represented via `ptr/null`.

See `test_src/suite1/function_pointer.k1`.

## Globals And Mutable Globals

Top-level `let` declarations create globals. Add `mutable` for globals that can
be changed, and use address/reference assignment syntax to mutate them.

```rust
let answer: int = 42
let mutable counter: int = 0
let mutable tls my-thread-local: i32 = 0

&counter <- counter + 1;
```

See `test_src/suite1/globals.k1`, `test_src/suite1/ability_default_fns.k1`, and
`test_src/threads.k1`.

## Static Parameters And Literal Types

Static values can participate in types and generic arguments:

```rust
fn add-static[i: static int](x: int): int {
  let value: int = core/meta/static-type-to-value[_, i]();
  value + x
}

let three: static int = 3;
add-static[type-of(three)](3);
add-static[3](3);
```

Literal types also appear directly:

```rust
let c: 'c' = 'c';
let pi: 3.14 = 3.14;
let hello: "hello" = "hello";
```

Static values can be converted back to runtime values with `.from-static()`.

See `test_src/suite1/static_parameter.k1`,
`test_src/suite1/static_type_literals.k1`, and
`test_src/suite1/static_run.k1`.

## Metaprogramming

K1 supports source-generating metaprogramming with `$`.

```rust
$ "let n: u64 = 255"

$ {
  let name = "point";
  "type {name} = {{ x: int, y: int }"
}
```

The `pre` namespace is compiled early for metaprogramming support:

```rust
$pre/make-point("my-point")

ns pre {
  fn make-point(name: string): string {
    "type {name} = {{ x: int, y: int }"
  }
}
```

`p"..."` and ``p`...` `` strings are used heavily for generated code where
literal braces are common. `code-writer` helpers also appear in metaprograms.

See `test_src/suite1/test_meta.k1`, `test_src/suite1/meta_defns.k1`, and
`test_src/stdlib/metaprintf.k1`.

## Conditional Compilation

`#if` conditionally includes or evaluates code at compile time. It can be used
as a statement or expression, and supports `else`.

```rust
#if feature-enabled {
  run-enabled-code();
} else {
  run-disabled-code();
};

let value = #if false "foo" else "bar";
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

- Literal patterns for ints, floats, strings, chars, booleans, and `{}`.
- Wildcard `_` and binding patterns.
- Type patterns such as `type[int](x)` and `type[string](s)`.
- `or` patterns in `switch` arms.
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
ns foo { fn make(): bar { { value: 1 } } }
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

String tests also cover `\n`, `\0`, `\t`, `\r`, `\"`, and `\\`, plus nested
interpolation and raw backtick strings.

See `test_src/suite1/char_test.k1`,
`test_src/suite1/string_test.k1`, and
`test_src/suite1/string_interp.k1`.

## RVO And Returned Locals

`let returned name = ...` appears in RVO tests and looks like a dedicated return
value optimization or return-slot feature.

```rust
let returned v = zeroed();
```

See `test_src/suite1/rvo_test.k1`.

## Threads And Runtime Module Behavior

Thread tests combine several runtime features:

- `MODULE_INFO` with `multithreading: true`.
- Thread-local mutable globals via `let mutable tls`.
- `std/thread/start` and `std/thread/join`.
- Arena-backed allocation for cross-thread values.

See `test_src/threads.k1`.

