# Site to-do

- dbx case study: write it up under examples (how dbx pulls SQLite into the k1
  fold at compile time: the dependency, the bindings, the schema, tables as k1
  types), then link it from the early execution section on the homepage, which
  alludes to it today without a link.
- Manifesto tenet 01 is a placeholder: write the opening tenet about the human
  wonder of computing and programming as an art, more specific than "the joy of
  programming" (overused; Odin has it). Candidates so far: programming is
  wonderful, programming is art, protecting the art of programming.
- OKLCH palette as a second live wasm demo: render the swatches the compile-time
  code produces, on the page, the way refchess runs perft.

## Learn pages, moderate

A second tier after the basics page (content/showcase/sections/00-basics.md),
one page each, same shape: a sentence, a program that runs, its output. Drawn
from a pass over typer.rs and the ai_docs; each line names what the page has to
cover.

- Casting and conversions: `: t` assertions, `.as[f64]` int/float, `.widen`,
  `.trunc`, `.signed()`/`.unsigned()`, the implicit lossless widening at
  coercion sites, `ptr.to-size()`/`to-ptr()`, `enum-from-value`, `mem/bitcast`.
- Abilities and ability constraints: default bodies, `ability@(self)/f()`,
  bounds on type params, blanket impls, `impl[t: a] b for t`, ability type
  params with `[impl t]`, `where t: a and b`, qualified calls `a/f(x)`,
  explicit impl selection when several apply.
- Functions, closures, fn pointers and dyn fns: `some fn` vs `dyn[fn]` vs
  `fn-ptr`, capture lists and `.&` captures, `.to-dyn()`, what each costs,
  context parameters, named arguments, `fn(inline)`, `fn(extern)`.
- Type-level expressions: `t.field`, `t:variant` for a payload type,
  `t.tag-enum`, `*t.value`, `type-of(f).return` and `.params`,
  `array[t, n].element`, `type(alias)`, `_struct_combine`/`_struct_remove`.
- References and places: `.&`, `.*`, `*t` never null, `optref[t]`, `ptr`,
  assignment through places, `.[i].&`, patterns that bind references
  (`:ok(v)*`, `true***`), RVO and `let(returned)`.
- Memory: arenas, `mem/with-arena`, `-in` variants, `heap`, marks and
  `pop-to-mark`, zero on reset, handles, `std/pool`, what a list owns.
- Collections: array vs buffer vs span vs list vs fixlist vs spill-list, the
  `as-span`/`as-buffer` ability defaults, `wrap-*` vs `from-*`, `as-*` vs
  `to-*`, index ability, SIMD `position`/`contains`.
- Iterators and ranges: `iterator`/`iterable`, `it`/`it-index`, `until`,
  labelled loops with values, `#for` unrolling, the `||` pipe.
- Strings and chars: span of chars, interpolation with format specs,
  juxtaposed literal concatenation, `string-builder`, `spill-string`, UTF-8
  as std, char literals and escapes.
- Errors: `result`, `.try` and the `try` ability, `?` fallback, `.!`, errno as
  an enum, `require ... else`, `defer r.` on the returned value.
- Generics: type params on types and fns, inference and `_`, `where` bounds,
  predicate bounds (`t: is-pod`), static parameters and literal types.
- Enums in depth: tag types, explicit values, `partial`, `.value`/`.tag`,
  generated helpers (`as-ok`, `enum-name`, `sum-name`), the quiet payload
  form, `empty` payload elision, exhaustiveness and guards.
- Globals and statics: `let` globals evaluated early, `let(mutable)`, a
  global's type is its value, `#static` blocks, `#if`, `test-compile`.
- Namespaces and modules: `ns`, `ns for`, `use`, `pre` blocks, `build.k1`,
  deps and `-D` params, `fn setup`, what `k1 check/build/run/test` do.
- Data layout: structs are C layout, `packed`, `union`, `opaque[n, a]`,
  `uninit`, `.0` zero, `types/size`/`align`, `types/assert-layout`.
- Operators as abilities: `add`, `equals`, `comparable` vs `scalar-cmp`,
  `index`, `print`/`show`/`writer`, `from-string`, `zero`/`one`, `neg` and
  `not`, no unary minus, negative literals.
- Control flow extras: `if` with `is` chains and bindings, `while x is`,
  `loop` with `break value`, `continue`, `never` and zero-sized types.
- Early execution for real work: `#static` calling C through libffi, reading
  files at compile time, `code` values and macros as the next step up.
