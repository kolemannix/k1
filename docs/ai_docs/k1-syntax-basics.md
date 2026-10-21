LLM-maintained K1 notes; fix wrong lines on sight. Companion: `k1-additional.md`
(comptime, macros, reflection, modules/FFI, layout, stdlib). Executable reference:
`test_src/suite1/*.k1`, one file per feature, named for it; newer tests win.

# K1 Basics

## Lexing and statements
- kebab-case for every name (ns, fn, type, field, variant, local); SCREAMING_SNAKE
  constants. Identifiers are ASCII letters, digits, `-`, `_`. Fn-pointer fields and
  params end in `-fn` (`run-fn`); a callback that runs whether or not the work ran
  is `finalize-fn`.
- Newline ends a statement; `;` only separates statements on one line.
- A line continues the previous expression only if it starts with a binary operator
  (`and or || == + ?`, comparisons...), `is`, `else`, or `.name` (leading-dot
  chains). `-`, `(`, `[` and string literals start a new statement: leave the
  operator at the end of the previous line instead. Juxtaposed string concat is
  same-line only.
- `f(x)` / `f[int]` call or apply type args only when adjacent: `if c (x)` is a
  condition then `(x)`. Type and value args may name their param,
  `pair[b = string, a = int]`, `add(3, y = 2)`: a named arg binds the param it
  names, an unnamed one its position (a method's receiver is the first non-context
  arg), each param once. `_` leaves a call's type param to inference. An ability
  call, including `ability@(t)/f(...)`, binds by the ability fn's names, its type
  args to the fn's own params (not Self); an impl fn declares as many own type
  params and the same context params, matched by position, so its names don't apply.
- Payloads never cross a newline: `return`, `break`, `:variant` at EOL are bare.
- No unary minus. `-5` is a literal. A `-` spaced before but not after is a sign:
  `if c -1 else 1` yields -1, `x -1` is a parse error, `x - 1` subtracts. Negate
  values with `x.negated()` (`neg`: wrapping on ints, sign flip on floats, `not` on bool).
- `not` is bool-only and binds one postfix chain; a binary op right after its
  operand is a parse error: `(not a) == b`, `not (a == b)`, `xs.contains(x).negated()`.
- Precedence, tightest first: `||` (pipe), `* / %`, `+ -`, `<< >>`, `&`, `^`, `|`,
  `< <= > >=`, `== !=`, `and`, `or`, `?`. Bitwise beats comparison:
  `a & b == c` is `(a & b) == c`. `and`/`or` short-circuit.
- `==`/`!=` are the `equals` ability (`not-equals` defaults to `not equals`).
- Ints: `+ - *` wrap, shift counts are `u32` taken mod width, `/ %` crash on 0
  and MIN/-1.
  Identical in the VM and native, every opt level; checked math is library code.
- Literals: `0xff 0o755 0b1010 1_000 255u8 -3i8 10: u8 'c' '\n' "s" .{} .0`.

## Files, namespaces, use
```k1
ns geom                          // whole file; or ns geom { ... }, reopenable anywhere
ns a/b { }                       // = ns a { ns b { } }
geom/area(r)  core/types/id[int] // paths use /
use core/types/id                // also inside fn bodies
use std/libc/files/SEEK_END as seek-end
ns for point { fn len(self): int { self.x + self.y } }   // methods
```
Inside `ns for t` and `impl` blocks `self` is a type name: `self`, `*self`, `self[t]`.

## Functions
```k1
fn add(x: int, y: int): int { x + y }   // tail expr is the value; `return e`, bare `return` = unit
fn first[t](xs: span[t]): t { xs.[0] }  // f[int, _](v): _ is inferred
fn advance(*atlas-cursor, n: int)       // bare-type param: named `atlas-cursor`
```
A bare-type param (or struct field) is named after the type's base ident, dropping
`*`, `?`, type args and ns path (`core/code` -> `code`). Anonymous structs, fn types
and `some t` need `name: t`.

## Values, places, references
- Blocks are expressions. Unit type `{}`, value `.{}`; `{}` as a body yields `.{}`.
- `:` means type, `=` means value: struct type `{ x: int, *cursor }`, value
  `.{ x = 1, y }` (shorthand from a local). Bare `{` in expr position is a block.
  `base | .{ a = 2 }` copies `base` with fields replaced (base evaluates first).
- `type p = ...` is nominal (struct, `either`, `opaque`, an instance like `pr[int, bool]`);
  `type(alias) n = t` is not.
- Bare sigil literals infer from the expected type; a type path written flush
  before the sigil nominates: `.0`/`t.0` (zero), `.{ }`/`t.{ }`, `:v`/`t:v`/
  `opt[int]:some 42` (type args go on the type, never after the variant).
  `x: t` ascription is for non-path types. All allocation is zeroed.
- `uninit` is not an expression: only a `let` initializer or struct-literal field
  value.
- Every local is assignable (`let`, no `mut`). `let(mutable)` is for globals.
- `=` stores to any place: `a.f`, `a.[i]`, `r.*`. A bare var on the left rebinds it;
  store through a reference with `r.* = v`. `.*` derefs, `.&` takes a place's address.
  `a.f` auto-derefs; `p-ref.x.&` is `*int`.
- `*t` is never null, aligned, dereferenceable while live; optimized builds
  speculate loads. Holding one across its memory's free (arena reset, list growth)
  is UB even if never read. Nullable: `optref[t]`. Raw: `ptr`.
- `a.[k]` = `a.index-ref(k).*` (`index` ability), a place. Arrays are intrinsic.
  Map `.[k]` crashes on a missing key: `map.get(k)` / `index-opt` are optional.
  Implement `index` for `*t` when elements live inline in `t`. `as-span` does not
  imply `index`.
- Dropping a `try`-type result (`opt`, `result`) warns; `let _ = e` discards it
  (and turns a tail expression into `empty`).

## Scalars and conversions
`bool char string int i8 i16 i32 i64 u8 u16 u32 u64 size f32 f64 ptr`.
- `x.widen[i64]` lossless (implicit at arg/let/return coercions), `x.trunc[u8]`
  modular, `.signed()`/`.unsigned()` same-width flip. zext:
  `x.unsigned().widen[u64]`; sext: `x.widen[i64].unsigned()`.
- `.as[t]`: int<->float, and total views behind references (`*t` -> `ptr`,
  `*big` -> `*prefix-struct`). Claiming views (`*u8` -> `*bool`, `*u32` -> `*enum`,
  `ptr` -> `*t`) are `.narrow()`. Both take the target from the expected type:
  `let r: *f32 = u.&.as()`.
- `c.as-u8()` (char), `b.as-u8()` (bool), `mem/bitcast[a, b](v)` same-size bits.
- ptr: `to-int() to-uint() to-size()`, `n.to-ptr()`, `p.ref[t]` (null-checked in
  debug and comptime), `p.ref-opt[t]`, `p.ref-at[t](i)` (gep), `ptr/null` or `.0`.
  `p is null` is a pattern on `ptr` only (a type error on `*t`); parenthesize
  before an enum-literal consequent: `if (p is null) :none else ...`. Never
  `ptr/null.ref[t]`.

## Enums and sums
```k1
type color = either(u8) { red, green, blue }        // tag type optional
type status = either { ok = 10, missing = 20 }
type res[t] = either { ok(t), err(string) }
type ev = either(u32, partial) { quit = 256 }       // open C enum
let c: color = :red; color:green; :ok(42); :ok 42
```
- Quiet payload (`:ok 42`): same line, starting with an ident, literal, list,
  nonempty struct literal or `:variant`; binds one postfix expr (`:ok f(x).y`);
  binary ops take the whole variant (`:ok 1 == x` is `(:ok 1) == x`). Otherwise
  parenthesize.
- An `empty` payload may be elided: `:ok` constructs and matches `:ok(.{})`.
- A value outside the declared tags is UB, zeroed memory included. `partial`
  (payload-less only, tag type required; bindgen emits it) makes matches need `_`
  and prints undeclared values as `ev(8192)`.
- `.value` / `.enum-value()`, `.tag`, `.as-ok(): ?t`, `.enum-name()`, `.sum-name()`,
  `enum/enum-from-value(v): ?e` (impl picked from the expected type),
  `enum-from-value-unchecked`, `types/enum@(e)/members()`, `sum.tag-enum` (type of
  the tags).

## Optional, result
`?t` with `:some x` / `:none` (or `some(x)`). `.!` unwraps or crashes, `o ? d`
falls back, `.try` propagates through the `try` ability. `result[t, e]` is
`:ok`/`:err`. Most special-looking syntax is an ability; look for it before
assuming an intrinsic.

## Matching
```k1
if v is :some(x) and x > 0 { }           // `is` is a bool expr; binds at the top of an `and` chain, `or` breaks binding
if p is { .{ x = 0, y } -> y, .{ x, y } if x == y -> x, :a or :b -> 1, _ -> 0 }   // exhaustive match expr
require v is :some(x) else crash("none") // x in scope after; else must diverge
if r is :ok(v)* { v.* = 100 }            // trailing * binds a payload reference when matching through *sum
```
Patterns: literals (int, float, string, char, bool, `.{}`), `_`, bindings, struct,
sum, `type[int](x)`, ref depth `true***`. `#if x is { ... }` typechecks only the
chosen arm.

## Loops and defer
- `while c { }`; `loop { }` (`while true` is an error); `break v`, bare `break`,
  `continue`. A `loop` without `break` is `never`.
- `for x in xs { }` or `for xs { }` (element named `it`); `it-index` is always bound.
- `0.until(10)` is the half-open `range[t]` (any `add + scalar-cmp + one` type).
- Labels: `loop @find { for i in xs { if i > 10 break @find :some i } break :none }`;
  `continue @l` too. `#for` unrolls and takes no label.
- `defer e` runs on any scope exit (return, break, continue, `.try`), LIFO,
  typechecked where written. The exit value is computed first, so a defer changes
  it only through a `let(returned)` var. `defer r. if r is :err _ unlock()` binds
  the returned value (fn top level only). A defer body cannot exit.

## Collections
`array[t, n]`, `list[t]`, `buffer[t]`, `span[t]`, `fixlist[t, n]`, `spill-list[t, n]`,
`std/hash` `map`/`set`. `[a, b]` literals build arrays and lists.
- `wrap-*` constructors are zero-copy views (dangerous); `from-*` copy. `as-*`
  views, `to-*` owned copies.
- Bare ops allocate in the ambient arena (`mem/current-arena()`, TLS stack,
  default tmp; scope with `mem/with-arena(a, expr)`). `-in` variants take any
  `allocator` (`*arena`, `heap`) after self: `push-in`, `cloned-in`. Long-lived
  data takes an explicit allocator.
- Arenas: `reset`/`pop-to-mark` re-zero what they release (O(bytes used)), `free`
  is a no-op. `arena/init(size)` chains regions; `init-from-buffer(zeroed bytes)`
  is fixed and crashes when full. `arena/handle[t]`, `span-handle[t]`: u32 offsets
  into the first region (`.0` is nil): `handle-from`, `push-handle`, `get(h)`.
- `std/pool`: `pool[t]` fixed slots, `pool/handle[t]`, slot 0 a zeroed nil: `get`
  on a dead handle returns nil (crashes if `t` holds `*t`), full `acquire` returns
  `.0`; check `is-live`.
- `x.non-empty()`, never `not x.is-empty()` or `x.len() > 0`.
- Mutators take `*self` (`sort`, `reverse`); copies end `-ed` (`sorted`).
- Shared ops are ability defaults: reads on `as-span` (`len first last slice take
  drop starts-with chunks sorted index-of-span contains-span index-opt`),
  mutation on `as-buffer` (`swap sort reverse fill first-ref ref-iter`;
  `as-buffer` derives `as-span`). View types shadow them with shape-preserving
  inherents (string ops return string). Each method name belongs to one ability.
- Element search is `position`/`contains` (any iterable, SIMD for int/char/bool
  contiguous data); subsequence search is `index-of-span`/`contains-span`.

## Lambdas and fn values
```k1
fn(x: int) x + 1    fn x. x + 1    fn. { work() }
fn[base, counter.&](x: int) { counter.* = counter.* + base + x }
```
- Plain lambdas capture nothing (error). `[name]` copies at creation, `[name.&]`
  captures the address (body sees a reference). Nested lambdas re-list captures.
- Param types: `some fn int -> int` (specialized, default choice), `dyn[fn() -> {}]`
  (dynamic object), `*fn(int) -> int` (raw pointer: `add-one.&`; null is `ptr/null`).
- Param names belong to declarations, not fn types (`*fn(x: int)` is `*fn(int)`):
  named args need a declared fn, lambda or ability fn; calls through `*fn`, `dyn`
  and `some fn` values are positional.
- Any callable expr calls: `(s.callback)(x)` (`s.callback(x)` is a method lookup),
  `(make-adder(2))(3)`. A named fn's value has the ZST `type-of(f)`.
- `xs || map(fn x. x + 1) || filter(fn x. x % 2 == 0)` pipes.

## Abilities
```k1
ability show-id { fn show-id(self): string }
impl show-id for user { fn show-id(self): string { "user-${self.id}" } }
impl[t: increment] incr-four for t { }                 // blanket
fn f[t: print and equals](x: t)   fn g[t](a: t) where t: num, t: to-string
t: as-pair[x, bb = y]                                  // ability args bind like type args
print/print-to(v, w)   has-secret[a = int]@(foo)/secret()   // qualified / impl-selected
```
- Default methods, overridable. `where` on an ability fn gates it per impl; impls
  don't restate it. Overlapping impls resolve by which one makes the call typecheck.
- `pred` bounds: `type w[t: pred is-int]`, `ability a[out: pred is-scalar]`; the
  predicate fn must live in `ns pre` or a dependency, and its own constraints on its
  type param must hold (an error, not `false`). Impls are declared before
  type bodies, so constrained generic fields find them; an impl for the type being
  defined still can't satisfy that type's own constraints.

## Strings and formatting
- `"hi $name ${expr}"` interpolates into the ambient arena. `$ident` is greedy
  through kebab (`"${n}-th"`); `$` before a non-ident is literal; `\$` escapes.
  Braces are plain characters.
- Spec: `${v:[[fill]align][0][width][,][.precision]}`; `${x: f64}` (space) is an
  ascription. `${3.14159:.2}` "3.14", `${-42:06}` "-00042", `${7:x>3}` "xx7",
  `${1234567:,}` "1,234,567". Precision needs `print-precise` (rejected on ints).
- `"${}".fmt(42)` (bare hole takes the argument), `"$y-$m".fmt(.{ y = 2026, m = 6 })`
  (named holes read its fields, else locals; all-local templates need no `.fmt`). `w.write("x ${}", 42)` / `w.writeln` on any `writer`; a non-template
  argument is a plain write. Templates must be literals.
- Backticks: multi-line, unescaped `"`, still interpolate and escape. Content
  starting on the next line is a block: leading newline dropped, common indent
  (closing backtick's line included) stripped, whitespace-only last line removed.
- Escapes: `\n \0 \t \r \\ \" \$`, `'\''`.

## Tests
```k1
fn(test) adds() { assert-equals(1 + 1, 2) }
fn(test) checks-bounds() {
  k1/test/expect-failure("Index out of bounds: 44 >= 10")   // literal, direct statement of a test body
  let xs: array[int, 10] = .0
  let i = 44
  let _ = xs.[i]
}
```
- `k1 test` collects every `fn(test)` in the program and its submodules. No
  params; return `core/test/outcome`: `empty`, `opt` (`:none` fails), any `try`
  (error fails).
- `k1/test` is true when the compile collects tests (`k1 test`, editor
  test-on-save): `#if k1/test ns x-test { ... }` keeps tests and their helpers
  out of other builds.
- Each runs statically in the VM and at runtime in a forked child, up to
  `cpu-count` children at once, so fixed paths and ports collide; reported in order.
  `fn(test-runtime)` (FFI, threads, fork) / `fn(test-static)` opt out of one;
  `--skip-static` / `--skip-runtime` for the run. `k1 test . name` filters by
  qualified-name substring (`suite1/ns/name`).
- `expect-failure` passes only if the test fails with the needle in its output or
  exit rendering ("exited with code 3", "killed by signal 6"); in-process runtimes
  (wasi, bare) skip it.
- `test-compile(expr): ?string` is the compile error, if any. `assert`,
  `assert-equals`, `assert-not-equals`.
