# std/json: encoder and decoder

Decided 2026-10-02. Target: encode and decode any K1 type from its schema at
compile time, decode the `json` AST the same way, and parse within 2x of
simdjson DOM (1+ GB/s on twitter.json, canada.json, citm_catalog.json).

## Dispatch

Two abilities. Concrete impls win over blanket impls in the typer, so the
blanket impl is the generator and any type can replace it:

```
ability json-write { fn write-json[w: writer, o: static opts](self, w) }
ability json-read  { fn read-json[o: static opts](r: *reader): result[self, error] }

impl[t] json-write for t { fn write-json(...) { #meta { generate from types/schema[t] } } }
impl[t] json-read  for t { fn read-json(...)  { #meta { generate from types/schema[t] } } }
impl json-write for json { hand-written tree walk }
impl json-read  for json { hand-written reader walk }
```

Generated code emits `field.write-json(w)` / `t/read-json(r)` per field, so
recursion re-enters concrete-first lookup. Free wrappers supply defaults:
`json/write[t](w, v, opts = .{})`, `json/to-string[t](v, opts = .{})`,
`json/parse[t](input: string, opts = .{}): result[t, error]`.

Schema mapping, both directions: struct -> object keyed by field name; `?t` ->
null or value, missing key reads as none; either -> `"name"` without payload,
`{"name": payload}` with; list, span, array[t, n] -> array; `*t` -> the inner
value, reads allocate in the ambient arena; ints -> integer with overflow
error; char -> one-char string; enum -> name string. Anything else: the
generator crashes naming the type.

`opts` is a static struct: `key-case: either { as-is, camel, snake }`,
`skip-none: bool`, `renames: span[{ field: string, key: string }]`. Every
combination specializes separately; no runtime branching on options. Options
are passed as a named global in type position (`v.write-json[_, my-opts](w)`);
struct literals are not type expressions. Dropped from the first proposal:
internally tagged eithers (need lookahead past other keys, which a single-pass
reader cannot do cheaply) and `indent` (pretty printing is a JSON-aware
reindent pass over compact output, `json/pretty` and `json/to-string-pretty`,
so the ability signature stays free of a depth parameter). `max-depth` arrives
with the reader.

## AST

```
type json = either {
  null, bool(bool), int(i64), uint(u64), float(f64), str(string),
  array(span[json]), object(span[{ key: string, value: json }])
}
```

Numbers follow simdjson: no fraction or exponent and fits i64 -> int; fits
u64 -> uint; else float. Containers are exact-size arena spans gathered on a
scratch stack during the parse; key order is preserved and `get(key)` is a
linear scan. Builders that mutate use `list` and convert.

## Reader

Single pass, scalar state machine, SIMD helpers; no structural index, no
padding requirement (scalar tails as in `std/simd`). Both the AST impl and
generated typed decoders are written against it:

```
type reader = { input: string, pos: size, depth: size, scratch: list[json] }
ns for reader {
  fn skip-ws, fn peek: char
  fn object-start/object-next-key: ?string/object-end, array-start/array-next/array-end
  fn read-string: string, read-i64/read-u64/read-f64, read-bool, read-null
  fn skip-value                      // unknown keys
  fn fail(kind): error
}
```

Helpers, each a SIMD loop with a scalar tail: whitespace run (`lt-lanes` <=
0x20 and eq-lanes on the four JSON blanks), string scan (first `"`, `\`, or
byte < 0x20 via eq/lt masks and ctz), digit run, literal match through
`load-word`. Integers parse from the digit run with an overflow check; floats
go through `ffc/parse_double` which is already fast_float.

Strings without escapes are spans into the caller's input; the caller keeps
the input alive. Escaped strings are unescaped into the ambient arena, with
`\uXXXX` encoded as UTF-8, surrogate pairs combined, lone surrogates an error.
Bare control bytes in strings are an error.

UTF-8: the string scan ORs every chunk into an accumulator, so a string with
no byte >= 0x80 costs nothing; one with high bytes runs the shuffle-based
nibble-lookup validator over 16-byte blocks, with the tail copied into a
zeroed block so no separate incomplete-sequence check is needed. The
validator is `std/utf8/is-valid`.

Strictness: RFC 8259 with every JSONTestSuite `n_*` rejected and `y_*`
accepted. No leading zeros, NaN, Infinity, trailing commas, or trailing
content. Depth counted per container against `max-depth`. Duplicate keys: last
wins.

Typed field matching: the generator switches on key length then byte-compares
against the field names; unknown keys `skip-value`.

## Errors

```
type error = { pos: size, kind: either {
  unexpected-char(char), unterminated-string, bad-escape, bad-number, depth,
  invalid-utf8, missing-field(string), wrong-type(expected: string),
  unknown-variant(string), trailing } }
```

`impl print` renders line and column from the input.

## Encoder

Writes to any `writer`. Strings: SIMD scan for the first `"`, `\`, or byte <
0x20, `write-bytes` the clean run, emit the escape (`\n \r \t \b \f \" \\`,
else `\u00XX`). Floats already go through ryu shortest in k1rt; ints through
`print-to` (a two-digit table is a later improvement if the bench shows it).

## Vector intrinsics

Two new `VecOpIr` kinds across typer builtins, ir, bc lowering (per-lane
loops so `#static` works) and codegen_llvm: `lt-lanes` (unsigned for unsigned
elements, signed otherwise, ordered for floats; `gt-lanes` swaps the args in
K1) and `shuffle-bytes(table, idx)` on `vector[u8, 16]` with pshufb
semantics: a lane with its top bit set yields 0, otherwise `table[idx & 15]`.
arm64 lowers to `tbl1` and wasm to `swizzle` after masking the index with
0x8F; x86 lowers to `pshufb`, so the x86 baseline cpu is `x86-64-v2`.
`shift-bytes[k](prev, cur)` is two shuffles plus a mask in K1, no intrinsic.

## Tests and benchmarks

- `test_src/stdlib/json_test.k1`: encoder, AST round trip, typed decode,
  every error kind, opts.
- `perf/json/`: twitter.json, canada.json, citm_catalog.json.
- `content/showcase/benchmarks/json/`: one K1 program using std/json beside
  simdjson (dom and ondemand) and serde_json (Value and derive), reporting
  GB/s for AST parse, typed parse of twitter and canada, and write. `just
  bench-json` runs `run.sh`, which fetches the simdjson amalgamation into
  `gen/` and builds the Rust lane with cargo.

## Replaced

`examples/logreport`'s hand-written `to-json` is a std/json call. The schema
encoders under `content/showcase` and in `test_src/suite1/test_meta.k1` stay:
they document and test `#meta` itself, not JSON. The current `parser` type and
string-typed errors are deleted with the reader.

## Phases

1. Encoder: abilities, blanket generator, `json` impl, opts, escape scan.
   Done.
2. Intrinsics. Done.
3. Reader with structured errors. Done; UTF-8 validation is still the scalar
   pass.
4. `json-read` for the AST. Done.
5. Typed decode generator. Done.
6. Corpus, conformance (316 JSONTestSuite cases pass natively and in the
   VM), showcase comparison. Done; numbers in the showcase results.
7. Performance pass: k1rt (fast_float, ryu) was built without `-O2`, which
   alone took canada parse from 0.19 to 0.8 GB/s; ryu's digit output got a
   pair table; `reduce-or` landed so chunk scans skip `to-mask` on the no-hit
   path; whitespace skipping eats eight spaces per step; literals compare
   bytes inline; digit runs are one SIMD compare; the reader caches its
   arena. UTF-8 validation is the shuffle-based validator with the scalar
   one kept as the test oracle. Results in
   `content/showcase/benchmarks/json/results.md`: canada is inside the 2x
   target, twitter and citm sit at 2.4x of simdjson DOM.

## Phase 1 findings (2026-10-02)

- Static struct params on ability functions and `#meta` blanket impl bodies
  both work; concrete impls win as designed.
- `use std/json/json` shadows the `json` namespace, so `json/to-string` then
  fails to resolve. Import the namespace only and write `json/json` for the
  type.
- Fixed in the compiler: `if x is :v return y` parsed `return` as a payload
  binding (payload-less variants carry the empty payload, so it typechecked
  and silently matched); method calls through an ability named only by path
  in a constraint (`t: json/json-write`) were invisible to resolution, which
  only consulted abilities in scope by bare name.
- `impl[t] json-write for t` makes `where t: json-write` vacuous; unsupported
  field types surface as the generator's crash naming the type.
- `load-unchecked` past the end of input: scalar tails, never padding.
