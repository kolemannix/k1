LLM-maintained K1 notes; fix wrong lines on sight. Read `k1-syntax-basics.md` first.

# K1 Advanced Surfaces

## Compile-time values
- `#static e` runs `e` in the VM. Literal types: `let a: 42 = #static 40 + 2`,
  `'c'`, `3.14`, `"hi"`. `.from-static()` returns to a runtime value.
- Baked values (`#static`, global initializers) may hold `*fn` pointers: a table
  built at compile time links in every function it points to.
- `fn f[i: static int](x: int)`, called `f[3](x)` or `f[type-of(three)](x)`
  (`let three: static int = 3`). A static param's name is its value in expressions;
  `#static`/`#meta` see only static-typed locals.
- `#if c { } else { }` as statement or expression; `#for xs { }` unrolls.
- `#if k1/platform` is the target even in static code, which runs on
  `k1/host-platform`. OS-specific code reachable from both uses
  `k1/select-os(macos = fn. ..., linux = fn. ...)`: the unselected arm never links.
- `#type e` turns a `types/type-id` into a type.

## Context params
```k1
fn add-tracked(context hist: *list[string])(x: int, y: int): int
add-tracked(context history)(1, 2)                    // explicit
let(context) history: *list[string] = mem/new([])     // or implicit from scope
fn greet[w: writer](context out: w)(name: string)
let(context(impl writer)) out: *string-builder = sb.&  // ability-keyed
```
- An ability-keyed var is the only kind that solves an ability-constrained param
  during inference, and it is authoritative (conflict with other args is an error;
  pass the context explicitly). Its declared type must implement the ability
  itself, so ref-implemented writers need a ref binding. It also registers under
  its concrete type. Several abilities: most keys hit wins; ties are ambiguous.
- A context param can't be `some fn`: its type comes from its own arg.

## Metaprogramming: `code`
- `code = { text: string, spans: list[code-span{start, len, source: u64}] }`;
  text without a span is sourceless. Errors in spanned bytes point at the original
  source; sourceless ones stay in the generated file with a note naming the call.
- A string literal (either quote, holes or not) whose expected type is `code`
  elaborates as `code`: literal parts keep spans, `code` holes splice with spans,
  other holes print sourceless. `.fmt` works too. Ascribe when nothing expects it:
  `` `1 + 2`: code ``. A string *value* is a type error in `code` position; wrap it
  with `code/from-string(s)` (no alloc). `code` has no `print`; `.text` is the escape.
- Escapes resolve before emission: `\n` emits a newline; `\\n` emits an escape.
  Emit `\$` for holes the generated code should interpolate. `meta/str-lit(s)`
  quotes any string as a K1 literal.
- `\ stmt` quotes a parsed statement as `code`; `\{ ... }` quotes a brace group
  that only has to lex (text is the inside).
- `code-builder` (one buffer, no per-part alloc): `cb.write(tmpl, ...)`/`writeln`,
  `cb.write(s)` (sourceless), `write-code(c)`, `write-token(t)`,
  `write-tokens(ts)` (restores spacing), `write-spanned(text, source)`, `line(c)`,
  `block(h, fn[cb]. ...)`, `block-close(h, close, fn[cb]. ...)`,
  `sep(items, ", ", fn[cb] x. ...)`, `indent`/`dedent`, `build()` (no copy).
  Every `code`-typed template costs its own builder: in hot emitters prefer
  `cb.writeln(...)` over `cb.line("...")`.
- Emit conditions with explicit parens (`(not a) and b`).
- No expansion dump exists. `#debug` on a macro definition prints the macro fn's
  IR/bytecode; `#debug $call(...)` does not parse.

## `#meta`, macros, `pre`
```k1
#meta "let n: u64 = 255"                 // definition-level; body yields code
#meta pre/make-point("my-point")
macro debug(e: code) { #if build-debug "log/debug($e)" else "" }   // returns code implicitly
debug(expensive())                        // expression call site
$pre/make-point(my-point)                 // definition-level macro call; `$` adjacent to name
ns pre { macro make-point(name: code) { "type $name = { x: i32, y: i32 }" } }
```
- Macro params: `code` gets the argument's source and span; `span[code-token]`
  gets it lexed; any other type is evaluated statically. Every param is annotated.
  Unused params don't warn. Macros are not values; self-recursive expansion errors.
  Hygiene is emitting fully qualified names (`http/llhttp/method`), by design: no
  definition-site resolution, and caller-side shadowing is not a caveat.
- Expression macros expand at typecheck: define them in the same module or
  upstream. Definition-level `$`/`#meta` run early: the macro or fn lives in `ns pre`
  or upstream. Output may contain more `#meta`/`$` (expanded in order) or `macro`
  defs (binds a static arg once per ns: `define-scaled` in `test_src/suite1/test_meta.k1`).
- Passing a type the caller wrote as tokens: emit a call to a second macro taking
  `types/id[{ ...tokens... }]` (`content/showcase/examples/macro_record.k1`).
- `code-token = { kind: ident|number|string|char|punct, text, source, spacing:
  none|inline|newline, parts }`; keywords are `ident`; a string literal is one
  token whose `parts` are `:text { value (cooked), source }` and
  `:hole { tokens, spec, source }`. `tok.fail(msg)` errors at the token.
  `token-cursor` (`expect`, `expect-ident`, `eat-until-close`) walks them.
- Worked examples: `std/html` (`html/write(out, \{ <p class={c}>{t}</p> })`,
  `html/render(\{ ... })`; tag structure checked at compile time; braced attr
  values: `bool` on boolean attrs emits bare/nothing, `opt` omits on `:none`;
  `render` binds a `writer` context), `std/flag-sum`, `std/string-enum`
  (`$std/string-enum/define(\{ status = either(u8) { new = "New", pending } })`
  emits the enum, `to-value`, `from-value`, `members`, `print`, `from-string`).
- `std/html/dom`: `node` tree, one fn per tag taking `span[node]` (`:attr` entries
  are attributes). Tag names collide with locals and the `div` ability: `use
  std/html/dom as h`.

## Reflection and type synthesis
- `types/id[t]` -> `type-id`: `.name() .schema() .instance-info() .size() .stride()
  .align() .info()`. `types/size[t]`/`stride`/`align` fold to constants. No-layout
  types (never, ZST, abstract) report size 0, align 1.
- Schemas are sums: `:struct` (packed included; see offsets), `:reference`,
  `:either`, `:array`, `:function`, `:union`, `:int(_)`, ...
- A bare generic name is a type: `types/id[list]` is `list[t]`; instances report it
  as parent (`types/instance-info[list[int]]`, `.is-instance-of(types/id[list])`).
- Fn member types: a fn name in type position is its `type-of(do-it)`, displayed
  `fn do-it` (schema `:named-function .{ function-type-id, param-names }`):
  `do-it.return`, `do-it.arg-name`. A fn type has `.return` and positional
  `.param-0` (spelling provisional).
- Synthesis: `types/make-struct(:struct|:union|:packed, [types/field[i64]("x")])`,
  `make-either(tag-type: ?type-id, variants)` / `make-either-default`
  (`variant-data[t]("a")`, `variant-empty("b")`), `make-reference`, `make-array`,
  `make-fn`, `make-instance`. Use with `type(alias) p = #type types/make-...`.

## dyn
`dyn[ability[params bound]]` = `{ state: ptr, fn ptr per dispatchable fn }`.
`r.to-dyn[source[t = u8]]` from `*s` (no alloc), from a value it allocates first;
`let d: dyn[...] = r.to-dyn()`. Dispatches defaults, overrides, by-value receivers
(thunk), and self-free fns (no state passed). Excluded per fn: self outside the
receiver, own type params, context params. No dispatchable fns = no dyn type. Its
state pointer means a dyn can't cross `#static` into a runtime constant.

## Layout
- `opaque[size, align]`. `union { a: u32, f: f32 }`: a literal sets at most one
  member, `.{}` is fully uninit. `packed { tag: u8, v: u64 }`: align 1, no padding;
  `.&` of a field warns. `types/assert-layout[t](size, align, [offsets])`.
- `_struct_combine[a, b]`, `_struct_remove[t, { f: x }]`.
- `let(returned) r: t = ...` names the return slot (RVO).
- Settled: by-value aggregate params are not copied and may alias caller-reachable
  storage (`f(p.*, bump(p))` sees the bump). Only the sret slot never aliases an
  argument.

## Globals and fn modifiers
- `let g = 1` immutable global; `let(mutable, tls, extern("sym")) ...`, at most one
  of `extern`/`export`, each with an optional symbol name. An `extern` global has no
  initializer and takes its library from the enclosing `ns(lib("foo"))`.
- `fn(inline)`: body copied at every direct call, all modes and the VM; needs a
  body; inline-only cycles error; rejected in reloadable namespaces.
  `fn(cold)` (diverging fns already are): never IR-inlined, LLVM `cold`.
  `fn(noinline)`: the out-of-line half of a fast path.
- `fn(extern)` / `fn(extern("sym"))` declares FFI; provider via `ns(lib("foo"))`
  (whole ns, openings must agree) or `fn(extern, lib("foo"))`.
- `fn(export)` / `fn(export("sym"))`: concrete, non-ability, with body; duplicate
  symbols error.

## Modules and build.k1
- A dir with `build.k1` (may be empty) is a module: every `.k1` under it, minus
  folders it ignores, submodule and dependency folders, folders with their own
  `build.k1`, and dot-folders. A lone file is a program; file names mean nothing.
  `build.k1` compiles on the host against core+std only. All entry points are
  optional and take their parameter only when they use it:
  - `fn build(req: k1/build-request): k1/build-config`, primary only:
    `req.default`, `req.options` (`-D name[=value]`), `req.host`, `req.editor`.
    `cpu`/`features` `""` = target default; `b.no-std = true` drops std.
  - `fn editor(req): k1/editor-settings`, primary, editor compiles
    (`test-on-save`).
  - `fn module()` / `fn module(b: k1/build-config)` returns data (`k1/module` in
    builtin.k1): `.0 | .{ library = true, depend = ["sqlite", "./deps/dbx"],
    submodules = [.{ dir = "web", depend = ["store"] }], ignore = ["attic"],
    link = [.{ name = "uv", link-type = :static }], link-args, setup-inputs,
    setup-outputs, configure }`.
  - `fn setup()` / `fn setup(ctx: k1/setup-ctx)` declares setup; runs on the
    host, cwd = module dir, when `setup-outputs` are missing or stale against
    `setup-inputs`, `build.k1`, params and target; failure leaves no stamp. All
    setups finish before program source is read.
- `depend`: `/`, `./`, `../` is a folder relative to that `build.k1`; a bare name
  is the library the program chose by that name, else `$K1_HOME/modules/<name>`.
  A library is named for its folder; one name with two folders errors. A module
  sees core, std, its submodules and its own `depend`, never transitive deps, so
  macro output naming a library needs the caller to depend on it.
- A submodule is a folder of the module's own code compiled separately, keyed
  `app/dir` and written `dir/...` in the owner. It sees core, std and its
  `depend` (sibling submodules first, then libraries), never its owner; it has no
  `build.k1`, no nesting, and `k1/module-dir()` is its own folder. `k1 test` runs
  the program's and its submodules' tests; the CLI given a submodule folder works
  on the owner.
- Params: `let params: { f: t } = k1/module-params(defaults)` in a library's
  `build.k1`. The program's `configure` is a backtick literal keyed by library
  name (`.{ http = .{ tlog = ${b.optimize} } }`), each value merged over that
  library's defaults; only the program configures. Final after all manifests
  run (readable in `fn setup`, not `fn module`). Program code reads `params.f`
  inside the module, `y/params.f` elsewhere.
- Plan cached in `.k1-out/cache/plan-*`. The editor compiles the nearest
  `build.k1` at or below the workspace root whose module holds the file, else the
  file alone.
- Host facts: `b.target`, `ctx.build.target`, `k1/host-platform.dylib-ext()`.
  `k1/module-dir()` is the dir of the module whose namespace compiles the call
  (all macro output counts as the caller's).
- A `library = true` primary emits `lib<name>.{dylib,so}` and a fat `lib<name>.a` exposing
  only exports (`examples/klib`). `k1 run` rejects it; `k1 test` accepts it.

## Concurrency
- Atomics are ops on ordinary memory (`core/atomic.k1`): `load store xchg
  fetch-add/sub/and/or/xor/min/max cmpxchg cmpxchg-weak fence`;
  `cmpxchg` returns `{ prev, ok }`. Integer-sized scalars (`ptr` for non-arith).
  Orderings (`:relaxed :acquire :release :acq-rel :seq-cst`) must be literals;
  `atomic/cell[t]` matches over a runtime one.
- `std/thread/task[t]`: `task/spawn(fn[x]. work(x)).join()`; raw
  `thread/start`/`join` over `*fn(*t) -> ptr`.

## Stdlib notes
- `files` (core, posix, works in `#static`): `read-to-string write-entire-file
  list-dir make-dir link rename remove remove-dir` return `result[_, files/error]`
  (`not-found()`, `already-exists()`); `exists` is a bool.
- `std/hash`: `hash/bytes` is fast, unseeded, unstable across versions;
  `hash/fnv1a` is the stable one.
- `std/time`: `unix-nanos()`, `unix-seconds()`; monotonic `read-cpu-counter()` +
  `elapsed-nanos`/`elapsed-ms`.
- `std/json`: `to-string`, `write(w, v)`, `to-string-pretty`, `parse[t]`; schema
  blanket impls, a concrete `json-read`/`json-write` impl overrides. Options are a
  static type arg naming a global: `json/parse-with[t, camel](s)`. Unescaped decoded
  strings borrow the input. `use std/json/json` shadows the ns: write `json/json`.
- `std/url`: `url/parse` borrows; accessors are zero-copy, still-encoded views;
  path/query/fragment accept any visible byte, `normalized()` is strict RFC 3986;
  `resolve`, `path-segments`, `query-pairs`, `url/builder`, `url/percent`,
  `url/form`. `http/header.url` is parsed (400 if not).
- SIMD (`core/vector.k1`): `eq-lanes lt-lanes gt-lanes to-mask shuffle-bytes`
  (16 lanes, pshufb), `vector/shuffle(a, b, mask)` (static lane indices into a
  then b), `vector/shift-bytes[k](prev, cur)`. Library byte scans
  vectorize: `index-of`/`position` first; a stop-at-any-of-N scan follows
  `std/url` `find-stop` (`vector/splat` + `load-unchecked` over `k1/simd-bytes`
  lanes, scalar tail); `json` `escape-position` ORs `eq-lanes`/`lt-lanes` classes.
