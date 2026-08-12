# clone, flat, and friends

Stance: flat data is the blessed path. Plain assignment is already a bitwise
copy of any value, so data built from scalars, enums, indices/handles, and
inline collections (fixlist) never needs clone at all — copying bits is
correct. `clone` exists only for types that own storage
(buffer/list/string/map), and recursive element-wise copies are a perf smell
we deliberately do not make easy: derived clone never follows a pointer. Code
should be shaped so that a memcpy of a flat region is the whole move; pools +
handles over pointer webs.

Goal: `x.cloned-in(dest)` duplicates owned storage into a chosen allocator so
the result is self-contained in `dest` and the source arena can be reset. The
no-argument form uses the current arena.

## Ability shape

```k1
ability clone {
  fn cloned-in[a: allocator](self, alloc: a): self
  fn cloned(self): self { self.cloned-in(mem/current-arena()) }
}
```

- Names follow the existing `cloned` / `cloned-in` convention on
  buffer/span/list; those free fns become the impls, callers keep their
  syntax. `clone` keeps its Java/Rust connotation on purpose: the explicit,
  possibly-expensive, semantically-deep one.
- The fn-level `[a: allocator]` param inside an ability is the same shape as
  `print-to[w: writer]`, whose derived impls already synthesize bodies
  per-specialization (typer.rs `specialize_function_body`, the
  StructPrintTo/SumPrintTo guard). Proven machinery, not new ground.
- Needs a pinned `ABILITY_ID_CLONE`; follow the from-string precedent for
  pinning. The decl references `allocator`, so either allocator moves up into
  builtin.k1 (its default bodies only need `types/`, already there) or clone
  pins from mem.k1 the way from-string pinned from its own file.

## flat: an ability, computed structurally

This is Rust's Copy — but only half of it, and the half Rust's name obscures.
Copy does two jobs there: license for implicit bitwise duplication (vs move),
and the structural claim "the bits are the whole value". K1 has no moves —
assignment already copies everything — so the first job is vacuous here. We
are naming only the second: *a bitcopy is a complete clone; the value owns
nothing*. The name is `flat` — a shape claim, which is what the property is:
no references, no indirection, one self-contained extent of bits. "list[
string] is not flat" explains itself. (`pod` rejected: collides with a real
word whose meaning fights the concept; `trivial` rejected: a complexity
claim, not a structural one.)

It is an ability, not just a TypeInfo bit, because the two things the design
needs are exactly what the ability system provides and a bare property can't:

- **bounds**: `fn f[t: flat]` in generic code, and the collection fast paths
  become ordinary impl selection instead of a special predicate. The bound is
  also the enforcement tool: constraining a hot type's usage to `t: flat` is
  the compile-time assert that it stays on the blessed path.
- **the override valve**: `perm[t]` contains a pointer, yet declares
  `impl flat for perm[t]`. No structural walk can ever say yes to that; an
  explicit impl just does. The opposite direction is rejection impls, below.

The derive composes per-field through the impl search, exactly like the
equals derive: scalars/enums are flat leaves, references fail, `perm[t]`
rescues, `type(nocopy)` types (below) are refused, and lookups are cached per
(type, ability) so the cost model is the one equals already pays. No TypeInfo
bit: a bit computed at type registration cannot see impls (positive or
negative) declared later, so any fast path keyed on it would answer
differently than the impl search once overrides exist.

flat has no fns (if fn-less abilities need a small typer allowance, that's
the whole cost). What it buys clone is one line:

```k1
impl[t: flat] clone for t {
  fn cloned-in[a: allocator](self, alloc: a): self { self }
}
```

One blanket impl covers every flat type; the clone derive never reasons about
triviality, it only composes non-flat types.

## Rejection impls

A type whose bits are structurally clean can still be wrong to derive things
for — an id type whose zero is a reserved sentinel, a secret-bearing config
that must not print. The type author states it:

```k1
impl(not) zero for order-id
impl(not) print for api-config
```

`impl(not)` follows the modifier-call declaration style. Semantics:

- It blocks *derivation* of that ability for that type, and poisons
  containers naturally: the per-field search hits the rejection and fails
  with an error naming the field. A container author can still write an
  explicit impl and take responsibility — rejection binds only the derive for
  the rejecting type itself.
- It is registered in the impl-declaration phase alongside positive impls, so
  it is exactly as order-robust as blanket impls — no query whose answer
  changes as compilation proceeds. Rejections belong in the type's own
  module, which is natural since the type author writes them.
- It is not a bound. `t: not flat` does not exist — negative reasoning as a
  constraint wrecks coherence and inference. Rejections only make searches
  fail, loudly.
- Explicit positive impl + rejection for the same (type, ability) is an
  error.

General across the derivable abilities: `flat`, `zero`, `print`, `equals`
(identity types where structural equality lies).

## copy, nocopy, and the by-value footgun

"copy" in K1 already ambiently means bitwise duplication — assignment, param
passing, struct construction. It is not an ability and must not become one:
no fns, no impls, no meaningful bound; dressing it as one invites `impl copy`
and `t: copy` machinery we'd regret. flat implies copy-is-fine by
definition; the interesting direction is denial.

`type(nocopy)` is a declaration modifier for identity types — values whose
duplication is itself the bug, not merely confusing: rc, `in[heap, t]`, a
future mutex. Being on the type declaration (not an impl) means it is visible
the instant the type is registered — zero phase-ordering hazards — and the
coupling is one-directional for free: the flat derive refuses nocopy types,
never the reverse. A nocopy value binds and passes by reference only;
duplication requires an explicit `.cloned()`. Errors surface at the copy
site, including inside generic code at specialization (where K1 reports
generic problems anyway) — no `t: copy` bound exists or is needed.

Note on rc: K1 has no drop, so copying an rc to a *reader* is harmless —
nothing auto-releases. nocopy on rc is discipline enforcement, not soundness:
it makes every duplication an auditable clone-bump or a reference. The type
author's call, which is why the mechanism is opt-in.

For the alias-safe owners (list, string, buffer), copying is *not* the bug —
read-only by-value passing is correct and cheap, and every struct literal
with a list field is a by-value copy, so blanket prevention or blanket
warnings are untenable. The bug is mutating a copy you believe is the
original. One targeted, default-on warning covers it: *mutating a non-flat
value received by value as a parameter* (`fn add(l: list[t]) { l.push(x) }` —
the lost update). Never fires on read-only passing, construction, or
deliberate local snapshots; stays a warning because param-as-scratch is
legitimate expert code. (Go is the natural experiment for these exact
semantics — value-copied headers, no dangling — and its answer was the same:
community norms plus targeted per-type vet lints, e.g. copylocks.)

`type(nocopy)` also opens the door to `type(linear)` — notes at the end;
nothing planned.

## zeroability: reuse the zero ability

`ability zero { fn zero(): self }` already exists (builtin.k1), implemented
by hand for every numeric type, and `TypeInfo.is_zero_safe` is already
computed compositionally in types.rs — with no consumers yet. Connect them:
derive `zero` on demand, body `mem/zeroed()`, and delete the dozen
hand-written numeric impls (the derived body produces the same bits).

The bytes/value split resolves cleanly: `is_zero_safe` keeps its current
meaning — all-zero bytes is a valid *bit pattern* (references included:
"nullable" is true at that level) — which is the notion allocators and
serialization want. The `zero` *ability* is the stricter value-level notion,
and gets it by deriving per-field like flat instead of reading the bit:
references are not zero leaves (no null references as ordinary values), and
sentinel-zero types opt out with `impl(not) zero`. No second bit needed.

One wrinkle stands: `zero` today reads as additive identity, paired with
`one` for algebra. Conflating algebraic zero with zeroed memory is safe
wherever both exist (they coincide), but it does commit us to "zero the
value" = "zero the bits". In a bits-are-values language that seems right;
flagging it.

Like flat, `zero` needs a pinned ability id for the derive.

## References: derivation refuses, never recurses

- `*t`, `*mut t`, raw `ptr`: **no impl, no blanket impl.** A type with a bare
  reference field fails clone derivation, and the error says why and what to
  do: use an index/handle into a pool, wrap in `perm[t]` if the referent
  outlives the clone, or write a manual impl if you truly want to follow the
  pointer. Deep-following a reference is always hand-written code —
  explicit, greppable, cost visible at the site. This is the friction that
  moves the needle: the derive hands out duplication of owned storage, never
  graph traversal.
- `perm[t]`: a wrapper marking a reference as permanent — the referent
  (interned data, static tables, an arena that outlives all clones) is
  stable, so copying the reference bits is a complete clone. Its entire
  definition is `impl flat for perm[t]`; clone arrives via the blanket. A
  struct of handles + perms stays memcpy-clean, and the wrapper documents
  lifetime intent in the type. Naming and deref ergonomics (`.get()` vs
  auto-deref) open.
- Types with interior sharing (rc) implement clone manually; rc's clone is a
  refcount bump and stays on the heap — it does not migrate allocators.
- Aliasing/cycles: not clone's problem by construction — it never walks
  references. A manual impl that chooses to follow pointers owns that risk.

## Collections and the cost vocabulary

Three intents, three homes — the caller can always state exactly as much as
they mean, and each name is a checked claim:

- `copied` / `copied-in`: flat memcpy of storage, unconstrained t, no clone
  semantics claimed — "I want these bytes, I understand aliasing" (Zig's
  `dupe`). Also serves the shallow-snapshot cases (e.g. pubsub's
  subscriber-list copy into tmp) whose elements aren't cloneable at all.
- `cloned` / `cloned-in`: correct duplication; costs whatever the type owns.
  `impl[t: clone] clone for buffer[t]` is element-wise *semantically*, with
  memcpy as an implementation fast path taken when t is flat — a static
  branch inside one impl, not competing blankets, so there is no
  impl-precedence question.
- `cloned-flat[t: flat]`: the caller says flat and the compiler holds them to
  it — one line delegating to `copied-in`, and if the element type ever grows
  a string field, every call site breaks loudly instead of silently becoming
  a walk. The same tool works anywhere: `[t: flat]` on your own hot function
  makes every clone inside it provably a memcpy.

In short: `copied` = O(bytes) always; `cloned` = O(owned); a `flat` bound =
proof they coincide.

- span: clone the backing buffer and rewrap (existing span/cloned behavior —
  a self-contained result must not keep views into the old arena). list:
  cloned buffer + len. string, opt, map/set: straightforward.
- fixlist is the blessed collection: inline storage, flat when t is,
  clone-free by construction. spill-list is not (heap spill).

## Move idioms (no separate ability)

The blessed move for pool-shaped data is a handful of buffer memcpys: clone
the pools, handles stay valid bits. For everything else:

```k1
let out = mem/with-arena(scratch, build()).cloned-in(dest)
// reset scratch; out is self-contained in dest
```

Ambient cloning composes with the arena stack:
`mem/with-arena(a, x.cloned())` ≡ `x.cloned-in(a)` for arena targets.

## Allocator-tagged storage (heap-string, generalized)

`x.cloned-in(heap)` returns a plain `string` — correct bits, but the type no
longer records what backs it, and heap-string/heap-bytes exist precisely for
that guarantee: rc can only free storage it *knows* is heap. So clone does
not subsume them. It does the opposite — it supplies the missing ingredient
that lets them stop being string-specific:

```k1
type in[a: allocator, t] = { alloc: a, raw: t }

ns in {
  fn cloned[a: allocator, t: clone](alloc: a, value: t): in[a, t] {
    .{ alloc, raw = value.cloned-in(alloc) }
  }
  // re-clone stays in the pinned allocator, ignoring the ambient one
  fn recloned[a: allocator, t: clone](self: in[a, t]): in[a, t] {
    in/cloned(self.alloc, self.raw)
  }
}
```

The constructor *is* the guarantee: clone's contract (result self-contained
in the destination) is exactly "raw's storage came from alloc", enforced for
any `t: clone` rather than hand-written per type. heap-string = `in[heap,
string]`, heap-bytes = `in[heap, span[u8]]`; the wrapper costs zero bytes for
heap (empty struct) and, with a stored `*arena`, also expresses arena-pinned
values — storage that knows its arena. Name open: `in[a, t]` / `backed[a, t]`.

Boundaries of v1: `in[a, t]` does not implement clone (`cloned-in(dest)`
returning `self` would lie about the tag — moving out of the pin is
`self.raw.cloned-in(dest)`, an unwrap by design), and it is a `type(nocopy)`
candidate — duplicating the tag duplicates release responsibility. Freeing
is not generic: releasing t's storage through `a` requires knowing t's
allocation shape, which today rc knows only for its single-buffer raws
(string, span). Generic deep-free is the destructor problem — `owned`'s
territory, out of scope here.

## Derivation mechanics (compiler, on the equals/print rail)

- flat: per-field impl search with scalars/enums as leaves; rejection impls
  and `type(nocopy)` fail the search and poison containers.
- clone: flat types are covered by the blanket, so the derive only fires for
  non-flat struct/sum — derive iff every field/payload implements clone; body
  is a struct literal / tag match cloning the non-flat parts.
- zero: per-field derive like flat (leaves: numerics, char, bool, enums with
  a zero member; references excluded; rejections honored), body
  `mem/zeroed()`.

## dyn

`cloned-in` is generic, so it is excluded from dyn tables by the existing
per-fn exclusion. Whether `cloned` (default body) earns a table slot is not a
launch requirement.

## Steps

1. Rejection impls (`impl(not)`) in the impl-declaration phase.
2. `flat` ability + pinned id + per-field derive. `perm[t]` with `impl flat`.
3. `type(nocopy)` modifier + copy-site errors; mark rc (and likely
   `in[a, t]`).
4. `clone` ability + pinned id + the flat blanket impl + manual collection
   impls (buffer with flat fast path, `cloned-flat`, span/list/string/opt/
   map); `copied-in` and the shallow-snapshot migration.
5. Compiler derive of clone for non-flat struct/sum (StructClone/SumClone).
6. `zero` derive; delete the hand-written numeric zero impls.
7. `in[a, t]` and the heap-string/heap-bytes migration onto it.
8. The mutating-a-by-value-param warning for non-flat types.

## Tests (suite1, registered by hand in suite1.k1)

- derive round-trips: struct of collections, sum with payloads, nested
  generics.
- flat collapse: clone-of-flat == assignment; buffer[flat-t] clone hits the
  memcpy path; `cloned-flat` compiles for flat elements and errors for
  non-flat.
- the move proof: build in arena A, `cloned-in(B)`, clobber/reset A, verify
  the clone intact.
- clone to heap; ambient clone under with-arena.
- negative: struct with a `*t` field → clone derivation fails naming the
  field and suggesting handle / perm / manual impl; `type(nocopy)` value
  copied by value → error at the copy site; `impl(not) zero` sentinel type →
  no derived zero.
- perm: struct{ perm[big], data } clones bitwise, referent shared.

## Notes on type(linear) — pipedream, nothing planned

The declaration surface generalizes: `nocopy` = may not duplicate;
`linear` = may not duplicate *and* must be consumed exactly once. K1 is
unusually well-placed for this because it has no drop, no moves, and no
lifetime inference — linearity reduces to a per-function flow count
(Austral's thesis: linearity with a checker you can explain on one page).

- By-value use *is* the consume; the obligation travels with the bits. No
  fn-boundary annotations: `f(h)` consumes, `f(h.&)` inspects. The existing
  value/reference distinction at call sites is the consume/borrow
  distinction.
- The reference lane is unchecked (no lifetimes): this is a discipline
  guarantee — no forgotten release, no double release — not a memory-safety
  proof. Matches K1's scope.
- Flow rules: every control path consumes exactly once; consuming in a loop
  needs a fresh producer per iteration; returning is a consume. Escape hatch:
  `.leak()` (consume without effect).
- Infection, mirror of flat poisoning: storing a linear value in a struct is
  a consume, so the container carries the obligation and must itself declare
  `type(linear)` — explicit, not inferred. Collections: references only, at
  least at first.
- Pairs beautifully with defer: defer schedules the release in plain sight;
  linearity verifies every path actually did it. Everything visible, still
  helping.

## Open decisions

- `perm`'s name + deref ergonomics; `in[a, t]` vs `backed[a, t]`.
- Rejection spelling (`impl(not)`).
- Where the clone decl lives (builtin.k1 vs mem.k1) given the allocator
  constraint and pinned-id mechanics.
