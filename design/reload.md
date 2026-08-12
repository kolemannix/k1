# ns(reload): live function reload

Plan as designed 2026-08-11. Prereq work landed: keyword-call modifier lists and
whole-ns modifier tracking (ns(lib(..)) is the template for ns(reload)).

## Surface

```
ns(reload) handlers {
  fn handle-index(req: http/request): http-response { ... }
}

fn main() {
  handlers/load(:once).!        // or :watch in dev
  ...
  handlers/handle-index(r)      // normal call syntax
}
```

- Functions only in a reloadable ns. No globals, no consts: state lives in the
  host and survives reload by construction. Types are allowed (they are baked
  into every caller like any type; the api hash covers them).
- Call syntax is unchanged. Callers never know the ns is reloadable.
- Reloadability is kept in release builds (patch a running system without
  restart). Only the trigger differs: `:watch` is the dev mode; production
  calls `load` explicitly (admin endpoint, signal handler). Nothing ever
  auto-reloads without one of those.
- `handlers/loaded-version(): u64` for observability; every swap is logged.

## Semantics

- `reload` is whole-ns, exactly like `lib`: any opening may declare it, all
  openings share it, every fn whose DIRECT ns is the reloadable ns goes in the
  dylib. No inheritance into nested ns.
- Typer rejects, inside a reloadable ns (silent partial reload is the worst
  outcome, so everything that would bake into the host is an error, not a wart):
  - globals and consts ("functions only")
  - generic fns (specializations instantiate into callers)
  - ability defns (their fns specialize into impls in the host)
  - ability impls (monomorphized into callers)
  - macro defns, `#static`/`#meta` defns (expanded at host compile time)
  - nested ns (v1: keep the dylib unit flat)
  - extern and intrinsic fns (no body to reload)
- The VM/comptime lane ignores `reload` entirely: `#static` evaluation calls
  these fns directly. Only the LLVM lane splits.

## Call path

- The typer's whole contribution is `TypedFunction.is_reloadable`; fn-addr
  globals, stubs, and descriptors are codegen-level LLVM entities. Host
  carries, per reloadable fn:
  - a FN-ADDR global: the fn's current address, an exported LLVM `ptr` global,
    null-initialized, named `__k1_reload_fn_addr_` + the fn's stable symbol,
    so independently rebuilt dylibs and the loader agree on it.
  - a STUB: the fn itself, body replaced in host codegen: acquire-load the
    fn addr, tail-call it; on null call core's `reload-not-loaded(ns, fn)`
    (cstring args), which crashes with "reloadable ns `handlers` not loaded;
    call handlers/load()". All call sites call the stub directly.
- iropt never inlines a reloadable callee: an inlined body in a host unit
  would bypass the stub.
- The stub is the single patch point, and taking `handlers/handle-index` as a
  value (`.&`, dyn, storing it) yields the stub's address — stable across
  reloads. Raw dylib addresses never escape.
- Calls BETWEEN fns of the same reloadable ns compile direct inside the dylib:
  a call tree that entered version N completes entirely in version N. Only
  host→ns entries go through fn-addr globals. This is the version-coherence story; there
  is no cross-version interleaving within a request.

## Codegen split

- The typed program partitions into the host LLVM module and one dylib LLVM
  module per reloadable ns.
- Dylib module contents: the ns's fns (exported, deterministic mangling — the
  fully-qualified `module/ns/fn` path, no compiler-internal ids, since dlsym
  needs stable names across independent rebuilds) PLUS internal-linkage copies
  of the full callee closure (library fns, generic specializations they
  instantiate). Function CODE may be duplicated freely; GLOBALS may not — every
  global reference stays an external declaration resolved against the host at
  load. Single copy of every global (the TLS arena stack above all) is the
  invariant that keeps allocation correct.
- Host module: reloadable fns are replaced by fn-addr global + stub;
  everything else as today.
- Link: dylib target is PIC, linked `-dynamiclib` with
  `-undefined dynamic_lookup` (macOS) / `-shared` with undefined symbols
  allowed (Linux). Host link gains `-Wl,-export_dynamic` / `-rdynamic` when the
  program has any reloadable ns, so its globals are visible to dlopen'd images.
- Artifact: `.k1-out/<program>.<ns>.dylib` beside the binary; the host
  descriptor stores the exe-relative path.

## Load machinery

Per reloadable ns, host codegen emits an exported descriptor global
`__k1_reload_ns_<module/ns>`:

    { dylib_file_name, api_hash, entries: [(symbol_name, fn_addr_ptr)] }

The loader is plain K1 in `modules/std/reload.k1`. The typer synthesizes
no-arg `load` and `load-async` fns into each reloadable ns — one call passing
the ns's qualified path to std/reload (the only typer involvement beyond
`is_reloadable`). They declare before the ns's own fns, so a user fn with
either name fails as an ordinary duplicate. load-ns finds the descriptor
via dlsym on the host itself and resolves the dylib beside the exe
(`sys/exe-path`):

1. Copy the artifact to a unique temp name, dlopen(RTLD_NOW | RTLD_LOCAL)
   that, unlink the copy. The copy is what makes RE-load work: dyld caches
   images by path, so dlopen of a replaced artifact returns the stale image.
2. read the dylib's exported `__k1_reload_hash_<ns>` global; compare against
   descriptor hash. Mismatch → error, nothing is patched: "restart required:
   api of `handlers` changed".
3. dlsym every entry (any miss → error, nothing patched), then release-store
   every fn addr. Stub loads are acquire.
4. record version, log the swap. NEVER dlclose: old images leak deliberately;
   in-flight call trees drain in their own version. Dev sessions leak a few MB
   per reload; a prod patch leaks once.

`load(): result[unit, reload-error]` — error cases: file missing, dlopen
failure, hash mismatch, symbol missing. Caller decides what a failed reload
means.

`:watch` spawns a std/thread watching the dylib artifact via `std/watch`
(kqueue on macOS, inotify on Linux; see below). On change it runs the same
gated load; failure logs and keeps the current version. The app only ever
watches the ARTIFACT — rebuilding is someone else's job (`k1 b --watch`, the
still-open snapshot-plan stage 2; until then a Justfile watchexec recipe).

## Api hash

Computed over the ns's surface, InputsHash-family:

- each fn (ordered by name): name + full typed signature
- layout fingerprint of every type reachable from those signatures
- compiler build id (a dylib from a drifted toolchain must not load)

Stamped into the host descriptor and exported from the dylib. This is what
makes prod patching sane: signature or layout drift refuses loudly instead of
corrupting memory.

## Testing

- Fail tests (test_src, //errmsg): global/const/generic fn/ability impl/macro/
  nested ns inside ns(reload); load-before-... (abort test via //abortmsg for
  calling an unloaded fn).
- Live-swap test can't run inside the suite's single compile: dogfood-style
  driver program using std/process — build app with v1 source, run it (loads,
  asserts old behavior), rebuild dylib from v2 source over the artifact, poke
  the app (stdin), app reloads and asserts new behavior. Plus the negative:
  v2 with a changed signature → load returns hash-mismatch error, old fns
  still live.

## Phases

1. Front end: `reload` in the ns modifier list (merge/conflict like lib_name),
   `Namespace.reload` flag, all rejection rules, fail tests. Small; land first.
2. Codegen split: partition, dylib emission + closure copy, fn-addr
   globals/stubs, mangling, link flags, artifact placement. Codegen-emitted
   descriptor + `load` one-shot via std/reload. Sandbox end-to-end.
3. Hash gate: api hash, dylib stamp, reload-error channel, loaded-version,
   swap logging. Driver-based dogfood test.
4. `std/watch` (kqueue/inotify file watching, its own std surface) + `:watch`
   thread on top of it + dev-loop recipe.
5. Deferred: `k1 b --watch`; multiple reloadable ns sharing one dylib.

## Non-goals

- No state migration: there is no state in the ns to migrate.
- No partial patch: a load either fully swaps or changes nothing.
- No dlclose, ever.
- No VM/bc changes.
