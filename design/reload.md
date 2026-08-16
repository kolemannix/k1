# ns(reload): live function reload

Landed through phase 4 (2026-08-13): front end, codegen split, api-hash gate,
loaded-version, swap logging, std/watch + watch(). Deferred: `k1 b --watch`;
multiple reloadable ns sharing one dylib.

## Surface

```
ns(reload) handlers {
  fn handle-index(req: http/request): http-response { ... }
}

fn main() {
  handlers/load().!             // or handlers/watch().! in dev
  ...
  handlers/handle-index(r)      // normal call syntax
}
```

Synthesized into every reloadable ns (user fns with these names fail as
ordinary duplicates; the names are not reserved):

- `load(): result[empty, reload/load-error]` — one gated swap
- `load-async(): *reload/pending-load` — same, off-thread; poll() the cell
- `loaded-version(): u64` — 0 until the first successful load
- `watch(): result[empty, reload/load-error]` — dev mode: load if never
  loaded, then a detached thread keeps the ns current with its artifact

- Functions only in a reloadable ns. No globals, no consts: state lives in the
  host and survives reload by construction. Types are allowed (they are baked
  into every caller like any type; the api hash covers them).
- Call syntax is unchanged. Callers never know the ns is reloadable.
- Reloadability is kept in release builds (patch a running system without
  restart). Nothing ever auto-reloads without load() or a watch() thread:
  production calls load explicitly (admin endpoint, signal handler).
- Every swap is logged with its new version.

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
  - nested ns (the dylib unit is flat; permanent semantics, no escape hatch)
  - extern and intrinsic fns (no body to reload)
- The VM/comptime lane ignores `reload` entirely: `#static` evaluation calls
  these fns directly. Only the LLVM lane splits.

## Call path

- The typer's whole contribution is `TypedFunction.is_reloadable` plus the
  synthesized fns above; fn-addr globals, stubs, and descriptors are
  codegen-level LLVM entities. Host carries, per reloadable fn:
  - a FN-ADDR global: the fn's current address, an exported LLVM `ptr` global,
    null-initialized, named `__k1_reload_fn_addr_` + the fn's stable symbol,
    so independently rebuilt dylibs and the loader agree on it.
  - a STUB: the fn itself, body replaced in host codegen: acquire-load the
    fn addr, tail-call it; on null call core's `crash-unloaded-ns(ns, fn)`
    (cstring args), which crashes with "reloadable ns `handlers` not loaded;
    call handlers/load()". All call sites call the stub directly.
- iropt never inlines a reloadable callee: an inlined body in a host unit
  would bypass the stub.
- The stub is the single patch point, and taking `handlers/handle-index` as a
  value (`.&`, dyn, storing it) yields the stub's address — stable across
  reloads. Raw dylib addresses never escape.
- Calls BETWEEN fns of the same reloadable ns compile direct inside the dylib:
  a call tree that entered version N completes entirely in version N. Only
  host→ns entries go through fn-addr globals. This is the version-coherence
  story; there is no cross-version interleaving within a request.

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
  descriptor stores the exe-relative file name.

## Load machinery

Per reloadable ns, host codegen emits an exported descriptor global
`__k1_reload_ns_<module/ns>`:

    { dylib_file_name, api_hash, version: *mut u64,
      entry_count, entries: [(symbol_name, fn_addr_ptr)] }

The loader is plain K1 in `modules/std/reload.k1`; the synthesized fns are
one-call bodies passing the ns's qualified path to it. load-ns finds the
descriptor via dlsym on the host itself and resolves the dylib beside the exe
(`sys/exe-path`):

1. Copy the artifact to a unique temp name, dlopen(RTLD_NOW | RTLD_LOCAL)
   that, unlink the copy. The copy is what makes RE-load work: loaders cache
   images by path (dyld on macOS, glibc's by-name match), so dlopen of a
   replaced artifact returns the stale image.
2. Read the dylib's exported `__k1_reload_hash_<module/ns>` global; compare
   against the descriptor hash. Mismatch → `:api-changed`, nothing patched:
   "restart required: api of `handlers` changed".
3. dlsym every entry (any miss → error, nothing patched), then release-store
   every fn addr. Stub loads are acquire.
4. Bump the version cell, log the swap. NEVER dlclose: old images leak
   deliberately; in-flight call trees drain in their own version. Dev sessions
   leak a few MB per reload; a prod patch leaks once.

`watch()` spawns a detached std/thread watching the artifact's DIRECTORY via
`std/watch` (kqueue on macOS, inotify on Linux) — a file watch would lose the
inode when the artifact is replaced. On events it hashes the artifact and, only
when the bytes actually changed, runs the same gated load; failure logs and
keeps the current version. The app only ever watches the ARTIFACT — rebuilding
is someone else's job (`just watch <dir>` runs watchexec; `k1 b --watch` is the
still-open snapshot-plan stage 2).

Note: dlopen of never-before-loaded code is ~100ms flat on macOS (first-load
validation, cached per-inode in the kernel); the copy dance guarantees that
worst case per reload. `load-async` exists for latency-sensitive hosts.

## Api hash

Computed in the typer (`reload_api_hash`) over the ns's surface:

- each fn (ordered by name): name + full typed signature
- structure and layout of every type reachable from those signatures
- compiler build id (a dylib from a drifted toolchain must not load)

Structural, not id-based: TypeIds are not stable across the independent
compiles that produce a running host and a later dylib. Stamped into the host
descriptor and exported from the dylib. This is what makes prod patching sane:
signature or layout drift refuses loudly instead of corrupting memory. Body
edits and fn renames-of-parameters keep the hash; adding/removing/changing any
fn signature or reachable type layout changes it.

## Testing

- Fail tests (test_src, //errmsg): reload_global_fail, reload_generic_fn_fail;
  reload_unloaded_abort (//abortmsg) locks in the null-stub crash message.
- `dogfood/reload_test`: the live-swap driver, in test.sh. Builds the app
  template into /tmp, runs it, rebuilds scene.k1 underneath it via K1_EXE, and
  drives it over stdin (std/process child API): body-only rebuild swaps live;
  signature change → `:api-changed`, old fns stay live; watch() picks up a
  rebuild with no poking.

## Non-goals

- No state migration: there is no state in the ns to migrate.
- No partial patch: a load either fully swaps or changes nothing.
- No dlclose, ever.
- No VM/bc changes.
