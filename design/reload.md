# ns(reload): live function and global reload

Landed through phase 4 (2026-08-13): front end, codegen split, api-hash gate,
loaded-version, swap logging, std/watch + watch(). Reloadable globals landed
2026-08-16. Deferred: `k1 b --watch`; multiple reloadable ns sharing one dylib.

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

- Globals and consts are allowed and are RELOADABLE STATE: storage lives in
  the dylib, so every swap maps a fresh copy with the new initializer baked
  in. Edit `let(mutable) speed: i64 = 3` to `= 4`, rebuild, and the running
  host reads 4 — that is the point. The flip side is deliberate: runtime
  mutations are lost on swap (a reload-ns global is a tuning knob, not a
  persistent counter; persistent state belongs in the host). tls, extern, and
  exported globals stay rejected. Types are allowed (they are baked into every
  caller like any type; the api hash covers them).
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
  - tls globals (a patched address slot cannot represent per-thread storage
    across dlopen'd images), extern and exported globals
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
  reloads. Raw dylib addresses never escape... except through a reload
  global's `.&`, which is the current version's storage by design.
- Reload-ns GLOBALS mirror the fn story with a slot instead of a stub. The
  dylib defines and exports the global (initializer baked in as a constant);
  the host carries `__k1_reload_global_addr_` + the stable symbol, a
  null-initialized `ptr` slot the loader patches. Host-side accesses call a
  private `__k1_reload_global_load(slot, ns, name)` helper: acquire-load,
  crash-unloaded-ns on null, else the storage address — a call, not inline
  control flow, so `Value::GlobalAddr` stays legal anywhere a value is
  (phi incomings resolve the call in the edge's source block). Since each
  access re-reads the slot, a host loop re-reading a tuning global sees every
  swap.
- ir-level const folding skips reload-ns globals: IR is shared across codegen
  units, and a folded scalar would bake one version's value into the host.
- Calls BETWEEN fns of the same reloadable ns compile direct inside the dylib,
  and the ns's own fns read their own globals direct: a call tree that entered
  version N completes entirely in version N, data included. Only host→ns
  entries go through the patched slots. This is the version-coherence story;
  there is no cross-version interleaving within a request.

## Codegen split

- The typed program partitions into the host LLVM module and one dylib LLVM
  module per reloadable ns.
- Dylib module contents: the ns's fns (exported, deterministic mangling — the
  fully-qualified `module/ns/fn` path, no compiler-internal ids, since dlsym
  needs stable names across independent rebuilds) PLUS internal-linkage copies
  of the full callee closure (library fns, generic specializations they
  instantiate). Function CODE may be duplicated freely; HOST globals may not —
  every host-global reference stays an external declaration resolved against
  the host at load. Single copy of every host global (the TLS arena stack
  above all) is the invariant that keeps allocation correct. The ns's OWN
  globals invert this: defined in the dylib, one copy per loaded version.
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
      entry_count, entries: [(symbol_name, slot_ptr)] }

Entries cover fns and globals alike — a slot is a slot, and dlsym resolves
data symbols the same way. The descriptor and dylib are ns-driven, not
fn-driven: a reload ns holding only globals still gets its artifact and gate.

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
- each global (ordered by name): name + structural type — never the value,
  so a value edit swaps cleanly while adding/removing/re-typing a global
  refuses with :api-changed
- structure and layout of every type reachable from those signatures
- compiler build id (a dylib from a drifted toolchain must not load)

Structural, not id-based: TypeIds are not stable across the independent
compiles that produce a running host and a later dylib. Stamped into the host
descriptor and exported from the dylib. This is what makes prod patching sane:
signature or layout drift refuses loudly instead of corrupting memory. Body
edits and fn renames-of-parameters keep the hash; adding/removing/changing any
fn signature or reachable type layout changes it.

## Testing

- Fail tests (test_src, //errmsg): reload_global_fail (tls global),
  reload_generic_fn_fail; reload_unloaded_abort (//abortmsg) locks in the
  null-stub crash message.
- `dogfood/reload_test`: the live-swap driver, in test.sh. Builds the app
  template into /tmp, runs it, rebuilds scene.k1 underneath it via K1_EXE, and
  drives it over stdin (std/process child API): body-only rebuild swaps live;
  a global's value edit swaps in and resets a host-side mutation; signature
  change or an added global → `:api-changed`, old fns and values stay live;
  watch() picks up a rebuild with no poking.

## Non-goals

- No state migration: a swap replaces the ns's globals with fresh copies;
  nothing carries values across versions.
- No partial patch: a load either fully swaps or changes nothing.
- No dlclose, ever.
- No VM/bc changes.
