# Codegen units: how the split composes

This note started as the analysis behind `parallel_llvm`'s second cut and
now describes what landed. Sections 1 and 2 are the rule, 3 the features
under it, 4 planning, 5 the debt kept, 6 the dev pipeline measurement.

Landed shape: `Cg::prepare_host`/`prepare_dylib` lower ir and compute
physical types on the main thread, `plan_units` cuts the live closure into
at most 32 units, `codegen_units` runs them on `min(units, cores)` threads,
and `write_unit_artifacts` combines: objects for dev builds, ThinLTO through
`src/thinlto_shim.cpp` for `--optimize`, and an IR merge for every
single-file artifact. Reload dylibs go through the same planner.

Measured (600-unit stress / suite1, 14 cores): dev build 12.0s→3.6s /
491→179ms; optimized 24.9s→6.6s / 1.65s→0.57s; optimized rebuild with a warm
ThinLTO cache: codegen 6.2s→1.6s. brotli under ThinLTO is within ±4% of
whole-program O3, the bench's noise floor.

## 1. Carve-outs in the first cut

| situation | today | why |
|---|---|---|
| `--emit-llvm` | one unit | one `.ll` expected |
| `--filc` | one unit | Fil-C's clang consumes one `.ll` |
| bare targets, cross-built libraries | one unit | the object is the artifact; hidden symbols would be visible to its consumer |
| `ns(reload)` | one unit | slot globals are defined by whichever unit first references them; descriptors and stubs assume one module |
| constant `let` globals | private copy per unit | the initializer must be visible for folding (`if k1/is-static`) |
| strings, statics | private copy per unit | discovered on demand |
| reachability | folds `JumpIf` on a constant-bool global | static-only arms reference symbols the target lacks (`isatty` on wasi) |
| backend builtins | fixpoint over the walk | ir lowering does not enqueue them |
| `--optimize` | our import heuristic | LLVM's ThinLTO is not in the C API |
| roots | `program-exit`, `crash-unloaded-ns` hand-listed | codegen references them itself |

All of these are gone except the last, which is the honest shape: codegen
lists the functions it references itself, in one place (`prepare_host`,
`prepare_ir`).

## 2. The rule

The program is one logical module. A unit is a *view* of it: definitions
for what it owns, declarations for everything else. Symbols fall in three
classes, and that classification is the whole design:

**Owned.** Functions, mutable and external-visible globals, and everything
codegen synthesizes: reload slots, descriptors, the entry wrapper. Exactly
one unit defines them, `external` with `hidden` visibility unless the
program exports them (or is a reload host, which exports everything for
`dlopen`). Ownership of K1 functions is the plan; ownership of everything
synthesized is unit 0, and all of it is enumerable up front.

**Shared immutable data.** Constant globals, string literals, static
values. Any unit that meets one emits it as `linkonce_odr hidden
unnamed_addr constant`; the linker keeps one copy. This is C++'s inline
variable model and it holds on ELF (comdat), Mach-O (weak def) and wasm
(comdat). It preserves address identity, lets every unit fold the
initializer, and needs no ownership bookkeeping. It replaces both the
private-per-unit duplication and the constant-global special case.

**Declared.** Everything else, by name.

The output stage then decides how units combine, and no feature ever needs
a single unit:

| output | combination |
|---|---|
| executable, dylib | link the N objects with `cc` (today) |
| single-file artifact (`--emit-llvm`, `--filc`, bare targets, cross-built libraries) | parse each unit's bitcode into one context, `Module::link_in_module`, internalize, then print or emit one object |

One mechanism covers every single-file artifact. `LLVMLinkModules2` is in
the C API as `link_in_module`; it drops `available_externally` copies and
dedupes `linkonce_odr` by construction. Internalizing is a loop over the
merged module turning hidden visibility back into `internal` linkage, which
restores the "non-exported symbols are local" contract of the old single
module without any object tool. Units codegen in parallel and hand back
unoptimized bitcode; the pipeline and the emit run serially on the merged
module, so the printed IR and the object are exactly the single module of
old. That serial tail is only paid by the lanes that need one file, the
rare, small ones (freestanding libraries, cross builds, IR dumps), which is
the "fewer parallelism sometimes" trade this note accepts.

The alternative, a relocatable link of the N objects, needs an ELF and
Mach-O driver added to the in-process lld shim plus a localize step
(`llvm::objcopy` as a library, since no `llvm-objcopy` ships with k1). It
buys parallel emission for the artifact lanes at the cost of a second
combination mechanism. Not worth it until one of those lanes is slow.

`cc` stays the executable linker: it supplies the sysroot, crt objects,
default libraries and platform flags we do not want to own.

## 3. Features, one by one

### Reload

The only real obstacle is lazy definition: `reload_fn_addr_global` and
`codegen_reload_global_addr_slot` add the slot with an initializer in
whichever unit asks first, so two units would both define it. Under the
rule, slots and descriptors are synthesized and enumerable (every reloadable
fn and global of every reload ns), so unit 0 defines them all before its
bodies and other units get name-keyed declarations, which the lookup path
already produces for the dylib side. Stubs are ordinary functions: the
reloadable fn's host definition is its stub, planned like any function, and
the descriptor in unit 0 references stubs by symbol. `crash-unloaded-ns`
stays a root; it belongs in the same enumeration rather than a separate
list. The reload host already makes every global `external` with default
visibility so the dylib can bind them, and that is exactly the "exported"
case of the owned class.

The dylib is a program of its own with roots = the ns's functions and its
globals; it can go through the same planner. Nothing in it is special once
slots are owned by unit 0.

### Bare targets and cross-built libraries

The artifact contract is "one object whose non-exported symbols are
local". The IR merge followed by internalizing hidden symbols gives exactly
that: one object, `internal` linkage for everything the program does not
export, emitted by the same target machine as before. No host or shipped
tool is involved, so the "cross-built library cannot be linked with host
tools" caveat goes away with it. The freestanding lane's `--undefined-only`
check is unaffected.

### `--emit-llvm` and `--filc`

IR-level merge as above; the pipeline runs on the merged module, so the
printed IR is post-pipeline as it always was, and `--filc` gets the
unoptimized merge its clang expects (it runs the dev pipeline there, as
before).

### `--debug`

Full DWARF per unit is one compile unit per object, the same shape as a C
program with several translation units. Declarations of foreign functions
already get declaration subprograms. `test.sh` now builds suite1 with
`--debug` and runs it with `--emit-llvm`. One thing `--debug` exposed: it
runs no LLVM passes, so the `k1/is-static` arm must be folded by codegen
itself, which `live_successors` and the `JumpIf` emission do.

### `--optimize`: replace the import heuristic with ThinLTO

The import machinery (`import_depth_for`, owned/imported instruction
counters, the `available_externally` lane, the 400/100/0.7/2× constants)
exists only because ThinLTO's two missing pieces are not in the C API: a
summary-bearing bitcode writer and a driver. Both are one shim away, in the
build's existing pattern (`src/lld_shim.cpp`, `build.rs`, static libstdc++
on Linux). `libLLVMLTO.a` is already linked by llvm-sys.

- `k1_thinlto_bitcode(LLVMModuleRef) -> LLVMMemoryBufferRef`: builds the
  module summary index and writes summary-bearing bitcode. Units first run
  `thinlto-pre-link<O3>` through the existing `run_passes`.
- `k1_thinlto_codegen(buffers, n, cpu, features, pic, preserved,
  cross-referenced, cache dir, emit callback)`: `llvm::ThinLTOCodeGenerator`,
  which does the thin link, dead-symbol computation, promotion, importing,
  internalization, the post-link pipeline and codegen on its own thread
  pool, and writes an object per module. Each unit reports its
  default-visibility definitions (preserved: `main`, exports, everything a
  reload host exports) and its declarations (cross-referenced). The second
  list is the linker's half of a ThinLTO link: without it the driver
  internalizes hidden symbols that other units call and they vanish. Its
  cache dir under `.k1-out/cache/thinlto` makes optimized rebuilds
  incremental, keyed by module hash and import set.

What that deletes: the whole import lane, the per-unit constant
duplication (ThinLTO wants the `linkonce_odr` shape anyway), and the
`isatty` class of problem in optimized builds, since the thin link drops
unreferenced hidden functions across the program. What stays: the split and
the three symbol classes (ThinLTO needs a frontend split too), the
reachability walk for dev builds, and our own threads for dev builds.

## 4. Planning the units

Today: LPT by ir instruction count, floor 8k instructions per unit, count
= cores. Two problems: the object layout depends on the machine's core
count, and any edit reshuffles every unit.

- Fix the maximum at 16 and run `min(units, cores)` threads. Objects are
  then machine-independent; rustc made the same choice for the same reason.
- Consider source-structured units: one per K1 module or namespace,
  size-split when large, size-merged when small. Balance is worse than
  LPT, but `core` and `std` objects would hash-stably reuse across builds
  the way the typer snapshot does, and ThinLTO's cache does the same for
  optimized builds. That is the dev-loop win past parallelism, and it is
  worth fewer units.

## 5. Debt that stays

- `SharedProgram` is `unsafe impl Sync` and `IdentPool::get_string` reads
  past its `RefCell`. Sound under "codegen never interns, reports or
  lowers", enforced by `CgError` and the prep step. Making `TypedProgram`
  `Sync` by construction is a separate project, deliberately not started.
- `compute_all_physical_types` warms every type before the threads start;
  milliseconds, the price of a frozen program.

Resolved from the first cut: `k1/is-static` reads lower to
`Value::IsStatic` (bc substitutes true, codegen false, and codegen's block
walk and branch emission take the one live edge), so codegen inspects no
initializers; every call lowers through `add_call`, which enqueues the
callee, so the closure is one pass.

## 6. The dev pipeline's instcombine

`function(mem2reg,instcombine,simplifycfg),globaldce` spends three quarters
of its time in instcombine. Actionable: A/B three pipelines on build wall
time and on the runtime of suite1 and brotli non-optimized:

1. as is;
2. `mem2reg,simplifycfg,globaldce`;
3. `instcombine<no-verify-fixpoint;max-iterations=1>` in place of the full
   run.

Measured on stress (build) and brotli non-optimized (runtime): dropping
instcombine saves 7% build time and costs 3 to 6% at runtime;
`max-iterations=1` saves 2% build time at no measurable runtime cost. The
pipeline now runs instcombine with `max-iterations=1`.
