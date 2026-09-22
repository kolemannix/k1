# `types/id[static-literal].name()` SIGTRAPs

Wall: showcase-walls-2026-09-16.md:25. Repros: `design/showcase-walls-repros/exp/exp1c.k1`,
`exp1f.k1`, `exp1h.k1`.

## Root cause

One key mismatch. `type_infos` is keyed by the **family** type; every emitted
`type-id` value carries the **actual** type.

`src/k1/typer/reflect.rs:242`, first line of `get_type_info`:

```rust
let type_id = self.get_type_family_type(type_id);
```

`get_type_family_type` (typer/types.rs:1823) maps `static[i64, 15]` -> `i64`, so
`register_type_metainfo(static[i64, 15])` writes `type_infos[i64]`.

Meanwhile `BuiltinTyperInline::TypeId` (typer.rs:12578) registers the metainfo and then
calls `synth_type_id_literal(type_id, span)` (typer/synth.rs:518) with the *unmapped* id.
Measured: `types/id[fifteen].inner == 6962`, `types/id[int].inner == 9`.

Both consumers then look up 6962 and miss:

- `src/k1/bc/exec.rs:1125-1127` — `k1.type_infos.get(&type_id)` -> `kbail!("Missing type
  info: static[i64, 15]")`.
- `src/k1/codegen_llvm.rs:3387-3415` — the runtime switch's arms are the `type_infos`
  keys, so 6962 falls to the default block at `codegen_llvm.rs:3371-3382`, which is
  `llvm.trap` + `unreachable`. That is the SIGTRAP (signal 5), and because nothing
  flushes, the block-buffered stdout is lost.

Two consequences confirm the diagnosis:

- The bug only bites through the runtime switch. `types/name[fifteen]` and
  `types/id[fifteen].info().name` are constant-folded in the typer
  (typer.rs:12376-12385, `is_static_type_id_expr`) and print `i64` today — folded and
  switched paths disagree. `.name()` is `fn name(self) { self.info().name }`, whose
  receiver is a parameter, so it always takes the switch.
- It is not specific to `types/id`. Generic instance args go through the same
  register-family / emit-actual pair (`reflect.rs:280`), so
  `types/instance-info[fixlist[u8, 8]].!.args` traps on the `static[i64, 8]` arg.
  Verified.

The default block is independently reachable: `types/type-id/make(999999).name()` SIGTRAPs
too, so it must diagnose regardless of the key fix.

## Fix

### 1. Key `type_infos` by the real type; delegate only the *contents* to the family

`src/k1/typer/reflect.rs`, `get_type_info` — move the one existing call, add nothing:

```rust
pub(super) fn get_type_info(&mut self, type_id: TypeId) -> StaticValueId {
    if let Some(existing) = self.type_infos.get(&type_id) {
        return *existing;
    }
    let reserved_id = self.static_values.pool.reserve_id();
    self.type_infos.insert(type_id, reserved_id);

    let family_type_id = self.get_type_family_type(type_id);
    let name_value_id = self.build_type_name(type_id);
    let schema_value_id = self.build_type_schema(family_type_id);
    let instance_value_id = self.build_instance_info(family_type_id);
    ...
```

A static type now has its own entry: **its own name** (`build_type_name` ->
`static[i64, 15]`, already what dump.rs:493 prints) and **its family's schema and instance
info**. Registration key == lookup key everywhere, so the VM, the fold and the switch all
agree. Schema delegation is what keeps `types/schema[fifteen] is :int` true; without it
`Type::StaticValue` falls into the `other` catch-all at reflect.rs:685 and that is a
regression.

`static[i64, 15]` is not abstract (types.rs:2074-2086: `unresolved_static_count` is 0 once
`value_id` is set), so codegen emits an arm for it. Unresolved `static[t, ?]` stays
abstract and is still skipped, which is right — it exists only inside un-specialized
generic code.

Rejected alternative: canonicalize at the *source*, i.e. have `types/id[T]` emit the family
id. One line, but it makes `types/id[15] == types/id[16]` and `types/id[15] ==
types/id[int]` true, destroying a distinction users can observe today. Reflection should
report the type it was handed. It also fixes only the top-level case, not the instance-arg
case above.

### 2. Make the remaining miss a diagnostic, not a trap

`modules/core/builtin.k1`, in `ns k1` beside `crash-unloaded-ns` (builtin.k1:1104-1111):

```k1
fn crash-missing-type-info(type-id: u64): never {
  crash("no type info for type id $type-id")
}
```

`crash` (builtin.k1:1072) flushes stdout, prints a backtrace and aborts, and has a
`k1/no-std` branch, so the freestanding lane is fine.

`src/k1/codegen_llvm.rs`:

- In the `BackendBuiltin::TypeInfo` arm, replace the `else_block` body (the `llvm.trap`
  get-or-add, its call, lines 3371-3382) with a call to that fn passing the already-loaded
  `type_id_arg`, then `build_unreachable`. `type_id_arg` is built in `entry_block`, which
  dominates `else_block`.
- Generalize `crash_unloaded_fn_value` (codegen_llvm.rs:1549) to
  `k1_crash_fn(&mut self, name: &str)` — ident lookup in `k1_scope_id` +
  `declare_llvm_function`. Two callers, one helper.
- `prepare_ir` (codegen_llvm.rs:993-1002): root `crash-missing-type-info` when
  `!k1.type_infos.is_empty()`, mirroring the `ns.reload` root. Factor the two root pushes
  into one small `root_k1_fn(k1, &mut roots, name)`.

### Deleted

- `codegen_llvm.rs`: the `llvm.trap` declaration/lookup block — its only use in the crate.
- `codegen_llvm.rs`: `crash_unloaded_fn_value`, absorbed into `k1_crash_fn`.
- `reflect.rs:242` in its current position.

Do not touch `exec.rs:1126`; after the key fix its message only fires for genuinely-absent
types and is already a proper diagnostic.

## Tests

- `test_src/suite1/static_type_literals.k1` (`ns static_literals`, already registered at
  `suite1.k1:75`), appended to `test()` — must go through `.name()` on a value receiver so
  the switch is exercised, not the fold:
  - `assert-equals(types/id[fifteen].name(), "static[i64, 15]")` for a
    `let fifteen: static int = 15`
  - `assert(types/schema[fifteen] is :int)` — pins the family delegation
  - `assert(types/id[fifteen] != types/id[int])` — pins that static types stay distinct
  - `types/instance-info[fixlist[u8, 8]].!.args.[1].name() == "static[i64, 8]"` — the
    nested case, which the top-level fix alone would not cover
- `test_src/type_info_missing.k1`, new one-file case for the trap-to-diagnostic change:
  `types/type-id/make(999999).name()` after a `println`, with
  `//abortmsg: no type info for type id`. It also pins that stdout survives.

## Verification already done

Part 1 was applied and reverted (`reflect.rs` back at md5 66de1cd9). With it:

- exp1c / exp1f / exp1h all print (`static[i64, 15]`,
  `static[{ x: i64, y: i64 }, .{ x: 12, y: 2 }]`); exp1h's `#static` path resolves.
- `fixlist[u8, 8]` instance args print `u8`, `static[i64, 8]`.
- `just ts1` passes plain and `--optimize`; `k1 run test_src/stdlib` passes;
  `cargo test --lib` 162/162; `comptime_parity`, `logreport`, `selfhost`, `httpapp` build
  and run.
- Behavior change with no test pinned on it: `types/name[fifteen]` goes `i64` ->
  `static[i64, 15]`. `array_type_test.k1`'s `types/name[array[i32, 10]]` is the array's own
  name and is unaffected.

Part 2 is unimplemented. Run `just test` after it, plus `just ts-freestanding` for the
no-std `crash` path.

## Effort

Small. ~20 min for part 1 (one line moved), ~45 min for part 2, ~20 min for the tests,
plus a `just test` cycle.
