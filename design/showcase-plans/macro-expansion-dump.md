# Seeing macro expansions

Walls (showcase 04/08/09): `#debug macro` dumps the macro's own typed body + IR +
bc, never an expansion; `ai_docs/k1-additional.md:280` claims otherwise; emitted
files reach `.k1-out/generated/` only behind `has_diagnostic`; a crash inside a
macro points at the macro definition, never at the call site.

## What is actually there today (measured)

`build_emitted_source` (static_exec.rs:1143) concatenates the `code` chunks into
one `String`, records chunk byte ranges against their source spans, and
`compile_emitted_code` (1201) registers that string as a real `SourceFile` under
`.k1-out/generated/meta_<stem>_<line>_<serial>.k1` and parses it. Spans in
emitted code are therefore real spans into an in-memory file; error rendering
prints the emitted text inline whether or not the file exists on disk. Verified:

```
.k1-out/generated/meta_dbg2_7_2.k1:3:0: error
│ ->nonexistent-fn(1)
│  Function not found: 'nonexistent-fn'
note: in code compiled in place of this call:   <- dbg2.k1:7  mk(3);
```

`report_ext` (report.rs:223) remaps the span back through the chunk table
before storing it, so `write_error`'s note block (report.rs:308-312) only fires
when the span lands in *glue* (text from `code/from-string`, escapes, dedent).
When the span remaps out into the macro's own template literal — the common
case — the error box points at the macro definition and the note is silently
lost. That is the reported error-location wall, and it is the same plumbing as
the crash-in-macro wall.

Volume, `--chatty true --cache false check`: suite1 16 emitted / 3 kb, httpapp
43 / 9 kb, stdlib 11 / 6 kb, refchess / brotli / selfhost 1 each. Writing is
sub-microsecond.

Three findings that change the shape of the answer:

1. **`write_emitted_sources` runs in the LSP.** compiler.rs:1447 sits inside
   `compile_program_ext`, which is what `lsp_main.rs:390,546` and
   `lsp_support.rs:623` call. Dropping the gate means ~50 file writes per
   keystroke. The gate stays.
2. **The gate is already the right predicate.** The emitted file's path is
   printed in the error box; gating on `has_diagnostic` makes the path exist
   exactly when something names it. Always-writing would also leave stale
   serial-numbered files lying around (`out_dir_generated` is only
   `create_dir_all`'d at compiler.rs:1284, never cleared).
3. **Expression-level `#debug` is dead code.** `eval_expr` (typer.rs:6733) calls
   `push_debug_level` (3806), which raises `log::set_max_level` — but
   `env_logger` re-filters at `default_filter_or("info")` (compiler_main.rs:31),
   so `debug!` records still drop. Confirmed: `#debug twice(...)` prints zero
   lines; the pre-existing `debug!("Emitted source:...")` at static_exec.rs:1246
   only appears under `RUST_LOG=debug`, where everything appears anyway. Under
   `RUST_LOG=trace` the push *lowers* the level. It is inert at best.

4. `nearest_parent_function(ctx.scope_id)` already answers "what were we
   compiling": specialized bodies get `ScopeOwnerId::Function(specialized_id)`
   at typer.rs:13109, and `display_function_signature` (dump.rs:1405) already
   renders `name[t := i64]` from `specialization_info`. **No new plumbing.** The
   work-stack trace is not usable for this: `Trace::push` (trace.rs:365) returns
   `None` for non-essential kinds unless `profiling_mode`, and
   `FunctionSpecialize`/`MacroCall`/`FunctionTypecheck` are all non-essential,
   so the stack is empty in a normal compile.

## Recommendation

Keep files as the errored-expansion artifact; make the live cases inline.
Four pieces, one plumbing change shared by all of them.

### 1. An `EmitOrigin` threaded into `compile_emitted_code`

```rust
#[derive(Clone, Copy)]
enum EmitOrigin { MetaBlock, Macro(FunctionId) }
```

Two call sites: static_exec.rs:1105 (`#meta`) passes `MetaBlock`; :1490
(`run_macro_and_compile_output`) passes the *specialized* macro id it already
has as `function_id`. One caller of `run_macro_and_compile_output` (:1473)
forwards `function_to_run`.

### 2. A real header, built where the context is

Move header construction out of `build_emitted_source` (delete the `writeln!` at
static_exec.rs:1158-1164 and its `get_span_location`/capacity fiddling; it
returns the body, offsets body-relative) into `compile_emitted_code`, which is
the only place that has `ctx`. Header:

```
// emitted by macro regex/compile
// at showcase/re.k1:29:12
```

built from `EmitOrigin` + `get_span_location(span)`. Shift each `CodeChunkPos`
by `header.len()` in the existing loop before pushing `EmittedSource`. Rename
the file `<macro-name>_<stem>_<line>_<serial>.k1` (`meta_...` for `MetaBlock`)
so the dir greps. Delete the TODO at static_exec.rs:1231-1233.

**The specialization context does not go in the header.** The parse cache
(`emitted_parse_cache`, keyed `(content_hash, span)`) short-circuits before any
file is created; if the header carried `fn scan[t := string]`, every
specialization would miss the cache and mint its own file, multiplying emitted
files by the specialization count. A file on disk is a deduped artifact and can
only honestly carry the facts that are constant across the hits: emitter and
call site. Hash the **body** only (build the header after the cache probe, so a
hit costs nothing). The per-expansion facts the author wants go in (3) and (4),
where they are always accurate. If the author would rather have
per-specialization files, it is a one-line flip (hash header+body) with the file
count as the price — call it out, do not default to it.

### 3. `#debug` prints each expansion

Replace `debug_level_stack: RefCell<Vec<log::LevelFilter>>` (typer.rs:2833,
3030, snapshot.rs:108) with `expansion_debug_depth: u32`; delete
`push_debug_level`/`pop_debug_level` (typer.rs:3806-3818) and make the existing
`eval_expr` scopeguard (typer.rs:6733-6741) increment/decrement it. That turns
dead machinery into the useful thing without adding a concept.

In `compile_emitted_code`, print header + emitted text to stderr when either:

- `self.expansion_debug_depth > 0` — i.e. `#debug some-macro(x)` at one call
  site, which is the control that was missing entirely; or
- `EmitOrigin::Macro(f)` and `get_function(f).compiler_debug()` — `#debug macro
  foo`, every expansion.

The stderr block carries the line the header cannot:

```
expansion of macro regex/compile at showcase/re.k1:29:12
  while compiling fn scan[t := string]
```

from `nearest_parent_function(ctx.scope_id)` +
`display_function_signature(w, &sig, f.specialization_info)`.

Also guard the IR dump for macros: `ir.rs:1558` `let is_debug =
f.compiler_debug() && !f.is_macro();`. The typed-body eprintln at
typer.rs:15778-15790 stays. Today `#debug macro` buries the useful output under
two IR listings and a bc disassembly of a five-line macro; that noise is why
the wall reads "prints typed body/IR/bc, not the expansion".

### 4. Call site in the diagnostic

One helper in report.rs, modelled on `with_bc_trace` (bc/exec.rs:183) so
`K1Message` stays 12 bytes and the snapshot format is untouched:

```rust
fn note_expansion_site(&mut self, e: K1Message, call_span: SpanId) -> K1Message
```

appends one line per hop, `expanded from <path>:<line>:<col>`, walking the
emitted chain (same ≤16 loop as `remap_span`, report.rs:177). Applied at:

- `run_macro_and_compile_output` (static_exec.rs:1485-1489) on the `Err` out of
  `do_with_vm` — fixes the crash-in-macro wall (probe6: today the box points at
  `crash(...)` in the macro body and never mentions `shout("")`).
- the `#meta` arm, static_exec.rs:1092-1104, same.
- `report_ext` (report.rs:223), computed from the span **before**
  `remap_to_source_span` — fixes the remapped-out case where the box lands on
  the macro's template literal.

Then delete the `write_error` note block (report.rs:308-312): it only covered
the glue case, which (3) now covers uniformly. `//errmsg` keeps working —
substring match against the message text.

### 5. `--dump-emitted`

`#debug` requires editing the macro definition, which you cannot do for a macro
in `modules/core` or a dependency. One flag covers that:
`write_emitted_sources` (static_exec.rs:1288) becomes `if emitted.has_diagnostic
|| self.config.dump_emitted`. `Args` (compiler.rs:~338) + `CompilerConfig`
(~403) + five literal `Args` sites (compiler.rs:2242, lsp_main.rs:380,542,
test_suite.rs:119, lsp_support.rs:621), all `false`. Off in the LSP by
construction.

While there: `remove_dir_all` + `create_dir_all` on `out_dir_generated` at
compiler.rs:1284. Serial-numbered names shift between runs, so today's dir
accumulates stale expansions that read as current — a pre-existing trap that
`--dump-emitted` would make routine.

## Deletions

- `// generated by #meta block at ...` header string, static_exec.rs:1158-1164
  (wrong for macro calls; replaced).
- TODO, static_exec.rs:1231-1233 (answered).
- `debug_level_stack` + `push_debug_level` + `pop_debug_level`, typer.rs:2833,
  3030, 3806-3818, snapshot.rs:108.
- `write_error`'s emitted-source note block, report.rs:308-312.
- `ai_docs/k1-additional.md:280-281`: "to inspect an expansion put `#debug` on
  the macro *definition*" — currently false. Rewrite: `#debug` on the macro
  definition prints every expansion, `#debug` on a call expression prints that
  one, `--dump-emitted` writes all of them; keep the true half ("definition-level
  `#debug $call(...)` does not parse").
- `ai_docs/k1-additional.md:318-324`: accurate about span remapping, silent
  about when files exist. Add one sentence: emitted files are written when a
  diagnostic lands in them or under `--dump-emitted`, and every emitted file
  carries an emitter + call-site header.

## Tests

- `compiler_test` (compiler.rs:2225) — new case: temp module with a macro called
  from a generic fn specialized at two types. Assert on the returned
  `TypedProgram`: `emitted_sources.len() == 1` (body-only hashing keeps the
  cache hit across specializations), its `SourceFile` content starts with the
  emitter + call-site header, and `.k1-out/generated/` is empty. Flip
  `dump_emitted` and assert the file appears. Pins the design decision in §2.
- `test_src/macro_crash_site.k1` — probe6 shape, `//errmsg: expanded from` (plus
  the call line) — pins §4's crash path.
- `test_src/macro_emitted_site.k1` — `code/from-string` emitting a bad call, same
  expectation for the glue path.
- `test_src/suite1/test_macro.k1` keeps covering behaviour; no expansion text
  changes, so nothing there goes stale.
- `just ts1`, then `just test` bare.

## Effort

~1 day. static_exec.rs ~120 lines (mostly moved, not added); report.rs ~35;
typer.rs/ir.rs ~30, net negative; compiler.rs ~15; docs 2 spots; tests ~80.
No `EmittedSource` field change, so no `SNAP_MAGIC` bump.
