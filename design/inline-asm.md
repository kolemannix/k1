# Inline asm

Status: design only. Nothing here is implemented.

## The model

Inline asm is not "embedding text in the output". It is a single opaque
instruction with a user-declared contract: which values flow in, which flow
out, where each must live (a register class or a specific register), what else
gets destroyed, and what memory/side effects occur. The compiler never parses
the assembly text. It typechecks and register-allocates against the contract,
substitutes register names into the template's placeholders at emission, and
the assembler (LLVM's MC layer) is the first thing that ever reads the
instructions. The contract is trusted: lying about a clobber is a miscompile
that only reproduces under register pressure. That is the whole design
pressure on the surface syntax — make the contract structured and checkable,
because the compiler cannot verify it against the text.

Prefer `fn(intern("llvm.*"))` whenever an intrinsic exists: the optimizer sees
through intrinsics and treats asm as a black box. Asm is for what intrinsics
can't reach — system registers, syscalls, instructions LLVM has no intrinsic
for, exact instruction sequences (crypto, timing), and eventually naked
functions and module-level symbol definitions.

## Surface syntax

One expression form, everything in a single argument list, options in the
modifier keyword-call grammar (`fn(extern("x"), lib("y"))` flavor):

```
asm(<template>, <operand struct>?, <option>*)
```

```k1
fn umul128(a: u64, b: u64): { lo: u64, hi: u64 } {
  asm(`
    mul {b}
  `, .{
    a  = in(rax, a),
    b  = in(reg, b),
    lo = out[u64](rax),
    hi = out[u64](rdx),
  }, pure, nostack)
}
```

- Template: a backtick block literal (dedent already works). Placeholders are
  `{name}` referring to operand fields, with per-arch modifiers (`{x:e}` =
  32-bit sub-register on x86-64; `{v:w}`/`{v:x}` on aarch64). `{{`/`}}`
  escape. A `raw` option disables placeholder processing entirely.
- Operands: one anonymous struct whose fields are built by intern fns from
  `core/asm`:
  - `in(loc, v)` — input.
  - `out[t](loc)` / `lateout[t](loc)` — output. `out` is early-clobber (may be
    written before inputs are consumed; regalloc must not overlap it with
    inputs). `lateout` promises the write happens after all inputs are read
    and may share a register with an input.
  - `inout(loc, v)` / `inlateout(loc, v)` — tied input/output, one register.
  - `imm(v)` — `v: static t`, substituted as an immediate.
  - `sym(f)` — a function reference; substitutes the mangled/exported symbol
    name, for calls and address formation inside the template.
- Locations are identifiers, not strings: register classes (`reg`, `vreg`,
  `reg8`, ...) and fixed registers (`rax`, `x0`, `v3`, ...) are constants of a
  builtin `asm/loc` type living in per-arch namespaces (`asm/x86-64`,
  `asm/aarch64`). The operand struct of an asm expression is typechecked with
  the current target's arch namespace opened, so `rax` on an aarch64 build is
  an ordinary unresolved-identifier error at the asm's span. No constraint
  strings anywhere in user code.
- Result type: the struct of the `out`/`lateout`/`inout` fields, in
  declaration order — `{}` when there are none, unwrapped to the bare value
  when there is exactly one. No out-pointers; asm stays a value expression.

### Options

Defaults are the safe maximum: side-effecting, memory clobbered, flags
clobbered, may touch the stack. Every option is a promise that relaxes one of
those, unlocking optimization:

- `pure` — no side effects beyond the outputs; requires at least one output.
  Enables CSE/DCE like any pure call. Must be combined with `nomem` or
  `readonly`.
- `nomem` / `readonly` — the asm touches no memory / only reads memory. Drops
  the implicit `~{memory}` clobber (compiler barrier) and sets the call-site
  memory effects.
- `preserves-flags` — drops the implicit flags clobber (`~{cc}` on x86,
  `~{nzcv}` on aarch64).
- `nostack` — the asm does not push/pop or write below sp; drops `alignstack`
  and keeps the red zone usable.
- `noreturn` — the expression types as `never`; codegen appends `unreachable`.
  No outputs allowed.
- `att` — AT&T dialect on x86 (default is Intel).
- `clobbers(rcx, xmm0, ...)` — registers written that carry no value.
- `clobber-abi(c)` — expand to the target C ABI's full call-clobbered set;
  for templates that contain calls.

Note volatile-by-default does not mean executed-exactly-once: LLVM may still
duplicate side-effecting asm (unrolling, inlining). The guarantee is only
"executed as often as its source position executes, not reordered against
other side effects, never deleted".

## Target gating

Add alongside `k1/platform`:

```k1
type arch = either { x86-64, aarch64, wasm }
let arch: arch = builtin
```

Portability is plain `#if k1/arch is :aarch64 { ... }` — dead-arch asm is
skipped exactly like `#if platform` code today, and the arch-scoped register
namespaces catch anything unguarded. No new cfg machinery. Asm on a target
with no asm story (wasm) is a typecheck error at the expression.

## Pipeline

- **parse.rs**: `asm` is a keyword expression head. `ParsedAsm { template:
  StringId, operands: Option<ParsedExprId>, options }`. Template keeps its
  token so per-line spans can be recovered.
- **typer.rs**: typecheck the operand struct; every field must be one of the
  `core/asm` builder intern fns (same interception style as
  `intern("llvm.*")` tyapps); `loc` arguments and `imm` values are evaluated
  as static values. Verify every `{name}` resolves to an operand field, warn
  on unreferenced operands, check option coherence (`pure` needs an out,
  `noreturn` forbids outs). Compute the result type from the out fields.
- **ir.rs**: `IrCallee::InlineAsm { asm_id }` mirroring
  `IrCallee::LlvmIntrinsic`, with an `AsmSpec` side pool: template, operand
  list (direction + loc + type), clobbers, flags. iropt treats it as a call:
  opaque, effectful unless `pure`.
- **bc/lower.rs + vm**: a `CallAsm` opcode whose handler raises a comptime
  error at the asm's span: "inline asm cannot execute at compile time".
  Functions containing asm still load and lower for the VM; only reaching the
  instruction errors. Dual-path code guards with `if k1/is-static` (the VM
  side takes the fallback branch; the asm is never reached).
- **codegen_llvm.rs**: synthesize the LLVM constraint string from the spec —
  outputs first (`=&r` for `out`, `=r` for `lateout`, `={rax}` for fixed,
  tied `0` digits for inout), then inputs, then clobbers (`~{rcx}`,
  `~{memory}`, `~{cc}`); build the `InlineAsm` callee value with
  `sideeffect` (unless `pure`), `alignstack` (unless `nostack`), and the
  dialect flag; emit the call; append `unreachable` for `noreturn`.
  Constraint strings exist only here, as an encoding detail of the backend.
- **Diagnostics**: attach `!srcloc` metadata (span id as cookie) to the call
  and install the context diagnostic handler so MC-layer assembler errors
  ("invalid operand", bad mnemonic) come back as K1 errors pointing at the
  template line, not a post-hoc `llc` failure. This is the piece that makes
  asm feel native instead of bolted on.

## Deferred

- `fn(naked)` — body restricted to a single asm expression, no prologue;
  needed for interrupt handlers and precise ABI shims.
- Module-level asm (`#asm` at top level) — defining symbols/data outside any
  function.
- `may-unwind`, label operands (asm goto).
- VM execution of asm when target == host (JIT the fragment). Almost
  certainly never worth it; the `is-static` guard pattern covers real uses.

## Box checklist vs rustc `asm!`

Typed operands, direction split incl. late-outs, register classes and fixed
registers, tied operands, immediates, `sym`, clobbers + `clobber-abi`, effect
options, template modifiers, per-target gating, assembler diagnostics mapped
to source, explicit comptime story. Improvements over Rust: locations are
resolved identifiers instead of strings, outputs are the expression's value
instead of `let`-forward mutation, and the arch namespace makes cross-arch
misuse a name-resolution error.
