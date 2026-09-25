# IR Audit (2026-09-24)

| status | item | finding |
|---|---|---|
| Planned             | B2, B8, B9   | Reassigned scalar locals, splittable aggregate allocas and copy-only temps live in memory: alloca promotion pass, see below |
| Not started         | B6           | ~2,000 jump-only blocks that exist to feed a phi (select, or phi-tolerant trampoline removal)          |
| Not started         | B7           | `for` loops round-trip each `?item` through memory every iteration                                               |
| Proposed, after promo | B10          | Recomputation (1,551 duplicate `struct_offset`s, repeated branches, fixlist re-checking `used < cap`): value numbering at emission, see below |

## Alloca promotion pass

One pass in `optimize_unit`, after inlining, so the VM and LLVM both get it.
Lowering keeps emitting aggregates into slots and scalar lets as allocas
unless `is_ssa_value`; only the pass sees whole inlined units, and only whole
units decide whether a temp, an inlined param or a place passed as an
argument escapes.

1. Use scan. Walk each alloca's uses through `struct_offset` and
   constant-index `array_offset`, tracking the offset. Scalar load/store at a
   known offset is a field read/write; `copy` src/dst is a whole read/write;
   an aggregate by-value call argument is a whole read for the duration of the
   call (the callee can neither take its param's address nor write it);
   any other pointer argument, a stored or returned pointer, or a dynamic
   `array_offset` escapes. `volatile` pins. A never-escaping alloca is local.
2. Split (phase 2). A local aggregate with only known-offset accesses, never
   passed whole to a non-inlined call, under a field-count limit, becomes one
   scalar alloca per field. A whole `copy` between split allocas becomes
   per-field load/store pairs; the loads of a self-reading assignment
   (`x = .{ a = x.b, b = x.a }`) come before its stores by construction.
3. Promote (phase 1). Scalar locals, including the fields from step 2,
   become SSA values with phis at dominance frontiers. `Dominators` moves
   from the validator into `unit.rs`.
4. Forward (B2), on aggregates still in memory: large ones, dynamically
   indexed arrays, escaping destinations, argument sources.
   - Source forwarding: `copy t <- s`, `t` local, written only by this copy,
     then only read. Reads of `t` become reads of `s`; `t` and the copy go.
     Legal when nothing writes `s` between the copy and `t`'s last read (for a
     local `s`, direct writes only; otherwise any call or unknown store bails).
   - Build in place: `copy d <- t`, `t` local, the copy its last use. `t`
     becomes `d`. Legal when nothing reads or writes `d` from `t`'s first
     write to the copy (same local/non-local rule), with `d`'s address
     available at `t`'s first write (derived offsets are re-emitted there).
   - First version: copy and uses in one block. Then multi-block, checking
     the blocks reachable from the copy that reach a use.

Assignment keeps its temp (`store_assignment_value` compiles the rhs with no
dst) and relies on steps 2 and 4; aggregate call arguments are already passed
as places at emission. Split before forward: forwarding a small local into a
call argument would force it back into memory.

## B10 proposal: value numbering at emission

`push_value` looks up each pure instruction (int/float ops, compares, casts,
`struct_offset`, `array_offset`), keyed by operation and operands, in a table
next to `fold.rs::simplify`, and reuses the existing value on a hit. The table
is scoped by the builder's structured recursion, which matches dominance:
values emitted before a match, `if` or loop are visible in its arms and body,
and values emitted inside an arm die at the join. On entering either side of a
`jmpif c`, the table also records `c` as true or false, so a repeated test of
the same condition folds to a constant and its branch folds with it. Cloned
instructions in `inline_body` go through the same table. Loads never enter it.

Covers the duplicate `struct_offset`s, the repeated branches and the fixlist
bounds re-check. Leaves `current-arena()` per push (a call, not pure) and the
always-maintained `it-index` (a dead store). Do it after alloca promotion: once
variables are SSA values, operands of identical computations are identical
and the table finds far more.
