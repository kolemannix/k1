---
name: work-ledger
description: Work the k1 coherence ledger - make approved fixes, move TODO.md rows, test once, run the coherence pass. Use when the user says to work or act on the ledger.
---

The ledger and its row shape are in `.claude/agents/coherence-pass.md`. `status` and `note` are the user's: read every note first, never overwrite one. `open` waits for triage (act only when the user says so in chat), `approved` is a fix to make, `todo` moves to TODO.md, `done` is fixed and awaits the user's commit, `wontfix` stays so passes skip it.

1. Delete `done` rows whose fix is in HEAD.
2. Re-check each `approved` row against HEAD, say what moved or already landed, and fix the rest at every site its rule reaches. Mark it `done`, `commit: "wip"`.
3. Move `todo` rows into TODO.md and delete them.
4. Ask each `reopens` row as a question; never act on one unasked.
5. With every fix made, run `just test` once; its lanes compile src/, so leave src/ alone while it runs.
6. Run `coherence-pass` once on what changed since the last pass (`git diff` against the index when earlier rounds are staged), naming the rows it works; finish what it lists under **Finish in this change**, then `just test` again. No second pass.
7. Report rows done, rows added, net LOC, and workflow friction as proposed edits here or to the agent.
