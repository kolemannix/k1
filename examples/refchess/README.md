# refchess

A reference UCI chess engine in K1: bitboards, magic sliding attacks, strictly
legal move generation, and a plain alpha-beta search. It passes the standard
perft suites and runs unchanged natively and as wasm64 under wasmtime.

```bash
just refchess        # perft + search suites, native
just refchess-wasm   # the same suites under wasmtime
just refchess-uci    # UCI on stdin
echo "go perft 6" | examples/refchess/.k1-out/refchess
```

## Engine

- `bitboard.k1`: leaper tables, magic rook/bishop attacks, `BETWEEN`/`LINE`.
- `position.k1`: position, FEN, Zobrist keys, copy-make `make`.
- `movegen.k1`: legal moves from checkers and pin rays; en passant is
  checked by re-testing the king after the capture.
- `perft.k1`, `search.k1`, `eval.k1`: perft/divide; iterative deepening PVS
  with a transposition table, killers, MVV-LVA, quiescence and a check
  extension; material plus tapered piece-square tables.
- `uci.k1`, `input.k1`: the protocol loop and a zero-copy line reader.

Besides the UCI commands, `d` prints the board, FEN, key and arena usage, and
`go perft N` prints a divide in Stockfish's format (`autoperft.py` reads it).

## Compile time

Every table is a global initializer, so the compile-time VM builds it and the
binary carries it as data: knight/king/pawn attacks, `BETWEEN`/`LINE`, the
Zobrist keys, the piece-square tables, and the 107,648-entry magic attack
table. The magic factors are constants; the table build fails the compile
if any of them has a destructive collision.

## Memory

Nothing in the engine calls a general-purpose allocator. Each lifetime gets
an arena:

| arena     | holds                               | released                        |
|-----------|-------------------------------------|---------------------------------|
| `session` | the stdin line buffer               | never                           |
| `game`    | position history keys               | `reset` on every `position`     |
| `command` | the ambient arena for one UCI line  | `reset` after every line        |
| `ply`     | move lists and move scores per node | `pop-to-mark` when a node exits |
| `tt`      | the transposition table             | `reset` on `ucinewgame`, `release` on `setoption Hash` |

The `ply` arena is the search stack. `gen-moves` takes it as a context
parameter, and each node marks it, generates into a list that grows in
place at the top of the arena, and pops back on the way out. The test
suite checks that it is empty again after every perft and search. It is
fixed-size: a mark/pop stack must never chain into a new chunk.

Input lines are views into the fixed `session` buffer: reading a command
allocates nothing, and a search can check stdin for `stop` without
blocking (`core/platform/io/poll-readable`, `poll(2)` natively and
`poll_oneoff` on WASI).
