# fractal

One K1 program, three ways to run it: a native terminal montage, the same
montage under wasmtime, and an interactive canvas in the browser — all from the
same source, and the last two from the same `.wasm` file.

```bash
just fractal        # native, ANSI truecolor half-blocks
just fractal-wasi   # wasm64 under wasmtime, same output
just fractal-web    # wasm64 in the browser, then open http://localhost:8088
```

## What it shows

`escape.k1` defines the `escape-set` ability — `seed` places a point's starting
orbit, `advance` steps it — and implements it three times: `mandelbrot` and
`burning-ship` (empty types, no per-instance data) and `julia` (carries its
constant). `escape-time` and `render` are generic over that ability, so the
compiler specializes one renderer into three, and adding a fourth set means
adding one `impl`.

The 256-entry color ramp is a plain global: `let PALETTE = build-palette()`.
K1 evaluates global initializers in its compile-time VM, so the interpolation
runs during compilation and the ramp lands in the data section as constants.

`log2` is computed here rather than called from `std/math`, which reaches libm —
a wasm module has no libm to link against.

## The browser side

`fractal.k1` exports two functions to JavaScript with the `export` modifier:

- `k1_frame(width, height)` sizes the RGBA framebuffer and returns its address
  in wasm linear memory
- `k1_render(kind, center-re, center-im, span, max-iter, julia-re, julia-im)`
  fills it

`index.html` supplies the WASI imports the module needs (`fd_write`,
`proc_exit`, `fd_fdstat_get`), calls `_start` so the program's own `main` runs —
its stdout appears in the page, ANSI colors and all — and then drives the two
exports, wrapping the framebuffer in an `ImageData` and blitting it to a canvas.
No copy, no glue library: the canvas reads the bytes K1 wrote.

The module is wasm64, so the browser needs memory64 (Chrome 133+, Firefox 134+,
Safari 18.4+).
