# std/json against simdjson and serde_json

`just bench-json` builds the three lanes and runs them interleaved three
times. Numbers below are medians of three runs on an Apple M3 Max,
2026-10-02, GB/s of input (parse) or output (write), best of 20 iterations
per run. The k1 lane is `k1/bench.k1`, a plain user of `std/json`; the C++
lane is simdjson's `dom` parser and `ondemand` for the typed traversal; the
Rust lane is `serde_json::Value` and `#[derive(Deserialize)]`. Each lane
reuses its allocation between iterations and none zeroes memory inside the
timed region.

| file          | lane            | k1   | simdjson | serde_json |
|---------------|-----------------|------|----------|------------|
| twitter.json  | parse to a tree | 1.84 | 4.23     | 0.60       |
| twitter.json  | parse to structs| 1.89 | 5.72     | 1.96       |
| twitter.json  | write           | 1.70 | 1.52     | 1.61       |
| citm_catalog  | parse to a tree | 1.96 | 4.78     | 0.82       |
| citm_catalog  | write           | 1.07 | 1.15     | 1.21       |
| canada.json   | parse to a tree | 0.81 | 1.50     | 0.52       |
| canada.json   | parse to structs| 0.86 | 1.65     | 1.00       |
| canada.json   | write           | 0.56 | 0.89     | 1.04       |

The k1 reader is a single pass with SIMD helpers (whitespace runs, string
scans with `eq-lanes`/`lt-lanes`/`reduce-or`, digit runs, a
`shuffle-bytes` UTF-8 validator that only runs on strings with bytes above
0x7F) and no structural index; strings without escapes borrow the input and
containers are gathered on a scratch stack then copied once into exact-size
arena spans. The typed lane is the schema-derived decoder, which skips
unknown keys with the same scanner. canada.json is float-bound: parsing goes
through fast_float and writing through ryu, both in k1rt.
