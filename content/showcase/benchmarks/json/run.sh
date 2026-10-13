#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")"
ROOT=$(cd ../../../.. && pwd)
CORPUS=$ROOT/perf/json
K1=${k1:-k1}
export K1_HOME=${K1_HOME:-$ROOT}
mkdir -p gen
$k1 --optimize --no-cache build k1/bench.k1
cp k1/.k1-out/bench gen/json-k1
SIMDJSON=https://raw.githubusercontent.com/simdjson/simdjson/master/singleheader
[ -f gen/simdjson.h ] || curl -sLo gen/simdjson.h $SIMDJSON/simdjson.h
[ -f gen/simdjson.cpp ] || curl -sLo gen/simdjson.cpp $SIMDJSON/simdjson.cpp
[ -f gen/json-cpp ] || clang++ -O3 -std=c++20 -Igen -o gen/json-cpp cpp/bench.cpp gen/simdjson.cpp
cargo build --release --quiet --manifest-path rust/Cargo.toml --target-dir gen/rust-target
for run in 1 2 3; do
  gen/json-k1 "$CORPUS"
  gen/json-cpp "$CORPUS"
  gen/rust-target/release/json-rust "$CORPUS"
done
