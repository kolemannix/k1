#!/bin/bash
set -e
cd "$(dirname "$0")/.."
export K1_HOME="$(pwd)"
k1=${K1:-target/debug/k1}
make -C modules/core/libs wasm
$k1 -D web --no-cache build examples/refchess
cp examples/refchess/.k1-out/refchess examples/refchess/refchess.wasm
python3 site/gen-data.py
python3 site/gen-highlight.py
content/showcase/check.sh
hugo --source site --minify "$@"
