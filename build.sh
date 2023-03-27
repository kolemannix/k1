#!/bin/bash
set -x
rm a.out
cargo run --release -- resources/test_src/codegen_fn.nx
clang++ -g resources/test_src/codegen_fn.nx.out -o a.out
./a.out
