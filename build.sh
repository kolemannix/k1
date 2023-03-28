#!/bin/bash
set -x
rm a.out
export RUST_BACKTRACE=1
cargo run -- resources/test_src/codegen_fn.nx
clang++ -g resources/test_src/codegen_fn.nx.out -o a.out
./a.out
