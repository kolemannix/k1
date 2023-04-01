#!/bin/bash
set -x
rm a.out
export RUST_BACKTRACE=1
cargo run -- resources/test_src/$1
clang++ -g $1.o -o $1.out
./$1.out
