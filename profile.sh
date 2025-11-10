#!/bin/sh
set -x
set -e
cargo build --features profile --profile profiling --bin compiler
target/profiling/k1 --profile check $1
