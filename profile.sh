#!/bin/sh
set -x
set -e
cargo build --features profile --profile profiling --bin k1
target/profiling/k1 --profile check $1
