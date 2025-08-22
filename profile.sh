#!/bin/sh
set -x
set -e
cargo build --features profile --profile profiling --bin compiler
target/profiling/compiler --profile check $1
