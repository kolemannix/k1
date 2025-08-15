#!/bin/sh
set -x
set -e
cargo build --profile profiling --bin compiler
target/profiling/compiler --profile check $1
