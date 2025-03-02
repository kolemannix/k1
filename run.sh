#!/bin/bash
# Show output and commands
set -x
# Exit on errors
set -e
export RUST_BACKTRACE=1
RUST_LOG=info cargo run --features=llvm-sys/prefer-dynamic -- --write-llvm run $@
