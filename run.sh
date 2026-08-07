#!/bin/bash
# Show output and commands
set -x
# Exit on errors
set -e
RUST_BACKTRACE=full \
  RUST_LOG=info \
  cargo run --features=llvm-sys/prefer-dynamic -- --emit-llvm $@
