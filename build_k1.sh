#!/bin/bash
# Show output and commands
set -x
# Exit on errors
set -e
export RUST_BACKTRACE=1

cargo build --bin k1
cargo build --release --bin k1
