#!/bin/bash
# Show output and commands
set -x
# Exit on errors
set -e 
rm a.out || true
export RUST_BACKTRACE=1
cargo run -- --run $1

