#!/bin/bash
# Show output and commands
set -x
# Exit on errors
set -e
cargo test
export RUST_BACKTRACE=1
cargo run --bin test_suite -- $1
