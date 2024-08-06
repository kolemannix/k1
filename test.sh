#!/bin/bash
# Show output and commands
set -x
# Exit on errors
set -e
export RUST_BACKTRACE=1
cargo test
cargo run --bin test_suite -- $1

./run.sh dogfood/a_proj

if rg -c 'nocommit' -t rust -t c .
then
    echo "Found nocommit messages"
    exit 1
fi
