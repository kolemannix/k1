#!/bin/bash
# Show output and commands
set -x
# Exit on errors
set -e
export RUST_BACKTRACE=1
cargo build --features=llvm-sys/prefer-dynamic --bin test_suite
cargo build --features=llvm-sys/prefer-dynamic --bin compiler
cargo test


target/debug/test_suite $1

export RUST_LOG=info
target/debug/compiler --write-llvm check dogfood/refchess

if rg --type-add 'k1:*.k1' -c 'nocommit' -t rust -t c -t k1 .
then
    echo "Found nocommit messages"
    exit 1
fi
