#!/bin/bash
# Show output and commands
set -x
# Exit on errors
set -e
export RUST_BACKTRACE=1
cargo test --lib
cargo build --features=llvm-sys/prefer-dynamic --bin k1_test --bin k1


export MallocScribble=1
make -C k1lib/core/libs clean build
make -C test_src/ffi_abi_test/libs clean build
target/debug/k1_test $1
unset MallocScribble

export RUST_LOG=info
target/debug/k1 --emit-llvm build dogfood/refchess
target/debug/k1 --emit-llvm build dogfood/profiling
target/debug/k1             test  dogfood/k1bindgen

if rg --type-add 'k1:*.k1' -c 'nocommit' -t rust -t c -t k1 .
then
    echo "Found nocommit messages"
    exit 1
fi
