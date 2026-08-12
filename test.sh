#!/bin/bash
# Show output and commands
set -x
# Exit on errors
set -e
export RUST_BACKTRACE=1
cargo test --lib
cargo build --features=llvm-sys/prefer-dynamic --bin k1_test --bin k1


# export MallocScribble=1
make -C modules/core/libs clean build
make -C test_src/ffi_abi_test/libs clean build
# set k1 home to absolute cwd
K1_HOME=$(pwd) target/debug/k1_test $1
# unset MallocScribble

export RUST_LOG=info
# Correctness gates compile cold, never from cached state
target/debug/k1 --emit-llvm --cache false build dogfood/refchess
target/debug/k1 --emit-llvm --cache false build dogfood/profiling
target/debug/k1 --cache false test  dogfood/k1bindgen
# Exercises the module system end-to-end: deps (http -> libuv), setup steps
# (cmake/cc/k1bindgen on first compile of a machine), cross-module linking.
# First build starts from an empty cache (cold compile, writes snapshots);
# second build restores them and re-links, covering the disk cache end-to-end
rm -rf dogfood/httpapp/.k1-out/cache
K1_HOME=$(pwd) target/debug/k1 build dogfood/httpapp
K1_HOME=$(pwd) target/debug/k1 build dogfood/httpapp
K1_HOME=$(pwd) target/debug/k1 run dogfood/brotli

if rg --type-add 'k1:*.k1' -c 'nocommit' -t rust -t c -t k1 .
then
    echo "Everything passed! But we're failing since there are nocommit messages"
    exit 1
fi
