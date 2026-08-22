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
# suite1 again under --optimize
K1_HOME=$(pwd) target/debug/k1 --optimize --cache false run test_src/suite1

if command -v wasmtime > /dev/null; then
    make -C modules/core/libs wasm
    K1_HOME=$(pwd) target/debug/k1 --cache false --target wasm64-wasi run test_src/suite1
    K1_HOME=$(pwd) target/debug/k1 --optimize --cache false --target wasm64-wasi run test_src/suite1
    K1_HOME=$(pwd) target/debug/k1 --optimize --cache false --target wasm64-wasi run dogfood/fractal > /dev/null
fi

if docker info > /dev/null 2>&1; then
    just ts-freestanding
fi

target/debug/k1 --emit-llvm --cache false build dogfood/refchess
target/debug/k1 --emit-llvm --cache false build dogfood/profiling
target/debug/k1 --cache false test  dogfood/k1bindgen
rm -rf dogfood/httpapp/.k1-out/cache
K1_HOME=$(pwd) target/debug/k1 build dogfood/httpapp
K1_HOME=$(pwd) target/debug/k1 build dogfood/httpapp
K1_HOME=$(pwd) target/debug/k1 run dogfood/logreport
K1_HOME=$(pwd) target/debug/k1 run dogfood/comptime_parity
K1_HOME=$(pwd) target/debug/k1 run dogfood/brotli
K1_HOME=$(pwd) target/debug/k1 build dogfood/gengame
K1_HOME=$(pwd) target/debug/k1 run dogfood/fractal > /dev/null
K1_HOME=$(pwd) target/debug/k1 --cache false build dogfood/klib
make -C dogfood/klib/consumer clean run
K1_HOME=$(pwd) K1_EXE=$(pwd)/target/debug/k1 target/debug/k1 run dogfood/reload_test

if rg --type-add 'k1:*.k1' -c 'nocommit' -t rust -t c -t k1 .
then
    echo "Everything passed! But we're failing since there are nocommit messages"
    exit 1
fi
