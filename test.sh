#!/bin/bash
# Show output and commands
set -x
# Exit on errors
set -e
export RUST_BACKTRACE=1

profile=debug

cargo test --lib
cargo build --features=llvm-sys/prefer-dynamic --profile $profile --bin k1_test --bin k1


# export MallocScribble=1
make -C modules/core/libs clean build
make -C test_src/ffi_abi_test/libs clean build
# set k1 home to absolute cwd
K1_HOME=$(pwd) target/$profile/k1_test $1
# unset MallocScribble

export RUST_LOG=info
# suite1 again under --optimize
K1_HOME=$(pwd) target/$profile/k1 --optimize --cache false run test_src/suite1

target/$profile/k1 --cache false build dogfood/refchess
target/$profile/k1 --cache false build dogfood/profiling
target/$profile/k1 --cache false test  dogfood/k1bindgen
rm -rf dogfood/httpapp/.k1-out/cache
K1_HOME=$(pwd) target/$profile/k1 --cache false build dogfood/httpapp
K1_HOME=$(pwd) target/$profile/k1 --cache true  build dogfood/httpapp
K1_HOME=$(pwd) target/$profile/k1 --cache false run dogfood/logreport
K1_HOME=$(pwd) target/$profile/k1 --cache false run dogfood/comptime_parity
K1_HOME=$(pwd) target/$profile/k1 --cache false run dogfood/brotli
K1_HOME=$(pwd) target/$profile/k1 --cache false build dogfood/gengame
K1_HOME=$(pwd) target/$profile/k1 --cache false run dogfood/fractal > /dev/null
K1_HOME=$(pwd) target/$profile/k1 --cache false build dogfood/klib
make -C dogfood/klib/consumer clean run
K1_HOME=$(pwd) K1_EXE=$(pwd)/target/$profile/k1 target/$profile/k1 run dogfood/reload_test

if command -v wasmtime > /dev/null; then
    make -C modules/core/libs wasm
    K1_HOME=$(pwd) target/$profile/k1 --optimize --cache false --target wasm64-wasi run test_src/suite1
fi

if rg --type-add 'k1:*.k1' -c 'nocommit' -t rust -t c -t k1 .
then
    echo "Everything passed! But we're failing since there are nocommit messages"
    exit 1
fi
