#!/bin/bash
set -ex
export RUST_BACKTRACE=1
export K1_HOME=$(pwd)

profile=dev-opt
k1=target/$profile/k1
k1_single_thread=target/single-thread/$profile/k1
k1_test_filter=$1
wasmtime=$(command -v wasmtime || true)
logs=target/test-logs

rm -rf $logs
mkdir -p $logs

c_libs() {
    make -C modules/core/libs clean build
    make -C test_src/ffi_abi_test/libs clean build
    if [ -n "$wasmtime" ]; then
        make -C modules/core/libs wasm
    fi
}

cargo build --features=llvm-sys/prefer-dynamic,single-thread --profile $profile --target-dir target/single-thread --bin k1 > $logs/single-thread-build.log 2>&1 &
single_thread_build=$!
( set -e; c_libs ) > $logs/c-libs.log 2>&1 &
c_libs_build=$!
cargo build --features=llvm-sys/prefer-dynamic --profile $profile --bin k1_test --bin k1
wait $single_thread_build || { cat $logs/single-thread-build.log; exit 1; }
wait $c_libs_build || { cat $logs/c-libs.log; exit 1; }

export PATH="$(pwd)/tools/k1bindgen/.k1-out:$PATH"
$k1 --no-cache build tools/k1bindgen

unit() {
    cargo test --lib --features=llvm-sys/prefer-dynamic,lsp
    cargo check --features=llvm-sys/prefer-dynamic,lsp --bin lsp
}

test_src() {
    $k1 setup test_src/http_test
    target/$profile/k1_test $k1_test_filter
    $k1 --optimize --no-cache test --skip-static test_src/suite1
    $k1 --debug --no-cache test --skip-static test_src/suite1
    $k1_single_thread --optimize --no-cache test test_src/suite1
    if [ -n "$wasmtime" ]; then
        $k1 --optimize --no-cache --target wasm64-wasi test --skip-static test_src/suite1
    fi
}

httpapp() {
    rm -rf examples/httpapp/.k1-out/cache
    $k1 --no-cache build examples/httpapp
    $k1 build examples/httpapp
}

refchess() {
    $k1 --optimize --no-cache test examples/refchess
    if [ -n "$wasmtime" ]; then
        $k1 --optimize --no-cache --target wasm64-wasi test --skip-static examples/refchess
    fi
}

fractal() {
    $k1 --no-cache run examples/fractal > /dev/null
}

klib() {
    $k1 --no-cache build examples/klib
    make -C examples/klib/consumer clean run
}

lanes=()
pids=()
lane() {
    local name=$1
    shift
    ( set -e; start=$SECONDS; "$@"; echo "ok   $name ($((SECONDS - start))s)" ) > $logs/$name.log 2>&1 &
    lanes+=($name)
    pids+=($!)
}

lane unit unit
lane test_src test_src
lane refchess refchess
lane k1bindgen $k1 --no-cache test tools/k1bindgen
lane uv $k1 --no-cache test modules/uv
lane http $k1 --no-cache test modules/http
lane httpapp httpapp
lane sqlite $k1 --no-cache test modules/sqlite
lane profiling $k1 --no-cache build perf/profiling
lane logreport $k1 --no-cache run examples/logreport
lane brotli $k1 --optimize --no-cache run examples/brotli_parity
if [ "$(uname -s)" = Darwin ]; then
    lane gengame $k1 --no-cache build examples/gengame
fi
lane fractal fractal
lane klib klib
lane reload_test env K1_EXE=$(pwd)/$k1 $k1 run examples/reload_test

set +x
failed=()
for i in "${!pids[@]}"; do
    if wait ${pids[$i]}; then
        tail -n 1 $logs/${lanes[$i]}.log
    else
        failed+=(${lanes[$i]})
    fi
done
for name in "${failed[@]}"; do
    echo "FAIL $name, $logs/$name.log:"
    cat $logs/$name.log
done
[ ${#failed[@]} -eq 0 ]

if rg --type-add 'k1:*.k1' -c 'nocommit' -t rust -t c -t k1 .
then
    echo "Everything passed! But we're failing since there are nocommit messages"
    exit 1
fi
