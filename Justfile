os := os()
bundle-name := if os == "linux" {
  "k1-linux-x86"
} else if os == "macos" {
  "k1-macos"
} else {
  error("Unsupported OS: " + os)
}
export LLVM_SYS_231_PREFIX := env_var_or_default("LLVM_SYS_231_PREFIX", "./llvm/install-llvm")

run-frag +args:
  RUST_BACKTRACE=full RUST_LOG=info cargo run --features=llvm-sys/prefer-dynamic --bin k1 -- {{args}}

# Run the current scratch file; use for writing reproducers or not-yet-working code, then move it to test_src/ when done or fixed
a:
  just run-frag run sandbox

# fastest path to checking compiler correctness; suite1 covers most features.
ts1:
  cargo build --features=llvm-sys/prefer-dynamic --bin k1
  target/debug/k1            --no-cache run test_src/suite1
  target/debug/k1 --optimize --no-cache run test_src/suite1

ts1-wasm:
  make -C modules/core/libs wasm
  just run-frag --no-cache --target wasm64-wasi run test_src/suite1
  just run-frag --optimize --no-cache --target wasm64-wasi run test_src/suite1

# a simple one-shot ai-generated k1 parser
slophost:
  cargo build --features=llvm-sys/prefer-dynamic --bin k1
  target/debug/k1 test examples/selfhost
  target/debug/k1 build examples/selfhost
  examples/selfhost/.k1-out/selfhost $(find {{justfile_directory()}}/modules {{justfile_directory()}}/test_src {{justfile_directory()}}/examples -name '*.k1' -not -path '*/.k1-out/*' -not -path '*vendor*' -not -path '*/selfhost/*' -not -name 'fn_lib_requires_extern_fail.k1')

# The fractal demo: same program natively, under wasmtime, and on a canvas
fractal:
  just run-frag run examples/fractal

fractal-wasi:
  make -C modules/core/libs wasm
  just run-frag -D web run examples/fractal

fractal-web:
  make -C modules/core/libs wasm
  just run-frag -D web build examples/fractal
  python3 -m http.server -d examples 8088

# The reference chess engine: perft and search suites natively and under wasmtime, then UCI on stdin
refchess:
  just run-frag --optimize test examples/refchess

refchess-wasm:
  make -C modules/core/libs wasm
  just run-frag --optimize --target wasm64-wasi test examples/refchess

refchess-wasm-web:
  make -C modules/core/libs wasm
  just run-frag -D web build examples/refchess
  cp examples/refchess/.k1-out/refchess examples/refchess/refchess.wasm

refchess-web: refchess-wasm-web
  python3 -m http.server -d examples 8088

refchess-uci:
  just run-frag --optimize run examples/refchess

# Cross-built freestanding k1 library consumed by a C bootstrap, run in docker.
# The clang/ld.lld/llvm-nm binaries here play the CONSUMER's toolchain, not k1's
ts-freestanding:
  make -C modules/core/libs nocrt
  just run-frag --no-cache build examples/freestanding_lib
  llvm/install-llvm/bin/llvm-nm --undefined-only examples/freestanding_lib/.k1-out/freestanding_lib.o | awk '$2 !~ /^k1_platform_/ && $2 !~ /^(memcpy|memmove|memset|memcmp|bcmp)$/' | awk 'END { exit NR != 0 }'
  llvm/install-llvm/bin/clang --target=x86_64-unknown-linux-gnu -ffreestanding -nostdinc -O2 -fno-stack-protector -c examples/freestanding_lib/consumer/consumer.c -o examples/freestanding_lib/.k1-out/consumer.o
  llvm/install-llvm/bin/ld.lld -z separate-loadable-segments examples/freestanding_lib/.k1-out/consumer.o examples/freestanding_lib/.k1-out/freestanding_lib.o modules/core/libs/libk1rt-nocrt.a -o examples/freestanding_lib/.k1-out/consumer_bin
  docker run --rm --platform linux/amd64 -v {{justfile_directory()}}/examples/freestanding_lib/.k1-out:/w alpine:latest /w/consumer_bin | grep -cxE 'k1 sum: 100|103' | grep -x 2

# Dev loop for a reloadable app: rebuild on source change
watch dir:
  watchexec -w {{dir}} -e k1 -- target/debug/k1 build {{dir}}

# The language site: hugo dev server, and the full build (wasm demo, benchmark data, showcase check, hugo)
site: refchess-wasm-web
  hugo server --source site --port 1313 --buildDrafts

site-build:
  cargo build --features=llvm-sys/prefer-dynamic --bin k1
  site/build.sh

# exhaustive path; notably builds and runs every project in examples
test:
  ./test.sh

lsp:
  cargo build --features lsp --bin lsp

  rm ~/.k1/bin/k1lsp
  cp target/debug/lsp ~/.k1/bin/k1lsp

  just install-modules

lsprelease:
  cargo build --profile release --features lsp --features=llvm-sys/force-static --bin lsp

build-r:
  cargo build --release --bin k1  --features=llvm-sys/force-static

build-profile:
  cargo build --profile profiling --bin k1 --features=llvm-sys/force-static

# std/json against simdjson and serde_json on the perf/json corpus
bench-json:
  cargo build --features=llvm-sys/prefer-dynamic --bin k1
  K1=target/debug/k1 content/showcase/benchmarks/json/run.sh

profile-suite1: build-profile
  hyperfine --warmup 100 'K1_HOME=. target/profiling/k1 --no-cache c test_src/suite1'

profile-stress: build-profile
  python3 perf/gen_stress.py 3600
  hyperfine --warmup 2 'K1_HOME=. target/profiling/k1 --no-cache c perf/stress100'
  K1_HOME=. target/profiling/k1 --no-cache --chatty c perf/stress100 2> perf/latest_stress.txt

# metaprogramming stress: 200 #meta define-soa expansions in one file
profile-soa: build-profile
  hyperfine --warmup 2 'K1_HOME=. target/profiling/k1 --no-cache c perf/soa_x_200.k1'

profile-magic: build-profile
  hyperfine --warmup 1 'K1_HOME=. target/profiling/k1 --no-cache c perf/magic.k1'
  K1_HOME=. target/profiling/k1 --no-cache --chatty c perf/magic.k1 2>&1 | grep -E '^\s+run\s|ops:'

valgrind-linux:
  git pull
  cargo build --profile profiling
  valgrind --tool=callgrind --dump-instr=yes --collect-jumps=yes --callgrind-out-file=cg.out target/profiling/k1 c test_src/suite1

bundle:
  just lsprelease
  just build-r
  cargo build --profile release --bin k1_test
  make -C modules/core/libs build wasm nocrt
  K1_HOME={{justfile_directory()}} target/release/k1 --optimize build tools/k1bindgen
  ./builds/bundle.sh target/release builds/{{bundle-name}}

install: bundle
  tar -xzf builds/{{bundle-name}}.tar.gz -C builds
  cd builds/{{bundle-name}} && ./install.sh

install-modules:
  rsync -a --exclude .k1-out modules/ ~/.k1/modules/

repl +args:
  RUST_BACKTRACE=1 RUST_LOG=info \
    cargo run --features=llvm-sys/prefer-dynamic -- repl {{args}}

server +args:
  RUST_BACKTRACE=1 cargo run --bin k1 -- server {{args}}
