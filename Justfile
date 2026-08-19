os := os()
bundle-name := if os == "linux" {
  "k1-linux-x86"
} else if os == "macos" {
  "k1-macos"
} else {
  error("Unsupported OS: " + os)
}
export LLVM_SYS_211_PREFIX := env_var_or_default("LLVM_SYS_211_PREFIX", "./llvm/install-llvm")

run-frag +args:
  RUST_BACKTRACE=full RUST_LOG=info cargo run --features=llvm-sys/prefer-dynamic --bin k1 -- {{args}}

# Run the current scratch file; use for writing reproducers or not-yet-working code, then move it to test_src/ when done or fixed
a:
  just run-frag run sandbox

# fastest path to checking compiler correctness; suite1 covers most features.
ts1:
  just run-frag --cache false run test_src/suite1
  just run-frag --optimize --cache false run test_src/suite1

ts1-wasm:
  make -C modules/core/libs wasm
  just run-frag --cache false --target wasm64 run test_src/suite1
  just run-frag --optimize --cache false --target wasm64 run test_src/suite1

# Cross-built freestanding k1 library consumed by a C bootstrap, run in docker.
# The clang/ld.lld/llvm-nm binaries here play the CONSUMER's toolchain, not k1's
ts-freestanding:
  make -C modules/core/libs nocrt
  just run-frag --no-std --target linux-intel64 --cache false build dogfood/freestanding_lib
  llvm/install-llvm/bin/llvm-nm --undefined-only dogfood/freestanding_lib/.k1-out/freestanding_lib.o | awk 'END { exit NR != 0 }'
  llvm/install-llvm/bin/clang --target=x86_64-unknown-linux-gnu -ffreestanding -nostdinc -O2 -fno-stack-protector -c dogfood/freestanding_lib/consumer/consumer.c -o dogfood/freestanding_lib/.k1-out/consumer.o
  llvm/install-llvm/bin/ld.lld -z separate-loadable-segments dogfood/freestanding_lib/.k1-out/consumer.o dogfood/freestanding_lib/.k1-out/freestanding_lib.o -o dogfood/freestanding_lib/.k1-out/consumer_bin
  docker run --rm --platform linux/amd64 -v {{justfile_directory()}}/dogfood/freestanding_lib/.k1-out:/w alpine:latest /w/consumer_bin | grep -x 103

# Dev loop for a reloadable app: rebuild on source change
watch dir:
  watchexec -w {{dir}} -e k1 -- target/debug/k1 build {{dir}}

# exhaustive path; notably runs larger projects in dogfood
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

profile-suite1: build-profile
  hyperfine --warmup 100 'K1_HOME=. target/profiling/k1 --cache false c test_src/suite1'

profile-stress: build-profile
  python3 perf/gen_stress.py 3600
  hyperfine --warmup 2 'K1_HOME=. target/profiling/k1 --cache false c perf/stress100'

valgrind-linux:
  git pull
  cargo build --profile profiling
  valgrind --tool=callgrind --dump-instr=yes --collect-jumps=yes --callgrind-out-file=cg.out target/profiling/k1 c test_src/suite1

bundle:
  just lsprelease
  just build-r
  cargo build --profile release --bin k1_test
  make -C modules/core/libs build wasm nocrt
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
