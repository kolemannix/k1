a:
  ./run.sh sandbox

ts1:
  ./run.sh test_src/suite1

test:
  ./test.sh

lsp:
  cargo build --features lsp --bin lsp

  rm ~/.k1/bin/k1lsp
  cp target/debug/lsp ~/.k1/bin/k1lsp

  just install-k1lib

lsprelease:
  cargo build --profile release --features lsp --features=llvm-sys/force-static --bin lsp

build-k1r:
  cargo build --release --bin k1  --features=llvm-sys/force-static

build-k1-profile:
  cargo build --profile profiling --bin k1 --features=llvm-sys/force-static

valgrind-linux:
  git pull
  cargo build --profile profiling
  valgrind --tool=callgrind --dump-instr=yes --collect-jumps=yes --callgrind-out-file=cg.out target/profiling/k1 c test_src/suite1

bundle-linux: 
  just lsprelease
  just build-k1r
  cargo build --profile release --bin k1_test
  ./builds/bundle.sh target/release builds/k1-linux-x86

install-linux: bundle-linux
  tar -xzf builds/k1-linux-x86.tar.gz -C builds
  cd builds/k1-linux-x86 && ./install.sh

bundle-macos:
  just lsprelease
  just build-k1r
  cargo build --profile release --bin k1_test
  ./builds/bundle.sh target/release builds/k1-macos

install-macos: bundle-macos
  tar -xzf builds/k1-macos.tar.gz -C builds
  cd builds/k1-macos && ./install.sh

install-k1lib:
  # rm -r ~/.k1/k1lib
  cp -r k1lib/. ~/.k1/k1lib

repl +args:
  RUST_BACKTRACE=1 RUST_LOG=info \
    cargo run --features=llvm-sys/prefer-dynamic -- repl {{args}}
