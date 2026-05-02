a:
  ./run.sh sandbox/a.k1

ts1:
  ./run.sh test_src/suite1

test:
  ./test.sh

lsp:
  cargo build --features lsp --bin lsp
  cp target/debug/lsp ~/.k1/bin/k1lsp
  cp -r k1lib ~/.k1/k1lib

lsprelease:
  cargo build --profile release --features lsp --bin lsp

build-k1r:
  cargo build --release --bin k1  --features=llvm-sys/force-static

valgrind-linux:
  git pull
  cargo build --profile profiling
  valgrind --tool=callgrind --dump-instr=yes --collect-jumps=yes --callgrind-out-file=cg.out target/profiling/k1 c test_src/suite1

bundle-linux-from-linux: 
  just lsprelease
  just build-k1r
  cargo build --release --bin lsp --features=llvm-sys/force-static --features=lsp
  ./builds/bundle.sh target/release builds/k1-linux-x86

install-linux-from-linux: bundle-linux-from-linux
  tar -xzf builds/k1-linux-x86.tar.gz -C builds
  cd builds/k1-linux-x86 && ./install.sh
  rm -r builds/k1-linux-x86

bundle-macos-from-macos:
  just lsprelease
  just build-k1r
  ./builds/bundle.sh target/release builds/k1-macos

install-macos-from-macos: bundle-macos-from-macos
  tar -xzf builds/k1-macos.tar.gz -C builds
  cd builds/k1-macos && ./install.sh
  rm -r builds/k1-macos
