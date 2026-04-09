ts1:
  ./run.sh test_src/suite1

test:
  ./test.sh

lsp:
  cargo build --features lsp --bin lsp

lsprelease:
  cargo build --profile release --features lsp --bin lsp

valgrind-linux:
  git pull
  cargo build --profile profiling
  valgrind --tool=callgrind --dump-instr=yes --collect-jumps=yes --callgrind-out-file=cg.out target/profiling/k1 c test_src/suite1
