ts1:
  ./run.sh test_src/suite1

test:
  ./test.sh

lsp:
  cargo build --features lsp --bin lsp

lsprelease:
  cargo build --profile release --features lsp --bin lsp
