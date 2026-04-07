set -e
set -x

# target triple
TT=x86_64-unknown-linux-gnu

cross build --bin k1 --profile release --target=$TT --features=llvm-sys/force-static
cross build --bin lsp --features lsp --profile release --target=$TT --features=llvm-sys/force-static
