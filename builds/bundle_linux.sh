set -e
set -x

# target triple
TT=x86_64-unknown-linux-gnu
DST=builds/k1-linux-x86

cross build --bin k1 --profile release --target=$TT --features=llvm-sys/force-static
# TODO: LSP doesnt even need llvm
cross build --bin lsp --features lsp --profile release --target=$TT --features=llvm-sys/force-static

mkdir -p $DST
cp target/$TT/release/k1 $DST/k1
cp target/$TT/release/lsp $DST/k1lsp
make -C k1lib/core/libs clean
cp -r k1lib $DST/k1lib
cp builds/install.sh $DST
tar -czvf $DST.tar.gz $DST
rm -r $DST
