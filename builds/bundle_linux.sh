set -e
set -x

TT=target/x86_64-unknown-linux-gnu/release
DST=builds/k1-linux-x86

mkdir -p $DST
cp $TT/k1 $DST/k1
cp $TT/lsp $DST/k1lsp
make -C k1lib/core/libs clean
cp -r k1lib $DST/k1lib
cp builds/install.sh $DST
tar -czvf $DST.tar.gz $DST
rm -r $DST
