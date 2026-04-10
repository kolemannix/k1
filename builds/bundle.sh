set -e
set -x

TT=$1
DST=$2

mkdir -p $DST
cp $TT/k1 $DST/k1
cp $TT/lsp $DST/k1lsp
make -C k1lib/core/libs clean
cp -r k1lib $DST/k1lib
cp builds/install.sh $DST
tar -czvf $DST.tar.gz -C "$(dirname "$DST")" "$(basename "$DST")"
rm -r $DST
