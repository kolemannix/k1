set -e
set -x

TT=$1
DST=$2

mkdir -p $DST
cp $TT/k1 $DST/k1
cp $TT/lsp $DST/k1lsp
cp $TT/k1_test $DST/k1_test

# Hack to not ship compiled k1lib
make -C k1lib/core/libs clean
cp -a k1lib/. "$DST/k1lib/"
cp -a test_src/. "$DST/test_src/"
cp builds/install.sh $DST
rm $DST.tar.gz
tar -czvf $DST.tar.gz -C "$(dirname "$DST")" "$(basename "$DST")"
rm -r $DST

# Hack to not ship compiled k1lib
make -C k1lib/core/libs
