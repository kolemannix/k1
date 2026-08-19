#!/bin/bash
set -e
set -x

TT=$1
DST=$2

EXCLUDES=(
  --exclude '.k1-out'
  # Includes must precede the excludes for rsync to honor them
  --include 'libs/libk1rt.*'
  --include 'libs/libk1rt-freestanding.a'
  --include 'libs/libk1rt-wasm.a'
  --include 'libs/libk1rt-nocrt.a'
  --exclude 'libs/*.a'
  --exclude 'libs/*.so'
  --exclude 'libs/*.dylib'
  --exclude 'libs/*.o'
  --exclude '*_module_dump.txt'
  --exclude 'k1_lsp.log*'
)

rm -rf $DST
mkdir -p $DST
cp $TT/k1 $DST/k1
cp $TT/lsp $DST/k1lsp
cp $TT/k1_test $DST/k1_test

rsync -a "${EXCLUDES[@]}" modules/ "$DST/modules/"
rsync -a "${EXCLUDES[@]}" test_src/ "$DST/test_src/"
cp builds/install.sh $DST

rm -f $DST.tar.gz
tar -czf $DST.tar.gz -C "$(dirname "$DST")" "$(basename "$DST")"
rm -r $DST
