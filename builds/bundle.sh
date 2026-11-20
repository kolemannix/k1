#!/bin/bash
set -e
set -x

DST=$1

EXCLUDES=(
  # Includes must precede the excludes for rsync to honor them
  --include 'libs/libk1rt.*'
  --include 'libs/libk1rt-freestanding.a'
  --include 'libs/libk1rt-wasm.a'
  --include 'libs/libk1rt-nocrt.a'
  --include 'libs/libfoo.*'
  --filter='.- .gitignore'
  --filter=':- .gitignore'
)

cargo build --profile release --bin k1 --bin k1_test --features=llvm-sys/force-static
cargo build --profile lsp --bin lsp --features=lsp,llvm-sys/force-static
make -C modules/core/libs clean build wasm nocrt
make -C test_src/ffi_abi_test/libs clean build
K1_HOME=$PWD target/release/k1 --optimize build tools/k1bindgen

rm -rf $DST
mkdir -p $DST
cp target/release/k1 $DST/k1
cp target/lsp/lsp $DST/k1lsp
cp target/release/k1_test $DST/k1_test
cp tools/k1bindgen/.k1-out/k1bindgen $DST/k1bindgen
for d in "$LLVM_SYS_231_PREFIX"/lib/clang/*; do
  mkdir -p "$DST/lib/clang/$(basename "$d")"
  cp -r "$d/include" "$DST/lib/clang/$(basename "$d")/"
done

rsync -a "${EXCLUDES[@]}" modules/ "$DST/modules/"
rsync -a "${EXCLUDES[@]}" test_src/ "$DST/test_src/"
cp builds/install.sh $DST

rm -f $DST.tar.gz
tar -czf $DST.tar.gz -C "$(dirname "$DST")" "$(basename "$DST")"
rm -r $DST
