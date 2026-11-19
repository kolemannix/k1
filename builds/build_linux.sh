#!/bin/bash
# Runs inside builds/Dockerfile-linux with the repo at /k1; see `just bundle-linux`
set -euxo pipefail

DST=$1
OUT=target/linux
export CARGO_TARGET_DIR=$OUT

cd /k1
cargo build --profile release --bin k1 --bin k1_test --features=llvm-sys/force-static
cargo build --profile lsp --bin lsp --features=lsp,llvm-sys/force-static
K1_HOME=/k1 $OUT/release/k1 --optimize build tools/k1bindgen
./builds/bundle.sh $OUT/release $OUT/lsp/lsp "$DST"

# === glibc floor: nothing shipped may need a newer glibc than this image's ===
floor=$(ldd --version | head -1 | awk '{print $NF}')
check=$(mktemp -d)
tar -xzf "$DST.tar.gz" -C "$check"
need=$(find "$check" -type f -exec objdump -T {} + 2>/dev/null | grep -o 'GLIBC_[0-9.]*' | sed 's/GLIBC_//' | sort -uV | tail -1)
rm -rf "$check"
if [ "$(printf '%s\n' "$need" "$floor" | sort -V | tail -1)" != "$floor" ]; then
  echo "bundle needs glibc $need, above the $floor floor" >&2
  exit 1
fi
echo "bundle needs glibc $need (floor $floor)"
