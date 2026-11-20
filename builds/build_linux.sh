#!/bin/bash
# Runs inside builds/Dockerfile-linux with the repo read-only at /k1; see `just bundle-linux`
set -euxo pipefail

DST=$1

rsync -a --filter=':- .gitignore' /k1/ /work/
cd /work
./builds/bundle.sh "$DST"

# === glibc floor: nothing shipped may need a newer glibc than this image's ===
floor=$(ldd --version | awk 'NR == 1 {print $NF}')
check=$(mktemp -d)
tar -xzf "$DST.tar.gz" -C "$check"
need=$({ find "$check" -type f -exec objdump -T {} + 2>/dev/null || true; } | grep -o 'GLIBC_[0-9.]*' | sed 's/GLIBC_//' | sort -uV | tail -1)
rm -rf "$check"
if [ "$(printf '%s\n' "$need" "$floor" | sort -V | tail -1)" != "$floor" ]; then
  echo "bundle needs glibc $need, above the $floor floor" >&2
  exit 1
fi
echo "bundle needs glibc $need (floor $floor)"
cp "$DST.tar.gz" /out/
