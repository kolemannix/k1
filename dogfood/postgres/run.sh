#!/bin/zsh
set -e
set -x

link_flags=()
if [[ -d /opt/homebrew/opt/libpq/lib ]]; then
  link_flags+=(-c -L/opt/homebrew/opt/libpq/lib)
fi
if [[ -d /usr/local/opt/libpq/lib ]]; then
  link_flags+=(-c -L/usr/local/opt/libpq/lib)
fi
if [[ ${#link_flags[@]} -eq 0 ]]; then
  echo "libpq not found in Homebrew locations" >&2
  exit 1
fi

/Users/knix/dev/k1/target/debug/k1 \
  --write-llvm \
  "${link_flags[@]}" \
  -c -lpq \
  run /Users/knix/dev/k1/dogfood/postgres
