#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p gen
k1 --optimize --no-cache build k1/int_strings.k1
cp k1/.k1-out/int_strings gen/int-strings-k1
clang++ -O3 -std=c++17 -o gen/int-strings-cpp cpp/int_strings.cpp
for run in 1 2 3; do
  gen/int-strings-k1
  gen/int-strings-cpp
done
