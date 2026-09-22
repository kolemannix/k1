#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")"
root=$(cd ../../../.. && pwd)
mod="$root/dogfood/brotli"
cenc="$root/modules/cbrotli/vendor/enc"
runs=${RUNS:-1}
mkdir -p gen

k1 --optimize --cache false build "$mod" > gen/build.log 2>&1
cp "$mod/.k1-out/brotli" gen/brotli-opt
rm -f gen/bench-*.log

{
  echo "# Brotli encoder benchmark"
  echo
  echo "Measured $(date -u +%Y-%m-%dT%H:%M:%SZ)."
  echo
  if [[ -n ${BENCH_NOTE:-} ]]; then
    echo "$BENCH_NOTE"
    echo
  fi
  echo
  echo "- commit: $(git -C "$root" rev-parse --short HEAD)"
  echo "- machine: $(sysctl -n machdep.cpu.brand_string), $(sysctl -n hw.ncpu) cores, $(( $(sysctl -n hw.memsize) / 1000000000 )) GB"
  echo "- k1: $(k1 --version); cc: $(cc --version | head -1)"
  echo "- load average at start: $(uptime | sed 's/.*load averages*: *//')"
  echo
  echo "Each run checks byte identity with C and streaming round trips. K1 and C are interleaved per repetition; throughput is the median of seven rounds, with the best round in parentheses."
  echo
  for run in $(seq "$runs"); do
    echo "### bench run $run"
    echo
    echo "| corpus | q | K1 MB/s (best) | C MB/s (best) | K1/C | bytes in -> out | reps x rounds |"
    echo "|---|---|---|---|---|---|---|"
    (cd "$mod" && ./.k1-out/brotli bench) | tee "gen/bench-$run.log" | awk '
      /ratio/ {
        gsub(/[(),:]/, " ")
        printf "| %s | %s | %s (%s) | %s (%s) | %s | %s -> %s | %s x %s |\n", $1, $2, $4, $7, $9, $12, $14, $15, $17, $19, $22
      }'
    echo
  done
  if command -v hyperfine > /dev/null; then
    echo "### compile time"
    echo
    hyperfine --warmup 1 --runs 5 --export-markdown gen/compile.md \
      -n "k1 --optimize build dogfood/brotli" "k1 --optimize --cache false build $mod" \
      -n "k1 build dogfood/brotli" "k1 --cache false build $mod" \
      -n "cc -O2 -c compress_fragment.c compress_fragment_two_pass.c encode.c" \
        "cd gen && cc -O2 -I$root/modules/cbrotli/vendor/include -c $cenc/compress_fragment.c $cenc/compress_fragment_two_pass.c $cenc/encode.c" \
      > gen/hyperfine.log 2>&1
    cat gen/compile.md
    rm -f gen/*.o
    echo
  fi
} > results.md

tail -n +1 results.md | tail -40
