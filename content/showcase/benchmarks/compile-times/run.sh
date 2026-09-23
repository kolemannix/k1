#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")"
HERE=$PWD
REPO=$(cd ../../../.. && pwd)
RUNS=${RUNS:-3}
SIZES=${SIZES:-"10 100 300"}
MODES=${MODES:-"check debug opt"}
RES=$HERE/gen/results
rm -rf "$RES"
mkdir -p "$RES" gen/brotli-c

export GOCACHE=$HERE/gen/gocache
export DOTNET_CLI_TELEMETRY_OPTOUT=1
export DOTNET_NOLOGO=1
GOCACHE_COLD=$HERE/gen/gocache-cold
ZIG_GLOBAL=$HERE/gen/zig-global
ZIG_GLOBAL_COLD=$HERE/gen/zig-global-cold
ZIG_LOCAL=$HERE/gen/zig-local
ZIG="zig build-exe -target aarch64-macos --cache-dir $ZIG_LOCAL"
BRO_INC="-I$REPO/modules/cbrotli/vendor/enc -I$REPO/modules/cbrotli/vendor/include"
BRO_SRC=""
for f in compress_fragment.c compress_fragment_two_pass.c encode.c fast_log.c; do
  BRO_SRC="$BRO_SRC $REPO/dogfood/brotli/vendor/$f"
done

{
  echo "date: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "cpu: $(sysctl -n machdep.cpu.brand_string), $(sysctl -n hw.ncpu) cores, $(( $(sysctl -n hw.memsize) / 1024 / 1024 / 1024 )) GiB"
  echo "os: macOS $(sw_vers -productVersion)"
  echo "k1: $(k1 --version)"
  echo "clang: $(clang --version | head -1)"
  echo "rustc: $(rustc --version)"
  echo "go: $(go version)"
  echo "zig: $(zig version)"
  echo "javac: $(javac -version 2>&1)"
  echo "dotnet: sdk $(dotnet --version)"
  echo "hyperfine: $(hyperfine --version)"
  echo "runs: $RUNS"
  echo "sizes: $SIZES"
  echo "modes: $MODES"
  echo "load-start: $(uptime | sed 's/.*load averages: //')"
} > "$RES/versions.txt"
cat "$RES/versions.txt"

hf() {
  local name=$1; shift
  echo "== $name: ${*: -1}"
  hyperfine -N --warmup 1 --min-runs "$RUNS" --export-json "$RES/$name.json" "$@" | grep -E 'Time|Range'
}

k1mode() {
  case $1 in
    check) echo "check" ;;
    debug) echo "build" ;;
    opt) echo "--optimize build" ;;
  esac
}

cflag() {
  case $1 in
    check) echo "-fsyntax-only" ;;
    debug) echo "-O0 -c" ;;
    opt) echo "-O2 -c" ;;
  esac
}

srcof() {
  case $1 in
    k1|k1warm) echo "$2.k1" ;;
    c) echo "$2.c" ;;
    cpp) echo "$2.cpp" ;;
    cpp-hand-vec) echo "$2-hand-vec.cpp" ;;
    rs) echo "$2.rs" ;;
    go|gocold) echo "$2.go" ;;
    zig|zigcold) echo "$2.zig" ;;
    cs) echo "$2.cs" ;;
    java) echo "$(dirname "$2")/$(basename "$2" | awk '{print toupper(substr($0, 1, 1)) substr($0, 2)}').java" ;;
  esac
}

cmd() {
  local src=$3 out=$4
  case "$1-$2" in
    k1-*) echo "k1 --no-cache $(k1mode "$2") $src" ;;
    k1warm-*) echo "k1 $(k1mode "$2") $src" ;;
    c-check) echo "clang -fsyntax-only $src" ;;
    c-debug) echo "clang -O0 -o ${out}_c $src" ;;
    c-opt) echo "clang -O2 -o ${out}_c $src" ;;
    cpp-check|cpp-hand-vec-check) echo "clang++ -std=c++20 -fsyntax-only $src" ;;
    cpp-debug|cpp-hand-vec-debug) echo "clang++ -std=c++20 -O0 -o ${out}_$1 $src" ;;
    cpp-opt|cpp-hand-vec-opt) echo "clang++ -std=c++20 -O2 -o ${out}_$1 $src" ;;
    rs-check) echo "rustc --edition 2021 --emit=metadata -o ${out}.rmeta $src" ;;
    rs-debug) echo "rustc --edition 2021 -C opt-level=0 -o ${out}_rs $src" ;;
    rs-opt) echo "rustc --edition 2021 -C opt-level=3 -o ${out}_rs $src" ;;
    go-debug) echo "go build -o ${out}_go $src" ;;
    gocold-debug) echo "env GOCACHE=$GOCACHE_COLD go build -o ${out}_go $src" ;;
    zig-check) echo "$ZIG --global-cache-dir $ZIG_GLOBAL -fno-emit-bin $src" ;;
    zig-debug) echo "$ZIG --global-cache-dir $ZIG_GLOBAL -O Debug -femit-bin=${out}_zig $src" ;;
    zig-opt) echo "$ZIG --global-cache-dir $ZIG_GLOBAL -O ReleaseFast -femit-bin=${out}_zig $src" ;;
    zigcold-check) echo "$ZIG --global-cache-dir $ZIG_GLOBAL_COLD -fno-emit-bin $src" ;;
    zigcold-debug) echo "$ZIG --global-cache-dir $ZIG_GLOBAL_COLD -O Debug -femit-bin=${out}_zig $src" ;;
    zigcold-opt) echo "$ZIG --global-cache-dir $ZIG_GLOBAL_COLD -O ReleaseFast -femit-bin=${out}_zig $src" ;;
    java-debug) echo "javac -d ${out}_java $src" ;;
    cs-opt) echo "dotnet build -c Release ${out}.csproj" ;;
    *) return 1 ;;
  esac
}

prep() {
  local src=$2 out=$3
  case $1 in
    go) echo "./prep.sh go $src ${out}_go" ;;
    gocold) echo "./prep.sh rm $GOCACHE_COLD" ;;
    k1warm) echo "./prep.sh nonce $src" ;;
    zig) echo "./prep.sh rm $ZIG_LOCAL" ;;
    zigcold) echo "./prep.sh rm $ZIG_LOCAL $ZIG_GLOBAL_COLD" ;;
    java) echo "./prep.sh rm ${out}_java" ;;
    cs) echo "./prep.sh nonce $src" ;;
    *) echo "true" ;;
  esac
}

bench() {
  local langs=$1 tag=$2 src_prefix=$3
  for lang in $langs; do
    local src out=$src_prefix
    src=$(srcof "$lang" "$src_prefix")
    for mode in $MODES; do
      local c
      c=$(cmd "$lang" "$mode" "$src" "$out") || continue
      hf "$lang-$mode-$tag" --prepare "$(prep "$lang" "$src" "$out")" "$c"
    done
  done
}

python3 gen.py hello gen/hello
bench "k1 c cpp rs go zig java cs gocold zigcold" hello gen/hello/hello

for n in $SIZES; do
  python3 gen.py "$n" "gen/n$n"
  bench "k1 c cpp cpp-hand-vec rs go zig java cs k1warm" "$n" "gen/n$n/stress"
done

big=${SIZES##* }
for mode in $MODES; do
  k1 --no-cache --chatty $(k1mode "$mode") "gen/n$big/stress.k1" 2> "$RES/k1-chatty-$mode-$big.txt" > /dev/null
done

for mode in $MODES; do
  hf "brotli-k1-$mode" --prepare true "k1 --no-cache $(k1mode "$mode") $REPO/dogfood/brotli"
done
(
  cd gen/brotli-c
  for mode in $MODES; do
    hf "brotli-c-$mode" --prepare true "clang $(cflag "$mode") $BRO_INC $BRO_SRC"
    hf "brotli-cpar-$mode" --prepare true "$HERE/par-cc.sh $(cflag "$mode")"
  done
)
last=${MODES##* }
k1 --no-cache --chatty $(k1mode "$last") "$REPO/dogfood/brotli" 2> "$RES/k1-chatty-brotli-$last.txt" > /dev/null
for f in $BRO_SRC; do clang -M $BRO_INC "$f"; done | tr ' \\' '\n\n' | grep -E '\.(h|c)$' | sed "s|^$REPO/||" | grep -v '^/' | sort -u > "$RES/brotli-c-inputs.txt"

echo "load-end: $(uptime | sed 's/.*load averages: //')" >> "$RES/versions.txt"
python3 report.py
echo "wrote results.md"
