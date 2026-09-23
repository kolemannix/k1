#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")"
ROOT=$PWD
GEN=$ROOT/gen
BIN=$GEN/bin
OUT=$GEN/out
rm -rf "$BIN" "$OUT" "$GEN/k1" "$GEN/classes" "$GEN/csharp"
mkdir -p "$BIN" "$OUT" "$GEN/k1" "$GEN/classes" "$GEN/csharp" "$GEN/zig-cache" "$GEN/zig-global-cache" "$GEN/go-cache"

BENCHES="binary-trees hashmap byte-scan"

DOTNET_ROOT=$(dirname "$(dotnet --list-sdks | head -1 | sed 's/.*\[//;s/\]$//')")
export DOTNET_ROOT
export DOTNET_CLI_TELEMETRY_OPTOUT=1
export DOTNET_NOLOGO=1

ZIG_SYSROOT=
for sdk in /Library/Developer/CommandLineTools/SDKs/MacOSX*.sdk; do
  if grep -m1 '^targets:' "$sdk/usr/lib/libSystem.tbd" 2>/dev/null | grep -q 'arm64-macos'; then ZIG_SYSROOT=$sdk; break; fi
done

langs() {
  case $1 in
    hashmap) echo "k1 c rust go zig java java-prim csharp python" ;;
    *) echo "k1 c rust go zig java csharp python" ;;
  esac
}

prog() {
  case $2 in
    java|java-prim) echo "java -cp $GEN/classes/$1-$2 Main" ;;
    csharp) echo "$GEN/csharp/$1/publish/bench" ;;
    python) echo "python3 $(ls "$ROOT/$1/python/"*.py)" ;;
    *) echo "$BIN/$1-$2" ;;
  esac
}

build_k1() {
  local bench=$1 tag=$2 flags=$3
  local src name dir
  src=$(ls "$ROOT/$bench/k1/"*.k1)
  name=$(basename "$src" .k1)
  dir=$GEN/k1/$tag/$bench
  mkdir -p "$dir"
  cp "$src" "$dir/"
  (cd "$dir" && k1 $flags --cache false build "$name.k1")
  cp "$dir/.k1-out/$name" "$BIN/$bench-$tag"
}

for bench in $BENCHES; do
  echo "== building $bench"
  build_k1 "$bench" k1 --optimize
  build_k1 "$bench" k1-noopt ""
  clang -O2 -o "$BIN/$bench-c" "$ROOT/$bench/c/"*.c
  rustc --edition 2021 -C opt-level=3 -o "$BIN/$bench-rust" "$ROOT/$bench/rust/"*.rs
  (cd "$ROOT/$bench/go" && GOCACHE=$GEN/go-cache go build -o "$BIN/$bench-go" main.go)
  zig build-exe -O ReleaseFast ${ZIG_SYSROOT:+--sysroot "$ZIG_SYSROOT"} \
    --cache-dir "$GEN/zig-cache" --global-cache-dir "$GEN/zig-global-cache" \
    -femit-bin="$BIN/$bench-zig" "$ROOT/$bench/zig/"*.zig
  rm -f "$BIN/$bench-zig.o"
  for tag in java java-prim; do
    if [ -d "$ROOT/$bench/$tag" ]; then
      mkdir -p "$GEN/classes/$bench-$tag"
      javac -d "$GEN/classes/$bench-$tag" "$ROOT/$bench/$tag/Main.java"
    fi
  done
  mkdir -p "$GEN/csharp/$bench"
  cp "$ROOT/$bench/csharp/"* "$GEN/csharp/$bench/"
  (cd "$GEN/csharp/$bench" && dotnet publish -c Release -o "$GEN/csharp/$bench/publish" > publish.log)
done

for bench in $BENCHES; do
  : > "$GEN/sizes-$bench.txt"
  for tag in $(langs "$bench") k1-noopt; do
    case $tag in
      java|java-prim) echo "$tag $(cat "$GEN/classes/$bench-$tag/"*.class | wc -c)" ;;
      csharp) echo "$tag $(wc -c < "$GEN/csharp/$bench/publish/bench.dll")" ;;
      python) echo "$tag $(cat "$ROOT/$bench/python/"*.py | wc -c)" ;;
      *) echo "$tag $(wc -c < "$BIN/$bench-$tag")" ;;
    esac >> "$GEN/sizes-$bench.txt"
  done
done

for bench in $BENCHES; do
  echo "== verifying $bench"
  $(prog "$bench" c) > "$OUT/$bench-c.txt"
  for tag in $(langs "$bench") k1-noopt; do
    $(prog "$bench" "$tag") > "$OUT/$bench-$tag.txt"
    cmp "$OUT/$bench-c.txt" "$OUT/$bench-$tag.txt"
  done
  cat "$OUT/$bench-c.txt"
done

{
  echo "machine: $(sysctl -n machdep.cpu.brand_string), $(sysctl -n hw.perflevel0.physicalcpu)P+$(sysctl -n hw.perflevel1.physicalcpu)E cores, $(( $(sysctl -n hw.memsize) / 1024 / 1024 / 1024 )) GB, macOS $(sw_vers -productVersion)"
  echo "load average before timing: $(uptime | sed 's/.*load averages*: //')"
  echo "k1: $(k1 --version) at $(which k1); repo HEAD when run: $(git -C "$ROOT" rev-parse --short HEAD)"
  echo "clang: $(clang --version | head -1)"
  echo "rustc: $(rustc --version)"
  echo "go: $(go version)"
  echo "zig: $(zig version)${ZIG_SYSROOT:+ (--sysroot $ZIG_SYSROOT: the macOS 26 SDK libSystem.tbd lists no arm64-macos target, which zig 0.14 needs)}"
  echo "java: $(java -version 2>&1 | head -1 | tr -d '\r'), $(java -XX:+PrintFlagsFinal -version 2>/dev/null | awk '/ MaxHeapSize/ {printf "default max heap %d MB", $4 / 1024 / 1024}'), default GC $(java -XX:+PrintFlagsFinal -version 2>/dev/null | awk '/ UseG1GC/ {print ($4 == "true" ? "G1" : "not G1")}')"
  echo "dotnet: sdk $(dotnet --version), runtime $(dotnet --list-runtimes | awk '/^Microsoft.NETCore.App/ {print $2; exit}') at $DOTNET_ROOT"
  echo "python: $(python3 --version) at $(which python3)"
  echo "hyperfine: $(hyperfine --version)"
  echo "date: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
} > "$GEN/env.txt"

for bench in $BENCHES; do
  echo "== timing $bench"
  args=()
  for tag in $(langs "$bench"); do args+=(-n "$tag" "$(prog "$bench" "$tag")"); done
  hyperfine -N --warmup 2 --min-runs 5 --export-json "$GEN/$bench.json" "${args[@]}"
  hyperfine -N --runs 1 --export-json "$GEN/$bench-noopt.json" -n "k1 (no --optimize)" "$(prog "$bench" k1-noopt)"
done

for bench in $BENCHES; do
  echo "== peak rss $bench"
  for tag in $(langs "$bench") k1-noopt; do
    /usr/bin/time -l $(prog "$bench" "$tag") > /dev/null 2> "$GEN/rss-$bench-$tag.txt"
  done
done

for bench in $BENCHES; do
  echo "== steady state $bench"
  for tag in $(langs "$bench"); do
    case $tag in
      java|java-prim|csharp)
        $(prog "$bench" "$tag") steady > /dev/null 2> "$GEN/steady-$bench-$tag.txt" ;;
    esac
  done
done

echo "load average after timing: $(uptime | sed 's/.*load averages*: //')" >> "$GEN/env.txt"

python3 "$ROOT/report.py" "$GEN" > "$ROOT/results.md"
echo "wrote $ROOT/results.md"
