#!/bin/sh
# ir.sh <example.k1> <keep-regex> [absent-regex]
# Builds the example with --emit-llvm in a scratch dir, strips debug locations,
# calling conventions, pointer attributes, the module prefix and the numeric
# suffix of every symbol from the IR, then prints: the
# compile-time output, the count of absent-regex in the whole file when given,
# every define matching keep-regex, main with the lines not matching keep-regex
# elided, and every constant matching keep-regex.
set -eu
src=$1 keep=$2 absent=${3:-}
name=$(basename "$src" .k1)
dir=$(mktemp -d)
cp "$src" "$dir/"
echo "\$ k1 build $name.k1 --emit-llvm"
(cd "$dir" && k1 build --emit-llvm "$name.k1" 2>&1 >/dev/null | grep ' info\] ' || true)
ll=$dir/.k1-out/$name.ll
if [ -n "$absent" ]; then
  echo "\$ grep -c '$absent' .k1-out/$name.ll"
  grep -c "$absent" "$ll" || true
fi
sed -E -e 's/, !dbg ![0-9]+//g' -e 's/ #[0-9]+ !dbg ![0-9]+//' -e 's/fastcc //g' -e 's/noundef nonnull //g' \
  -e 's/ align [0-9]+ dereferenceable\([0-9]+\)//g' -e 's/llvm\.memcpy\.p0\.p0\.i64/llvm.memcpy/' \
  -e 's/internal unnamed_addr constant/constant/' -e "s/@$name\\./@/g" -e 's/_[0-9]+\(/(/g' "$ll" > "$dir/clean.ll"
grep -E "^define.*($keep)" "$dir/clean.ll" | grep -v "@core\." | grep -v "\.main" | sed 's/ {$//'
awk -v keep="$keep" -v main="void @main(" '
  index($0, main) && /^define/ { on = 1; print; next }
  on && /^}/ { if (gap) print "  ..."; print; on = 0; next }
  on && /^prelude:/ { next }
  on { if ($0 ~ keep || $0 ~ /^  ret /) { if (gap) print "  ..."; gap = 0; print } else gap = 1 }
' "$dir/clean.ll"
grep -E "^@.*constant" "$dir/clean.ll" | grep -E "$keep" || true
rm -rf "$dir"
