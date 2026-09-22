#!/bin/bash
cd "$(dirname "$0")/../.."
k1_test --tests-dir content/showcase/examples "$@"
tests=$?
python3 - <<'PY'
import re, pathlib, sys
showcase = pathlib.Path("content/showcase")
examples = {p: p.read_text() for p in (showcase / "examples").rglob("*.k1")}
bad = 0
for md in sorted((showcase / "sections").glob("*.md")):
    for m in re.finditer(r"```k1(?: path=(\S+))?\n(.*?)```", md.read_text(), re.S):
        path, block = m.group(1), m.group(2)
        if path:
            ok = block in pathlib.Path(path).read_text()
        else:
            ok = any(block in src for src in examples.values())
        if not ok:
            bad += 1
            where = path or "examples/"
            print(f"{md}: k1 block not found verbatim in {where}:\n{block[:300]}\n")
print("sections: all k1 blocks verified" if not bad else f"sections: {bad} unverified blocks")
sys.exit(1 if bad else 0)
PY
sections=$?
exit $(( tests != 0 || sections != 0 ))
