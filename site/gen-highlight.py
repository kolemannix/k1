#!/usr/bin/env python3
import hashlib, html, json, pathlib, re, shutil, subprocess, sys, tempfile
repo = pathlib.Path(__file__).resolve().parent.parent
if not shutil.which("k1"):
    sys.exit("gen-highlight: k1 is not on PATH")
examples = {p: p.read_bytes() for p in sorted((repo / "content/showcase/examples").rglob("*.k1"))}
FENCE = re.compile(rb"```k1(?: path=(\S+))?\n(.*?)```", re.S)

parsed = {}
def tokens_of(path):
    if path not in parsed:
        r = subprocess.run(["k1", "parse", str(path)], capture_output=True, text=True)
        if r.returncode not in (0, 1) or not r.stdout:
            sys.exit(f"gen-highlight: k1 parse {path} failed:\n{r.stderr}")
        toks = []
        for line in r.stdout.splitlines():
            s, l, c = line.split()
            toks.append((int(s), int(l), c))
        parsed[path] = toks
    return parsed[path]

def render(text, toks, off):
    out, pos = [], 0
    for s, l, c in toks:
        s -= off
        if s < pos or s + l > len(text):
            continue
        out.append(html.escape(text[pos:s].decode(), quote=False))
        cls = "k-" + c.replace(".decl", " k-decl")
        out.append(f'<span class="{cls}">{html.escape(text[s:s + l].decode(), quote=False)}</span>')
        pos = s + l
    out.append(html.escape(text[pos:].decode(), quote=False))
    return "".join(out)

sections = repo / "content/showcase/sections"
blocks = []
for md in sorted(sections.glob("*.md")) + sorted((repo / "site/content").rglob("*.md")):
    for m in FENCE.finditer(md.read_bytes()):
        path = repo / m.group(1).decode() if m.group(1) else None
        blocks.append((m.group(2).rstrip(b"\n"), path, md, md.is_relative_to(sections)))

for card in json.loads((repo / "site/data/carousel.json").read_text()):
    for block in card.get("blocks", []):
        if block["kind"] == "k1":
            blocks.append((block["code"].encode(), None, "carousel.json", False))

out = {}
partial = 0
with tempfile.TemporaryDirectory() as tmp:
    for text, path, origin, strict in blocks:
        key = hashlib.sha1(text).hexdigest()
        if key in out:
            continue
        if path is None:
            for p, src in examples.items():
                if text in src:
                    path = p
                    break
        if path is None:
            if strict:
                sys.exit(f"gen-highlight: {origin}: k1 block not found in any source:\n{text[:300].decode()}")
            path = pathlib.Path(tmp) / f"{key}.k1"
            path.write_bytes(text + b"\n")
            partial += 1
        src = path.read_bytes()
        off = src.find(text)
        if off < 0:
            sys.exit(f"gen-highlight: {origin}: k1 block not found in {path}")
        out[key] = render(text, tokens_of(path), off)
(repo / "site/data/highlight.json").write_text(json.dumps(out, indent=1) + "\n")
print(f"highlight.json: {len(out)} blocks, {partial} without a source file")
