#!/usr/bin/env python3
import json, re, pathlib, sys
repo = pathlib.Path(__file__).resolve().parent.parent
out = repo / "site" / "data"

def need(cond, msg):
    if not cond:
        sys.exit(f"gen-data: {msg}")

stress = (repo / "perf" / "latest_stress.txt").read_text()
m = re.search(r"took (\d+)ms \(([\d.]+) line/s, (\d+) lines\)", stress)
need(m, "no timing line in perf/latest_stress.txt")
stress_ms, stress_lps, stress_lines = int(m.group(1)), float(m.group(2)), int(m.group(3))

ct = (repo / "content/showcase/benchmarks/compile-times/results.md").read_text()
lines = re.search(r"^\| k1 \| (\d+) \|", ct, re.M)
need(lines, "no k1 line count in compile-times results")
k1_lines = int(lines.group(1))

NO_CHECK_MODE = {"Go", "Java", "Kotlin", "C#"}

def mode_rows(label):
    if f"### {label}" not in ct:
        return None
    sec = ct[ct.index(f"### {label}"):]
    table = re.search(r"\n(\|[^\n]*\n)+", sec).group(0)
    header = [h.strip() for h in table.strip().splitlines()[0].strip("|").split("|")]
    need("N=100 wall s" in header, f"{label} table has no N=100 column")
    col = header.index("N=100 wall s")
    rows = {}
    for line in table.strip().splitlines()[2:]:
        cells = [c.strip() for c in line.strip("|").split("|")]
        lang, wall = cells[0], re.match(r"([\d.]+) ±", cells[col])
        if not wall or "hand-rolled" in lang or "warm cache" in lang or (label == "typecheck only" and lang in NO_CHECK_MODE):
            continue
        rows[lang] = int(round(float(wall.group(1)) * 1000))
    need("k1" in rows, f"{label} table missing k1")
    order = sorted(rows.items(), key=lambda kv: kv[1])
    return [{"name": lang, "ms": ms, "k1": lang == "k1", "pct": round(ms / order[-1][1] * 100, 2)} for lang, ms in order]

timing = {
  "lines": k1_lines,
  "typecheck": mode_rows("typecheck only"),
  "build": mode_rows("debug build"),
  "stress": {"lines": stress_lines, "ms": stress_ms, "lines_per_s": round(stress_lps)},
}
need(timing["typecheck"], "no typecheck table in compile-times results")
(out / "timing.json").write_text(json.dumps(timing, indent=2) + "\n")

bench = repo / "content/showcase/benchmarks"
provenance = {}
for suite in ["runtime", "brotli", "compile-times", "comptime", "json", "int-strings", "loc"]:
    entry = {}
    if (bench / suite / "run.sh").exists():
        entry["script"] = f"content/showcase/benchmarks/{suite}/run.sh"
    results = bench / suite / "results.md"
    if results.exists():
        entry["results"] = f"content/showcase/benchmarks/{suite}/results.md"
        text = results.read_text()
        for pat in (r"^date: (\d{4}-\d{2}-\d{2})", r"Generated(?: by `run.sh`)?(?: on)? (\d{4}-\d{2}-\d{2})", r"Measured (\d{4}-\d{2}-\d{2})", r"Counted (\d{4}-\d{2}-\d{2})", r"(\d{4}-\d{2}-\d{2})"):
            m = re.search(pat, text, re.M)
            if m:
                entry["date"] = m.group(1)
                break
    provenance[suite] = entry
(out / "benchmarks.json").write_text(json.dumps(provenance, indent=2) + "\n")

BASICS = ["Functions and variables", "list, span, and buffer", "Structs", "either", "Optionals and results", "Control flow",
          "First-class functions and lambdas", "Abilities", "Generics", "References", "Early execution"]

CARDS = [
  {"section": "05-macros", "title": "A macro that implements an http routing dsl",
   "blurb": "The macro is a plain function named `define` that receives a list of tokens, parses into endpoints, validates for uniqueness, generates a sum type representing each possible route, puts them in a trie, and generates a hand-written dispatcher",
   "blocks": [("the routes", "$http/routing/define(route, \\{"), ("the macro", "macro define(name: code, rows: span[code-token]) {"), ("what comes out", "type route = either {", "text"), ("use", "  if route/parse-request(req.header) is {", "path=examples/httpapp/app.k1", "  }")]},
  {"section": "04-reflection", "title": "JSON from reflection",
   "blurb": "json/to-string reads the type's schema at compile time and generates a writer for exactly these fields.",
   "blocks": [("the type", "type contact = either { email(string), phone(string), unreachable }"), ("to JSON", "  println(json/to-string(ada))"), ("output", "{\"name\":\"Ada Lovelace\"", "text")]},
  {"section": "09-model", "title": "Abilities, not inheritance",
   "blurb": "Typeclasses: implement one for a type you did not write, bound a generic on it, or implement it for every type that has another.",
   "blocks": [("the ability", "ability shape {"), ("an impl", "impl shape for circle {"), ("a bound and a blanket impl", "fn bigger[t: shape](a: *t, b: *t): *t {"), ("operators are abilities", "type vec2 = { x: f64, y: f64 }"), ("use", "  let v = vec2.{ x = 1.0, y = 2.0 } + vec2.{ x = 0.5, y = 0.25 }"), ("output", "1.5 2.25", "text")]},
  {"section": "09-model", "title": "One require, five bindings",
   "blurb": "A struct pattern, a literal, a variant carrying a struct, an optional and two guards in one statement, and the names live on.",
   "blocks": [("the request", "type auth = either {"), ("the handler", "fn known(user: string, pass: string): bool {"), ("output", "ada posted 5 bytes to /api/notes", "text")]},
  {"section": "08-types", "title": "Types from a .proto file",
   "blurb": "A .proto file read and parsed at compile time becomes a real k1 struct and enum.",
   "blocks": [("user.proto", "syntax = \"proto3\";", "proto"), ("derive", "type(alias) status = #type proto/enum(\"user.proto\", \"Status\")"), ("use", "  let u: user = .{ id = 7, name = \"ada\", score = 99.5, status = :suspe"), ("output", "7 ada 99.5 suspended=1 on hold", "text")]},
  {"section": "06-regex", "title": "A regex engine that runs at compile time",
   "blurb": "Parser, NFA, DFA and minimization are plain k1 that happens to run in the compiler.",
   "blocks": [("define", "$regex/define(number, \"[0-9]+([.][0-9]+)?\")"), ("match and find", "  for s in [\"42\", \"3.14\", \"3.\", \".5\", \"1e5\"]"), ("output", "number(\"42\") = true", "text")]},
  {"section": "01-static", "title": "A palette designed in OKLCH, shipped as bytes",
   "blurb": "The colour math runs early, so every swatch is an sRGB triple baked into the binary.",
   "blocks": [("the math", "fn oklch(l: f32, c: f32, h: f32): srgb8 {"), ("the palette", "let UI-HUE: f32 = 267.0"), ("the CSS", "let CSS: string = {"), ("output", ":root {", "text")]},
  {"section": "10-systems", "title": "Painless SIMD",
   "blurb": "A macro generates a lane-wide scanner, and the same code runs under #static in the compile-time VM.",
   "blocks": [("generate the scanner", "$std/simd/first-of(delims, "), ("use it", "fn scan-all-simd(text: string): u64 {"), ("at compile time too", "  let static-hits: 4u64 = #static scan-all-simd("), ("output", "compile-time scan found 4 delimiters", "text")]},
  {"section": "10-systems", "title": "Hot reload a namespace",
   "blurb": "Mark a namespace ns(reload), watch it, and every edit swaps in under the running process with its globals intact.",
   "blocks": [("the namespace", "ns(reload) handlers {"), ("watch and serve", "fn main() {\n  handlers/watch().!"), ("output", "reload: sys_reload/handlers swapped to v1 (2 syms)", "text")]},
  {"section": "03-comptime-ffi", "title": "C at compile time",
   "blurb": "Declare a C function and the compile-time VM calls it through libffi.",
   "blocks": [("declare", "fn(extern(\"gethostname\")) gethostname(name: ptr, len: size): i32"), ("call them while compiling", "let BUILD_HOST: string = hostname()"), ("output", "$ k1 run cffi_build_stamp.k1", "text")]},
  {"title": "dbx: SQLite in the k1 fold", "placeholder": "Case study in progress. dbx pulls SQLite into the k1 fold at compile time: the dependency, the bindings and the schema, so tables become k1 types before the program runs."},
]
cards = []
for card in CARDS:
    if "placeholder" in card:
        cards.append(card)
        continue
    text = (repo / "content/showcase/sections" / f"{card['section']}.md").read_text()
    fences = {kind: re.findall(r"```" + kind + r"(?: path=\S+)?\n(.*?)```", text, re.S) for kind in ("k1", "text", "proto")}
    shown = []
    for spec in card["blocks"]:
        label, first = spec[0], spec[1]
        kind = spec[2] if len(spec) > 2 else "k1"
        if kind.startswith("path="):
            path, end = kind[5:], (spec[3] if len(spec) > 3 else "}")
            lines = (repo / path).read_text().splitlines()
            start = next((i for i, l in enumerate(lines) if l.startswith(first)), None)
            need(start is not None, f"{path}: no line starting with {first!r}")
            stop = next((i for i in range(start + 1, len(lines)) if lines[i] == end), len(lines) - 1)
            shown.append({"label": label, "kind": "k1", "code": "\n".join(lines[start:stop + 1]), "path": path})
            continue
        block = next((b for b in fences[kind] if b.startswith(first)), None)
        need(block, f"{card['section']}: no {kind} block starting with {first!r}")
        shown.append({"label": label, "kind": kind, "code": block.rstrip("\n")})
    cards.append({"title": card["title"], "section": card["section"], "blurb": card["blurb"], "blocks": shown})

basics_md = (repo / "content/showcase/sections/00-basics.md").read_text()
basics = []
for title in BASICS:
    start = basics_md.index("## " + title + "\n")
    chunk = basics_md[start:]
    chunk = chunk[:chunk.index("\n## ")] if "\n## " in chunk else chunk
    blurb = chunk[chunk.index("\n") + 1:chunk.index("```")].strip()
    blocks = []
    for m in re.finditer(r"```(k1|text)( path=\S+)?\n(.*?)```", chunk, re.S):
        kind, path, code = m.group(1), m.group(2), m.group(3).rstrip("\n")
        if path and blocks and blocks[-1]["label"] == "in core":
            blocks[-1]["code"] += "\n" + code
            continue
        label = "in core" if path else "program" if kind == "k1" else "~llvm ir" if code.startswith("$ content/showcase/ir.sh") else "output"
        blocks.append({"label": label, "kind": kind, "code": code})
    folded = []
    for b in blocks:
        if b["label"].startswith("~"):
            b["label"] = b["label"][1:]
            folded[-1]["alt"] = b
        else:
            folded.append(b)
    blocks = folded
    basics.append({"title": title, "section": "00-basics", "anchor": re.sub(r"[^a-z0-9]+", "-", title.lower()).strip("-"), "blurb": blurb, "blocks": blocks})
(out / "basics.json").write_text(json.dumps(basics, indent=2) + "\n")
(out / "carousel.json").write_text(json.dumps(cards, indent=2) + "\n")
print(f"timing.json: {k1_lines} lines, typecheck {len(timing['typecheck'])} rows, build {len(timing['build'] or [])} rows, stress {stress_lines} lines {stress_ms} ms")
print(f"carousel.json: {len(cards)} cards, basics.json: {len(basics)} cards")
