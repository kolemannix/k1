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
    return rows

cx = (repo / "content/showcase/benchmarks/comptime/results.md").read_text()

def derive_rows(size):
    sec = cx[cx.index("## reflect-serialize\n"):]
    secs = {}
    for n, lang, t in re.findall(r"^\| (\d+) \| ([^|]+) \| ([\d.]+) ±", sec, re.M):
        secs[(int(n), lang)] = float(t)
    rows = {}
    for (n, lang), t in secs.items():
        if n == size:
            rows[lang] = int(round((t - secs[(0, lang)]) / size * 1e6))
    need("k1" in rows and len(rows) > 1, f"reflect-serialize table has no k1 row at {size}")
    return rows

def bar_rows(rows):
    order = sorted(rows.items(), key=lambda kv: kv[1])
    return [{"name": lang, "value": v, "k1": lang == "k1", "pct": round(v / order[-1][1] * 100, 2)} for lang, v in order]

compile_panels = []
for label in ["typecheck only", "debug build"]:
    rows = mode_rows(label)
    if rows:
        compile_panels.append({"label": label.removesuffix(" only"), "unit": "ms", "rows": bar_rows(rows)})
need(compile_panels and compile_panels[0]["label"] == "typecheck", "no typecheck table in compile-times results")
timing = {
  "lines": k1_lines,
  "compile": compile_panels,
  "stress": {"lines": stress_lines, "ms": stress_ms, "lines_per_s": round(stress_lps)},
  "comptime": [{"label": "derive a JSON serializer from a type", "unit": "µs", "rows": bar_rows(derive_rows(1000))}],
}
(out / "timing.json").write_text(json.dumps(timing, indent=2) + "\n")

BASICS = ["Functions and variables", "list, span, and buffer", "Structs", "either", "Optionals and results", "Control flow",
          "First-class functions and lambdas", "Abilities", "Generics", "References", "Early execution"]

CARDS = [
  {"section": "05-macros", "title": "typesafe, runtime-optimal http routing",
   "blurb": "The macro is a plain function named `define` that receives a list of tokens, parses into endpoints, validates for uniqueness, generates a sum type representing each possible route, puts them in a trie, and generates a hand-written dispatcher; there's no runtime routing tree structure like in every other HTTP framework.",
   "blocks": [("route declaration", "$http/routing/define(route) {"), ("macro definition", "macro define(name: code, body: span[code-token]) {"), ("usage; handling a request", "  if route/parse-request(req.head) is {", "path=examples/httpapp/app.k1", "  }")]},
  {"title": "An embedded html+k1 language, implemented with code-token macros and `eval`",
   "blurb": "`html/markup` is just a macro in std. Tags, attributes, k1 expressions, `#for`, and `#if` are parsed while compiling and become calls that directly build up the rendered html; skipping an intermediate AST/DOM tree.",
   "blocks": [("usage", "fn render-sign-in-form", "path=content/showcase/helme2/sign_in.k1"), ("std/html: the macro", "macro markup(markup: span[code-token]) {", "path=modules/std/html.k1")]},
  {"section": "11-dbx", "title": "compile-time sql queries and schema-driven codegen",
   "blurb": "'dbx' is a k1 sqlite library that will read and execute your schema migrations at compile-time, generate a row type for each table, and insert/update queries; and typecheck your own queries",
   "blocks": [("db schema, the actual sql migrations", "let schema: dbx/schema", "path=content/showcase/helme2/db.k1", "$dbx/define([wren/wren-schema, schema])"), ("your queries", "fn read-org", "path=content/showcase/helme2/db.k1", None), ("dbx: the macros", "macro define(schemas: code) {", "path=content/showcase/helme2/dbx.k1", None), ("a caught error", "┌", "text")]},
  {"title": "One-line reflection",
   "blurb": "`types/schema[t]` returns a plain value describing any type",
   "blocks": [("std/cliarg: help text from a struct", "fn gen-help-text[t](summary: string): string {", "path=modules/std/cliarg.k1"), ("match on a type's schema", "  fn to[t](self: int-value): t {", "path=modules/core/types.k1", "  }"), ("std/json", "fn to-string[t: codec/encode]", "path=modules/std/json.k1"), ("a json endpoint", "fn respond-playground-tuition", "path=content/showcase/helme2/tuition_json.k1")]},
  {"title": "Abilities, for modular late-bound polymorphism",
   "blurb": "Typeclasses: implement one for a type you did not write, bound a generic on it, or implement it for every type that has another.",
   "blocks": [("the ability", "ability form-value {", "path=content/showcase/helme2/forms.k1"), ("a type we do not own", "impl form-value for string {", "path=content/showcase/helme2/forms.k1"), ("a name", "type name-text", "path=content/showcase/helme2/forms.k1"), ("an amount of money", "type amount", "path=content/showcase/helme2/forms.k1")]},
  {"title": "Specialty types via macros",
   "blurb": "`std/deftype/flags` turns a list of names into a bit-set type with its own flag enum, set operations and printing.",
   "blocks": [("declaration; this is just a macro call", "$std/deftype/flags(view-options)", "path=content/showcase/helme2/view_options.k1"), ("std/deftype: what the macro emits", "    type ${declared.name} = { bits: $backing }", "path=modules/std/deftype.k1", "    }"), ("using the type", "fn make-reveal-toggles", "path=content/showcase/helme2/view_options.k1")]},
  {"section": "01-static", "title": "A palette designed in OKLCH, shipped as bytes",
   "blurb": "The colour math runs early, so every swatch is an sRGB triple baked into the binary.",
   "blocks": [("the math", "fn oklch(l: f32, c: f32, h: f32): srgb8 {"), ("the palette", "let UI-HUE: f32 = 267.0"), ("the CSS", "let CSS: string = {"), ("output", ":root {", "text")]},
  {"title": "SIMD in the standard library",
   "blurb": "`vector[u8, k1/simd-bytes]` is the target's natural vector width, so std/json finds the next byte to escape a whole vector at a time, with a scalar loop for the tail.",
   "blocks": [("std/json", "  fn escape-position", "path=modules/std/json.k1", "  }")]},
  {"title": "hot reload",
   "blurb": "Put your request handler, or your game tick fn, in an `ns(reload)`, and a hot-reloadable library and API is generated. The reloadable code includes the transitive closure of everything it calls, so you only need your entrypoint(s) to be reloadable!",
   "blocks": [("the namespace", "ns(reload) live {", "path=content/showcase/helme2/live.k1"), ("watch in dev", "fn serve(", "path=content/showcase/helme2/live.k1"), ("the log", "reload: ", "path=content/showcase/helme2/reload.log", None)]},
]
cards = []
for card in CARDS:
    text = (repo / "content/showcase/sections" / f"{card['section']}.md").read_text() if "section" in card else ""
    fences = {kind: re.findall(r"```" + kind + r"(?: path=\S+)?\n(.*?)```", text, re.S) for kind in ("k1", "text")}
    shown = []
    for spec in card["blocks"]:
        label, first = spec[0], spec[1]
        kind = spec[2] if len(spec) > 2 else "k1"
        if kind.startswith("path="):
            path, end = kind[5:], (spec[3] if len(spec) > 3 else "}")
            lines = (repo / path).read_text().splitlines()
            start = next((i for i, l in enumerate(lines) if l.startswith(first)), None)
            need(start is not None, f"{path}: no line starting with {first!r}")
            stop = len(lines) - 1 if end is None else next((i for i in range(start + 1, len(lines)) if lines[i] == end), None)
            need(stop is not None, f"{path}: no line {end!r} after {first!r}")
            shown.append({"label": label, "kind": "k1" if path.endswith(".k1") else "text", "code": "\n".join(lines[start:stop + 1]), "path": path})
            continue
        block = next((b for b in fences[kind] if b.startswith(first)), None)
        need(block, f"{card['section']}: no {kind} block starting with {first!r}")
        shown.append({"label": label, "kind": kind, "code": block.rstrip("\n")})
    cards.append({"title": card["title"], "blurb": card["blurb"], "blocks": shown})

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
    basics.append({"title": title, "blurb": blurb, "blocks": blocks})
(out / "basics.json").write_text(json.dumps(basics, indent=2) + "\n")
(out / "carousel.json").write_text(json.dumps(cards, indent=2) + "\n")
print(f"timing.json: {k1_lines} lines, {len(compile_panels)} compile panels, stress {stress_lines} lines {stress_ms} ms")
print(f"carousel.json: {len(cards)} cards, basics.json: {len(basics)} cards")
