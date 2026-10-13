#!/usr/bin/env python3
"""Draws the chart SVGs for every benchmark suite. compile-times, runtime, comptime and brotli
read the exports their run.sh leaves under gen/; json reads the table run.sh's numbers were
written into; int-strings carries the numbers from the blog post (chart.svg is the current
state, journey.svg the whole road). The SVGs use the site's CSS variables with the dark palette
as fallback, so inlined in a page they follow its theme."""
import glob
import json
import re
import signal
from html import escape
from pathlib import Path

HERE = Path(__file__).resolve().parent
NAMES = {"k1": "k1", "c": "C", "c-bump": "C, bump arena", "cpp": "C++", "cpp26": "C++26 P2996", "rs": "Rust",
         "rust": "Rust", "go": "Go", "zig": "Zig", "java": "Java", "java-prim": "Java (primitive)", "csharp": "C#",
         "python": "Python"}


def hyperfine(path):
    return {r["command"]: r["mean"] for r in json.load(open(path))["results"]}


def first(path):
    return next(iter(hyperfine(path).values()))


def seconds(v):
    return f"{v:.3f} s" if v < 1 else f"{v:.2f} s"


def row(name, value, kind=None, detail=None, group=None):
    return {"name": name, "value": value, "kind": kind or ("k1" if name.startswith("k1") else "other"),
            "detail": detail, "group": group}


def panel(title, rows, fmt=seconds, sub=None, **opts):
    return {"title": title, "sub": sub, "fmt": fmt, "rows": rows, "opts": opts}


def compile_times():
    res = HERE / "compile-times" / "gen" / "results"

    def rows(tag, langs):
        return [row(name, first(res / f"{key}-check-{tag}.json")) for key, name in langs]

    return [
        panel("typecheck, 100 generated units", rows("100", [("k1", "k1"), ("k1warm", "k1, warm cache"), ("c", "C"),
              ("cpp", "C++"), ("cpp-hand-vec", "C++, hand-rolled vec"), ("rs", "Rust"), ("zig", "Zig")]),
              sub="seconds, lower is better"),
        panel("typecheck, hello world", rows("hello", [("k1", "k1"), ("c", "C"), ("cpp", "C++"), ("rs", "Rust"),
              ("zig", "Zig"), ("zigcold", "Zig, cold cache")])),
        panel("typecheck the brotli encoder", [row("k1, whole program", first(res / "brotli-k1-check.json")),
              row("C, one clang", first(res / "brotli-c-check.json")),
              row("C, 4 clangs in parallel", first(res / "brotli-cpar-check.json"))]),
    ]


def runtime():
    panels = []
    for bench, title in [("hashmap", "hashmap, 5M inserts + 5M lookups"), ("byte-scan", "byte-scan, 256 MiB"),
                         ("binary-trees", "binary-trees, depth 20")]:
        rows = [row(NAMES[key], v) for key, v in hyperfine(HERE / "runtime" / "gen" / f"{bench}.json").items()]
        rows.append(row("k1, no --optimize", first(HERE / "runtime" / "gen" / f"{bench}-noopt.json"), "k1-dim"))
        limit = 4 * sorted(r["value"] for r in rows)[-2]
        for r in rows:
            if r["value"] > limit:
                r["value"] = f"{seconds(r['value'])}, off the chart"
        panels.append(panel(title, rows, sub="seconds, lower is better" if not panels else None))
    return panels


def comptime():
    by = {}
    for r in json.load(open(HERE / "comptime" / "gen" / "results.json")):
        by[(r["bench"], r["size"], r["lang"], r["comptime"])] = r

    def difference(measured, control, scale=1):
        for r in (measured, control):
            if r is None:
                return "not run"
            if r["status"] != "ok":
                if r["status"] == "error" and r.get("returncode", 0) < 0:
                    return f"killed ({signal.Signals(-r['returncode']).name})"
                return {"error": "compile error", "timeout": "timeout (>300 s)",
                        "skipped": "skipped after timeout"}[r["status"]]
        result = (measured["timing"]["mean"] - control["timing"]["mean"]) * scale
        return result if result > 0 else "below timing resolution"

    def cost(bench, size):
        return [row(NAMES[l], difference(by.get((bench, size, l, True)), by.get((bench, size, l, False))))
                for l in ["k1", "zig", "cpp", "rust"]]

    types = 10_000
    reflect = [row(NAMES[l], difference(by.get(("reflect-serialize", types, l, True)),
                                        by.get(("reflect-serialize", 0, l, True)), 1000 / types))
               for l in ["k1", "zig", "cpp", "cpp26", "rust"]]
    return [
        panel("format 1M numbers", cost("strbuild", 1_000_000), sub="compile-time cost in seconds"),
        panel("sieve to 1M", cost("sieve", 1_000_000)),
        panel("crc32 over 8 MB", cost("crc32", 8_388_608)),
        panel("reflection-generated serializer", reflect, fmt=lambda v: f"{v:.2f} ms",
              sub=f"ms per type, {types:,} types, front end only"),
    ]


def brotli():
    runs = {}
    for log in glob.glob(str(HERE / "brotli" / "gen" / "bench-*.log")):
        for line in open(log):
            m = re.match(r"(\S+) (q\d): k1 ([\d.]+) MB/s .*?, c ([\d.]+) MB/s", line)
            if m:
                runs.setdefault((m[1], m[2]), []).append((float(m[3]), float(m[4])))
    rows = []
    for corpus, q in [("typer.rs", "q0"), ("typer.rs", "q1"), ("cycle-251", "q0"), ("zeros-1mb", "q0"),
                      ("random-300k", "q0"), ("random-300k", "q1")]:
        pairs = runs[(corpus, q)]
        rows.append(row(f"{corpus} {q}", sum(k / c for k, c in pairs) / len(pairs), "k1"))
    return [panel("brotli encoder, k1 port vs C", rows, fmt=lambda v: f"{v:.2f}×",
                  sub="k1 throughput as a fraction of C, per corpus; higher is better", ref=(1.0, "C"), sort=False)]


def json_suite():
    tasks = {}
    for line in (HERE / "json" / "results.md").read_text().splitlines():
        cells = [c.strip() for c in line.strip("|").split("|")]
        if len(cells) != 5 or not re.match(r"[\d.]+$", cells[2]):
            continue
        file, task = cells[0].replace(".json", ""), cells[1]
        for lane, v in zip(["k1", "simdjson", "serde_json"], cells[2:]):
            tasks.setdefault(task, []).append(row(lane, float(v), group=file))
    panels = []
    for task, rows in tasks.items():
        panels.append(panel(task, rows, fmt=lambda v: f"{v:.2f}", sub="GB/s, higher is better" if not panels else None,
                            higher=True, sort=False))
    return panels


def loc():
    data = json.load(open(HERE / "loc" / "counts.json"))
    rows = [row(r["name"], float(r["code"]), "muted" if r["name"] == "LLVM" else None) for r in data["rows"]]
    return [panel("lines of code in the compiler", rows, fmt=lambda v: f"{int(v):,}",
                  sub="code lines by scc, no tests, no standard library; LLVM is the back end under k1, clang and rustc")]


JOURNEY = [
    ("C++ std::to_string", "Lemire's M4 Max", 183.8, "muted"),
    ("Go strconv.Itoa", "Lemire's M4 Max", 84.3, "muted"),
    ("Bun String(i)", "Lemire's M4 Max", 68.5, "muted"),
    ("Rust to_string()", "Lemire's M4 Max", 63.6, "muted"),
    ("Python str(i)", "Lemire's M4 Max", 22.9, "muted"),
    ("C++ std::to_string", "clang -O3, this machine", 160.0, "other"),
    ("k1 show()", "naive formatter, growing builder", 63.9, "k1"),
    ("k1 show()", "stack buffer", 68.1, "k1"),
    ("k1 show()", "two digits per step", 174.2, "k1"),
    ("k1 smolstr", "23 bytes inline, no spill (unfair)", 408.3, "k1"),
    ("k1 spill-string[22]", "spill-list as either (fair)", 397.9, "k1"),
    ("k1 spill-string[22]", "spill-list as hand-rolled union", 416.9, "k1"),
]


def int_strings():
    current = [row(name, value, kind, group=detail) for name, detail, value, kind in JOURNEY
               if kind != "k1" or detail == "spill-list as hand-rolled union"]
    for r in current:
        if r["kind"] == "k1":
            r["name"], r["group"] = "k1 spill-string", "this machine"
        elif r["kind"] == "other":
            r["group"] = "this machine"
    return [panel("integer to string, million strings per second", current, fmt=lambda v: f"{v:.1f}", higher=True)]


def journey():
    rows = [row(name, value, kind, detail=detail) for name, detail, value, kind in JOURNEY]
    return [panel("integer to string, million strings per second", rows, fmt=lambda v: f"{v:.1f}",
                  sub="buf[i & 1023] = to_string(i) for i in 0..100M, best of 5, Apple M3 Max unless noted; higher is better",
                  higher=True, sort=False)]


W, PAD, VALUE, BAR, CH = 1080, 24, 84, 14, 8.0


def draw_panel(x, y, pw, p):
    rows, fmt = p["rows"], p["fmt"]
    higher, sort, ref = p["opts"].get("higher", False), p["opts"].get("sort", True), p["opts"].get("ref")
    detailed = any(r["detail"] for r in rows)
    group_w = max([len(r["group"] or "") for r in rows]) * (CH - 1) + 24 if any(r["group"] for r in rows) else 0
    label = int(group_w + max(len(r["name"]) for r in rows) * CH + 12)
    if detailed:
        label = max(label, int(max(len(r["detail"]) for r in rows) * (CH - 1) + 12))
    row_h, barw = (36 if detailed else 22), pw - label - VALUE
    out = [f'<text class="t1" x="{x}" y="{y + 14}">{escape(p["title"])}</text>']
    top = y + 44 if p["sub"] else y + 30
    if p["sub"]:
        out.append(f'<text class="t2" x="{x}" y="{y + 30}">{escape(p["sub"])}</text>')
    numeric = [r["value"] for r in rows if isinstance(r["value"], float)]
    peak = numeric + ([ref[0]] if ref else [])
    scale = barw / max(peak) if peak else 0
    if sort:
        rows = sorted(rows, key=lambda r: (not isinstance(r["value"], float),
                                           (-r["value"] if higher else r["value"]) if isinstance(r["value"], float) else 0))
    cy = top
    for r in rows:
        ty = cy + (13 if detailed else 11)
        if r["group"]:
            out.append(f'<text class="t2" x="{x}" y="{ty}">{escape(r["group"])}</text>')
        out.append(f'<text class="t3 {r["kind"]}" x="{x + label - 10}" y="{ty}" text-anchor="end">{escape(r["name"])}</text>')
        if r["detail"]:
            out.append(f'<text class="t2" x="{x + label - 10}" y="{ty + 14}" text-anchor="end">{escape(r["detail"])}</text>')
        by = cy + (6 if detailed else 0)
        v = r["value"]
        if isinstance(v, float):
            w = max(v * scale, 1.5)
            out.append(f'<rect class="{r["kind"]}" x="{x + label}" y="{by}" width="{w:.1f}" height="{BAR}"/>')
            vx = x + label + (barw if ref else w) + 6
            out.append(f'<text class="t3" x="{vx:.1f}" y="{by + 11}">{fmt(v)}</text>')
        else:
            out.append(f'<text class="t4" x="{x + label}" y="{by + 11}">{escape(v)}</text>')
        cy += row_h
    if ref:
        rx = x + label + ref[0] * scale
        out[1:1] = [f'<line class="ref" x1="{rx:.1f}" y1="{top - 4}" x2="{rx:.1f}" y2="{cy - 4}"/>',
                    f'<text class="t2" x="{rx:.1f}" y="{top - 8}" text-anchor="middle">{ref[1]}</text>']
    return out, cy


def draw(path, panels):
    cols = min(3, len(panels))
    pw = (W - PAD * (cols + 1)) // cols
    body, y = [], PAD
    for i in range(0, len(panels), cols):
        tallest = y
        for j, p in enumerate(panels[i:i + cols]):
            out, bottom = draw_panel(PAD + j * (pw + PAD), y, pw, p)
            body += out
            tallest = max(tallest, bottom)
        y = tallest + 20
    h = y + PAD - 20
    svg = f'''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {h}" width="{W}" height="{h}" style="max-width:100%;height:auto" font-family="var(--mono, 'Martian Mono', ui-monospace, Menlo, monospace)" font-size="12">
<style>
  .bg {{ fill: var(--p, #151920); }}
  .t1 {{ fill: var(--b, #FFFFFF); font-weight: 600; font-size: 13px; }}
  .t2 {{ fill: var(--m, #7E8794); font-size: 10.5px; }}
  .t3 {{ fill: var(--f, #E6E9EE); font-size: 11.5px; font-variant-numeric: tabular-nums; }}
  .t4 {{ fill: var(--m, #7E8794); font-size: 11px; font-style: italic; }}
  text.muted {{ fill: var(--m, #7E8794); }}
  rect.k1 {{ fill: var(--s, #E6C070); }}
  rect.k1-dim {{ fill: var(--s, #E6C070); opacity: .45; }}
  rect.other {{ fill: var(--l2, #333944); }}
  rect.muted {{ fill: var(--l, #262B34); }}
  .ref {{ stroke: var(--m, #7E8794); stroke-width: 1; stroke-dasharray: 3 3; }}
</style>
<rect class="bg" width="{W}" height="{h}"/>
{chr(10).join(body)}
</svg>
'''
    (HERE / path).write_text(svg)
    print(f"wrote {path} ({len(panels)} panels, {W}x{h})")


for path, make in [("compile-times/chart.svg", compile_times), ("runtime/chart.svg", runtime),
                   ("comptime/chart.svg", comptime), ("brotli/chart.svg", brotli), ("json/chart.svg", json_suite),
                   ("int-strings/chart.svg", int_strings), ("int-strings/journey.svg", journey), ("loc/chart.svg", loc)]:
    draw(path, make())
