#!/usr/bin/env python3
"""Draws benchmarks.svg from the exports the four run.sh scripts leave under gen/.
No dependencies: every number is read from hyperfine JSON, comptime/gen/results.json
or brotli/gen/bench-*.log, and the SVG is written by hand."""
import glob
import json
import re
import signal
from html import escape
from pathlib import Path

HERE = Path(__file__).resolve().parent
NAMES = {"k1": "K1", "c": "C", "cpp": "C++", "cpp26": "C++26 P2996", "rs": "Rust", "rust": "Rust",
         "go": "Go", "zig": "Zig", "java": "Java", "java-prim": "Java (primitive)", "csharp": "C#", "cs": "C#"}


def hyperfine(path):
    return {r["command"]: r["mean"] for r in json.load(open(path))["results"]}


def compile_time(mode, langs):
    rows = []
    for lang in langs:
        f = HERE / "compile-times" / "gen" / "results" / f"{lang}-{mode}-300.json"
        rows.append((NAMES[lang], next(iter(hyperfine(f).values()))))
    return rows


def runtime(bench):
    return [(NAMES[k], v) for k, v in hyperfine(HERE / "runtime" / "gen" / f"{bench}.json").items()]


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
        rows.append((f"{corpus} {q}", sum(k / c for k, c in pairs) / len(pairs)))
    return rows


def comptime():
    by = {}
    for r in json.load(open(HERE / "comptime" / "gen" / "results.json")):
        by[(r["bench"], r["size"], r["lang"], r["comptime"])] = r

    def cost(bench, size, lang):
        c, r = by.get((bench, size, lang, True)), by.get((bench, size, lang, False))
        return difference(c, r)

    def difference(measured, control, scale=1):
        for row in (measured, control):
            if row is None:
                return "not run"
            if row["status"] != "ok":
                if row["status"] == "error" and row.get("returncode", 0) < 0:
                    return f"killed ({signal.Signals(-row['returncode']).name})"
                return {"error": "compile error", "timeout": "timeout (>300 s)",
                        "skipped": "skipped after timeout"}[row["status"]]
        result = (measured["timing"]["mean"] - control["timing"]["mean"]) * scale
        return result if result > 0 else "below timing resolution"

    def per_type(lang):
        c = by.get(("reflect-serialize", 500, lang, True))
        base = by.get(("reflect-serialize", 0, lang, True))
        return difference(c, base, 1000 / 500)

    langs = ["k1", "zig", "cpp", "rust"]
    return ([(NAMES[l], cost("sieve", 1_000_000, l)) for l in langs],
            [(NAMES[l], cost("crc32", 8_388_608, l)) for l in langs],
            [(NAMES[l], cost("strbuild", 1_000_000, l)) for l in langs],
            [(NAMES[l], per_type(l)) for l in ["k1", "zig", "cpp", "cpp26", "rust"]])


def seconds(v):
    return f"{v:.3f} s" if v < 1 else f"{v:.2f} s"


sieve, crc, strings, reflect = comptime()
PANELS = [
    ("typecheck, 300 generated units", "seconds, lower is better",
     compile_time("check", ["k1", "c", "zig", "rs", "cpp"]) + [("Java (javac)", compile_time("debug", ["java"])[0][1]),
                                                              ("Go (go build)", compile_time("debug", ["go"])[0][1])], seconds),
    ("default/debug build, same program", "seconds, lower is better",
     compile_time("debug", ["k1", "c", "rs", "go", "zig", "java", "cpp"]), seconds),
    ("optimized build, same program", "seconds, lower is better; C# emits IL",
     compile_time("opt", ["k1", "c", "rs", "zig", "cpp", "cs"])
     + [("Go", compile_time("debug", ["go"])[0][1])], seconds),
    ("hashmap, 5M inserts + 5M lookups", "seconds, lower is better", runtime("hashmap"), seconds),
    ("byte-scan, 256 MiB", "seconds, lower is better", runtime("byte-scan"), seconds),
    ("binary-trees, depth 20", "seconds, lower is better", runtime("binary-trees"), seconds),
    ("sieve to 1M at compile time", "compile-time cost in seconds", sieve, seconds),
    ("crc32 over 8 MB at compile time", "compile-time cost in seconds", crc, seconds),
    ("format 1M numbers at compile time", "compile-time cost in seconds", strings, seconds),
    ("reflection-generated serializer", "ms per type, 500 types", reflect, lambda v: f"{v:.2f} ms"),
    ("brotli encoder, K1 port vs C", "K1 throughput as a fraction of C, per corpus", brotli(), lambda v: f"{v:.2f}x", (1.0, "C")),
    ("brotli optimized build", "seconds; C: 4 objects, no link",
     [(name, next(iter(hyperfine(HERE / "compile-times" / "gen" / "results" / f"{key}-opt.json").values())))
      for key, name in [("brotli-k1", "K1"), ("brotli-c", "C (serial)"), ("brotli-cpar", "C (parallel)")]], seconds),
]

W, COLS, PAD, LABEL, VALUE, ROW, BAR = 1080, 3, 24, 118, 82, 22, 14
PW = (W - PAD * (COLS + 1)) // COLS
BARW = PW - LABEL - VALUE


def bar_path(x, y, w, h):
    r = min(4, w / 2)
    return (f"M{x},{y} h{w - r:.2f} a{r},{r} 0 0 1 {r},{r} v{h - 2 * r:.2f} "
            f"a{r},{r} 0 0 1 -{r},{r} h-{w - r:.2f} z")


def panel(x, y, title, subtitle, rows, fmt, ref=None):
    out = [f'<text class="t1" x="{x}" y="{y + 14}">{title}</text>',
           f'<text class="t2" x="{x}" y="{y + 30}">{subtitle}</text>']
    numeric = [v for _, v in rows if isinstance(v, float)]
    scale = BARW / max(numeric + ([ref[0]] if ref else [1e-9]))
    top = cy = y + 44
    if ref is None:
        rows = sorted(rows, key=lambda r: (not isinstance(r[1], float), r[1] if isinstance(r[1], float) else 0))
    for name, v in rows:
        out.append(f'<text class="t3" x="{x + LABEL - 10}" y="{cy + 11}" text-anchor="end">{name}</text>')
        if isinstance(v, float):
            w = max(v * scale, 1.5)
            cls = "other" if ref is None and name != "K1" else "k1"
            out.append(f'<path class="{cls}" d="{bar_path(x + LABEL, cy, w, BAR)}"/>')
            value_x = x + LABEL + (BARW if ref else w) + 6
            out.append(f'<text class="t3" x="{value_x:.1f}" y="{cy + 11}">{fmt(v)}</text>')
        else:
            out.append(f'<text class="t4" x="{x + LABEL}" y="{cy + 11}">{escape(v)}</text>')
        cy += ROW
    if ref:
        rx = x + LABEL + ref[0] * scale
        out[2:2] = [f'<line class="ref" x1="{rx:.1f}" y1="{top - 4}" x2="{rx:.1f}" y2="{cy - 4}"/>',
                    f'<text class="t2" x="{rx:.1f}" y="{top - 8}" text-anchor="middle">{ref[1]}</text>']
    return out, cy


def main():
    body, y = [], PAD
    for i in range(0, len(PANELS), COLS):
        tallest = y
        for j, spec in enumerate(PANELS[i:i + COLS]):
            out, bottom = panel(PAD + j * (PW + PAD), y, *spec)
            body += out
            tallest = max(tallest, bottom)
        y = tallest + 28
    env = (HERE / "runtime" / "gen" / "env.txt").read_text().splitlines()
    machine = env[0].split(": ", 1)[1]
    head = re.search(r"repo HEAD when run: (\w+)", "\n".join(env))[1]
    info_path = HERE / "run-info.json"
    provenance = json.loads(info_path.read_text()) if info_path.exists() else {}
    revision = f"{head} + working tree" if provenance.get("working_tree") else head
    measured = provenance.get("completed_utc", "")[:10]
    body.append(f'<text class="t2" x="{PAD}" y="{y}">{escape(machine)}. '
                f'K1 {revision}. Measured {measured}.</text>')
    y += 16
    body.append(f'<text class="t2" x="{PAD}" y="{y}">'
                'Hyperfine means; Brotli averages the per-run K1/C ratios. Methods and limits: each suite’s results.md.</text>')
    h = y + PAD
    svg = f'''<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{h}" viewBox="0 0 {W} {h}" font-family="system-ui, -apple-system, Helvetica, Arial, sans-serif" font-size="12">
<style>
  .bg {{ fill: #fcfcfb; }}
  .t1 {{ fill: #0b0b0b; font-weight: 600; font-size: 13px; }}
  .t2 {{ fill: #52514e; font-size: 11px; }}
  .t3 {{ fill: #0b0b0b; font-variant-numeric: tabular-nums; }}
  .t4 {{ fill: #898781; font-style: italic; }}
  .k1 {{ fill: #2a78d6; }}
  .other {{ fill: #c3c2b7; }}
  .ref {{ stroke: #898781; stroke-width: 1; }}
  @media (prefers-color-scheme: dark) {{
    .bg {{ fill: #1a1a19; }}
    .t1, .t3 {{ fill: #ffffff; }}
    .t2 {{ fill: #c3c2b7; }}
    .k1 {{ fill: #3987e5; }}
    .other {{ fill: #4a4946; }}
  }}
</style>
<rect class="bg" width="{W}" height="{h}"/>
{chr(10).join(body)}
</svg>
'''
    (HERE / "benchmarks.svg").write_text(svg)
    print(f"wrote benchmarks.svg ({W}x{h})")


main()
