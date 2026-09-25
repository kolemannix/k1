#!/usr/bin/env python3
import subprocess
from html import escape
from pathlib import Path

HERE = Path(__file__).resolve().parent

ROWS = [
    ("lemire", "C++ std::to_string", "Lemire's M4 Max", 183.8),
    ("lemire", "Go strconv.Itoa", "Lemire's M4 Max", 84.3),
    ("lemire", "Bun String(i)", "Lemire's M4 Max", 68.5),
    ("lemire", "Rust to_string()", "Lemire's M4 Max", 63.6),
    ("lemire", "Python str(i)", "Lemire's M4 Max", 22.9),
    ("cpp", "C++ std::to_string", "clang -O3, this machine", 160.0),
    ("k1", "k1 show()", "naive formatter, growing builder", 63.9),
    ("k1", "k1 show()", "stack buffer", 68.1),
    ("k1", "k1 show()", "two digits per step", 174.2),
    ("k1", "k1 smolstr", "23 bytes inline, no spill (unfair)", 408.3),
    ("k1", "k1 spill-string[22]", "spill-list as either (fair)", 397.9),
    ("k1", "k1 spill-string[22]", "spill-list as hand-rolled union", 416.9),
]

W, LEFT, RIGHT, TOP, ROW, BAR, MAX = 1200, 330, 90, 96, 50, 24, 450


def bar(x0, y, w, cls):
    r = min(4, w)
    return (f'<path class="{cls}" d="M{x0},{y} h{w - r} a{r},{r} 0 0 1 {r},{r} '
            f'v{BAR - 2 * r} a{r},{r} 0 0 1 -{r},{r} h-{w - r} z"/>')


def main():
    h = TOP + ROW * len(ROWS) + 40
    scale = (W - LEFT - RIGHT) / MAX
    body = []
    for tick in range(0, MAX + 1, 50):
        x = LEFT + tick * scale
        body.append(f'<line class="grid" x1="{x:.1f}" y1="{TOP - 8}" x2="{x:.1f}" y2="{h - 36}"/>')
        body.append(f'<text class="axis" x="{x:.1f}" y="{h - 16}" text-anchor="middle">{tick}</text>')
    for i, (kind, name, detail, value) in enumerate(ROWS):
        y = TOP + i * ROW
        mid = y + BAR / 2
        muted = kind == "lemire"
        body.append(f'<text class="{"name-muted" if muted else "name"}" x="{LEFT - 14}" y="{mid - 2}" '
                    f'text-anchor="end">{escape(name)}</text>')
        body.append(f'<text class="detail" x="{LEFT - 14}" y="{mid + 15}" text-anchor="end">{escape(detail)}</text>')
        w = value * scale
        body.append(bar(LEFT, y, w, kind))
        body.append(f'<text class="{"value-muted" if muted else "value"}" x="{LEFT + w + 8:.1f}" y="{mid + 5}">'
                    f'{value:.1f}</text>')
    svg = f'''<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{h}" viewBox="0 0 {W} {h}" font-family="system-ui, -apple-system, Helvetica, Arial, sans-serif">
<style>
  .bg {{ fill: #fcfcfb; }}
  .title {{ fill: #0b0b0b; font-weight: 600; font-size: 26px; }}
  .sub {{ fill: #52514e; font-size: 16px; }}
  .name {{ fill: #0b0b0b; font-weight: 600; font-size: 16px; }}
  .name-muted {{ fill: #898781; font-weight: 600; font-size: 16px; font-style: italic; }}
  .detail {{ fill: #52514e; font-size: 13px; }}
  .value {{ fill: #0b0b0b; font-size: 16px; font-variant-numeric: tabular-nums; }}
  .value-muted {{ fill: #898781; font-size: 16px; font-style: italic; }}
  .axis {{ fill: #898781; font-size: 13px; }}
  .grid {{ stroke: #e8e7e3; stroke-width: 1; }}
  .k1 {{ fill: #2a78d6; }}
  .cpp {{ fill: #c3c2b7; }}
  .lemire {{ fill: #e8e7e3; stroke: #c3c2b7; stroke-width: 1; stroke-dasharray: 4 3; }}
</style>
<rect class="bg" width="{W}" height="{h}"/>
<text class="title" x="32" y="44">Integer to string, million strings per second</text>
<text class="sub" x="32" y="72">buf[i &amp; 1023] = to_string(i) for i in 0..100M, best of 5, Apple M3 Max unless noted. Higher is better.</text>
{chr(10).join(body)}
</svg>
'''
    (HERE / "chart.svg").write_text(svg)
    subprocess.run(["rsvg-convert", "-z", "2", "-o", str(HERE / "chart.png"), str(HERE / "chart.svg")], check=True)


main()
