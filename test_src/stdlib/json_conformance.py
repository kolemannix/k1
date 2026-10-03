#!/usr/bin/env python3
"""Regenerates json_conformance.k1 from a JSONTestSuite checkout: json_conformance.py <path/to/JSONTestSuite>"""
import os, sys
src = os.path.join(sys.argv[1], 'test_parsing')
MAX = 4096
def k1_string(b):
    out = []
    for c in b:
        if c == 0x22: out.append('\\"')
        elif c == 0x5C: out.append('\\\\')
        elif c == 0x0A: out.append('\\n')
        elif c == 0x09: out.append('\\t')
        elif c == 0x0D: out.append('\\r')
        elif c == 0x24: out.append('\\$')
        elif 0x20 <= c <= 0x7E: out.append(chr(c))
        else: return None
    return '"' + ''.join(out) + '"'
lines = []
for name in sorted(os.listdir(src)):
    b = open(os.path.join(src, name), 'rb').read()
    if len(b) > MAX: continue
    verdict = {'y': ':accept', 'n': ':reject', 'i': ':any'}[name[0]]
    lit = k1_string(b)
    stem = name[:-5]
    if lit is not None:
        lines.append(f'  check("{stem}", {lit}, {verdict})')
    else:
        lines.append(f'  check-bytes("{stem}", [{", ".join(str(c) for c in b)}], {verdict})')
body = '\n'.join(lines)
out = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'json_conformance.k1')
open(out, 'w').write(f"""ns json_conformance
use std/json

type verdict = either {{ accept, reject, any }}

fn check(name: string, text: string, v: verdict) {{ check-bytes(name, text.span-bytes(), v) }}

fn check-bytes(name: string, bytes: span[u8], v: verdict) {{
  let r = json/parse[json/json](string/wrap-bytes(bytes))
  if v is {{
    :accept -> if r is :err e {{ crash("$name should parse: $e") }},
    :reject -> if r is :ok _ {{ crash("$name should be rejected") }},
    :any -> {{}},
  }}
}}

fn test() {{
{body}
}}
""")
print(len(lines), 'cases')
