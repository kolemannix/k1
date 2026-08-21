#!/usr/bin/env python3
"""Generates perf/stress100, a large K1 program for compiler profiling.

Usage: python3 perf/gen_stress.py [units]

Each unit is an independent namespace shaped like real application code:
mostly concrete types (structs and sums) and plain functions full of
matches, loops, and blocks, which instantiate a small set of generics
(bx, duo, ?t, list[t]) at many types. Each unit keeps two abilities and
a handful of impls so specialization, inference, and impl resolution
stay in the profile, but the bulk is ordinary expression and statement
typechecking, matching the density of real programs. suite1 compiles
too fast for most changes to be measurable.
"""

import sys
from pathlib import Path

UNITS = int(sys.argv[1]) if len(sys.argv) > 1 else 300

UNSIGNED = ["u8", "u16", "u32", "u64"]
SCALARS = UNSIGNED + ["bool", "string", "f32", "f64"]
SIZED = ["u32", "u64"]


def widen(t: str) -> str:
    return "" if t == "u64" else ".as[u64]"


def unit(i: int) -> str:
    impls_sz = "\n".join(
        f"  impl sz for {t} {{ fn sz(self): u64 "
        f"{{ self{widen(t)} + {n + 1} }} }}"
        for n, t in enumerate(SIZED)
    )
    impls_enc = "\n".join(
        f"  impl enc[o = u64] for {t} {{ fn enc(self): u64 "
        f"{{ self{widen(t)} }} }}"
        for t in SIZED
    )
    boxed = "\n".join(
        f"  acc = acc + get(mk({n + 1}: {t})){widen(t)}"
        for n, t in enumerate(UNSIGNED)
    )
    totals = "\n".join(
        f"  acc = acc + total(mk({n + 1}: {t}))" for n, t in enumerate(SIZED)
    )
    deep = "\n".join(
        f"  acc = acc + get(get(wrap2({n + 1}: {t}))){widen(t)}"
        for n, t in enumerate(UNSIGNED)
    )
    pairs = []
    for n in range(len(SCALARS)):
        a = SCALARS[n]
        b = SCALARS[(n + 1) % len(SCALARS)]
        pairs.append(
            f"  let p{n} = swap(mk-pair(zero[{a}](), zero[{b}]()))\n"
            f"  acc = acc + measure(p{n}.first) + measure(p{n}.second)"
        )
    pairs = "\n".join(pairs)
    encs = "\n".join(
        f"  acc = acc + encode({n + 1}: {t})" for n, t in enumerate(SIZED)
    )
    return f"""ns u{i} {{
  type bx[t] = {{ value: t, tag: u64 }}
  type duo[a, b] = {{ first: a, second: b }}
  type vec2 = {{ x: u64, y: u64 }}
  type shape = either {{ dot, line(u64), rect(vec2) }}
  type op = either {{ push(u64), pop, reset }}

  type item = {{ sku: u64, qty: u32, unit-price: u64, discount: ?u64, note: ?string }}
  type order-status = either {{
    draft,
    placed(u64),
    shipped({{ carrier: u64, eta: u64 }}),
    delivered(u64),
    canceled(string)
  }}
  type order = {{ id: u64, status: order-status, lines: list[item], shipping: duo[u64, u64], paid: bool }}
  type event = either {{ credit(u64), debit(u64), hold(duo[u64, u64]), release, note(string) }}
  type account = {{ id: u64, balance: u64, held: u64, tier: u8, closed: bool, last-event: ?event }}
  type sample = {{ at: u64, value: u64, quality: u8 }}
  type series = {{ name: string, points: list[sample], window: duo[u64, u64] }}
  type stat = either {{ count(u64), sum(u64), extent(duo[u64, u64]), empty }}

  fn area(s: shape): u64 {{
    s is {{
      :dot -> 1,
      :line(len) -> len,
      :rect(d) -> d.x * d.y
    }}
  }}

  fn step(state: vec2, o: op): vec2 {{
    o is {{
      :push(n) -> .{{ x = state.x + n, y = state.y + 1 }},
      :pop -> {{
        if state.y > 0 {{
          .{{ x = state.x, y = state.y - 1 }}
        }} else state
      }},
      :reset -> .{{ x = 0, y = 0 }}
    }}
  }}

  fn churn(seed: u64): u64 {{
    let base = seed + {i}
    let low = base % 7
    let high = base / 3
    let start: vec2 = .{{ x = low, y = high }}
    let s: shape = if low > high :rect(start) else if low == 0 :dot else :line(high)
    let a = area(s)
    let cur = start
    cur = step(cur, :push(a))
    cur = step(cur, :push(low))
    cur = step(cur, :pop)
    cur = if cur.x > 100 step(cur, :reset) else cur
    let k = 0: u64
    let sum = 0: u64
    while k < low {{
      sum = sum + k * 2 + cur.x
      k = k + 1
    }}
    let verdict = cur is {{
      .{{ x = 0, y }} -> y,
      .{{ x, y }} if x == y -> x + y,
      .{{ x, y }} -> x * 2 + y
    }}
    if verdict > sum verdict - sum else sum - verdict + a
  }}

  fn line-cost(line: item): u64 {{
    let gross = line.unit-price * line.qty.as[u64]
    let disc = line.discount ? 0
    let cut = if line.note is :some(_) 1: u64 else 0
    if disc + cut >= gross 0: u64 else gross - disc - cut
  }}

  fn order-total(o: order): u64 {{
    let sum = 0: u64
    for line in o.lines {{
      sum = sum + line-cost(line)
    }}
    let ship = o.shipping.first + o.shipping.second
    if o.paid sum else sum + ship
  }}

  fn advance(o: order, now: u64): order {{
    let next: order-status = o.status is {{
      :draft -> :placed(now),
      :placed(at) -> {{
        if now - at > 10 {{
          :shipped(.{{ carrier = {i}, eta = now + 3 }})
        }} else :placed(at)
      }},
      :shipped(s) -> if now >= s.eta :delivered(now) else :shipped(s),
      :delivered(at) -> :delivered(at),
      :canceled(why) -> :canceled(why)
    }}
    o.with(.{{ status = next }})
  }}

  fn apply-event(a: account, e: event): account {{
    if a.closed {{
      return a
    }}
    let next = e is {{
      :credit(n) -> a.with(.{{ balance = a.balance + n }}),
      :debit(n) -> {{
        if n > a.balance {{
          a.with(.{{ closed = true }})
        }} else {{
          a.with(.{{ balance = a.balance - n }})
        }}
      }},
      :hold(h) -> a.with(.{{ held = a.held + h.first + h.second }}),
      :release -> a.with(.{{ balance = a.balance + a.held, held = 0 }}),
      :note(_) -> a
    }}
    next.with(.{{ last-event = :some(e) }})
  }}

  fn settle(seed: u64): u64 {{
    let lines = list/empty[item]()
    let count = seed % 5 + 1
    for k in (0: u64).until(count) {{
      lines.push(.{{
        sku = seed * 31 + k,
        qty = (k + 1).as[u32],
        unit-price = 100 + k * 7,
        discount = if k % 2 == 0 :some(k * 3) else :none,
        note = :none
      }})
    }}
    let o: order = .{{
      id = seed,
      status = :draft,
      lines,
      shipping = mk-pair(5: u64, seed % 9),
      paid = seed % 3 == 0
    }}
    let t = 0: u64
    while t < 4 {{
      o = advance(o, seed + t * 6)
      t = t + 1
    }}
    let due = order-total(o)
    let acct: account = .{{
      id = seed,
      balance = due,
      held = 0,
      tier = (seed % 4).as[u8],
      closed = false,
      last-event = :none
    }}
    acct = apply-event(acct, :hold(mk-pair(due / 2, 1: u64)))
    acct = apply-event(acct, :debit(seed % 50))
    acct = apply-event(acct, :release)
    acct = apply-event(acct, :credit(3))
    let bonus = acct.last-event is {{
      :some(:credit(n)) -> n,
      :some(_) -> 1,
      :none -> 0
    }}
    let late = o.status is {{ :delivered(at) -> at % 13, _ -> 0 }}
    acct.balance + bonus + late
  }}

  fn quantize(v: u64): u8 {{
    if v < 10 0: u8
    else if v < 100 1
    else if v < 1000 2
    else 3
  }}

  fn summarize(s: series): stat {{
    if s.points.len == 0 {{
      return :empty
    }}
    let lo = s.points.get(0).value
    let hi = lo
    let total = 0: u64
    let kept = 0: u64
    for p in s.points {{
      if p.quality == 0 {{
        continue
      }}
      if p.at < s.window.first or p.at > s.window.second {{
        continue
      }}
      if p.value < lo {{
        lo = p.value
      }}
      if p.value > hi {{
        hi = p.value
      }}
      total = total + p.value
      kept = kept + 1
    }}
    if kept == 0 :count(0)
    else if kept == 1 :sum(total)
    else :extent(mk-pair(lo, hi))
  }}

  fn sample-run(seed: u64): u64 {{
    let pts = list/empty[sample]()
    let cap = seed % 7 + 2
    let k = 0: u64
    while k < cap {{
      let q = if k % 3 == 0 {{ 0: u8 }} else quantize(k * seed + {i})
      pts.push(.{{ at = k * 10, value = (seed + k * k) % 251, quality = q }})
      k = k + 1
    }}
    let s: series = .{{ name = "run", points = pts, window = mk-pair(0: u64, 900 + seed) }}
    let verdict = summarize(s) is {{
      :empty -> 0: u64,
      :count(n) -> n,
      :sum(n) -> n % 100,
      :extent(e) -> e.second - e.first
    }}
    verdict + k
  }}

  ability sz {{ fn sz(self): u64 }}
  ability enc[o] {{ fn enc(self): o }}

{impls_sz}
  impl sz for item {{ fn sz(self): u64 {{ self.sku % 1000 + self.qty.as[u64] }} }}
  impl sz for sample {{ fn sz(self): u64 {{ self.at + self.value }} }}
  impl[t: sz] sz for bx[t] {{ fn sz(self): u64 {{ self.value.sz() + 1 }} }}
{impls_enc}
  impl enc[o = u64] for item {{ fn enc(self): u64 {{ self.sku * 31 + self.unit-price }} }}

  fn mk[t](value: t): bx[t] {{ .{{ value, tag = {i} }} }}
  fn get[t](b: bx[t]): t {{ b.value }}
  fn wrap2[t](value: t): bx[bx[t]] {{ mk(mk(value)) }}
  fn mk-pair[a, b](first: a, second: b): duo[a, b] {{ .{{ first, second }} }}
  fn swap[a, b](p: duo[a, b]): duo[b, a] {{ .{{ first = p.second, second = p.first }} }}
  fn zero[t](): ?t {{ :none }}
  fn measure[t](v: ?t): u64 {{ if v is :some(_) 1 else 0 }}
  fn total[t: sz](v: t): u64 {{ v.sz() }}
  fn encode[t: enc[o = u64]](v: t): u64 {{ v.enc() }}

  fn test(): u64 {{
  let acc = {i}: u64
{boxed}
{totals}
{deep}
{pairs}
{encs}
  let probe: item = .{{ sku = {i}, qty = 2, unit-price = 40, discount = :none, note = :some("x") }}
  let probe2: sample = .{{ at = 1, value = acc % 5, quality = 1 }}
  acc = acc + total(probe) + encode(probe) + total(mk(probe)) + total(probe2)
  acc = acc + churn(acc)
  acc = acc + churn(acc % 97)
  acc = acc + settle(acc % 61)
  acc = acc + settle({i} + 7)
  acc = acc + sample-run(acc % 43)
  acc = acc + sample-run({i} + 11)
  acc
  }}
}}
"""


def main() -> None:
    out_dir = Path(__file__).parent / "stress100"
    out_dir.mkdir(parents=True, exist_ok=True)
    parts = [f"// Generated by perf/gen_stress.py {UNITS} -- do not edit.\n"]
    for i in range(UNITS):
        parts.append(unit(i))
    calls = "\n".join(f"  acc = acc + u{i}/test()" for i in range(UNITS))
    parts.append(f"fn main(): i32 {{\n  let acc = 0: u64\n{calls}\n  if acc > 0 0 else 1\n}}\n")
    out = out_dir / "stress100.k1"
    out.write_text("\n".join(parts))
    print(f"wrote {out} ({UNITS} units, {sum(p.count(chr(10)) for p in parts)} lines)")


main()
