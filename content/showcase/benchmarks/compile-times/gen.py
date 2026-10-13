#!/usr/bin/env python3
"""Emits perf/gen_stress.py's program shape in k1, C, C++, Rust, Go, Zig, OCaml, Java, Kotlin and C#.

Usage: gen.py <units> <outdir>   -> <outdir>/stress.{k1,c,cpp,rs,go,zig,ml,kt,cs}, stress-hand-vec.cpp, Stress.java, stress.csproj
       gen.py hello <outdir>     -> <outdir>/hello.{k1,c,cpp,rs,go,zig,ml,kt,cs}, Hello.java, hello.csproj

The k1 text is gen_stress.py's lib() and unit() verbatim. Every other
language gets the same program: one shared lib block (the generic bx/duo
types, the two abilities, their scalar and blanket impls, and the generics),
emitted once, then the same unit, statement for statement: the unit's own
types and eleven functions, the two abilities implemented for its item and
sample, and the same calls into the shared generics in test(). C has no
generics, so its lib block is the generics instantiated at the scalar types
once, and each unit still instantiates them at its own item and sample.
`§` in a template is the unit index.
"""

import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[3]
STRESS = REPO / "perf" / "gen_stress.py"

UNSIGNED = ["u8", "u16", "u32", "u64"]
SCALARS = UNSIGNED + ["bool", "string", "f32", "f64"]
SIZED = ["u32", "u64"]
PAIRS = [(SCALARS[n], SCALARS[(n + 1) % len(SCALARS)]) for n in range(len(SCALARS))]
LANGS = ["k1", "c", "cpp", "cpp-hand-vec", "rs", "go", "zig", "ml", "java", "kt", "cs"]


def k1_unit_fn():
    src = STRESS.read_text()
    src = src[: src.rindex("\nmain()")]
    saved = sys.argv
    sys.argv = [str(STRESS)]
    ns = {}
    exec(compile(src, str(STRESS), "exec"), ns)
    sys.argv = saved
    return ns["lib"], ns["unit"]


def k1_program(n):
    lib, unit = k1_unit_fn()
    parts = [lib()]
    for i in range(n):
        parts.append(unit(i))
    calls = "\n".join(f"  acc = acc + u{i}/test()" for i in range(n))
    parts.append(
        "let NONCE: u64 = 0\n\n"
        f"fn main(): i32 {{\n  let acc = NONCE\n{calls}\n  if acc > 0 0 else 1\n}}\n"
    )
    return "\n".join(parts)



C_TY = {
    "u8": "uint8_t", "u16": "uint16_t", "u32": "uint32_t", "u64": "uint64_t",
    "bool": "bool", "string": "const char *", "f32": "float", "f64": "double",
}

C_UNIT = r"""
typedef struct { uint64_t x; uint64_t y; } u§_vec2;
typedef struct { enum { u§_shape_dot, u§_shape_line, u§_shape_rect } tag; union { uint64_t line; u§_vec2 rect; }; } u§_shape;
typedef struct { enum { u§_op_push, u§_op_pop, u§_op_reset } tag; union { uint64_t push; }; } u§_op;
typedef struct { uint64_t sku; uint32_t qty; uint64_t unit_price; lib_opt_u64 discount; lib_opt_string note; } u§_item;
typedef struct { uint64_t carrier; uint64_t eta; } u§_shipped_info;
typedef struct { enum { u§_status_draft, u§_status_placed, u§_status_shipped, u§_status_delivered, u§_status_canceled } tag; union { uint64_t placed; u§_shipped_info shipped; uint64_t delivered; const char *canceled; }; } u§_order_status;
typedef struct { u§_item *data; size_t len; size_t cap; } u§_list_item;
typedef struct { uint64_t id; u§_order_status status; u§_list_item lines; lib_duo_u64_u64 shipping; bool paid; } u§_order;
typedef struct { enum { u§_event_credit, u§_event_debit, u§_event_hold, u§_event_release, u§_event_note } tag; union { uint64_t credit; uint64_t debit; lib_duo_u64_u64 hold; const char *note; }; } u§_event;
typedef struct { bool has; u§_event val; } u§_opt_event;
typedef struct { uint64_t id; uint64_t balance; uint64_t held; uint8_t tier; bool closed; u§_opt_event last_event; } u§_account;
typedef struct { uint64_t at; uint64_t value; uint8_t quality; } u§_sample;
typedef struct { u§_sample *data; size_t len; size_t cap; } u§_list_sample;
typedef struct { const char *name; u§_list_sample points; lib_duo_u64_u64 window; } u§_series;
typedef struct { enum { u§_stat_count, u§_stat_sum, u§_stat_extent, u§_stat_empty } tag; union { uint64_t count; uint64_t sum; lib_duo_u64_u64 extent; }; } u§_stat;
typedef struct { u§_item value; uint64_t tag; } u§_bx_item;

static void u§_push_item(u§_list_item *l, u§_item v) {
  if (l->len == l->cap) {
    l->cap = l->cap ? l->cap * 2 : 4;
    l->data = realloc(l->data, l->cap * sizeof(u§_item));
  }
  l->data[l->len++] = v;
}

static void u§_push_sample(u§_list_sample *l, u§_sample v) {
  if (l->len == l->cap) {
    l->cap = l->cap ? l->cap * 2 : 4;
    l->data = realloc(l->data, l->cap * sizeof(u§_sample));
  }
  l->data[l->len++] = v;
}

static uint64_t u§_area(u§_shape s) {
  switch (s.tag) {
    case u§_shape_dot: return 1;
    case u§_shape_line: return s.line;
    default: return s.rect.x * s.rect.y;
  }
}

static u§_vec2 u§_step(u§_vec2 state, u§_op o) {
  switch (o.tag) {
    case u§_op_push: return (u§_vec2){ state.x + o.push, state.y + 1 };
    case u§_op_pop:
      if (state.y > 0) {
        return (u§_vec2){ state.x, state.y - 1 };
      } else {
        return state;
      }
    default: return (u§_vec2){ 0, 0 };
  }
}

static uint64_t u§_churn(uint64_t seed) {
  uint64_t base = seed + §;
  uint64_t low = base % 7;
  uint64_t high = base / 3;
  u§_vec2 start = { low, high };
  u§_shape s;
  if (low > high) {
    s = (u§_shape){ .tag = u§_shape_rect, .rect = start };
  } else if (low == 0) {
    s = (u§_shape){ .tag = u§_shape_dot };
  } else {
    s = (u§_shape){ .tag = u§_shape_line, .line = high };
  }
  uint64_t a = u§_area(s);
  u§_vec2 cur = start;
  cur = u§_step(cur, (u§_op){ .tag = u§_op_push, .push = a });
  cur = u§_step(cur, (u§_op){ .tag = u§_op_push, .push = low });
  cur = u§_step(cur, (u§_op){ .tag = u§_op_pop });
  cur = cur.x > 100 ? u§_step(cur, (u§_op){ .tag = u§_op_reset }) : cur;
  uint64_t k = 0;
  uint64_t sum = 0;
  while (k < low) {
    sum = sum + k * 2 + cur.x;
    k = k + 1;
  }
  uint64_t verdict;
  if (cur.x == 0) {
    verdict = cur.y;
  } else if (cur.x == cur.y) {
    verdict = cur.x + cur.y;
  } else {
    verdict = cur.x * 2 + cur.y;
  }
  return verdict > sum ? verdict - sum : sum - verdict + a;
}

static uint64_t u§_line_cost(u§_item line) {
  uint64_t gross = line.unit_price * (uint64_t)line.qty;
  uint64_t disc = line.discount.has ? line.discount.val : 0;
  uint64_t cut = line.note.has ? 1 : 0;
  return disc + cut >= gross ? 0 : gross - disc - cut;
}

static uint64_t u§_order_total(u§_order o) {
  uint64_t sum = 0;
  for (size_t n = 0; n < o.lines.len; n++) {
    u§_item line = o.lines.data[n];
    sum = sum + u§_line_cost(line);
  }
  uint64_t ship = o.shipping.first + o.shipping.second;
  return o.paid ? sum : sum + ship;
}

static u§_order u§_advance(u§_order o, uint64_t now) {
  u§_order_status next;
  switch (o.status.tag) {
    case u§_status_draft:
      next = (u§_order_status){ .tag = u§_status_placed, .placed = now };
      break;
    case u§_status_placed:
      if (now - o.status.placed > 10) {
        next = (u§_order_status){ .tag = u§_status_shipped, .shipped = { §, now + 3 } };
      } else {
        next = (u§_order_status){ .tag = u§_status_placed, .placed = o.status.placed };
      }
      break;
    case u§_status_shipped:
      next = now >= o.status.shipped.eta
        ? (u§_order_status){ .tag = u§_status_delivered, .delivered = now }
        : (u§_order_status){ .tag = u§_status_shipped, .shipped = o.status.shipped };
      break;
    case u§_status_delivered:
      next = (u§_order_status){ .tag = u§_status_delivered, .delivered = o.status.delivered };
      break;
    default:
      next = (u§_order_status){ .tag = u§_status_canceled, .canceled = o.status.canceled };
      break;
  }
  o.status = next;
  return o;
}

static u§_account u§_apply_event(u§_account a, u§_event e) {
  if (a.closed) {
    return a;
  }
  u§_account next = a;
  switch (e.tag) {
    case u§_event_credit:
      next.balance = a.balance + e.credit;
      break;
    case u§_event_debit:
      if (e.debit > a.balance) {
        next.closed = true;
      } else {
        next.balance = a.balance - e.debit;
      }
      break;
    case u§_event_hold:
      next.held = a.held + e.hold.first + e.hold.second;
      break;
    case u§_event_release:
      next.balance = a.balance + a.held;
      next.held = 0;
      break;
    default:
      break;
  }
  next.last_event = (u§_opt_event){ true, e };
  return next;
}

static uint64_t u§_settle(uint64_t seed) {
  u§_list_item lines = { 0 };
  uint64_t count = seed % 5 + 1;
  for (uint64_t k = 0; k < count; k++) {
    u§_push_item(&lines, (u§_item){
      .sku = seed * 31 + k,
      .qty = (uint32_t)(k + 1),
      .unit_price = 100 + k * 7,
      .discount = k % 2 == 0 ? (lib_opt_u64){ true, k * 3 } : (lib_opt_u64){ false, 0 },
      .note = { false, NULL }
    });
  }
  u§_order o = {
    .id = seed,
    .status = { .tag = u§_status_draft },
    .lines = lines,
    .shipping = lib_mk_pair_u64_u64(5, seed % 9),
    .paid = seed % 3 == 0
  };
  uint64_t t = 0;
  while (t < 4) {
    o = u§_advance(o, seed + t * 6);
    t = t + 1;
  }
  uint64_t due = u§_order_total(o);
  u§_account acct = {
    .id = seed,
    .balance = due,
    .held = 0,
    .tier = (uint8_t)(seed % 4),
    .closed = false,
    .last_event = { false }
  };
  acct = u§_apply_event(acct, (u§_event){ .tag = u§_event_hold, .hold = lib_mk_pair_u64_u64(due / 2, 1) });
  acct = u§_apply_event(acct, (u§_event){ .tag = u§_event_debit, .debit = seed % 50 });
  acct = u§_apply_event(acct, (u§_event){ .tag = u§_event_release });
  acct = u§_apply_event(acct, (u§_event){ .tag = u§_event_credit, .credit = 3 });
  uint64_t bonus;
  if (acct.last_event.has && acct.last_event.val.tag == u§_event_credit) {
    bonus = acct.last_event.val.credit;
  } else if (acct.last_event.has) {
    bonus = 1;
  } else {
    bonus = 0;
  }
  uint64_t late = o.status.tag == u§_status_delivered ? o.status.delivered % 13 : 0;
  return acct.balance + bonus + late;
}

static uint8_t u§_quantize(uint64_t v) {
  if (v < 10) {
    return 0;
  } else if (v < 100) {
    return 1;
  } else if (v < 1000) {
    return 2;
  } else {
    return 3;
  }
}

static u§_stat u§_summarize(u§_series s) {
  if (s.points.len == 0) {
    return (u§_stat){ .tag = u§_stat_empty };
  }
  uint64_t lo = s.points.data[0].value;
  uint64_t hi = lo;
  uint64_t total = 0;
  uint64_t kept = 0;
  for (size_t n = 0; n < s.points.len; n++) {
    u§_sample p = s.points.data[n];
    if (p.quality == 0) {
      continue;
    }
    if (p.at < s.window.first || p.at > s.window.second) {
      continue;
    }
    if (p.value < lo) {
      lo = p.value;
    }
    if (p.value > hi) {
      hi = p.value;
    }
    total = total + p.value;
    kept = kept + 1;
  }
  if (kept == 0) {
    return (u§_stat){ .tag = u§_stat_count, .count = 0 };
  } else if (kept == 1) {
    return (u§_stat){ .tag = u§_stat_sum, .sum = total };
  } else {
    return (u§_stat){ .tag = u§_stat_extent, .extent = lib_mk_pair_u64_u64(lo, hi) };
  }
}

static uint64_t u§_sample_run(uint64_t seed) {
  u§_list_sample pts = { 0 };
  uint64_t cap = seed % 7 + 2;
  uint64_t k = 0;
  while (k < cap) {
    uint8_t q = k % 3 == 0 ? 0 : u§_quantize(k * seed + §);
    u§_push_sample(&pts, (u§_sample){ .at = k * 10, .value = (seed + k * k) % 251, .quality = q });
    k = k + 1;
  }
  u§_series s = { .name = "run", .points = pts, .window = lib_mk_pair_u64_u64(0, 900 + seed) };
  u§_stat st = u§_summarize(s);
  uint64_t verdict;
  switch (st.tag) {
    case u§_stat_empty: verdict = 0; break;
    case u§_stat_count: verdict = st.count; break;
    case u§_stat_sum: verdict = st.sum % 100; break;
    default: verdict = st.extent.second - st.extent.first; break;
  }
  return verdict + k;
}

static uint64_t u§_sz_item(u§_item self) { return self.sku % 1000 + (uint64_t)self.qty; }
static uint64_t u§_sz_sample(u§_sample self) { return self.at + self.value; }
static uint64_t u§_enc_item(u§_item self) { return self.sku * 31 + self.unit_price; }
static u§_bx_item u§_mk_item(u§_item value) { return (u§_bx_item){ value, 1 }; }
static uint64_t u§_sz_bx_item(u§_bx_item b) { return u§_sz_item(b.value) + 1; }
static uint64_t u§_total_item(u§_item v) { return u§_sz_item(v); }
static uint64_t u§_total_bx_item(u§_bx_item v) { return u§_sz_bx_item(v); }
static uint64_t u§_total_sample(u§_sample v) { return u§_sz_sample(v); }
static uint64_t u§_encode_item(u§_item v) { return u§_enc_item(v); }

static uint64_t u§_test(void) {
  uint64_t acc = §;
%TEST_BODY%
  u§_item probe = { .sku = §, .qty = 2, .unit_price = 40, .discount = { false, 0 }, .note = { true, "x" } };
  u§_sample probe2 = { .at = 1, .value = acc % 5, .quality = 1 };
  acc = acc + u§_total_item(probe) + u§_encode_item(probe) + u§_total_bx_item(u§_mk_item(probe)) + u§_total_sample(probe2);
  acc = acc + u§_churn(acc);
  acc = acc + u§_churn(acc % 97);
  acc = acc + u§_settle(acc % 61);
  acc = acc + u§_settle(§ + 7);
  acc = acc + u§_sample_run(acc % 43);
  acc = acc + u§_sample_run(§ + 11);
  return acc;
}
"""


def c_lib():
    out = []
    for t in SCALARS:
        out.append(f"typedef struct {{ bool has; {C_TY[t]} val; }} lib_opt_{t};")
    for t in UNSIGNED:
        out.append(f"typedef struct {{ {C_TY[t]} value; uint64_t tag; }} lib_bx_{t};")
        out.append(f"typedef struct {{ lib_bx_{t} value; uint64_t tag; }} lib_bx_bx_{t};")
    out.append("typedef struct { uint64_t first; uint64_t second; } lib_duo_u64_u64;")
    for a, b in PAIRS:
        out.append(f"typedef struct {{ lib_opt_{a} first; lib_opt_{b} second; }} lib_duo_opt_{a}_opt_{b};")
        out.append(f"typedef struct {{ lib_opt_{b} first; lib_opt_{a} second; }} lib_duo_opt_{b}_opt_{a};")
    out.append("")
    out.append("static uint64_t lib_sz_u32(uint32_t self) { return (uint64_t)self + 1; }")
    out.append("static uint64_t lib_sz_u64(uint64_t self) { return self + 2; }")
    for t in SIZED:
        out.append(f"static uint64_t lib_sz_bx_{t}(lib_bx_{t} b) {{ return lib_sz_{t}(b.value) + 1; }}")
    out.append("static uint64_t lib_enc_u32(uint32_t self) { return (uint64_t)self; }")
    out.append("static uint64_t lib_enc_u64(uint64_t self) { return self; }")
    out.append("")
    for t in UNSIGNED:
        ty = C_TY[t]
        out.append(f"static lib_bx_{t} lib_mk_{t}({ty} value) {{ return (lib_bx_{t}){{ value, 1 }}; }}")
        out.append(f"static lib_bx_bx_{t} lib_mk_bx_{t}(lib_bx_{t} value) {{ return (lib_bx_bx_{t}){{ value, 1 }}; }}")
        out.append(f"static {ty} lib_get_{t}(lib_bx_{t} b) {{ return b.value; }}")
        out.append(f"static lib_bx_{t} lib_get_bx_{t}(lib_bx_bx_{t} b) {{ return b.value; }}")
        out.append(f"static lib_bx_bx_{t} lib_wrap2_{t}({ty} value) {{ return lib_mk_bx_{t}(lib_mk_{t}(value)); }}")
    out.append("static lib_duo_u64_u64 lib_mk_pair_u64_u64(uint64_t first, uint64_t second) { return (lib_duo_u64_u64){ first, second }; }")
    for a, b in PAIRS:
        out.append(
            f"static lib_duo_opt_{a}_opt_{b} lib_mk_pair_opt_{a}_opt_{b}(lib_opt_{a} first, lib_opt_{b} second) "
            f"{{ return (lib_duo_opt_{a}_opt_{b}){{ first, second }}; }}"
        )
        out.append(
            f"static lib_duo_opt_{b}_opt_{a} lib_swap_opt_{a}_opt_{b}(lib_duo_opt_{a}_opt_{b} p) "
            f"{{ return (lib_duo_opt_{b}_opt_{a}){{ p.second, p.first }}; }}"
        )
    for t in SCALARS:
        out.append(f"static lib_opt_{t} lib_zero_{t}(void) {{ return (lib_opt_{t}){{ false }}; }}")
        out.append(f"static uint64_t lib_measure_{t}(lib_opt_{t} v) {{ return v.has ? 1 : 0; }}")
    for t in SIZED:
        out.append(f"static uint64_t lib_total_bx_{t}(lib_bx_{t} v) {{ return lib_sz_bx_{t}(v); }}")
        out.append(f"static uint64_t lib_encode_{t}({C_TY[t]} v) {{ return lib_enc_{t}(v); }}")
    return "\n".join(out) + "\n"


def c_unit(i):
    body = []
    for n, t in enumerate(UNSIGNED):
        body.append(f"  acc = acc + lib_get_{t}(lib_mk_{t}({n + 1}));")
    for n, t in enumerate(SIZED):
        body.append(f"  acc = acc + lib_total_bx_{t}(lib_mk_{t}({n + 1}));")
    for n, t in enumerate(UNSIGNED):
        body.append(f"  acc = acc + lib_get_{t}(lib_get_bx_{t}(lib_wrap2_{t}({n + 1})));")
    for n, (a, b) in enumerate(PAIRS):
        body.append(
            f"  lib_duo_opt_{b}_opt_{a} p{n} = lib_swap_opt_{a}_opt_{b}(lib_mk_pair_opt_{a}_opt_{b}(lib_zero_{a}(), lib_zero_{b}()));\n"
            f"  acc = acc + lib_measure_{b}(p{n}.first) + lib_measure_{a}(p{n}.second);"
        )
    for n, t in enumerate(SIZED):
        body.append(f"  acc = acc + lib_encode_{t}({n + 1});")
    return C_UNIT.replace("%TEST_BODY%", "\n".join(body)).replace("§", str(i))


def c_program(n):
    parts = ["#include <stdbool.h>\n#include <stddef.h>\n#include <stdint.h>\n#include <stdlib.h>\n", c_lib()]
    for i in range(n):
        parts.append(c_unit(i))
    calls = "\n".join(f"  acc = acc + u{i}_test();" for i in range(n))
    parts.append(f"\nint main(void) {{\n  uint64_t acc = 0;\n{calls}\n  return acc > 0 ? 0 : 1;\n}}\n")
    return "\n".join(parts)



CPP_TY = {
    "u8": "uint8_t", "u16": "uint16_t", "u32": "uint32_t", "u64": "uint64_t",
    "bool": "bool", "string": "const char *", "f32": "float", "f64": "double",
}

CPP_LIB = r"""
namespace lib {

template <class T> struct bx { T value; uint64_t tag; };
template <class A, class B> struct duo { A first; B second; };

static uint64_t sz(uint32_t self) { return (uint64_t)self + 1; }
static uint64_t sz(uint64_t self) { return self + 2; }
template <class T> uint64_t sz(bx<T> self) { return sz(self.value) + 1; }
static uint64_t enc(uint32_t self) { return (uint64_t)self; }
static uint64_t enc(uint64_t self) { return self; }

template <class T> bx<T> mk(T value) { return bx<T>{ value, 1 }; }
template <class T> T get(bx<T> b) { return b.value; }
template <class T> bx<bx<T>> wrap2(T value) { return mk(mk(value)); }
template <class A, class B> duo<A, B> mk_pair(A first, B second) { return duo<A, B>{ first, second }; }
template <class A, class B> duo<B, A> swap(duo<A, B> p) { return duo<B, A>{ p.second, p.first }; }
template <class T> std::optional<T> zero() { return std::nullopt; }
template <class T> uint64_t measure(std::optional<T> v) { return v.has_value() ? 1 : 0; }
template <class T> uint64_t total(T v) { return sz(v); }
template <class T> uint64_t encode(T v) { return enc(v); }

}
"""

CPP_USES = ["bx", "duo", "mk", "get", "wrap2", "mk_pair", "swap", "zero", "measure", "total", "encode"]

CPP_UNIT = r"""
namespace u§ {

%USES%
struct vec2 { uint64_t x; uint64_t y; };
struct shape { enum { dot, line, rect } tag; union { uint64_t line_; vec2 rect_; }; };
struct op { enum { push, pop, reset } tag; union { uint64_t push_; }; };

struct item { uint64_t sku; uint32_t qty; uint64_t unit_price; std::optional<uint64_t> discount; std::optional<const char *> note; };
struct shipped_info { uint64_t carrier; uint64_t eta; };
struct order_status {
  enum { draft, placed, shipped, delivered, canceled } tag;
  union { uint64_t placed_; shipped_info shipped_; uint64_t delivered_; const char *canceled_; };
};
struct order { uint64_t id; order_status status; std::vector<item> lines; duo<uint64_t, uint64_t> shipping; bool paid; };
struct event {
  enum { credit, debit, hold, release, note } tag;
  union { uint64_t credit_; uint64_t debit_; duo<uint64_t, uint64_t> hold_; const char *note_; };
};
struct account { uint64_t id; uint64_t balance; uint64_t held; uint8_t tier; bool closed; std::optional<event> last_event; };
struct sample { uint64_t at; uint64_t value; uint8_t quality; };
struct series { const char *name; std::vector<sample> points; duo<uint64_t, uint64_t> window; };
struct stat { enum { count, sum, extent, empty } tag; union { uint64_t count_; uint64_t sum_; duo<uint64_t, uint64_t> extent_; }; };

static uint64_t area(shape s) {
  switch (s.tag) {
    case shape::dot: return 1;
    case shape::line: return s.line_;
    default: return s.rect_.x * s.rect_.y;
  }
}

static vec2 step(vec2 state, op o) {
  switch (o.tag) {
    case op::push: return vec2{ state.x + o.push_, state.y + 1 };
    case op::pop:
      if (state.y > 0) {
        return vec2{ state.x, state.y - 1 };
      } else {
        return state;
      }
    default: return vec2{ 0, 0 };
  }
}

static uint64_t churn(uint64_t seed) {
  uint64_t base = seed + §;
  uint64_t low = base % 7;
  uint64_t high = base / 3;
  vec2 start{ low, high };
  shape s;
  if (low > high) {
    s = shape{ .tag = shape::rect, .rect_ = start };
  } else if (low == 0) {
    s = shape{ .tag = shape::dot };
  } else {
    s = shape{ .tag = shape::line, .line_ = high };
  }
  uint64_t a = area(s);
  vec2 cur = start;
  cur = step(cur, op{ .tag = op::push, .push_ = a });
  cur = step(cur, op{ .tag = op::push, .push_ = low });
  cur = step(cur, op{ .tag = op::pop });
  cur = cur.x > 100 ? step(cur, op{ .tag = op::reset }) : cur;
  uint64_t k = 0;
  uint64_t sum = 0;
  while (k < low) {
    sum = sum + k * 2 + cur.x;
    k = k + 1;
  }
  uint64_t verdict;
  if (cur.x == 0) {
    verdict = cur.y;
  } else if (cur.x == cur.y) {
    verdict = cur.x + cur.y;
  } else {
    verdict = cur.x * 2 + cur.y;
  }
  return verdict > sum ? verdict - sum : sum - verdict + a;
}

static uint64_t line_cost(item line) {
  uint64_t gross = line.unit_price * (uint64_t)line.qty;
  uint64_t disc = line.discount.value_or(0);
  uint64_t cut = line.note.has_value() ? 1 : 0;
  return disc + cut >= gross ? 0 : gross - disc - cut;
}

static uint64_t order_total(order o) {
  uint64_t sum = 0;
  for (item line : o.lines) {
    sum = sum + line_cost(line);
  }
  uint64_t ship = o.shipping.first + o.shipping.second;
  return o.paid ? sum : sum + ship;
}

static order advance(order o, uint64_t now) {
  order_status next;
  switch (o.status.tag) {
    case order_status::draft:
      next = order_status{ .tag = order_status::placed, .placed_ = now };
      break;
    case order_status::placed:
      if (now - o.status.placed_ > 10) {
        next = order_status{ .tag = order_status::shipped, .shipped_ = { §, now + 3 } };
      } else {
        next = order_status{ .tag = order_status::placed, .placed_ = o.status.placed_ };
      }
      break;
    case order_status::shipped:
      next = now >= o.status.shipped_.eta
        ? order_status{ .tag = order_status::delivered, .delivered_ = now }
        : order_status{ .tag = order_status::shipped, .shipped_ = o.status.shipped_ };
      break;
    case order_status::delivered:
      next = order_status{ .tag = order_status::delivered, .delivered_ = o.status.delivered_ };
      break;
    default:
      next = order_status{ .tag = order_status::canceled, .canceled_ = o.status.canceled_ };
      break;
  }
  o.status = next;
  return o;
}

static account apply_event(account a, event e) {
  if (a.closed) {
    return a;
  }
  account next = a;
  switch (e.tag) {
    case event::credit:
      next.balance = a.balance + e.credit_;
      break;
    case event::debit:
      if (e.debit_ > a.balance) {
        next.closed = true;
      } else {
        next.balance = a.balance - e.debit_;
      }
      break;
    case event::hold:
      next.held = a.held + e.hold_.first + e.hold_.second;
      break;
    case event::release:
      next.balance = a.balance + a.held;
      next.held = 0;
      break;
    default:
      break;
  }
  next.last_event = e;
  return next;
}

static uint64_t settle(uint64_t seed) {
  std::vector<item> lines;
  uint64_t count = seed % 5 + 1;
  for (uint64_t k = 0; k < count; k++) {
    lines.push_back(item{
      .sku = seed * 31 + k,
      .qty = (uint32_t)(k + 1),
      .unit_price = 100 + k * 7,
      .discount = k % 2 == 0 ? std::optional<uint64_t>(k * 3) : std::nullopt,
      .note = std::nullopt
    });
  }
  order o{
    .id = seed,
    .status = { .tag = order_status::draft },
    .lines = lines,
    .shipping = mk_pair(uint64_t(5), seed % 9),
    .paid = seed % 3 == 0
  };
  uint64_t t = 0;
  while (t < 4) {
    o = advance(o, seed + t * 6);
    t = t + 1;
  }
  uint64_t due = order_total(o);
  account acct{
    .id = seed,
    .balance = due,
    .held = 0,
    .tier = (uint8_t)(seed % 4),
    .closed = false,
    .last_event = std::nullopt
  };
  acct = apply_event(acct, event{ .tag = event::hold, .hold_ = mk_pair(due / 2, uint64_t(1)) });
  acct = apply_event(acct, event{ .tag = event::debit, .debit_ = seed % 50 });
  acct = apply_event(acct, event{ .tag = event::release });
  acct = apply_event(acct, event{ .tag = event::credit, .credit_ = 3 });
  uint64_t bonus;
  if (acct.last_event.has_value() && acct.last_event->tag == event::credit) {
    bonus = acct.last_event->credit_;
  } else if (acct.last_event.has_value()) {
    bonus = 1;
  } else {
    bonus = 0;
  }
  uint64_t late = o.status.tag == order_status::delivered ? o.status.delivered_ % 13 : 0;
  return acct.balance + bonus + late;
}

static uint8_t quantize(uint64_t v) {
  if (v < 10) {
    return 0;
  } else if (v < 100) {
    return 1;
  } else if (v < 1000) {
    return 2;
  } else {
    return 3;
  }
}

static stat summarize(series s) {
  if (s.points.size() == 0) {
    return stat{ .tag = stat::empty };
  }
  uint64_t lo = s.points[0].value;
  uint64_t hi = lo;
  uint64_t total = 0;
  uint64_t kept = 0;
  for (sample p : s.points) {
    if (p.quality == 0) {
      continue;
    }
    if (p.at < s.window.first || p.at > s.window.second) {
      continue;
    }
    if (p.value < lo) {
      lo = p.value;
    }
    if (p.value > hi) {
      hi = p.value;
    }
    total = total + p.value;
    kept = kept + 1;
  }
  if (kept == 0) {
    return stat{ .tag = stat::count, .count_ = 0 };
  } else if (kept == 1) {
    return stat{ .tag = stat::sum, .sum_ = total };
  } else {
    return stat{ .tag = stat::extent, .extent_ = mk_pair(lo, hi) };
  }
}

static uint64_t sample_run(uint64_t seed) {
  std::vector<sample> pts;
  uint64_t cap = seed % 7 + 2;
  uint64_t k = 0;
  while (k < cap) {
    uint8_t q = k % 3 == 0 ? 0 : quantize(k * seed + §);
    pts.push_back(sample{ .at = k * 10, .value = (seed + k * k) % 251, .quality = q });
    k = k + 1;
  }
  series s{ .name = "run", .points = pts, .window = mk_pair(uint64_t(0), 900 + seed) };
  stat st = summarize(s);
  uint64_t verdict;
  switch (st.tag) {
    case stat::empty: verdict = 0; break;
    case stat::count: verdict = st.count_; break;
    case stat::sum: verdict = st.sum_ % 100; break;
    default: verdict = st.extent_.second - st.extent_.first; break;
  }
  return verdict + k;
}

static uint64_t sz(item self) { return self.sku % 1000 + (uint64_t)self.qty; }
static uint64_t sz(sample self) { return self.at + self.value; }
static uint64_t enc(item self) { return self.sku * 31 + self.unit_price; }

static uint64_t test() {
  uint64_t acc = §;
%TEST_BODY%
  item probe{ .sku = §, .qty = 2, .unit_price = 40, .discount = std::nullopt, .note = "x" };
  sample probe2{ .at = 1, .value = acc % 5, .quality = 1 };
  acc = acc + total(probe) + encode(probe) + total(mk(probe)) + total(probe2);
  acc = acc + churn(acc);
  acc = acc + churn(acc % 97);
  acc = acc + settle(acc % 61);
  acc = acc + settle(§ + 7);
  acc = acc + sample_run(acc % 43);
  acc = acc + sample_run(§ + 11);
  return acc;
}

}
"""


def cpp_unit(i):
    body = []
    for n, t in enumerate(UNSIGNED):
        body.append(f"  acc = acc + get(mk({CPP_TY[t]}({n + 1})));")
    for n, t in enumerate(SIZED):
        body.append(f"  acc = acc + total(mk({CPP_TY[t]}({n + 1})));")
    for n, t in enumerate(UNSIGNED):
        body.append(f"  acc = acc + get(get(wrap2({CPP_TY[t]}({n + 1}))));")
    for n, (a, b) in enumerate(PAIRS):
        body.append(
            f"  auto p{n} = swap(mk_pair(zero<{CPP_TY[a]}>(), zero<{CPP_TY[b]}>()));\n"
            f"  acc = acc + measure(p{n}.first) + measure(p{n}.second);"
        )
    for n, t in enumerate(SIZED):
        body.append(f"  acc = acc + encode({CPP_TY[t]}({n + 1}));")
    uses = "\n".join(f"using lib::{name};" for name in CPP_USES)
    return CPP_UNIT.replace("%USES%", uses).replace("%TEST_BODY%", "\n".join(body)).replace("§", str(i))


def cpp_body(n):
    parts = [CPP_LIB]
    for i in range(n):
        parts.append(cpp_unit(i))
    calls = "\n".join(f"  acc = acc + u{i}::test();" for i in range(n))
    parts.append(f"\nint main() {{\n  uint64_t acc = 0;\n{calls}\n  return acc > 0 ? 0 : 1;\n}}\n")
    return "\n".join(parts)


def cpp_program(n):
    return "#include <cstdint>\n#include <optional>\n#include <vector>\n\n" + cpp_body(n)


CPP_HAND_VEC = r"""#include <cstdint>
#include <cstdlib>

template <class T> struct vec {
  T *p = nullptr;
  size_t n = 0, cap = 0;
  size_t size() const { return n; }
  T &operator[](size_t i) { return p[i]; }
  const T &operator[](size_t i) const { return p[i]; }
  T *begin() { return p; }
  T *end() { return p + n; }
  const T *begin() const { return p; }
  const T *end() const { return p + n; }
  void push_back(T v) {
    if (n == cap) {
      cap = cap ? cap * 2 : 8;
      p = (T *)realloc(p, cap * sizeof(T));
    }
    p[n++] = v;
  }
};

struct nullopt_t {};
inline constexpr nullopt_t nullopt{};

template <class T> struct opt {
  T v{};
  bool has = false;
  opt() = default;
  opt(nullopt_t) {}
  opt(T x) : v(x), has(true) {}
  bool has_value() const { return has; }
  T value() const { return v; }
  const T &operator*() const { return v; }
  const T *operator->() const { return &v; }
  T value_or(T d) const { return has ? v : d; }
};

"""


def cpp_hand_vec_program(n):
    body = cpp_body(n).replace("std::vector<", "vec<").replace("std::optional<", "opt<").replace("std::nullopt", "nullopt")
    return CPP_HAND_VEC + body



RS_TY = {
    "u8": "u8", "u16": "u16", "u32": "u32", "u64": "u64",
    "bool": "bool", "string": "&'static str", "f32": "f32", "f64": "f64",
}

RS_LIB = r"""
mod lib {
    pub struct Bx<T> { pub value: T, pub tag: u64 }
    #[derive(Clone, Copy)]
    pub struct Duo<A, B> { pub first: A, pub second: B }

    pub trait Sz { fn sz(self) -> u64; }
    pub trait Enc<O> { fn enc(self) -> O; }

    impl Sz for u32 { fn sz(self) -> u64 { self as u64 + 1 } }
    impl Sz for u64 { fn sz(self) -> u64 { self + 2 } }
    impl<T: Sz> Sz for Bx<T> { fn sz(self) -> u64 { self.value.sz() + 1 } }
    impl Enc<u64> for u32 { fn enc(self) -> u64 { self as u64 } }
    impl Enc<u64> for u64 { fn enc(self) -> u64 { self } }

    pub fn mk<T>(value: T) -> Bx<T> { Bx { value, tag: 1 } }
    pub fn get<T>(b: Bx<T>) -> T { b.value }
    pub fn wrap2<T>(value: T) -> Bx<Bx<T>> { mk(mk(value)) }
    pub fn mk_pair<A, B>(first: A, second: B) -> Duo<A, B> { Duo { first, second } }
    pub fn swap<A, B>(p: Duo<A, B>) -> Duo<B, A> { Duo { first: p.second, second: p.first } }
    pub fn zero<T>() -> Option<T> { None }
    pub fn measure<T>(v: Option<T>) -> u64 { if v.is_some() { 1 } else { 0 } }
    pub fn total<T: Sz>(v: T) -> u64 { v.sz() }
    pub fn encode<T: Enc<u64>>(v: T) -> u64 { v.enc() }
}
"""

RS_UNIT = r"""
mod u§ {
    use super::lib;
    use super::lib::{Duo, mk, get, wrap2, mk_pair, swap, zero, measure, total, encode};
    #[derive(Clone, Copy)]
    struct Vec2 { x: u64, y: u64 }
    enum Shape { Dot, Line(u64), Rect(Vec2) }
    enum Op { Push(u64), Pop, Reset }

    #[derive(Clone, Copy)]
    struct Item { sku: u64, qty: u32, unit_price: u64, discount: Option<u64>, note: Option<&'static str> }
    #[derive(Clone, Copy)]
    struct ShippedInfo { carrier: u64, eta: u64 }
    enum OrderStatus { Draft, Placed(u64), Shipped(ShippedInfo), Delivered(u64), Canceled(&'static str) }
    struct Order { id: u64, status: OrderStatus, lines: Vec<Item>, shipping: Duo<u64, u64>, paid: bool }
    enum Event { Credit(u64), Debit(u64), Hold(Duo<u64, u64>), Release, Note(&'static str) }
    struct Account { id: u64, balance: u64, held: u64, tier: u8, closed: bool, last_event: Option<Event> }
    struct Sample { at: u64, value: u64, quality: u8 }
    struct Series { name: &'static str, points: Vec<Sample>, window: Duo<u64, u64> }
    enum Stat { Count(u64), Sum(u64), Extent(Duo<u64, u64>), Empty }

    fn area(s: Shape) -> u64 {
        match s {
            Shape::Dot => 1,
            Shape::Line(len) => len,
            Shape::Rect(d) => d.x * d.y,
        }
    }

    fn step(state: Vec2, o: Op) -> Vec2 {
        match o {
            Op::Push(n) => Vec2 { x: state.x + n, y: state.y + 1 },
            Op::Pop => {
                if state.y > 0 {
                    Vec2 { x: state.x, y: state.y - 1 }
                } else {
                    state
                }
            }
            Op::Reset => Vec2 { x: 0, y: 0 },
        }
    }

    fn churn(seed: u64) -> u64 {
        let base = seed + §;
        let low = base % 7;
        let high = base / 3;
        let start = Vec2 { x: low, y: high };
        let s: Shape = if low > high { Shape::Rect(start) } else if low == 0 { Shape::Dot } else { Shape::Line(high) };
        let a = area(s);
        let mut cur = start;
        cur = step(cur, Op::Push(a));
        cur = step(cur, Op::Push(low));
        cur = step(cur, Op::Pop);
        cur = if cur.x > 100 { step(cur, Op::Reset) } else { cur };
        let mut k: u64 = 0;
        let mut sum: u64 = 0;
        while k < low {
            sum = sum + k * 2 + cur.x;
            k = k + 1;
        }
        let verdict = match cur {
            Vec2 { x: 0, y } => y,
            Vec2 { x, y } if x == y => x + y,
            Vec2 { x, y } => x * 2 + y,
        };
        if verdict > sum { verdict - sum } else { sum - verdict + a }
    }

    fn line_cost(line: &Item) -> u64 {
        let gross = line.unit_price * line.qty as u64;
        let disc = line.discount.unwrap_or(0);
        let cut = if line.note.is_some() { 1 } else { 0 };
        if disc + cut >= gross { 0 } else { gross - disc - cut }
    }

    fn order_total(o: &Order) -> u64 {
        let mut sum: u64 = 0;
        for line in &o.lines {
            sum = sum + line_cost(line);
        }
        let ship = o.shipping.first + o.shipping.second;
        if o.paid { sum } else { sum + ship }
    }

    fn advance(mut o: Order, now: u64) -> Order {
        let next: OrderStatus = match o.status {
            OrderStatus::Draft => OrderStatus::Placed(now),
            OrderStatus::Placed(at) => {
                if now - at > 10 {
                    OrderStatus::Shipped(ShippedInfo { carrier: §, eta: now + 3 })
                } else {
                    OrderStatus::Placed(at)
                }
            }
            OrderStatus::Shipped(s) => if now >= s.eta { OrderStatus::Delivered(now) } else { OrderStatus::Shipped(s) },
            OrderStatus::Delivered(at) => OrderStatus::Delivered(at),
            OrderStatus::Canceled(why) => OrderStatus::Canceled(why),
        };
        o.status = next;
        o
    }

    fn apply_event(a: Account, e: Event) -> Account {
        if a.closed {
            return a;
        }
        let next = match e {
            Event::Credit(n) => Account { balance: a.balance + n, ..a },
            Event::Debit(n) => {
                if n > a.balance {
                    Account { closed: true, ..a }
                } else {
                    Account { balance: a.balance - n, ..a }
                }
            }
            Event::Hold(h) => Account { held: a.held + h.first + h.second, ..a },
            Event::Release => Account { balance: a.balance + a.held, held: 0, ..a },
            Event::Note(_) => a,
        };
        Account { last_event: Some(e), ..next }
    }

    fn settle(seed: u64) -> u64 {
        let mut lines: Vec<Item> = Vec::new();
        let count = seed % 5 + 1;
        for k in 0..count {
            lines.push(Item {
                sku: seed * 31 + k,
                qty: (k + 1) as u32,
                unit_price: 100 + k * 7,
                discount: if k % 2 == 0 { Some(k * 3) } else { None },
                note: None,
            });
        }
        let mut o = Order {
            id: seed,
            status: OrderStatus::Draft,
            lines,
            shipping: mk_pair(5u64, seed % 9),
            paid: seed % 3 == 0,
        };
        let mut t: u64 = 0;
        while t < 4 {
            o = advance(o, seed + t * 6);
            t = t + 1;
        }
        let due = order_total(&o);
        let mut acct = Account {
            id: seed,
            balance: due,
            held: 0,
            tier: (seed % 4) as u8,
            closed: false,
            last_event: None,
        };
        acct = apply_event(acct, Event::Hold(mk_pair(due / 2, 1u64)));
        acct = apply_event(acct, Event::Debit(seed % 50));
        acct = apply_event(acct, Event::Release);
        acct = apply_event(acct, Event::Credit(3));
        let bonus = match acct.last_event {
            Some(Event::Credit(n)) => n,
            Some(_) => 1,
            None => 0,
        };
        let late = match o.status { OrderStatus::Delivered(at) => at % 13, _ => 0 };
        acct.balance + bonus + late
    }

    fn quantize(v: u64) -> u8 {
        if v < 10 { 0u8 }
        else if v < 100 { 1 }
        else if v < 1000 { 2 }
        else { 3 }
    }

    fn summarize(s: &Series) -> Stat {
        if s.points.len() == 0 {
            return Stat::Empty;
        }
        let mut lo = s.points[0].value;
        let mut hi = lo;
        let mut total: u64 = 0;
        let mut kept: u64 = 0;
        for p in &s.points {
            if p.quality == 0 {
                continue;
            }
            if p.at < s.window.first || p.at > s.window.second {
                continue;
            }
            if p.value < lo {
                lo = p.value;
            }
            if p.value > hi {
                hi = p.value;
            }
            total = total + p.value;
            kept = kept + 1;
        }
        if kept == 0 { Stat::Count(0) }
        else if kept == 1 { Stat::Sum(total) }
        else { Stat::Extent(mk_pair(lo, hi)) }
    }

    fn sample_run(seed: u64) -> u64 {
        let mut pts: Vec<Sample> = Vec::new();
        let cap = seed % 7 + 2;
        let mut k: u64 = 0;
        while k < cap {
            let q = if k % 3 == 0 { 0u8 } else { quantize(k * seed + §) };
            pts.push(Sample { at: k * 10, value: (seed + k * k) % 251, quality: q });
            k = k + 1;
        }
        let s = Series { name: "run", points: pts, window: mk_pair(0u64, 900 + seed) };
        let verdict = match summarize(&s) {
            Stat::Empty => 0u64,
            Stat::Count(n) => n,
            Stat::Sum(n) => n % 100,
            Stat::Extent(e) => e.second - e.first,
        };
        verdict + k
    }

    impl lib::Sz for Item { fn sz(self) -> u64 { self.sku % 1000 + self.qty as u64 } }
    impl lib::Sz for Sample { fn sz(self) -> u64 { self.at + self.value } }
    impl lib::Enc<u64> for Item { fn enc(self) -> u64 { self.sku * 31 + self.unit_price } }

    pub fn test() -> u64 {
        let mut acc: u64 = §;
%TEST_BODY%
        let probe = Item { sku: §, qty: 2, unit_price: 40, discount: None, note: Some("x") };
        let probe2 = Sample { at: 1, value: acc % 5, quality: 1 };
        acc = acc + total(probe) + encode(probe) + total(mk(probe)) + total(probe2);
        acc = acc + churn(acc);
        acc = acc + churn(acc % 97);
        acc = acc + settle(acc % 61);
        acc = acc + settle(§ + 7);
        acc = acc + sample_run(acc % 43);
        acc = acc + sample_run(§ + 11);
        acc
    }
}
"""


def rs_unit(i):
    body = []
    for n, t in enumerate(UNSIGNED):
        body.append(f"        acc = acc + get(mk({n + 1}{t})) as u64;")
    for n, t in enumerate(SIZED):
        body.append(f"        acc = acc + total(mk({n + 1}{t}));")
    for n, t in enumerate(UNSIGNED):
        body.append(f"        acc = acc + get(get(wrap2({n + 1}{t}))) as u64;")
    for n, (a, b) in enumerate(PAIRS):
        body.append(
            f"        let p{n} = swap(mk_pair(zero::<{RS_TY[a]}>(), zero::<{RS_TY[b]}>()));\n"
            f"        acc = acc + measure(p{n}.first) + measure(p{n}.second);"
        )
    for n, t in enumerate(SIZED):
        body.append(f"        acc = acc + encode({n + 1}{t});")
    return RS_UNIT.replace("%TEST_BODY%", "\n".join(body)).replace("§", str(i))


def rs_program(n):
    parts = ["#![allow(dead_code)]\n", RS_LIB]
    for i in range(n):
        parts.append(rs_unit(i))
    calls = "\n".join(f"    acc = acc + u{i}::test();" for i in range(n))
    parts.append(
        f"\nfn main() {{\n    let mut acc: u64 = 0;\n{calls}\n"
        "    std::process::exit(if acc > 0 { 0 } else { 1 });\n}\n"
    )
    return "\n".join(parts)



GO_TY = {
    "u8": "uint8", "u16": "uint16", "u32": "uint32", "u64": "uint64",
    "bool": "bool", "string": "string", "f32": "float32", "f64": "float64",
}

GO_LIB = r"""
type bx[T any] struct {
	value T
	tag   uint64
}
type duo[A, B any] struct {
	first  A
	second B
}
type opt[T any] struct {
	has bool
	val T
}

type sz interface{ sz() uint64 }
type enc[O any] interface{ enc() O }

type u32 uint32
type u64 uint64

func (x u32) sz() uint64  { return uint64(x) + 1 }
func (x u64) sz() uint64  { return uint64(x) + 2 }
func (x u32) enc() uint64 { return uint64(x) }
func (x u64) enc() uint64 { return uint64(x) }

func szBx[T sz](b bx[T]) uint64 { return b.value.sz() + 1 }

func mk[T any](value T) bx[T]                      { return bx[T]{value, 1} }
func get[T any](b bx[T]) T                         { return b.value }
func wrap2[T any](value T) bx[bx[T]]               { return mk(mk(value)) }
func mkPair[A, B any](first A, second B) duo[A, B] { return duo[A, B]{first, second} }
func swap[A, B any](p duo[A, B]) duo[B, A]         { return duo[B, A]{p.second, p.first} }
func zero[T any]() opt[T]                          { return opt[T]{} }
func measure[T any](v opt[T]) uint64 {
	if v.has {
		return 1
	}
	return 0
}
func total[T sz](v T) uint64           { return v.sz() }
func encode[T enc[uint64]](v T) uint64 { return v.enc() }
"""

GO_UNIT = r"""
type vec2_§ struct{ x, y uint64 }
type shape_§ struct {
	tag  int
	line uint64
	rect vec2_§
}

const (
	shapeDot_§ = iota
	shapeLine_§
	shapeRect_§
)

type op_§ struct {
	tag  int
	push uint64
}

const (
	opPush_§ = iota
	opPop_§
	opReset_§
)

type item_§ struct {
	sku       uint64
	qty       uint32
	unitPrice uint64
	discount  opt[uint64]
	note      opt[string]
}
type shippedInfo_§ struct{ carrier, eta uint64 }
type orderStatus_§ struct {
	tag       int
	placed    uint64
	shipped   shippedInfo_§
	delivered uint64
	canceled  string
}

const (
	statusDraft_§ = iota
	statusPlaced_§
	statusShipped_§
	statusDelivered_§
	statusCanceled_§
)

type order_§ struct {
	id       uint64
	status   orderStatus_§
	lines    []item_§
	shipping duo[uint64, uint64]
	paid     bool
}
type event_§ struct {
	tag    int
	credit uint64
	debit  uint64
	hold   duo[uint64, uint64]
	note   string
}

const (
	eventCredit_§ = iota
	eventDebit_§
	eventHold_§
	eventRelease_§
	eventNote_§
)

type account_§ struct {
	id        uint64
	balance   uint64
	held      uint64
	tier      uint8
	closed    bool
	lastEvent opt[event_§]
}
type sample_§ struct {
	at      uint64
	value   uint64
	quality uint8
}
type series_§ struct {
	name   string
	points []sample_§
	window duo[uint64, uint64]
}
type stat_§ struct {
	tag    int
	count  uint64
	sum    uint64
	extent duo[uint64, uint64]
}

const (
	statCount_§ = iota
	statSum_§
	statExtent_§
	statEmpty_§
)

func area_§(s shape_§) uint64 {
	switch s.tag {
	case shapeDot_§:
		return 1
	case shapeLine_§:
		return s.line
	default:
		return s.rect.x * s.rect.y
	}
}

func step_§(state vec2_§, o op_§) vec2_§ {
	switch o.tag {
	case opPush_§:
		return vec2_§{state.x + o.push, state.y + 1}
	case opPop_§:
		if state.y > 0 {
			return vec2_§{state.x, state.y - 1}
		} else {
			return state
		}
	default:
		return vec2_§{0, 0}
	}
}

func churn_§(seed uint64) uint64 {
	base := seed + §
	low := base % 7
	high := base / 3
	start := vec2_§{low, high}
	var s shape_§
	if low > high {
		s = shape_§{tag: shapeRect_§, rect: start}
	} else if low == 0 {
		s = shape_§{tag: shapeDot_§}
	} else {
		s = shape_§{tag: shapeLine_§, line: high}
	}
	a := area_§(s)
	cur := start
	cur = step_§(cur, op_§{tag: opPush_§, push: a})
	cur = step_§(cur, op_§{tag: opPush_§, push: low})
	cur = step_§(cur, op_§{tag: opPop_§})
	if cur.x > 100 {
		cur = step_§(cur, op_§{tag: opReset_§})
	}
	k := uint64(0)
	sum := uint64(0)
	for k < low {
		sum = sum + k*2 + cur.x
		k = k + 1
	}
	var verdict uint64
	if cur.x == 0 {
		verdict = cur.y
	} else if cur.x == cur.y {
		verdict = cur.x + cur.y
	} else {
		verdict = cur.x*2 + cur.y
	}
	if verdict > sum {
		return verdict - sum
	}
	return sum - verdict + a
}

func lineCost_§(line item_§) uint64 {
	gross := line.unitPrice * uint64(line.qty)
	disc := uint64(0)
	if line.discount.has {
		disc = line.discount.val
	}
	cut := uint64(0)
	if line.note.has {
		cut = 1
	}
	if disc+cut >= gross {
		return 0
	}
	return gross - disc - cut
}

func orderTotal_§(o order_§) uint64 {
	sum := uint64(0)
	for _, line := range o.lines {
		sum = sum + lineCost_§(line)
	}
	ship := o.shipping.first + o.shipping.second
	if o.paid {
		return sum
	}
	return sum + ship
}

func advance_§(o order_§, now uint64) order_§ {
	var next orderStatus_§
	switch o.status.tag {
	case statusDraft_§:
		next = orderStatus_§{tag: statusPlaced_§, placed: now}
	case statusPlaced_§:
		if now-o.status.placed > 10 {
			next = orderStatus_§{tag: statusShipped_§, shipped: shippedInfo_§{§, now + 3}}
		} else {
			next = orderStatus_§{tag: statusPlaced_§, placed: o.status.placed}
		}
	case statusShipped_§:
		if now >= o.status.shipped.eta {
			next = orderStatus_§{tag: statusDelivered_§, delivered: now}
		} else {
			next = orderStatus_§{tag: statusShipped_§, shipped: o.status.shipped}
		}
	case statusDelivered_§:
		next = orderStatus_§{tag: statusDelivered_§, delivered: o.status.delivered}
	default:
		next = orderStatus_§{tag: statusCanceled_§, canceled: o.status.canceled}
	}
	o.status = next
	return o
}

func applyEvent_§(a account_§, e event_§) account_§ {
	if a.closed {
		return a
	}
	next := a
	switch e.tag {
	case eventCredit_§:
		next.balance = a.balance + e.credit
	case eventDebit_§:
		if e.debit > a.balance {
			next.closed = true
		} else {
			next.balance = a.balance - e.debit
		}
	case eventHold_§:
		next.held = a.held + e.hold.first + e.hold.second
	case eventRelease_§:
		next.balance = a.balance + a.held
		next.held = 0
	default:
	}
	next.lastEvent = opt[event_§]{true, e}
	return next
}

func settle_§(seed uint64) uint64 {
	var lines []item_§
	count := seed%5 + 1
	for k := uint64(0); k < count; k++ {
		var discount opt[uint64]
		if k%2 == 0 {
			discount = opt[uint64]{true, k * 3}
		}
		lines = append(lines, item_§{
			sku:       seed*31 + k,
			qty:       uint32(k + 1),
			unitPrice: 100 + k*7,
			discount:  discount,
			note:      opt[string]{},
		})
	}
	o := order_§{
		id:       seed,
		status:   orderStatus_§{tag: statusDraft_§},
		lines:    lines,
		shipping: mkPair(uint64(5), seed%9),
		paid:     seed%3 == 0,
	}
	t := uint64(0)
	for t < 4 {
		o = advance_§(o, seed+t*6)
		t = t + 1
	}
	due := orderTotal_§(o)
	acct := account_§{
		id:        seed,
		balance:   due,
		held:      0,
		tier:      uint8(seed % 4),
		closed:    false,
		lastEvent: opt[event_§]{},
	}
	acct = applyEvent_§(acct, event_§{tag: eventHold_§, hold: mkPair(due/2, uint64(1))})
	acct = applyEvent_§(acct, event_§{tag: eventDebit_§, debit: seed % 50})
	acct = applyEvent_§(acct, event_§{tag: eventRelease_§})
	acct = applyEvent_§(acct, event_§{tag: eventCredit_§, credit: 3})
	var bonus uint64
	if acct.lastEvent.has && acct.lastEvent.val.tag == eventCredit_§ {
		bonus = acct.lastEvent.val.credit
	} else if acct.lastEvent.has {
		bonus = 1
	} else {
		bonus = 0
	}
	late := uint64(0)
	if o.status.tag == statusDelivered_§ {
		late = o.status.delivered % 13
	}
	return acct.balance + bonus + late
}

func quantize_§(v uint64) uint8 {
	if v < 10 {
		return 0
	} else if v < 100 {
		return 1
	} else if v < 1000 {
		return 2
	} else {
		return 3
	}
}

func summarize_§(s series_§) stat_§ {
	if len(s.points) == 0 {
		return stat_§{tag: statEmpty_§}
	}
	lo := s.points[0].value
	hi := lo
	total := uint64(0)
	kept := uint64(0)
	for _, p := range s.points {
		if p.quality == 0 {
			continue
		}
		if p.at < s.window.first || p.at > s.window.second {
			continue
		}
		if p.value < lo {
			lo = p.value
		}
		if p.value > hi {
			hi = p.value
		}
		total = total + p.value
		kept = kept + 1
	}
	if kept == 0 {
		return stat_§{tag: statCount_§, count: 0}
	} else if kept == 1 {
		return stat_§{tag: statSum_§, sum: total}
	} else {
		return stat_§{tag: statExtent_§, extent: mkPair(lo, hi)}
	}
}

func sampleRun_§(seed uint64) uint64 {
	var pts []sample_§
	cap := seed%7 + 2
	k := uint64(0)
	for k < cap {
		q := uint8(0)
		if k%3 != 0 {
			q = quantize_§(k*seed + §)
		}
		pts = append(pts, sample_§{at: k * 10, value: (seed + k*k) % 251, quality: q})
		k = k + 1
	}
	s := series_§{name: "run", points: pts, window: mkPair(uint64(0), 900+seed)}
	st := summarize_§(s)
	var verdict uint64
	switch st.tag {
	case statEmpty_§:
		verdict = 0
	case statCount_§:
		verdict = st.count
	case statSum_§:
		verdict = st.sum % 100
	default:
		verdict = st.extent.second - st.extent.first
	}
	return verdict + k
}

func (x item_§) sz() uint64   { return x.sku%1000 + uint64(x.qty) }
func (x sample_§) sz() uint64 { return x.at + x.value }
func (x item_§) enc() uint64  { return x.sku*31 + x.unitPrice }

func test_§() uint64 {
	acc := uint64(§)
%TEST_BODY%
	probe := item_§{sku: §, qty: 2, unitPrice: 40, discount: opt[uint64]{}, note: opt[string]{true, "x"}}
	probe2 := sample_§{at: 1, value: acc % 5, quality: 1}
	acc = acc + total(probe) + encode(probe) + szBx(mk(probe)) + total(probe2)
	acc = acc + churn_§(acc)
	acc = acc + churn_§(acc%97)
	acc = acc + settle_§(acc%61)
	acc = acc + settle_§(§+7)
	acc = acc + sampleRun_§(acc%43)
	acc = acc + sampleRun_§(§+11)
	return acc
}
"""


def go_unit(i):
    body = []
    for n, t in enumerate(UNSIGNED):
        body.append(f"\tacc = acc + uint64(get(mk({GO_TY[t]}({n + 1}))))")
    for n, t in enumerate(SIZED):
        body.append(f"\tacc = acc + szBx(mk({t}({n + 1})))")
    for n, t in enumerate(UNSIGNED):
        body.append(f"\tacc = acc + uint64(get(get(wrap2({GO_TY[t]}({n + 1})))))")
    for n, (a, b) in enumerate(PAIRS):
        body.append(
            f"\tp{n} := swap(mkPair(zero[{GO_TY[a]}](), zero[{GO_TY[b]}]()))\n"
            f"\tacc = acc + measure(p{n}.first) + measure(p{n}.second)"
        )
    for n, t in enumerate(SIZED):
        body.append(f"\tacc = acc + encode({t}({n + 1}))")
    return GO_UNIT.replace("%TEST_BODY%", "\n".join(body)).replace("§", str(i))


def go_program(n):
    parts = ["package main\n\nimport \"os\"\n\n// nonce 0\n", GO_LIB]
    for i in range(n):
        parts.append(go_unit(i))
    calls = "\n".join(f"\tacc = acc + test_{i}()" for i in range(n))
    parts.append(f"\nfunc main() {{\n\tacc := uint64(0)\n{calls}\n\tif acc == 0 {{\n\t\tos.Exit(1)\n\t}}\n}}\n")
    return "\n".join(parts)



ZIG_TY = {
    "u8": "u8", "u16": "u16", "u32": "u32", "u64": "u64",
    "bool": "bool", "string": "[]const u8", "f32": "f32", "f64": "f64",
}

ZIG_LIB = r"""
const lib = struct {
    fn Bx(comptime T: type) type {
        return struct { value: T, tag: u64 };
    }
    fn Duo(comptime A: type, comptime B: type) type {
        return struct { first: A, second: B };
    }

    fn sz(v: anytype) u64 {
        const T = @TypeOf(v);
        return switch (T) {
            u32 => @as(u64, v) + 1,
            u64 => v + 2,
            else => if (@hasDecl(T, "sz")) v.sz() else sz(v.value) + 1,
        };
    }
    fn enc(v: anytype) u64 {
        return switch (@TypeOf(v)) {
            u32 => @as(u64, v),
            u64 => v,
            else => v.enc(),
        };
    }

    fn mk(value: anytype) Bx(@TypeOf(value)) {
        return .{ .value = value, .tag = 1 };
    }
    fn get(b: anytype) @TypeOf(b.value) {
        return b.value;
    }
    fn wrap2(value: anytype) Bx(Bx(@TypeOf(value))) {
        return mk(mk(value));
    }
    fn mkPair(first: anytype, second: anytype) Duo(@TypeOf(first), @TypeOf(second)) {
        return .{ .first = first, .second = second };
    }
    fn swap(p: anytype) Duo(@TypeOf(p.second), @TypeOf(p.first)) {
        return .{ .first = p.second, .second = p.first };
    }
    fn zero(comptime T: type) ?T {
        return null;
    }
    fn measure(v: anytype) u64 {
        return if (v != null) 1 else 0;
    }
    fn total(v: anytype) u64 {
        return sz(v);
    }
    fn encode(v: anytype) u64 {
        return enc(v);
    }
};
"""

ZIG_USES = ["Bx", "Duo", "mk", "get", "wrap2", "mkPair", "swap", "zero", "measure", "total", "encode"]

ZIG_UNIT = r"""
const unit§ = struct {
%USES%
    const Vec2 = struct { x: u64, y: u64 };
    const Shape = union(enum) { dot, line: u64, rect: Vec2 };
    const Op = union(enum) { push: u64, pop, reset };

    const Item = struct {
        sku: u64,
        qty: u32,
        unit_price: u64,
        discount: ?u64,
        note: ?[]const u8,
        pub fn sz(self: Item) u64 {
            return self.sku % 1000 + self.qty;
        }
        pub fn enc(self: Item) u64 {
            return self.sku * 31 + self.unit_price;
        }
    };
    const OrderStatus = union(enum) {
        draft,
        placed: u64,
        shipped: struct { carrier: u64, eta: u64 },
        delivered: u64,
        canceled: []const u8,
    };
    const Order = struct { id: u64, status: OrderStatus, lines: std.ArrayList(Item), shipping: Duo(u64, u64), paid: bool };
    const Event = union(enum) { credit: u64, debit: u64, hold: Duo(u64, u64), release, note: []const u8 };
    const Account = struct { id: u64, balance: u64, held: u64, tier: u8, closed: bool, last_event: ?Event };
    const Sample = struct {
        at: u64,
        value: u64,
        quality: u8,
        pub fn sz(self: Sample) u64 {
            return self.at + self.value;
        }
    };
    const Series = struct { name: []const u8, points: std.ArrayList(Sample), window: Duo(u64, u64) };
    const Stat = union(enum) { count: u64, sum: u64, extent: Duo(u64, u64), empty };

    fn area(s: Shape) u64 {
        return switch (s) {
            .dot => 1,
            .line => |len| len,
            .rect => |d| d.x * d.y,
        };
    }

    fn step(state: Vec2, o: Op) Vec2 {
        return switch (o) {
            .push => |n| Vec2{ .x = state.x + n, .y = state.y + 1 },
            .pop => if (state.y > 0) Vec2{ .x = state.x, .y = state.y - 1 } else state,
            .reset => Vec2{ .x = 0, .y = 0 },
        };
    }

    fn churn(seed: u64) u64 {
        const base = seed + §;
        const low = base % 7;
        const high = base / 3;
        const start = Vec2{ .x = low, .y = high };
        const s: Shape = if (low > high) Shape{ .rect = start } else if (low == 0) Shape.dot else Shape{ .line = high };
        const a = area(s);
        var cur = start;
        cur = step(cur, .{ .push = a });
        cur = step(cur, .{ .push = low });
        cur = step(cur, .pop);
        cur = if (cur.x > 100) step(cur, .reset) else cur;
        var k: u64 = 0;
        var sum: u64 = 0;
        while (k < low) {
            sum = sum + k * 2 + cur.x;
            k = k + 1;
        }
        const verdict = if (cur.x == 0) cur.y else if (cur.x == cur.y) cur.x + cur.y else cur.x * 2 + cur.y;
        return if (verdict > sum) verdict - sum else sum - verdict + a;
    }

    fn lineCost(line: Item) u64 {
        const gross = line.unit_price * line.qty;
        const disc = line.discount orelse 0;
        const cut: u64 = if (line.note != null) 1 else 0;
        return if (disc + cut >= gross) 0 else gross - disc - cut;
    }

    fn orderTotal(o: Order) u64 {
        var sum: u64 = 0;
        for (o.lines.items) |line| {
            sum = sum + lineCost(line);
        }
        const ship = o.shipping.first + o.shipping.second;
        return if (o.paid) sum else sum + ship;
    }

    fn advance(o: Order, now: u64) Order {
        const next: OrderStatus = switch (o.status) {
            .draft => .{ .placed = now },
            .placed => |at| if (now - at > 10) OrderStatus{ .shipped = .{ .carrier = §, .eta = now + 3 } } else OrderStatus{ .placed = at },
            .shipped => |s| if (now >= s.eta) OrderStatus{ .delivered = now } else OrderStatus{ .shipped = s },
            .delivered => |at| .{ .delivered = at },
            .canceled => |why| .{ .canceled = why },
        };
        var out = o;
        out.status = next;
        return out;
    }

    fn applyEvent(a: Account, e: Event) Account {
        if (a.closed) {
            return a;
        }
        var next = a;
        switch (e) {
            .credit => |n| next.balance = a.balance + n,
            .debit => |n| {
                if (n > a.balance) {
                    next.closed = true;
                } else {
                    next.balance = a.balance - n;
                }
            },
            .hold => |h| next.held = a.held + h.first + h.second,
            .release => {
                next.balance = a.balance + a.held;
                next.held = 0;
            },
            .note => {},
        }
        next.last_event = e;
        return next;
    }

    fn settle(seed: u64) !u64 {
        var lines: std.ArrayList(Item) = .empty;
        const count = seed % 5 + 1;
        for (0..count) |k| {
            try lines.append(alloc, .{
                .sku = seed * 31 + k,
                .qty = @truncate(k + 1),
                .unit_price = 100 + k * 7,
                .discount = if (k % 2 == 0) k * 3 else null,
                .note = null,
            });
        }
        var o = Order{
            .id = seed,
            .status = .draft,
            .lines = lines,
            .shipping = mkPair(@as(u64, 5), seed % 9),
            .paid = seed % 3 == 0,
        };
        var t: u64 = 0;
        while (t < 4) {
            o = advance(o, seed + t * 6);
            t = t + 1;
        }
        const due = orderTotal(o);
        var acct = Account{
            .id = seed,
            .balance = due,
            .held = 0,
            .tier = @truncate(seed % 4),
            .closed = false,
            .last_event = null,
        };
        acct = applyEvent(acct, .{ .hold = mkPair(due / 2, @as(u64, 1)) });
        acct = applyEvent(acct, .{ .debit = seed % 50 });
        acct = applyEvent(acct, .release);
        acct = applyEvent(acct, .{ .credit = 3 });
        const bonus: u64 = if (acct.last_event) |ev| switch (ev) {
            .credit => |n| n,
            else => 1,
        } else 0;
        const late: u64 = switch (o.status) {
            .delivered => |at| at % 13,
            else => 0,
        };
        return acct.balance + bonus + late;
    }

    fn quantize(v: u64) u8 {
        return if (v < 10) 0 else if (v < 100) 1 else if (v < 1000) 2 else 3;
    }

    fn summarize(s: Series) Stat {
        if (s.points.items.len == 0) {
            return .empty;
        }
        var lo = s.points.items[0].value;
        var hi = lo;
        var tally: u64 = 0;
        var kept: u64 = 0;
        for (s.points.items) |p| {
            if (p.quality == 0) {
                continue;
            }
            if (p.at < s.window.first or p.at > s.window.second) {
                continue;
            }
            if (p.value < lo) {
                lo = p.value;
            }
            if (p.value > hi) {
                hi = p.value;
            }
            tally = tally + p.value;
            kept = kept + 1;
        }
        return if (kept == 0) Stat{ .count = 0 } else if (kept == 1) Stat{ .sum = tally } else Stat{ .extent = mkPair(lo, hi) };
    }

    fn sampleRun(seed: u64) !u64 {
        var pts: std.ArrayList(Sample) = .empty;
        const cap = seed % 7 + 2;
        var k: u64 = 0;
        while (k < cap) {
            const q: u8 = if (k % 3 == 0) 0 else quantize(k * seed + §);
            try pts.append(alloc, .{ .at = k * 10, .value = (seed + k * k) % 251, .quality = q });
            k = k + 1;
        }
        const s = Series{ .name = "run", .points = pts, .window = mkPair(@as(u64, 0), 900 + seed) };
        const verdict: u64 = switch (summarize(s)) {
            .empty => 0,
            .count => |n| n,
            .sum => |n| n % 100,
            .extent => |e| e.second - e.first,
        };
        return verdict + k;
    }

    fn run() !u64 {
        var acc: u64 = §;
%TEST_BODY%
        const probe = Item{ .sku = §, .qty = 2, .unit_price = 40, .discount = null, .note = "x" };
        const probe2 = Sample{ .at = 1, .value = acc % 5, .quality = 1 };
        acc = acc + total(probe) + encode(probe) + total(mk(probe)) + total(probe2);
        acc = acc + churn(acc);
        acc = acc + churn(acc % 97);
        acc = acc + try settle(acc % 61);
        acc = acc + try settle(§ + 7);
        acc = acc + try sampleRun(acc % 43);
        acc = acc + try sampleRun(§ + 11);
        return acc;
    }
};
"""


def zig_unit(i):
    body = []
    for n, t in enumerate(UNSIGNED):
        body.append(f"        acc = acc + get(mk(@as({t}, {n + 1})));")
    for n, t in enumerate(SIZED):
        body.append(f"        acc = acc + total(mk(@as({t}, {n + 1})));")
    for n, t in enumerate(UNSIGNED):
        body.append(f"        acc = acc + get(get(wrap2(@as({t}, {n + 1}))));")
    for n, (a, b) in enumerate(PAIRS):
        body.append(
            f"        const p{n} = swap(mkPair(zero({ZIG_TY[a]}), zero({ZIG_TY[b]})));\n"
            f"        acc = acc + measure(p{n}.first) + measure(p{n}.second);"
        )
    for n, t in enumerate(SIZED):
        body.append(f"        acc = acc + encode(@as({t}, {n + 1}));")
    uses = "\n".join(f"    const {name} = lib.{name};" for name in ZIG_USES)
    return ZIG_UNIT.replace("%USES%", uses).replace("%TEST_BODY%", "\n".join(body)).replace("§", str(i))


def zig_program(n):
    parts = ['const std = @import("std");\nconst alloc = std.heap.page_allocator;\n', ZIG_LIB]
    for i in range(n):
        parts.append(zig_unit(i))
    calls = "\n".join(f"    acc = acc + try unit{i}.run();" for i in range(n))
    parts.append(f"\npub fn main() !u8 {{\n    var acc: u64 = 0;\n{calls}\n    return if (acc > 0) 0 else 1;\n}}\n")
    return "\n".join(parts)



ML_TY = {
    "u8": "int", "u16": "int", "u32": "int", "u64": "int",
    "bool": "bool", "string": "string", "f32": "float", "f64": "float",
}

ML_LIB = r"""
module Lib = struct
  type 'a bx = { value : 'a; tag : int }
  type ('a, 'b) duo = { first : 'a; second : 'b }

  module type SZ = sig type t val sz : t -> int end
  module type ENC = sig type t type o val enc : t -> o end

  module Sz_u32 = struct type t = int let sz v = v + 1 end
  module Sz_u64 = struct type t = int let sz v = v + 2 end
  module Sz_bx (S : SZ) = struct type t = S.t bx let sz (b : t) = S.sz b.value + 1 end
  module Enc_u32 = struct type t = int type o = int let enc v = v end
  module Enc_u64 = struct type t = int type o = int let enc v = v end

  let mk v = { value = v; tag = 1 }
  let get (b : 'a bx) = b.value
  let wrap2 v = mk (mk v)
  let mk_pair first second = { first; second }
  let swap (p : ('a, 'b) duo) = { first = p.second; second = p.first }
  let zero () = None
  let measure v = if Option.is_some v then 1 else 0
  let total (type a) (module S : SZ with type t = a) (v : a) = S.sz v
  let encode (type a) (module E : ENC with type t = a and type o = int) (v : a) = E.enc v
end
"""

ML_UNIT = r"""
module U§ = struct
  open Lib
  type vec2 = { x : int; y : int }
  type shape = Dot | Line of int | Rect of vec2
  type op = Push of int | Pop | Reset

  type item = { sku : int; qty : int; unit_price : int; discount : int option; note : string option }
  type shipped_info = { carrier : int; eta : int }
  type order_status = Draft | Placed of int | Shipped of shipped_info | Delivered of int | Canceled of string
  type order = { id : int; status : order_status; lines : item list; shipping : (int, int) duo; paid : bool }
  type event = Credit of int | Debit of int | Hold of (int, int) duo | Release | Note of string
  type account = { id : int; balance : int; held : int; tier : int; closed : bool; last_event : event option }
  type sample = { at : int; value : int; quality : int }
  type series = { name : string; points : sample list; window : (int, int) duo }
  type stat = Count of int | Sum of int | Extent of (int, int) duo | Empty

  let area (s : shape) : int =
    match s with
    | Dot -> 1
    | Line len -> len
    | Rect d -> d.x * d.y

  let step (state : vec2) (o : op) : vec2 =
    match o with
    | Push n -> { x = state.x + n; y = state.y + 1 }
    | Pop -> if state.y > 0 then { x = state.x; y = state.y - 1 } else state
    | Reset -> { x = 0; y = 0 }

  let churn (seed : int) : int =
    let base = seed + § in
    let low = base mod 7 in
    let high = base / 3 in
    let start = { x = low; y = high } in
    let s = if low > high then Rect start else if low = 0 then Dot else Line high in
    let a = area s in
    let cur = step start (Push a) in
    let cur = step cur (Push low) in
    let cur = step cur Pop in
    let cur = if cur.x > 100 then step cur Reset else cur in
    let sum = ref 0 in
    for k = 0 to low - 1 do
      sum := !sum + k * 2 + cur.x
    done;
    let verdict =
      match cur with
      | { x = 0; y } -> y
      | { x; y } when x = y -> x + y
      | { x; y } -> x * 2 + y
    in
    if verdict > !sum then verdict - !sum else !sum - verdict + a

  let line_cost (line : item) : int =
    let gross = line.unit_price * line.qty in
    let disc = Option.value line.discount ~default:0 in
    let cut = if Option.is_some line.note then 1 else 0 in
    if disc + cut >= gross then 0 else gross - disc - cut

  let order_total (o : order) : int =
    let sum = List.fold_left (fun sum line -> sum + line_cost line) 0 o.lines in
    let ship = o.shipping.first + o.shipping.second in
    if o.paid then sum else sum + ship

  let advance (o : order) (now : int) : order =
    let next =
      match o.status with
      | Draft -> Placed now
      | Placed at -> if now - at > 10 then Shipped { carrier = §; eta = now + 3 } else Placed at
      | Shipped s -> if now >= s.eta then Delivered now else Shipped s
      | Delivered at -> Delivered at
      | Canceled why -> Canceled why
    in
    { o with status = next }

  let apply_event (a : account) (e : event) : account =
    if a.closed then a
    else
      let next =
        match e with
        | Credit n -> { a with balance = a.balance + n }
        | Debit n -> if n > a.balance then { a with closed = true } else { a with balance = a.balance - n }
        | Hold h -> { a with held = a.held + h.first + h.second }
        | Release -> { a with balance = a.balance + a.held; held = 0 }
        | Note _ -> a
      in
      { next with last_event = Some e }

  let settle (seed : int) : int =
    let count = seed mod 5 + 1 in
    let lines =
      List.init count (fun k ->
          { sku = seed * 31 + k; qty = k + 1; unit_price = 100 + k * 7;
            discount = (if k mod 2 = 0 then Some (k * 3) else None); note = None })
    in
    let o = ref { id = seed; status = Draft; lines; shipping = mk_pair 5 (seed mod 9); paid = seed mod 3 = 0 } in
    for t = 0 to 3 do
      o := advance !o (seed + t * 6)
    done;
    let o = !o in
    let due = order_total o in
    let acct = { id = seed; balance = due; held = 0; tier = seed mod 4; closed = false; last_event = None } in
    let acct = apply_event acct (Hold (mk_pair (due / 2) 1)) in
    let acct = apply_event acct (Debit (seed mod 50)) in
    let acct = apply_event acct Release in
    let acct = apply_event acct (Credit 3) in
    let bonus = match acct.last_event with Some (Credit n) -> n | Some _ -> 1 | None -> 0 in
    let late = match o.status with Delivered at -> at mod 13 | _ -> 0 in
    acct.balance + bonus + late

  let quantize (v : int) : int =
    if v < 10 then 0 else if v < 100 then 1 else if v < 1000 then 2 else 3

  let summarize (s : series) : stat =
    match s.points with
    | [] -> Empty
    | p0 :: _ ->
      let lo = ref p0.value in
      let hi = ref !lo in
      let total = ref 0 in
      let kept = ref 0 in
      List.iter
        (fun (p : sample) ->
          if p.quality <> 0 && p.at >= s.window.first && p.at <= s.window.second then begin
            if p.value < !lo then lo := p.value;
            if p.value > !hi then hi := p.value;
            total := !total + p.value;
            kept := !kept + 1
          end)
        s.points;
      if !kept = 0 then Count 0 else if !kept = 1 then Sum !total else Extent (mk_pair !lo !hi)

  let sample_run (seed : int) : int =
    let cap = seed mod 7 + 2 in
    let pts =
      List.init cap (fun k ->
          let q = if k mod 3 = 0 then 0 else quantize (k * seed + §) in
          { at = k * 10; value = (seed + k * k) mod 251; quality = q })
    in
    let s = { name = "run"; points = pts; window = mk_pair 0 (900 + seed) } in
    let verdict =
      match summarize s with
      | Empty -> 0
      | Count n -> n
      | Sum n -> n mod 100
      | Extent e -> e.second - e.first
    in
    verdict + cap

  module Sz_item = struct type t = item let sz (v : t) = v.sku mod 1000 + v.qty end
  module Sz_sample = struct type t = sample let sz (v : t) = v.at + v.value end
  module Enc_item = struct type t = item type o = int let enc (v : t) = v.sku * 31 + v.unit_price end

  let test () : int =
    let acc = § in
%TEST_BODY%
    let probe = { sku = §; qty = 2; unit_price = 40; discount = None; note = Some "x" } in
    let probe2 = { at = 1; value = acc mod 5; quality = 1 } in
    let acc = acc + total (module Sz_item) probe + encode (module Enc_item) probe + total (module Sz_bx (Sz_item)) (mk probe) + total (module Sz_sample) probe2 in
    let acc = acc + churn acc in
    let acc = acc + churn (acc mod 97) in
    let acc = acc + settle (acc mod 61) in
    let acc = acc + settle (§ + 7) in
    let acc = acc + sample_run (acc mod 43) in
    let acc = acc + sample_run (§ + 11) in
    acc
end
"""


def ml_unit(i):
    body = []
    for n, t in enumerate(UNSIGNED):
        body.append(f"    let acc = acc + get (mk {n + 1}) in")
    for n, t in enumerate(SIZED):
        body.append(f"    let acc = acc + total (module Sz_bx (Sz_{t})) (mk {n + 1}) in")
    for n, t in enumerate(UNSIGNED):
        body.append(f"    let acc = acc + get (get (wrap2 {n + 1})) in")
    for n, (a, b) in enumerate(PAIRS):
        body.append(
            f"    let p{n} = swap (mk_pair (zero () : {ML_TY[a]} option) (zero () : {ML_TY[b]} option)) in\n"
            f"    let acc = acc + measure p{n}.first + measure p{n}.second in"
        )
    for n, t in enumerate(SIZED):
        body.append(f"    let acc = acc + encode (module Enc_{t}) {n + 1} in")
    return ML_UNIT.replace("%TEST_BODY%", "\n".join(body)).replace("§", str(i))


def ml_program(n):
    parts = [ML_LIB]
    for i in range(n):
        parts.append(ml_unit(i))
    calls = "\n".join(f"  let acc = acc + U{i}.test () in" for i in range(n))
    parts.append(f"\nlet () =\n  let acc = 0 in\n{calls}\n  exit (if acc > 0 then 0 else 1)\n")
    return "\n".join(parts)



JAVA_TY = {
    "u8": "Byte", "u16": "Short", "u32": "Integer", "u64": "Long",
    "bool": "Boolean", "string": "String", "f32": "Float", "f64": "Double",
}

JAVA_LIT = {
    "u8": "(byte) {}", "u16": "(short) {}", "u32": "{}", "u64": "{}L",
}

JAVA_WIDEN = {
    "u8": "Byte.toUnsignedLong({})", "u16": "Short.toUnsignedLong({})",
    "u32": "Integer.toUnsignedLong({})", "u64": "{}",
}

JAVA_LIB = r"""
    static final class Bx<T> {
        T value;
        long tag;

        Bx(T value, long tag) {
            this.value = value;
            this.tag = tag;
        }
    }

    static final class Duo<A, B> {
        A first;
        B second;

        Duo(A first, B second) {
            this.first = first;
            this.second = second;
        }
    }

    static final class Opt<T> {
        boolean has;
        T val;

        Opt() {
        }

        Opt(boolean has, T val) {
            this.has = has;
            this.val = val;
        }
    }

    interface Sz {
        long sz();
    }

    interface Enc<O> {
        O enc();
    }

    static final class U32Val implements Sz, Enc<Long> {
        int v;

        U32Val(int v) {
            this.v = v;
        }

        public long sz() {
            return Integer.toUnsignedLong(v) + 1;
        }

        public Long enc() {
            return Integer.toUnsignedLong(v);
        }
    }

    static final class U64Val implements Sz, Enc<Long> {
        long v;

        U64Val(long v) {
            this.v = v;
        }

        public long sz() {
            return v + 2;
        }

        public Long enc() {
            return v;
        }
    }

    static <T extends Sz> long szBx(Bx<T> b) {
        return b.value.sz() + 1;
    }

    static <T> Bx<T> mk(T value) {
        return new Bx<T>(value, 1);
    }

    static <T> T get(Bx<T> b) {
        return b.value;
    }

    static <T> Bx<Bx<T>> wrap2(T value) {
        return mk(mk(value));
    }

    static <A, B> Duo<A, B> mkPair(A first, B second) {
        return new Duo<A, B>(first, second);
    }

    static <A, B> Duo<B, A> swap(Duo<A, B> p) {
        return new Duo<B, A>(p.second, p.first);
    }

    static <T> Opt<T> zero() {
        return new Opt<T>();
    }

    static <T> long measure(Opt<T> v) {
        if (v.has) {
            return 1;
        }
        return 0;
    }

    static <T extends Sz> long total(T v) {
        return v.sz();
    }

    static <T extends Enc<Long>> long encode(T v) {
        return v.enc();
    }
"""

JAVA_UNIT = r"""
    static final class U§ {
        static final class Vec2 {
            long x;
            long y;

            Vec2(long x, long y) {
                this.x = x;
                this.y = y;
            }
        }

        static final int SHAPE_DOT = 0;
        static final int SHAPE_LINE = 1;
        static final int SHAPE_RECT = 2;

        static final class Shape {
            int tag;
            long line;
            Vec2 rect;

            Shape(int tag, long line, Vec2 rect) {
                this.tag = tag;
                this.line = line;
                this.rect = rect;
            }
        }

        static final int OP_PUSH = 0;
        static final int OP_POP = 1;
        static final int OP_RESET = 2;

        static final class Op {
            int tag;
            long push;

            Op(int tag, long push) {
                this.tag = tag;
                this.push = push;
            }
        }

        static final class Item implements Sz, Enc<Long> {
            long sku;
            int qty;
            long unitPrice;
            Opt<Long> discount;
            Opt<String> note;

            Item(long sku, int qty, long unitPrice, Opt<Long> discount, Opt<String> note) {
                this.sku = sku;
                this.qty = qty;
                this.unitPrice = unitPrice;
                this.discount = discount;
                this.note = note;
            }

            public long sz() {
                return sku % 1000 + Integer.toUnsignedLong(qty);
            }

            public Long enc() {
                return sku * 31 + unitPrice;
            }
        }

        static final class ShippedInfo {
            long carrier;
            long eta;

            ShippedInfo(long carrier, long eta) {
                this.carrier = carrier;
                this.eta = eta;
            }
        }

        static final int STATUS_DRAFT = 0;
        static final int STATUS_PLACED = 1;
        static final int STATUS_SHIPPED = 2;
        static final int STATUS_DELIVERED = 3;
        static final int STATUS_CANCELED = 4;

        static final class OrderStatus {
            int tag;
            long placed;
            ShippedInfo shipped;
            long delivered;
            String canceled;

            OrderStatus(int tag, long placed, ShippedInfo shipped, long delivered, String canceled) {
                this.tag = tag;
                this.placed = placed;
                this.shipped = shipped;
                this.delivered = delivered;
                this.canceled = canceled;
            }
        }

        static final class Order {
            long id;
            OrderStatus status;
            ArrayList<Item> lines;
            Duo<Long, Long> shipping;
            boolean paid;

            Order(long id, OrderStatus status, ArrayList<Item> lines, Duo<Long, Long> shipping, boolean paid) {
                this.id = id;
                this.status = status;
                this.lines = lines;
                this.shipping = shipping;
                this.paid = paid;
            }

            Order(Order other) {
                this(other.id, other.status, other.lines, other.shipping, other.paid);
            }
        }

        static final int EVENT_CREDIT = 0;
        static final int EVENT_DEBIT = 1;
        static final int EVENT_HOLD = 2;
        static final int EVENT_RELEASE = 3;
        static final int EVENT_NOTE = 4;

        static final class Event {
            int tag;
            long credit;
            long debit;
            Duo<Long, Long> hold;
            String note;

            Event(int tag, long credit, long debit, Duo<Long, Long> hold, String note) {
                this.tag = tag;
                this.credit = credit;
                this.debit = debit;
                this.hold = hold;
                this.note = note;
            }
        }

        static final class Account {
            long id;
            long balance;
            long held;
            byte tier;
            boolean closed;
            Opt<Event> lastEvent;

            Account(long id, long balance, long held, byte tier, boolean closed, Opt<Event> lastEvent) {
                this.id = id;
                this.balance = balance;
                this.held = held;
                this.tier = tier;
                this.closed = closed;
                this.lastEvent = lastEvent;
            }

            Account(Account other) {
                this(other.id, other.balance, other.held, other.tier, other.closed, other.lastEvent);
            }
        }

        static final class Sample implements Sz {
            long at;
            long value;
            byte quality;

            Sample(long at, long value, byte quality) {
                this.at = at;
                this.value = value;
                this.quality = quality;
            }

            public long sz() {
                return at + value;
            }
        }

        static final class Series {
            String name;
            ArrayList<Sample> points;
            Duo<Long, Long> window;

            Series(String name, ArrayList<Sample> points, Duo<Long, Long> window) {
                this.name = name;
                this.points = points;
                this.window = window;
            }
        }

        static final int STAT_COUNT = 0;
        static final int STAT_SUM = 1;
        static final int STAT_EXTENT = 2;
        static final int STAT_EMPTY = 3;

        static final class Stat {
            int tag;
            long count;
            long sum;
            Duo<Long, Long> extent;

            Stat(int tag, long count, long sum, Duo<Long, Long> extent) {
                this.tag = tag;
                this.count = count;
                this.sum = sum;
                this.extent = extent;
            }
        }

        static long area(Shape s) {
            switch (s.tag) {
                case SHAPE_DOT:
                    return 1;
                case SHAPE_LINE:
                    return s.line;
                default:
                    return s.rect.x * s.rect.y;
            }
        }

        static Vec2 step(Vec2 state, Op o) {
            switch (o.tag) {
                case OP_PUSH:
                    return new Vec2(state.x + o.push, state.y + 1);
                case OP_POP:
                    if (state.y > 0) {
                        return new Vec2(state.x, state.y - 1);
                    } else {
                        return state;
                    }
                default:
                    return new Vec2(0, 0);
            }
        }

        static long churn(long seed) {
            long base = seed + §;
            long low = base % 7;
            long high = base / 3;
            Vec2 start = new Vec2(low, high);
            Shape s;
            if (low > high) {
                s = new Shape(SHAPE_RECT, 0, start);
            } else if (low == 0) {
                s = new Shape(SHAPE_DOT, 0, null);
            } else {
                s = new Shape(SHAPE_LINE, high, null);
            }
            long a = area(s);
            Vec2 cur = start;
            cur = step(cur, new Op(OP_PUSH, a));
            cur = step(cur, new Op(OP_PUSH, low));
            cur = step(cur, new Op(OP_POP, 0));
            if (cur.x > 100) {
                cur = step(cur, new Op(OP_RESET, 0));
            }
            long k = 0;
            long sum = 0;
            while (k < low) {
                sum = sum + k * 2 + cur.x;
                k = k + 1;
            }
            long verdict;
            if (cur.x == 0) {
                verdict = cur.y;
            } else if (cur.x == cur.y) {
                verdict = cur.x + cur.y;
            } else {
                verdict = cur.x * 2 + cur.y;
            }
            if (verdict > sum) {
                return verdict - sum;
            }
            return sum - verdict + a;
        }

        static long lineCost(Item line) {
            long gross = line.unitPrice * Integer.toUnsignedLong(line.qty);
            long disc = 0;
            if (line.discount.has) {
                disc = line.discount.val;
            }
            long cut = 0;
            if (line.note.has) {
                cut = 1;
            }
            if (disc + cut >= gross) {
                return 0;
            }
            return gross - disc - cut;
        }

        static long orderTotal(Order o) {
            long sum = 0;
            for (Item line : o.lines) {
                sum = sum + lineCost(line);
            }
            long ship = o.shipping.first + o.shipping.second;
            if (o.paid) {
                return sum;
            }
            return sum + ship;
        }

        static Order advance(Order o, long now) {
            OrderStatus next;
            switch (o.status.tag) {
                case STATUS_DRAFT:
                    next = new OrderStatus(STATUS_PLACED, now, null, 0, null);
                    break;
                case STATUS_PLACED:
                    if (now - o.status.placed > 10) {
                        next = new OrderStatus(STATUS_SHIPPED, 0, new ShippedInfo(§, now + 3), 0, null);
                    } else {
                        next = new OrderStatus(STATUS_PLACED, o.status.placed, null, 0, null);
                    }
                    break;
                case STATUS_SHIPPED:
                    if (now >= o.status.shipped.eta) {
                        next = new OrderStatus(STATUS_DELIVERED, 0, null, now, null);
                    } else {
                        next = new OrderStatus(STATUS_SHIPPED, 0, o.status.shipped, 0, null);
                    }
                    break;
                case STATUS_DELIVERED:
                    next = new OrderStatus(STATUS_DELIVERED, 0, null, o.status.delivered, null);
                    break;
                default:
                    next = new OrderStatus(STATUS_CANCELED, 0, null, 0, o.status.canceled);
                    break;
            }
            Order copy = new Order(o);
            copy.status = next;
            return copy;
        }

        static Account applyEvent(Account a, Event e) {
            if (a.closed) {
                return a;
            }
            Account next = new Account(a);
            switch (e.tag) {
                case EVENT_CREDIT:
                    next.balance = a.balance + e.credit;
                    break;
                case EVENT_DEBIT:
                    if (e.debit > a.balance) {
                        next.closed = true;
                    } else {
                        next.balance = a.balance - e.debit;
                    }
                    break;
                case EVENT_HOLD:
                    next.held = a.held + e.hold.first + e.hold.second;
                    break;
                case EVENT_RELEASE:
                    next.balance = a.balance + a.held;
                    next.held = 0;
                    break;
                default:
                    break;
            }
            next.lastEvent = new Opt<Event>(true, e);
            return next;
        }

        static long settle(long seed) {
            ArrayList<Item> lines = new ArrayList<Item>();
            long count = seed % 5 + 1;
            for (long k = 0; k < count; k++) {
                Opt<Long> discount = new Opt<Long>();
                if (k % 2 == 0) {
                    discount = new Opt<Long>(true, k * 3);
                }
                lines.add(new Item(seed * 31 + k, (int) (k + 1), 100 + k * 7, discount, new Opt<String>()));
            }
            Order o = new Order(seed, new OrderStatus(STATUS_DRAFT, 0, null, 0, null), lines,
                    mkPair(5L, seed % 9), seed % 3 == 0);
            long t = 0;
            while (t < 4) {
                o = advance(o, seed + t * 6);
                t = t + 1;
            }
            long due = orderTotal(o);
            Account acct = new Account(seed, due, 0, (byte) (seed % 4), false, new Opt<Event>());
            acct = applyEvent(acct, new Event(EVENT_HOLD, 0, 0, mkPair(due / 2, 1L), null));
            acct = applyEvent(acct, new Event(EVENT_DEBIT, 0, seed % 50, null, null));
            acct = applyEvent(acct, new Event(EVENT_RELEASE, 0, 0, null, null));
            acct = applyEvent(acct, new Event(EVENT_CREDIT, 3, 0, null, null));
            long bonus;
            if (acct.lastEvent.has && acct.lastEvent.val.tag == EVENT_CREDIT) {
                bonus = acct.lastEvent.val.credit;
            } else if (acct.lastEvent.has) {
                bonus = 1;
            } else {
                bonus = 0;
            }
            long late = 0;
            if (o.status.tag == STATUS_DELIVERED) {
                late = o.status.delivered % 13;
            }
            return acct.balance + bonus + late;
        }

        static byte quantize(long v) {
            if (v < 10) {
                return 0;
            } else if (v < 100) {
                return 1;
            } else if (v < 1000) {
                return 2;
            } else {
                return 3;
            }
        }

        static Stat summarize(Series s) {
            if (s.points.size() == 0) {
                return new Stat(STAT_EMPTY, 0, 0, null);
            }
            long lo = s.points.get(0).value;
            long hi = lo;
            long total = 0;
            long kept = 0;
            for (Sample p : s.points) {
                if (p.quality == 0) {
                    continue;
                }
                if (p.at < s.window.first || p.at > s.window.second) {
                    continue;
                }
                if (p.value < lo) {
                    lo = p.value;
                }
                if (p.value > hi) {
                    hi = p.value;
                }
                total = total + p.value;
                kept = kept + 1;
            }
            if (kept == 0) {
                return new Stat(STAT_COUNT, 0, 0, null);
            } else if (kept == 1) {
                return new Stat(STAT_SUM, 0, total, null);
            } else {
                return new Stat(STAT_EXTENT, 0, 0, mkPair(lo, hi));
            }
        }

        static long sampleRun(long seed) {
            ArrayList<Sample> pts = new ArrayList<Sample>();
            long cap = seed % 7 + 2;
            long k = 0;
            while (k < cap) {
                byte q = 0;
                if (k % 3 != 0) {
                    q = quantize(k * seed + §);
                }
                pts.add(new Sample(k * 10, (seed + k * k) % 251, q));
                k = k + 1;
            }
            Series s = new Series("run", pts, mkPair(0L, 900 + seed));
            Stat st = summarize(s);
            long verdict;
            switch (st.tag) {
                case STAT_EMPTY:
                    verdict = 0;
                    break;
                case STAT_COUNT:
                    verdict = st.count;
                    break;
                case STAT_SUM:
                    verdict = st.sum % 100;
                    break;
                default:
                    verdict = st.extent.second - st.extent.first;
                    break;
            }
            return verdict + k;
        }

        static long test() {
            long acc = §;
%TEST_BODY%
            Item probe = new Item(§, 2, 40, new Opt<Long>(), new Opt<String>(true, "x"));
            Sample probe2 = new Sample(1, acc % 5, (byte) 1);
            acc = acc + total(probe) + encode(probe) + szBx(mk(probe)) + total(probe2);
            acc = acc + churn(acc);
            acc = acc + churn(acc % 97);
            acc = acc + settle(acc % 61);
            acc = acc + settle(§ + 7);
            acc = acc + sampleRun(acc % 43);
            acc = acc + sampleRun(§ + 11);
            return acc;
        }
    }
"""


def java_unit(i):
    body = []
    for n, t in enumerate(UNSIGNED):
        lit = JAVA_LIT[t].format(n + 1)
        body.append(f"            acc = acc + {JAVA_WIDEN[t].format(f'get(mk({lit}))')};")
    for n, t in enumerate(SIZED):
        body.append(f"            acc = acc + szBx(mk(new {t.upper()}Val({n + 1})));")
    for n, t in enumerate(UNSIGNED):
        lit = JAVA_LIT[t].format(n + 1)
        body.append(f"            acc = acc + {JAVA_WIDEN[t].format(f'get(get(wrap2({lit})))')};")
    for n, (a, b) in enumerate(PAIRS):
        body.append(
            f"            var p{n} = swap(mkPair(Stress.<{JAVA_TY[a]}>zero(), Stress.<{JAVA_TY[b]}>zero()));\n"
            f"            acc = acc + measure(p{n}.first) + measure(p{n}.second);"
        )
    for n, t in enumerate(SIZED):
        body.append(f"            acc = acc + encode(new {t.upper()}Val({n + 1}));")
    return JAVA_UNIT.replace("%TEST_BODY%", "\n".join(body)).replace("§", str(i))


def java_program(n):
    parts = ["import java.util.ArrayList;\n\npublic final class Stress {", JAVA_LIB]
    for i in range(n):
        parts.append(java_unit(i))
    calls = "\n".join(f"        acc = acc + U{i}.test();" for i in range(n))
    parts.append(
        "    public static void main(String[] args) {\n"
        f"        long acc = 0;\n{calls}\n"
        "        if (acc == 0) {\n            System.exit(1);\n        }\n    }\n}\n"
    )
    return "\n".join(parts)


KT_TY = {
    "u8": "UByte", "u16": "UShort", "u32": "UInt", "u64": "ULong",
    "bool": "Boolean", "string": "String", "f32": "Float", "f64": "Double",
}

KT_LIT = {"u8": "{}u", "u16": "{}u", "u32": "{}u", "u64": "{}uL"}

KT_LIB = r"""
class Bx<T>(val value: T, val tag: ULong)
class Duo<A, B>(val first: A, val second: B)

interface Sz {
    fun sz(): ULong
}

interface Enc<O> {
    fun enc(): O
}

class U32Val(val v: UInt) : Sz, Enc<ULong> {
    override fun sz(): ULong = v.toULong() + 1uL
    override fun enc(): ULong = v.toULong()
}

class U64Val(val v: ULong) : Sz, Enc<ULong> {
    override fun sz(): ULong = v + 2uL
    override fun enc(): ULong = v
}

fun <T : Sz> szBx(b: Bx<T>): ULong = b.value.sz() + 1uL

fun <T> mk(value: T): Bx<T> = Bx(value, 1uL)
fun <T> get(b: Bx<T>): T = b.value
fun <T> wrap2(value: T): Bx<Bx<T>> = mk(mk(value))
fun <A, B> mkPair(first: A, second: B): Duo<A, B> = Duo(first, second)
fun <A, B> swap(p: Duo<A, B>): Duo<B, A> = Duo(p.second, p.first)
fun <T> zero(): T? = null
fun <T> measure(v: T?): ULong = if (v != null) 1uL else 0uL
fun <T : Sz> total(v: T): ULong = v.sz()
fun <T : Enc<ULong>> encode(v: T): ULong = v.enc()
"""

KT_UNIT = r"""
object U§ {
    class Vec2(val x: ULong, val y: ULong)

    sealed interface Shape {
        object Dot : Shape
        class Line(val len: ULong) : Shape
        class Rect(val rect: Vec2) : Shape
    }

    sealed interface Op {
        class Push(val n: ULong) : Op
        object Pop : Op
        object Reset : Op
    }

    class Item(val sku: ULong, val qty: UInt, val unitPrice: ULong, val discount: ULong?, val note: String?) : Sz, Enc<ULong> {
        override fun sz(): ULong = sku % 1000uL + qty.toULong()
        override fun enc(): ULong = sku * 31uL + unitPrice
    }

    class ShippedInfo(val carrier: ULong, val eta: ULong)

    sealed interface OrderStatus {
        object Draft : OrderStatus
        class Placed(val at: ULong) : OrderStatus
        class Shipped(val info: ShippedInfo) : OrderStatus
        class Delivered(val at: ULong) : OrderStatus
        class Canceled(val why: String) : OrderStatus
    }

    data class Order(val id: ULong, val status: OrderStatus, val lines: ArrayList<Item>, val shipping: Duo<ULong, ULong>, val paid: Boolean)

    sealed interface Event {
        class Credit(val n: ULong) : Event
        class Debit(val n: ULong) : Event
        class Hold(val h: Duo<ULong, ULong>) : Event
        object Release : Event
        class Note(val text: String) : Event
    }

    data class Account(val id: ULong, val balance: ULong, val held: ULong, val tier: UByte, val closed: Boolean, val lastEvent: Event?)

    class Sample(val at: ULong, val value: ULong, val quality: UByte) : Sz {
        override fun sz(): ULong = at + value
    }

    class Series(val name: String, val points: ArrayList<Sample>, val window: Duo<ULong, ULong>)

    sealed interface Stat {
        class Count(val n: ULong) : Stat
        class Sum(val n: ULong) : Stat
        class Extent(val e: Duo<ULong, ULong>) : Stat
        object Empty : Stat
    }

    fun area(s: Shape): ULong = when (s) {
        is Shape.Dot -> 1uL
        is Shape.Line -> s.len
        is Shape.Rect -> s.rect.x * s.rect.y
    }

    fun step(state: Vec2, o: Op): Vec2 = when (o) {
        is Op.Push -> Vec2(state.x + o.n, state.y + 1uL)
        is Op.Pop -> if (state.y > 0uL) Vec2(state.x, state.y - 1uL) else state
        is Op.Reset -> Vec2(0uL, 0uL)
    }

    fun churn(seed: ULong): ULong {
        val base = seed + §uL
        val low = base % 7uL
        val high = base / 3uL
        val start = Vec2(low, high)
        val s: Shape = if (low > high) Shape.Rect(start) else if (low == 0uL) Shape.Dot else Shape.Line(high)
        val a = area(s)
        var cur = start
        cur = step(cur, Op.Push(a))
        cur = step(cur, Op.Push(low))
        cur = step(cur, Op.Pop)
        cur = if (cur.x > 100uL) step(cur, Op.Reset) else cur
        var k = 0uL
        var sum = 0uL
        while (k < low) {
            sum = sum + k * 2uL + cur.x
            k = k + 1uL
        }
        val verdict = if (cur.x == 0uL) cur.y else if (cur.x == cur.y) cur.x + cur.y else cur.x * 2uL + cur.y
        return if (verdict > sum) verdict - sum else sum - verdict + a
    }

    fun lineCost(line: Item): ULong {
        val gross = line.unitPrice * line.qty.toULong()
        val disc = line.discount ?: 0uL
        val cut = if (line.note != null) 1uL else 0uL
        return if (disc + cut >= gross) 0uL else gross - disc - cut
    }

    fun orderTotal(o: Order): ULong {
        var sum = 0uL
        for (line in o.lines) {
            sum = sum + lineCost(line)
        }
        val ship = o.shipping.first + o.shipping.second
        return if (o.paid) sum else sum + ship
    }

    fun advance(o: Order, now: ULong): Order {
        val next = when (val st = o.status) {
            is OrderStatus.Draft -> OrderStatus.Placed(now)
            is OrderStatus.Placed -> if (now - st.at > 10uL) OrderStatus.Shipped(ShippedInfo(§uL, now + 3uL)) else OrderStatus.Placed(st.at)
            is OrderStatus.Shipped -> if (now >= st.info.eta) OrderStatus.Delivered(now) else OrderStatus.Shipped(st.info)
            is OrderStatus.Delivered -> OrderStatus.Delivered(st.at)
            is OrderStatus.Canceled -> OrderStatus.Canceled(st.why)
        }
        return o.copy(status = next)
    }

    fun applyEvent(a: Account, e: Event): Account {
        if (a.closed) {
            return a
        }
        val next = when (e) {
            is Event.Credit -> a.copy(balance = a.balance + e.n)
            is Event.Debit -> if (e.n > a.balance) a.copy(closed = true) else a.copy(balance = a.balance - e.n)
            is Event.Hold -> a.copy(held = a.held + e.h.first + e.h.second)
            is Event.Release -> a.copy(balance = a.balance + a.held, held = 0uL)
            is Event.Note -> a
        }
        return next.copy(lastEvent = e)
    }

    fun settle(seed: ULong): ULong {
        val lines = ArrayList<Item>()
        val count = seed % 5uL + 1uL
        for (k in 0uL until count) {
            lines.add(Item(seed * 31uL + k, (k + 1uL).toUInt(), 100uL + k * 7uL, if (k % 2uL == 0uL) k * 3uL else null, null))
        }
        var o = Order(seed, OrderStatus.Draft, lines, mkPair(5uL, seed % 9uL), seed % 3uL == 0uL)
        var t = 0uL
        while (t < 4uL) {
            o = advance(o, seed + t * 6uL)
            t = t + 1uL
        }
        val due = orderTotal(o)
        var acct = Account(seed, due, 0uL, (seed % 4uL).toUByte(), false, null)
        acct = applyEvent(acct, Event.Hold(mkPair(due / 2uL, 1uL)))
        acct = applyEvent(acct, Event.Debit(seed % 50uL))
        acct = applyEvent(acct, Event.Release)
        acct = applyEvent(acct, Event.Credit(3uL))
        val last = acct.lastEvent
        val bonus = if (last is Event.Credit) last.n else if (last != null) 1uL else 0uL
        val status = o.status
        val late = if (status is OrderStatus.Delivered) status.at % 13uL else 0uL
        return acct.balance + bonus + late
    }

    fun quantize(v: ULong): UByte = if (v < 10uL) 0u else if (v < 100uL) 1u else if (v < 1000uL) 2u else 3u

    fun summarize(s: Series): Stat {
        if (s.points.size == 0) {
            return Stat.Empty
        }
        var lo = s.points[0].value
        var hi = lo
        var total = 0uL
        var kept = 0uL
        for (p in s.points) {
            if (p.quality == 0u.toUByte()) {
                continue
            }
            if (p.at < s.window.first || p.at > s.window.second) {
                continue
            }
            if (p.value < lo) {
                lo = p.value
            }
            if (p.value > hi) {
                hi = p.value
            }
            total = total + p.value
            kept = kept + 1uL
        }
        return if (kept == 0uL) Stat.Count(0uL) else if (kept == 1uL) Stat.Sum(total) else Stat.Extent(mkPair(lo, hi))
    }

    fun sampleRun(seed: ULong): ULong {
        val pts = ArrayList<Sample>()
        val cap = seed % 7uL + 2uL
        var k = 0uL
        while (k < cap) {
            val q: UByte = if (k % 3uL == 0uL) 0u else quantize(k * seed + §uL)
            pts.add(Sample(k * 10uL, (seed + k * k) % 251uL, q))
            k = k + 1uL
        }
        val s = Series("run", pts, mkPair(0uL, 900uL + seed))
        val verdict = when (val st = summarize(s)) {
            is Stat.Empty -> 0uL
            is Stat.Count -> st.n
            is Stat.Sum -> st.n % 100uL
            is Stat.Extent -> st.e.second - st.e.first
        }
        return verdict + k
    }

    fun test(): ULong {
        var acc = §uL
%TEST_BODY%
        val probe = Item(§uL, 2u, 40uL, null, "x")
        val probe2 = Sample(1uL, acc % 5uL, 1u)
        acc = acc + total(probe) + encode(probe) + szBx(mk(probe)) + total(probe2)
        acc = acc + churn(acc)
        acc = acc + churn(acc % 97uL)
        acc = acc + settle(acc % 61uL)
        acc = acc + settle(§uL + 7uL)
        acc = acc + sampleRun(acc % 43uL)
        acc = acc + sampleRun(§uL + 11uL)
        return acc
    }
}
"""


def kt_unit(i):
    body = []
    for n, t in enumerate(UNSIGNED):
        widen = "" if t == "u64" else ".toULong()"
        body.append(f"        acc = acc + get(mk<{KT_TY[t]}>({KT_LIT[t].format(n + 1)})){widen}")
    for n, t in enumerate(SIZED):
        body.append(f"        acc = acc + szBx(mk({t.upper()}Val({KT_LIT[t].format(n + 1)})))")
    for n, t in enumerate(UNSIGNED):
        widen = "" if t == "u64" else ".toULong()"
        body.append(f"        acc = acc + get(get(wrap2<{KT_TY[t]}>({KT_LIT[t].format(n + 1)}))){widen}")
    for n, (a, b) in enumerate(PAIRS):
        body.append(
            f"        val p{n} = swap(mkPair(zero<{KT_TY[a]}>(), zero<{KT_TY[b]}>()))\n"
            f"        acc = acc + measure(p{n}.first) + measure(p{n}.second)"
        )
    for n, t in enumerate(SIZED):
        body.append(f"        acc = acc + encode({t.upper()}Val({KT_LIT[t].format(n + 1)}))")
    return KT_UNIT.replace("%TEST_BODY%", "\n".join(body)).replace("§", str(i))


def kt_program(n):
    parts = ["import kotlin.system.exitProcess\n", KT_LIB]
    for i in range(n):
        parts.append(kt_unit(i))
    calls = "\n".join(f"    acc = acc + U{i}.test()" for i in range(n))
    parts.append(f"\nfun main() {{\n    var acc = 0uL\n{calls}\n    if (acc == 0uL) {{\n        exitProcess(1)\n    }}\n}}\n")
    return "\n".join(parts)


CS_TY = {
    "u8": "byte", "u16": "ushort", "u32": "uint", "u64": "ulong",
    "bool": "bool", "string": "string", "f32": "float", "f64": "double",
}

CS_LIT = {"u8": "(byte){}", "u16": "(ushort){}", "u32": "{}u", "u64": "{}ul"}

CS_LIB = r"""
static class Lib
{
    public interface ISz
    {
        ulong Sz();
    }

    public interface IEnc<O>
    {
        O Enc();
    }

    public struct Bx<T>
    {
        public T Value;
        public ulong Tag;
    }

    public struct Duo<A, B>
    {
        public A First;
        public B Second;
    }

    public struct Opt<T>
    {
        public bool Has;
        public T Val;
    }

    public struct U32Val : ISz, IEnc<ulong>
    {
        public uint V;

        public ulong Sz()
        {
            return V + 1;
        }

        public ulong Enc()
        {
            return V;
        }
    }

    public struct U64Val : ISz, IEnc<ulong>
    {
        public ulong V;

        public ulong Sz()
        {
            return V + 2;
        }

        public ulong Enc()
        {
            return V;
        }
    }

    public static ulong SzBx<T>(Bx<T> b) where T : ISz
    {
        return b.Value.Sz() + 1;
    }

    public static Bx<T> Mk<T>(T value)
    {
        return new Bx<T> { Value = value, Tag = 1 };
    }

    public static T Get<T>(Bx<T> b)
    {
        return b.Value;
    }

    public static Bx<Bx<T>> Wrap2<T>(T value)
    {
        return Mk(Mk(value));
    }

    public static Duo<A, B> MkPair<A, B>(A first, B second)
    {
        return new Duo<A, B> { First = first, Second = second };
    }

    public static Duo<B, A> Swap<A, B>(Duo<A, B> p)
    {
        return new Duo<B, A> { First = p.Second, Second = p.First };
    }

    public static Opt<T> Zero<T>()
    {
        return new Opt<T>();
    }

    public static ulong Measure<T>(Opt<T> v)
    {
        if (v.Has)
        {
            return 1;
        }
        return 0;
    }

    public static ulong Total<T>(T v) where T : ISz
    {
        return v.Sz();
    }

    public static ulong Encode<T>(T v) where T : IEnc<ulong>
    {
        return v.Enc();
    }
}
"""

CS_UNIT = r"""
static class U§
{
    public struct Vec2
    {
        public ulong X;
        public ulong Y;
    }

    public enum ShapeTag
    {
        Dot,
        Line,
        Rect,
    }

    public struct Shape
    {
        public ShapeTag Tag;
        public ulong Line;
        public Vec2 Rect;
    }

    public enum OpTag
    {
        Push,
        Pop,
        Reset,
    }

    public struct Op
    {
        public OpTag Tag;
        public ulong Push;
    }

    public struct Item : ISz, IEnc<ulong>
    {
        public ulong Sku;
        public uint Qty;
        public ulong UnitPrice;
        public Opt<ulong> Discount;
        public Opt<string> Note;

        public ulong Sz()
        {
            return Sku % 1000 + Qty;
        }

        public ulong Enc()
        {
            return Sku * 31 + UnitPrice;
        }
    }

    public struct ShippedInfo
    {
        public ulong Carrier;
        public ulong Eta;
    }

    public enum StatusTag
    {
        Draft,
        Placed,
        Shipped,
        Delivered,
        Canceled,
    }

    public struct OrderStatus
    {
        public StatusTag Tag;
        public ulong Placed;
        public ShippedInfo Shipped;
        public ulong Delivered;
        public string Canceled;
    }

    public struct Order
    {
        public ulong Id;
        public OrderStatus Status;
        public List<Item> Lines;
        public Duo<ulong, ulong> Shipping;
        public bool Paid;
    }

    public enum EventTag
    {
        Credit,
        Debit,
        Hold,
        Release,
        Note,
    }

    public struct Event
    {
        public EventTag Tag;
        public ulong Credit;
        public ulong Debit;
        public Duo<ulong, ulong> Hold;
        public string Note;
    }

    public struct Account
    {
        public ulong Id;
        public ulong Balance;
        public ulong Held;
        public byte Tier;
        public bool Closed;
        public Opt<Event> LastEvent;
    }

    public struct Sample : ISz
    {
        public ulong At;
        public ulong Value;
        public byte Quality;

        public ulong Sz()
        {
            return At + Value;
        }
    }

    public struct Series
    {
        public string Name;
        public List<Sample> Points;
        public Duo<ulong, ulong> Window;
    }

    public enum StatTag
    {
        Count,
        Sum,
        Extent,
        Empty,
    }

    public struct Stat
    {
        public StatTag Tag;
        public ulong Count;
        public ulong Sum;
        public Duo<ulong, ulong> Extent;
    }

    public static ulong Area(Shape s)
    {
        switch (s.Tag)
        {
            case ShapeTag.Dot:
                return 1;
            case ShapeTag.Line:
                return s.Line;
            default:
                return s.Rect.X * s.Rect.Y;
        }
    }

    public static Vec2 Step(Vec2 state, Op o)
    {
        switch (o.Tag)
        {
            case OpTag.Push:
                return new Vec2 { X = state.X + o.Push, Y = state.Y + 1 };
            case OpTag.Pop:
                if (state.Y > 0)
                {
                    return new Vec2 { X = state.X, Y = state.Y - 1 };
                }
                else
                {
                    return state;
                }
            default:
                return new Vec2 { X = 0, Y = 0 };
        }
    }

    public static ulong Churn(ulong seed)
    {
        ulong bas = seed + §;
        ulong low = bas % 7;
        ulong high = bas / 3;
        Vec2 start = new Vec2 { X = low, Y = high };
        Shape s;
        if (low > high)
        {
            s = new Shape { Tag = ShapeTag.Rect, Rect = start };
        }
        else if (low == 0)
        {
            s = new Shape { Tag = ShapeTag.Dot };
        }
        else
        {
            s = new Shape { Tag = ShapeTag.Line, Line = high };
        }
        ulong a = Area(s);
        Vec2 cur = start;
        cur = Step(cur, new Op { Tag = OpTag.Push, Push = a });
        cur = Step(cur, new Op { Tag = OpTag.Push, Push = low });
        cur = Step(cur, new Op { Tag = OpTag.Pop });
        if (cur.X > 100)
        {
            cur = Step(cur, new Op { Tag = OpTag.Reset });
        }
        ulong k = 0;
        ulong sum = 0;
        while (k < low)
        {
            sum = sum + k * 2 + cur.X;
            k = k + 1;
        }
        ulong verdict;
        if (cur.X == 0)
        {
            verdict = cur.Y;
        }
        else if (cur.X == cur.Y)
        {
            verdict = cur.X + cur.Y;
        }
        else
        {
            verdict = cur.X * 2 + cur.Y;
        }
        if (verdict > sum)
        {
            return verdict - sum;
        }
        return sum - verdict + a;
    }

    public static ulong LineCost(Item line)
    {
        ulong gross = line.UnitPrice * line.Qty;
        ulong disc = 0;
        if (line.Discount.Has)
        {
            disc = line.Discount.Val;
        }
        ulong cut = 0;
        if (line.Note.Has)
        {
            cut = 1;
        }
        if (disc + cut >= gross)
        {
            return 0;
        }
        return gross - disc - cut;
    }

    public static ulong OrderTotal(Order o)
    {
        ulong sum = 0;
        foreach (Item line in o.Lines)
        {
            sum = sum + LineCost(line);
        }
        ulong ship = o.Shipping.First + o.Shipping.Second;
        if (o.Paid)
        {
            return sum;
        }
        return sum + ship;
    }

    public static Order Advance(Order o, ulong now)
    {
        OrderStatus next;
        switch (o.Status.Tag)
        {
            case StatusTag.Draft:
                next = new OrderStatus { Tag = StatusTag.Placed, Placed = now };
                break;
            case StatusTag.Placed:
                if (now - o.Status.Placed > 10)
                {
                    next = new OrderStatus { Tag = StatusTag.Shipped, Shipped = new ShippedInfo { Carrier = §, Eta = now + 3 } };
                }
                else
                {
                    next = new OrderStatus { Tag = StatusTag.Placed, Placed = o.Status.Placed };
                }
                break;
            case StatusTag.Shipped:
                if (now >= o.Status.Shipped.Eta)
                {
                    next = new OrderStatus { Tag = StatusTag.Delivered, Delivered = now };
                }
                else
                {
                    next = new OrderStatus { Tag = StatusTag.Shipped, Shipped = o.Status.Shipped };
                }
                break;
            case StatusTag.Delivered:
                next = new OrderStatus { Tag = StatusTag.Delivered, Delivered = o.Status.Delivered };
                break;
            default:
                next = new OrderStatus { Tag = StatusTag.Canceled, Canceled = o.Status.Canceled };
                break;
        }
        o.Status = next;
        return o;
    }

    public static Account ApplyEvent(Account a, Event e)
    {
        if (a.Closed)
        {
            return a;
        }
        Account next = a;
        switch (e.Tag)
        {
            case EventTag.Credit:
                next.Balance = a.Balance + e.Credit;
                break;
            case EventTag.Debit:
                if (e.Debit > a.Balance)
                {
                    next.Closed = true;
                }
                else
                {
                    next.Balance = a.Balance - e.Debit;
                }
                break;
            case EventTag.Hold:
                next.Held = a.Held + e.Hold.First + e.Hold.Second;
                break;
            case EventTag.Release:
                next.Balance = a.Balance + a.Held;
                next.Held = 0;
                break;
            default:
                break;
        }
        next.LastEvent = new Opt<Event> { Has = true, Val = e };
        return next;
    }

    public static ulong Settle(ulong seed)
    {
        List<Item> lines = new List<Item>();
        ulong count = seed % 5 + 1;
        for (ulong k = 0; k < count; k++)
        {
            Opt<ulong> discount = new Opt<ulong>();
            if (k % 2 == 0)
            {
                discount = new Opt<ulong> { Has = true, Val = k * 3 };
            }
            lines.Add(new Item
            {
                Sku = seed * 31 + k,
                Qty = (uint)(k + 1),
                UnitPrice = 100 + k * 7,
                Discount = discount,
                Note = new Opt<string>(),
            });
        }
        Order o = new Order
        {
            Id = seed,
            Status = new OrderStatus { Tag = StatusTag.Draft },
            Lines = lines,
            Shipping = MkPair(5ul, seed % 9),
            Paid = seed % 3 == 0,
        };
        ulong t = 0;
        while (t < 4)
        {
            o = Advance(o, seed + t * 6);
            t = t + 1;
        }
        ulong due = OrderTotal(o);
        Account acct = new Account
        {
            Id = seed,
            Balance = due,
            Held = 0,
            Tier = (byte)(seed % 4),
            Closed = false,
            LastEvent = new Opt<Event>(),
        };
        acct = ApplyEvent(acct, new Event { Tag = EventTag.Hold, Hold = MkPair(due / 2, 1ul) });
        acct = ApplyEvent(acct, new Event { Tag = EventTag.Debit, Debit = seed % 50 });
        acct = ApplyEvent(acct, new Event { Tag = EventTag.Release });
        acct = ApplyEvent(acct, new Event { Tag = EventTag.Credit, Credit = 3 });
        ulong bonus;
        if (acct.LastEvent.Has && acct.LastEvent.Val.Tag == EventTag.Credit)
        {
            bonus = acct.LastEvent.Val.Credit;
        }
        else if (acct.LastEvent.Has)
        {
            bonus = 1;
        }
        else
        {
            bonus = 0;
        }
        ulong late = 0;
        if (o.Status.Tag == StatusTag.Delivered)
        {
            late = o.Status.Delivered % 13;
        }
        return acct.Balance + bonus + late;
    }

    public static byte Quantize(ulong v)
    {
        if (v < 10)
        {
            return 0;
        }
        else if (v < 100)
        {
            return 1;
        }
        else if (v < 1000)
        {
            return 2;
        }
        else
        {
            return 3;
        }
    }

    public static Stat Summarize(Series s)
    {
        if (s.Points.Count == 0)
        {
            return new Stat { Tag = StatTag.Empty };
        }
        ulong lo = s.Points[0].Value;
        ulong hi = lo;
        ulong total = 0;
        ulong kept = 0;
        foreach (Sample p in s.Points)
        {
            if (p.Quality == 0)
            {
                continue;
            }
            if (p.At < s.Window.First || p.At > s.Window.Second)
            {
                continue;
            }
            if (p.Value < lo)
            {
                lo = p.Value;
            }
            if (p.Value > hi)
            {
                hi = p.Value;
            }
            total = total + p.Value;
            kept = kept + 1;
        }
        if (kept == 0)
        {
            return new Stat { Tag = StatTag.Count, Count = 0 };
        }
        else if (kept == 1)
        {
            return new Stat { Tag = StatTag.Sum, Sum = total };
        }
        else
        {
            return new Stat { Tag = StatTag.Extent, Extent = MkPair(lo, hi) };
        }
    }

    public static ulong SampleRun(ulong seed)
    {
        List<Sample> pts = new List<Sample>();
        ulong cap = seed % 7 + 2;
        ulong k = 0;
        while (k < cap)
        {
            byte q = 0;
            if (k % 3 != 0)
            {
                q = Quantize(k * seed + §);
            }
            pts.Add(new Sample { At = k * 10, Value = (seed + k * k) % 251, Quality = q });
            k = k + 1;
        }
        Series s = new Series { Name = "run", Points = pts, Window = MkPair(0ul, 900 + seed) };
        Stat st = Summarize(s);
        ulong verdict;
        switch (st.Tag)
        {
            case StatTag.Empty:
                verdict = 0;
                break;
            case StatTag.Count:
                verdict = st.Count;
                break;
            case StatTag.Sum:
                verdict = st.Sum % 100;
                break;
            default:
                verdict = st.Extent.Second - st.Extent.First;
                break;
        }
        return verdict + k;
    }

    public static ulong Test()
    {
        ulong acc = §;
%TEST_BODY%
        Item probe = new Item { Sku = §, Qty = 2, UnitPrice = 40, Discount = new Opt<ulong>(), Note = new Opt<string> { Has = true, Val = "x" } };
        Sample probe2 = new Sample { At = 1, Value = acc % 5, Quality = 1 };
        acc = acc + Total(probe) + Encode(probe) + SzBx(Mk(probe)) + Total(probe2);
        acc = acc + Churn(acc);
        acc = acc + Churn(acc % 97);
        acc = acc + Settle(acc % 61);
        acc = acc + Settle(§ + 7);
        acc = acc + SampleRun(acc % 43);
        acc = acc + SampleRun(§ + 11);
        return acc;
    }
}
"""


def cs_unit(i):
    body = []
    for n, t in enumerate(UNSIGNED):
        body.append(f"        acc = acc + Get(Mk({CS_LIT[t].format(n + 1)}));")
    for n, t in enumerate(SIZED):
        body.append(f"        acc = acc + SzBx(Mk(new {t.upper()}Val {{ V = {n + 1} }}));")
    for n, t in enumerate(UNSIGNED):
        body.append(f"        acc = acc + Get(Get(Wrap2({CS_LIT[t].format(n + 1)})));")
    for n, (a, b) in enumerate(PAIRS):
        body.append(
            f"        var p{n} = Swap(MkPair(Zero<{CS_TY[a]}>(), Zero<{CS_TY[b]}>()));\n"
            f"        acc = acc + Measure(p{n}.First) + Measure(p{n}.Second);"
        )
    for n, t in enumerate(SIZED):
        body.append(f"        acc = acc + Encode(new {t.upper()}Val {{ V = {n + 1} }});")
    return CS_UNIT.replace("%TEST_BODY%", "\n".join(body)).replace("§", str(i))


def cs_program(n):
    parts = ["using System;\nusing System.Collections.Generic;\nusing static Lib;\n\n// nonce 0\n", CS_LIB]
    for i in range(n):
        parts.append(cs_unit(i))
    calls = "\n".join(f"        acc = acc + U{i}.Test();" for i in range(n))
    parts.append(
        "\nstatic class Stress\n{\n    static void Main()\n    {\n"
        f"        ulong acc = 0;\n{calls}\n"
        "        if (acc == 0)\n        {\n            Environment.Exit(1);\n        }\n    }\n}\n"
    )
    return "\n".join(parts)


CSPROJ = """<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <NoWarn>CS0649</NoWarn>
  </PropertyGroup>
</Project>
"""


HELLO = {
    "k1": 'fn main(): i32 {\n  println("hello world")\n  0\n}\n',
    "c": '#include <stdio.h>\n\nint main(void) {\n  printf("hello world\\n");\n  return 0;\n}\n',
    "cpp": '#include <cstdio>\n\nint main() {\n  std::printf("hello world\\n");\n  return 0;\n}\n',
    "rs": 'fn main() {\n    println!("hello world");\n}\n',
    "go": 'package main\n\nimport "fmt"\n\n// nonce 0\n\nfunc main() {\n\tfmt.Println("hello world")\n}\n',
    "zig": 'const std = @import("std");\n\npub fn main(init: std.process.Init) !void {\n    var buf: [64]u8 = undefined;\n    var out = std.Io.File.stdout().writer(init.io, &buf);\n    try out.interface.print("hello world\\n", .{});\n    try out.interface.flush();\n}\n',
    "ml": 'let () = print_endline "hello world"\n',
    "java": 'public final class Hello {\n    public static void main(String[] args) {\n        System.out.println("hello world");\n    }\n}\n',
    "kt": 'fun main() {\n    println("hello world")\n}\n',
    "cs": 'using System;\n\n// nonce 0\n\nstatic class Hello\n{\n    static void Main()\n    {\n        Console.WriteLine("hello world");\n    }\n}\n',
}

PROGRAMS = {
    "k1": k1_program, "c": c_program, "cpp": cpp_program, "cpp-hand-vec": cpp_hand_vec_program,
    "rs": rs_program, "go": go_program, "zig": zig_program, "ml": ml_program,
    "java": java_program, "kt": kt_program, "cs": cs_program,
}


def src_name(lang, stem):
    if lang == "java":
        return stem.capitalize() + ".java"
    if lang == "cpp-hand-vec":
        return f"{stem}-hand-vec.cpp"
    return f"{stem}.{lang}"


def main():
    what, out_dir = sys.argv[1], Path(sys.argv[2])
    out_dir.mkdir(parents=True, exist_ok=True)
    if what == "hello":
        for lang, text in HELLO.items():
            (out_dir / src_name(lang, "hello")).write_text(text)
        (out_dir / "hello.csproj").write_text(CSPROJ)
        return
    n = int(what)
    for lang in LANGS:
        text = PROGRAMS[lang](n)
        path = out_dir / src_name(lang, "stress")
        path.write_text(text)
        print(f"{path} {text.count(chr(10))} lines")
    (out_dir / "stress.csproj").write_text(CSPROJ)


main()
