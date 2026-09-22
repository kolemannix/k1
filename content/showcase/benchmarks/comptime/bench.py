#!/usr/bin/env python3
"""Measure compile-time execution across K1, Zig, C++ and Rust and write results.md. Optional arguments are regexes
that must fully match "<bench>/<size>/<lang>" to restrict the cells measured;
the other cells are kept from the previous gen/results.json."""
import datetime
import json
import os
import pathlib
import re
import shlex
import signal
import subprocess
import sys
import time
import zlib

import gen

ROOT = gen.ROOT
GEN = gen.GEN
TIMEOUT = 300
SLOW = 60
STACK_KB = 65520
CONSTEXPR_STEPS = "2000000000"
CXX26 = os.environ.get("CXX_P2996", "/private/tmp/claude-502/-Users-knix-dev-k1/32ab624d-47a3-49c6-b70e-100d66c086d9/scratchpad/clang-p2996/cxx")
HAVE_CXX26 = os.access(CXX26, os.X_OK)

SIZES = {
    "sieve": [10_000, 100_000, 1_000_000],
    "crc32": [65_536, 1_048_576, 8_388_608],
    "strbuild": [10_000, 100_000, 1_000_000],
    "reflect-serialize": [0, 10, 100, 500],
}
LANGS = {
    "sieve": ["k1", "zig", "cpp", "rust"],
    "crc32": ["k1", "zig", "cpp", "rust"],
    "strbuild": ["k1", "zig", "cpp", "rust"],
    "reflect-serialize": ["k1", "zig", "cpp", *(["cpp26"] if HAVE_CXX26 else []), "rust"],
}
LANG_NAME = {"k1": "K1", "zig": "Zig", "cpp": "C++", "cpp26": "C++26 (P2996)", "rust": "Rust"}
IR_ROWS = ("optimize", "reuse", "delete", "deduce", "promote", "inline")


def cmdline(cmd):
    return shlex.join(cmd)


CHILD = None


def run(cmd, cwd=None, env=None, timeout=TIMEOUT, shell=False):
    global CHILD
    proc = subprocess.Popen(cmd, cwd=cwd, env=env, shell=shell, start_new_session=True,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    CHILD = proc
    try:
        out, err = proc.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        kill_group(proc)
        proc.communicate()
        raise
    finally:
        CHILD = None
    return proc.returncode, out, err


def kill_group(proc):
    try:
        os.killpg(proc.pid, signal.SIGKILL)
    except ProcessLookupError:
        pass


def die(signum, frame):
    if CHILD:
        kill_group(CHILD)
    sys.exit(128 + signum)


def make_cell(bench, lang, size, comptime):
    f = gen.emit(bench, lang, size, comptime)
    d = f.parent
    reflect = bench == "reflect-serialize"
    name = "serialize" if reflect else bench
    cell = {"bench": bench, "lang": lang, "size": size, "comptime": comptime, "dir": d,
            "prepare": None, "env": None, "chatty": None}
    if lang == "k1":
        cell["build"] = ["k1", "--cache", "false", "build", str(f)]
        cell["chatty"] = ["k1", "--cache", "false", "--chatty", "true", "build", str(f)]
        cell["exe"] = d / ".k1-out" / name
    elif lang == "zig":
        cell["build"] = ["zig", "build-exe", "-O", "Debug", "-target", "aarch64-macos", "--stack", str(STACK_KB * 1024),
                         "--cache-dir", str(d / "zc"), "--global-cache-dir", str(GEN / "zig-global"),
                         f"-femit-bin={d / name}", str(f)]
        cell["prepare"] = f"rm -rf {d / 'zc'}"
        cell["exe"] = d / name
    elif lang == "cpp":
        steps = [] if reflect else [f"-fconstexpr-steps={CONSTEXPR_STEPS}"]
        cell["build"] = ["clang++", "-std=c++23", "-O0", *steps, str(f), "-o", str(d / name)]
        cell["exe"] = d / name
    elif lang == "cpp26":
        cell["build"] = [CXX26, "-O0", str(f), "-o", str(d / name)]
        cell["exe"] = d / name
    elif lang == "rust" and reflect:
        target = GEN / "cargo-target"
        cell["build"] = ["cargo", "build", "--manifest-path", str(f)]
        cell["env"] = {"CARGO_TARGET_DIR": str(target)}
        cell["prepare"] = f"touch {d / 'src' / 'main.rs'}"
        cell["exe"] = target / "debug" / "reflect_serialize"
    elif lang == "rust":
        cfg = ["--cfg", "comptime"] if comptime else []
        cell["build"] = ["rustc", "-C", "opt-level=0", *cfg, str(f), "-o", str(d / name)]
        cell["exe"] = d / name
    return cell


def env_for(cell):
    if cell["env"] is None:
        return None
    return {**os.environ, **cell["env"]}


def run_prepare(cell):
    if cell["prepare"]:
        code, out, err = run(cell["prepare"], shell=True, cwd=cell["dir"])
        if code != 0:
            raise RuntimeError(f"prepare failed: {cell['prepare']}\n{err}")


def feasibility(cell):
    run_prepare(cell)
    start = time.monotonic()
    try:
        code, out, err = run(cell["build"], cwd=cell["dir"], env=env_for(cell))
    except subprocess.TimeoutExpired:
        return {"status": "timeout", "elapsed": time.monotonic() - start}
    elapsed = time.monotonic() - start
    if code != 0:
        return {"status": "error", "elapsed": elapsed, "returncode": code, "stderr": (out + err).strip()}
    return {"status": "ok", "elapsed": elapsed}


def run_exe(cell):
    code, out, err = run(["sh", "-c", f'ulimit -s {STACK_KB}; exec "$0"', str(cell["exe"])])
    text = (out + err).strip()
    return text if code == 0 else f"exit {code}: {text}"


def hyperfine(cell, slow):
    out = cell["dir"] / "hyperfine.json"
    runs = ["--runs", "2"] if slow else ["--warmup", "1", "--min-runs", "5"]
    cmd = ["hyperfine", "--export-json", str(out), *runs]
    if cell["prepare"]:
        cmd += ["--prepare", cell["prepare"]]
    cmd.append(cmdline(cell["build"]))
    code, _, err = run(cmd, cwd=cell["dir"], env=env_for(cell), timeout=TIMEOUT * 8)
    if code != 0:
        raise RuntimeError(f"hyperfine failed: {cmdline(cmd)}\n{err}")
    res = json.loads(out.read_text())["results"][0]
    return {"mean": res["mean"], "stddev": res["stddev"] or 0.0, "min": res["min"], "max": res["max"],
            "runs": len(res["times"]), "slow": slow}


def parse_count(text):
    m = re.match(r"([\d.]+)([kM]?)", text)
    return float(m.group(1)) * {"": 1, "k": 1e3, "M": 1e6}[m.group(2)]


def chatty(cell):
    run_prepare(cell)
    _, _, err = run(cell["chatty"], cwd=cell["dir"])
    info = {}
    for line in err.splitlines():
        m = re.search(r"program \S+ took (\d+)ms", line)
        if m:
            info["total_ms"] = int(m.group(1))
        m = re.match(r"\s+([a-z ]+?)\s+(\d+)\s+([\d.]+)\s+\d+%(.*)$", line)
        if m:
            info[m.group(1)] = {"count": int(m.group(2)), "ms": float(m.group(3))}
            if m.group(1) == "run":
                instrs = re.search(r"(\S+) instrs", m.group(4))
                info["run"]["instrs"] = parse_count(instrs.group(1)) if instrs else 0
    return info


def ir_ms(chatty):
    return sum(chatty.get(row, {}).get("ms", 0) for row in IR_ROWS)


def primes_below(n):
    composite = bytearray(n)
    primes = []
    for i in range(2, n):
        if not composite[i]:
            primes.append(i)
            composite[i * i::i] = b"\x01" * len(range(i * i, n, i))
    return primes


def sieve_ops(n, primes):
    ops = n - 2
    for p in primes:
        if p * p >= n:
            break
        ops += (n - p * p + p - 1) // p
    return ops


def xorshift_bytes(n):
    x = 2463534242
    out = bytearray(n)
    for i in range(n):
        x ^= (x << 13) & 0xFFFFFFFF
        x ^= x >> 17
        x ^= (x << 5) & 0xFFFFFFFF
        out[i] = x & 0xFF
    return bytes(out)


def fnv1a(data):
    h = 2166136261
    for b in data:
        h = ((h ^ b) * 16777619) & 0xFFFFFFFF
    return h


def expected(bench, size):
    if bench == "sieve":
        primes = primes_below(size)
        return f"{len(primes)} {primes[-1]}", sieve_ops(size, primes), "sieve steps"
    if bench == "crc32":
        return str(zlib.crc32(xorshift_bytes(size))), size, "bytes"
    if bench == "strbuild":
        s = ",".join(str(i) for i in range(size)).encode()
        return f"{len(s)} {fnv1a(s)}", size, "numbers"
    return str(gen.expected_total(size)), size, "types"


def measure(bench, size, lang, comptime, timed_out):
    cell = make_cell(bench, lang, size, comptime)
    key = (bench, lang, comptime)
    result = {"bench": bench, "size": size, "lang": lang, "comptime": comptime, "build": cmdline(cell["build"])}
    if key in timed_out:
        result["status"] = "skipped"
        result["note"] = f"not attempted: N={timed_out[key]} already exceeded the {TIMEOUT} s limit"
        return result
    feas = feasibility(cell)
    result.update(feas)
    if feas["status"] == "timeout":
        timed_out[key] = size
        return result
    if feas["status"] == "error":
        return result
    result["output"] = run_exe(cell)
    result["timing"] = hyperfine(cell, feas["elapsed"] > SLOW)
    if cell["chatty"]:
        result["chatty"] = chatty(cell)
    return result


def tool_version(cmd):
    try:
        r = subprocess.run(cmd, capture_output=True, text=True)
        return (r.stdout + r.stderr).strip().splitlines()[0]
    except (OSError, IndexError):
        return "missing"


def sysctl(name):
    return subprocess.run(["sysctl", "-n", name], capture_output=True, text=True).stdout.strip()


def machine_info():
    return {
        "cpu": sysctl("machdep.cpu.brand_string"),
        "cores": sysctl("hw.ncpu"),
        "memory_gb": int(sysctl("hw.memsize")) // (1024 ** 3),
        "os": "macOS " + subprocess.run(["sw_vers", "-productVersion"], capture_output=True, text=True).stdout.strip(),
        "k1": tool_version(["k1", "--version"]),
        "zig": "zig " + tool_version(["zig", "version"]),
        "clang": tool_version(["clang++", "--version"]),
        "clang26": tool_version([CXX26, "--version"]) if HAVE_CXX26 else "missing (set CXX_P2996 to a clang-p2996 wrapper)",
        "rustc": tool_version(["rustc", "--version"]),
        "cargo": tool_version(["cargo", "--version"]),
        "hyperfine": tool_version(["hyperfine", "--version"]),
    }


def fmt_time(t):
    if not t:
        return "-"
    mark = " †" if t["slow"] else ""
    return f"{t['mean']:.3f} ± {t['stddev']:.3f}{mark}"


def fmt_status(r):
    if r["status"] == "timeout":
        return f"timed out (> {TIMEOUT} s)"
    if r["status"] == "error":
        return "compile error"
    return "not attempted"


def fmt_rate(ops, seconds, unit):
    if seconds is None or seconds <= 0:
        return "-"
    rate = ops / seconds
    if rate >= 1e6:
        return f"{rate / 1e6:.1f} M {unit}/s"
    if rate >= 1e3:
        return f"{rate / 1e3:.1f} k {unit}/s"
    return f"{rate:.0f} {unit}/s"


def fmt_size(bench, size):
    if bench == "crc32":
        return f"{size // 1024} KB" if size < 1024 * 1024 else f"{size // (1024 * 1024)} MB"
    return f"{size:,}"


def render(results, expectations, info, load):
    by = {}
    for r in results:
        by[(r["bench"], r["size"], r["lang"], r["comptime"])] = r
    out = []
    out.append("# Compile-time execution benchmarks\n")
    if os.environ.get("BENCH_NOTE"):
        out.append(f"> **{os.environ['BENCH_NOTE']}**\n")
    out.append(f"Generated {datetime.datetime.now():%Y-%m-%d %H:%M} by `run.sh`. "
               f"Load average at start {load[0]:.1f}, at end {load[1]:.1f} (a busy machine inflates every number).\n")
    out.append("## Machine and toolchains\n")
    out.append(f"- {info['cpu']}, {info['cores']} cores, {info['memory_gb']} GB, {info['os']}")
    for k in ["k1", "zig", "clang", "clang26", "rustc", "cargo", "hyperfine"]:
        out.append(f"- {info[k]}")
    out.append("")
    out.append("## Methodology\n")
    out.append(f"""- Every cell is a full compile to an executable, timed with `hyperfine --warmup 1 --min-runs 5`
  (mean ± standard deviation in seconds). A cell whose first compile took more than {SLOW} s is
  timed with `--runs 2` instead and marked †. A first compile that exceeds {TIMEOUT} s is a timeout;
  larger sizes of that language are then not attempted.
- Commands: K1 `k1 --cache false build <file>`; Zig `zig build-exe -O Debug -target aarch64-macos`
  with the local cache dir deleted before every run (the global cache keeps compiler_rt; the
  explicit target works around Zig 0.14.0 failing to find libSystem on this macOS);
  C++ `clang++ -std=c++23 -O0 -fconstexpr-steps={CONSTEXPR_STEPS}`; Rust `rustc -C opt-level=0`
  (`#![allow(long_running_const_eval)]` in the source, `--cfg comptime` selects the comptime variant);
  Rust reflect `cargo build` with dependencies prebuilt and `src/main.rs` touched before every run;
  C++26 the Bloomberg clang-p2996 fork (`$CXX_P2996`, a wrapper baking
  in `-std=c++26 -freflection-latest -stdlib=libc++`) at `-O0`.
- Compiles run in their own process group and a timeout kills the whole group, so no compiler
  outlives the harness.
- Control: the same source with the work moved to runtime (K1 `#static` dropped, Zig `comptime`
  dropped, C++ `constexpr` dropped so clang cannot fold it, Rust the `const` item removed). The
  compile-time cost is the difference of the two means; throughput divides the benchmark's work
  by that difference. K1 also reports what the compiler measures itself (`--chatty true`): the
  time spent running bytecode in the compile-time VM and the number of VM instructions executed.
- Every executable is run once (`ulimit -s {STACK_KB}`, and `--stack {STACK_KB * 1024}` for Zig whose
  linked-in main-thread stack size ignores the rlimit, so the runtime controls can hold their
  arrays on the stack) and its output compared with a Python reference; mismatches are flagged.
""")
    limits = []
    mismatches = []
    for bench in ["sieve", "crc32", "strbuild"]:
        out.append(f"## {bench}\n")
        out.append(DESCRIPTIONS[bench] + "\n")
        out.append("| N | language | comptime build (s) | runtime-control build (s) | comptime cost (s) | throughput | output |")
        out.append("|---|---|---|---|---|---|---|")
        for size in SIZES[bench]:
            exp, ops, unit = expectations[(bench, size)]
            for lang in LANGS[bench]:
                c = by.get((bench, size, lang, True))
                r = by.get((bench, size, lang, False))
                if c is None and r is None:
                    continue
                ct = c.get("timing") if c else None
                rt = r.get("timing") if r else None
                cost = ct["mean"] - rt["mean"] if ct and rt else None
                output = f"`{c['output']}`" if c and c.get("output") is not None else "-"
                ok = "" if c is None or c.get("output") is None or c["output"] == exp else " **MISMATCH**"
                if ok:
                    mismatches.append((bench, size, lang, c["output"], exp))
                if r and r.get("output") not in (None, exp):
                    mismatches.append((bench, size, lang + " (runtime)", r["output"], exp))
                cost_s = f"{cost:.3f}" if cost is not None else "-"
                out.append(f"| {fmt_size(bench, size)} | {LANG_NAME[lang]} | {fmt_time(ct) if ct else fmt_status(c)} | "
                           f"{fmt_time(rt) if rt else (fmt_status(r) if r else '-')} | {cost_s} | "
                           f"{fmt_rate(ops, cost, unit)} | {output}{ok} |")
                for x in (c, r):
                    if x and x["status"] in ("timeout", "error"):
                        limits.append(x)
        out.append("")
        out.append("K1 compile-time VM, as reported by `k1 --chatty true` for the comptime variant:\n")
        out.append("| N | VM run (ms) | VM instructions | VM instr/s | typecheck (ms) | IR optimization (ms) | whole compile (ms) |")
        out.append("|---|---|---|---|---|---|---|")
        for size in SIZES[bench]:
            c = by.get((bench, size, "k1", True))
            if not c or "chatty" not in c:
                continue
            ch = c["chatty"]
            run = ch.get("run", {"ms": 0, "instrs": 0})
            rate = fmt_rate(run["instrs"], run["ms"] / 1000, "instr") if run["ms"] else "-"
            out.append(f"| {fmt_size(bench, size)} | {run['ms']:.1f} | {run['instrs'] / 1e6:.1f} M | {rate} | "
                       f"{ch.get('typecheck', {}).get('ms', 0):.1f} | {ir_ms(ch):.1f} | {ch.get('total_ms', 0)} |")
        out.append("")
    bench = "reflect-serialize"
    out.append(f"## {bench}\n")
    out.append(DESCRIPTIONS[bench] + "\n")
    out.append("| N types | language | build (s) | per type (ms) | output |")
    out.append("|---|---|---|---|---|")
    for size in SIZES[bench]:
        exp, _, _ = expectations[(bench, size)]
        for lang in LANGS[bench]:
            c = by.get((bench, size, lang, True))
            if c is None:
                continue
            base = by.get((bench, 0, lang, True))
            t = c.get("timing")
            bt = base.get("timing") if base else None
            per = f"{(t['mean'] - bt['mean']) * 1000 / size:.2f}" if t and bt and size else "-"
            output = f"`{c['output']}`" if c.get("output") is not None else "-"
            ok = "" if c.get("output") is None or c["output"] == exp else " **MISMATCH**"
            if ok:
                mismatches.append((bench, size, lang, c["output"], exp))
            out.append(f"| {size} | {LANG_NAME[lang]} | {fmt_time(t) if t else fmt_status(c)} | {per} | {output}{ok} |")
            if c["status"] in ("timeout", "error"):
                limits.append(c)
    out.append("")
    out.append("K1 `--chatty true` rows for the comptime work of reflect-serialize (`meta` is the metaprogram expansion, `run` the VM time behind it):\n")
    out.append("| N types | meta (ms) | VM run (ms) | VM instructions | typecheck (ms) | IR optimization (ms) | codegen (ms) | LLVM passes (ms) | link (ms) | whole compile (ms) |")
    out.append("|---|---|---|---|---|---|---|---|---|---|")
    for size in SIZES[bench]:
        c = by.get((bench, size, "k1", True))
        if not c or "chatty" not in c:
            continue
        ch = c["chatty"]
        g = lambda k: ch.get(k, {}).get("ms", 0)
        out.append(f"| {size} | {g('meta'):.1f} | {g('run'):.1f} | {ch.get('run', {}).get('instrs', 0) / 1e6:.2f} M | "
                   f"{g('typecheck'):.1f} | {ir_ms(ch):.1f} | {g('codegen'):.1f} | {g('passes'):.1f} | {g('link'):.1f} | {ch.get('total_ms', 0)} |")
    out.append("")
    out.append("## Output verification\n")
    if mismatches:
        for bench, size, lang, got, exp in mismatches:
            out.append(f"- {bench} N={size} {lang}: got `{got}`, expected `{exp}`")
    else:
        out.append("Every executable that was built printed the reference value for its benchmark and size.")
    out.append("")
    out.append("## Limits hit\n")
    if not limits:
        out.append("None.")
    for x in limits:
        variant = "comptime" if x["comptime"] else "runtime control"
        out.append(f"### {x['bench']} N={x['size']:,} {LANG_NAME[x['lang']]} ({variant})\n")
        out.append(f"`{x['build']}`\n")
        if x["status"] == "timeout":
            out.append(f"Killed after {TIMEOUT} s.\n")
        elif not x.get("stderr"):
            out.append(f"Exited with code {x.get('returncode')} after {x['elapsed']:.0f} s with no diagnostic output.\n")
        else:
            out.append("```text")
            out.append("\n".join(x["stderr"].splitlines()[:30]))
            out.append("```\n")
    return "\n".join(out) + "\n"


DESCRIPTIONS = {
    "sieve": "Sieve of Eratosthenes below N at compile time, collecting the primes into an array; the runtime prints the "
             "count and the last prime. Work = outer iterations plus composite marks.",
    "crc32": "Generate the CRC-32 table (polynomial 0xEDB88320), generate B pseudo-random bytes with xorshift32, "
             "and checksum them, all at compile time; the runtime prints the checksum. Work = bytes.",
    "strbuild": "Format the integers 0..N in decimal with `,` separators into one string at compile time, then take "
                "its length and FNV-1a checksum; the runtime prints both. K1 uses `string-builder` and its `${}` "
                "formatter inside `#static`, Zig a fixed `[N * 8]u8` buffer and `std.fmt.formatIntBuf`, C++ a transient "
                "`constexpr std::string` with hand-written digit formatting (`std::to_string` is not constexpr), Rust a "
                "fixed `[u8; N * 8]` array with hand-written digits (no heap and no formatting machinery in const eval). "
                "Work = numbers formatted.",
    "reflect-serialize": "N generated struct types with 8 fields (a rotating mix of i32, u8, bool, string, i64, u16, u32, "
                         "i8) and a JSON serializer derived per type: K1 `#meta` walking `types/schema` and emitting the "
                         "writer code, Zig `@typeInfo` with `inline for`, C++ a template over a generator-written member "
                         "list (clang 21 has no reflection, so this is the manual alternative), C++26 P2996 reflection "
                         "(`nonstatic_data_members_of` + `template for`, no per-struct list) on the clang-p2996 fork, "
                         "Rust `serde` derive through "
                         "proc macros (cargo, dependencies prebuilt). Each program serializes one value per type and prints the total output length. "
                         "N=0 is the baseline; per type = (build(N) - build(0)) / N.",
}


def main():
    for sig in (signal.SIGINT, signal.SIGTERM, signal.SIGHUP):
        signal.signal(sig, die)
    filters = [re.compile(a) for a in sys.argv[1:]]
    GEN.mkdir(exist_ok=True)
    load_start = os.getloadavg()[0]
    info = machine_info()
    results = []
    if filters and (GEN / "results.json").exists():
        for p in json.loads((GEN / "results.json").read_text()):
            if not any(f.fullmatch(f"{p['bench']}/{p['size']}/{p['lang']}") for f in filters):
                results.append(p)
    expectations = {}
    timed_out = {}
    for bench, sizes in SIZES.items():
        for size in sizes:
            expectations[(bench, size)] = expected(bench, size)
            for lang in LANGS[bench]:
                label = f"{bench}/{size}/{lang}"
                if filters and not any(f.fullmatch(label) for f in filters):
                    continue
                variants = [True] if bench == "reflect-serialize" else [True, False]
                for comptime in variants:
                    print(f"{label} {'comptime' if comptime else 'runtime'} ...", flush=True)
                    r = measure(bench, size, lang, comptime, timed_out)
                    results.append(r)
                    t = r.get("timing")
                    print(f"  {r['status']} {fmt_time(t) if t else ''} {r.get('output', '')}", flush=True)
                    (GEN / "results.json").write_text(json.dumps(results, indent=1, default=str))
    load_end = os.getloadavg()[0]
    (ROOT / "results.md").write_text(render(results, expectations, info, (load_start, load_end)))
    print(f"wrote {ROOT / 'results.md'}")


if __name__ == "__main__":
    main()
