#!/usr/bin/env python3
"""Emit the sized benchmark sources under gen/.

sieve, crc32 and strbuild are the hand-written sources with their size and
comptime/runtime constant rewritten. reflect-serialize is the hand-written
serializer with N generated struct types (8 fields each, a rotating mix of
scalar types) and a serialize-all function appended.
"""
import json
import pathlib
import shutil
import sys

ROOT = pathlib.Path(__file__).resolve().parent
GEN = ROOT / "gen"

FIELD_TYPES = {
    "k1": ["i32", "u8", "bool", "string", "i64", "u16", "u32", "i8"],
    "zig": ["i32", "u8", "bool", "[]const u8", "i64", "u16", "u32", "i8"],
    "cpp": ["int32_t", "uint8_t", "bool", "std::string_view", "int64_t", "uint16_t", "uint32_t", "int8_t"],
    "cpp26": ["int32_t", "uint8_t", "bool", "std::string_view", "int64_t", "uint16_t", "uint32_t", "int8_t"],
    "rust": ["i32", "u8", "bool", "&'static str", "i64", "u16", "u32", "i8"],
}
KINDS = ["int", "int", "bool", "str", "int", "int", "int", "int"]

SIZE_LINE = {
    ("sieve", "k1"): ("let N: int = 10000", "let N: int = {n}"),
    ("sieve", "zig"): ("const N = 10_000;", "const N = {n};"),
    ("sieve", "cpp"): ("constexpr long N = 10000;", "constexpr long N = {n};"),
    ("sieve", "rust"): ("const N: usize = 10_000;", "const N: usize = {n};"),
    ("crc32", "k1"): ("let B: int = 65536", "let B: int = {n}"),
    ("crc32", "zig"): ("const B = 65536;", "const B = {n};"),
    ("crc32", "cpp"): ("constexpr size_t B = 65536;", "constexpr size_t B = {n};"),
    ("crc32", "rust"): ("const B: usize = 65536;", "const B: usize = {n};"),
    ("strbuild", "k1"): ("let N: int = 10000", "let N: int = {n}"),
    ("strbuild", "zig"): ("const N = 10_000;", "const N = {n};"),
    ("strbuild", "cpp"): ("constexpr int N = 10000;", "constexpr int N = {n};"),
    ("strbuild", "rust"): ("const N: usize = 10_000;", "const N: usize = {n};"),
}
RUNTIME_LINE = {
    "k1": ("let COMPTIME: bool = true", "let COMPTIME: bool = false"),
    "zig": ("const comptime_work = true;", "const comptime_work = false;"),
    "cpp": ("#define COMPTIME 1", "#define COMPTIME 0"),
}
EXT = {"k1": "k1", "zig": "zig", "cpp": "cpp", "cpp26": "cpp", "rust": "rs"}


def field(i, k):
    kind = KINDS[(i + k) % 8]
    if kind == "int":
        return kind, (i * 8 + k) % 97
    if kind == "bool":
        return kind, (i + k) % 2 == 0
    return kind, f"s{i}_{k}"


def expected_total(n):
    total = 0
    for i in range(n):
        obj = {f"f{k}": field(i, k)[1] for k in range(8)}
        total += len(json.dumps(obj, separators=(",", ":")))
    return total


def literal(lang, kind, value):
    if kind == "bool":
        return "true" if value else "false"
    if kind == "str":
        return f'"{value}"'
    return str(value)


def struct_types(lang, i):
    return [FIELD_TYPES[lang][(i + k) % 8] for k in range(8)]


def reflect_source(lang, n):
    header = (ROOT / "reflect-serialize" / lang / f"serialize.{EXT[lang]}").read_text()
    out = [header, ""]
    for i in range(n):
        types = struct_types(lang, i)
        if lang == "k1":
            fields = ", ".join(f"f{k}: {t}" for k, t in enumerate(types))
            out.append(f"type t{i} = {{ {fields} }}")
        elif lang == "zig":
            fields = ", ".join(f"f{k}: {t}" for k, t in enumerate(types))
            out.append(f"const T{i} = struct {{ {fields} }};")
        elif lang in ("cpp", "cpp26"):
            fields = " ".join(f"{t} f{k};" for k, t in enumerate(types))
            out.append(f"struct T{i} {{ {fields} }};")
            if lang == "cpp":
                members = ", ".join(f'std::pair{{"f{k}", &T{i}::f{k}}}' for k in range(8))
                out.append(f"constexpr auto members(const T{i}&) {{ return std::tuple{{{members}}}; }}")
        elif lang == "rust":
            fields = ", ".join(f"f{k}: {t}" for k, t in enumerate(types))
            out.append("#[derive(Serialize)]")
            out.append(f"struct T{i} {{ {fields} }}")
    out.append("")
    calls = []
    for i in range(n):
        vals = [literal(lang, *field(i, k)) for k in range(8)]
        if lang == "k1":
            init = ", ".join(f"f{k} = {v}" for k, v in enumerate(vals))
            calls.append(f"  total = total + to-json(t{i}.{{ {init} }}).len()")
        elif lang == "zig":
            init = ", ".join(f".f{k} = {v}" for k, v in enumerate(vals))
            calls.append(f"    try writeJson(w, T{i}{{ {init} }});")
        elif lang in ("cpp", "cpp26"):
            calls.append(f"    total += json_len(T{i}{{{', '.join(vals)}}});")
        elif lang == "rust":
            init = ", ".join(f"f{k}: {v}" for k, v in enumerate(vals))
            calls.append(f"    total += json_len(&T{i} {{ {init} }});")
    body = "\n".join(calls)
    if lang == "k1":
        out.append(f"fn serialize-all(): int {{\n  let total = 0\n{body}\n  total\n}}")
    elif lang == "zig":
        out.append(f"fn serializeAll(w: anytype) !void {{\n{body or '    _ = w;'}\n}}")
    elif lang in ("cpp", "cpp26"):
        out.append(f"size_t serialize_all() {{\n    size_t total = 0;\n{body}\n    return total;\n}}")
    elif lang == "rust":
        out.append(f"fn serialize_all() -> usize {{\n    let mut total = 0;\n{body}\n    total\n}}")
    return "\n".join(out) + "\n"


def sized_source(bench, lang, n, comptime):
    src = (ROOT / bench / lang / f"{bench}.{EXT[lang]}").read_text()
    old, new = SIZE_LINE[(bench, lang)]
    assert old in src, (bench, lang, old)
    src = src.replace(old, new.format(n=n))
    if not comptime and lang in RUNTIME_LINE:
        old, new = RUNTIME_LINE[lang]
        assert old in src, (bench, lang, old)
        src = src.replace(old, new)
    return src


def emit(bench, lang, n, comptime):
    variant = "comptime" if comptime else "runtime"
    if bench == "reflect-serialize":
        d = GEN / bench / str(n) / lang
        d.mkdir(parents=True, exist_ok=True)
        if lang == "rust":
            shutil.copy(ROOT / bench / "rust" / "Cargo.toml", d / "Cargo.toml")
            (d / "src").mkdir(exist_ok=True)
            (d / "src" / "main.rs").write_text(reflect_source(lang, n))
            return d / "Cargo.toml"
        f = d / f"serialize.{EXT[lang]}"
        f.write_text(reflect_source(lang, n))
        return f
    d = GEN / bench / str(n) / lang / variant
    d.mkdir(parents=True, exist_ok=True)
    f = d / f"{bench}.{EXT[lang]}"
    f.write_text(sized_source(bench, lang, n, comptime))
    return f



if __name__ == "__main__":
    print(emit(sys.argv[1], sys.argv[2], int(sys.argv[3]), sys.argv[4:5] != ["runtime"]))
