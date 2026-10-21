import os
import re
import subprocess
import sys

CONTAINER_KW = {"impl", "trait", "mod", "extern", "ns", "ability"}
ITEM_KW = CONTAINER_KW | {"fn", "struct", "enum", "union", "type", "const", "static", "macro_rules!", "let", "use"}
NESTED_KW = ITEM_KW - {"let", "use", "extern", "ns", "ability"}
SKIP_DIRS = {"target", "llvm", "node_modules"}

TOKEN = re.compile(
    r"""(?P<lc>//[^\n]*)"""
    r"""|(?P<bc>/\*)"""
    r"""|(?P<rs>\bb?r(?P<h>\#*)")"""
    r"""|(?P<s>"(?:\\.|[^"\\])*")"""
    r"""|(?P<bt>`(?:\\.|[^`\\])*`)"""
    r"""|(?P<ch>'(?:\\(?:x..|u\{[0-9a-fA-F]*\}|.)|[^\\'\n])')"""
    r"""|(?P<p>[{};\n])""",
    re.S,
)
LINE_HEAD = re.compile(
    r"""\s*(?:#!?\[[^\]]*\]\s*)*"""
    r"""((?:(?:pub(?:\s*\([^)]*\))?|const|unsafe|async|default|safe|extern(?:\s*"[^"]*")?)\s+)*)"""
    r"""([A-Za-z_][A-Za-z0-9_]*!?)?(.?)"""
)
QUAL_WORD = re.compile(r"[a-z]+")

CONTAINER, VERBATIM, BODY, INNER = range(4)


def scan(src, expand):
    keep = bytearray(len(src))
    stack = []
    bodies = []
    item_kw = None
    item_line = 1
    item_pos = 0
    nested = False
    line = 1
    n = len(src)

    def top():
        return stack[-1][0] if stack else CONTAINER

    def keeps_code():
        return nested or top() in (CONTAINER, VERBATIM)

    def check_line_start(pos):
        nonlocal item_kw, item_line, item_pos, nested
        head = LINE_HEAD.match(src, pos)
        quals = QUAL_WORD.findall(head.group(1))
        kw = quals[-1] if quals else None
        if head.group(2) in ITEM_KW and head.group(3) in ("", " ", "\t", "<", "[", "(", "!"):
            kw = head.group(2)
        if top() == CONTAINER and kw in ITEM_KW:
            item_kw, item_line, item_pos = kw, line, pos
        elif top() in (BODY, INNER) and kw in NESTED_KW and not nested:
            item_kw, item_line, item_pos, nested = kw, line, pos, True

    check_line_start(0)
    pos = 0
    while True:
        m = TOKEN.search(src, pos)
        s = m.start() if m else n
        if keeps_code():
            keep[pos:s] = b"\x01" * (s - pos)
        if not m:
            break
        e = m.end()
        kind = m.lastgroup
        if kind == "bc":
            depth = 1
            while e < n and depth:
                if src.startswith("/*", e):
                    depth, e = depth + 1, e + 2
                elif src.startswith("*/", e):
                    depth, e = depth - 1, e + 2
                else:
                    e += 1
        elif kind == "rs":
            close = src.find('"' + m.group("h"), e)
            e = n if close < 0 else close + 1 + len(m.group("h"))
        is_comment = kind in ("lc", "bc")
        if keeps_code() or (is_comment and top() in (BODY, INNER)):
            keep[s:e] = b"\x01" * (e - s)
        c = src[s]
        if kind != "p":
            line += src.count("\n", s, e)
        elif c == "\n":
            line += 1
            check_line_start(e)
        elif c == "{":
            if top() == CONTAINER or nested:
                keep[s] = 1
                if item_kw == "fn" and s not in expand:
                    stack.append((BODY, s, item_line, line))
                elif item_kw in CONTAINER_KW:
                    stack.append((CONTAINER, s, item_line, line))
                else:
                    stack.append((VERBATIM, s, item_line, line))
                item_kw, nested = None, False
            else:
                stack.append((VERBATIM if top() == VERBATIM else INNER, s, line, line))
        elif c == "}":
            closed, open_pos, header_line, open_line = stack.pop() if stack else (CONTAINER, s, line, line)
            if closed != INNER:
                keep[s] = 1
            if closed == BODY:
                bodies.append((open_pos, header_line, open_line, line))
            if top() == CONTAINER:
                item_kw = None
        elif nested or top() == CONTAINER:
            header = src[item_pos:s]
            if header.count("(") + header.count("[") == header.count(")") + header.count("]"):
                item_kw, nested = None, False
        pos = e
    return keep, bodies


def main():
    if len(sys.argv) < 2:
        sys.exit("usage: codemap.py PATH...")

    # Collect .rs and .k1 files under each path
    files = []
    for p in sys.argv[1:]:
        if os.path.isfile(p):
            files.append(p)
            continue
        for dirpath, dirs, names in os.walk(p):
            dirs[:] = sorted(d for d in dirs if d not in SKIP_DIRS and not d.startswith("."))
            for name in sorted(names):
                if name.endswith((".rs", ".k1")):
                    files.append(os.path.join(dirpath, name))

    # Changed lines per file in the working tree vs HEAD; untracked files are wholly changed
    root = subprocess.run(["git", "rev-parse", "--show-toplevel"], capture_output=True, text=True).stdout.strip()
    rel = [os.path.relpath(os.path.abspath(f), root) for f in files] if root else files
    changes = {}
    if root:
        current = None
        diff = subprocess.run(["git", "-C", root, "diff", "-U0", "--no-color", "--no-renames", "HEAD", "--", *rel], capture_output=True, text=True).stdout
        for line in diff.split("\n"):
            if line.startswith("+++ "):
                current = changes.setdefault(line[6:], set()) if line != "+++ /dev/null" else None
            elif line.startswith("@@") and current is not None:
                m = re.match(r"@@ -\S+ \+(\d+)(?:,(\d+))? @@", line)
                start = int(m.group(1))
                current.update(range(start, start + int(m.group(2) or "1")))
        untracked = subprocess.run(["git", "-C", root, "ls-files", "--others", "--exclude-standard", "--", *rel], capture_output=True, text=True).stdout
        for path in untracked.split("\n"):
            if path:
                changes[path] = True

    out = [
        "# codemap: source text; '{…N}' elides a fn body spanning N lines, comments inside it stay.",
        "# fn bodies the working-tree diff vs HEAD touches, and untracked files, are shown in full.",
    ]
    for path, key in zip(files, rel):
        with open(path) as f:
            src = f.read()
        changed = changes.get(key, set())

        # Elide every fn body except those the diff touches
        keep, bodies = scan(src, set())
        expand = set()
        for open_pos, first, _, last in bodies:
            if changed is True or any(ln in changed for ln in range(first, last + 1)):
                expand.add(open_pos)
        if expand:
            keep, bodies = scan(src, expand)
        elided = {}
        for open_pos, _, open_line, last in bodies:
            elided.setdefault(open_line, {})[open_pos] = last - open_line + 1

        # Emit kept text per source line
        out.append(f"=== {path} ({src.count(chr(10)) + 1} lines)")
        base = 0
        for ln, text in enumerate(src.split("\n"), 1):
            seg = keep[base : base + len(text)]
            kept_count = seg.count(1)
            if kept_count == len(text) and ln not in elided:
                kept = text
            elif kept_count == 0:
                kept = ""
            else:
                marks = elided.get(ln, {})
                chars = []
                for k, ch in enumerate(text):
                    if seg[k]:
                        chars.append(ch)
                        if base + k in marks:
                            chars.append(f"…{marks[base + k]}")
                kept = "".join(chars)
            base += len(text) + 1
            kept = kept.strip()
            if not kept:
                continue
            if kept == "}" and re.search(r"\{…\d+$", out[-1]):
                out[-1] += "}"
                continue
            out.append(text[: len(text) - len(text.lstrip())] + kept)

    text = "\n".join(out) + "\n"
    sys.stdout.write(text)
    sys.stderr.write(f"codemap: {len(files)} files, {len(out)} lines, {len(text)} bytes, ~{len(text) // 2150}k tokens via Read\n")


main()
