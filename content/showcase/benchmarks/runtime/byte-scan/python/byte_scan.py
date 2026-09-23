import re

SIZE = 256 * 1024 * 1024
REPEAT = 8
NEWLINE_STRIDE = 4093
DELIM_STRIDE = 1_000_003
MASK = (1 << 64) - 1
DELIMS = re.compile(rb'[,:"]')


def fill(buf):
    words = memoryview(buf).cast("Q")
    x = 0x2545F4914F6CDD1D
    for i in range(len(words)):
        x ^= (x << 13) & MASK
        x ^= x >> 7
        x ^= (x << 17) & MASK
        words[i] = (x & 0x1F1F1F1F1F1F1F1F) | 0x4040404040404040
    for i in range(NEWLINE_STRIDE, len(buf), NEWLINE_STRIDE):
        buf[i] = ord("\n")
    marks = b',:"'
    for which, d in enumerate(range(DELIM_STRIDE, len(buf), DELIM_STRIDE)):
        buf[d] = marks[which % 3]


def count_newlines(buf):
    count = 0
    p = buf.find(b"\n")
    while p >= 0:
        count += 1
        p = buf.find(b"\n", p + 1)
    return count


def main():
    buf = bytearray(SIZE)
    fill(buf)
    absent = newlines = delims = index_sum = which_sum = 0
    for _ in range(REPEAT):
        if 0 in buf:
            absent += 1
        newlines += count_newlines(buf)
        for m in DELIMS.finditer(buf):
            delims += 1
            index_sum += m.start()
            which_sum += b',:"'.index(m[0])
    print(f"absent: {absent} newlines: {newlines} delims: {delims} index-sum: {index_sum} which-sum: {which_sum}")


main()
