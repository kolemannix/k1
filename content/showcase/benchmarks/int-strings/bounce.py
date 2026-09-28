#!/usr/bin/env python3
import subprocess
import tempfile
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

from chart import HERE, ROWS

LANES = [
    ("k1 spill-string[22]", "spill-list as hand-rolled union", "k1 spill-string"),
    ("k1 show()", "two digits per step", "k1 show()"),
    ("C++ std::to_string", "clang -O3, this machine", "C++"),
    ("Go strconv.Itoa", "Lemire's M4 Max", "Go"),
    ("Bun String(i)", "Lemire's M4 Max", "Bun"),
    ("Rust to_string()", "Lemire's M4 Max", "Rust"),
    ("Python str(i)", "Lemire's M4 Max", "Python"),
]

S = 2
W, H = 1280, 720
FPS = 50
RUN_SECONDS = 10
HOLD_SECONDS = 1.5
LABEL_RIGHT, TRACK_LEFT, TRACK_RIGHT = 250, 280, 1170
TOP, LANE, TRACK_H, RADIUS = 126, 76, 40, 15
TRAIL = 4

BG = (14, 15, 18)
TRACK = (29, 31, 36)
TEXT = (242, 242, 240)
MUTED = (137, 135, 129)
BALL = {"k1": (59, 139, 255), "cpp": (195, 194, 183), "lemire": (120, 119, 114)}


def font(size, weight="Regular", italic=False):
    f = ImageFont.truetype("/System/Library/Fonts/SFNSItalic.ttf" if italic else "/System/Library/Fonts/SFNS.ttf", size * S)
    f.set_variation_by_name(f"{weight} Italic" if italic else weight)
    return f


def lanes():
    values = {}
    for kind, name, detail, value in ROWS:
        values[(name, detail)] = (kind, value)
    out = []
    for name, detail, label in LANES:
        kind, value = values[(name, detail)]
        out.append((kind, label, value))
    return out


def background(rows):
    img = Image.new("RGB", (W * S, H * S), BG)
    d = ImageDraw.Draw(img)
    d.text((40 * S, 30 * S), "Integer to string, million strings per second", font=font(30, "Bold"), fill=TEXT)
    d.text((40 * S, 72 * S), "buf[i & 1023] = to_string(i), 100M integers. Faster ball, more strings.",
           font=font(17), fill=MUTED)
    for i, (kind, label, value) in enumerate(rows):
        mid = TOP + i * LANE + TRACK_H / 2
        muted = kind == "lemire"
        d.text((LABEL_RIGHT * S, (mid - 4) * S), label, anchor="rs",
               font=font(21, "Semibold", italic=muted), fill=MUTED if muted else TEXT)
        d.text((LABEL_RIGHT * S, (mid + 16) * S), f"{value:.1f} M/s", anchor="rs",
               font=font(15, italic=muted), fill=MUTED)
        y = TOP + i * LANE
        d.rounded_rectangle((TRACK_LEFT * S, y * S, TRACK_RIGHT * S, (y + TRACK_H) * S),
                            radius=TRACK_H * S // 2, fill=TRACK)
    d.text((40 * S, (H - 26) * S),
           "Go, Bun, Rust, Python: Lemire's numbers from an M4 Max, a ~13% faster machine. "
           "C++ and k1: Apple M3 Max, best of 5.",
           font=font(14), fill=MUTED)
    return img


def ball(color, alpha):
    r = RADIUS * S
    img = Image.new("RGBA", (2 * r + 2, 2 * r + 2), (0, 0, 0, 0))
    ImageDraw.Draw(img).ellipse((1, 1, 2 * r, 2 * r), fill=color + (alpha,))
    return img


def position(speed, t):
    span = TRACK_RIGHT - TRACK_LEFT - TRACK_H
    trips = speed * t
    phase = trips % 2
    x = phase if phase <= 1 else 2 - phase
    return TRACK_LEFT + TRACK_H / 2 + x * span, int(trips)


def draw_frames(rows, frames):
    slowest = min(value for _, _, value in rows)
    bg = background(rows)
    sprites = {}
    for kind, color in BALL.items():
        sprites[kind] = [ball(color, 255 * (TRAIL - k) // (TRAIL + 1)) for k in range(TRAIL + 1)]
    counter = font(18, "Semibold")
    run_frames = RUN_SECONDS * FPS
    for n in range(run_frames + int(HOLD_SECONDS * FPS)):
        t = min(n, run_frames) / FPS
        img = bg.copy()
        d = ImageDraw.Draw(img)
        for i, (kind, label, value) in enumerate(rows):
            speed = value / slowest / RUN_SECONDS
            cy = TOP + i * LANE + TRACK_H / 2
            for k in range(TRAIL, -1, -1):
                if n > run_frames and k > 0:
                    continue
                x, _ = position(speed, max(0.0, t - k / FPS))
                sprite = sprites[kind][k]
                img.paste(sprite, (int(x * S) - sprite.width // 2, int(cy * S) - sprite.height // 2), sprite)
            _, trips = position(speed, t)
            d.text(((TRACK_RIGHT + 14) * S, cy * S), f"×{trips}", anchor="lm", font=counter,
                   fill=TEXT if kind == "k1" else MUTED)
        img.resize((W, H), Image.LANCZOS).save(frames / f"{n:05d}.png")


def encode(frames):
    src = ["-framerate", str(FPS), "-i", str(frames / "%05d.png")]
    subprocess.run(["ffmpeg", "-y", "-loglevel", "error", *src, "-c:v", "libx264", "-pix_fmt", "yuv420p",
                    "-crf", "18", str(HERE / "bounce.mp4")], check=True)
    subprocess.run(["ffmpeg", "-y", "-loglevel", "error", *src, "-vf",
                    "split[a][b];[a]palettegen=max_colors=64:stats_mode=diff[p];[b][p]paletteuse=dither=none",
                    str(HERE / "bounce.gif")], check=True)


def main():
    with tempfile.TemporaryDirectory() as tmp:
        draw_frames(lanes(), Path(tmp))
        encode(Path(tmp))


main()
