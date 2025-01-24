"""AutoPerft"""
# Copyright (c) 2024 Soham Korade
#------------------------------------------------------------------------------
# Modify this function to customize the checker for your engine


def customize(teacher, student):
    # customize strings
    if student.get_name() == "Chess by Soham":
        student.PERFT_CMD = "perft {depth}"
        student.NODE_COUNT_REGEX = r"Nodes: (\d+)"


#------------------------------------------------------------------------------

import subprocess
import re
import time


class ChessEngine:

    def __init__(self, path):
        self.path = path
        self.output = []

        # regex patterns
        self.DIVIDE_REGEX = r"([a-h][1-8][a-h][1-8][qrbn]?): (\d+)"
        self.NODE_COUNT_REGEX = r"Nodes searched: (\d+)"
        self.UCI_NAME_REGEX = r"id name (.+)"

        # command strings
        self.SETUP_CMD = ""
        self.UCI_CMD = "uci"
        self.PERFT_CMD = "go perft {depth}"
        self.POSITION_STARTPOS_CMD = "position startpos"
        self.POSITION_FEN_CMD = "position fen {fen}"
        self.POSITION_FEN_MOVES_CMD = "position fen {fen} moves {move_list}"

    def run(self, cmd):
        engine = subprocess.Popen(
            self.path,
            universal_newlines=True,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
        )
        cmd = self.SETUP_CMD + "\n" + cmd

        t0 = time.time()
        self.output = engine.communicate(cmd)[0].rstrip().split("\n")
        t1 = time.time()
        return t1 - t0

    def get_name(self):
        self.run(self.UCI_CMD)
        for line in self.output:
            name = re.search(self.UCI_NAME_REGEX, line)
            if name:
                return name.group(1)
        return ""

    def get_perft(self, depth, fen="", moves=[]):
        if fen == "":
            cmd = self.POSITION_STARTPOS_CMD
        else:
            cmd = self.POSITION_FEN_CMD.format(fen=fen)
        if len(moves) > 0:
            move_list = " ".join(moves)
            cmd = self.POSITION_FEN_MOVES_CMD.format(fen=fen,
                                                     move_list=move_list)
        cmd += "\n" + self.PERFT_CMD.format(depth=depth)
        time_taken = self.run(cmd)
        for line in self.output:
            nodes_count = re.search(self.NODE_COUNT_REGEX, line)
            if nodes_count:
                return int(nodes_count.group(1)), time_taken
        raise Exception(
            "Unable to parse nodes count. Check if NODE_COUNT_REGEX is set properly."
        )

    def parse_divide(self):
        divide = {}
        for line in self.output:
            divide_match = re.search(self.DIVIDE_REGEX, line)
            if divide_match:
                divide[divide_match.group(1)] = int(divide_match.group(2))
        return divide


def red(text):
    return f"\033[91m{text}\033[0m"


def green(text):
    return f"\033[92m{text}\033[0m"


def bisect(teacher, student, depth=1, fen=""):
    print("  Bisecting...")

    moves = []
    for depth in range(depth, 0, -1):
        teacher.get_perft(depth, fen, moves)
        student.get_perft(depth, fen, moves)
        teacher_moves = teacher.parse_divide()
        student_moves = student.parse_divide()

        # lowest difference greater than 0
        min_diff = float("inf")
        faulty_moves = 0
        move = None
        for key in teacher_moves:
            if key not in student_moves:
                print(red(f"  - {key} missing"))
                continue
            diff = abs(teacher_moves[key] - student_moves[key])
            if diff > 0 and diff < min_diff:
                min_diff = diff
                move = key
                faulty_moves += 1
        for key in student_moves:
            if key not in teacher_moves:
                print(red(f"  + {key} extra"))

        if move:
            moves.append(move)
            print(
                f"{'  '*len(moves)} {move} ({red(student_moves[move])} != {green(teacher_moves[move])}) out of {faulty_moves} faulty"
            )

    print(
        f"  lichess: https://lichess.org/analysis/{fen.replace(' ','_')} {' '.join(moves)}"
    )
    exit(1)


import argparse

parser = argparse.ArgumentParser(description="Perft checker")
parser.add_argument("teacher_path", help="Teacher engine path")
parser.add_argument("student_path", help="Student engine path")
epd_or_fen = parser.add_mutually_exclusive_group(required=True)
epd_or_fen.add_argument("--fen", help="FEN string")
epd_or_fen.add_argument("--epd", help="EPD path")
parser.add_argument("--max_depth", type=int, default=6, help="Max depth")
args = parser.parse_args()

# load engines
teacher = ChessEngine(args.teacher_path)
print(f"Teacher '{teacher.get_name()}' loaded")

student = ChessEngine(args.student_path)
print(f"Student '{student.get_name()}' loaded")

customize(teacher, student)

# load fen/epd file
if args.fen:
    epd_list = [args.fen]
else:
    with open(args.epd) as f:
        epd_list = [i.rstrip("\n") for i in f.readlines()]
n_tests = len(epd_list)
print(f"{n_tests} tests found")

# run tests
total_student_time = 0
for i in range(len(epd_list)):
    print(f"Test {i+1}/{n_tests}")
    fen, *known_depths = epd_list[i].split(" ;")
    print(" FEN:", fen)
    if args.epd:
        known_depths = [
            int(i.split(" ")[1]) for i in known_depths
            if re.match(r"D\d+ \d+", i)
        ]
        max_depth = min(args.max_depth, len(known_depths))
    if args.fen or len(known_depths) == 0:
        max_depth = args.max_depth
    for depth in range(1, 1 + max_depth):
        if args.epd and depth <= len(known_depths):
            teacher_nodes = known_depths[depth - 1]
        else:
            teacher_nodes, _ = teacher.get_perft(depth, fen)
        student_nodes, student_time_taken = student.get_perft(depth, fen)
        total_student_time += student_time_taken
        print(f" Depth {depth} {student_nodes:15}", end="")
        if teacher_nodes == student_nodes:
            print(f" {green('PASS')}, {student_time_taken*1000:.2f} ms")
        else:
            print(f" {red('FAIL')}, expected {teacher_nodes}")
            bisect(teacher, student, depth, fen)

print(green("All tests passed!"))
print(f"took {total_student_time:.2f} s")
