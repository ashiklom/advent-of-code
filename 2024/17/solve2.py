#!/usr/bin/env python

from itertools import product

# Do the puzzle
with open("2024/17/input", "r") as f:
    raw = f.read().splitlines()

program = list(map(int, raw[4].split(":")[1].split(",")))

# [2,4, 1,1, 7,5, 4,6, 1,4, 0,3, 5,5, 3,0]
def prog(A):
    """
    Complete program; literal interpretation of instructions.
    """
    B = A % 8   # 2,4 -- combo(4) % 8
    B = B ^ 1   # 1,1 -- B ^ 1
    C = A >> B  # 7,5 -- A // 2**combo(5)
    B = B ^ C   # 4,6 -- B ^ C (6 ignored)
    B = B ^ 4   # 1,4 -- B ^ 4
    A = A >> 3  # 0,3 -- (A // 2**combo(3))
    out = B % 8 # 5,5 -- print(combo(5))
    return A, out   # 3,0 -- Loop

def run_prog(A):
    result = []
    while A != 0:
        A, out = prog(A)
        result.append(out)
    return result

if run_prog(59397658) != [4,6,1,4,2,1,3,1,6]:
    raise ValueError("Bug in program")

def expand_end(slist: list[int], target: int):
    result = []
    for s in slist:
        s2 = s << 3
        checks = [s2 + i for i in range(8)]
        out = [c for c in checks if prog(c)[1] == target]
        result.extend(out)
    return result

solve = [0]
prog_sub = program.copy()
result = []
while prog_sub:
    p = prog_sub.pop()
    solve = expand_end(solve, p)
    result.append((p, solve))

answer = min(solve)
if run_prog(answer) != program:
    raise ValueError("Results are not equal")

print(answer)
