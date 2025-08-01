#!/usr/bin/env python

from math import log2

# Do the puzzle
with open("2024/17/input", "r") as f:
    raw = f.read().splitlines()

program = list(map(int, raw[4].split(":")[1].split(",")))

def prog(A):
    B = A % 8
    B = B ^ 1
    C = A // (2**B)
    B = B ^ C
    B = B ^ A
    A = A // 8
    out = B % 8
    return A, out

def run_prog(A):
    result = []
    while A != 0:
        A, out = prog(A)
        result.append(out)
    return result

def get_solutions(A: int) -> list[int]:
    solutions = []
    for i in range(2**4):
        _, x = prog(i)
        if x == A:
            solutions.append(i)
    return solutions

def expand_solutions(slist: list[int], target: int):
    result = []
    for s in slist:
        s2 = s << 3
        checks = [s2 + i for i in range(8)]
        out = [c for c in checks if prog(c)[1] == target]
        result.extend(out)
    return result

# Get the solution for the second to last number (the last is zero)
solve = get_solutions(program[-2])
prog_sub = program[:-2]
result = [(program[-2], solve)]
while prog_sub:
    p = prog_sub.pop()
    solve = expand_solutions(solve, p)
    result.append((p, solve))

print(solve)

# Get the solution for the first number
solve_1 = get_solutions(program[0])

expand_solutions(solve_1[0])

check2 = []
[check2.extend(expand_solutions(s)) for s in solve_1]
check2 = set(check2)

solutions = {p: get_solutions(p) for p in set(program)}
solve_list = [solutions[k] for k in program]

# def shift(s: list[int], i: int):
#     return [x << i for x in s] 
#
# solve_shifted = [shift(s, i) for i, s in enumerate(solve_list)]

# Now, whittle down the solutions
def compare_solutions(a, b):
    ra = []
    rb = []
    for aa in a:
        for bb in b:
            abits = bin(aa >> 3)[2:]
            bbits = bin(bb)[5:]
            if abits == bbits:
                ra.append(aa)
                rb.append(bb)
    return ra, rb

{prog(a)[1] for a in solve_list[1]}
[bin(s) for s in solve_list[0]]

a0, b1 = compare_solutions(solve_list[0], solve_list[1])
a1, b2 = compare_solutions(b1, solve_list[2])
a2, b3 = compare_solutions(b2, solve_list[3])

solutions = []
for i in range(1024):
    _, x = prog(i)
    if x == 3:
        solutions.append(i)

[bin(s) for s in solutions]

prog(int(0b01_1_010_101_010))
prog(int(0b11_1_010_101_010))

# def prog(A):
#     # A = abc_def_ghi_yyy
#     # 2,4 -- B = combo % 8
#     # B = yyy
#     B = A % 8
#     # 1,1 -- B = B ^ op
#     # B = yyy^1
#     B = B ^ 1
#     # 7,5 -- C = int(A / 2**combo)
#     # C = 000_00a_bcd_efg (suppose yyy^1=5)
#     C = int(A / (2**B))
#     # 4,6 -- B = B ^ C
#     # B = (000_000_000_yyy ^ 000_00a_bcd_efg)
#     # B = (0_000_yyy ^ a_bcd_efg)
#     # B = (a_bcd_(yyy^efg))
#     #   B = (yyy^efg)
#     B = B ^ C
#     # 1,4 -- B ^ op
#     # B = (a_bcd_???) ^ (0_000_100)
#     # B = (a_bcd_???) ^ (0_000_100)
#     B = B ^ 4
#     # 0,3
#     A = int(A / 8)
#     # 5,5
#     # out = Last 3 bits of A
#     out = B % 8
#     # 3,0 -- reset
#     return A, out

# Take the last 3 bits of A
# Do XOR with 1
# Shift A by ^^ many bits and grab the last 3 bits
# Take the XOR of last 3 bits of A, result of ^^, and 4.
# That's the output.

# In reverse:
# a1 = output ^ 4
# a1 == last 3 bits of A ^ bitshifted bits of A

# A = xxx_xxx_xxx_yyy
# A/2**

# X ^ 4 ^ (A/2**(A%8 ^ 1))
# (A >> (yyy ^ 1)) ^ 4 ^ X

A = 59397658
n = int(log2(A) / 3) + 1
for _ in range(n):
    A = prog(A)

A = int("100_000_000_110", 2)
while A > 0:
    # print(f"{i}, {prog(i)}")
    print(f"{A} = {bin(A)}", end=" --> ")
    A, out = prog(A)
    print(f"{out} = {bin(out)}")

# Prog: [2,4, 1,1, 7,5, 4,6, 1,4, | 0,3, 5,5, 3,0]
# Out: 4,6,1,4,2,1,3,1,6

# Sequence
# A = x*_yyy
# 4 ^ 1 ^ x* = 0*_yyy ^ x*_yyy
# 4 ^ 1 ^ x* = x*_000
# 4 ^ 1 = x*_000 ^ 000_x*
# 4 ^ 1 = 000_?+_000
# 4 ^ 1 ^ xxx_xxx = 000_000_000
# 4 ^ 1 = 000_000_000 ^ 000_xxx_xxx

# 4 ^ 1 ^ (A/8) = (A % 8) ^ A
# X = 0*_yyy ^ 1 ^ 000_x* ^ x*_yyy

# 6 = yyy ^ 1 ^ yyy


# Must be odd (because )

#   - 2,4 -- B = A % 8 (last 3 bits of A value)
#   - 1,1 -- B = B ^ 1 (B XOR 001)
#   - 7,5 -- C = A / 2**B
#   - 4,6 -- B = B ^ C
#   - 1,4 -- B = B ^ A
#   - 0,3 -- A = A / 8 (trim the least 3 bits of A)
#   - 5,5 -- Write contents of B
#   - 3,0 -- Repeat

# Observations:
# I always jump at the end of the program to the beginning
# unless the value of register A is 0, in which case the program ends.
#
# The only action that modifies register A is opcode 0 (adv),
# which shrinks it. In my program, that always happens with op 3.
# That's a literal 3, so every program pass,
# A = floor(A / 8) (b/c 8 = 2^3).
# 
# Each pass, the program writes output once.
# The final length of my program is 16.
# That means I need 16 passes through the program.
# So, 8**15 < A < 8**16 (because on the last time, A has to be zero).
#
# I always write B (combo op 5) % 8; i.e., the first 3 bits of B.
# Higher-order bits of B don't matter.
#
# Values of C and A should be the same every loop.
# Only the last 3 bits of B matter.
# So, I just need to figure out the combination of
# reversing bitwise XOR operations at each iteration
# to give me the values I need.
#
# Dividing by 8 is the same as dropping the 3 least significant bits.
# So dividing by 8**3 drops the 9 least significant bits.
# So, I just need to craft the XOR pattern, 3 bits at a time, to produce
# the values in the program.

# p = Program((8**15)+1, B, C, program)
# p.execute()
# print(p.program)
# print(p.output)
