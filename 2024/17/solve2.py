#!/usr/bin/env python

from math import log2

# Do the puzzle
with open("2024/17/input", "r") as f:
    raw = f.read().splitlines()

program = list(map(int, raw[4].split(":")[1].split(",")))

def prog(A):
    # 2,4 -- B = combo % 8
    B = A % 8
    # 1,1 -- B = B ^ op
    B = B ^ 1
    # 7,5 -- C = int(A / 2**combo)
    C = int(A / (2**B))
    # 4,6 -- B = B ^ C
    B = B ^ C
    # 1,4 -- B ^ op
    B = B ^ 4
    # 0,3
    A = int(A / 8)
    # 5,5
    print(B % 8)
    # 3,0 -- reset
    return A

A = 59397658
n = int(log2(A) / 3) + 1
for _ in range(n):
    A = prog(A)

for i in range(30):
    # print(f"{i}, {prog(i)}")
    a, b = prog(i)
    print(f"{i}, {bin(i)} => {bin(a), bin(b)}")

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
