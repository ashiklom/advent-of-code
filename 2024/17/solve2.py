#!/usr/bin/env python

from typing import Callable


class Program:
    def __init__(self, A: int, B: int, C: int, program: list[int]) -> None:
        self.A = A
        self.B = B
        self.C = C
        self.program = program
        self.n_program = len(self.program)
        self.instruction_ptr = 0
        self.output = []

        pass

    def combo(self, op: int) -> int:
        if op <= 3:
            return op
        if op == 4:
            return self.A
        if op == 5:
            return self.B
        if op == 6:
            return self.C
        raise ValueError(f"Invalid op {op}")

    def run_instruction(self) -> None:
        opcode = self.program[self.instruction_ptr]
        op = self.program[self.instruction_ptr + 1]
        self.instruction(opcode, op)

    def execute(self) -> None:
        while self.instruction_ptr < self.n_program:
            self.run_instruction()

    def instruction(self, opcode: int, op: int) -> None:
        fnlist = {
            0: self.adv,
            1: self.bxl,
            2: self.bst,
            3: self.jnz,
            4: self.bxc,
            5: self.out,
            6: self.bdv,
            7: self.cdv,
        }
        fnlist[opcode](op)

    # Opcode 0
    def adv(self, op: int) -> None:
        self.A = int(self.A / (2 ** self.combo(op)))
        self.instruction_ptr += 2

    # Opcode 1
    def bxl(self, op: int):
        self.B = self.B ^ op
        self.instruction_ptr += 2

    # Opcode 2
    # Does not affect the output 
    # (because I only write the last 3 bits anyway)
    def bst(self, op: int) -> None:
        self.B = self.combo(op) % 8
        self.instruction_ptr += 2

    # Opcode 3
    def jnz(self, op: int) -> None:
        if self.A == 0:
            self.instruction_ptr += 2
            return
        self.instruction_ptr = op

    # Opcode 4
    # Only opcode that affects the output
    def bxc(self, _) -> None:
        self.B = self.B ^ self.C
        self.instruction_ptr += 2

    # Opcode 5
    def out(self, op: int) -> None:
        self.output.append(self.combo(op) % 8)
        self.instruction_ptr += 2

    # Opcode 6
    # This can be ignored -- never appears in the program
    def bdv(self, op: int) -> None:
        self.B = int(self.A / (2 ** self.combo(op)))
        self.instruction_ptr += 2

    # Opcode 7
    # Values of C and A should be the same every loop.
    def cdv(self, op: int) -> None:
        self.C = int(self.A / (2 ** self.combo(op)))
        self.instruction_ptr += 2


def test(A: int, B: int, C: int, program: list[int], check: Callable):
    t = Program(A, B, C, program)
    t.execute()
    if not check(t):
        raise ValueError("Failed test")


# Do the puzzle
with open("2024/17/input", "r") as f:
    raw = f.read().splitlines()

A = int(raw[0].split(":")[1])
B = int(raw[1].split(":")[1])
C = int(raw[2].split(":")[1])
program = list(map(int, raw[4].split(":")[1].split(",")))

# Prog: [2,4, 1,1, 7,5, 4,6, 1,4 ,0,3, 5,5, 3,0]
# Out: 4,6,1,4,2,1,3,1,6

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

p = Program((8**15)+1, B, C, program)
p.execute()
print(p.program)
print(p.output)

# Instructions that involve register A

# puzzle = Program(A, B, C, program)
# puzzle.execute()
# print(puzzle.output)
#
# result = ",".join(map(str, puzzle.output))
# print(result)
