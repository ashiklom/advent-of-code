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

    def adv(self, op: int) -> None:     # opcode 0
        self.A = int(self.A / (2 ** self.combo(op)))
        self.instruction_ptr += 2

    def bxl(self, op: int):             # opcode 1
        self.B = self.B ^ op
        self.instruction_ptr += 2

    def bst(self, op: int) -> None:     # opcode 2 
        self.B = self.combo(op) % 8
        self.instruction_ptr += 2

    def jnz(self, op: int) -> None:     # opcode 3
        if self.A == 0:
            self.instruction_ptr += 2
            return
        self.instruction_ptr = op

    def bxc(self, _) -> None:           # opcode 4
        self.B = self.B ^ self.C
        self.instruction_ptr += 2

    def out(self, op: int) -> None:     # opcode 5
        self.output.append(self.combo(op) % 8)
        self.instruction_ptr += 2

    def bdv(self, op: int) -> None:     # opcode 6
        self.B = self.A // (2 ** self.combo(op))
        self.instruction_ptr += 2

    def cdv(self, op: int) -> None:     # opcode 7
        self.C = self.A // (2 ** self.combo(op))
        self.instruction_ptr += 2


def test(A: int, B: int, C: int, program: list[int], check: Callable):
    t = Program(A, B, C, program)
    t.execute()
    if not check(t):
        raise ValueError("Failed test")


# Basic tests
test(0, 0, 9, [2, 6], lambda x: x.B == 1)
test(10, 0, 0, [5, 0, 5, 1, 5, 4], lambda x: x.output == [0, 1, 2])
test(
    2024,
    0,
    0,
    [0, 1, 5, 4, 3, 0],
    lambda x: x.output == [4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0] and x.A == 0,
)
test(0, 29, 0, [1, 7], lambda x: x.B == 26)
test(0, 2024, 43690, [4, 0], lambda x: x.B == 44354)

# Test input
test(
    729, 0, 0, [0, 1, 5, 4, 3, 0], lambda x: x.output == [4, 6, 3, 5, 6, 3, 5, 2, 1, 0]
)

# Do the puzzle
with open("2024/17/input", "r") as f:
    raw = f.read().splitlines()

A = int(raw[0].split(":")[1])
B = int(raw[1].split(":")[1])
C = int(raw[2].split(":")[1])
program = list(map(int, raw[4].split(":")[1].split(",")))

puzzle = Program(A, B, C, program)
puzzle.execute()
print(puzzle.output)

result = ",".join(map(str, puzzle.output))
print(result)
