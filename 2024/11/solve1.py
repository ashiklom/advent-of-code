#!/usr/bin/env python

# raw = ["125", "17"]
with open("2024/11/input", "r") as f:
    raw = f.read().strip().split()

def rule(stone) -> list[str]:
    if stone == "0":
        return ["1"]
    if len(stone) % 2 == 0:
        half = len(stone) // 2
        return [str(int(stone[:half])), str(int(stone[half:]))]
    return [str(int(stone)*2024)]

def blink(stones: list[str]):
    result = []
    for stone in stones:
        result += rule(stone)
    return result

stones = raw
for _ in range(25):
    stones = blink(stones)

print(len(stones))
# 222461
