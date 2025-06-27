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

# Each stone's trajectory is strictly deterministic.
# Don't have to apply rules to each stone,
# only each unique stone.
def blink(stones: dict[str, int]) -> dict[str, int]:
    output = dict()
    for stone, count in stones.items():
        result = rule(stone)
        for item in result:
            if item in output:
                output[item] += count
            else:
                output[item] = count
    return output


stones = {x: raw.count(x) for x in set(raw)}

for i in range(75):
    stones = blink(stones)

print(sum(stones.values()))
