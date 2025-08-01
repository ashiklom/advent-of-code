#!/usr/bin/env python

import re

def parse_input(fname: str):
    with open(fname, "r") as f:
        raw = f.read().splitlines()
    patterns = [s.strip() for s in raw[0].split(",")]
    designs = raw[2:]
    return (patterns, designs)

def solve(input: str):
    patterns, designs = parse_input(input)
    rxp = re.compile(f"^({'|'.join(patterns)})+$")
    results = []
    for i, d in enumerate(designs):
        print(f"{i}/{len(designs)}")
        if rxp.match(d):
            results.append(d)
    return results

test = solve("2024/19/testinput")
if len(test) != 6:
    raise ValueError(f"Test failed. Expected 6, got {len(test)}")

challenge = solve("2024/19/input")
print(len(challenge))
