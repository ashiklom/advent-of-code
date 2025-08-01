#!/usr/bin/env python

import re

def parse_input(fname: str):
    with open(fname, "r") as f:
        raw = f.read().splitlines()
    patterns = [s.strip() for s in raw[0].split(",")]
    designs = raw[2:]
    return (patterns, designs)

def check_design(design, patterns):
    print(design)
    has_pat = [pat for pat in patterns if pat in design]
    rxp = re.compile(f"^({'|'.join(has_pat)})+$")
    return rxp.match(design)

all_patterns, designs = parse_input("2024/19/input")

# Patterns combining multiple single-letter patterns
# don't need to be checked.
c1pat = {p for p in all_patterns if len(p) == 1}
patterns = list(c1pat) + [p for p in all_patterns if (set(p) - c1pat)]
# design = designs[1]

result = [check_design(d, patterns) for d in designs]

subset = [r for r in result if r]
print(len(subset))
