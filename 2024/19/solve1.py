#!/usr/bin/env python

import re

def parse_input(fname: str):
    with open(fname, "r") as f:
        raw = f.read().splitlines()
    patterns = [s.strip() for s in raw[0].split(",")]
    designs = raw[2:]
    return (patterns, designs)

def check_design(design, patterns):
    has_pat = [pat for pat in patterns if pat in design]
    rxp = re.compile(f"^({'|'.join(has_pat)})+$")
    return rxp.match(design)

all_patterns, designs = parse_input("2024/19/input")

# patsub = all_patterns.copy()
# pat = set()
# for i in range(1, 5):
#     cp = {p for p in all_patterns if len(p) == 1}
#     rx = re.compile(f"^({'|'.join(pat | cp)})")
#     patsub = [p for p in patsub if not rx.match(p)]

# Patterns that are combinations of other (shorter) patterns
# don't need to be checked.
c1p = {p for p in all_patterns if len(p) == 1}
# 1-character -- just use set logic
patsub = [p for p in all_patterns if (set(p) - c1p)]

# 2-character patterns; use regex
c2p = {p for p in patsub if len(p) == 2}
c2prx = re.compile(f"^({'|'.join(c1p | c2p)})+$")
patsub = [p for p in patsub if not c2prx.match(p)]

# 3-character patterns; use regex
c3p = {p for p in patsub if len(p) == 3}
c3prx = re.compile(f"^({'|'.join(c1p | c2p | c3p)})+$")
patsub = [p for p in patsub if not c3prx.match(p)]

# 4-character patterns
c4p = {p for p in patsub if len(p) == 4}
c4prx = re.compile(f"^({'|'.join(c1p | c2p | c3p | c4p)})+$")
patsub = {p for p in patsub if not c4prx.match(p)}

patterns = c1p | c2p | c3p | c4p | patsub

# Filter 1-letter designs quickly
d1 = [d for d in designs if not check_design(d, c1p)]
print(len(d1))
d2 = [d for d in d1 if not check_design(d, c1p | c2p)]
print(len(d2))
d3 = [d for d in d2 if not check_design(d, c1p | c2p | c3p)]
print(len(d3))
d4 = [d for d in d3 if not check_design(d, c1p | c2p | c3p | c4p)]
print(len(d4))

print(len(designs) - len(d4))
