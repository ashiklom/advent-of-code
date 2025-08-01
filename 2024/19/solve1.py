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

all_patterns, all_designs = parse_input("2024/19/input")

bad_designs = all_designs.copy()
patsub = all_patterns.copy()
pat = set()
for i in range(1, 5):
    pat |= {p for p in patsub if len(p) == i}
    bad_designs = [d for d in bad_designs if not check_design(d, pat)]
    rx = re.compile(f"^({'|'.join(pat)})+$")
    patsub = [p for p in patsub if not rx.match(p)]

print(f"{len(bad_designs)} designs can't be made.")
print(f"{len(all_designs) - len(bad_designs)} are possible.")
