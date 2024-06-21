#!/usr/bin/env python

import re

with open("2023/02/input", "r") as f:
    input = [x.strip() for x in f.readlines()]

# Part 1

# Which games are possible if the bag contained only 12 red cubes, 13 green 
# cubes, and 14 green cubes?

line = input[0]
def count_cubes(line):
    rxp = re.compile(r'(\d+) ([a-zA-Z]+)(,|;)?')
    results = re.findall(rxp, line)

