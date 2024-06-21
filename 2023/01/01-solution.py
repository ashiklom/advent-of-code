#!/usr/bin/env python

import re

with open("input", "r") as f:
    input = f.readlines()

# Part 1:

# Combine the first digit and last digit to form a two-digit number
# In some cases, the _only_ number is both the first and last number.
def part1(input):
    firstnum_rxp = re.compile(r"^.*?(\d).*$")
    firstnums = [re.match(firstnum_rxp, x).groups()[0] for x in input]
    lastnum_rxp = re.compile(r"^.*(\d).*?$")
    lastnums = [re.match(lastnum_rxp, x).groups()[0] for x in input]
    numbers = [int(''.join(pair)) for pair in zip(firstnums, lastnums)]
    return sum(numbers)

part1(input)

# Part 2:
# Some of the numbers are spelled out (e.g., one, two, three). First, replace 
# all of these with the corresponding number; then, redo.
def replace_digit(s: str):
    numstrings = {
        "one": "1",
        "two": "2",
        "three": "3",
        "four": "4",
        "five": "5",
        "six": "6",
        "seven": "7",
        "eight": "8",
        "nine": "9"
    }
    result = s
    for key, value in numstrings.items():
        result = result.replace(key, value)
    return result

re.sub(r'[^0-9]', '', "string123")
"string123".replace(re.compile('^(\d)'), '')

str.maketrans({"xx": "doublex"})
"12xx34".translate(str.maketrans({"xx": "xx"}))

input[0]
input_fixed[0]
part1(input[0:1])
part1(input_fixed[0:1])
input_fixed = [replace_digit(x) for x in input]
part1(input_fixed)
