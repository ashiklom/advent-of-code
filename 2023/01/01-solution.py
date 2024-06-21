#!/usr/bin/env python

import re

with open("input", "r") as f:
    input = f.readlines()

# Part 1:

# Combine the first digit and last digit to form a two-digit number
# In some cases, the _only_ number is both the first and last number.
def part1(input):
    # Remove all non-number characters
    nums = [re.sub(r'\D', '', x) for x in input]
    # Combine the first and last numbers (note that sometimes, these are the same)
    numbers = [int(x[0] + x[-1]) for x in nums]
    return sum(numbers)

part1(input)

# Part 2:
# Some of the numbers are spelled out (e.g., one, two, three). First, replace 
# all of these with the corresponding number; then, redo.
# NOTE: A challenge here is that you can sometimes have strings right next to 
# each other; e.g., `oneight` needs to be converted to 18 to be parsed correctly.

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

def get_number(s: str, numstrings):
    rxp = r"(\d|" + '|'.join(numstrings.keys()) + r")"
    result = re.findall(rxp, s)[0]
    for key, value in numstrings.items():
        result = result.replace(key, value)
    return result

def first_number(s):
    return get_number(s, numstrings)

def last_number(s):
    numstrings_rev = {key[::-1]: value for key, value in numstrings.items()}
    return get_number(s[::-1], numstrings_rev)

def code(s):
    return int(first_number(s) + last_number(s))

numbers_part2 = (code(x) for x in input)
# Part 2 solution:
sum(numbers_part2)
