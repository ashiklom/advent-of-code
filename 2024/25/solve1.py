#!/usr/bin/env python

from itertools import product

def parse_input(fname):
    # fname = "2024/25/testinput"
    with open(fname, "r") as f:
        raw = f.read().strip()
    raw_blocks = raw.split("\n\n")
    blocks = [b.split("\n") for b in raw_blocks]
    nrow = len(blocks[0])
    ncol = len(blocks[0][0])
    keys = []
    locks = []
    for block in blocks:
        if block[0] == "#" * ncol:
            lock = []
            for c in range(ncol):
                lock.append(sum(block[r][c] == "#" for r in range(1, nrow)))
            locks.append(lock)
        else:
            key = []
            for c in range(ncol):
                key.append(sum(block[r][c] == "#" for r in range(0, nrow-1)))
            keys.append(key)
    return keys, locks

def check(key, lock):
    for K, L in zip(key, lock):
        if K + L > 5:
            return False
    return True

keys, locks = parse_input("2024/25/input")

result = sum(check(key, lock) for key, lock in product(keys, locks))
print(result)
