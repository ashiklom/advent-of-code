#!/usr/bin/env python

from collections import defaultdict
from itertools import chain
from typing import Sequence

def mix_prune(num: int, secret: int):
    # 2^24 == 1 + 24 zero bits
    PRUNE = 16_777_216
    result = num ^ secret
    return result % PRUNE

def next_secret(secret: int) -> int:
    step1 = mix_prune(secret * 64, secret)
    step2 = mix_prune(step1 // 32, step1)
    step3 = mix_prune(step2 * 2048, step2)
    return step3

def secret_list(secret: int, n: int) -> Sequence[tuple[int, int, int|None]]:
    result = [(secret, secret % 10, None)]
    for i in range(1, n):
        secret = next_secret(secret)
        digit = secret % 10
        result += [(secret, digit, digit - result[i-1][1])]
    return result

def get_seqs(slist: Sequence):
    result = defaultdict(list)
    for i in range(4, len(slist)):
        sseq = (tuple(s[2] for s in slist[(i-3):(i+1)]))
        sval = slist[i][1]
        result[sseq].append(sval)
    return result

def get_seq_sum(seq, data):
    total = 0
    for item in data:
        if seq not in item:
            continue
        x = item[seq][0]
        total += x
    return total

with open("2024/22/input") as f:
    raw = f.read().splitlines()

input = list(map(int, raw))
seclist = [secret_list(i, 2000) for i in input]
seqs = [get_seqs(val) for val in seclist]
all_seqs = set(chain.from_iterable(s.keys() for s in seqs))
results = []
for i, seq in enumerate(all_seqs):
    print(f"{i} / {len(all_seqs)}")
    results += [(get_seq_sum(seq, seqs), seq)]
results = sorted(results)
print(results[-1])
