#!/usr/bin/env python

def score(s: str, pat, scores=None):
    if scores is None:
        scores = {}
    if s in scores:
        return scores[s]
    psub = {p: np for p, np in pat.items() if p in s}
    if not psub:
        scores[s] = 0
        return 0
    if s[0] == s:
        scores[s] = 1
        return 1
    result = 0
    for p, np in psub.items():
        if s[:np] != p:
            continue
        right = s[np:]
        # print(f"{s} - {p}, {right}")
        if not right:
            result += 1
            continue
        result += score(right, psub, scores=scores)
    # print(f"{s} = {result}")
    scores[s] = result
    return result

patterns = {"r", "wr", "b", "g", "bwu", "rb", "gb", "br"}
patterns = {s: len(s) for s in patterns}

tests = {
    "brwrr": 2,
    "bggr": 1,
    "gbbr": 4,
    "rrbgbr" : 6,
    "bwurrg": 1,
    "brgr": 2
}
for s, n in tests.items():
    assert score(s, patterns) == n

scores = {}

fname = "./2024/19/input"
with open(fname, "r") as f:
    raw = f.read().splitlines()
patterns_l = {s.strip() for s in raw[0].split(",")}
patterns = {s: len(s) for s in patterns_l}
designs = raw[2:]

print(sum(score(d, patterns, scores) for d in designs))
