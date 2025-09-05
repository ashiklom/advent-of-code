#!/usr/bin/env python

def score(s: str, pat, scores=None):
    if scores is None:
        scores = {}
    if s in scores:
        return scores[s]
    if len(s) == 1:
        if s not in pat:
            scores[s] = 0
            return 0
        scores[s] = 1
        return 1
    result = 0
    for p in pat:
        if not s.startswith(p):
            continue
        right = s[len(p):]
        # print(f"{s} - {p}, {right}")
        if not right:
            result += 1
            continue
        result += score(right, pat, scores=scores)
    # print(f"{s} = {result}")
    scores[s] = result
    return result

patterns = {"r", "wr", "b", "g", "bwu", "rb", "gb", "br"}

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
patterns = {s.strip() for s in raw[0].split(",")}
designs = raw[2:]

# print(score(designs[0], patterns, scores))
# with open("./2024/19/bad") as f:
#     bad = f.read().splitlines()
#
# print(sorted(patterns, key=len))
# score(bad[0], patterns)

print(sum(score(d, patterns, scores) for d in designs))
