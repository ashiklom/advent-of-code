#!/usr/bin/env python

def split(s: str, p: str) -> tuple[str,str] | None:
    j = len(p)
    s0 = s[:j]
    if s0 == p:
        return (s0, s[j:])
    return None

assert split("b", "b") == ("b","")
assert split("bw", "w") is None
assert split("bw", "b") == ("b", "w")

def psplit(s: str, pat: set[str]):
    return [sp for p in pat if (sp := split(s, p))]

assert set(psplit("brwrr", {"b", "r", "br"})) == {
    ("b", "rwrr"),
    ("br", "wrr")
}

assert len(psplit("bw", {"b", "w", "bw"})) == 2

type scoredict = dict[str, int]

def get_score(s: str, pat: set[str], scores: scoredict | None = None):
    if scores is None:
        scores = {}
    if s in scores:
        return scores[s]
    if len(s) == 1:
        scores[s] = 1
        return 1
    splits = psplit(s, pat)
    # print(f"{s} : {splits}")
    final_score = 0
    for left, right in splits:
        if not left:
            scores[s] = 0
            return 0
        if not right:
            final_score += 1
            continue
        final_score += get_score(right, pat, scores)
    scores[s] = final_score
    # print(f"{s} = {final_score}")
    return final_score

assert get_score("b", {"b"}) == 1
assert get_score("bw", {"b", "w"}) == 1
assert get_score("bw", {"b", "w", "bw"}) == 2
assert get_score("bwb", {"b", "w", "bw"}) == 2
assert get_score("bwbw", {"b", "w", "bw"}) == 4

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
    assert get_score(s, patterns) == n
    # print(f"{s} = {n}?")
    # print(get_score(s, patterns))

scores = {}


