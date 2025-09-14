from itertools import chain, pairwise, product
from functools import cache

# 789
# 456
# 123
#  0A
numpad = {
    "A": (3,2),
    "0": (3,1),
    "1": (2,0),
    "2": (2,1),
    "3": (2,2),
    "4": (1,0),
    "5": (1,1),
    "6": (1,2),
    "7": (0,0),
    "8": (0,1),
    "9": (0,2)
}

#  ^A
# <v>
dpad_dict = {
    "A^": ["<A"],
    "Av": ["v<A", "<vA"],
    "A<": ["v<<A"],
    "A>": ["vA"],
    "^A": [">A"],
    "^>": [">vA", "v>A"],
    "^<": ["v<A"],
    "vA": [">^A", "^>A"],
    "v>": [">A"],
    "v<": ["<A"],
    "<A": [">>^A"],
    "<^": [">^A"],
    "<v": [">A"],
    ">A": ["^A"],
    ">^": ["<^A"],
    ">v": ["<A"],
    ">>": ["A"],
    "^^": ["A"],
    "<<": ["A"],
    "vv": ["A"],
    "AA": ["A"]
}

def get_pairs(string: str) -> list[str]:
    pairs = pairwise("A" + string)
    return ["".join(pair) for pair in pairs]

@cache
def get_len(string: str, nbots: int) -> int:
    if nbots == 0:
        return len(string)
    pairs = get_pairs(string)
    codes = [dpad_dict[pair] for pair in pairs]
    code_prod = product(*codes)
    result = 0
    for code in code_prod:
        length = sum(get_len(c, nbots-1) for c in code)
        if not result or (length < result):
            result = length
    return result

# Punch numpad
def move_numpad(start, end):
    if start == end:
        return [["A"]]
    r0, c0 = numpad[start]
    r1, c1 = numpad[end]
    dr = r1 - r0
    dc = c1 - c0
    if dr >= 0:
        rmv = ["v"] * abs(dr)
    else:
        rmv = ["^"] * abs(dr)
    if dc >= 0 :
        cmv = [">"] * abs(dc)
    else:
        cmv = ["<"] * abs(dc)
    if not rmv:
        return [cmv + ["A"]]
    if not cmv:
        return [rmv + ["A"]]
    if (r0 == 3) and (c1 == 0):
        return [rmv + cmv + ["A"]]
    if (c0 == 0) and (r1 == 3):
        return [cmv + rmv + ["A"]]
    options = [rmv + cmv + ["A"], cmv + rmv + ["A"]]
    return options

def enter_code(code):
    steps = [move_numpad(a,b) for a,b in pairwise("A"+code)]
    sprod = product(*steps)
    return["".join(chain.from_iterable(s)) for s in sprod]

def solve_code(code, n):
    keys = enter_code(code)
    return min(get_len(k, n) for k in keys)

# testcodes = [
#     "029A",
#     "980A",
#     "179A",
#     "456A",
#     "379A"
# ]
# testsolve = {k: solve_code(k, 2) for k in testcodes}

with open("2024/21/input", "r") as f:
    codes = f.read().splitlines()

# part1 = sum(solve_code(c, 2) * int(c[:-1]) for c in codes)
part2 = sum(solve_code(c, 25) * int(c[:-1]) for c in codes)
print(part2)
