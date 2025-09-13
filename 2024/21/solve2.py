from itertools import chain, pairwise, permutations, product
from math import prod

# +---+---+---+
# | 7 | 8 | 9 |
# +---+---+---+
# | 4 | 5 | 6 |
# +---+---+---+
# | 1 | 2 | 3 |
# +---+---+---+
#     | 0 | A |
#     +---+---+
numpad = {
    "A": (3,2),
    0: (3,1),
    1: (2,0),
    2: (2,1),
    3: (2,2),
    4: (1,0),
    5: (1,1),
    6: (1,2),
    7: (0,0),
    8: (0,1),
    9: (0,2)
}

#     +---+---+
#     | ^ | A |
# +---+---+---+
# | < | v | > |
# +---+---+---+
dpad = {
    "A": (0,2),
    "^": (0,1),
    "<": (1,0),
    "v": (1,1),
    ">": (1,2)
}

dpad_pars = {
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
    ">^": ["<^A", "^>A"],
    ">v": ["<A"],
    ">>": ["A"],
    "^^": ["A"],
    "<<": ["A"],
    "vv": ["A"],
    "AA": ["A"]
}

#   029A = <A^A^^>AvvA
#   980A
#   179A
#   456A
#   379A = ^A<<^^A>>AvvA

code = list("^A<<^^A>>AvvA")
code_pairs = list(["".join(pair) for pair in pairwise(code)])
bot1 = [dpad_pars[k] for k in code_pairs]
bot1_perm = list("".join(p) for p in product(*bot1))
bot2 = [dpad_pars["".join(k)] for code in bot1_perm for k in pairwise(code)]
list(map(len, bot2))
bot2_perm = list("".join(p) for p in product(*bot2))

# 789
# 456
# 123
#  0A

# Punch numpad
def move_pad(start, end, pad):
    if start == end:
        return ["A"]
    r0, c0 = pad[start]
    r1, c1 = pad[end]
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
        return cmv + ["A"]
    if not cmv:
        return rmv + ["A"]
    if pad == numpad:
        if (r0 == 3) and (c1 == 0):
            return rmv + cmv + ["A"]
        if (c0 == 0) and (r1 == 3):
            return cmv + rmv + ["A"]
    elif pad == dpad:
        if (r0 == 0) and (c1 == 0):
            return rmv + cmv + ["A"]
        if (c0 == 0) and (r1 == 0):
            return cmv + rmv + ["A"]
    options = [rmv + cmv + ["A"], cmv + rmv + ["A"]]
    return options
    # return cmv + rmv + ["A"]

dpad_pairs = list(permutations(dpad.keys(), 2))
lst = {(a,b): move_pad(a, b, dpad) for a,b in dpad_pairs}
lst[("<", "A")]

def enter_code(code, pad):
    steps = [move_pad(a,b, pad) for a,b in pairwise(["A"]+code)]
    return list(chain.from_iterable(steps))

# print(move_pad("A", 0, numpad))
# print(move_pad(0, 2, numpad))
# print(move_pad(2, 9, numpad))
# print(move_pad(9, "A", numpad))

# "".join(enter_code(enter_code([">", "^", "^", "A"], dpad), dpad))
# "".join(enter_code(enter_code(["^", "^", ">", "A"], dpad), dpad))

def full_code(code, nbots):
    dcode = enter_code(code, numpad)
    for _ in range(0, nbots):
        dcode = enter_code(dcode, dpad)
    human = enter_code(dcode, dpad)
    return human

def get_score(code, nbots):
    bot3 = full_code(code, nbots)
    blen = len(bot3)
    numcode = int("".join(str(c) for c in code[:-1]))
    return (blen, numcode)

testcodes = [
    [0, 2, 9, "A"],
    [9, 8, 0, "A"],
    [1, 7, 9, "A"],
    [4, 5, 6, "A"],
    [3, 7, 9, "A"]
]

# get_score(testcodes[4], 5)
#
# [get_score(code, 1) for code in testcodes]
# [get_score(code, 25) for code in testcodes]

# ---
# <v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
#    <   A > A  v <<   AA >  ^ AA > A
#    <A>Av<<AA>^AA>A
#     ^ A   <<  ^^ A
#     ^A<<^^A
#      3    7
# v<<A>>^AvA^Av<<A>>^AAv<A<A>>^AAvAA^<A>Av<A^>AA<A>Av<A<A>>^AAA<Av>A^A
#    <   A > A   <   AA  v <   AA >>  ^ A  v  AA ^ A  v <   A
#    <A>A<AAv<AA>>^AvAA^Av<A
#     ^ A ^^  <<   A >> A  v
#     ^A^^<<A>>Av
#      3    7  9

# Yes: <vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
# No:  v<<A>>^AAv<A<A>>^AAvAA^<A>Av<A^>AA<A>Av<A<A>>^AAA<Av>A^A

with open("2024/21/input", "r") as f:
    raw = f.read().splitlines()

codes = list()
for code in raw:
    lst = []
    for char in code:
        if char == "A":
            lst.append("A")
        else:
            lst.append(int(char))
    codes.append(lst)

scores = [get_score(code, 1) for code in codes]
total = sum(a*b for a,b in scores)
print(total)

