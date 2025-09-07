from itertools import chain, pairwise, permutations, product

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

def drop_long(steps: set):
    stepdict = {}
    for step in steps:
        stepdict[step] = sum(a!=b for a,b in pairwise(step))
    min_moves = min(stepdict.values())
    keep_steps = {step for step, moves in stepdict.items() if moves == min_moves}
    return keep_steps

def drop_invalid(steps: set, pad: dict, r0: int, c0: int, r1: int, c1: int):
    if pad == numpad:
        if (r0 == 3) and (c1 == 0):
            result = set()
            invalid = "<" * abs(c1-c0)
            ninv = len(invalid)
            for step in steps:
                if step[:ninv] == invalid:
                    continue
                result.add(step)
        elif (c0 == 0) and (r1 == 3):
            result = set()
            invalid = "v" * abs(r1-r0)
            ninv = len(invalid)
            for step in steps:
                if step[:ninv] == invalid:
                    continue
                result.add(step)
        else:
            return steps
    elif pad == dpad:
        if (r0 == 0) and (c1 == 0):
            result = set()
            invalid = "<" * abs(c1-c0)
            ninv = len(invalid)
            for step in steps:
                if step[:ninv] == invalid:
                    continue
                result.add(step)
        elif (c0 == 0) and (r1 == 0):
            result = set()
            invalid = "^" * abs(r1-r0)
            ninv = len(invalid)
            for step in steps:
                if step[:ninv] == invalid:
                    continue
                result.add(step)
        else:
            return steps
    else:
        raise ValueError("Invalid pad")
    return result

# Punch numpad
def move_pad(start, end, pad):
    # 
    if start == end:
        return {"A"}
    r0, c0 = pad[start]
    r1, c1 = pad[end]
    dr = r1 - r0
    dc = c1 - c0
    if dr >= 0:
        rmv = "v" * abs(dr)
    else:
        rmv = "^" * abs(dr)
    if dc >= 0 :
        cmv = ">" * abs(dc)
    else:
        cmv = "<" * abs(dc)
    rclist = rmv + cmv
    rcperm = set("".join(p)+"A" for p in permutations(rclist))
    # TODO Remove entries that are invalid
    rcvalid = drop_invalid(rcperm, pad, r0, c0, r1, c1)
    rcshort = drop_long(rcvalid)
    return rcshort

# print(move_pad("A", 0, numpad))
# print(move_pad(0, 2, numpad))
# print(move_pad(2, 9, numpad))
# print(move_pad(9, "A", numpad))

def enter_code(code, pad):
    steps = [move_pad(a,b, pad) for a,b in pairwise(["A"]+code)]
    seqs = set("".join(p) for p in product(*steps))
    return seqs

def full_code(code):
    bot1 = enter_code(code, numpad)
    bot2 = set(chain.from_iterable(enter_code(list(b), dpad) for b in bot1))
    bot3 = set(chain.from_iterable(enter_code(list(b), dpad) for b in bot2))
    return bot3

def get_score(code):
    bot3 = full_code(code)
    blen = min(len(x) for x in bot3)
    numcode = int("".join(str(c) for c in code[:-1]))
    return (blen, numcode)

# testcodes = [
#     [0, 2, 9, "A"],
#     [9, 8, 0, "A"],
#     [1, 7, 9, "A"],
#     [4, 5, 6, "A"],
#     [3, 7, 9, "A"]
# ]

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

scores = [get_score(code) for code in codes]
total = sum(a*b for a,b in scores)
print(total)
