# 029A: <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
# 980A: <v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A
# 179A: <v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
# 456A: <v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A
# 379A: <v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A

# +---+---+---+
# | 7 | 8 | 9 |
# +---+---+---+
# | 4 | 5 | 6 |
# +---+---+---+
# | 1 | 2 | 3 |
# +---+---+---+
#     | 0 | A |
#     +---+---+

#     +---+---+
#     | ^ | A |
# +---+---+---+
# | < | v | > |
# +---+---+---+

# dir -> dir -> dir -> num

from itertools import permutations

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

dpad = {
    "A": (0,2),
    "^": (0,1),
    "<": (1,0),
    "v": (1,1),
    ">": (1,2)
}

# Punch numpad
code = [0, 2, 9, "A"]

def move_numpad(start, end):
    r0, c0 = numpad[start]
    r1, c1 = numpad[end]
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
    rcperm = set(permutations(rclist))
    # TODO Remove entries that are invalid
    return rcperm

print(move_numpad("A", 0))
print(move_numpad(0, 2))
print(move_numpad(2, 9))
print(move_numpad(9, "A"))
