from itertools import combinations

with open("2024/08/input", "r") as f:
    raw = f.read()

grid = [list(row) for row in raw.splitlines()]
nrow = len(grid)
ncol = len(grid[0])

nodes = set()

def place_node(r, c):
    if r < 0 or r >= nrow:
        return
    if c < 0 or c >= ncol:
        return
    nodes.add((r, c))

def place_antinodes(p1, p2):
    place_node(*p1)
    place_node(*p2)
    dr = p2[0] - p1[0]
    dc = p2[1] - p1[1]
    # Step back
    r = p1[0]
    c = p1[1]
    while True:
        r -= dr
        if r < 0:
            break
        c -= dc
        if c < 0:
            break
        place_node(r, c)
    # Step forward
    r = p1[0]
    c = p1[1]
    while True:
        r += dr
        if r >= nrow:
            break
        c += dc
        if c >= ncol:
            break
        place_node(r, c)

refs = {}
for r, row in enumerate(grid):
    for c, x in enumerate(row):
        if x != ".":
            if x not in refs:
                refs[x] = []
            refs[x].append((r, c))

for k, vals in refs.items():
    for comb in combinations(vals, 2):
        place_antinodes(*comb)

print(len(nodes))
