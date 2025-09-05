#!/usr/bin/env python

with open("./2024/20/testinput", "r") as f:
    rows = f.read().splitlines()
    grid = [list(s) for s in rows]

start = None

for i, row in enumerate(grid):
    for j, x in enumerate(row):
        if x == "S":
            start = (i, j)
            break

if not start:
    raise ValueError("Start not found")

# Trace the route
r, c = start
pos = {}
step = 0
while True:
    pos[(r,c)] = step
    if grid[r][c] == "E":
        break
    for r1,c1 in ((r+1,c), (r-1,c), (r,c+1), (r,c-1)):
        if (r1,c1) in pos:
            continue
        if grid[r1][c1] in ".E":
            r = r1
            c = c1
            break
    step += 1

cheats = {}
for (r,c), n in pos.items():
    for rc in ((r+2,c),(r-2,c),(r,c+2),(r,c-2)):
        if rc not in pos:
            continue
        m = pos[rc]
        if m > (n+2):
            delta = m - (n+2)
            if delta not in cheats:
                cheats[delta] = 0
            cheats[delta] += 1

print(cheats)

# {4: 14, 2: 14, 12: 3, 10: 2, 8: 4, 6: 2, 40: 1, 64: 1,
# 38: 1, 36: 1, 20: 1}
