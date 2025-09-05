#!/usr/bin/env python

with open("./2024/20/input", "r") as f:
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

def get_cheats(rc, n, pos):
    r,c = rc
    cheats = 0
    for key, val in pos.items():
        rdist = (r-key[0])
        if not -20 <= rdist <= 20:
            continue
        rd = abs(rdist)
        cdist = (c-key[1])
        clo = -20 + rd
        chi = 20 - rd
        if not clo <= cdist <= chi:
            continue
        tdist = rd + abs(cdist)
        delta = val - n - tdist
        if tdist <= 20 and delta >= 100:
            cheats += 1
    return cheats

cheats = 0
for (r,c), n in pos.items():
    cheats += get_cheats((r,c), n, pos)

print(cheats)
# 986545

