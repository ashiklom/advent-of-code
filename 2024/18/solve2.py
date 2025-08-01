#!/usr/bin/env python

from heapq import heappush, heappop

def parse_coord(x: str) -> tuple[int, int]:
    xstr, ystr = x.strip().split(',')
    return (int(xstr), int(ystr))

def draw(grid: list[list[str]]) -> None:
    print("\n".join("".join(row) for row in grid))

type coord = tuple[int, int]

def dist(x: coord, target: coord):
    return abs(x[0]-target[0]) + abs(x[1]-target[1])

def solve(coords: list[coord]):
    rowmax = 70
    colmax = 70

    grid = [["."]*(colmax+1) for _ in range(rowmax+1)]

    for x,y in coords:
        grid[y][x] = "#"
    grid[0][0] = "S"
    grid[-1][-1] = "E"

    start = (0, 0)
    end = (colmax, rowmax)

    visited = {start}
    edges = []
    for c in [(0,1), (1,0)]:
        heappush(edges, (1 + dist(c, end), 1, c))

    steps = 0
    last_coord = coords[-1]
    while edges:
        _, steps, (x, y) = heappop(edges)
        if (x,y) in visited:
            continue
        if x < 0 or x > colmax:
            continue
        if y < 0 or y > rowmax:
            continue
        if grid[y][x] == "#":
            continue
        if (x,y) == end:
            last_coord = None
            break
        visited.add((x,y))
        for nxy in ((x-1, y), (x+1,y), (x,y-1), (x,y+1)):
            heappush(edges, (steps+1 + dist(nxy, end), steps+1, nxy))

    return last_coord

with open("2024/18/input", "r") as f:
    raw = f.read().strip()

coords = list(map(parse_coord, raw.split('\n')))
# Binary search
result = None
n = 1024
rng = [n, len(coords)]
while (rng[1] - rng[0]) > 1:
    n = rng[0] + ((rng[1]-rng[0]) // 2)
    # print(f"{rng[0]} -- {n} -- {rng[1]}")
    result = solve(coords[:n])
    if not result:
        rng[0] = n
    else:
        rng[1] = n

print(f"{result} at coord {n}")
# Answer: (34, 40) (at ncoords = 2906)
