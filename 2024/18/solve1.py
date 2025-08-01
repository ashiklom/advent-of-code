#!/usr/bin/env python

from heapq import heappush, heappop

def parse_coord(x: str):
    x2 = x.strip().split(',')
    return tuple(map(int, x2))

def draw(grid: list[list[str]]) -> None:
    print("\n".join("".join(row) for row in grid))

type coord = tuple[int, int]

def dist(x: coord, target: coord):
    return abs(x[0]-target[0]) + abs(x[1]-target[1])

def solve(input: str, rowmax: int, colmax: int, ncoord: int):
    with open(input, "r") as f:
        raw = f.read().strip()

    coords = list(map(parse_coord, raw.split('\n')))

    grid = [["."]*(colmax+1) for _ in range(rowmax+1)]
    coords_sub = coords[:ncoord]

    for x,y in coords_sub:
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
            break
        visited.add((x,y))
        grid[y][x] = "O"
        for nxy in ((x-1, y), (x+1,y), (x,y-1), (x,y+1)):
            heappush(edges, (steps+1 + dist(nxy, end), steps+1, nxy))

    return {"steps": steps, "grid": grid}

test = solve("2024/18/testinput", 6, 6, 12)
if test["steps"] != 22:
    raise ValueError(f"Test solution failed. Expected 22, got {test['steps']}")

challenge = solve("2024/18/input", 70, 70, 1024)
# draw(challenge["grid"])
print(challenge["steps"])
# Answer: 320
