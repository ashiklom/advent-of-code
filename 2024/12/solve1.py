#/usr/bin/env python3

from itertools import product

# fname = "2024/12/test"
fname = "2024/12/input"

with open(fname, "r") as f:
    raw = f.read().strip().split("\n")

nrow = len(raw)
ncol = len(raw[0])

toscan = set(product(range(nrow), range(ncol)))


def dirs(r: int, c: int):
    up = (r-1, c)
    down = (r+1, c)
    left = (r, c-1)
    right = (r, c+1)
    return (up, left, down, right)


def search(r: int, c: int, char: str, coords: set):
    if (r, c) not in toscan:
        return None
    if raw[r][c] != char:
        return None
    toscan.remove((r,c))
    for dr, dc in dirs(r, c):
        if not 0 <= dr < nrow:
            continue
        if not 0 <= dc < ncol:
            continue
        if raw[dr][dc] == char:
            coords.add((dr, dc))
            search(dr, dc, char, coords)
    return (char, coords)


def perim(region: set[tuple]) -> int:
    perim = 0
    for r, c in region:
        p = 4
        for direction in dirs(r, c):
            if direction in region:
                p -= 1
        perim += p
    return perim


def area(region: set[tuple]) -> int:
    return len(region)


def price(region):
    return perim(region) * area(region)


regions = list()
for r, c in list(toscan):
    char = raw[r][c]
    s = search(r, c, raw[r][c], {(r, c)})
    if s:
        regions.append(s)

prices = [(c[0], area(c[1]), perim(c[1]), price(c[1])) for c in regions]
result = sum(price(r[1]) for r in regions)
print(result)

with open("2024/12/result1", "w") as f:
    f.write(str(result))

