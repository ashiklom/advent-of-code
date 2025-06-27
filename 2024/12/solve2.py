#/usr/bin/env python3

from itertools import product

# fname = "2024/12/test2"
fname = "2024/12/input"

with open(fname, "r") as f:
    raw = f.read().strip().split("\n")

# print("\n".join(raw))

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


def corner(r, c, region, dr, dc):
    H = (r, c+dc) in region
    V = (r+dr, c) in region
    C = (r+dr, c+dc) in region
    if C and H and V:
        # oo
        # oX
        return False
    if not C and not H and not V:
        # ..
        # .X
        return True
    if C and not H and not V:
        # o.
        # .X
        return True
    if not C and H and V:
        # .o
        # oX
        return True
    return False


def cell_corners(r, c, region):
    corners = 0
    for dr in (-1,1):
        for dc in (-1,1):
            corners += corner(r, c, region, dr, dc)
    return corners


def corners(region: set[tuple]) -> int:
    # Number of sides == number of corners
    corners = 0
    for r, c in region:
        corners += cell_corners(r, c, region)
    return corners


def area(region: set[tuple]) -> int:
    return len(region)


def price(region):
    return corners(region) * area(region)


regions = list()
for r, c in list(toscan):
    char = raw[r][c]
    s = search(r, c, raw[r][c], {(r, c)})
    if s:
        regions.append(s)

result = sum(price(r) for _, r in regions)
print(result)
# 830516
