#!/usr/bin/env python

from helpers import turns

def i2rc(i, ncol):
    r = i // (ncol + 1)
    c = i - (ncol + 1) * r
    return r, c


# fname = "2024/16/test1"
# fname = "2024/16/test2"
fname = "2024/16/input"

with open(fname, "r") as f:
    raw = f.read().strip()

rows = raw.split("\n")
grid = list(map(list, rows))
nrow = len(grid)
ncol = len(grid[0])

sr, sc = i2rc(raw.find("S"), ncol)
er, ec = i2rc(raw.find("E"), ncol)
d0 = (0, 1)


def gn(r: int, c: int, direction: tuple[int, int]):
    """
    A* heuristic for route next steps.
    """
    # Taxicab distance
    dr = er - r
    dc = ec - c
    if dr == 0 and dc == 0:
        # At destination. gn = 0
        return 0
    return abs(dr) + abs(dc) + turns(dr, dc, direction[0], direction[1]) * 1000


class Cell:
    def __init__(self, r, c, direction, fn) -> None:
        self.r = r
        self.c = c
        self.direction = direction
        self.fn = fn
        self.gn = gn(self.r, self.c, self.direction)
        self.astar = self.fn + self.gn

    def __repr__(self) -> str:
        return str(
            {
                "rc": (self.r, self.c),
                "dir": self.direction,
                "fn": self.fn,
                "gn": self.gn,
                "astar": self.astar,
            }
        )


cells = [Cell(sr, sc, d0, 0)]
cell_coords = {(cells[0].r, cells[0].c)}
edges = [Cell(sr, sc, d0, 0)]


def options(cell: Cell, cell_coords: set[tuple[int,int]]) -> list[Cell]:
    """
    List of valid routes at a location.
    """
    opts = []
    for dr, dc in ((0, 1), (1, 0), (0, -1), (-1, 0)):
        rr, cc = (cell.r + dr, cell.c + dc)
        if not 0 <= rr < nrow:
            continue
        if not 0 <= cc < ncol:
            continue
        obj = grid[rr][cc]
        if obj == "#":
            continue
        if (rr, cc) in cell_coords:
            continue
        fn = cell.fn + 1
        if (dr, dc) != cell.direction:
            fn += 1000
        new = Cell(rr, cc, (dr, dc), fn)
        opts.append(new)
    return opts


cell = None

while edges:
    cell = edges.pop()
    if (cell.r == er) and (cell.c == ec):
        break
    cells.append(cell)
    cell_coords.add((cell.r, cell.c))
    opts = options(cell, cell_coords)
    if opts:
        edges += opts
        edges.sort(key=lambda x: x.astar, reverse=True)

# draw(route)
print(cell)
