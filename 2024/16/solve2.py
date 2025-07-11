#!/usr/bin/env python

from copy import deepcopy

fname = "2024/16/test1"
astar_target = 7036

# fname = "2024/16/test2"

# fname = "2024/16/input"
# astar_target = 66404

with open(fname, "r") as f:
    raw = f.read().strip()

rows = raw.split("\n")
grid = list(map(list, rows))
nrow = len(grid)
ncol = len(grid[0])


def i2rc(i, ncol):
    r = i // (ncol + 1)
    c = i - (ncol + 1) * r
    return r, c


sr, sc = i2rc(raw.find("S"), ncol)
er, ec = i2rc(raw.find("E"), ncol)
d0 = (0, 1)


def turns(dr: int, dc: int, going_r: int, going_c: int) -> int:
    """
    Minimum number of turns needed given a distance and a heading.
    """
    if dr == 0:
        target_r = 0
    else:
        target_r = int(dr / abs(dr))

    if dc == 0:
        target_c = 0
    else:
        target_c = int(dc / abs(dc))

    if dr == 0:
        # Same row
        if going_r == 0:
            # Moving horizontally
            if going_r == target_r:
                # Same direction; no turns
                return 0
            # Opposite direction; 3 turns to turn around
            else:
                return 3
        # Moving vertically
        else:
            # Going vertical. Need to go horizontal. Only need 1 turn.
            return 1

    elif dc == 0:
        # Same logic, but for same column
        if going_c == 0:
            if going_c == target_c:
                return 0
            else:
                return 3
        else:
            return 1

    # Different row and column
    # 1 turn if heading in the right direction; 2 turns if in the wrong direction
    else:
        if going_r == 0:
            if going_c == target_c:
                return 1
            else:
                return 2
        if going_c == 0:
            if going_r == target_r:
                return 1
            else:
                return 2

    raise ValueError("Can't calcluate turns")

def gn(r: int, c: int, direction: tuple[int, int]) -> int:
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
    def __init__(
        self,
        r: int,
        c: int,
        direction: tuple[int, int],
        fn: int,
        parent: "Cell|None" = None,
    ) -> None:
        self.r = r
        self.c = c
        self.direction = direction
        self.fn = fn
        self.gn = gn(self.r, self.c, self.direction)
        self.astar = self.fn + self.gn
        self.parent = parent

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


def options(cell: Cell, cells: dict) -> list[Cell]:
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
        if (rr, cc) in cells:
            continue
        fn = cell.fn + 1
        if (dr, dc) != cell.direction:
            fn += 1000
        new = Cell(rr, cc, (dr, dc), fn, parent=cell)
        opts.append(new)
    return opts

def draw(grid: list[list[str]], cells: dict[tuple, Cell]):
    g = deepcopy(grid)
    for r, c in cells.keys():
        g[r][c] = "x"
    result = "\n".join("".join(row) for row in g)
    print(result)

def route(cell: Cell, cells: dict[tuple[int, int], Cell]) -> dict[tuple[int,int], Cell]:
    parent = cell.parent
    result = {(cell.r, cell.c): cell}
    while parent:
        cell = cells[(parent.r, parent.c)]
        result[(cell.r, cell.c)] = cell
        parent = cell.parent
    return result
        

cell = Cell(sr, sc, d0, 0)
cells = {(sr, sc): cell}
edges = [cell]

result = {(sr, sc): cell}

while edges:
    cell = edges.pop()
    if (cell.r == er) and (cell.c == ec):
        # Reached the end
        if cell.astar == astar_target:
            result |= route(cell, cells)
            # result.update(route(cell, cells))
        continue
    opts = options(cell, cells)
    if opts:
        edges += opts
        edges.sort(key=lambda x: x.astar, reverse=True)
    cells[(cell.r, cell.c)] = cell

draw(grid, result)
