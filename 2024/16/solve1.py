#!/usr/bin/env python


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


def turns(dr: int, dc: int, going_r: int, going_c: int) -> int:
    """
    Minimum number of turns needed given a distance and a heading.
    """
    if dr == 0:
        target_r = 0
    else:
        target_r = dr / abs(dr)

    if dc == 0:
        target_c = 0
    else:
        target_c = dc / abs(dc)

    if dr == 0:
        # Same row
        if going_r != 0:
            # Moving horizontally
            if going_r == target_r:
                # Same direction; no turns
                return 0
            # Opposite direction; 3 turns to turn around
            else:
                return 3
        # Moving vertically
        else:
            if going_c == target_c:
                # Right direction; only one turn
                return 1
            else:
                # Wrong direction; 2 turns to turn around
                return 2

    elif dc == 0:
        # Same logic, but for same column
        if going_c != 0:
            if going_c == target_c:
                return 0
            else:
                return 3
        else:
            if going_r == target_r:
                return 1
            else:
                return 2

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
edges = [Cell(sr, sc, d0, 0)]


def coords_checked(coord, cells):
    coords = [(c.r, c.c) for c in cells]
    return coord in coords


def options(cell: Cell, cells: list[Cell]) -> list[Cell]:
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
        if coords_checked((rr, cc), cells):
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
    opts = options(cell, cells)
    if opts:
        edges += opts
        edges.sort(key=lambda x: x.astar, reverse=True)

# draw(route)
print(cell)
