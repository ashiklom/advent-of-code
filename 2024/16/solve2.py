#!/usr/bin/env python

from copy import deepcopy

# fname = "2024/16/test1"
# astar_target = 7036
# answer = 45

# fname = "2024/16/test2"
# astar_target = 11048
# answer = 64

fname = "2024/16/input"
astar_target = 66_404

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

# Find every fork
forks = set()
for r, row in enumerate(grid):
    for c, x in enumerate(row):
        if x == "#":
            continue
        n = 0
        n += grid[r-1][c] != "#"
        n += grid[r+1][c] != "#"
        n += grid[r][c-1] != "#"
        n += grid[r][c+1] != "#"
        if n >= 3:
            forks.add((r, c))

type coord = tuple[int, int]

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
            if going_c == target_c:
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
            if going_r == target_r:
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


def gn(r: int, c: int, direction: tuple[int, int], er: int, ec: int):
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
        self, r: int, c: int, direction: coord, fn: int, route: list = []
    ) -> None:
        self.r = r
        self.c = c
        self.direction = direction
        self.fn = fn
        self.astar = 0
        self.route = route + [(self.r, self.c)]
        self.opts = []

    def get_options(self, cells: dict[coord, "Cell"], end: coord) -> None:
        """
        List of valid routes at a location.
        """
        for dr, dc in ((0, 1), (1, 0), (0, -1), (-1, 0)):
            rr, cc = (self.r + dr, self.c + dc)
            obj = grid[rr][cc]
            if obj == "#":
                continue
            try:
                if (rr, cc) == self.route[-2]:
                    continue
            except IndexError:
                pass
            if (rr, cc) in cells:
                continue
            fn = self.fn + 1
            if (dr, dc) != self.direction:
                fn += 1000
            if fn > astar_target:
                continue
            astar = fn + gn(rr, cc, (dr, dc), end[0], end[1])
            if astar > astar_target:
                continue
            new = Cell(rr, cc, (dr, dc), fn, route=self.route)
            new.astar = astar
            self.opts.append(new)

    def __repr__(self) -> str:
        return str(
            {
                "rc": (self.r, self.c),
                "dir": self.direction,
                "fn": self.fn
            }
        )

def solve(start: coord, end: coord, d0: coord, fn: int = 0):
    cell = Cell(start[0], start[1], d0, fn)
    cells = {start: cell}
    edges = [cell]
    while edges:
        cell = edges.pop()
        if ((cell.r == end[0]) and (cell.c == end[1])):
            start_cell = cells[cell.route[0]]
            return (start_cell, cell)
        cell.get_options(cells, end)
        if cell.opts:
            edges += cell.opts
            edges.sort(key=lambda x: x.astar, reverse=True)
        cells[(cell.r, cell.c)] = cell
    return None

test = solve((sr, sc), (er, ec), (0, 1))

result = set()

def solve_fork(fork: coord):
    solve_end = solve(fork, (er, ec), (0, 1))
    if not solve_end:
        return None
    init, to_end = solve_end
    # Start going in the exact opposite direction of where the end solution started
    d0 = (-init.direction[0], -init.direction[1])
    solve_start = solve(fork, (sr, sc), d0, fn=to_end.fn)
    if not solve_start:
        return None
    result.update(set(to_end.route))
    result.update(set(solve_start[1].route))

def draw(grid: list[list[str]], cells: set[coord], alt: set[coord] = set()):
    g = deepcopy(grid)
    for r, c in cells:
        g[r][c] = "x"
    for r, c in alt:
        g[r][c] = "O"
    g[sr][sc] = "S"
    g[er][ec] = "E"
    result = "\n".join("".join(row) for row in g)
    print(result)

for i, fork in enumerate(forks):
    if i % 100 == 0:
        print(f"{i} / {len(forks)}")
    solve_fork(fork)

draw(grid, result)
# draw(grid, dead_ends)
# print(len(dead_ends))
print(len(result))
