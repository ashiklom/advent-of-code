#!/usr/bin/env python

from copy import deepcopy

from helpers import turns, i2rc

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

def gn(r: int, c: int, direction: coord, er: int, ec: int):
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
        self, r: int, c: int, direction: coord, fn: int, parent: "Cell|None" = None
    ) -> None:
        self.r = r
        self.c = c
        self.direction = direction
        self.fn = fn
        self.astar = 0
        self.parent = parent
        self.opts = []

    def get_options(self, cells: dict[coord, "Cell"], end: coord) -> None:
        """
        List of valid routes at a location.
        """
        for dr, dc in ((0, 1), (1, 0), (0, -1), (-1, 0)):
            rr, cc = (self.r + dr, self.c + dc)
            if self.parent and (rr, cc) == self.parent.coord:
                continue
            if (rr, cc) in cells:
                continue
            obj = grid[rr][cc]
            if obj == "#":
                continue
            fn = self.fn + 1
            if (dr, dc) != self.direction:
                fn += 1000
            if fn > astar_target:
                continue
            astar = fn + gn(rr, cc, (dr, dc), end[0], end[1])
            if astar > astar_target:
                continue
            new = Cell(rr, cc, (dr, dc), fn, parent=self)
            new.astar = astar
            self.opts.append(new)

    @property
    def coord(self) -> coord:
        return (self.r, self.c)

    def __repr__(self) -> str:
        return str(
            {
                "rc": (self.r, self.c),
                "dir": self.direction,
                "fn": self.fn,
                "astar": self.astar
            }
        )


def reconstruct_route(cell: "Cell"):
    route = [cell.coord]
    while cell.parent:
        cell = cell.parent
        route.append(cell.coord)
    return route

def draw(grid: list[list[str]], cells: set[coord], alt: set[coord] = set()):
    g = deepcopy(grid)
    for r, c in cells:
        g[r][c] = "x"
    for r, c in alt:
        g[r][c] = "O"
    # g[sr][sc] = "S"
    # g[er][ec] = "E"
    result = "\n".join("".join(row) for row in g)
    print(result)


def solve(start: Cell, end: coord):
    cells = {start.coord: start}
    edges = [start]
    while edges:
        cell = edges.pop()
        if cell.coord == end:
            return cell
        cell.get_options(cells, end)
        if cell.opts:
            edges += cell.opts
            edges.sort(key=lambda x: x.astar, reverse=True)
        cells[cell.coord] = cell
    return None

start = Cell(sr, sc, (0, 1), 0)
test = solve(start, (er, ec))
print(test)
test2 = solve(start, (67, 59))
print(test2)

result = set()

def solve_fork(fork: coord):
    start = Cell(sr, sc, (0, 1), 0)
    to_fork = solve(start, fork)
    if not to_fork:
        return None
    to_end = solve(to_fork, (er, ec))
    if not to_end:
        return None
    result.update(set(reconstruct_route(to_end)))


for i, fork in enumerate(forks):
    if i % 100 == 0:
        print(f"{i} / {len(forks)}")
    solve_fork(fork)

# draw(grid, result)
print(len(result))
