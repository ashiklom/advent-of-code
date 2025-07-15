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

type coord = tuple[int, int]

class Cell:
    def __init__(
        self, r: int, c: int, direction: coord, fn: int, route: list = []
    ) -> None:
        self.r = r
        self.c = c
        self.direction = direction
        self.fn = fn
        self.route = route + [(self.r, self.c)]
        self.opts = []
        self.checked_opts = False
        self.is_fork = False

    def get_options(self) -> None:
        """
        List of valid routes at a location.
        """
        nopts = 0
        self.checked_opts = True
        ndead = 0
        for dr, dc in ((0, 1), (1, 0), (0, -1), (-1, 0)):
            rr, cc = (self.r + dr, self.c + dc)
            obj = grid[rr][cc]
            if obj == "#":
                ndead += 1
                continue
            try:
                if (rr, cc) == self.route[-2]:
                    ndead += 1
                    continue
            except IndexError:
                pass
            if (rr, cc) in dead_ends:
                ndead += 1
                continue
            # All of the reasons above this are valid reasons for calling something a dead end.
            # The reasons below are not.
            if (rr, cc) in self.route:
                continue
            fn = cell.fn + 1
            if (dr, dc) != self.direction:
                fn += 1000
            if fn > astar_target:
                continue
            new = Cell(rr, cc, (dr, dc), fn, route=cell.route)
            self.opts.append(new)
            nopts += 1
        if nopts > 1:
            self.is_fork = True
        if ndead == 4:
            # Complete dead end. Mark self and route back to last fork as dead end.
            dead_ends.add((self.r, self.c))
            i = -2
            c = cells[self.route[i]]
            while not c.is_fork:
                i -= 1 
                try:
                    c = cells[self.route[i]]
                except IndexError:
                    break

    def find_last_fork(self, cells: dict[coord, "Cell"]) -> "Cell":
        i = -2
        c = cells[self.route[i]]
        while not c.is_fork:
            i -= 1
            c = cells[self.route[i]]
        return c

    def __repr__(self) -> str:
        return str(
            {
                "rc": (self.r, self.c),
                "dir": self.direction,
                "fn": self.fn
            }
        )


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


cell = Cell(sr, sc, d0, 0)
cells = {(sr, sc): cell}
edges = [cell]

dead_ends = set()

result = set()

i = 0
while True:
    i += 1
    if i % 500_000 == 0:
        # print(dead_ends)
        draw(grid, set(cells.keys()), dead_ends)
    if not cell.checked_opts:
        cell.get_options()
    if not cell.opts:
        # Dead end; double back
        cell.is_fork = False
        try:
            cell = cell.find_last_fork(cells)
        except IndexError:
            # We've explored the entire maze, so we're done
            break
        continue
    cell = cell.opts.pop()
    cells[(cell.r, cell.c)] = cell
    if (cell.r == er) and (cell.c == ec):
        result.update(set(cell.route))
        cell = cell.find_last_fork(cells)

draw(grid, result)

# i = 0
# while edges:
#     i += 1
#     if i % 10_000 == 0:
#         print(i)
#     cell = edges.pop()
#     if (cell.r == er) and (cell.c == ec):
#         if cell.fn <= astar_target:
#             result.update(cell.route)
#         continue
#     cell.get_options()
#     if cell.opts:
#         edges += cell.opts
#     else:
#         rr = cell.r + cell.direction[0]
#         cc = cell.c + cell.direction[1]
#         if grid[rr][cc] == "#":
#             # True dead end.
#             # Add it and all children that aren't forks to the list of dead ends so they aren't searched again.
#             dead_cell = cell
#             while len(dead_cell.route) >= 2 and len(dead_cell.opts) <= 1:
#                 dead_ends.add((dead_cell.r, dead_cell.c))
#                 nxt = dead_cell.route[-2]
#                 dead_cell = cells[nxt]
#     cells[(cell.r, cell.c)] = cell

# draw(grid, result)
# draw(grid, dead_ends)
# print(len(dead_ends))
print(len(result))
