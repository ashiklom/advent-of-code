

def i2rc(i, ncol):
    r = i // (ncol+1)
    c = i - (ncol+1)*r
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
d0 = (0,1)

def gn(r: int, c: int, direction: tuple[int,int]):
    """
    A* heuristic for route next steps.
    """
    # Taxicab distance
    dr = er - r
    dc = ec - c
    if dr == 0 and dc == 0:
        # At destination. gn = 0
        return 0
    dra = abs(dr)
    dca = abs(dc)
    # Minimum number of turns required
    turns = 0
    if dr != 0 and dc != 0:
        turns += direction[0] != dr/dra
        turns += direction[1] != dc/dca
    elif dr == 0:
        turns += 3 * (direction[1] != dc/dca)
    elif dc == 0:
        turns += 3 * (direction[0] != dr/dra)
    return dra + dca + turns*1000


class Cell:
    def __init__(self, r, c, direction, fn) -> None:
        self.r = r
        self.c = c
        self.direction = direction
        self.fn = fn
        self.gn = gn(self.r, self.c, self.direction)
        self.astar = self.fn + self.gn

    def __repr__(self) -> str:
        return str({
            "rc": (self.r, self.c),
            "dir": self.direction,
            "fn": self.fn,
            "gn": self.gn,
            "astar": self.astar
        })

cells = [Cell(sr, sc, d0, 0)]
edges = [Cell(sr, sc, d0, 0)]

def coords_checked(coord, cells):
    coords = {(c.r, c.c) for c in cells}
    return (coord in coords)

def options(cell: Cell, cells: list[Cell]) -> list[Cell]:
    """
    List of valid routes at a location.
    """
    opts = []
    for dr, dc in ((0,1), (1,0), (0,-1), (-1,0)):
        rr, cc = (cell.r+dr, cell.c+dc)
        if coords_checked((rr,cc), cells):
            continue
        if not 0 <= rr < nrow:
            continue
        if not 0 <= cc < ncol:
            continue
        obj = grid[rr][cc]
        if obj == "#":
            continue
        fn = cell.fn + 1
        if (dr,dc) != cell.direction:
            fn += 1000
        new = Cell(rr, cc, (dr,dc), fn)
        opts.append(new)
    return opts

# def draw(route: Route):
#     g = deepcopy(grid)
#     for cell in route.cells:
#         r = cell.r
#         c = cell.c
#         match cell.direction:
#             case (0,1):
#                 char = ">"
#             case (1,0):
#                 char = "v"
#             case (0,-1):
#                 char = "<"
#             case (-1,0):
#                 char = "^"
#             case _:
#                 raise ValueError(f"Invalid direction: {cell.direction}")
#         if g[r][c] not in ("S", "E"):
#             g[r][c] = char
#     print("\n".join("".join(row) for row in g))
#     print("-"*12)

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
