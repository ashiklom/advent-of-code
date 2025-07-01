import re
from itertools import pairwise

# fname = "2024/14/test1"
# nrow, ncol = (7, 11)
fname = "2024/14/input"
nrow, ncol = (103, 101)
with open(fname, "r") as f:
    raw = f.read().strip().split("\n")

ptrn = re.compile(r"p=(\d+),(\d+) v=(-?\d+),(-?\d+)")

# Get robots
px = []
py = []
vx = []
vy = []

for s in raw:
    m = ptrn.match(s)
    if not m:
        raise ValueError
    grps = tuple(map(int, m.groups()))
    px.append(grps[0])
    py.append(grps[1])
    vx.append(grps[2])
    vy.append(grps[3])


def move(t):
    for i,_ in enumerate(raw):
        px[i] = (px[i] + vx[i]*t) % ncol
        py[i] = (py[i] + vy[i]*t) % nrow


def in_a_row(x: list[int], n: int = 6):
    nx = len(x)
    if nx < n:
        return False
    filter = list(range(n))
    xs = sorted(x)
    for i in range(0, nx-n):
        sub = [x-xs[i] for x in xs[i:(i+n)]]
        if sub == filter:
            return True
    return False

def ctree(px, py) -> bool:
    # Unique locations
    cells = {(x,y) for x,y in zip(px, py)}
    rows = {}
    for x,y in cells:
        if y not in rows:
            rows[y] = []
        rows[y].append(x)

    for _, row in rows.items():
        if in_a_row(row, 8):
            return True
    
    return False

def draw(px, py):
    cells = {(x,y) for x,y in zip(px,py)}
    grid = [["." for _ in range(ncol)] for _ in range(nrow)]
    for x, y in cells:
        grid[y][x] = "x"
    result = "\n".join("".join(g) for g in grid)
    print(result)

i = 0
while not ctree(px, py):
    i += 1
    move(1)
    if i % 100_000 == 0:
        print(i)

print("Found!")
draw(px, py)
print(i)
# 7492
