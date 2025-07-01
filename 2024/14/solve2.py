import re

# fname = "2024/14/test1"
# nrow, ncol = (7, 11)
fname = "2024/14/input"
nrow, ncol = (103, 101)
with open(fname, "r") as f:
    raw = f.read().strip().split("\n")

ptrn = re.compile(r"p=(\d+),(\d+) v=(-?\d+),(-?\d+)")


class Robot:

    def __init__(self, string: str):
        m = ptrn.match(string)
        if not m:
            raise ValueError(f"Invalid string {string}")
        grps = tuple(map(int, m.groups()))
        self.px, self.py, self.vx, self.vy = grps

    def move(self, t=1):
        self.px = (self.px + self.vx*t) % ncol
        self.py = (self.py + self.vy*t) % nrow

    def __repr__(self):
        return f"p: {(self.px, self.py)}  v:{(self.vx, self.vy)}"


def ctree(robots: list[Robot]) -> bool:
    # Unique locations
    rows = {}
    for r in robots:
        if r.py not in rows:
            rows[r.py] = set()
        rows[r.py].add(r.px)

    for key, val in rows.items():
        n = len(val)
        if n == 1:
            break
    else:
        return False

    try:
        r = rows[key+1]
    except KeyError:
        return False
    x = val.pop()
    v = (x-1, x, x+1)
    if not all(x in r for x in v):
        return False

    try:
        r = rows[key+2]
    except KeyError:
        return False
    v = (x-2, x-1, x, x+1, x+2)
    if not all(x in r for x in v):
        return False

    try:
        r = rows[key+3]
    except KeyError:
        return False
    v = (x-3, x-2, x-1, x, x+1, x+2, x+3)
    if not all(x in r for x in v):
        return False

    return True

robots = [Robot(s) for s in raw]
i = 0
# i = 1600000
# [r.move(1600000) for r in robots]
while not ctree(robots):
    i += 1
    [r.move(1) for r in robots]
    if i % 100_000 == 0:
        print(i)
    if i > 1_000:
        raise ValueError

print("Found!")
print(i)
