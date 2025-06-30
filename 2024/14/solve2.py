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
    return False

robots = [Robot(s) for s in raw]
[r.move(100) for r in robots]

xmid = ncol // 2
ymid = nrow // 2

q1 = [r for r in robots if r.px < xmid and r.py < ymid]
q2 = [r for r in robots if r.px > xmid and r.py < ymid]
q3 = [r for r in robots if r.px < xmid and r.py > ymid]
q4 = [r for r in robots if r.px > xmid and r.py > ymid]

result = len(q1)*len(q2)*len(q3)*len(q4)
print(result)
# Too low
# 92887200
