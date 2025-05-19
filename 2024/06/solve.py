import re

guard = re.compile(r"\^|>|<|v")

with open("2024/06/input") as f:
    grid = [s.strip() for s in f.readlines()]

result = grid

# Find starting position
starty = None
startx = None
for idx,row in enumerate(grid):
    gf = guard.search(row)
    if gf:
        starty = idx
        startx = gf.span()[0]
        break

class Guard:
    def __init__(grid, x,y,d):
        self.grid = grid
        self.x = x
        self.y = y
        self.d = d

# print(starty)
# print(startx)
d = grid[starty][startx]
print(d)
