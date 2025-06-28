from copy import deepcopy

# fname = "2024/16/test1"
# fname = "2024/16/test2"
fname = "2024/16/input"

with open(fname, "r") as f:
    raw = f.read().strip()

rows = raw.split("\n")
grid = list(map(list, rows))
nrow = len(grid)
ncol = len(grid[0])

def i2rc(i, ncol):
    r = i // (ncol+1)
    c = i - (ncol+1)*r
    return r, c

def routes(r: int, c: int, current_route: list) -> list[tuple[int, int]]:
    options = []
    for dr, dc in ((0,1), (1,0), (0,-1), (-1,0)):
        rr, cc = (r+dr, c+dc)
        if (rr, cc) in current_route:
            continue
        if not 0 <= rr < nrow:
            continue
        if not 0 <= cc < ncol:
            continue
        obj = grid[rr][cc]
        if obj == "#":
            continue
        options.append((rr, cc))
    return options

def draw(rte: list):
    g = deepcopy(grid)
    for r, c in rte:
        if g[r][c] not in ("S", "E"):
            g[r][c] = "x"
    print("\n".join("".join(row) for row in g))
    print("-"*12)

sr, sc = i2rc(raw.find("S"), ncol)
er, ec = i2rc(raw.find("E"), ncol)

# Initial direction: East
dr, dc = (0, 1)

# Traverse the maze
r = sr
c = sc
forks = []
exits = set()
current_route = []
while True:
    current_route.append((r, c))
    if grid[r][c] == "E":
        exits.add(tuple(current_route))
        # print("exit. looking for other routes")
        # Return to a previous fork
        fork = forks[-1]
        current_route = fork["route"].copy()
        r, c = fork["opts"].pop()
        if not fork["opts"]:
            forks.pop()
        continue
    opts = routes(r, c, current_route)
    if not opts:
        # Dead end. Return to last fork and pick
        # a different route.
        # print("doubling back")
        try:
            fork = forks[-1]
            current_route = fork["route"].copy()
            r, c = fork["opts"].pop()
            if not fork["opts"]:
                # No more options at this fork
                forks.pop()
        except IndexError:
            # We have explored the entire maze!
            break
        continue
    r, c = opts.pop()
    if len(opts) > 0:
        # Mark the fork and take first option.
        # print("fork discovered")
        # draw(current_route)
        forks.append({"route": current_route.copy(),
                      "opts": opts})

def point(rte):
    pts = len(rte)-1
    # Start facing East
    dr, dc = (0,1)
    for i in range(1, len(rte)):
        drn = rte[i][0] - rte[i-1][0]
        dcn = rte[i][1] - rte[i-1][1]
        if (dr, dc) != (drn, dcn):
            pts += 1000
            dr = drn
            dc = dcn
    return pts

points = map(point, exits)
print(min(points))
# print(len(exits))
# for rte in exits:
#     draw(rte)
