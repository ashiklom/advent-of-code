from copy import deepcopy

with open("2024/06/input") as f:
    grid = [list(s.strip()) for s in f.readlines()]

nrow = len(grid)
ncol = len(grid[0])

# Find starting position
rc = (0, 0)
for idx,row in enumerate(grid):
    for g in "^>v<":
        try:
            rc = (idx, row.index(g))
            break
        except ValueError:
            pass
else:
    ValueError("Guard not found.")

def go(guard, rc):
    r, c = rc
    match guard:
        case "^":
            r -= 1
        case ">":
            c += 1
        case "v":
            r += 1
        case "<":
            c -= 1
        case _:
            ValueError("Invalid guard")
    return (r, c)

def turn(guard):
    match guard:
        case "^":
            return ">"
        case ">":
            return "v"
        case "v":
            return "<"
        case "<":
            return "^"
        case _:
            ValueError("Invalid guard")

def is_loop(grid, guard0, rc0):
    guard = guard0
    rc = rc0
    history = set()
    while True:
        if (guard, rc) in history:
            return True
        nxt = go(guard, rc)
        if not (0 <= nxt[0] < nrow):
            return False
        if not (0 <= nxt[1] < ncol):
            return False
        target = grid[nxt[0]][nxt[1]]
        if target == "#":
            guard = turn(guard)
        else:
            history.add((guard, rc))
            rc = nxt

# Initial guard state
guard = grid[rc[0]][rc[1]]
guard0 = guard
rc0 = rc

loops = set()

step = 0

while True:
    if step > 0:
        # See if adding an obstacle at nxt creates a loop
        newgrid = deepcopy(grid)
        newgrid[rc[0]][rc[1]] = "#"
        if is_loop(newgrid, guard0, rc0):
            loops.add(rc)
    step += 1
    if step % 100 == 0:
        print(f"step {step}")
        print(f"current loops: {len(loops)}")
    nxt = go(guard, rc)
    if not (0 <= nxt[0] < nrow):
        break
    if not (0 <= nxt[1] < nrow):
        break
    target = grid[nxt[0]][nxt[1]]
    if target == "#":
        guard = turn(guard)
    else:
        rc = nxt

print(len(loops))

# Part 2
# At each X position of the solved grid (except
# starting), try placing an object.
# Then, run a simulation and see if it causes a loop.
# Loop detection: If you pass the same location in the 
# same direction, you're in a loop.
# So just replace X with guard as the state marker, 
# and exit if the next guard matches the saved state.
