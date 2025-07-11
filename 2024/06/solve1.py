from copy import deepcopy

with open("2024/06/input") as f:
    grid = [list(s.strip()) for s in f.readlines()]

nrow = len(grid)
ncol = len(grid[0])

result = deepcopy(grid)
# Find starting position
rc = (0, 0)
for idx,row in enumerate(result):
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

guard = result[rc[0]][rc[1]]

# Initial guard state
guard0 = guard
rc0 = rc

while True:
    result[rc[0]][rc[1]] = "X"
    nxt = go(guard, rc)
    if not (0 <= nxt[0] < nrow):
        break
    if not (0 <= nxt[1] < nrow):
        break
    target = result[nxt[0]][nxt[1]]
    if target == "#":
        guard = turn(guard)
    else:
        rc = nxt

solution = "\n".join("".join(row) for row in result)
print(solution.count("X"))
