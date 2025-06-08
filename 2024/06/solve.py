with open("2024/06/input") as f:
    grid = [list(s.strip()) for s in f.readlines()]

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

# Initial guard state
nrow = len(grid)
ncol = len(grid[0])
guard = grid[rc[0]][rc[1]]
while True:
    grid[rc[0]][rc[1]] = "X"
    nxt = go(guard, rc)
    if not (0 <= nxt[0] <= nrow):
        break
    if not (0 <= nxt[1] <= nrow):
        break
    target = grid[nxt[0]][nxt[1]]
    if target == "#":
        guard = turn(guard)
    else:
        rc = nxt

# grid[irc[0]][irc[1]] = guard0
# with open("2024/06/test", "w") as f:
#     f.write("\n".join(["".join(row) for row in grid]))

result = "\n".join("".join(row) for row in grid)
print(result.count("X"))
