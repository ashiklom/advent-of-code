import re

fname = "2024/13/test"
# fname = "2024/13/input"

with open(fname, "r") as f:
    raw = f.read()

buttons = raw.split("\n"*2)
# print(buttons)

def parse_button(string):
    pat = re.compile(
        r"Button A: X\+(\d+), Y\+(\d+)\n"
        + r"Button B: X\+(\d+), Y\+(\d+)\n"
        + r"Prize: X=(\d+), Y=(\d+)"
    )
    m = pat.match(string)
    if m:
        return tuple(map(int, m.groups()))
    else:
        raise ValueError("No match")


# Solve by Gaussian elimination
def solve(ax, ay, bx, by, X, Y):
    A = (X * by/bx - Y) / (ax * by/bx - ay)
    if A % 1 != 0:
        return None
    B = (X - ax*A) / bx
    if B % 1 != 0:
        return None
    checkX = ax * A + bx * B
    checkY = ay * A + by * B
    if checkX != X or checkY != Y:
        raise ValueError(f"{(checkX-X, checkY-Y)}")
    return (A, B)

def solve_button(button: str):
    params = parse_button(button)
    return solve(*params)

def cost(button: str):
    solution = solve_button(button)
    if solution:
        return solution[0]*3 + solution[1]*1
    else:
        return 0

# print([solve_button(b) for b in buttons])
result = int(sum(cost(b) for b in buttons))
print(result)
with open("2024/13/result1", "w") as f:
    f.write(str(result))
