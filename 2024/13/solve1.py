import re
from math import fabs

# fname = "2024/13/test"
fname = "2024/13/input"

with open(fname, "r") as f:
    raw = f.read()

buttons = raw.split("\n"*2)

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
    tol = 1e-6
    A = (X * by/bx - Y) / (ax * by/bx - ay)
    if fabs(A - round(A)) > tol:
        return None
    B = (X - ax*A) / bx
    if fabs(B - round(B)) > tol:
        return None
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

result = int(sum(cost(b) for b in buttons))
print(result)
# with open("2024/13/result1", "w") as f:
#     f.write(str(result))
