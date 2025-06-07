with open("2024/04/input") as f:
    rows = [s.strip() for s in f.readlines()]

def block(mat, r, c):
    if mat[r][c] != "A":
        return False
    try:
        tlbr = mat[r-1][c-1] + mat[r+1][c+1]
        if tlbr not in ("MS", "SM"):
            return False
        trbl = mat[r+1][c-1] + mat[r-1][c+1]
        if trbl not in ("MS", "SM"):
            return False
        return True
    except IndexError:
        return False

nrow = len(rows)
ncol = len(rows[0])

xmas = 0
for r in range(1, nrow):
    for c in range(1, ncol):
        xmas += block(rows, r, c)

print(xmas)

