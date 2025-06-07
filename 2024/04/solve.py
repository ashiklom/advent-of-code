# import re

def count(s):
    return s.count("XMAS") + s.count("SAMX")

with open("2024/04/input") as f:
    rows = [s.strip() for s in f.readlines()]

xmas = 0
xmas += sum(count(row) for row in rows) 

nrow = len(rows)
ncol = len(rows[0])

# Scan cols
for c in range(ncol):
    col = "".join([rows[r][c] for r in range(nrow)])
    xmas += count(col)

# Diagonals
for r in range(nrow):
    for c in range(ncol):
        # Down right
        dri = zip(range(r,r+4), range(c,c+4))
        try:
            dr = "".join(rows[a][b] for a,b in dri)
        except IndexError:
            dr = ""
        xmas += count(dr)
        # Down left
        dli = zip(range(r,r+4), range(c+3,c-1,-1))
        try:
            dl = "".join(rows[a][b] for a,b in dli)
        except IndexError:
            dl = ""
        xmas += count(dl)

print(xmas)
