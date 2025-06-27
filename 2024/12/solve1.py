#/usr/bin/env python3

with open("2024/12/test", "r") as f:
    raw = f.read().strip().split("\n")

# print("\n".join(raw))
nrow = len(raw)
ncol = len(raw[0])

scanned = set()

def get_region(r0, char):
    region = set()
    for r in range(r0, nrow):
        row = raw[r]
        try:
            c0 = row.index(char)
        except ValueError:
            break
        for c in range(c0, ncol):
            if row[c] == char:
                region.add((r, c))
                scanned.add((r, c))
    return (char, region)

regions = []
for r, row in enumerate(raw):
    for c, char in enumerate(row):
        if (r,c) not in scanned:
            regions.append(get_region(r, char))

print(regions)
# Area: number of cells
# Perimeter: For each cell, 4 - number of
# adjacent cells in the same region.

