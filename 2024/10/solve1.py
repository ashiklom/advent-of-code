#!/usr/bin/env python3

class Matrix:
    def __init__(self, data: list[list[int]]):
        self.data = data
        self.nrow = len(data)
        self.ncol = len(data[0])
        self._it = 0

    def __getitem__(self, idxs):
        if not isinstance(idxs, tuple):
            idxs = (idxs, slice(self.ncol))
        r, c = idxs
        if isinstance(r, int):
            r = slice(r, r+1)
        if isinstance(c, int):
            c = slice(c, c+1)
        rowsub = self.data[r]
        result = [row[c] for row in rowsub]
        return Matrix(result)
    
    def val(self):
        if not self.nrow == 1 and self.ncol == 1:
            raise ValueError("Multiple values")
        return self.data[0][0]

    def __repr__(self):
        out = "\n".join("".join(map(str, row)) for row in self.data)
        return out

    def __iter__(self):
        return self

    def __next__(self):
        r = self._it // self.nrow
        if r == self.nrow:
            raise StopIteration
        else:
            c = self._it % self.ncol
            self._it += 1
            return (r, c, self[r,c].val())

    def search(self, r, c, summits: set):
        x = self[r, c].val()
        if x == 9:
            summits.add((r, c))
            return summits
        nxt = x + 1
        if (c-1) >= 0 and self[r, c-1].val() == nxt:
            summits = self.search(r, c-1, summits)
        if (c+1) < self.ncol and self[r, c+1].val() == nxt:
            summits = self.search(r, c+1, summits)
        if (r-1) >= 0 and self[r-1, c].val() == nxt:
            summits = self.search(r-1, c, summits)
        if (r+1) < self.nrow and self[r+1, c].val() == nxt:
            summits = self.search(r+1, c, summits)
        return summits

# with open("2024/10/test", "r") as f:
with open("2024/10/input", "r") as f:
    raw = f.read().strip().split("\n")

dat = Matrix([list(map(int, row)) for row in raw])

result = {}

for r, c, x in dat:
    if x != 0:
        continue
    summits = dat.search(r, c, set())
    if summits:
        result[(r, c)] = (len(summits), summits)

print(sum(r[0] for r in result.values()))
