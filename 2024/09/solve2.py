#!/usr/bin/env python3

with open("2024/09/input") as f:
    raw = f.read().strip()

# raw = "2333133121414131402"

class Block:
    def __init__(self, contents: list):
        self.size = len(contents)
        self.contents = contents
        self.free = self.contents.count(None)
        self.start = 0

    def is_empty(self) -> bool:
        return self.free == self.size
    
    def fill(self, values: list):
        nv = len(values)
        if nv > self.free:
            ValueError("Not enough space.")
        self.contents[self.start:(self.start+nv)]  = values
        self.start += nv
        self.free -= nv

    def empty(self) -> list:
        out = self.contents
        self.contents = [None for _ in range(self.size)]
        self.start = 0
        self.free = self.size
        return out

    def __repr__(self) -> str:
        return f"({self.free}): {self.contents}"

def none2zero(x):
    if x is None:
        return 0
    return x

files = list(map(int, raw[::2]))
free = list(map(int, raw[1::2])) + [0]

fids = [Block([f]*n) for f,n in enumerate(files)]
freeb = [Block([None]*n) for n in free]
# Do this with chain?
blocks: list[Block] = []
for fid,fr in zip(fids, freeb):
    blocks += [fid]
    blocks += [fr]

# NOTE: Attempt to move each block only once.
# If it doesn't fit anywhere, skip and move on.
# So, do a for loop over the fids in reverse and
# try to fill; not the other way.
for i in range(len(blocks)-1, -1, -1):
    block = blocks[i]
    if block.is_empty():
        continue
    for target in blocks[:i]:
        if not target.free:
            continue
        if block.size <= target.free:
            target.fill(block.empty())
            break

# Fill the result
result = []
for b in blocks:
    result += map(none2zero, b.contents)

output = sum(idx * f for idx, f in enumerate(result))
print(output)
