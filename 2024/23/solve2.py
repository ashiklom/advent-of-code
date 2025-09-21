from collections import defaultdict
from functools import reduce

with open("./2024/23/testinput", "r") as f:
# with open("./2024/23/input", "r") as f:
    raw = f.read().splitlines()

pairs = [s.split("-") for s in raw]

d = defaultdict(set)

for pair in pairs:
    d[pair[0]].add(pair[1])
    d[pair[1]].add(pair[0])

setpairs = [set(pair) for pair in pairs]

def mutual_intersect(net: set):
    result = net.copy()
    while True:
        nmap = map(lambda x: d[x], result)
        mutual = set(reduce(lambda x,y: x.intersection(y), nmap))
        if not mutual:
            break
        result |= mutual
    return ",".join(sorted(result))

net = set(["aq", "vc"])

results = set(mutual_intersect(sp) for sp in setpairs)
print(results)

# NOTE: Not quite -- `co,de,ka,ta` is the correct answer, but this has some 
# false positives (e.g., )
