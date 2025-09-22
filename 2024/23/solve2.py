from collections import defaultdict

# with open("./2024/23/testinput", "r") as f:
with open("./2024/23/input", "r") as f:
    raw = f.read().splitlines()

pairs = [s.split("-") for s in raw]

d = defaultdict(set)

for pair in pairs:
    d[pair[0]].add(pair[1])
    d[pair[1]].add(pair[0])

setpairs = [set(pair) for pair in pairs]

def mutual_intersect(net: set):
    result = net.copy()
    # Simple history to avoid infinite loops alternating between states
    hist = []
    while True:
        nmap = list(map(lambda x: d[x] | set([x]), result))
        mutual = set.intersection(*nmap)
        if result == mutual:
            break
        if mutual in hist:
            break
        result = mutual
        hist.append(mutual)
    return ",".join(sorted(result))

results = set(mutual_intersect(sp) for sp in setpairs)
print(sorted(results, key=len, reverse=True)[0])
