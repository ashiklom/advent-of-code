from collections import defaultdict
from itertools import chain

def get_networks(base, d):
    bset = set([base])
    look = d[base]
    networks = set()
    for item in look:
        # if network and (item in network):
        #     continue
        check = d[item] - bset
        for c in check:
            dc = d[c]
            if item in dc and base in dc:
                networks.add(tuple(sorted([base, item, c])))
    return networks

def has_t(net):
    for n in net:
        if n.startswith("t"):
            return True
    return False    

# aq,cg,yn
# aq,vc,wq
# co,de,ka
# co,de,ta
# co,ka,ta
# de,ka,ta
# kh,qp,ub
# qp,td,wh
# tb,vc,wq
# tc,td,wh
# td,wh,yn
# ub,vc,wq

# with open("./2024/23/testinput", "r") as f:
with open("./2024/23/input", "r") as f:
    raw = f.read().splitlines()

pairs = [s.split("-") for s in raw]

d = defaultdict(set)

for pair in pairs:
    d[pair[0]].add(pair[1])
    d[pair[1]].add(pair[0])

uniq = set(chain.from_iterable(pairs))
networks = set()
while uniq:
    k = uniq.pop()
    net = get_networks(k, d)
    for n in net:
        networks.add(n)

# ns = sorted(sorted(n) for n in networks)
# print(ns)
# print(len(networks))
print(sum(has_t(net) for net in networks))
