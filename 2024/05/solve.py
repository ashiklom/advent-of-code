with open("2024/05/input") as f:
    raw = [s.strip() for s in f.readlines()]

blank = raw.index("")

rules_list = [s.split("|") for s in raw[0:blank]]
rules = {}
for rule in rules_list:
    if rule[0] not in rules:
        rules[rule[0]] = [rule[1]]
    else:
        rules[rule[0]].append(rule[1])

pages = [s.split(",") for s in raw[(blank+1):]]

def apply_rules(page: list):
    pc = page.copy()
    for _ in range(len(page)):
        item = pc.pop()
        r = rules[item]
        if any(rr in pc for rr in r):
            return False
    # All rules satisfied
    return int(page[len(page)//2])

results = [apply_rules(page) for page in pages]
print(sum(results))

invalid = [page for i, page in enumerate(pages) if results[i] is False]
# print(len(pages))
# print(len(valid))
# print(valid[0:5])

import graphlib

# Part 2
def sort_rules(page: list):
    ps = set(page)
    rlist = {item: set(rules[item]).intersection(ps) for item in ps}
    result = list(graphlib.TopologicalSorter(rlist).static_order())
    return int(result[len(result)//2])

results2 = [sort_rules(page) for page in invalid]
print(sum(results2))
