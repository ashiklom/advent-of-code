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

# Part 2
def apply_rules2(page: list):
    pc = page.copy()
    result = []
    for _ in range(len(page)):
        item = pc.pop()
        r = rules[item]
        if any(rr in pc for rr in r):
            return False
    # All rules satisfied. Now, apply sorting.
    return int(result[len(result)//2])
