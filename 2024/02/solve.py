def parse(line):
    return [int(s) for s in line.split()]

def is_safe(num):
    for i in range(len(num)-1):
        diff = num[i+1] - num[i]
        if not (1 <= abs(diff) <= 3):
            return False
        if i == 0:
            desc = diff < 0
        else:
            if desc != (diff < 0):
                return False
    return True

with open("input") as f:
    lines = [parse(s.strip()) for s in f.readlines()]

# Part 1
safes = [is_safe(line) for line in lines]
print(sum(safes))

# Part 2
def try_again(line):
    if is_safe(line):
        return True
    for i in range(len(line)):
        l = line.copy()
        l.pop(i)
        safe = is_safe(l)
        if safe:
            return True
    return False

print(sum(try_again(line) for line in lines))

