with open("input") as f:
    raw = [s.strip() for s in f.readlines()]

pairs = [s.split() for s in raw]
first_uns = [int(s[0]) for s in pairs]
first = sorted(first_uns)
second = sorted(int(s[1]) for s in pairs)
diff = [abs(a-b) for a,b in zip(first,second)]

print(sum(diff))

### Part 2

p2 = 0

for i in range(len(first_uns)):
    x = first_uns[i]
    n = second.count(x)
    p2 += x*n

print(p2)


