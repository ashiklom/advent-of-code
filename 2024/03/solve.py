import re

pat = re.compile(r"mul\((\d{1,3}),(\d{1,3})\)")

with open("input") as f:
    raw = f.read()

matches = pat.findall(raw)

print(sum(int(a)*int(b) for a,b in matches))

# Part 2
pat2 = re.compile(r"(do\(\))|(don't\(\))|(?:mul\((\d{1,3}),(\d{1,3})\))")

mat2 = pat2.findall(raw)
# print(mat2[:30])

out2 = 0
do = True
for m in mat2:
    if m[0] == "do()":
        do = True
    if m[1] == "don't()":
        do = False
    if do and not ('' in m[2:4]):
        out2 += int(m[2]) * int(m[3])

print(out2)



