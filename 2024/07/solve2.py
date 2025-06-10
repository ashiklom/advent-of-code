from operator import add, mul
from itertools import product

def concat(a, b):
    return int(str(a)+str(b))

with open("2024/07/input") as f:
    lines = f.read().splitlines()

def flist(n):
    return product((add, mul), repeat=n)

def flist2(n):
    return product((add, mul, concat), repeat=n)

def apply(fs, nums):
    nc = nums.copy()
    r = nc.pop(0)
    for f, num in zip(fs, nc):
        r = f(r, num)
    return r

def attempt(answer, nums):
    fs1 = set(flist(len(nums)-1))
    for fs in fs1:
        r = apply(fs, nums)
        if r == answer:
            return r

    fs2_all = set(flist2(len(nums)-1))
    fs2 = fs2_all.difference(fs1)
    for fs in fs2:
        r = apply(fs, nums)
        if r == answer:
            return r

    return 0

result = 0

for line in lines:
    answer_str, numlist = line.split(":")
    answer = int(answer_str)
    nums = list(map(int, numlist.split()))
    result += attempt(answer, nums)

print(result)
