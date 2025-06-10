from operator import add, mul
from itertools import product

with open("2024/07/input") as f:
    lines = f.read().splitlines()


def flist(n):
    return product((add, mul), repeat=n)

def apply(fs, nums):
    nc = nums.copy()
    r = nc.pop(0)
    for f, num in zip(fs, nc):
        r = f(r, num)
    return r

def attempt(answer, nums):
    for fs in flist(len(nums)-1):
        r = apply(fs, nums)
        if r == answer:
            return r
    else:
        return 0

result = 0

for line in lines:
    answer_str, numlist = line.split(":")
    answer = int(answer_str)
    nums = list(map(int, numlist.split()))
    result += attempt(answer, nums)

print(result)
