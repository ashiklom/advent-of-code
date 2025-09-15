#!/usr/bin/env python

def mix_prune(num: int, secret: int):
    PRUNE = 16_777_216
    result = num ^ secret
    return result % PRUNE

def next_secret(secret: int) -> int:
    step1 = mix_prune(secret * 64, secret)
    step2 = mix_prune(step1 // 32, step1)
    step3 = mix_prune(step2 * 2048, step2)
    return step3

def do_secret(secret: int, n: int):
    for _ in range(n):
        secret = next_secret(secret)
    return secret

# assert do_secret(123, 10) == 5908254

test2_in = [1, 10, 100, 2024]
test2_out = {i: do_secret(i, 2000) for i in test2_in}

# assert test2_out == {
#     1: 8685429,
#     10: 4700978,
#     100: 15273692,
#     2024: 8667524
# }

with open("2024/22/input") as f:
    raw = f.read().splitlines()

input = list(map(int, raw))
output = sum(do_secret(i, 2000) for i in input)
print(output)
