from itertools import chain
with open("2024/09/input") as f:
    raw = f.read().strip()

files = map(int, raw[::2])
# Pad end with zero so I can zip
free = map(int, raw[1::2]+"0")

fids = [[f] * n for f,n in enumerate(files)]
# fids_comb = list(chain.from_iterable(fids))

space = [[None] * n for n in free]

together = [f + s for f, s in zip(fids, space)]
big = list(chain.from_iterable(together))

def strip_none(lst):
    while True:
        if lst[-1] is None:
            lst.pop()
        else:
            break

i = 0
while True:
    if big[i] is None:
        strip_none(big)
        try:
            big[i] = big[-1]
            big[-1] = None
        except IndexError:
            break
    i += 1

result = sum(idx * f for idx, f in enumerate(big))
print(result)
