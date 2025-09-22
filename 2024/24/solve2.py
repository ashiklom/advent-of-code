#!/usr/bin/env python

from operator import and_, or_, xor

def parse_gate(raw_gate: str):
    sp = raw_gate.split(" ")
    inputs = (sp[0], sp[2])
    target = sp[4]
    match sp[1]:
        case "AND":
            gatefn = and_
        case "OR":
            gatefn = or_
        case "XOR":
            gatefn = xor
        case _:
            raise ValueError(f"Invalid string '{raw_gate}'")
    return (gatefn, inputs, target)

with open("2024/24/testinput_part2", "r") as f:
# with open("2024/24/input", "r") as f:
    raw = f.read()

_, raw_gates = [x.splitlines() for x in raw.split("\n\n")]
gates = [parse_gate(g) for g in raw_gates]

def nums2wires(a: int, b: int, bitmax: int = 6):
    MAX = 2**bitmax
    if a >= MAX:
        raise ValueError(f"{a=} >= {MAX}")
    if b >= 32:
        raise ValueError(f"{b=} >= {MAX}")
    abin = list(f"{a:0{bitmax}b}")
    bbin = list(f"{b:0{bitmax}b}")
    c = a + b
    cbin = list(f"{c:0{bitmax}b}")
    wires = dict()
    for i, abit in enumerate(reversed(abin)):
        wires[f"x0{i}"] = int(abit)
    for i, bbit in enumerate(reversed(bbin)):
        wires[f"y0{i}"] = int(bbit)
    for i, cbit in enumerate(reversed(cbin)):
        wires[f"z0{i}"] = int(cbit)
    return wires

wires_t1 = nums2wires(27, 3)

########################################

# IDEA: Given correct X, Y, and Z (from above),
# try to solve for X, Y, and Z (one missing) and
# compare the answers.
# To go from Z to X and Y, I'll need to reverse the gates.

def solve(gate_orig, wires_orig):
    gates = gate_orig.copy()
    wires = wires_orig.copy()
    ng = len(gates)
    ctr = 0
    while gates and ctr < ng:
        g = gates.pop(0)
        inputs = g[1]
        if (inputs[0] in wires) and (inputs[1] in wires):
            i0 = wires[inputs[0]]
            i1 = wires[inputs[1]]
            wires[g[2]] = g[0](i0, i1)
            ng = len(gates)
            ctr = 0
        else:
            gates.append(g)
            ctr += 1
    return wires

w = solve(gates, wires_t1)

def compare_wires(w0, w1):
    diffs = {}
    for k in w0.keys():
        if w0[k] != w1[k]:
            diffs[k] = (w0[k], w1[k])
    return diffs

compare_wires(wires_t1, w)


zkeys = sorted((k for k in wires.keys() if k.startswith("z")), reverse=True)
zvals = [str(wires[z]) for z in zkeys]
zbits = "".join(zvals)
result = int(zbits, 2)
print(result)
