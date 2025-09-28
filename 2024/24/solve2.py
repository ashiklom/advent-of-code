#!/usr/bin/env python

from copy import deepcopy
from itertools import chain

def parse_gate(g):
    gsp = g.split(" ")
    return (gsp[-1], (set([gsp[0], gsp[2]]), gsp[1]))

with open("2024/24/input", "r") as f:
    raw = f.read()

_, raw_gates = [x.splitlines() for x in raw.split("\n\n")]
gates = dict(parse_gate(g) for g in raw_gates)

n = 45
zs = [f"z{i:02d}" for i in range(0, n+1)]
zgates = {z: gates[z] for z in zs}

# First, the obvious check: Z bit is always the result of an XOR.
# If that's not the case, we know the wires are wrong.
bad_zgates = {k: v for k,v in zgates.items() if v[1] != "XOR"}
bad_zgates.pop("z45")

def find_wire(target, gates):
    for k, v in gates.items():
        if v == target:
            return k
    else:
        raise ValueError("Not found")

def find_correct_wire(wrong_wire, gate_dict):
    kx = wrong_wire.replace("z", "x")
    ky = wrong_wire.replace("z", "y")
    pt1_target = (set([kx, ky]), "XOR")
    pt1_wire = find_wire(pt1_target, gate_dict)

    # Find all XOR wires involving pt1_wire
    targets_w_pt1 = {}
    for k, v in gate_dict.items():
        if (v[1] == "XOR") and (pt1_wire in v[0]):
            targets_w_pt1[k] = v
    if len(targets_w_pt1) > 1:
        raise ValueError("Multiple possible wires")

    right_wire = next(iter(targets_w_pt1.keys()))
    return (wrong_wire, right_wire)

wire_swaps = []
for wire in bad_zgates.keys():
    wire_swaps.append(find_correct_wire(wire, gates))

gates_fixed = deepcopy(gates)
for wrong, right in wire_swaps:
    gates_fixed[wrong] = gates[right]
    gates_fixed[right] = gates[wrong]

# Now, a less obvious check: One of the wires involved in the Z bit should be 
# an XOR of the corresponding X and Y bits.
# We already found 3 bad wires above, so we only need to find one more now.
for z in zs[2:n]:
    x = z.replace("z", "x")
    y = z.replace("z", "y")
    gz = gates_fixed[z]
    wires = list(gz[0])
    wire_vals = [gates_fixed[w] for w in wires]
    xy = ({x, y}, "XOR")
    if xy not in wire_vals:
        print(f"Bad wire: {z}")
        bad_wire = z
        break
    wire_vals.pop(wire_vals.index(xy))
    if wire_vals[0][1] != "OR":
        print(f"Bad wire: {z}")
else:
    raise ValueError("No bad wires found")

kx = bad_wire.replace("z", "x")
ky = bad_wire.replace("z", "y")
pt1_target = (set([kx, ky]), "XOR")
pt1_wire = find_wire(pt1_target, gates_fixed)
bad_inputs = list(gates_fixed[bad_wire][0])

for bad_input in bad_inputs:
    gtype = gates_fixed[bad_input][1]
    if gtype not in ("OR", "XOR"):
        wrong_wire = bad_input
        break
else:
    raise ValueError("No bad inputs found")

wire_swaps.append((pt1_wire, wrong_wire))

answer = sorted(chain.from_iterable(wire_swaps))
print(",".join(answer))
