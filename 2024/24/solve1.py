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

# with open("2024/24/testinput_small", "r") as f:
# with open("2024/24/testinput_large", "r") as f:
with open("2024/24/input", "r") as f:
    raw = f.read()

raw_wires, raw_gates = [x.splitlines() for x in raw.split("\n\n")]

wires = dict()
for wire in raw_wires:
    key, val = wire.split(": ")
    wires[key] = int(val)

gates = [parse_gate(g) for g in raw_gates]
while gates:
    g = gates.pop(0)
    inputs = g[1]
    if (inputs[0] in wires) and (inputs[1] in wires):
        i0 = wires[inputs[0]]
        i1 = wires[inputs[1]]
        wires[g[2]] = g[0](i0, i1)
    else:
        gates.append(g)

zkeys = sorted((k for k in wires.keys() if k.startswith("z")), reverse=True)
zvals = [str(wires[z]) for z in zkeys]
zbits = "".join(zvals)
result = int(zbits, 2)
print(result)
