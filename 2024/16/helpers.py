#!/usr/bin/env python

def turns(dr: int, dc: int, going_r: int, going_c: int) -> int:
    """
    Minimum number of turns needed given a distance and a heading.
    """
    if dr == 0:
        target_r = 0
    else:
        target_r = int(dr / abs(dr))

    if dc == 0:
        target_c = 0
    else:
        target_c = int(dc / abs(dc))

    if dr == 0:
        # Same row
        if going_r == 0:
            # Moving horizontally
            if going_c == target_c:
                # Same direction; no turns
                return 0
            # Opposite direction; 3 turns to turn around
            else:
                return 3
        # Moving vertically
        else:
            # Going vertical. Need to go horizontal. Only need 1 turn.
            return 1

    elif dc == 0:
        # Same logic, but for same column
        if going_c == 0:
            if going_r == target_r:
                return 0
            else:
                return 3
        else:
            return 1

    # Different row and column
    # 1 turn if heading in the right direction; 2 turns if in the wrong direction
    else:
        if going_r == 0:
            if going_c == target_c:
                return 1
            else:
                return 2
        if going_c == 0:
            if going_r == target_r:
                return 1
            else:
                return 2

    raise ValueError("Can't calcluate turns")


def i2rc(i, ncol):
    r = i // (ncol + 1)
    c = i - (ncol + 1) * r
    return r, c

