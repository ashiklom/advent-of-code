from copy import deepcopy

# fname = "2024/15/test_small2"
# fname = "2024/15/test_big"
fname = "2024/15/input"

with open(fname, "r") as f:
    raw = f.read().strip()

rawgrid, moves = raw.split("\n"*2)
moves = moves.replace("\n", "")

def n2rc(n: int, ncol: int) -> tuple[int, int]:
    r = n // (ncol+1)
    c = n - (ncol+1)*r
    return r, c


def char2dyx(char: str):
    match char:
        case ">":
            return (0,1)
        case "v":
            return (1,0)
        case "<":
            return (0,-1)
        case "^":
            return (-1,0)
        case _:
            raise ValueError(f"Invalid char: {char}")

def expand(rowstring: str):
    trans = {
        "#": "##",
        ".": "..",
        "O": "[]",
        "@": "@."
    }
    result = []
    for c in rowstring:
        result += list(trans[c])
    return result


class Grid:
    def __init__(self, string: str):
        rows = string.split("\n")
        self.grid = list(map(expand, rows))
        # Expand
        self.nrow = len(self.grid)
        self.ncol = len(self.grid[0])
        # Find robot
        found = False
        for r, row in enumerate(self.grid):
            for c, char in enumerate(row):
                if char == "@":
                    self.robot_r = r
                    self.robot_c = c
                    found = True
                    break
            if found:
                break
        self.grid[self.robot_r][self.robot_c] = "."

    def iswall(self, r, c):
        return self.grid[r][c] == "#"

    def isbox(self, r, c):
        x = self.grid[r][c]
        return x in ("[", "]")

    def getbox(self, r: int, c: int) -> set[tuple[int,int,str]]:
        x = self.grid[r][c]
        if x == "[":
            return {(r,c, "["), (r,c+1, "]")}
        if x == "]":
            return {(r,c-1, "["), (r,c, "]")}
        raise ValueError("Not a box")

    def findall(self, char: str) -> list:
        results = []
        for r, row in enumerate(self.grid):
            for c, x in enumerate(row):
                if x == char:
                    results.append((r, c))
        return results


    def boxrow(self, r: int, c0: int, dx: int) -> None:
        """
        Search forward. If you hit a wall, end.
        If you hit empty space, shift everything.
        """
        c = c0
        stuff = ["."]
        obj = self.grid[r][c]
        while obj != ".":
            stuff.append(obj)
            c += dx
            if self.iswall(r, c):
                return
            obj = self.grid[r][c]

        # Shift boxes and robot
        c += dx
        self.robot_c = c0
        self.grid[r][c0:c:dx] = stuff

    def boxcol(self, r0: int, c: int, dy: int) -> None:
        """
        For each box piece in the current row,
        need to find any boxes in the next row.
        Once we find open space, shift everything.
        """
        r = r0
        bx = self.getbox(r, c)
        boxrows = [(r, bx)]
        while True:
            r += dy
            cols = {c for _,c,_ in bx}
            bx = set()
            for cc in cols:
                if self.iswall(r, cc):
                    return
                if self.isbox(r, cc):
                    bx.update(self.getbox(r, cc))
            if not bx:
                break
            boxrows.append((r, bx))

        # Now, move
        self.robot_r = r0
        self.grid[r0][c] = "."
        while boxrows:
            r, boxes = boxrows.pop()
            for _, c, char in boxes:
                self.grid[r+dy][c] = char
                self.grid[r][c] = "."

    def move(self, char: str) -> None:
        dy, dx = char2dyx(char)
        r = self.robot_r + dy
        c = self.robot_c + dx
        if self.iswall(r, c):
            return
        if self.isbox(r, c):
            if dx == 0:
                self.boxcol(r, c, dy)
            elif dy == 0:
                self.boxrow(r, c, dx)
            return
        self.robot_r = r
        self.robot_c = c

    def boxes(self) -> list[tuple]:
        return self.findall("[")

    def gps(self) -> int:
        boxes = self.boxes()
        return sum(r*100 + c for r, c in boxes)

    def __repr__(self):
        g = deepcopy(self.grid)
        g[self.robot_r][self.robot_c] = "@"
        return "\n".join("".join(row) for row in g)

grid = Grid(rawgrid)
# print(grid)
# print("+++")
for m in moves:
    # print(m)
    # print("---")
    grid.move(m)
    # print(grid)
    # print("---"*3)

# print(grid)
print(grid.gps())
# 1430439
