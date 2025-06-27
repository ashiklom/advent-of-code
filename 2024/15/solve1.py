from copy import deepcopy

# fname = "2024/15/test_small"
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

class Grid:
    def __init__(self, string: str):
        robot_loc = string.find("@")
        rows = string.split("\n")
        self.grid = list(map(list, rows))
        self.nrow = len(self.grid)
        self.ncol = len(self.grid[0])
        self.robot_r = robot_loc // (self.ncol + 1)
        self.robot_c = robot_loc - (self.ncol + 1)*self.robot_r
        if self.grid[self.robot_r][self.robot_c] != "@":
            raise ValueError("Misplaced robot.")
        self.grid[self.robot_r][self.robot_c] = "."

    def findall(self, char: str) -> list:
        results = []
        for r, row in enumerate(self.grid):
            for c, x in enumerate(row):
                if x == char:
                    results.append((r, c))
        return results

    def boxline(self, r: int, c: int, dy: int, dx: int) -> None:
        boxes = [(r, c)]
        while True:
            r += dy
            c += dx
            obj = self.grid[r][c]
            match obj:
                case "O":
                    boxes.append((r, c))
                case "#":
                    return
                case ".":
                    # Shift boxes and robot
                    r0, c0 = boxes[0]
                    self.robot_r = r0
                    self.robot_c = c0
                    self.grid[r0][c0] = "."
                    for br, bc in boxes:
                        br += dy
                        bc += dx
                        self.grid[br][bc] = "O"
                    return

    def move(self, char: str):
        dy, dx = char2dyx(char)
        r = self.robot_r + dy
        c = self.robot_c + dx
        obj = self.grid[r][c]
        match obj:
            case ".":
                # No obstruction. Just move.
                self.robot_r = r
                self.robot_c = c
                return
            case "#":
            # Wall. Don't move.
                return
            case "O":
            # Handle boxes
                self.boxline(r, c, dy, dx)
                return
            case _:
                raise ValueError("Invalid state")

    def boxes(self) -> list[tuple]:
        return self.findall("O")

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
