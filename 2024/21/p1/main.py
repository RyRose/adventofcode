from typing import List, Optional, Tuple
import heapq
import attrs
import collections
from enum import Enum

# robot #1 - 40 degrees
#     +---+---+
#     | ^ | A |
# +---+---+---+
# | < | v | > |
# +---+---+---+

# robot #2 - radiation
#     +---+---+
#     | ^ | A |
# +---+---+---+
# | < | v | > |
# +---+---+---+

# robot #3 - numeric keypad
# +---+---+---+
# | 7 | 8 | 9 |
# +---+---+---+
# | 4 | 5 | 6 |
# +---+---+---+
# | 1 | 2 | 3 |
# +---+---+---+
#     | 0 | A |
#     +---+---+


DIR = [
    ["", "^", "A"],
    ["<", "v", ">"],
]

NUM = [
    ["7", "8", "9"],
    ["4", "5", "6"],
    ["1", "2", "3"],
    ["", "0", "A"],
]


class Direction(Enum):

    UP = 0
    RIGHT = 1
    DOWN = 2
    LEFT = 3

    @classmethod
    def from_char(cls, char: str) -> "Direction":
        if char == "^":
            return cls.UP
        elif char == ">":
            return cls.RIGHT
        elif char == "v":
            return cls.DOWN
        elif char == "<":
            return cls.LEFT
        raise ValueError(f"Invalid char: {char}")

    def move(self, x: int, y: int) -> Tuple[int, int]:
        if self == Direction.UP:
            return x, y - 1
        elif self == Direction.RIGHT:
            return x + 1, y
        elif self == Direction.DOWN:
            return x, y + 1
        elif self == Direction.LEFT:
            return x - 1, y
        raise ValueError(f"Invalid direction: {self}")

    def movep(self, p: Tuple[int, int]) -> Tuple[int, int]:
        x, y = p
        return self.move(x, y)

    def char(self) -> str:
        if self == Direction.UP:
            return "^"
        elif self == Direction.RIGHT:
            return ">"
        elif self == Direction.DOWN:
            return "v"
        elif self == Direction.LEFT:
            return "<"
        raise ValueError(f"Invalid direction: {self}")


@attrs.frozen(auto_attribs=True, order=True)
class State:
    robot1: Tuple[int, int]
    robot2: Tuple[int, int]
    robot3: Tuple[int, int]
    out: str = ""

    def valid(self):
        check = (
            (self.robot1, DIR, (0, 0)),
            (self.robot2, DIR, (0, 0)),
            (self.robot3, NUM, (0, 3)),
        )
        for (x, y), grid, p in check:
            if (x, y) == p:
                return False
            if not (0 <= x < len(grid[0])):
                return False
            if not (0 <= y < len(grid)):
                return False
        return True

    def press(self) -> Optional["State"]:
        if not self.valid():
            return None

        check = (
            ("robot1", self.robot1),
            ("robot2", self.robot2),
            ("robot3", self.robot3),
        )

        for (_, (x, y)), (name, after) in zip(check, check[1:]):
            val = DIR[y][x]
            if not val:
                return None
            if val == "A":
                continue
            dir = Direction.from_char(val)
            ret = attrs.evolve(
                self,
                **{
                    name: dir.movep(after),
                },
            )
            return ret if ret.valid() else None

        robot3 = NUM[self.robot3[1]][self.robot3[0]]
        if not robot3:
            return None
        return attrs.evolve(
            self,
            out=self.out + robot3,
        )

    def move(self, direction: Direction) -> "State":
        return attrs.evolve(
            self,
            robot1=direction.movep(self.robot1),
        )


def solution0(input_value: str, start: State) -> Tuple[State, int]:
    queue = [(0, start)]
    distances = collections.defaultdict(lambda: float("inf"))
    while queue:
        dist, node = heapq.heappop(queue)
        if not node.valid():
            continue
        if not input_value.startswith(node.out):
            continue

        prev_dist = distances[node]
        if dist >= prev_dist:
            continue
        distances[node] = dist
        if node.out == input_value:
            return node, dist

        val = node.press()
        if val:
            heapq.heappush(queue, (dist + 1, val))
        for dir in Direction:
            heapq.heappush(queue, (dist + 1, node.move(dir)))
    raise ValueError("No solution found")


def main():
    inputs = []
    with open("./data/test.in") as f:
        for line in f.readlines():
            inputs.append(line.strip())

    initial = State(
        robot1=(2, 0),
        robot2=(2, 0),
        robot3=(2, 3),
    )
    complexity = 0
    for inp in inputs:
        state, dist = solution0(inp, initial)
        print("input", inp, "dist", dist, "state", state)
        complexity += dist * int(inp[:-1])
    print(complexity)


if __name__ == "__main__":
    main()
