from typing import List, Optional, Set, Tuple
import heapq
import attrs
import collections
from enum import Enum
import itertools
import functools

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

# In total, there are three shortest possible sequences of button presses on this directional keypad that would cause the robot to type 029A: <A^A>^^AvvvA, <A^A^>^AvvvA, and <A^A^^>AvvvA.

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


@attrs.frozen(auto_attribs=True, order=True, slots=True)
class State:
    robot01: Tuple[int, int]
    robot02: Tuple[int, int]
    robot03: Tuple[int, int]
    robot04: Tuple[int, int]
    robot05: Tuple[int, int]
    robot06: Tuple[int, int]
    robot07: Tuple[int, int]
    robot08: Tuple[int, int]
    robot09: Tuple[int, int]
    robot10: Tuple[int, int]
    robot11: Tuple[int, int]
    robot12: Tuple[int, int]
    robot13: Tuple[int, int]
    robot14: Tuple[int, int]
    robot15: Tuple[int, int]
    robot16: Tuple[int, int]
    robot17: Tuple[int, int]
    robot18: Tuple[int, int]
    robot19: Tuple[int, int]
    robot20: Tuple[int, int]
    robot21: Tuple[int, int]
    robot22: Tuple[int, int]
    robot23: Tuple[int, int]
    robot24: Tuple[int, int]
    robot25: Tuple[int, int]
    endrobot: Tuple[int, int]
    out: str = ""

    def press(self) -> Optional["State"]:
        check = (
            ("robot01", self.robot01),
            ("robot02", self.robot02),
            ("robot03", self.robot03),
            ("robot04", self.robot04),
            ("robot05", self.robot05),
            ("robot06", self.robot06),
            ("robot07", self.robot07),
            ("robot08", self.robot08),
            ("robot09", self.robot09),
            ("robot10", self.robot10),
            ("robot11", self.robot11),
            ("robot12", self.robot12),
            ("robot13", self.robot13),
            ("robot14", self.robot14),
            ("robot15", self.robot15),
            ("robot16", self.robot16),
            ("robot17", self.robot17),
            ("robot18", self.robot18),
            ("robot19", self.robot19),
            ("robot20", self.robot20),
            ("robot21", self.robot21),
            ("robot22", self.robot22),
            ("robot23", self.robot23),
            ("robot24", self.robot24),
            ("robot25", self.robot25),
            ("endrobot", self.endrobot),
        )

        for (_, (x, y)), (name, after) in zip(check, check[1:]):
            val = DIR[y][x]
            if not val:
                return None
            if val == "A":
                continue
            dir = Direction.from_char(val)
            next = dir.movep(after)
            if name == "endrobot":
                if not (0 <= next[0] < len(NUM[0])):
                    return None
                if not (0 <= next[1] < len(NUM)):
                    return None
                if next == (0, 3):
                    return None
            else:
                if not (0 <= next[0] < len(DIR[0])):
                    return None
                if not (0 <= next[1] < len(DIR)):
                    return None
                if next == (0, 0):
                    return None
            ret = attrs.evolve(self, **{name: next})
            return ret

        endrobot = NUM[self.endrobot[1]][self.endrobot[0]]
        if not endrobot:
            return None
        return attrs.evolve(
            self,
            out=self.out + endrobot,
        )

    def move(self, direction: Direction) -> Optional["State"]:
        next = direction.movep(self.robot01)
        if not (0 <= next[0] < len(DIR[0])):
            return None
        if not (0 <= next[1] < len(DIR)):
            return None
        return attrs.evolve(self, robot01=next)


def solution0(input_value: str, start: State) -> Tuple[State, int]:
    queue = [(0, start)]
    distances = collections.defaultdict(lambda: float("inf"))
    iterations = 0
    while queue:
        iterations += 1
        if iterations % 10000 == 0:
            print("Iterations", iterations, "queue", len(queue))
        dist, node = heapq.heappop(queue)
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
            val = node.move(dir)
            if val:
                heapq.heappush(queue, (dist + 1, val))
    raise ValueError("No solution found")


def presolution0(inputs: List[str]) -> None:
    initial = State(
        robot01=(2, 0),
        robot02=(2, 0),
        robot03=(2, 0),
        robot04=(2, 0),
        robot05=(2, 0),
        robot06=(2, 0),
        robot07=(2, 0),
        robot08=(2, 0),
        robot09=(2, 0),
        robot10=(2, 0),
        robot11=(2, 0),
        robot12=(2, 0),
        robot13=(2, 0),
        robot14=(2, 0),
        robot15=(2, 0),
        robot16=(2, 0),
        robot17=(2, 0),
        robot18=(2, 0),
        robot19=(2, 0),
        robot20=(2, 0),
        robot21=(2, 0),
        robot22=(2, 0),
        robot23=(2, 0),
        robot24=(2, 0),
        robot25=(2, 0),
        endrobot=(2, 3),
    )
    complexity = 0
    for inp in inputs:
        state, dist = solution0(inp, initial)
        print("input", inp, "dist", dist, "state", state)
        complexity += dist * int(inp[:-1])
    print(complexity)


@attrs.frozen(auto_attribs=True, order=True, slots=True)
class State1:
    robots: Tuple[Tuple[int, int], ...]
    endrobot: Tuple[int, int]
    out: str = ""

    def press(self) -> Optional["State1"]:
        if not self.robots:
            endrobot = NUM[self.endrobot[1]][self.endrobot[0]]
            if not endrobot:
                return None
            return attrs.evolve(
                self,
                out=self.out + endrobot,
            )

        zipped = zip(
            itertools.chain(self.robots, [self.endrobot]),
            itertools.chain(self.robots[1:], [self.endrobot]),
        )

        for i, ((x, y), after) in enumerate(zipped):
            val = DIR[y][x]
            if not val:
                return None
            if val == "A":
                continue
            dir = Direction.from_char(val)
            next = dir.movep(after)
            if i == len(self.robots) - 1:
                if not (0 <= next[0] < len(NUM[0])):
                    return None
                if not (0 <= next[1] < len(NUM)):
                    return None
                if next == (0, 3):
                    return None
                return attrs.evolve(self, endrobot=next)

            if not (0 <= next[0] < len(DIR[0])):
                return None
            if not (0 <= next[1] < len(DIR)):
                return None
            if next == (0, 0):
                return None

            return attrs.evolve(
                self, robots=self.robots[: i + 1] + (next,) + self.robots[i + 2 :]
            )

        endrobot = NUM[self.endrobot[1]][self.endrobot[0]]
        if not endrobot:
            return None
        return attrs.evolve(
            self,
            out=self.out + endrobot,
        )

    def move(self, direction: Direction) -> Optional["State1"]:
        if not self.robots:
            next = direction.movep(self.endrobot)
            if not (0 <= next[0] < len(NUM[0])):
                return None
            if not (0 <= next[1] < len(NUM)):
                return None
            if next == (0, 3):
                return None
            return attrs.evolve(self, endrobot=next)

        next = direction.movep(self.robots[0])
        if not (0 <= next[0] < len(DIR[0])):
            return None
        if not (0 <= next[1] < len(DIR)):
            return None
        if next == (0, 0):
            return None
        return attrs.evolve(self, robots=(next,) + self.robots[1:])


def solution1(input_value: str, start: State1) -> Tuple[State1, int, dict, str]:
    queue = [(0, start, "")]
    distances = collections.defaultdict(lambda: float("inf"))
    while queue:
        dist, node, chain = heapq.heappop(queue)
        if not input_value.startswith(node.out):
            continue

        prev_dist = distances[node]
        if dist >= prev_dist:
            continue
        distances[node] = dist
        if node.out == input_value:
            return node, dist, distances, chain

        val = node.press()
        if val:
            heapq.heappush(queue, (dist + 1, val, chain + "A"))
        for dir in Direction:
            val = node.move(dir)
            if val:
                heapq.heappush(queue, (dist + 1, val, chain + dir.char()))
    raise ValueError("No solution found")


def presolution1(inputs: List[str], num_robots) -> None:
    print(f"== {num_robots} ==")
    initial = State1(
        robots=tuple((2, 0) for _ in range(num_robots)),
        endrobot=(2, 3),
    )
    complexity = 0
    for inp in inputs:
        state, dist, distances, chain = solution1(inp, initial)
        print("input", inp, "dist", dist, "state", state, len(distances))
        print("chain", chain)
        complexity += dist * int(inp[:-1])
    print(complexity)


@attrs.frozen(auto_attribs=True, order=True, slots=True)
class State2:
    robot_type: str
    robot: Tuple[int, int]
    out: str = ""

    def press(self) -> Optional["State2"]:
        x, y = self.robot
        if self.robot_type == "num":
            endrobot = NUM[y][x]
        else:
            endrobot = DIR[y][x]
        if not endrobot:
            return None
        return attrs.evolve(
            self,
            out=self.out + endrobot,
        )

    def move(self, direction: Direction) -> Optional["State2"]:
        next = direction.movep(self.robot)
        if self.robot_type == "num":
            if not (0 <= next[0] < len(NUM[0])):
                return None
            if not (0 <= next[1] < len(NUM)):
                return None
            if next == (0, 3):
                return None
        else:
            if not (0 <= next[0] < len(DIR[0])):
                return None
            if not (0 <= next[1] < len(DIR)):
                return None
            if next == (0, 0):
                return None
        return attrs.evolve(self, robot=next)


def solution2(
    input_value: str, start: State2, mindist: float
) -> List[Tuple[State2, int, str]]:
    queue = [(0, start, "")]
    distances = collections.defaultdict(lambda: float("inf"))
    results = []
    while queue:
        dist, node, chain = heapq.heappop(queue)
        if not input_value.startswith(node.out):
            continue

        prev_dist = distances[node]
        if dist > prev_dist:
            continue
        distances[node] = dist
        if node.out == input_value:
            if dist > mindist:
                break
            if results and dist > results[0][1]:
                break
            results.append((node, dist, chain))

        val = node.press()
        if val:
            heapq.heappush(queue, (dist + 1, val, chain + "A"))
        for dir in Direction:
            val = node.move(dir)
            if val:
                heapq.heappush(queue, (dist + 1, val, chain + dir.char()))
    return results


def presolution2(inputs: List[str], num_robots) -> None:
    options = set([inputs[0]])
    for i in range(num_robots + 1):
        print(f"== {i} ==")
        print(options)
        if i == 0:
            initial = State2(
                robot_type="num",
                robot=(2, 3),
            )
        else:
            initial = State2(
                robot_type="dir",
                robot=(2, 0),
            )

        results = []
        mindist = float("inf")
        for i, option in enumerate(options):
            print(i)
            results.extend(solution2(option, initial, mindist))
            for _, dist, _ in results:
                mindist = min(mindist, dist)

        options.clear()
        prevdist = None
        results.sort(key=lambda x: x[1])
        for state, dist, chain in results:
            print("dist", dist, "state", state, "chain", chain)
            if prevdist and dist != prevdist:
                break
            prevdist = dist
            options.add(chain)
            if i == num_robots:
                print("dist", dist, "state", state)
                print("chain", chain)
                break


class memoized(object):
    """Decorator. Caches a function's return value each time it is called.
    If called later with the same arguments, the cached value is returned
    (not reevaluated).
    """

    def __init__(self, func):
        self.func = func
        self.cache = {}

    def __call__(self, *args):
        if args in self.cache:
            return self.cache[args]
        else:
            value = self.func(*args)
            self.cache[args] = value
            return value

    def __repr__(self):
        """Return the function's docstring."""
        return self.func.__doc__

    def __get__(self, obj, _):
        """Support instance methods."""
        return functools.partial(self.__call__, obj)


@memoized
def DIRindex(val: str) -> Tuple[int, int]:
    for y, row in enumerate(DIR):
        for x, cell in enumerate(row):
            if cell == val:
                return x, y
    raise ValueError(f"Invalid val: {val}")


@memoized
def NUMindex(val: str) -> Tuple[int, int]:
    for y, row in enumerate(NUM):
        for x, cell in enumerate(row):
            if cell == val:
                return x, y
    raise ValueError(f"Invalid val: {val}")


@memoized
def directions(
    p1: Tuple[int, int], p2: Tuple[int, int]
) -> Tuple[Tuple[Direction, ...], ...]:
    x1, y1 = p1
    x2, y2 = p2

    horizontals = []
    for _ in range(abs(x1 - x2)):
        if x1 < x2:
            horizontals.append(Direction.RIGHT)
        else:
            horizontals.append(Direction.LEFT)

    verticals = []
    for _ in range(abs(y1 - y2)):
        if y1 < y2:
            verticals.append(Direction.DOWN)
        else:
            verticals.append(Direction.UP)

    horizontals = tuple(horizontals)
    verticals = tuple(verticals)
    if not horizontals:
        return (verticals,)
    if not verticals:
        return (horizontals,)
    if not horizontals or not verticals:
        return tuple(tuple())

    return (horizontals + verticals, verticals + horizontals)


@memoized
def alldirections(
    p1: Tuple[int, int], p2: Tuple[int, int]
) -> Tuple[Tuple[Direction, ...], ...]:
    x1, y1 = p1
    x2, y2 = p2

    horizontals = []
    for _ in range(abs(x1 - x2)):
        if x1 < x2:
            horizontals.append(Direction.RIGHT)
        else:
            horizontals.append(Direction.LEFT)

    verticals = []
    for _ in range(abs(y1 - y2)):
        if y1 < y2:
            verticals.append(Direction.DOWN)
        else:
            verticals.append(Direction.UP)

    ret = set()
    for perm in itertools.permutations(horizontals + verticals):
        ret.add(perm)

    lst = []
    for val in ret:
        lst.append(val)
    return tuple(lst)


def valid_dirs(
    directions: List[Direction], p1: Tuple[int, int], grid: List[List[str]]
) -> bool:
    for direction in directions:
        p1 = direction.movep(p1)
        if not grid[p1[1]][p1[0]]:
            return False
    return True


def solution3(input_value: List[str], grid_type: str) -> List[str]:
    inp = ["A"] + input_value
    output = []
    percentage = 0
    fraction = len(inp) // 10
    for i, val in enumerate(inp):
        if i % max(fraction, 100) == 0:
            print("Percentage done", percentage)
            percentage += 10
        if i == len(inp) - 1:
            break
        nextval = inp[i + 1]
        if grid_type == "num":
            start = NUMindex(val.char() if isinstance(val, Direction) else val)
            end = NUMindex(
                nextval.char() if isinstance(nextval, Direction) else nextval
            )
            direction_options = directions(start, end)
            for option in direction_options:
                if valid_dirs(option, start, NUM):
                    output.extend(option)
                    break
        else:
            start = DIRindex(val.char() if isinstance(val, Direction) else val)
            end = DIRindex(
                nextval.char() if isinstance(nextval, Direction) else nextval
            )
            direction_options = directions(start, end)
            for option in direction_options:
                if valid_dirs(option, start, DIR):
                    output.extend(option)
                    break
        output.append("A")
    return output


def presolution3(inputs: List[str]) -> None:
    output = ["A"] + list(inputs[0])
    for i in range(25):
        print(f"== {i} ==")
        if i == 0:
            output = solution3(output, "num")
        else:
            output = solution3(output, "dir")
        print(len(output))


def solution4memo(memo: dict, endlevel: int, start: str, end: str, level: int):
    memo_key = (start, end, level)
    val = memo.get(memo_key)
    if val:
        return val

    # Compute start and end points for the current level.
    if level == 0:
        startindex = NUMindex(start)
        endindex = NUMindex(end)
    else:
        startindex = DIRindex(start)
        endindex = DIRindex(end)

    mindist = float("inf")
    for option in directions(startindex, endindex):
        if level == 0:
            if not valid_dirs(option, startindex, NUM):
                continue
        else:
            if not valid_dirs(option, startindex, DIR):
                continue
        dist = 0
        selected = ("A",) + option + ("A",)
        if level == endlevel:
            dist += len(selected) - 1
            mindist = min(mindist, dist)
            break
        else:
            for d1, d2 in zip(selected, selected[1:]):
                d1 = d1.char() if isinstance(d1, Direction) else d1
                d2 = d2.char() if isinstance(d2, Direction) else d2
                dist += solution4memo(memo, endlevel, d1, d2, level + 1)
        mindist = min(mindist, dist)

    memo[memo_key] = mindist
    return mindist


def solution4(input_value: List[str], endlevel: int) -> int:
    memo = {}
    dist = 0
    complexity = 0
    for inp in input_value:
        inp = ["A"] + list(inp)
        dist = 0
        for start, end in zip(inp, inp[1:]):
            dist += solution4memo(memo, endlevel, start, end, 0)
        print("dist", dist, "".join(inp[1:]))
        complexity += dist * int("".join(inp[1:-1]))
    return complexity


def main():
    inputs = []
    with open("./data/test.in") as f:
        for line in f.readlines():
            inputs.append(line.strip())

    print("Solution", solution4(inputs, 25))


if __name__ == "__main__":
    main()
