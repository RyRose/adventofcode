import collections

FILE = "./data/test0.in"


def get_grid():
    grid = []
    start = None
    with open(FILE, "r") as f:
        for y, line in enumerate(f.readlines()):
            gridline = []
            for x, char in enumerate(line.strip()):
                if char == START:
                    start = (x, y)
                gridline.append(char)
            grid.append(gridline)

    assert start is not None
    return grid, start


def in_bounds(point, grid):
    x, y = point
    return 0 <= x < len(grid[0]) and 0 <= y < len(grid)


EMPTY = "."
WALL = "#"
START = "^"

UP = 0
RIGHT = 1
DOWN = 2
LEFT = 3


def move(point, direction):
    x, y = point
    if direction == UP:
        return (x, y - 1)
    elif direction == RIGHT:
        return (x + 1, y)
    elif direction == DOWN:
        return (x, y + 1)
    elif direction == LEFT:
        return (x - 1, y)
    else:
        raise ValueError(f"Invalid direction: {direction}")


def rotate(direction):
    return (direction + 1) % 4


def search_cycle(grid, cur, direction):
    seen = collections.defaultdict(int)
    while in_bounds(cur, grid):
        seen[cur] += 1
        next = move(cur, direction)
        if not in_bounds(next, grid):
            break
        if grid[next[1]][next[0]] == WALL:
            direction = rotate(direction)
            continue
        if seen[cur] > 10:
            return True
        cur = next
    return False


def main():
    grid, start = get_grid()
    cycles = 0
    for y, row in enumerate(grid):
        for x, _ in enumerate(row):
            if grid[y][x] in (WALL, START):
                continue
            grid[y][x] = WALL
            if search_cycle(grid, start, UP):
                cycles += 1
            grid[y][x] = EMPTY

    print(cycles)


if __name__ == "__main__":
    main()
