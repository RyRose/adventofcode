import heapq
from typing import Tuple

UP = 0
RIGHT = 1
DOWN = 2
LEFT = 3


def move_forward(x, y, direction) -> Tuple[int, int]:
    if direction == UP:
        return x, y - 1
    elif direction == RIGHT:
        return x + 1, y
    elif direction == DOWN:
        return x, y + 1
    elif direction == LEFT:
        return x - 1, y
    raise ValueError(f"Invalid direction: {direction}")


def rotate_left(direction):
    return (direction - 1) % 4


def rotate_right(direction):
    return (direction + 1) % 4


def print_grid(grid):
    for row in grid:
        for cell in row:
            print(cell, end="")
        print()


def main():
    locs = []

    # num_bytes = 12
    # width = height = 7
    # with open("./data/sample0.in") as f:
    #     for line in f.readlines():
    #         locs.append(list(map(int, line.split(","))))

    num_bytes = 1024
    width = height = 71
    with open("./data/test.in") as f:
        for line in f.readlines():
            locs.append(list(map(int, line.split(","))))

    grid = []
    for y in range(height):
        row = []
        for x in range(width):
            row.append(".")
        grid.append(row)

    for i, loc in enumerate(locs):
        if i >= num_bytes:
            break
        grid[loc[1]][loc[0]] = "#"

    print_grid(grid)

    distances = {}
    start = (0, 0)
    end = (width - 1, height - 1)
    for y, row in enumerate(grid):
        for x, cell in enumerate(row):
            if cell == "#":
                continue
            distances[(x, y)] = float("inf")

    queue = [(0, start)]
    iterations = 0
    while queue:
        cur_dist, cur_node = heapq.heappop(queue)
        iterations += 1
        if iterations % 10000 == 0:
            print(f"{iterations} - {len(queue)}")

        if cur_node not in distances:
            continue

        cur_x, cur_y = cur_node
        prev_dist = distances[cur_node]

        if cur_dist >= prev_dist:
            continue

        distances[cur_node] = cur_dist

        heapq.heappush(queue, (cur_dist + 1, move_forward(cur_x, cur_y, UP)))
        heapq.heappush(queue, (cur_dist + 1, move_forward(cur_x, cur_y, LEFT)))
        heapq.heappush(queue, (cur_dist + 1, move_forward(cur_x, cur_y, RIGHT)))
        heapq.heappush(queue, (cur_dist + 1, move_forward(cur_x, cur_y, DOWN)))

    print(distances[end])


if __name__ == "__main__":
    main()
