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
    grid = []
    with open("./data/test.in") as f:
        for line in f.readlines():
            grid.append(list(line.strip()))

    distances = {}
    start = None
    end = None
    for y, row in enumerate(grid):
        for x, cell in enumerate(row):
            if cell in (".", "S", "E"):
                distances[(x, y, UP)] = (float("inf"), set())
                distances[(x, y, RIGHT)] = (float("inf"), set())
                distances[(x, y, DOWN)] = (float("inf"), set())
                distances[(x, y, LEFT)] = (float("inf"), set())
            if cell == "S":
                start = (x, y, RIGHT)
                distances[start] = (0, set())
            if cell == "E":
                end = (x, y)

    ret = None
    queue = [(0, set(), start)]
    iterations = 0
    while queue:
        cur_dist, cur_chain, cur_node = heapq.heappop(queue)
        iterations += 1
        if iterations % 10000 == 0:
            print(f"{iterations} - {len(queue)}")

        if cur_node not in distances:
            continue

        cur_chain = cur_chain.copy()
        cur_chain.add(cur_node[:2])

        cur_x, cur_y, cur_dir = cur_node
        prev_dist, prev_chain = distances[cur_node]

        if cur_dist > prev_dist:
            continue
        if cur_dist == prev_dist:
            cur_chain = cur_chain.union(prev_chain)

        distances[cur_node] = (cur_dist, cur_chain)

        # rotate left
        left_dist = cur_dist + 1000
        left_node = (cur_x, cur_y, rotate_left(cur_dir))
        heapq.heappush(queue, (left_dist, cur_chain, left_node))

        # rotate right
        right_dist = cur_dist + 1000
        right_node = (cur_x, cur_y, rotate_right(cur_dir))
        heapq.heappush(queue, (right_dist, cur_chain, right_node))

        # move forward
        forward_dist = cur_dist + 1
        forward_node = move_forward(cur_x, cur_y, cur_dir) + (cur_dir,)
        heapq.heappush(queue, (forward_dist, cur_chain, forward_node))

    mindist = float("inf")
    for direction in (UP, RIGHT, DOWN, LEFT):
        dist, chain = distances[end + (direction,)]
        if dist < mindist:
            mindist = dist
            ret = chain

    print(len(ret))


if __name__ == "__main__":
    main()
