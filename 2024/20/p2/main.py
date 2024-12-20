import collections
import heapq
from typing import Tuple


def print_grid(grid):
    for row in grid:
        for cell in row:
            print(cell, end="")
        print()


def pointdist(p1, p2):
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])


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


def move_forwardp(p, direction) -> Tuple[int, int]:
    x, y = p
    if direction == UP:
        return x, y - 1
    elif direction == RIGHT:
        return x + 1, y
    elif direction == DOWN:
        return x, y + 1
    elif direction == LEFT:
        return x - 1, y
    raise ValueError(f"Invalid direction: {direction}")


def alldijkstra(grid, start):
    distances = {}
    for y, row in enumerate(grid):
        for x, cell in enumerate(row):
            if cell == "#":
                continue
            distances[(x, y)] = float("inf")

    queue = [(0, start)]
    while queue:
        cur_dist, cur_node = heapq.heappop(queue)
        if cur_node not in distances:
            continue

        x, y = cur_node
        prev_dist = distances[cur_node]

        if cur_dist >= prev_dist:
            continue

        distances[cur_node] = cur_dist

        heapq.heappush(queue, (cur_dist + 1, move_forward(x, y, UP)))
        heapq.heappush(queue, (cur_dist + 1, move_forward(x, y, LEFT)))
        heapq.heappush(queue, (cur_dist + 1, move_forward(x, y, RIGHT)))
        heapq.heappush(queue, (cur_dist + 1, move_forward(x, y, DOWN)))
    return distances


def main():
    grid = []
    with open("./data/test.in") as f:
        for line in f.readlines():
            grid.append(list(line.strip()))

    for y, row in enumerate(grid):
        for x, cell in enumerate(row):
            if cell == "S":
                start = (x, y)
            elif cell == "E":
                end = (x, y)
    assert start
    assert end

    cheats = collections.defaultdict(lambda: float("inf"))
    distances = alldijkstra(grid, start)
    for startp, dist in distances.items():
        for endp, dist2 in distances.items():
            if startp == endp:
                continue
            pdist = pointdist(startp, endp)
            if pdist > 20:
                continue
            d = dist2 - dist - pdist
            if d <= 0:
                continue
            cheats[(startp, endp)] = min(cheats[(startp, endp)], d)

    best = 0
    for _, dist in cheats.items():
        if dist >= 100:
            best += 1
    print(best)


if __name__ == "__main__":
    main()
