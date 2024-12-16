import heapq

UP = 0
RIGHT = 1
DOWN = 2
LEFT = 3


def move_forward(x, y, direction):
    if direction == UP:
        return x, y - 1
    elif direction == RIGHT:
        return x + 1, y
    elif direction == DOWN:
        return x, y + 1
    elif direction == LEFT:
        return x - 1, y


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
    for y, row in enumerate(grid):
        for x, cell in enumerate(row):
            if cell in (".", "S", "E"):
                distances[(x, y, UP)] = float("inf")
                distances[(x, y, RIGHT)] = float("inf")
                distances[(x, y, DOWN)] = float("inf")
                distances[(x, y, LEFT)] = float("inf")
            if cell == "S":
                start = (x, y, RIGHT)
                distances[start] = 0

    queue = [(0, start)]
    while queue:
        cur_dist, cur_node = heapq.heappop(queue)

        if cur_node not in distances:
            continue

        cur_x, cur_y, cur_dir = cur_node
        if grid[cur_y][cur_x] == "E":
            print(cur_dist)
            break

        if cur_dist > distances[cur_node]:
            continue
        distances[cur_node] = cur_dist

        # rotate left
        left_dist = cur_dist + 1000
        left_node = (cur_x, cur_y, rotate_left(cur_dir))
        heapq.heappush(queue, (left_dist, left_node))

        # rotate right
        right_dist = cur_dist + 1000
        right_node = (cur_x, cur_y, rotate_right(cur_dir))
        heapq.heappush(queue, (right_dist, right_node))

        # move forward
        forward_dist = cur_dist + 1
        forward_node = move_forward(cur_x, cur_y, cur_dir) + (cur_dir,)
        heapq.heappush(queue, (forward_dist, forward_node))


if __name__ == "__main__":
    main()
