from typing import List
import attr

# #####
# ###.#
# #.#.#
# #.#.#
# ..#.#
# ..#..
# .....

COLUMNS = 5
ROWS = 7


@attr.frozen
class Data:
    keys: List[List[int]]
    locks: List[List[int]]


def get_input_data(fp: str) -> Data:
    grids = []
    with open(fp) as f:
        grid = []
        for line in f:
            line = line.strip()
            if not line:
                if grid:
                    grids.append(grid)
                grid = []
                continue
            grid.append(line)
    if grid:
        grids.append(grid)

    locks = []
    keys = []
    for grid in grids:
        item = []
        for col, _ in enumerate(grid[0]):
            cnt = sum(1 for row in grid if row[col] == "#")
            item.append(cnt - 1)

        # lock
        if grid[0][0] == "#":
            locks.append(item)
        else:
            keys.append(item)

    return Data(keys=keys, locks=locks)


def main():
    d = get_input_data("./data/test.in")

    cnt = 0
    for key in d.keys:
        for lock in d.locks:
            fit = [s for s in map(sum, zip(key, lock)) if s <= 5]
            if len(fit) == 5:
                cnt += 1

    print(cnt)


if __name__ == "__main__":
    main()
