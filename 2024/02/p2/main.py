FILE = "./data/test1.in"

with open(FILE) as f:
    lines = f.read().splitlines()


def iterate_over(line, remove):
    for i in range(len(line)):
        if i == remove:
            continue
        yield line[i]


safe = 0
for line in lines:
    line = list(map(int, line.split(" ")))
    for skip in range(-1, len(line)):
        greater = True
        lesser = True
        diff = True
        prev = None
        for cur in iterate_over(line, skip):
            if prev is None:
                prev = cur
                continue
            greater &= prev > cur
            lesser &= prev < cur
            diff &= 1 <= abs(cur - prev) <= 3
            prev = cur
        if (greater or lesser) and diff:
            safe += 1
            break
print(safe)
