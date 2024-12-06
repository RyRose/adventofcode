FILE = "./data/test1.in"

with open(FILE) as f:
    lines = f.read().splitlines()

safe = 0
for line in lines:
    greater = True
    lesser = True
    diff = True
    line = list(map(int, line.split(" ")))
    for i in range(1, len(line)):
        greater &= line[i] < line[i - 1]
        lesser &= line[i] > line[i - 1]
        diff &= 1 <= abs(line[i] - line[i - 1]) <= 3
    if (greater or lesser) and diff:
        safe += 1
print(safe)
