import re

FILE = "./data/test1.in"

with open(FILE) as f:
    content = f.read()

items = re.findall(r"mul\((\d+),(\d+)\)|(do)\(\)|(don)'t\(\)", content)
enabled = True
ret = 0
for lhs, rhs, do, dont in items:
    if do:
        enabled = True
        continue
    if dont:
        enabled = False
        continue
    if not enabled:
        continue
    ret += int(lhs) * int(rhs)
print(ret)
