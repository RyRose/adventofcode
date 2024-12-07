import re

FILE = "./data/test1.in"

with open(FILE) as f:
    content = f.read()

items = re.findall(r"mul\((\d+),(\d+)\)", content)
ret = 0
for lhs, rhs in items:
    ret += int(lhs) * int(rhs)
print(ret)
