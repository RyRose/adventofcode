import collections
from typing import Dict, FrozenSet

# Get all connections from start to ends
# A - B
# B - C
# C - D


def search(connections, seen, start) -> set[str]:
    next = seen & connections[start]
    if not next:
        return set([start])

    ret = set()
    for node in connections[start]:
        if node not in next:
            continue
        res = search(connections, next, node)
        ret = res if len(res) > len(ret) else ret
    return set([start]) | ret


def searchmemo(connections, memo, seen: FrozenSet[str], start: str) -> set[str]:
    val = memo.get((seen, start))
    if val:
        print("memo hit!")
        return val

    next = seen & connections[start]
    if not next:
        return set([start])

    ret = set()
    for node in connections[start]:
        if node not in next:
            continue
        res = search(connections, next, node)
        ret = res if len(res) > len(ret) else ret
    actual_ret = set([start]) | ret
    memo[(seen, start)] = actual_ret
    return actual_ret


def solution0():
    pairs = []
    with open("./data/test.in") as f:
        for line in f:
            a, b = line.strip().split("-")
            pairs.append((a, b))

    connections = collections.defaultdict(set)
    for a, b in pairs:
        connections[a].add(b)
        connections[b].add(a)

    print("Connections", len(connections))
    lst = set()
    for i, (a, tail) in enumerate(connections.items()):
        print("Iteration", i)
        i += 1

        ret = search(connections, tail, a)
        if len(ret) > len(lst):
            lst = ret

    out = list(lst)
    out.sort()
    print("Result:", out, ",".join(out))


def solution1():
    pairs = []
    with open("./data/test.in") as f:
        for line in f:
            a, b = line.strip().split("-")
            pairs.append((a, b))

    connections = collections.defaultdict(set)
    for a, b in pairs:
        connections[a].add(b)
        connections[b].add(a)

    print("Connections", len(connections))
    lst = set()
    memo = {}
    for i, (a, tail) in enumerate(connections.items()):
        print("Iteration", i)
        i += 1

        ret = searchmemo(connections, memo, frozenset(tail), a)
        if len(ret) > len(lst):
            lst = ret

    out = list(lst)
    out.sort()
    print("Result:", out, ",".join(out))


# A - B
# C - D
# a - b - c - d
def solution2():

    connections = collections.defaultdict(set)
    with open("./data/test.in") as f:
        for line in f:
            a, b = line.strip().split("-")
            connections[a].add(b)
            connections[b].add(a)

    maxnodes = set()
    for a in connections:
        nodes = set([a])
        for b in connections:
            found = True
            for node in nodes:
                if node not in connections[b]:
                    found = False
                    break
            if found:
                nodes.add(b)
        if len(nodes) > len(maxnodes):
            maxnodes = nodes

    lst = list(maxnodes)
    lst.sort()
    print("Result:", lst, ",".join(lst))


def main():
    solution2()


if __name__ == "__main__":
    main()
