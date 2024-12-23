import collections


def main():
    pairs = []
    with open("./data/test.in") as f:
        for line in f:
            a, b = line.strip().split("-")
            pairs.append((a, b))

    connections = collections.defaultdict(set)
    for a, b in pairs:
        connections[a].add(b)
        connections[b].add(a)

    components = set()
    for a, tail in connections.items():
        for b in tail:
            for c in connections[b]:
                if c not in tail:
                    continue
                components.add(frozenset([a, b, c]))

    count = 0
    for component in components:
        if any(comp.startswith("t") for comp in component):
            count += 1
    print("Result:", count)


if __name__ == "__main__":
    main()
