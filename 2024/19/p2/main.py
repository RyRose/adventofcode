# SLOW!
def valid_design(patterns, design):
    blocks = [("",)]
    ret = set()
    while blocks:
        block = blocks.pop()
        if len("".join(block)) >= len(design):
            continue
        for pattern in patterns:
            attempt = block + (pattern,)
            joined = "".join(attempt)
            if design.startswith(joined):
                if len(joined) == len(design):
                    ret.add(attempt)
                else:
                    blocks.append(attempt)
    return len(ret)


# SLOW!
def valid_design2(patterns, design):
    max_pattern_len = max(map(len, patterns))
    patterns = set(patterns)
    blocks = [("",)]
    ret = 0
    while blocks:
        block = blocks.pop()
        sblock = "".join(block)
        if len(sblock) == len(design):
            ret += 1
            continue
        idx = len(sblock)
        for i in range(1, max_pattern_len + 1):
            if idx + i > len(design):
                break
            next = design[idx : idx + i]
            if next in patterns:
                blocks.append(block + (design[idx : idx + i],))
    return ret


def search(patterns, design, memo, cur):
    if cur in memo:
        return memo[cur]
    if not design.startswith(cur):
        return 0
    if len(cur) == len(design):
        return 1

    res = 0
    for p in patterns:
        res += search(patterns, design, memo, cur + p)
    memo[cur] = res
    return res


def valid_design3(patterns, design):
    return search(patterns, design, {}, "")


def main():
    patterns = []
    designs = []
    with open("./data/test.in") as f:
        patterns.extend(f.readline().strip().split(", "))
        f.readline()
        for line in f.readlines():
            designs.append(line.strip())

    count = 0
    print("Num designs:", len(designs))
    print("Num patterns:", len(patterns))
    for i, design in enumerate(designs):
        print("Design iteration", i)
        count += valid_design3(patterns, design)
    print(count)


if __name__ == "__main__":
    main()
