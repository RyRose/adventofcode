def valid_design(patterns, design):
    blocks = [""]
    while blocks:
        block = blocks.pop()
        for pattern in patterns:
            attempt = block + pattern
            if design.startswith(attempt):
                if len(attempt) == len(design):
                    return True
                blocks.append(attempt)
    return False


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
        if i % 10 == 0:
            print("Iteration", i)
        if valid_design(patterns, design):
            count += 1
    print(count)


if __name__ == "__main__":
    main()
