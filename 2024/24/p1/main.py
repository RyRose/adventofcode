# AND gates output 1 if both inputs are 1; if either input is 0, these gates output 0.
# OR gates output 1 if one or both inputs is 1; if both inputs are 0, these gates output 0.
# XOR gates output 1 if the inputs are different; if the inputs are the same, these gates output 0.


def main():

    values = {}
    rules = set()
    with open("./data/test.in") as f:
        for line in f:
            if ":" in line:
                wire, value = line.split(": ")
                values[wire] = int(value.strip())
            if "->" in line:
                wire0, gate, wire1, _, output = line.split()
                rules.add((wire0, gate, wire1, output))

    found = True
    while found:
        found = False
        for rule in rules:
            wire0, gate, wire1, output = rule
            if output in values or wire0 not in values or wire1 not in values:
                continue
            if gate == "AND":
                values[output] = values[wire0] & values[wire1]
            if gate == "OR":
                values[output] = values[wire0] | values[wire1]
            if gate == "XOR":
                values[output] = values[wire0] ^ values[wire1]
            found = True

    zs = []
    for key, value in values.items():
        if not key.startswith("z"):
            continue
        zs.append((key, value))
    zs.sort()
    zs.reverse()
    print(int("".join([str(z[1]) for z in zs]), base=2))


if __name__ == "__main__":
    main()
