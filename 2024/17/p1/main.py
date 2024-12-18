def main():
    program = []
    with open("./data/test.in") as f:
        register_a = int(f.readline().split()[-1])
        register_b = int(f.readline().split()[-1])
        register_c = int(f.readline().split()[-1])
        f.readline()
        program.extend(map(int, f.readline().split()[-1].split(",")))

    ip = 0
    while ip < len(program) - 1:
        opcode, literal = program[ip], program[ip + 1]
        if literal in (0, 1, 2, 3):
            combo = literal
        elif literal == 4:
            combo = register_a
        elif literal == 5:
            combo = register_b
        elif literal == 6:
            combo = register_c
        else:
            raise ValueError(f"Invalid operand {literal}")

        if opcode == 0:
            register_a //= 2**combo
        if opcode == 1:
            register_b ^= literal
        if opcode == 2:
            register_b = combo % 8
        if opcode == 3 and register_a != 0:
            ip = literal
            # TODO: If not jumping, potentially don't continue. e.g. ip == literal
            continue
        if opcode == 4:
            register_b ^= register_c
        if opcode == 5:
            print(combo % 8, end=",")
        if opcode == 6:
            register_b = register_a // (2**combo)
        if opcode == 7:
            register_c = register_a // (2**combo)

        ip += 2


if __name__ == "__main__":
    main()
