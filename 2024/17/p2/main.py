import multiprocessing
import os
import sys


def run_program(register_a, register_b, register_c, program):

    output = []

    ip = 0
    oindex = 0
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
            output.append(combo % 8)
            oindex += 1
        if opcode == 6:
            register_b = register_a // (2**combo)
        if opcode == 7:
            register_c = register_a // (2**combo)

        ip += 2
    return output


def func(start):
    with open("out.txt", "a") as f:
        print("Iteration", start, file=f)

    program = []
    with open("./data/test.in") as f:
        # ignore register a
        int(f.readline().split()[-1])
        register_b = int(f.readline().split()[-1])
        register_c = int(f.readline().split()[-1])
        f.readline()
        program.extend(map(int, f.readline().split()[-1].split(",")))

    for i in range(start, start + 1_000_000):
        output = run_program(i, register_b, register_c, program)
        if len(output) != len(program):
            continue
        if output == program:
            print("============================================")
            print(i)
            print("============================================")
            print(i)
            print("============================================")
            print(i)
            print("============================================")
            print(i)
            print("============================================")
            print(i)
            print("============================================")
            sys.exit()


def func2(start, end):

    program = []
    with open("./data/test.in") as f:
        # ignore register a
        int(f.readline().split()[-1])
        register_b = int(f.readline().split()[-1])
        register_c = int(f.readline().split()[-1])
        f.readline()
        program.extend(map(int, f.readline().split()[-1].split(",")))

    for i in range(start, end):
        if i % 1_000_000 == 0:
            print("Iteration", i)
        output = run_program(i, register_b, register_c, program)
        if len(output) != len(program):
            continue
        if output == program:
            print("============================================")
            print(i)
            print("============================================")
            print(i)
            print("============================================")
            print(i)
            print("============================================")
            print(i)
            print("============================================")
            print(i)
            print("============================================")


def main():
    program = []
    with open("./data/test.in") as f:
        # ignore register a
        int(f.readline().split()[-1])
        register_b = int(f.readline().split()[-1])
        register_c = int(f.readline().split()[-1])
        f.readline()
        program.extend(map(int, f.readline().split()[-1].split(",")))

    # Program: 2,4,1,1,7,5,1,5,4,2,5,5,0,3,3,0

    # Iteration -10 164278496306046 165523231942078 1244735636032

    # END = 164282120368061

    # print("const", run_program(164278496489149, register_b, register_c, program))

    # start = 164278496306046
    # end = END
    start = 8**15
    end = 8**16
    for i in range(-1, -16, -1):
        print("Iteration", i, start, end, end - start)
        outputs = []
        diff = end - start
        for j in range(start, end, max(diff // 100000, 1)):
            outputs.append((j, run_program(j, register_b, register_c, program)))

        first = None
        last = None
        for j, output in outputs:
            if output[i:] != program[i:]:
                continue
            if first is None:
                first = j
            last = j
        if first is None or last is None:
            print(outputs[0], outputs[-1])
        assert first is not None
        assert last is not None
        start, end = first, last

    print(start, end, end - start)
    func2(start, end)


if __name__ == "__main__":
    main()
