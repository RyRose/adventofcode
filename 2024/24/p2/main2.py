import collections
import attr
import enum
from typing import Dict, Optional, OrderedDict


class Gate(enum.Enum):
    AND = "&"
    OR = "|"
    XOR = "^"


@attr.frozen
class Formula:
    wire0: str
    gate: Gate
    wire1: str


@attr.define
class Context:
    values: Dict[str, int]
    rules: OrderedDict[str, Formula]


@attr.define
class Junction:
    lhs: "Node"
    rhs: "Node"
    gate: Gate

    def prettystr(self):
        return f"({self.lhs.prettystr()} {self.gate.value} {self.rhs.prettystr()})"

    def prettystr_correct(self):
        return f"({self.lhs.prettystr_correct()} {self.gate.value} {self.rhs.prettystr_correct()})"

    def equals(self, other):
        return self.gate == other.gate and (
            (self.lhs.equals(other.lhs) and self.rhs.equals(other.rhs))
            or (self.lhs.equals(other.rhs) and self.rhs.equals(other.lhs))
        )


@attr.define
class Node:
    name: str = ""
    value: Optional[int] = None
    junction: Optional[Junction] = None
    correct: bool = False

    def prettystr(self):
        if self.junction:
            return self.junction.prettystr()
        return self.name

    def prettystr_correct(self):
        if self.correct and self.name:
            return self.name
        if self.junction:
            return self.junction.prettystr_correct()
        return self.name

    def equals(self, other):
        if self.junction and other.junction:
            return self.junction.equals(other.junction)
        if self.junction or other.junction:
            return False
        return self.name == other.name

    def mark_correct(self):
        self.correct = True
        if self.junction:
            self.junction.lhs.mark_correct()
            self.junction.rhs.mark_correct()


def make_ideal_nodes():
    nodes = []
    carry = None
    for i in range(46):
        x, y, z = f"x{i:02}", f"y{i:02}", f"z{i:02}"
        if not carry:
            carry = Node(
                junction=Junction(lhs=Node(name=x), rhs=Node(name=y), gate=Gate.AND)
            )
            nodes.append(
                Node(
                    name=z,
                    junction=Junction(
                        lhs=Node(name=x), rhs=Node(name=y), gate=Gate.XOR
                    ),
                )
            )
            continue
        nodes.append(
            Node(
                name=z,
                junction=Junction(
                    lhs=Node(
                        junction=Junction(
                            lhs=Node(name=x), rhs=Node(name=y), gate=Gate.XOR
                        )
                    ),
                    rhs=carry,
                    gate=Gate.XOR,
                ),
            )
        )
        carry = Node(
            junction=Junction(
                lhs=Node(
                    junction=Junction(
                        lhs=Node(
                            junction=Junction(
                                lhs=Node(name=x), rhs=Node(name=y), gate=Gate.XOR
                            ),
                        ),
                        rhs=carry,
                        gate=Gate.AND,
                    )
                ),
                rhs=Node(
                    junction=Junction(lhs=Node(name=x), rhs=Node(name=y), gate=Gate.AND)
                ),
                gate=Gate.OR,
            )
        )
    return nodes


def solution0():
    values: Dict[str, int] = {}
    rules: Dict[str, Formula] = collections.OrderedDict()
    with open("./data/test.in") as f:
        for line in f:
            if ":" in line:
                wire, value = line.split(": ")
                values[wire] = int(value.strip())
            if "->" in line:
                wire0, gate, wire1, _, output = line.split()
                if wire0[1:].isdigit():
                    wire1, wire0 = sorted([wire0, wire1])
                rules[output] = Formula(
                    wire0=wire0,
                    gate={"XOR": Gate.XOR, "AND": Gate.AND, "OR": Gate.OR}[gate],
                    wire1=wire1,
                )
    ctx = Context(values=values, rules=rules)

    swaps = [
        ("z12", "djg"),
        ("z19", "sbg"),
        ("mcq", "hjm"),
        ("z37", "dsd"),
    ]

    for a, b in swaps:
        ctx.rules[a], ctx.rules[b] = ctx.rules[b], ctx.rules[a]

    ideal_nodes = make_ideal_nodes()

    node_cache = {}

    def make_node(ctx, name):
        if name not in ctx.rules:
            return Node(name=name)
        if name in node_cache:
            return node_cache[name]
        formula = ctx.rules[name]
        node = Node(
            name=name,
            junction=Junction(
                rhs=make_node(ctx, formula.wire0),
                lhs=make_node(ctx, formula.wire1),
                gate=formula.gate,
            ),
        )
        node_cache[name] = node
        return node

    real_nodes = []
    for i in range(46):
        real_nodes.append(make_node(ctx, f"z{i:02}"))

    for ideal, real in zip(ideal_nodes, real_nodes):
        print(ideal.name)
        print("I:", ideal.prettystr())
        print("R:", real.prettystr())
        if ideal.equals(real):
            print("equal")
            real.mark_correct()
        print("R:", real.prettystr_correct())
        print()

    ret = []
    for swap in swaps:
        ret.extend(swap)
    ret.sort()
    print(",".join(ret))


def main():
    solution0()
    pass


if __name__ == "__main__":
    main()
