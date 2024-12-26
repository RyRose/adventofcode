from typing import Dict, List, Optional, OrderedDict, Set
import attr
import collections
import functools


@attr.frozen
class Formula:
    wire0: str
    gate: str
    wire1: str

    def ready(self, values: Dict[str, int]) -> bool:
        return self.wire0 in values and self.wire1 in values

    def calculate(self, values: Dict[str, int]) -> int:
        if self.gate == "AND":
            return values[self.wire0] & values[self.wire1]
        if self.gate == "OR":
            return values[self.wire0] | values[self.wire1]
        if self.gate == "XOR":
            return values[self.wire0] ^ values[self.wire1]
        raise ValueError(f"Unknown gate: {self.gate}")


@attr.define
class Context:
    values: Dict[str, int]
    rules: OrderedDict[str, Formula]

    @functools.cached_property
    def xvalue(self) -> int:
        xs = []
        for key, value in self.values.items():
            if not key.startswith("x"):
                continue
            xs.append((key, value))
        xs.sort()
        xs.reverse()
        return int("".join([str(x[1]) for x in xs]), base=2)

    @functools.cached_property
    def yvalue(self) -> int:
        ys = []
        for key, value in self.values.items():
            if not key.startswith("y"):
                continue
            ys.append((key, value))
        ys.sort()
        ys.reverse()
        return int("".join([str(y[1]) for y in ys]), base=2)

    @functools.cached_property
    def targetzvalue(self) -> int:
        return self.xvalue + self.yvalue

    @functools.cached_property
    def ozvalue(self) -> int:
        return self.zvalue()

    @functools.cached_property
    def zs(self) -> List[str]:
        zs = []
        for key in self.rules:
            if not key.startswith("z"):
                continue
            zs.append(key)
        zs.sort()
        zs.reverse()
        return zs

    @functools.cached_property
    def reverserules(self) -> Dict[str, set]:
        ret = collections.defaultdict(set)
        for output, formula in self.rules.items():
            ret[formula.wire0].add(output)
            ret[formula.wire1].add(output)
        return ret

    def allvalues(self, swaps=None) -> Dict[str, int]:
        swaps = swaps or []
        values = self.values.copy()
        rules = self.rules.copy()

        for a, b in swaps:
            rules[a], rules[b] = rules[b], rules[a]

        for rule in rules:
            if rule in values:
                continue
            stack = [rule]
            seen = collections.defaultdict(int)
            while stack:
                node = stack.pop()
                if node in values:
                    continue

                # If seen more than twice, then cycle detected.
                seen[node] += 1
                if seen[node] > 2:
                    return {}

                formula = rules[node]
                if formula.ready(values):
                    values[node] = formula.calculate(values)
                    continue
                stack.append(node)
                stack.append(formula.wire0)
                stack.append(formula.wire1)
        return values

    def zvalue(self, swaps=None) -> int:
        swaps = swaps or []
        values = self.values.copy()
        rules = self.rules.copy()

        for a, b in swaps:
            rules[a], rules[b] = rules[b], rules[a]

        for z in self.zs:
            if z in values:
                continue
            stack = [z]
            seen = collections.defaultdict(int)
            while stack:
                node = stack.pop()
                if node in values:
                    continue

                # If seen more than twice, then cycle detected.
                seen[node] += 1
                if seen[node] > 2:
                    return 0

                formula = rules[node]
                if formula.ready(values):
                    values[node] = formula.calculate(values)
                    continue
                stack.append(node)
                stack.append(formula.wire0)
                stack.append(formula.wire1)

        return int("".join([str(values[z]) for z in self.zs]), base=2)

    def slow_zvalue(self) -> int:
        values = self.values.copy()

        found = True
        while found:
            found = False
            for wire, formula in self.rules.items():
                if wire in values:
                    continue
                if not formula.ready(values):
                    continue
                values[wire] = formula.calculate(values)
                found = True

        zs = []
        for key, value in values.items():
            if not key.startswith("z"):
                continue
            zs.append((key, value))
        zs.sort()
        zs.reverse()
        return int("".join([str(z[1]) for z in zs]), base=2)

    def print_ztree(self):
        for z in self.zs:
            print(z)
            formula = self.rules[z]
            queue = [(formula.wire0, 1), (formula.gate, 1), (formula.wire1, 1)]
            while queue:
                item, level = queue.pop()
                if level > 5:
                    continue
                print("\t" * level + item)
                formula = self.rules.get(item)
                if formula:
                    queue.append((formula.wire0, level + 1))
                    queue.append((formula.gate, level + 1))
                    queue.append((formula.wire1, level + 1))

    def print_tree(self):
        for wire in self.rules:
            print(wire)
            formula = self.rules[wire]
            queue = [(formula.wire0, 1), (formula.gate, 1), (formula.wire1, 1)]
            while queue:
                item, level = queue.pop()
                if level > 3:
                    continue
                print("\t" * level + item)
                formula = self.rules.get(item)
                if formula:
                    queue.append((formula.wire0, level + 1))
                    queue.append((formula.gate, level + 1))
                    queue.append((formula.wire1, level + 1))

    def ancestors(self, node: str) -> Set[str]:
        all_ancestors = collections.defaultdict(set)
        for output, formula in self.rules.items():
            all_ancestors[formula.wire0].add(output)
            all_ancestors[formula.wire1].add(output)

        ret = set()
        stack = list(all_ancestors[node])
        while stack:
            node = stack.pop()
            if node in ret:
                return set()
            ret.add(node)
            stack.extend(all_ancestors[node])
        return ret

    def descendants(self, node: str) -> Optional[Set[str]]:
        ret = set()
        if node not in self.rules:
            return ret

        stack = [self.rules[node].wire0, self.rules[node].wire1]
        while stack:
            node = stack.pop()
            if node in ret:
                continue
            ret.add(node)
            if node in self.rules:
                stack.extend([self.rules[node].wire0, self.rules[node].wire1])
        return ret


# Bits that need flipped.
# Ideas:
#   1. Brute force all possible swaps. Too slow!
#   2. Do a filtered set of swaps. Still too slow!
#      - Don't swap if it introduces a cycle.
#      - Don't swap if the swap does not change values.
#
# Need some way to do fewer swaps. Doing all sways is way too slow.


def solution0search(ctx: Context, seen: Set[str], swaps=None):
    swaps = swaps or []
    if len(swaps) <= 3:
        print(swaps)
    if len(seen) == 8:
        ret = ctx.zvalue(swaps) == ctx.targetzvalue
        if ret:
            print(swaps)
        return ret

    values = ctx.allvalues()
    if not values:
        return False

    for w0 in ctx.rules:
        if w0 in seen:
            continue
        for w1 in ctx.rules:
            if values[w0] == values[w1]:
                continue
            if w0 == w1:
                continue
            if w1 in seen:
                continue
            res = solution0search(ctx, seen | set([w0, w1]), swaps + [(w0, w1)])
            if res:
                return res
    return False


def solution0(ctx: Context):
    return solution0search(ctx, set())


def main():
    values: Dict[str, int] = {}
    rules: Dict[str, Formula] = collections.OrderedDict()
    with open("./data/test.in") as f:
        for line in f:
            if ":" in line:
                wire, value = line.split(": ")
                values[wire] = int(value.strip())
            if "->" in line:
                wire0, gate, wire1, _, output = line.split()
                rules[output] = Formula(wire0=wire0, gate=gate, wire1=wire1)
    ctx = Context(values=values, rules=rules)
    print(" " + bin(ctx.xvalue))
    print(" " + bin(ctx.yvalue))
    print(bin(ctx.targetzvalue))

    swaps = [
        ("z12", "djg"),
        ("z19", "sbg"),
        ("mcq", "hjm"),
        ("z37", "dsd"),
    ]

    print(bin(ctx.zvalue(swaps)))
    # ctx.print_ztree()


if __name__ == "__main__":
    main()
