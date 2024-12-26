from typing import Dict, Optional, OrderedDict, Set, Tuple
import attr
import collections
import copy


@attr.define
class Context:
    memo: dict
    initialvalues: dict
    rules: list
    target: int


def calculateoutput(ctx: Context, swaps: frozenset) -> int:
    values = ctx.initialvalues.copy()
    swapdict = {}
    for swap in swaps:
        w0, w1 = list(swap)
        swapdict[w0] = w1
        swapdict[w1] = w0
    found = True
    while found:
        found = False
        for i, _ in enumerate(ctx.rules):
            if i in swapdict:
                rule = ctx.rules[swapdict[i]]
            else:
                rule = ctx.rules[i]
            wire0, gate, wire1, output = rule
            # This is a bad swap. Skip it.
            if rule in (wire0, wire1):
                return False
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
    return int("".join([str(z[1]) for z in zs]), base=2)


def search(ctx: Context, swaps: frozenset, seen: set) -> bool:
    if len(swaps) <= 1:
        print(swaps)
    value = ctx.memo.get(swaps)
    if value:
        return value

    if len(swaps) == 4:
        ret = calculateoutput(ctx, swaps) == ctx.target
        if ret:
            print("FOUND!!!!", swaps, ret)
        ctx.memo[swaps] = ret
        return ret

    found = False
    for i, _ in enumerate(ctx.rules):
        if i in seen:
            continue
        for j in range(i + 1, len(ctx.rules)):
            if j in seen or i == j:
                continue
            found |= search(
                ctx,
                swaps | frozenset([(min(i, j), max(i, j))]),
                seen | set([i, j]),
            )
    ctx.memo[swaps] = found
    return found


def solution0():
    values = {}
    rules = []
    with open("./data/test.in") as f:
        for line in f:
            if ":" in line:
                wire, value = line.split(": ")
                values[wire] = int(value.strip())
            if "->" in line:
                wire0, gate, wire1, _, output = line.split()
                rules.append((wire0, gate, wire1, output))

    xs = []
    for key, value in values.items():
        if not key.startswith("x"):
            continue
        xs.append((key, value))
    xs.sort()
    xs.reverse()
    xs_val = int("".join([str(x[1]) for x in xs]), base=2)

    ys = []
    for key, value in values.items():
        if not key.startswith("y"):
            continue
        ys.append((key, value))
    ys.sort()
    ys.reverse()
    ys_val = int("".join([str(y[1]) for y in ys]), base=2)

    target_val = xs_val + ys_val
    print(f"Number of rules: {len(rules)}")
    print(f"Target: {xs_val}+{ys_val}={target_val}")

    ctx = Context(
        memo={},
        initialvalues=values,
        rules=rules,
        target=target_val,
    )
    search(ctx, frozenset(), set())

    found = set()
    for k, v in ctx.memo.items():
        if len(k) == 4 and v:
            print("Result", k)
            found = k

    res = []
    for pair in found:
        for item in pair:
            res.append(item)
    res.sort()
    print("".join(res))


# descendants
# node => dependents
# node => ancestors

# w0, gate, w1, output

# original rules
# output -> (w0, gate, w1)
# w -> [output, ...]

# values
# w -> value

# don't swap if equal in value.

# recalculate all outputs for swapped wires if not equal.
# if cycle introduced, return False

# values
# output -> value


NUM_ZS = 45


def calculatevalues(rules, values) -> None:
    found = True
    while found:
        found = False
        for output, formula in rules.items():
            if output in values or not formula.ready(values):
                continue
            values[output] = formula.calculate(values)
            found = True


@attr.define
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
class Context1:
    values: Dict[str, int]
    rules: OrderedDict[str, Formula]
    dependents: Dict[str, Set[str]]

    def zvalue(self) -> int:
        zs = []
        for key, value in self.values.items():
            if not key.startswith("z"):
                continue
            zs.append((key, value))
        zs.sort()
        zs.reverse()
        return int("".join([str(z[1]) for z in zs]), base=2)

    # Returns empty set if cycle detected. Else, returns the set of all dependents.
    def alldependents(self, dependent: str) -> Set[str]:
        dependents = set()
        stack = [dependent]
        while stack:
            node = stack.pop()
            if node in dependents:
                return set()
            dependents.add(node)
            stack.extend(self.dependents[node])
        return dependents

    def swap(self, swap: Tuple[str, str]) -> bool:
        w1, w2 = swap
        if self.values[w1] == self.values[w2]:
            return False

        f1 = self.rules[w1]
        self.dependents[f1.wire0].remove(w1)
        self.dependents[f1.wire0].add(w2)
        self.dependents[f1.wire1].remove(w1)
        self.dependents[f1.wire1].add(w2)

        f2 = self.rules[w2]
        self.dependents[f2.wire0].remove(w2)
        self.dependents[f2.wire0].add(w1)
        self.dependents[f2.wire1].remove(w2)
        self.dependents[f2.wire1].add(w1)

        self.rules[w1], self.rules[w2] = f2, f1
        return True

    def recupdate(self, val: str):
        prev = self.values[val]
        cur = self.rules[val].calculate(self.values)
        if prev == cur:
            return
        self.values[val] = cur
        for dep in self.dependents[val]:
            self.recupdate(dep)

    def updatevalues(self, swap: Tuple[str, str]) -> bool:
        w1, w2 = swap

        w1_deps = self.alldependents(w1)
        if not w1_deps:
            return False

        w2_deps = self.alldependents(w2)
        if not w2_deps:
            return False

        chain = False
        w1parent = False
        if w1 in w2_deps:
            chain = True
        if w2 in w1_deps:
            assert not chain
            chain = True
            w1parent = True

        if chain:
            if w1parent:
                self.recupdate(w1)
            else:
                self.recupdate(w2)
        else:
            self.recupdate(w1)
            self.recupdate(w2)
        return True


def solution1():
    values = {}
    rules = collections.OrderedDict()
    dependents = collections.defaultdict(set)
    with open("./data/test.in") as f:
        for line in f:
            if ":" in line:
                wire, value = line.split(": ")
                values[wire] = int(value.strip())
            if "->" in line:
                wire0, gate, wire1, _, output = line.split()
                rules[output] = Formula(wire0=wire0, gate=gate, wire1=wire1)
                dependents[wire0].add(output)
                dependents[wire1].add(output)

    # exclude any dependents that aren't an output since their value will
    # never change due to swaps.
    # tmp = dependents.copy()
    # for dep in tmp.keys():
    #     if dep not in rules:
    #         del dependents[dep]

    # populate initial values with no swaps
    found = True
    while found:
        found = False
        for output, formula in rules.items():
            if output in values or not formula.ready(values):
                continue
            values[output] = formula.calculate(values)
            found = True
    calculatevalues(rules, values)

    xs = []
    for key, value in values.items():
        if not key.startswith("x"):
            continue
        xs.append((key, value))
    xs.sort()
    xs.reverse()
    xs_val = int("".join([str(x[1]) for x in xs]), base=2)

    ys = []
    for key, value in values.items():
        if not key.startswith("y"):
            continue
        ys.append((key, value))
    ys.sort()
    ys.reverse()
    ys_val = int("".join([str(y[1]) for y in ys]), base=2)

    target_val = xs_val + ys_val

    def search1(ctx: Context1, seen) -> Optional[int]:
        print(seen)
        if len(seen) == 8:
            if ctx.zvalue() == target_val:
                print(ctx)
                return ctx.zvalue()
            return None
        for i, w1 in enumerate(ctx.rules):
            for j, w2 in enumerate(ctx.rules):
                if j <= i:
                    continue
                if i in seen or j in seen:
                    continue
                newctx = copy.deepcopy(ctx)
                if not newctx.swap((w1, w2)):
                    continue
                if not newctx.updatevalues((w1, w2)):
                    continue
                search1(newctx, seen | set([i, j]))

    search1(Context1(values=values, rules=rules, dependents=dependents), set())


def main():
    solution1()


if __name__ == "__main__":
    main()
