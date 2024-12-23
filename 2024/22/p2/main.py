import itertools
import collections
from typing import List, Tuple


def mix(secret: int, value: int):
    return secret ^ value


def prune(secret: int):
    return secret % 16777216


#    Calculate the result of multiplying the secret number by 64. Then, mix this result into the secret number. Finally, prune the secret number.
#    Calculate the result of dividing the secret number by 32. Round the result down to the nearest integer. Then, mix this result into the secret number. Finally, prune the secret number.
#    Calculate the result of multiplying the secret number by 2048. Then, mix this result into the secret number. Finally, prune the secret number.
def next_number(secret: int):
    res0 = prune(mix(secret, secret * 64))
    res1 = prune(mix(res0, res0 // 32))
    res2 = prune(mix(res1, res1 * 2048))
    return res2


def simulate(secret: int, iterations: int):
    deltas = []
    prev = None
    for _ in range(iterations):
        secret = next_number(secret)
        if prev:
            deltas.append((secret % 10, (secret % 10) - (prev % 10)))
        prev = secret
    return deltas


def price(deltas: List[List[Tuple[int, int]]], sequence: Tuple[int, int, int, int]):
    ret = 0
    for delta in deltas:
        for i, _ in enumerate(delta):
            if i + 3 >= len(delta):
                break
            if (
                delta[i][1] == sequence[0]
                and delta[i + 1][1] == sequence[1]
                and delta[i + 2][1] == sequence[2]
                and delta[i + 3][1] == sequence[3]
            ):
                ret += delta[i + 3][0]
                break
    return ret


def valid_sequence(sequence: Tuple[int, int, int, int]) -> bool:
    start = 9
    for s in sequence:
        start += s
    if not 0 <= start <= 9:
        return False

    start = 0
    for s in sequence:
        start += s
    if not 0 <= start <= 9:
        return False

    return True


def solution0(buyers: List[int]):
    deltas = []
    for buyer in buyers:
        deltas.append(simulate(buyer, 2000))

    iterations = 0
    maxprice = float("-inf")
    for a, b, c, d in itertools.product(*(range(-9, 10) for _ in range(4))):
        if iterations % 100 == 0:
            print(iterations, a, b, c, d, maxprice)
        iterations += 1
        if not valid_sequence((a, b, c, d)):
            continue
        maxprice = max(maxprice, price(deltas, (a, b, c, d)))
    return maxprice


def solution1(buyers: List[int]):
    deltas = []
    for buyer in buyers:
        deltas.append(simulate(buyer, 2000))

    sequences = collections.defaultdict(dict)
    for idx, delta in enumerate(deltas):
        for i, _ in enumerate(delta):
            if i + 3 >= len(delta):
                break
            (_, a), (_, b), (_, c), (value, d) = delta[i : i + 4]
            if idx not in sequences[(a, b, c, d)]:
                sequences[(a, b, c, d)][idx] = value

    maxprice = float("-inf")
    for v in sequences.values():
        maxprice = max(maxprice, sum(v.values()))
    return maxprice


def main():
    buyers = []
    with open("./data/sample0.in") as f:
        for line in f:
            buyers.append(int(line.strip()))

    print("Sum", solution1(buyers))


if __name__ == "__main__":
    main()
