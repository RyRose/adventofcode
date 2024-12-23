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
    for _ in range(iterations):
        secret = next_number(secret)
    return secret


def main():
    buyers = []
    with open("./data/test.in") as f:
        for line in f:
            buyers.append(int(line.strip()))

    total = 0
    for buyer in buyers:
        total += simulate(buyer, 2000)
    print("Sum", total)


if __name__ == "__main__":
    main()
