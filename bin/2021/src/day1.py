from io import TextIOWrapper
f = './inp/day1.txt'


def parse_inp(f: TextIOWrapper):
    return int(f.readline().strip())


def day1_1():
    F = open(f)
    current = parse_inp(F)
    acc = 0
    try:
        while True:
            new = parse_inp(F)
            if new > current:
                acc += 1
            current = new
    except ValueError:
        F.close()
        return acc


def day1_2():
    F = open(f)
    L = [parse_inp(F) for _ in range(3)]
    current = sum(L)
    acc = 0
    try:
        while True:
            L.pop(0)
            L.append(parse_inp(F))
            new = sum(L)
            if new > current:
                acc += 1
            current = new
    except ValueError:
        F.close()
        return acc


print(day1_1())
print(day1_2())
