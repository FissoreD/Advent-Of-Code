from collections import Counter
f = './inp/day6.txt'


def parse_line(l: str):
    return list(map(int, l.strip().split(',')))


def day6_1():
    with open(f) as F:
        l = parse_line(F.readline())
        for _ in range(80):
            for i in range(len(l)):
                l[i] -= 1
                if l[i] == -1:
                    l[i] = 6
                    l.append(8)
        return len(l)


def day6_2():
    with open(f) as F:
        l = sorted(parse_line(F.readline()))
        l1 = [l.count(i) for i in range(7)]
        waiting = [0, 0]
        acc = len(l)
        for _ in range(256):
            nb = l1.pop(0)
            l1.append(nb + waiting.pop(0))
            waiting.append(nb)
            acc += nb
        return acc


print(day6_1())
print(day6_2())
