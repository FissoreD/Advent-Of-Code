from functools import reduce
from operator import mul
from bisect import insort
f = './inp/day9.txt'

ngb = [(-1, 0), (1, 0), (0, -1), (0, 1)]


def cond(M, a, b):
    return a >= 0 and b >= 0 and a < len(M[0]) and b < len(M)


def valid_ngb(M: list[list[int]], x, y):
    ngbs = [((x + i[0]), y+i[1]) for i in ngb]
    valid_ngbs = [j for j in ngbs if cond(M, j[0], j[1])]
    return [M[y][x] for (x, y) in valid_ngbs]


def day8_1():
    with open(f) as F:
        acc = 0
        M = [[int(i) for i in x.strip()] for x in F.readlines()]
        for x in range(len(M[0])):
            for y in range(len(M)):
                if M[y][x] < min(valid_ngb(M, x, y)):
                    acc += M[y][x]+1
        return acc


def mark_basin(M: list[list[int]], x: int, y: int, acc: int):
    if cond(M, x, y) and M[y][x] != 9:
        M[y][x] = 9
        acc += 1
        for a, b in ngb:
            acc = mark_basin(M, x+a, y+b, acc)
    return acc


def day8_2():
    with open(f) as F:
        L = [0, 0, 0]
        M = [[int(i) for i in x.strip()] for x in F.readlines()]
        for x in range(len(M[0])):
            for y in range(len(M)):
                insort(L, mark_basin(M, x, y, 0))
                L.pop(0)
        return reduce(mul, L, 1)


print(day8_1())
print(day8_2())
