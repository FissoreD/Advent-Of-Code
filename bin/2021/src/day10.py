f = './inp/day10.txt'

op = '([{<'
cl = ')]}>'


def corrupted_line(l: str):
    val = {
        ')': 3,
        ']': 57,
        '}': 1197,
        '>': 25137
    }
    P = []
    for i in l:
        if i in op:
            P.append(i)
        elif cl.index(i) == op.index(P[-1]):
            P.pop()
        else:
            return val[i]
    return 0


def day10_1():
    acc = 0
    with open(f) as F:
        for line in F.readlines():
            acc += corrupted_line(line.strip())
    return acc


def incomplete_line(l: str):
    P = []
    val = {
        '(': 1,
        '[': 2,
        '{': 3,
        '<': 4
    }
    for i in l:
        if i in op:
            P.append(i)
        elif cl.index(i) == op.index(P[-1]):
            P.pop()
        else:
            return 0
    acc = 0
    for i in reversed(P):
        acc = acc * 5 + val[i]
    return acc


def day10_2():
    L = []
    with open(f) as F:
        for line in F.readlines():
            x = incomplete_line(line.strip())
            None if x == 0 else L.append(x)
    return sorted(L)[len(L)//2]


print(day10_1())
print(day10_2())
