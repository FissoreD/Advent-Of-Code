from typing import List, Set


f = './inp/day8.txt'

D = {
    0: "abcefg",
    1: "cf",
    2: "acdeg",
    3: "acdfg",
    4: "bcdf",
    5: "abdfg",
    6: "abdefg",
    7: "acf",
    8: "abcdefg",
    9: "abcdfg"
}


def parse_line(l: str):
    a, b = tuple(l.strip().split('|'))
    return a.strip().split(), b.strip().split()


def day8_1():
    N = [len(D[i]) for i in [1, 4, 7, 8]]
    acc = 0
    with open(f) as F:
        for line in F.readlines():
            _, b = parse_line(line)
            acc += sum([i in N for i in [len(x) for x in b]])
    return acc


def remove_singleton(L: List[Set[str]]):
    Q = [i for i in L if len(i) == 1]
    while Q:
        fst = Q.pop()
        for pos in range(len(L)):
            if len(L[pos]) != 1:
                L[pos] = L[pos] - fst
                if len(L[pos]) == 1:
                    Q.append(L[pos])


def solve_simple(L):
    L1 = [len(i) for i in L]
    one = set(L[L1.index(len(D[1]))])
    four = set(L[L1.index(len(D[4]))])
    seven = set(L[L1.index(len(D[7]))])

    # values is a list of segments
    # at first each segment contains every letter from a to g
    values = [{chr(ord('a') + i) for i in range(7)} for _ in range(7)]
    values[0] = seven - one     # seg(0) = seg(7) - seg(1)
    values[2] = one.copy()      # seg(2) = seg(1)
    values[5] = one.copy()      # same
    values[1] = four - seven    # seg(1) = seg(4) - seg(7)
    values[3] = four - seven    # same
    # seg(4) = everysegment - (seg(7) U seg(4))
    values[4] = values[4] - (seven.union(four))

    zero_six_nine = [i for i in L if len(i) == 6]
    # seg(1), seg(5) & seg(6) must be in 0, 6 & 9
    for i in [1, 5, 6]:
        for j in zero_six_nine:
            values[i] = values[i].intersection(set(j))

    two_three_five = [i for i in L if len(i) == 5]
    # seg(3)& seg(6) must be in 2, 3 and 5
    for i in [3, 6]:
        for j in two_three_five:
            values[i] = values[i].intersection(set(j))

    remove_singleton(values)
    # list of set of one element to list of int
    values = [i.pop() for i in values]
    mySet = [set() for _ in range(10)]
    for j in range(len(mySet)):
        res = set()
        for x in D[j]:
            res.add(values[ord(x) - ord('a')])
        mySet[j] = res
    return mySet


def day8_2():
    acc = 0
    with open(f) as F:
        for line in F.readlines():
            a, b = parse_line(line)
            association = solve_simple(a)
            res = []
            for x in b:
                for pos, j in enumerate(association):
                    if j == set(x):
                        res.append(str(pos))
            acc += int("".join(res))
    return acc


print(day8_1())
print(day8_2())
