from io import TextIOWrapper
f = './inp/day3.txt'


def parse_line(l):
    return [i for i in l.strip()]


def update_list(L, M):
    for (pos, val) in enumerate(M):
        if val == '0':
            L[pos][0] += 1
        else:
            L[pos][1] += 1


def list_to_bit(L: list, bigger: bool):
    return "".join(['0' if bigger ^ (i[0] > i[1]) else '1' for i in L])


def day3_1():
    with open(f) as F:
        M = parse_line(F.readline())
        L = [[0, 0] for i in range(len(M))]
        update_list(L, M)
        for line in F:
            M = parse_line(line)
            update_list(L, M)
    return int(list_to_bit(L, True), 2) * int(list_to_bit(L, False), 2)


def find_best_most(M: list, index: int):
    nb_of_one = sum([elt[index] == '1' for elt in M])
    if nb_of_one == len(M) or nb_of_one == 0:
        return M
    if nb_of_one * 2 == len(M):
        return [i for i in M if i[index] == '1']
    most = '1' if nb_of_one * 2 > len(M) else '0'
    return [elt for elt in M if elt[index] == most]


def find_best_less(M: list, index: int):
    nb_of_one = sum([elt[index] == '1' for elt in M])
    if nb_of_one == len(M) or nb_of_one == 0:
        return M
    if nb_of_one * 2 == len(M):
        return [i for i in M if i[index] == '0']
    less = '1' if nb_of_one * 2 < len(M) else '0'
    return [elt for elt in M if elt[index] == less]


def day3_2():
    with open(f) as F:
        M = []
        for line in F:
            M.append(parse_line(line))
        M1 = [i for i in M]
        M2 = [i for i in M]
        for i in range(len(M1[0])):
            M1 = find_best_most(M1, i)
            if len(M1) == 1:
                M1 = int("".join(M1[0]), 2)
                break
            if len(M1) == 2:
                M1 = max(int("".join(i), 2) for i in M1)
                break
        for i in range(len(M2[0])):
            M2 = find_best_less(M2, i)
            if len(M2) == 1:
                M2 = int("".join(M2[0]), 2)
                break
            if len(M2) == 2:
                M2 = min(int("".join(i), 2) for i in M2)
                break
    return M1 * M2


print(day3_1())
print(day3_2())
