f = './inp/day11.txt'


def matrix():
    with open(f) as F:
        return [[int(i) for i in line.strip()] for line in F.readlines()]


def neigh(M, x, y):
    return [(a, b) for (a, b) in [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1, y+1)] if a >= 0 and b >= 0 and a < len(M[0]) and b < len(M)]


def increase_by_one(M):
    for line in M:
        for x in range(len(line)):
            line[x] += 1


def flash_mat(M, acc=0):
    mod = False
    for y, line in enumerate(M):
        for x, elt in enumerate(line):
            if elt >= 10:
                acc += 1
                N = neigh(M, x, y)
                for a, b in N:
                    M[b][a] += M[b][a] != 0
                    mod = True
                line[x] = 0
    return flash_mat(M, acc) if mod else acc


def day11_1():
    M = matrix()
    acc = 0
    for _ in range(100):
        increase_by_one(M)
        acc += flash_mat(M)
    return acc


def all_flash(M):
    res = flash_mat(M)
    return res == len(M) * len(M[0])


def day11_2():
    acc = 0
    M = matrix()
    while True:
        acc += 1
        increase_by_one(M)
        if all_flash(M):
            return acc


print(day11_1())
print(day11_2())
