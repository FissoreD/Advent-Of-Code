f = './inp/day5.txt'


def my_split(l: str): return tuple(map(int, l.split(',')))


def parse_line(l: str): return tuple(map(my_split, l.strip().split(' -> ')))


def day5_1(isPart2):
    L = [[[0, False] for _ in range(1024)] for _ in range(1024)]
    cpt = 0
    with open(f) as F:
        l = F.readline()
        while len(l) != 0:
            ((a, b), (c, d)) = parse_line(l)
            if b == d:
                for i in range(min(a, c), max(a, c)+1):
                    L[b][i][0] += 1
                    if L[b][i][0] == 2 and L[b][i][1] == False:
                        cpt += 1
                        L[b][i][1] = True
            elif a == c:
                for i in range(min(b, d), max(b, d)+1):
                    L[i][a][0] += 1
                    if L[i][a][0] == 2 and L[i][a][1] == False:
                        cpt += 1
                        L[i][a][1] = True
            elif isPart2 and abs(c - a) == abs(d - b):
                coeffX = 1 if a < c else -1
                coeffY = 1 if b < d else -1
                for i in range(abs(c-a)+1):
                    cY = a + i * coeffX
                    cX = b + i * coeffY
                    L[cX][cY][0] += 1
                    if L[cX][cY][0] == 2 and L[cX][cY][1] == False:
                        cpt += 1
                        L[cX][cY][1] = True
                assert(a+i*coeffX == c and d == b+i*coeffY)
            l = F.readline()
        return cpt


print(day5_1(False))
print(day5_1(True))
