f = './inp/day13.txt'


def parse_file():
    with open(f) as F:
        coords = []
        rules = []
        coord_end = False
        maxX = 0
        maxY = 0
        for line in F.readlines():
            line = line.strip()
            if len(line) == 0:
                coord_end = True
            elif coord_end:
                sp = line.split('=')
                rules.append((sp[0][-1], int(sp[1])))
            else:
                x, y = tuple(map(int, line.split(',')))
                maxX = max(x+1, maxX)
                maxY = max(y+1, maxY)
                coords.append((x, y))
        M = [[False]*maxX for _ in range(maxY)]
        for a, b in coords:
            M[b][a] = True
        return M, rules


def day13(is_first):
    M, rules = parse_file()
    minX = len(M[0])
    minY = len(M)
    for dir, nb in rules:
        if dir == 'y':
            for a in range(nb, minY):
                pos = minY-a-1
                M[pos] = [(e1 or e2) for (e1, e2) in zip(M[pos], M[a])]
            minY = min(nb+1, minY)
        else:
            for line in M:
                for a in range(nb, minX):
                    pos = minX-a-1
                    line[pos] = line[pos] or line[a]
            minX = min(nb+1, minX)
        if is_first:
            break
    acc = 0
    [[acc := acc + M[i][j] for j in range(minX)] for i in range(minY)]
    return acc


# print(day13(True))
print(day13(False))
