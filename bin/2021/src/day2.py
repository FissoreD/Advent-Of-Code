from io import TextIOWrapper
f = './inp/day2.txt'


def parse_line(f: TextIOWrapper):
    a, b = tuple(f.readline().strip().split())
    return (a == 'forward', int(b) * (-1 if a == 'up' else 1))


def day2_1():
    F = open(f)
    hor = 0
    dept = 0
    try:
        while True:
            x, y = parse_line(F)
            if x:
                hor += y
            else:
                dept += y
    except ValueError:
        F.close()
        return hor * dept


def day2_2():
    F = open(f)
    hor = 0
    aim = 0
    dept = 0
    try:
        while True:
            x, y = parse_line(F)
            if x:
                hor += y
                dept += aim * y
            else:
                aim += y
    except ValueError:
        F.close()
        return hor * dept


print(day2_1())
print(day2_2())
