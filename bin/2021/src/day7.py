from typing import Counter
f = './inp/day7.txt'


def get_line():
    with open(f) as F:
        return Counter(map(int, F.readline().strip().split(',')))


def day7_1():
    C = get_line()
    current_value = float('inf')
    for c in range(max(C)):
        c1 = sum([abs(x - c)*C[x] for x in C])
        if c1 < current_value:
            current_value = c1
        if c1 > current_value:
            break
    return current_value


def day7_2():
    C = get_line()
    current_value = float('inf')
    for c in range(max(C)):
        def val(x): return abs(x - c)
        def sumX(x): return val(x)*(val(x)+1)//2
        c1 = sum([sumX(x)*C[x] for x in C])
        if c1 < current_value:
            current_value = c1
        if c1 > current_value:
            break
    return current_value


print(day7_1())
print(day7_2())
