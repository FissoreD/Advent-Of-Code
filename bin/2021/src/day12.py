f = './inp/day12.txt'


def read_line(l: str):
    return tuple(l.strip().split('-'))


def create_graph():
    def add_to_dict(G, a, b):
        if a in G:
            G[a].append(b)
        else:
            G[a] = [b]
    G = {}
    with open(f) as F:
        for line in F.readlines():
            a, b = read_line(line)
            add_to_dict(G, a, b)
            add_to_dict(G, b, a)
    return G


def day12_1(start='start',
            explored=['start'],
            G=create_graph()):
    next = G[start]
    if start == 'end':
        return 1
    acc = 0
    for n in next:
        if not n in explored:
            acc += day12_1(n,
                           explored if n.isupper()
                           else (explored + [n]), G)
    return acc


def day12_2(start='start',
            explored=['start'],
            G=create_graph(),
            twice=False, start_visited=False):
    if start == 'end':
        return 1
    next = G[start]
    acc = 0
    for n in next:
        if start_visited and start == 'start':
            continue
        if not (n in explored):
            acc += day12_2(n,
                           explored if n.isupper()
                           else (explored + [n]), G, twice, True)
        elif not twice:
            acc += day12_2(n, explored, G, True, True)
    return acc


print(day12_1())
print(day12_2())
