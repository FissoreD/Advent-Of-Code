from io import TextIOWrapper
from typing import List
import numpy as np
f = './inp/day4.txt'


def first_line(s: str):
    return list(map(int, s.strip().split(',')))


def read_mat(f: TextIOWrapper):
    L = []
    if len(f.readline()) == 0:
        return
    for _ in range(5):
        L.append(list(map(int, f.readline().strip().split())))
    return L


def valid_mat(M):
    return 5 in M['lines'] or 5 in M['cols']


def day4_1():
    with open(f) as F:
        l = first_line(F.readline())
        M = []
        res = read_mat(F)
        while res is not None:
            M.append(res)
            res = read_mat(F)
        res = [{'lines': [0 for i in range(5)], 'cols':[
            0 for i in range(5)]} for _ in M]
        for i in l:
            for (posj, j) in enumerate(M):
                for (posk, k) in enumerate(j):
                    for (posl, l) in enumerate(k):
                        if l == i:
                            res[posj]['lines'][posk] += 1
                            res[posj]['cols'][posl] += 1
                            k[posl] = str(l)
                if valid_mat(res[posj]):
                    pos = sum([sum([int(x) for x in y if type(x) != str])
                              for y in j])
                    return pos * i


def day4_2():
    with open(f) as F:
        l = first_line(F.readline())
        M = []
        res = read_mat(F)
        while res is not None:
            M.append(res)
            res = read_mat(F)
        res = [{'lines': [0 for i in range(5)], 'cols':[
            0 for i in range(5)]} for _ in M]
        for i in l:
            popped = True
            while popped:
                popped = False
                for (posj, j) in enumerate(M):
                    for (posk, k) in enumerate(j):
                        for (posl, l) in enumerate(k):
                            if l == i:
                                res[posj]['lines'][posk] += 1
                                res[posj]['cols'][posl] += 1
                                k[posl] = str(l)
                    if valid_mat(res[posj]):
                        if len(M) == 1:
                            pos = sum([sum([int(x) for x in y if type(x) != str])
                                       for y in M[0]])
                            return pos * i
                        M.pop(posj)
                        res.pop(posj)
                        popped = True
                        break


print(day4_1())
print(day4_2())
