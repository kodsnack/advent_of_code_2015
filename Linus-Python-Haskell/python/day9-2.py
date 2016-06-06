#!/usr/bin/python3

import itertools
from parse import get_lines


def parseLine(line):
    l = line.split(' ')
    return l[0], (l[2], int(l[4]))


def path_length(d, path):
    length = 0
    for i in range(0, len(path)-1):
        if path[i] in d[path[i-1]]:
            length += d[path[i-1]][path[i]]
    return length


def shortest_hamiltonian_path(d):
    return max(map(lambda x: path_length(d, x), itertools.permutations(sorted(d.keys()))))

def buildGraph(lines):
    d = {}
    for line in lines:
        f, (t, l) = parseLine(line)
        if not f in d.keys():
            d[f] = {}
        if not t in d.keys():
            d[t] = {}
        d[f][t] = l
        d[t][f] = l

    return shortest_hamiltonian_path(d)


def solve(lines):
    d = buildGraph(lines)
    return d

if __name__ == '__main__':
    lines = (get_lines('input9.txt'))
    print(solve(lines))

