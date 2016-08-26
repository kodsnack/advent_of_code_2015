#!/usr/bin/python3

from parse import get_lines

def encode(s):
    return '"' + s.replace('\\', '\\\\').replace('"', '\\"') + '"'

def diffLine(line):
    return len(encode(line.strip())) - len(line.strip())


def solve(lines):
    return sum(map(diffLine, lines))


if __name__ == '__main__':
    lines = (get_lines('input8.txt'))
    print(solve(lines))

