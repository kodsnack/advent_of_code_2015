#!/usr/bin/python3

from parse import get_lines


def diffLine(line):
    return len(line.strip()) - len(eval(line.strip()))


def solve(lines):
    return sum(map(diffLine, lines))


if __name__ == '__main__':
    lines = (get_lines('input8.txt'))
    print(solve(lines))

