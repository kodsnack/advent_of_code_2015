#!/usr/bin/python3

import re
from parse import get_lines


def solve(lines):
    s = re.sub(r'[^0-9-]+', ' ', lines[0])
    return sum(map(int, s.strip().split(' ')))


if __name__ == '__main__':
    lines = (get_lines('input12.txt'))
    print(solve(lines))

