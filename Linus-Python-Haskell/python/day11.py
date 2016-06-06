#!/usr/bin/python3

from parse import get_lines


def inc(s):
    s = list(s)
    carry = 1
    pos = len(s)-1
    while carry == 1:
        c = s[pos]
        if c == 'z':
            s[pos] = 'a'
            pos -= 1
        else:
            s[pos] = chr(ord(c)+1)
            carry = 0
    return ''.join(s)


def has_increasing_sequence(s):
    s = list(s)
    z = zip(s, s[1:], s[2:])
    for a, b, c in z:
        if ord(a)+1 == ord(b) and ord(a)+2 == ord(c):
            return True
    return False


def has_two_pairs(s):
    pairs = set()
    s = list(s)
    z = zip(s, s[1:])
    for a, b in z:
        if a == b:
            pairs.add(a)
    if len(pairs) >= 2:
        return True
    return False


def is_ok(s):
    #print(has_increasing_sequence('asdfasgsdfabcasdfasdf'))
    #print(has_increasing_sequence('asdfasgsdfasdfasdf'))
    #print(has_two_pairs('aadfgdfgdfgbb'))
    #print(has_two_pairs('adfgdfgdfgb'))
    #return True
    return has_increasing_sequence(s) and not 'i' in s and not 'o' in s and not 'l' in s and has_two_pairs(s)


def solve(input):
    s = input
    while not is_ok(s):
        s = inc(s)
    return s

if __name__ == '__main__':
    print(solve('vzbxkghb'))
    print(solve(inc('vzbxxyzz')))

