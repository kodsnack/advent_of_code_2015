#!/usr/bin/python3

import time

d = {}

indent = 0

def eval(x):
    try:
        global indent
        indent += 1
        #time.sleep(0.1)
        #print(' ' * indent + 'start eval')
        if x.isdigit():
            return int(x)
        expr = d[x]

        #print(' ' * indent + expr)
        tokens = expr.split(' ')
        #print(tokens)
        result = ''
        if len(tokens) == 1:
            if tokens[0].isdigit():
                result = int(tokens[0])
            else :
                result = eval(tokens[0])
        elif tokens[0] == 'NOT':
            result = NOT(eval(tokens[1]))
        elif tokens[1] == 'AND':
            result = AND(eval(tokens[0]), eval(tokens[2]))
        elif tokens[1] == 'OR':
            result = OR(eval(tokens[0]), eval(tokens[2]))
        elif tokens[1] == 'RSHIFT':
            result = RSHIFT(eval(tokens[0]), eval(tokens[2]))
        elif tokens[1] == 'LSHIFT':
            result = LSHIFT(eval(tokens[0]), eval(tokens[2]))
        d[x] = str(result)
        return result
        raise RecursionError()
    finally:
      #print(' ' * indent + 'exit ' + str(x))
        indent -= 1
        pass

def NOT(x):
    return (~x) % (1 << 16)

def AND(x, y):
    return (x & y) % (1 << 16)

def OR(x, y):
    return (x | y) % (1 << 16)

def RSHIFT(x, y):
    return (x >> y) % (1 << 16)

def LSHIFT(x, y):
    return (x << y) % (1 << 16)


def getLines(inFile):
    lines = []
    try:
        with open(inFile, 'r') as f:
            while True:
                line = f.readline()
                if line == '':
                    break
                lines.append(line)
    except EOFError:
        pass
    return lines

def fillDict(lines):
    for line in lines:
        a = line.split(' -> ')
        d[a[1].strip()] = a[0]

if __name__ == '__main__':
    fillDict(getLines('input7.txt'))
    print(eval('a'))
    fillDict(getLines('input7-2.txt'))
    print(eval('a'))

