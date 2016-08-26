#!/usr/bin/python3

def turnOn(a):
  return 1

def turnOff(a):
  return 0;

def toogle(a):
  return 1-a

operation = {
    'turn on': turnOn,
    'turn off': turnOff,
    'toggle': toogle}

def parseLine(line):
  parts = line.split(' ')
  return (operation[' '.join(parts[:-3])], tuple(map(int,parts[-3].split(','))), tuple(map(int, parts[-1].split(','))))

def process(t, lines):
    res = 0
    (x, y) = t
    if y == 0:
        print(t)
    for line in lines:
        (op, (xMin, yMin), (xMax, yMax)) = line
        if x >= xMin and x <= xMax and y >= yMin and y <= yMax:
           res = op(res)
    return res

lamps = [(j,i) for j in range(0, 1000) for i in range(0, 1000)]


lines = []
try:
    with open('input6.txt', 'r') as f:
        while True:
            line = f.readline()
            if line == '':
                break
            lines.append(line)
except EOFError:
    pass

parsedLines = list(map(parseLine, lines))

def wrap(x):
    return process(x, parsedLines)

if __name__ == '__main__':
    print(sum(map(wrap, lamps)))
#print(parseLine('turn on 0,0 through 999,999'))
#print(parseLine('toggle 0,0 through 999,999'))

