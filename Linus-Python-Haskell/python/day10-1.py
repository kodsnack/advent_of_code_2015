#!/usr/bin/python3

def look_and_say(line):
    result = ''
    while len(line) != 0:
        c = line[0]
        for i in range(0, len(line)):
            if c != line[i]:
                result += ('%d%s' % (i, c))
                line = line[i:]
                break
            c = line[i]
        else:
            result += '%d%s' % (len(line), line[0])
            break
    return result


def solve(lines):
    for i in range(0, 40):
        print(i)
        lines = look_and_say(lines)
    return len(lines)


if __name__ == '__main__':
    lines = '1113122113'
    print(solve(lines))

