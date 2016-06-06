#!/usr/bin/python3

def look_and_say(line):
    result = ''
    pos = 0
    while pos < len(line):
        c = line[pos]
        for i in range(pos, len(line)):
            if c != line[i]:
                result += ('%d%s' % (i-pos, c))
                pos = i
                break
            c = line[i]
        else:
            result += '%d%s' % (len(line)-pos, line[pos])
            break
    return result


def solve(lines):
    for i in range(0, 50):
        print(i)
        lines = look_and_say(lines)
    return len(lines)


if __name__ == '__main__':
    lines = '1113122113'
    print(solve(lines))

