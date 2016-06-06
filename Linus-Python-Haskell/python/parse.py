
def get_lines(filename):
    lines = []
    try:
        with open(filename, 'r') as f:
            while True:
                line = f.readline().rstrip('\n')
                if line == '':
                    break
                lines.append(line)
    except EOFError:
        pass
    return lines
