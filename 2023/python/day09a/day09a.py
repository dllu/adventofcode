#!/usr/bin/env python3

import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def extrapolate(current):
    if all(map(lambda x: x == 0, current)):
        return 0
    else:
        successor = []
        for i in range(len(current) - 1):
            diff = current[i + 1] - current[i]
            successor.append(diff)
        return current[-1] + extrapolate(successor)

def process(contents):
    result = 0
    for line in contents.splitlines():
        result += extrapolate(list(map(int, line.split())))
    return result

def main():
    if len(sys.argv) < 2:
        usage()
    filename = sys.argv[1]
    try:
        infile = open(filename)
        contents = infile.read()
        result = process(contents)
        print(f'result = {result}')
    except IOError:
        print(f'read of input file "{filename}" failed.')
        sys.exit(1)

if __name__ == '__main__':
    main()
