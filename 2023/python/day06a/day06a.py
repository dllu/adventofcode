#!/usr/bin/env python3

import functools
import math
import operator
import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def calc_wins(time, distance):
    ftime = float(time)
    fdist = float(distance)
    disc = ftime * ftime - 4.0 * fdist
    root1 = (ftime - math.sqrt(disc)) / 2.0
    root2 = (ftime + math.sqrt(disc)) / 2.0
    if math.floor(root2) == root2 and math.ceil(root1) == root1:
        return int(math.floor(root2) - math.ceil(root1)) - 1
    else:
        return int(math.floor(root2) - math.ceil(root1)) + 1

def process(contents):
    lines = contents.splitlines()
    times = list(map(int, lines[0].split(':')[1].split()))
    distances = list(map(int, lines[1].split(':')[1].split()))
    wins = []
    for race in range(len(times)):
        wins.append(calc_wins(times[race], distances[race]))
    return functools.reduce(operator.mul, wins)

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
