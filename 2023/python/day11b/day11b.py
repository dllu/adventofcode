#!/usr/bin/env python3

import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def process(contents):
    total_dist = 0
    galaxies = []
    lines = contents.splitlines()
    num_rows = len(lines)
    num_cols = len(lines[0])
    for row, line in enumerate(lines):
        for col, ch in enumerate(line):
            if ch == '#':
                galaxies.append((row, col))
    blank_rows = []
    blank_cols = []
    for i in range(num_rows):
        if not any(map(lambda p: p[0] == i, galaxies)):
            blank_rows.append(i)
    for i in range(num_cols):
        if not any(map(lambda p: p[1] == i, galaxies)):
            blank_cols.append(i)
    for i in range(len(galaxies)):
        num_blank_rows = 0
        for blank_row in blank_rows:
            if galaxies[i][0] > blank_row:
                num_blank_rows += 999999
        num_blank_cols = 0
        for blank_col in blank_cols:
            if galaxies[i][1] > blank_col:
                num_blank_cols += 999999
        galaxies[i] = (galaxies[i][0] + num_blank_rows, galaxies[i][1] + num_blank_cols)
    for i in range(len(galaxies)):
        for j in range(i, len(galaxies)):
            total_dist += abs(galaxies[i][0] - galaxies[j][0]) + abs(galaxies[i][1] - galaxies[j][1])
    return total_dist

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
