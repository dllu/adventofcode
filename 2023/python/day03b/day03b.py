#!/usr/bin/env python3

import re
import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def build_maps(contents):
    numbers = []
    symbols = []
    row = 0
    for line in contents.splitlines():
        matches = re.finditer(r'\d+', line)
        for match in matches:
            numbers.append((match.group(0), (row, match.start())))
        col = 0
        for ch in line:
            if ch == '*':
                symbols.append((ch, (row, col)))
            col += 1
        row += 1
    return (numbers, symbols)

def get_adjacents(symbol, numbers):
    adjacents = set()
    (row, col) = symbol[1]
    neighbors = [(row - 1, col - 1), (row - 1, col), (row - 1, col + 1),
                 (row, col - 1), (row, col + 1),
                 (row + 1, col - 1), (row + 1, col), (row + 1, col + 1)]
    for number in numbers:
        for neighbor in neighbors:
            if neighbor[0] == number[1][0] and neighbor[1] >= number[1][1] \
                and neighbor[1] <= (number[1][1] + len(number[0]) - 1):
                    adjacents.add(int(number[0]))
    return adjacents

def process(contents):
    result = 0
    (numbers, symbols) = build_maps(contents)
    histo = {}
    for symbol in symbols:
        histo[symbol] = list(get_adjacents(symbol, numbers))
    ratios = []
    for symbol in histo.keys():
        if len(histo[symbol]) == 2:
            ratios.append(histo[symbol][0] * histo[symbol][1])
    return sum(ratios)

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
