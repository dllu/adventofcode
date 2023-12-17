#!/usr/bin/env python3

import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def process(contents):
    total = 0
    lines = contents.splitlines()
    for line in lines:
        _, rest = line.split(':')
        winning_str, hand_str = rest.split('|')
        winning = set(map(int, winning_str.split()))
        hand = set(map(int, hand_str.split()))
        count = len(winning & hand)
        total += 2 ** (count - 1) if count > 0 else 0
    return total

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
