#!/usr/bin/env python3

import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def process(contents):
    total = 0
    lines = contents.splitlines()
    instances = {}
    for line in lines:
        card_part, rest = line.split(':')
        _, card_str = card_part.split()
        card = int(card_str)
        winning_str, hand_str = rest.split('|')
        winning = set(map(int, winning_str.split()))
        hand = set(map(int, hand_str.split()))
        count = len(winning & hand)
        for i in range(card + 1, card + count + 1):
            instances[i] = instances.get(i, 0) + 1 + instances.get(card, 0)
    return sum(list(instances.values())) + card

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
