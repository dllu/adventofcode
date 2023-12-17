#!/usr/bin/env python3

import sys

TOTAL_RED = 12
TOTAL_GREEN = 13
TOTAL_BLUE = 14

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def process(contents):
    s = 0
    for line in contents.splitlines():
        game_part, sets_part = line.split(':')
        game_id = game_part.split()[1]
        valid = True
        for game_set in sets_part.split(';'):
            for cubes in game_set.split(','):
                number, color = cubes.split()
                if color == 'red' and int(number) > TOTAL_RED:
                    valid = False
                if color == 'green' and int(number) > TOTAL_GREEN:
                    valid = False
                if color == 'blue' and int(number) > TOTAL_BLUE:
                    valid = False
        if valid:
            s += int(game_id)
    return s

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
