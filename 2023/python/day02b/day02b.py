#!/usr/bin/env python3

import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def process(contents):
    s = 0
    for line in contents.splitlines():
        _, sets_part = line.split(':')
        max_red = None
        max_green = None
        max_blue = None
        for game_set in sets_part.split(';'):
            for cubes in game_set.split(','):
                number, color = cubes.split()
                if color == 'red' and ((max_red is None) or (int(number) > max_red)):
                    max_red = int(number)
                if color == 'green' and ((max_green is None) or (int(number) > max_green)):
                    max_green = int(number)
                if color == 'blue' and ((max_blue is None) or (int(number) > max_blue)):
                    max_blue = int(number)
        s += max_red * max_green * max_blue
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
