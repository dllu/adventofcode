#!/usr/bin/env python3

import math
import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def calc_filled(vertices):
    area = 0
    perimeter = 0
    for i in range(len(vertices) - 1):
        first = vertices[i]
        second = vertices[i + 1]
        x1, y1 = first
        x2, y2 = second
        area += (x1 * y2) - (x2 * y1)
        perimeter += int(math.sqrt(((x2 - x1) * (x2 -x1)) + (y2 - y1) * (y2 - y1)))
    return (abs(area) // 2) - (perimeter // 2) + 1 + perimeter

def process(contents):
    start = (0, 0)
    vertices = []
    vertices.append((start[0], start[1]))
    pos = start
    for line in contents.splitlines():
        dir_part, steps_part, _ = line.split()
        steps = int(steps_part)
        if dir_part == 'U':
            dir = (0, 1)
        elif dir_part == 'D':
            dir = (0, -1)
        elif dir_part == 'L':
            dir = (-1, 0)
        elif dir_part == 'R':
            dir = (1, 0)
        pos = (pos[0] + (steps * dir[0]), pos[1] + (steps * dir[1]))
        vertices.append((pos[0], pos[1]))
    return calc_filled(vertices)

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
