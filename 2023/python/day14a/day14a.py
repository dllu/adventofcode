#!/usr/bin/env python3

import sys

NORTH = 0

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def build_grid(contents):
    grid = []
    for line in contents.splitlines():
        grid.append(list(line))
    return grid

def move_rocks(grid, direction):
    num_rows = len(grid)
    num_cols = len(grid[0])
    move_count = 0
    if direction == NORTH:
        for row in range(1, num_rows):
            for col in range(num_cols):
                if grid[row][col] == 'O' and grid[row - 1][col] == '.':
                    grid[row][col] = '.'
                    grid[row - 1][col] = 'O'
                    move_count += 1
    return move_count > 0

def total_load(grid, direction):
    num_rows = len(grid)
    total_load = 0
    if direction == NORTH:
        for i, row in enumerate(grid):
            total_load += row.count('O') * (num_rows - i)
    return total_load

def process(contents):
    grid = build_grid(contents)
    while move_rocks(grid, NORTH):
        pass
    return total_load(grid, NORTH)

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
