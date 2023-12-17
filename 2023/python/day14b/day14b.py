#!/usr/bin/env python3

import copy
import sys

NORTH = 0
EAST = 1
SOUTH = 2
WEST = 3

CYCLES = 1000000000

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
    elif direction == SOUTH:
        for row in range(0, num_rows - 1):
            for col in range(0, num_cols):
                if grid[row][col] == 'O' and grid[row + 1][col] == '.':
                    grid[row][col] = '.'
                    grid[row + 1][col] = 'O'
                    move_count += 1
    elif direction == WEST:
        for row in range(num_rows):
            for col in range(1, num_cols):
                if grid[row][col] == 'O' and grid[row][col - 1] == '.':
                    grid[row][col] = '.'
                    grid[row][col - 1] = 'O'
                    move_count += 1
    elif direction == EAST:
        for row in range(num_rows):
            for col in range(0, num_cols - 1):
                if grid[row][col] == 'O' and grid[row][col + 1] == '.':
                    grid[row][col] = '.'
                    grid[row][col + 1] = 'O'
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
    grid_states = []
    for i in range(CYCLES):
        grid_states.append(copy.deepcopy(grid))
        while move_rocks(grid, NORTH):
            pass
        while move_rocks(grid, WEST):
            pass
        while move_rocks(grid, SOUTH):
            pass
        while move_rocks(grid, EAST):
            pass
        if grid in grid_states:
            cycle_start = grid_states.index(grid)
            cycle_len = i - cycle_start + 1
            print(f'cycle detected at iteration {i}, start = {cycle_start}, length = {cycle_len}')
            break
    final_index = (CYCLES - cycle_start) % cycle_len + cycle_start
    final_grid = grid_states[final_index]
    return total_load(final_grid, NORTH)

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
