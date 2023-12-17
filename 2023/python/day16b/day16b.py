#!/usr/bin/env python3

from collections import deque
import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

UP = 0
DOWN = 1
LEFT = 2
RIGHT = 3

def build_grid(contents):
    grid = []
    for line in contents.splitlines():
        grid.append(list(line))
    return grid

def in_bounds(pos, extents):
    return pos[0] >= 0 and pos[0] < extents[0] and pos[1] >= 0 and pos[1] < extents[1]

def calc_energized(grid, start_pos, start_dir):
    num_rows = len(grid)
    num_cols = len(grid[0])
    extents = (num_rows, num_cols)
    queue = deque()
    cache = {}
    queue.append((start_pos, start_dir))
    while queue:
        pos, dir = queue.popleft()
        if pos not in cache.keys():
            cache[pos] = { dir }
        else:
            cache[pos].add(dir)
        object = grid[pos[0]][pos[1]]
        if object == '.':
            if dir == UP:
                new_pos = (pos[0] - 1, pos[1])
            elif dir == DOWN:
                new_pos = (pos[0] + 1, pos[1])
            elif dir == LEFT:
                new_pos = (pos[0], pos[1] - 1)
            elif dir == RIGHT:
                new_pos = (pos[0], pos[1] + 1)
            if in_bounds(new_pos, extents) and ((not new_pos in cache) or (not dir in cache[new_pos])):
                queue.append((new_pos, dir))
        elif object == '\\':
            if dir == UP:
                new_pos = (pos[0], pos[1] - 1)
                new_dir = LEFT
            elif dir == DOWN:
                new_pos = (pos[0], pos[1] + 1)
                new_dir = RIGHT
            elif dir == LEFT:
                new_pos = (pos[0] - 1, pos[1])
                new_dir = UP
            elif dir == RIGHT:
                new_pos = (pos[0] + 1, pos[1])
                new_dir = DOWN
            if in_bounds(new_pos, extents) and ((not new_pos in cache) or (not new_dir in cache[new_pos])):
                queue.append((new_pos, new_dir))
        elif object == '/':
            if dir == UP:
                new_pos = (pos[0], pos[1] + 1)
                new_dir = RIGHT
            elif dir == DOWN:
                new_pos = (pos[0], pos[1] - 1)
                new_dir = LEFT
            elif dir == LEFT:
                new_pos = (pos[0] + 1, pos[1])
                new_dir = DOWN
            elif dir == RIGHT:
                new_pos = (pos[0] - 1, pos[1])
                new_dir = UP
            if in_bounds(new_pos, extents) and ((not new_pos in cache) or (not new_dir in cache[new_pos])):
                queue.append((new_pos, new_dir))
        elif object == '-':
            if dir == LEFT or dir == RIGHT:
                new_pos = (pos[0], pos[1] - 1) if dir == LEFT else (pos[0], pos[1] + 1)
                if in_bounds(new_pos, extents) and ((not new_pos in cache) or (not dir in cache[new_pos])):
                    queue.append((new_pos, dir))
            else:
                new_pos1 = (pos[0], pos[1] - 1)
                if in_bounds(new_pos1, extents) and ((not new_pos1 in cache) or (not LEFT in cache[new_pos1])):
                    queue.append((new_pos1, LEFT))
                new_pos2 = (pos[0], pos[1] + 1)
                if in_bounds(new_pos2, extents) and ((not new_pos2 in cache) or (not RIGHT in cache[new_pos2])):
                    queue.append((new_pos2, RIGHT))
        elif object == '|':
            if dir == UP or dir == DOWN:
                new_pos = (pos[0] - 1, pos[1]) if dir == UP else (pos[0] + 1, pos[1])
                if in_bounds(new_pos, extents) and ((not new_pos in cache) or (not dir in cache[new_pos])):
                    queue.append((new_pos, dir))
            else:
                new_pos1 = (pos[0] - 1, pos[1])
                if in_bounds(new_pos1, extents) and ((not new_pos1 in cache) or (not UP in cache[new_pos1])):
                    queue.append((new_pos1, UP))
                new_pos2 = (pos[0] + 1, pos[1])
                if in_bounds(new_pos2, extents) and ((not new_pos2 in cache) or (not DOWN in cache[new_pos2])):
                    queue.append((new_pos2, DOWN))
        else:
            raise ValueError
    return len(cache.keys())

def process(contents):
    grid = build_grid(contents)
    num_rows = len(grid)
    num_cols = len(grid[0])
    max_energy = 0
    for i in range(num_cols):
        max_energy = max(max_energy, calc_energized(grid, (0, i), DOWN))
    for i in range(num_rows):
        max_energy = max(max_energy, calc_energized(grid, (i, 0), RIGHT))
    for i in range(num_rows):
        max_energy = max(max_energy, calc_energized(grid, (i, num_cols - 1), LEFT))
    for i in range(num_cols):
        max_energy = max(max_energy, calc_energized(grid, (num_rows - 1, i), UP))
    return max_energy

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
