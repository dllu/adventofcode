#!/usr/bin/env python3

import heapq
import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def build_grid(contents):
    grid = []
    for line in contents.splitlines():
        grid.append([int(x) for x in line])
    return grid

def in_bounds(pos, extents):
    return pos[0] >= 0 and pos[0] < extents[0] and pos[1] >= 0 and pos[1] < extents[1]

def find_min_loss(grid):
    num_rows = len(grid)
    num_cols = len(grid[0])
    extents = (num_rows, num_cols)
    source = (0, 0)
    dest = (num_rows - 1, num_cols - 1)
    min_steps = 3
    max_steps = 10
    visited = set()
    q = [(grid[source[0]][source[1] + 1], (source[0], source[1] + 1), (source[0], source[1] + 1), 0),
         (grid[source[0] + 1][source[1]], (source[0] + 1, source[1]), (source[0] + 1, source[1]), 0)]
    while q:
        (loss, position, direction, steps) = heapq.heappop(q)
        if position == dest and min_steps <= steps:
            return loss
        if (position, direction, steps) in visited:
            continue
        visited.add((position, direction, steps))
        straight_pos = (position[0] + direction[0], position[1] + direction[1])
        if steps < (max_steps - 1) and in_bounds(straight_pos, extents):
            heapq.heappush(q, (loss + grid[straight_pos[0]][straight_pos[1]], straight_pos, direction, steps + 1))
        if min_steps <= steps:
            left_dir = (direction[1], -direction[0])
            left_pos = (position[0] + left_dir[0], position[1] + left_dir[1])
            if in_bounds(left_pos, extents):
                heapq.heappush(q, (loss + grid[left_pos[0]][left_pos[1]], left_pos, left_dir, 0))
            right_dir = (-direction[1], direction[0])
            right_pos = (position[0] + right_dir[0], position[1] + right_dir[1])
            if in_bounds(right_pos, extents):
                heapq.heappush(q, (loss + grid[right_pos[0]][right_pos[1]], right_pos, right_dir, 0))

def process(contents):
    grid = build_grid(contents)
    return find_min_loss(grid)

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
