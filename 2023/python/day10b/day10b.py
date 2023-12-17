#!/usr/bin/env python3

from collections import deque
import copy
import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def build_grid(contents):
    grid = {}
    start = (0, 0)
    for row, line in enumerate(contents.splitlines()):
        for col, ch in enumerate(line):
            grid[(row, col)] = ch
            if ch == 'S':
                start = (row, col)
    return grid, start

def build_loop(grid, start):
    pipe_loop = set()
    q = deque()
    max_row = max([x[0] for x in grid.keys()])
    max_col = max([x[1] for x in grid.keys()])
    possibles = set('|-LJ7F')
    q.append(start)
    while q:
        pos = q.popleft()
        pipe_loop.add(pos)
        pipe = grid[pos]
        if pos[0] > 0 and pipe in "S|LJ":
            north = (pos[0] - 1, pos[1])
            north_pipe = grid[north]
            if north_pipe in "|7F" and not north in pipe_loop:
                q.append(north)
                if pipe == 'S':
                    possibles = possibles & set('|LJ')
        if pos[0] < max_row and pipe in "S|7F":
            south = (pos[0] + 1, pos[1])
            south_pipe = grid[south]
            if south_pipe in "|LJ" and not south in pipe_loop:
                q.append(south)
                if pipe == 'S':
                    possibles = possibles & set('|7F')
        if pos[1] > 0 and pipe in "S-7J":
            west = (pos[0], pos[1] - 1)
            west_pipe = grid[west]
            if west_pipe in "-LF" and not west in pipe_loop:
                q.append(west)
                if pipe == 'S':
                    possibles = possibles & set('-7J')
        if pos[1] < max_col and pipe in "S-LF":
            east = (pos[0], pos[1] + 1)
            east_pipe = grid[east]
            if east_pipe in "-J7" and not east in pipe_loop:
                q.append(east)
                if pipe == 'S':
                    possibles = possibles & set('-LF')
    start_pipe = list(possibles)[0]
    new_grid = copy.deepcopy(grid)
    new_grid[start] = start_pipe
    return pipe_loop, new_grid

def count_interior(pipe_loop, grid):
    count = 0
    new_grid = {}
    max_row = max([x[0] for x in grid.keys()])
    max_col = max([x[1] for x in grid.keys()])
    for pos in grid.keys():
        if not pos in pipe_loop:
            new_grid[pos] = '.'
        else:
            new_grid[pos] = grid[pos]
    for row in range(max_row + 1):
        for col in range(max_col + 1):
            pos = (row, col)
            pipe = new_grid[pos]
            if pipe != '.':
                continue
            intersections = 0
            corner_pipes = deque()
            for i in range((col + 1), (max_col + 1)):
                scan_pos = (row, i)
                scan_pipe = new_grid[scan_pos]
                if scan_pipe == '|':
                    intersections += 1
                elif scan_pipe in "FL":
                    corner_pipes.append(scan_pipe)
                elif corner_pipes and ((scan_pipe == 'J' and corner_pipes[-1] == 'F') or \
                                       (scan_pipe == '7' and corner_pipes[-1] == 'L')):
                    corner_pipes.pop()
                    intersections += 1
            if intersections % 2 == 1:
                count += 1
    return count

def process(contents):
    grid, start = build_grid(contents)
    pipe_loop, new_grid = build_loop(grid, start)
    return count_interior(pipe_loop, new_grid)

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
