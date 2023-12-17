#!/usr/bin/env python3

from collections import deque
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
        if pos[0] < max_row and pipe in "S|7F":
            south = (pos[0] + 1, pos[1])
            south_pipe = grid[south]
            if south_pipe in "|LJ" and not south in pipe_loop:
                q.append(south)
        if pos[1] > 0 and pipe in "S-7J":
            west = (pos[0], pos[1] - 1)
            west_pipe = grid[west]
            if west_pipe in "-LF" and not west in pipe_loop:
                q.append(west)
        if pos[1] < max_col and pipe in "S-LF":
            east = (pos[0], pos[1] + 1)
            east_pipe = grid[east]
            if east_pipe in "-J7" and not east in pipe_loop:
                q.append(east)
    return pipe_loop

def process(contents):
    grid, start = build_grid(contents)
    pipe_loop = build_loop(grid, start)
    return len(pipe_loop) // 2

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
