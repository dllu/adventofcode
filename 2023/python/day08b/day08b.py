#!/usr/bin/env python3

import functools
import math
import re
import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def process(contents):
    lines = contents.splitlines()
    nodes_map = {}
    turns = lines[0]
    node_re = re.compile(r'([A-Z][A-Z][A-Z]) = \(([A-Z][A-Z][A-Z]), ([A-Z][A-Z][A-Z])\)')
    for line in lines[2:]:
        matches = node_re.match(line)
        source = matches[1]
        ldest = matches[2]
        rdest = matches[3]
        nodes_map[source] = (ldest, rdest)
    start_nodes = list(filter(lambda x: x.endswith('A'), nodes_map.keys()))
    all_steps = []
    for start_node in start_nodes:
        steps = 0
        turn_idx = 0
        curr_node = start_node
        while not curr_node.endswith('Z'):
            turn = turns[turn_idx]
            if turn == 'L':
                curr_node = nodes_map[curr_node][0]
            elif turn == 'R':
                curr_node = nodes_map[curr_node][1]
            steps += 1
            turn_idx = (turn_idx + 1) % len(turns)
        all_steps.append(steps)
    return functools.reduce(lambda x, y: math.lcm(x, y), all_steps)

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
