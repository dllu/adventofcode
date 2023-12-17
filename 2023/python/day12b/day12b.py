#!/usr/bin/env python3

import ast
import functools
import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

@functools.lru_cache
def recurse(springs, count_str):
    counts = ast.literal_eval(count_str)
    if not counts:
        if not '#' in springs:
            return 1
        return 0
    counter = 0
    upper_bound = len(springs) - sum(counts[1:]) + len(counts[1:]) - counts[0] + 1
    if upper_bound > 0:
        for position in range(upper_bound):
            broken = '.' * position
            working = '#' * counts[0]
            possible = broken + working + '.'
            for spring, possible_spring in zip(springs, possible):
                if spring != possible_spring and spring != '?':
                    break
            else:
                next_springs = springs[len(possible):]
                counter += recurse(next_springs, str(counts[1:]))
    return counter

def count_fits(line):
    spring_part, count_part = line.split()
    counts = list(map(int, count_part.split(',')))
    new_springs = '?'.join([spring_part] * 5)
    new_counts = counts * 5
    return recurse(new_springs, str(new_counts))

def process(contents):
    result = 0
    for line in contents.splitlines():
        result += count_fits(line)
    return result

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
