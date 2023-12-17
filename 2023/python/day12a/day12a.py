#!/usr/bin/env python3

from collections import deque
import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def build_possibles(springs):
    possibles = []
    q = deque([springs])
    while q:
        curr_springs = q.popleft()
        if not any(map(lambda c: c == '?', curr_springs)):
            possibles.append(curr_springs)
        else:
            i = curr_springs.find('?')
            temp1 = curr_springs[0:i] + '#' + curr_springs[(i + 1):]
            temp2 = curr_springs[0:i] + '.' + curr_springs[(i + 1):]
            q.append(temp1)
            q.append(temp2)
    return possibles

def does_fit(possible, counts):
    working_groups = [x for x in possible.split('.') if '#' in x]
    if len(working_groups) != len(counts):
        return False
    for i in range(len(working_groups)):
        if len(working_groups[i]) != counts[i]:
            return False
    return True

def count_fits(line):
    fits = 0
    spring_part, count_part = line.split()
    counts = list(map(int, count_part.split(',')))
    possibles = build_possibles(spring_part)
    for possible in possibles:
        if does_fit(possible, counts):
            fits += 1
    return fits

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
