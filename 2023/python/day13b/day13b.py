#!/usr/bin/env python3

import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def find_reflection(pattern):
    num_rows = len(pattern)
    num_cols = len(pattern[0])
    for split_col in range(1, num_cols):
        left_range = list(reversed(range(0, split_col)))
        right_range = list(range(split_col, num_cols))
        diffs = 0
        for pair in zip(left_range, right_range):
            left_col = ''.join(row[pair[0]] for row in pattern)
            right_col = ''.join(row[pair[1]] for row in pattern)
            diffs += sum(c1 != c2 for c1, c2 in zip(left_col, right_col))
        if diffs == 1:
            return split_col
    for split_row in range(1, num_rows):
        top_range = list(reversed(range(0, split_row)))
        bottom_range = list(range(split_row, num_rows))
        diffs = 0
        for pair in zip(top_range, bottom_range):
            top_row = pattern[pair[0]]
            bottom_row = pattern[pair[1]]
            diffs += sum(c1 != c2 for c1, c2 in zip(top_row, bottom_row))
        if diffs == 1:
            return 100 * split_row
    return 0

def split_patterns(contents):
    patterns = []
    current_pattern = []
    for line in contents.splitlines():
        if not line:
            patterns.append(current_pattern)
            current_pattern = []
        else:
            current_pattern.append(line)
    patterns.append(current_pattern)
    return patterns

def process(contents):
    result = 0
    patterns = split_patterns(contents)
    for pattern in patterns:
        result += find_reflection(pattern)
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
