#!/usr/bin/env python3

import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def process(contents):
    lines = contents.split()
    nums = []
    digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    for line in lines:
        left_indices = {}
        right_indices = {}
        for digit in digits:
            if digit in line:
                left_indices[digit] = line.find(digit)
                right_indices[digit] = line.rfind(digit)
        left_digit = min(left_indices, key=left_indices.get)
        right_digit = max(right_indices, key=right_indices.get)
        nums.append(int(left_digit + right_digit))
    return sum(nums)

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
