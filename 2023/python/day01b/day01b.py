#!/usr/bin/env python3

import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def process(contents):
    lines = contents.split()
    nums = []
    digits_map = { '0': '0', '1': '1', '2': '2', '3': '3', '4': '4', '5': '5',
                   '6': '6', '7': '7', '8': '8', '9': '9', 'zero': '0',
                   'one': '1', 'two': '2', 'three': '3', 'four': '4',
                   'five': '5', 'six': '6', 'seven': '7', 'eight': '8',
                   'nine': '9' }
    for line in lines:
        left_indices = {}
        right_indices = {}
        for digit in digits_map.keys():
            if digit in line:
                left_indices[digit] = line.find(digit)
                right_indices[digit] = line.rfind(digit)
        left_digit = min(left_indices, key=left_indices.get)
        right_digit = max(right_indices, key=right_indices.get)
        nums.append(int(digits_map[left_digit] + digits_map[right_digit]))
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
