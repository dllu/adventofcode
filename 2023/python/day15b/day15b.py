#!/usr/bin/env python3

import sys

NUM_BOXES = 256

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def hash_string(s):
    result = 0
    for ch in s:
        result += ord(ch)
        result *= 17
        result %= NUM_BOXES
    return result

def process(contents):
    result = 0
    boxes = []
    for _ in range(NUM_BOXES):
        boxes.append([])
    trimmed = contents.rstrip()
    for s in trimmed.split(','):
        if '=' in s:
            label, focal_str = s.split('=')
            focal = int(focal_str)
            hash = hash_string(label)
            for i, item in enumerate(boxes[hash]):
                if item[0] == label:
                    boxes[hash][i] = (label, focal)
                    break
            else:
                boxes[hash].append((label, focal))
        elif '-' in s:
            label = s[:-1]
            hash = hash_string(label)
            for item in boxes[hash]:
                if item[0] == label:
                    boxes[hash].remove(item)
                    break
    for box_num in range(NUM_BOXES):
        for slot_num in range(len(boxes[box_num])):
            result += (box_num + 1) * (slot_num + 1) * boxes[box_num][slot_num][1]
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
