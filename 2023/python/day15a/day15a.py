#!/usr/bin/env python3

import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def hash_string(s):
    result = 0
    for ch in s:
        result += ord(ch)
        result *= 17
        result %= 256
    return result

def process(contents):
    result = 0
    trimmed = contents.rstrip()
    for s in trimmed.split(','):
        result += hash_string(s)
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
