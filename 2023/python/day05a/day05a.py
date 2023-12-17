#!/usr/bin/env python3

import sys

class SeedRange:
    def __init__(self, src, dest, length):
        self.source = (src, src + length - 1)
        self.destination = (dest, dest + length - 1)

class ConversionMap:
    def __init__(self):
        self.ranges = []
        
    def add_range(self, seed_range):
        self.ranges.append(seed_range)
    
    def convert(self, value):
        result = value
        for seed_range in self.ranges:
            if value >= seed_range.source[0] and value <= seed_range.source[1]:
                result = value - seed_range.source[0] + seed_range.destination[0]
                break
        return result

def build_maps(lines):
    lineptr = 2
    sections = [
        'seed-to-soil',
        'soil-to-fertilizer',
        'fertilizer-to-water',
        'water-to-light',
        'light-to-temperature',
        'temperature-to-humidity',
        'humidity-to-location',
    ]
    conversion_maps = []
    for expect in sections:
        conversion_map = ConversionMap()
        if not lines[lineptr].startswith(expect):
            print(f'unexpected input: expected line starting with "{expect}"')
            sys.exit(1)
        lineptr += 1
        while True:
            if lineptr == len(lines):
                break
            elif not lines[lineptr]:
                lineptr += 1
                break
            else:
                values = list(map(int, lines[lineptr].split()))
                conversion_map.add_range(SeedRange(values[1], values[0], values[2]))
                lineptr += 1
        conversion_maps.append(conversion_map)
    return conversion_maps

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def process(contents):
    lines = contents.splitlines()
    lineptr = 0
    seed_line = lines[lineptr]
    if not seed_line.startswith('seeds:'):
        print('unexpected input: expected line starting with "seeds:"')
        sys.exit(1)
    seeds = list(map(int, seed_line.split(':')[1].split()))
    lineptr += 2
    maps = build_maps(lines)
    results = []
    for seed in seeds:
        for conversion in maps:
            seed = conversion.convert(seed)
        results.append(seed)
    return min(results)

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
