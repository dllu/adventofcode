#!/usr/bin/env python3

from collections import deque
import sys

class Interval:
    def __init__(self, first, last):
        self.first = first
        self.last = last

class Mapping:
    def __init__(self, source, destination, length):
        self.source = source
        self.destination = destination
        self.length = length

class ConversionMap:
    def __init__(self):
        self.mappings = []
        
    def add_mapping(self, mapping):
        self.mappings.append(mapping)
    
    def convert(self, value):
        result = value
        for seed_range in self.ranges:
            if value >= seed_range.source[0] and value <= seed_range.source[1]:
                result = value - seed_range.source[0] + seed_range.destination[0]
                break
        return result

def build_conversions(lines):
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
                conversion_map.add_mapping(Mapping(values[1], values[0], values[2]))
                lineptr += 1
        conversion_maps.append(conversion_map)
    return conversion_maps

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def process(contents):
    lowest = 2**64
    lines = contents.splitlines()
    lineptr = 0
    seed_line = lines[lineptr]
    if not seed_line.startswith('seeds:'):
        print('unexpected input: expected line starting with "seeds:"')
        sys.exit(1)
    seeds = list(map(int, seed_line.split(':')[1].split()))
    lineptr += 2
    conversions = build_conversions(lines)
    for i in range(0, len(seeds), 2):
        first = seeds[i]
        length = seeds[i + 1]
        ranges = deque([Interval(first, first + length - 1)])
        for conversion in conversions:
            new_ranges = []
            while ranges:
                seed_range = ranges.popleft()
                found = False
                for mapping in conversion.mappings:
                    if seed_range.first >= mapping.source and \
                        seed_range.last < mapping.source + mapping.length:
                            new_ranges.append(Interval(seed_range.first - mapping.source + mapping.destination,
                                                       seed_range.last - mapping.source + mapping.destination))
                            found = True
                    elif seed_range.first < mapping.source and seed_range.last >= mapping.source and \
                        seed_range.last < mapping.source + mapping.length:
                            ranges.append(Interval(seed_range.first, mapping.source - 1))
                            new_ranges.append(Interval(mapping.destination,
                                                       mapping.destination + seed_range.last - mapping.source))
                            found = True
                    elif seed_range.first < mapping.source + mapping.length and \
                        seed_range.last >= mapping.source + mapping.length and \
                        seed_range.first >= mapping.source:
                            ranges.append(Interval(mapping.source + mapping.length, seed_range.last))
                            new_ranges.append(Interval(mapping.destination + seed_range.first - mapping.source,
                                                       mapping.destination + mapping.length - 1))
                            found = True
                    elif seed_range.first < mapping.source and seed_range.last >= mapping.source + mapping.length:
                        ranges.append(Interval(seed_range.first, mapping.source - 1))
                        new_ranges.append(Interval(mapping.destination,
                                                   mapping.destination + mapping.length - 1))
                        ranges.append(Interval(mapping.source + mapping.length, seed_range.last))
                        found = True
                    if found:
                        break
                if not found:
                    new_ranges.append(seed_range)
            ranges = deque(new_ranges)
        lowest = min(lowest, min(map(lambda r: r.first, ranges)))
    return lowest

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
