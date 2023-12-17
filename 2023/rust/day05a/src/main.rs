use std::env;
use std::fs;
use std::process;

struct Range {
    source: (u64, u64),
    destination: (u64, u64),
}

impl Range {
    fn new(src: u64, dest: u64, len: u64) -> Range {
        Range {
            source: (src, src + len - 1),
            destination: (dest, dest + len - 1),
        }
    }
}

struct ConversionMap {
    ranges: Vec<Range>,
}

impl ConversionMap {
    fn convert(&self, value: u64) -> u64 {
        let mut result = value;
        for range in self.ranges.iter() {
            if value >= range.source.0 && value <= range.source.1 {
                result = value - range.source.0 + range.destination.0;
                break;
            }
        }
        result
    }
}

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn build_map(expect: &str, lineptr: &mut std::str::Lines) -> ConversionMap
{
    let mut conversion_map: ConversionMap = ConversionMap { ranges: vec![] };
    match lineptr.next() {
        Some(line) => {
            if !line.starts_with(expect) {
                panic!("unexpected input");
            }
        }
        None => panic!("unexpected EOF"),
    }
    loop {
        match lineptr.next() {
            Some(line) => {
                if line.is_empty() {
                    break;
                } else {
                    let values: Vec<u64> = line
                        .split_whitespace()
                        .map(|s| s.parse::<u64>().unwrap())
                        .collect();
                    conversion_map
                        .ranges
                        .push(Range::new(values[1], values[0], values[2]));
                }
            }
            None => break,
        }
    }
    conversion_map
}

fn process(contents: &str) -> u64 {
    let mut lineptr = contents.lines();
    let seeds: Vec<u64> = match lineptr.next() {
        Some(line) => {
            if !line.starts_with("seeds:") {
                panic!("expected seeds");
            }
            line.split_once(':')
                .unwrap()
                .1
                .split_whitespace()
                .map(|s| s.parse::<u64>().unwrap())
                .collect()
        }
        None => panic!("unexpected EOF"),
    };
    lineptr.next().expect("unexpected EOF");

    let maps: Vec<ConversionMap> = vec![
        build_map("seed-to-soil", &mut lineptr),
        build_map("soil-to-fertilizer", &mut lineptr),
        build_map("fertilizer-to-water", &mut lineptr),
        build_map("water-to-light", &mut lineptr),
        build_map("light-to-temperature", &mut lineptr),
        build_map("temperature-to-humidity", &mut lineptr),
        build_map("humidity-to-location", &mut lineptr),
    ];
    seeds
        .iter()
        .map(|s| maps.iter().fold(*s, |acc, m| m.convert(acc)))
        .min()
        .unwrap()
}

fn main() {
    if env::args().count() < 2 {
        usage();
    }
    let filename = env::args().nth(1).unwrap();
    let contents = fs::read_to_string(filename).expect("read of input file failed");
    let result = process(&contents);
    println!("result = {result}");
}
