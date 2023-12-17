use std::env;
use std::fs;
use std::process;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn hash_string(s: &str) -> u32 {
    let mut result: u32 = 0;
    s.chars().for_each(|ch| {
        result += ch as u32;
        result *= 17;
        result %= 256;
    });
    result
}

fn process(contents: &str) -> u32 {
    let mut result: u32 = 0;
    let trimmed = contents.trim_end_matches('\n');
    trimmed.split(',').for_each(|s| {
        result += hash_string(s);
    });
    result
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
