use std::collections::HashSet;
use std::env;
use std::fs;
use std::process;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn process(contents: &str) -> u32 {
    let mut total: u32 = 0;
    for line in contents.lines() {
        if let Some((_, rest)) = line.split_once(':') {
            if let Some((winning_str, hand_str)) = rest.split_once('|') {
                let winning = winning_str
                    .split_whitespace()
                    .map(|n| n.parse::<u32>().unwrap())
                    .collect::<HashSet<u32>>();
                let hand = hand_str
                    .split_whitespace()
                    .map(|n| n.parse::<u32>().unwrap())
                    .collect::<HashSet<u32>>();
                let count = winning.intersection(&hand).count() as u32;
                total += if count > 0 { u32::pow(2, count - 1) } else { 0 };
            }
        }
    }
    total
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
