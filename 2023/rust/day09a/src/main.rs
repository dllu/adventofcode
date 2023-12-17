use std::env;
use std::fs;
use std::process;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn extrapolate(current: &Vec<i64>) -> i64 {
    if current.iter().all(|x| *x == 0) {
        0
    } else {
        let mut next: Vec<i64> = vec![];
        for i in 0..current.len() - 1 {
            next.push(current[i + 1] - current[i]);
        }
        return current.iter().last().unwrap() + extrapolate(&next);
    }
}

fn process(contents: &str) -> i64 {
    let mut sum: i64 = 0;
    for line in contents.lines() {
        let input_seq: Vec<i64> = line
            .split_whitespace()
            .map(|n| n.parse::<i64>().unwrap())
            .collect();
        sum += extrapolate(&input_seq);
    }
    sum
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
