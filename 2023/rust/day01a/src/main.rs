use std::env;
use std::fs;
use std::process;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn process(contents: &str) -> u32 {
    let mut sum: u32 = 0;
    let digits: Vec<&str> = vec!["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"];
    for line in contents.lines() {
        let mut min_index: Option<usize> = None;
        let mut max_index: Option<usize> = None;
        let mut left_digit: char = 'x';
        let mut right_digit: char = 'x';
        digits.iter().for_each(|digit| {
            if line.contains(digit) {
                if let Some(left_index) = line.find(digit) {
                    if min_index.is_none() || left_index < min_index.unwrap() {
                        min_index = Some(left_index);
                        left_digit = digit.chars().next().unwrap();
                    }
                }
                if let Some(right_index) = line.rfind(digit) {
                    if max_index.is_none() || right_index > max_index.unwrap() {
                        max_index = Some(right_index);
                        right_digit = digit.chars().next().unwrap();
                    }
                }
            }
        });
        sum += (((left_digit as u32) - ('0' as u32)) * 10) + ((right_digit as u32) - ('0' as u32));
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
