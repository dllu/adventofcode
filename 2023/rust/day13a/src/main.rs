use std::env;
use std::fs;
use std::process;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn find_reflection(pattern: &Vec<Vec<char>>) -> u32 {
    let num_rows = pattern.len();
    let num_cols = pattern[0].len();
    for split_col in 1..num_cols {
        let left_range = (0..split_col).rev();
        let right_range = split_col..num_cols;
        if left_range.zip(right_range).all(|column_pair| {
            let left = pattern
                .iter()
                .map(|row| row[column_pair.0])
                .collect::<Vec<char>>();
            let right = pattern
                .iter()
                .map(|row| row[column_pair.1])
                .collect::<Vec<char>>();
            left == right
        }) {
            return split_col as u32;
        }
    }
    for split_row in 1..num_rows {
        let top_range = (0..split_row).rev();
        let bottom_range = split_row..num_rows;
        if top_range.zip(bottom_range).all(|row_pair| {
            let top = &pattern[row_pair.0];
            let bottom = &pattern[row_pair.1];
            top == bottom
        }) {
            return (split_row as u32) * 100;
        }
    }
    0
}

fn split_patterns(contents: &str) -> Vec<Vec<Vec<char>>> {
    let mut patterns: Vec<Vec<Vec<char>>> = vec![];
    let mut current_pattern: Vec<Vec<char>> = vec![];
    for line in contents.lines() {
        if line.is_empty() {
            patterns.push(current_pattern);
            current_pattern = vec![];
        } else {
            let row: Vec<char> = line.chars().collect();
            current_pattern.push(row);
        }
    }
    patterns.push(current_pattern);
    patterns
}

fn process(contents: &str) -> u32 {
    let mut sum: u32 = 0;
    let patterns = split_patterns(contents);
    for pattern in patterns {
        sum += find_reflection(&pattern);
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
