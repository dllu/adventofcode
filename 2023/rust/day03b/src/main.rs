use regex::Regex;
use std::env;
use std::fs;
use std::process;

struct Position(i32, i32);

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn build_numbers(contents: &str) -> Vec<(&str, Position)> {
    let mut numbers: Vec<(&str, Position)> = vec![];
    let num_re = Regex::new(r"\d+").unwrap();
    for (row, line) in contents.lines().enumerate() {
        for num_match in num_re.find_iter(line) {
            numbers.push((
                num_match.as_str(),
                Position(row as i32, num_match.start() as i32),
            ));
        }
    }
    numbers
}

fn build_gears(contents: &str) -> Vec<(char, Position)> {
    let mut gears: Vec<(char, Position)> = vec![];
    for (row, line) in contents.lines().enumerate() {
        for (col, ch) in line.chars().enumerate() {
            if ch == '*' {
                gears.push((ch, Position(row as i32, col as i32)));
            }
        }
    }
    gears
}

fn build_gear_ratios(
    num_rows: u32,
    num_cols: u32,
    numbers: &[(&str, Position)],
    gears: &[(char, Position)],
) -> Vec<u32> {
    let mut gear_ratios: Vec<u32> = vec![];
    for (_, gear_pos) in gears.iter() {
        let neighbors = [
            Position(gear_pos.0 - 1, gear_pos.1 - 1),
            Position(gear_pos.0 - 1, gear_pos.1),
            Position(gear_pos.0 - 1, gear_pos.1 + 1),
            Position(gear_pos.0, gear_pos.1 - 1),
            Position(gear_pos.0, gear_pos.1 + 1),
            Position(gear_pos.0 + 1, gear_pos.1 - 1),
            Position(gear_pos.0 + 1, gear_pos.1),
            Position(gear_pos.0 + 1, gear_pos.1 + 1),
        ]
        .into_iter()
        .filter(|p| p.0 >= 0 && p.1 >= 0 && p.0 < num_rows as i32 && p.1 < num_cols as i32)
        .collect::<Vec<Position>>();
        let mut adjacent_nums: Vec<u32> = vec![];
        for (num_str, num_start_pos) in numbers.iter() {
            let num_end_pos = Position(num_start_pos.0, num_start_pos.1 + num_str.len() as i32 - 1);
            for neighbor in neighbors.iter() {
                if neighbor.0 == num_start_pos.0
                    && neighbor.1 >= num_start_pos.1
                    && neighbor.1 <= num_end_pos.1
                {
                    adjacent_nums.push(num_str.parse::<u32>().unwrap());
                    break;
                }
            }
        }
        if adjacent_nums.len() == 2 {
            gear_ratios.push(adjacent_nums.iter().product());
        }
    }
    gear_ratios
}

fn process(contents: &str) -> u32 {
    let num_rows = contents.lines().count() as u32;
    let num_cols = contents.lines().next().unwrap().chars().count() as u32;
    let numbers = build_numbers(contents);
    let gears = build_gears(contents);
    let gear_ratios = build_gear_ratios(num_rows, num_cols, &numbers, &gears);
    gear_ratios.iter().sum()
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
