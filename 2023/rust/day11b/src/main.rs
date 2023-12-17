use std::env;
use std::fs;
use std::process;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Position(u64, u64);

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn process(contents: &str) -> u64 {
    let mut total_dist: u64 = 0;
    let mut galaxies: Vec<Position> = vec![];
    let num_rows: u64 = contents.lines().count() as u64;
    let num_cols: u64 = contents.lines().next().unwrap().chars().count() as u64;
    let mut blank_rows: Vec<u64> = vec![];
    let mut blank_cols: Vec<u64> = vec![];
    for (row, line) in contents.lines().enumerate() {
        for (col, ch) in line.chars().enumerate() {
            if ch == '#' {
                galaxies.push(Position(row as u64, col as u64));
            }
        }
    }
    for i in 0..num_rows {
        if !galaxies.iter().any(|p| p.0 == i) {
            blank_rows.push(i);
        }
    }
    for i in 0..num_cols {
        if !galaxies.iter().any(|p| p.1 == i) {
            blank_cols.push(i);
        }
    }
    for galaxy in galaxies.iter_mut() {
        let mut num_blank_rows = 0;
        for blank_row in blank_rows.iter() {
            if galaxy.0 > *blank_row {
                num_blank_rows += 999999;
            }
        }
        galaxy.0 += num_blank_rows;
        let mut num_blank_cols = 0;
        for blank_col in blank_cols.iter() {
            if galaxy.1 > *blank_col {
                num_blank_cols += 999999;
            }
        }
        galaxy.1 += num_blank_cols;
    }
    for i in 0..galaxies.len() {
        for j in i..galaxies.len() {
            total_dist += (i64::abs(galaxies[i].0 as i64 - galaxies[j].0 as i64)
                + i64::abs(galaxies[i].1 as i64 - galaxies[j].1 as i64))
                as u64;
        }
    }
    total_dist
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
