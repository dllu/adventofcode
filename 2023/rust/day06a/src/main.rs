use std::env;
use std::fs;
use std::process;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn calc_wins(time: i64, distance: i64) -> i64 {
    let ftime: f64 = time as f64;
    let fdist: f64 = distance as f64;
    let disc: f64 = ftime * ftime - 4.0 * fdist;
    let root1: f64 = (ftime - disc.sqrt()) / 2.0;
    let root2: f64 = (ftime + disc.sqrt()) / 2.0;
    if root2.floor() == root2 && root1.ceil() == root1 {
        (root2.floor() - root1.ceil()) as i64 - 1
    } else {
        (root2.floor() - root1.ceil()) as i64 + 1
    }
}

fn process(contents: &str) -> i64 {
    let mut lineptr = contents.lines();
    let mut times: Vec<i64> = vec![];
    if let Some(line) = lineptr.next() {
        if let Some((_, timevals)) = line.split_once(':') {
            times = timevals
                .split_whitespace()
                .map(|s| s.parse::<i64>().unwrap())
                .collect();
        } else {
            panic!("unexpected input");
        }
    }
    let mut distances: Vec<i64> = vec![];
    if let Some(line) = lineptr.next() {
        if let Some((_, distvals)) = line.split_once(':') {
            distances = distvals
                .split_whitespace()
                .map(|s| s.parse::<i64>().unwrap())
                .collect();
        } else {
            panic!("unexpected input");
        }
    }
    let mut wins: Vec<i64> = vec![];
    for race in 0..times.len() {
        wins.push(calc_wins(times[race], distances[race]));
    }
    wins.iter().product()
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
