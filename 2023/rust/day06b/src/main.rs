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
    let mut time_str: String = String::new();
    if let Some(line) = lineptr.next() {
        if let Some((_, timevals)) = line.split_once(':') {
            time_str = timevals.split_whitespace().collect();
        } else {
            panic!("unexpected input");
        }
    }
    let time: i64 = time_str.parse::<i64>().unwrap();
    let mut distance_str: String = String::new();
    if let Some(line) = lineptr.next() {
        if let Some((_, distvals)) = line.split_once(':') {
            distance_str = distvals.split_whitespace().collect();
        } else {
            panic!("unexpected input");
        }
    }
    let distance: i64 = distance_str.parse::<i64>().unwrap();
    calc_wins(time, distance)
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
