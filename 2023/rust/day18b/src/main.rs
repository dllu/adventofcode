use std::env;
use std::fs;
use std::process;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Position(i64, i64);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Direction(i64, i64);

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn calc_filled(vertices: &Vec<Position>) -> i64 {
    let mut area: i64 = 0;
    let mut perimeter: i64 = 0;

    // Shoelace formula
    for i in 0..(vertices.len() - 1) {
        let mut window = vertices[i..=(i + 1)].iter();
        let first = window.next().unwrap();
        let second = window.next().unwrap();
        area += (first.0 * second.1) - (first.1 * second.0);
        perimeter += f64::abs(f64::sqrt(
            ((second.0 - first.0) * (second.0 - first.0)) as f64
                + ((second.1 - first.1) * (second.1 - first.1)) as f64,
        )) as i64;
    }
    let area: i64 = i64::abs(area) / 2;

    // Pick's theorem
    let interior: i64 = area - (perimeter / 2) + 1;

    interior + perimeter
}

fn process(contents: &str) -> i64 {
    let start: Position = Position(0, 0);
    let mut vertices: Vec<Position> = vec![];
    vertices.push(start);
    let mut pos: Position = start;
    for line in contents.lines() {
        let hex_digits = line
            .split(' ')
            .nth(2)
            .unwrap()
            .chars()
            .filter(|c| c.is_digit(16))
            .collect::<String>();
        let steps: i64 = i64::from_str_radix(&hex_digits[0..=4], 16).unwrap();
        let dir_digit = &hex_digits[5..=5];
        let dir = match dir_digit {
            "3" => Direction(0, 1),
            "1" => Direction(0, -1),
            "2" => Direction(-1, 0),
            "0" => Direction(1, 0),
            _ => panic!("unknown direction!"),
        };
        pos.0 += steps * dir.0;
        pos.1 += steps * dir.1;
        vertices.push(pos);
    }
    calc_filled(&vertices)
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
