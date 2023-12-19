use std::env;
use std::fs;
use std::process;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Position(i32, i32);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Direction(i32, i32);

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn calc_filled(vertices: &Vec<Position>) -> i32 {
    let mut area: i32 = 0;
    let mut perimeter: i32 = 0;

    // Shoelace formula
    for i in 0..(vertices.len() - 1) {
        let first = vertices[i];
        let second = vertices[i + 1];
        area += (first.0 * second.1) - (first.1 * second.0);
        perimeter += f64::sqrt(
            ((second.0 - first.0) * (second.0 - first.0)) as f64
                + ((second.1 - first.1) * (second.1 - first.1)) as f64,
        ) as i32;
    }
    area = i32::abs(area) / 2;

    // Pick's theorem
    let interior: i32 = area - (perimeter / 2) + 1;

    interior + perimeter
}

fn process(contents: &str) -> i32 {
    let start: Position = Position(0, 0);
    let mut vertices: Vec<Position> = vec![];
    vertices.push(start);
    let mut pos: Position = start;
    for line in contents.lines() {
        let dir_part = line.split(' ').next().unwrap();
        let steps = line.split(' ').nth(1).unwrap().parse::<i32>().unwrap();
        let dir = match dir_part {
            "U" => Direction(0, 1),
            "D" => Direction(0, -1),
            "L" => Direction(-1, 0),
            "R" => Direction(1, 0),
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
