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
    for i in 0..(vertices.len() - 1) {
        let mut window = vertices[i..=(i + 1)].iter();
        let first = window.next().unwrap();
        let second = window.next().unwrap();
        area += (first.0 * second.1) - (first.1 * second.0);
        if first.0 == second.0 {
            perimeter += i32::abs(first.1 - second.1);
        } else if first.1 == second.1 {
            perimeter += i32::abs(first.0 - second.0);
        }
    }
    ((i32::abs(area) / 2) - (perimeter / 2) + 1) + perimeter
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
