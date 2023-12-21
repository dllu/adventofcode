use std::collections::HashSet;
use std::env;
use std::fs;
use std::process;

const STEPS: u32 = 64;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Position(i32, i32);

#[derive(Clone, Debug)]
struct Garden {
    rocks: HashSet<Position>,
    start: Position,
    extents: (Position, Position),
}

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn build_garden(contents: &str) -> Garden {
    let mut rocks: HashSet<Position> = HashSet::new();
    let mut start: Position = Position(0, 0);
    for (row, line) in contents.lines().enumerate() {
        for (col, ch) in line.chars().enumerate() {
            match ch {
                '#' => {
                    rocks.insert(Position(row as i32, col as i32));
                }
                'S' => {
                    start = Position(row as i32, col as i32);
                }
                _ => {}
            };
        }
    }
    let mut trans_rocks: HashSet<Position> = HashSet::new();
    for rock in rocks.iter() {
        let trans_rock = Position(rock.0 - start.0, start.1 - rock.1);
        trans_rocks.insert(trans_rock);
    }
    rocks = trans_rocks;
    let num_rows = contents.lines().count() as i32;
    let num_cols = contents.lines().next().unwrap().chars().count() as i32;
    let extents = (
        Position(-num_rows / 2, -num_cols / 2),
        Position(num_rows / 2, num_cols / 2),
    );
    Garden {
        rocks,
        start: Position(0, 0),
        extents,
    }
}

fn in_bounds(pos: Position, extents: (Position, Position)) -> bool {
    pos.0 >= extents.0 .0 && pos.0 <= extents.1 .0 && pos.1 >= extents.0 .1 && pos.1 < extents.1 .1
}

fn walk(garden: &Garden) -> u32 {
    let mut visited: HashSet<Position> = HashSet::new();
    visited.insert(garden.start);
    for _ in 0..STEPS {
        let mut new_visited = HashSet::new();
        for pos in visited.iter().clone() {
            let neighbors = [
                Position(pos.0 - 1, pos.1),
                Position(pos.0 + 1, pos.1),
                Position(pos.0, pos.1 - 1),
                Position(pos.0, pos.1 + 1),
            ]
            .into_iter()
            .filter(|p| in_bounds(*p, garden.extents) && !garden.rocks.contains(p));
            for neighbor in neighbors {
                new_visited.insert(neighbor);
            }
        }
        visited = new_visited;
    }
    visited.len() as u32
}

fn process(contents: &str) -> u32 {
    let garden = build_garden(contents);
    walk(&garden)
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
