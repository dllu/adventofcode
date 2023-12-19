use std::env;
use std::fs;
use std::process;

#[derive(Clone, Copy, Debug, PartialEq)]
struct Position(i32, i32);

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn build_grid(contents: &str) -> (Vec<Vec<char>>, Position) {
    let mut grid: Vec<Vec<char>> = vec![];
    let mut start: Position = Position(0, 0);
    for (row, line) in contents.lines().enumerate() {
        grid.push(vec![]);
        for (col, ch) in line.chars().enumerate() {
            grid[row].push(ch);
            if ch == 'S' {
                start = Position(row as i32, col as i32);
            }
        }
    }
    (grid, start)
}

fn in_bounds(pos: &Position, max_extent: &Position) -> bool {
    pos.0 >= 0 && pos.0 < max_extent.0 && pos.1 >= 0 && pos.1 < max_extent.1
}

fn connected(pos: &Position, next: &Position, grid: &[Vec<char>]) -> bool {
    let delta: Position = Position(next.0 - pos.0, next.1 - pos.1);
    let pos_ch: char = grid[pos.0 as usize][pos.1 as usize];
    let next_ch: char = grid[next.0 as usize][next.1 as usize];
    match pos_ch {
        'S' => match delta {
            Position(1, 0) => ['J', '|', 'L'].contains(&next_ch),
            Position(-1, 0) => ['7', '|', 'F'].contains(&next_ch),
            Position(0, 1) => ['J', '-', '7'].contains(&next_ch),
            Position(0, -1) => ['L', '-', 'F'].contains(&next_ch),
            _ => false,
        },
        '|' => match delta {
            Position(1, 0) => ['J', '|', 'L'].contains(&next_ch),
            Position(-1, 0) => ['7', '|', 'F'].contains(&next_ch),
            _ => false,
        },
        '-' => match delta {
            Position(0, 1) => ['J', '-', '7'].contains(&next_ch),
            Position(0, -1) => ['L', '-', 'F'].contains(&next_ch),
            _ => false,
        },
        'L' => match delta {
            Position(-1, 0) => ['7', '|', 'F'].contains(&next_ch),
            Position(0, 1) => ['J', '-', '7'].contains(&next_ch),
            _ => false,
        },
        'J' => match delta {
            Position(-1, 0) => ['7', '|', 'F'].contains(&next_ch),
            Position(0, -1) => ['L', '-', 'F'].contains(&next_ch),
            _ => false,
        },
        '7' => match delta {
            Position(1, 0) => ['J', '|', 'L'].contains(&next_ch),
            Position(0, -1) => ['L', '-', 'F'].contains(&next_ch),
            _ => false,
        },
        'F' => match delta {
            Position(1, 0) => ['J', '|', 'L'].contains(&next_ch),
            Position(0, 1) => ['J', '-', '7'].contains(&next_ch),
            _ => false,
        },
        _ => false,
    }
}

fn build_loop(grid: &Vec<Vec<char>>, start: &Position) -> Vec<Position> {
    let max_row: i32 = grid.len() as i32;
    let max_col: i32 = grid[0].len() as i32;
    let max_extent: Position = Position(max_row, max_col);
    let mut vertices: Vec<Position> = vec![];
    let mut pos: Position = *start;
    loop {
        vertices.push(pos);
        let neighbors = [
            Position(pos.0 + 1, pos.1),
            Position(pos.0 - 1, pos.1),
            Position(pos.0, pos.1 + 1),
            Position(pos.0, pos.1 - 1),
        ]
        .into_iter()
        .filter(|neighbor| {
            in_bounds(neighbor, &max_extent)
                && connected(&pos, neighbor, grid)
                && !vertices.contains(neighbor)
        })
        .collect::<Vec<Position>>();
        if neighbors.is_empty() {
            break;
        }
        pos = *neighbors.first().unwrap();
    }
    vertices
}

fn calc_interior(pipe_loop: &[Position]) -> u32 {
    let mut vertices: Vec<Position> = pipe_loop.to_vec();
    vertices.push(pipe_loop[0]);

    // Shoelace formula
    let mut area: i32 = 0;
    for i in 0..(vertices.len() - 1) {
        let first = vertices[i];
        let second = vertices[i + 1];
        area += (first.0 * second.1) - (first.1 * second.0);
    }
    area = i32::abs(area) / 2;

    // Pick's theorem
    let perimeter: i32 = vertices.len() as i32 - 1;
    let interior: i32 = area - (perimeter / 2) + 1;
    interior as u32
}

fn process(contents: &str) -> u32 {
    let (grid, start) = build_grid(contents);
    let pipe_loop = build_loop(&grid, &start);
    calc_interior(&pipe_loop)
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
