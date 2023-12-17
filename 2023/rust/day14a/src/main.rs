use std::env;
use std::fs;
use std::process;

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Position(i32, i32);

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Direction {
    North,
    //East,
    //South,
    //West,
}

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn build_grid(contents: &str) -> Vec<Vec<char>> {
    let mut grid: Vec<Vec<char>> = vec![];
    for line in contents.lines() {
        let row: Vec<char> = line.chars().collect();
        grid.push(row);
    }
    grid
}

fn move_rocks(grid: &mut Vec<Vec<char>>, direction: Direction) -> bool {
    let num_rows = grid.len();
    let num_cols = grid[0].len();
    let mut move_count: u32 = 0;
    match direction {
        Direction::North => {
            for row in 1..num_rows {
                for col in 0..num_cols {
                    if grid[row][col] == 'O' && grid[row - 1][col] == '.' {
                        grid[row][col] = '.';
                        grid[row - 1][col] = 'O';
                        move_count += 1;
                    }
                }
            }
            move_count > 0
        } //_ => {
          // other dirs are not implemented for now, so ignore them
          //    false
          //}
    }
}

fn total_load(grid: &Vec<Vec<char>>, direction: Direction) -> u32 {
    let num_rows = grid.len();
    let mut total_load: u32 = 0;
    match direction {
        Direction::North => {
            for (i, row) in grid.iter().enumerate().take(num_rows) {
                total_load += (row.iter().filter(|&c| *c == 'O').count() * (num_rows - i)) as u32;
            }
        } //_ => {
          // other dirs are not implemented for now
          //}
    }
    total_load
}

fn process(contents: &str) -> u32 {
    let mut grid = build_grid(contents);
    while move_rocks(&mut grid, Direction::North) {}
    total_load(&grid, Direction::North)
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
