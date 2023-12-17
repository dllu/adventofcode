use std::env;
use std::fs;
use std::process;

const CYCLES: u32 = 1000000000;

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Position(i32, i32);

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Direction {
    North,
    East,
    South,
    West,
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
        }
        Direction::South => {
            for row in 0..(num_rows - 1) {
                for col in 0..num_cols {
                    if grid[row][col] == 'O' && grid[row + 1][col] == '.' {
                        grid[row][col] = '.';
                        grid[row + 1][col] = 'O';
                        move_count += 1;
                    }
                }
            }
        }
        Direction::West => {
            for row in grid.iter_mut().take(num_rows) {
                for col in 1..num_cols {
                    if row[col] == 'O' && row[col - 1] == '.' {
                        row[col] = '.';
                        row[col - 1] = 'O';
                        move_count += 1;
                    }
                }
            }
        }
        Direction::East => {
            for row in grid.iter_mut().take(num_rows) {
                for col in 0..(num_cols - 1) {
                    if row[col] == 'O' && row[col + 1] == '.' {
                        row[col] = '.';
                        row[col + 1] = 'O';
                        move_count += 1;
                    }
                }
            }
        }
    }
    move_count > 0
}

fn total_load(grid: &Vec<Vec<char>>, direction: Direction) -> u32 {
    let num_rows = grid.len();
    let mut total_load: u32 = 0;
    match direction {
        Direction::North => {
            for (i, row) in grid.iter().enumerate().take(num_rows) {
                total_load += (row.iter().filter(|&c| *c == 'O').count() * (num_rows - i)) as u32;
            }
        }
        _ => {
            // not implemented
        }
    }
    total_load
}

fn process(contents: &str) -> u32 {
    let mut grid = build_grid(contents);
    let mut grid_states: Vec<Vec<Vec<char>>> = vec![];
    let mut cycle_start = 0;
    let mut cycle_len = 0;
    for i in 0..CYCLES {
        grid_states.push(grid.clone());
        while move_rocks(&mut grid, Direction::North) {}
        while move_rocks(&mut grid, Direction::West) {}
        while move_rocks(&mut grid, Direction::South) {}
        while move_rocks(&mut grid, Direction::East) {}
        if grid_states.contains(&grid) {
            cycle_start = grid_states.iter().position(|x| x == &grid).unwrap() as u32;
            cycle_len = i - cycle_start + 1;
            println!("cycle detected at iteration {i}, start = {cycle_start}, length {cycle_len}");
            break;
        }
    }
    let final_index = (CYCLES - cycle_start) % cycle_len + cycle_start;
    let final_grid = grid_states[final_index as usize].clone();
    total_load(&final_grid, Direction::North)
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
