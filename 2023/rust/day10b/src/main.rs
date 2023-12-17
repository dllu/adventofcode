use std::collections::{HashMap, HashSet, VecDeque};
use std::env;
use std::fs;
use std::process;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Position(u32, u32);

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn build_grid(contents: &str) -> (HashMap<Position, char>, Position) {
    let mut grid: HashMap<Position, char> = HashMap::new();
    let mut start: Position = Position(0, 0);
    for (row, line) in contents.lines().enumerate() {
        for (col, ch) in line.chars().enumerate() {
            grid.insert(Position(row as u32, col as u32), ch);
            if ch == 'S' {
                start = Position(row as u32, col as u32);
            }
        }
    }
    (grid, start)
}

fn build_loop(
    grid: &HashMap<Position, char>,
    start: &Position,
) -> (HashSet<Position>, HashMap<Position, char>) {
    let mut q: VecDeque<Position> = VecDeque::new();
    let mut pipe_loop: HashSet<Position> = HashSet::new();
    let mut possibles: HashSet<char> = HashSet::from(['|', '-', 'L', 'J', '7', 'F']);
    let max_row = grid.keys().map(|k| k.0).max().unwrap();
    let max_col = grid.keys().map(|k| k.1).max().unwrap();
    q.push_back(*start);
    while !q.is_empty() {
        let pos = q.pop_front().unwrap();
        pipe_loop.insert(pos);
        let pipe = grid.get(&pos).unwrap();
        if pos.0 > 0 && ['S', '|', 'L', 'J'].contains(pipe) {
            let north = Position(pos.0 - 1, pos.1);
            let north_pipe = grid.get(&north).unwrap();
            if ['|', '7', 'F'].contains(north_pipe) && !pipe_loop.contains(&north) {
                q.push_back(north);
                if *pipe == 'S' {
                    possibles = possibles
                        .intersection(&HashSet::from(['|', 'L', 'J']))
                        .cloned()
                        .collect();
                }
            }
        }
        if pos.0 < max_row && ['S', '|', '7', 'F'].contains(pipe) {
            let south = Position(pos.0 + 1, pos.1);
            let south_pipe = grid.get(&south).unwrap();
            if ['|', 'L', 'J'].contains(south_pipe) && !pipe_loop.contains(&south) {
                q.push_back(south);
                if *pipe == 'S' {
                    possibles = possibles
                        .intersection(&HashSet::from(['|', '7', 'F']))
                        .cloned()
                        .collect();
                }
            }
        }
        if pos.1 > 0 && ['S', '-', '7', 'J'].contains(pipe) {
            let west = Position(pos.0, pos.1 - 1);
            let west_pipe = grid.get(&west).unwrap();
            if ['-', 'L', 'F'].contains(west_pipe) && !pipe_loop.contains(&west) {
                q.push_back(west);
                if *pipe == 'S' {
                    possibles = possibles
                        .intersection(&HashSet::from(['-', '7', 'J']))
                        .cloned()
                        .collect();
                }
            }
        }
        if pos.1 < max_col && ['S', '-', 'L', 'F'].contains(pipe) {
            let east = Position(pos.0, pos.1 + 1);
            let east_pipe = grid.get(&east).unwrap();
            if ['-', 'J', '7'].contains(east_pipe) && !pipe_loop.contains(&east) {
                q.push_back(east);
                if *pipe == 'S' {
                    possibles = possibles
                        .intersection(&HashSet::from(['-', 'L', 'F']))
                        .cloned()
                        .collect();
                }
            }
        }
    }
    let start_pipe = possibles.iter().next().unwrap();
    let mut new_grid = grid.clone();
    new_grid.insert(*start, *start_pipe);
    (pipe_loop, new_grid)
}

fn count_interior(pipe_loop: &HashSet<Position>, grid: &HashMap<Position, char>) -> u32 {
    let mut count: u32 = 0;
    let mut new_grid: HashMap<Position, char> = HashMap::new();
    let max_row = grid.keys().map(|p| p.0).max().unwrap();
    let max_col = grid.keys().map(|p| p.1).max().unwrap();
    for pos in grid.keys() {
        if !pipe_loop.contains(pos) {
            new_grid.insert(*pos, '.');
        } else {
            let pipe = grid.get(pos).unwrap();
            new_grid.insert(*pos, *pipe);
        }
    }
    for row in 0..=max_row {
        for col in 0..=max_col {
            let pos = Position(row, col);
            let pipe = new_grid.get(&pos).unwrap();
            if *pipe != '.' {
                continue;
            }
            let mut intersections: u32 = 0;
            let mut corner_pipes: VecDeque<char> = VecDeque::new();
            for i in (col + 1)..=max_col {
                let scan_pos = Position(row, i);
                let scan_pipe = new_grid.get(&scan_pos).unwrap();
                if *scan_pipe == '|' {
                    intersections += 1;
                } else if ['F', 'L'].contains(scan_pipe) {
                    corner_pipes.push_back(*scan_pipe);
                } else if !corner_pipes.is_empty()
                    && ((*scan_pipe == 'J' && *corner_pipes.iter().last().unwrap() == 'F')
                        || (*scan_pipe == '7' && *corner_pipes.iter().last().unwrap() == 'L'))
                {
                    let _ = corner_pipes.pop_back().unwrap();
                    intersections += 1;
                }
            }
            if intersections % 2 == 1 {
                count += 1;
            }
        }
    }
    count
}

fn process(contents: &str) -> u32 {
    let (grid, start) = build_grid(contents);
    let (pipe_loop, new_grid) = build_loop(&grid, &start);
    count_interior(&pipe_loop, &new_grid)
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
