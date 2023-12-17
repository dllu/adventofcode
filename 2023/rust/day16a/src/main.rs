use std::collections::{HashMap, HashSet, VecDeque};
use std::env;
use std::fs;
use std::process;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Position(i32, i32);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn build_grid(contents: &str) -> Vec<Vec<char>> {
    let mut grid: Vec<Vec<char>> = vec![];
    for line in contents.lines() {
        grid.push(line.chars().collect::<Vec<char>>());
    }
    grid
}

fn in_bounds(pos: &Position, extents: &Position) -> bool {
    pos.0 >= 0 && pos.0 < extents.0 && pos.1 >= 0 && pos.1 < extents.1
}

fn calc_energized(grid: &Vec<Vec<char>>, start_pos: &Position, start_dir: &Direction) -> u32 {
    let num_rows = grid.len();
    let num_cols = grid[0].len();
    let extents = Position(num_rows as i32, num_cols as i32);
    let mut queue: VecDeque<(Position, Direction)> = VecDeque::new();
    let mut cache: HashMap<Position, HashSet<Direction>> = HashMap::new();
    queue.push_back((*start_pos, *start_dir));
    while !queue.is_empty() {
        let (pos, dir) = queue.pop_front().unwrap();
        cache
            .entry(pos)
            .and_modify(|dirs| {
                dirs.insert(dir);
            })
            .or_insert(std::iter::once(dir).collect::<HashSet<Direction>>());
        let object = grid[pos.0 as usize][pos.1 as usize];
        match object {
            '.' => {
                let new_pos = match dir {
                    Direction::Up => Position(pos.0 - 1, pos.1),
                    Direction::Down => Position(pos.0 + 1, pos.1),
                    Direction::Left => Position(pos.0, pos.1 - 1),
                    Direction::Right => Position(pos.0, pos.1 + 1),
                };
                if in_bounds(&new_pos, &extents)
                    && (!cache.contains_key(&new_pos)
                        || !cache.get(&new_pos).unwrap().contains(&dir))
                {
                    queue.push_back((new_pos, dir));
                }
            }
            '\\' => {
                let new_pos: Position;
                let new_dir: Direction;
                match dir {
                    Direction::Up => {
                        new_pos = Position(pos.0, pos.1 - 1);
                        new_dir = Direction::Left;
                    }
                    Direction::Down => {
                        new_pos = Position(pos.0, pos.1 + 1);
                        new_dir = Direction::Right;
                    }
                    Direction::Left => {
                        new_pos = Position(pos.0 - 1, pos.1);
                        new_dir = Direction::Up;
                    }
                    Direction::Right => {
                        new_pos = Position(pos.0 + 1, pos.1);
                        new_dir = Direction::Down;
                    }
                }
                if in_bounds(&new_pos, &extents)
                    && (!cache.contains_key(&new_pos)
                        || !cache.get(&new_pos).unwrap().contains(&new_dir))
                {
                    queue.push_back((new_pos, new_dir));
                }
            }
            '/' => {
                let new_pos: Position;
                let new_dir: Direction;
                match dir {
                    Direction::Up => {
                        new_pos = Position(pos.0, pos.1 + 1);
                        new_dir = Direction::Right;
                    }
                    Direction::Down => {
                        new_pos = Position(pos.0, pos.1 - 1);
                        new_dir = Direction::Left;
                    }
                    Direction::Left => {
                        new_pos = Position(pos.0 + 1, pos.1);
                        new_dir = Direction::Down;
                    }
                    Direction::Right => {
                        new_pos = Position(pos.0 - 1, pos.1);
                        new_dir = Direction::Up;
                    }
                }
                if in_bounds(&new_pos, &extents)
                    && (!cache.contains_key(&new_pos)
                        || !cache.get(&new_pos).unwrap().contains(&new_dir))
                {
                    queue.push_back((new_pos, new_dir));
                }
            }
            '-' => {
                match dir {
                    Direction::Left | Direction::Right => {
                        let mut new_pos = pos;
                        if dir == Direction::Left {
                            new_pos.1 -= 1;
                        } else {
                            new_pos.1 += 1;
                        }
                        if in_bounds(&new_pos, &extents)
                            && (!cache.contains_key(&new_pos)
                                || !cache.get(&new_pos).unwrap().contains(&dir))
                        {
                            queue.push_back((new_pos, dir));
                        }
                    }
                    Direction::Up | Direction::Down => {
                        let new_pos1 = Position(pos.0, pos.1 - 1);
                        if in_bounds(&new_pos1, &extents)
                            && (!cache.contains_key(&new_pos1)
                                || !cache.get(&new_pos1).unwrap().contains(&Direction::Left))
                        {
                            queue.push_back((new_pos1, Direction::Left));
                        }
                        let new_pos2 = Position(pos.0, pos.1 + 1);
                        if in_bounds(&new_pos2, &extents)
                            && (!cache.contains_key(&new_pos2)
                                || !cache.get(&new_pos2).unwrap().contains(&Direction::Right))
                        {
                            queue.push_back((new_pos2, Direction::Right));
                        }
                    }
                };
            }
            '|' => {
                match dir {
                    Direction::Up | Direction::Down => {
                        let mut new_pos = pos;
                        if dir == Direction::Up {
                            new_pos.0 -= 1;
                        } else {
                            new_pos.0 += 1;
                        }
                        if in_bounds(&new_pos, &extents)
                            && (!cache.contains_key(&new_pos)
                                || !cache.get(&new_pos).unwrap().contains(&dir))
                        {
                            queue.push_back((new_pos, dir));
                        }
                    }
                    Direction::Left | Direction::Right => {
                        let new_pos1 = Position(pos.0 - 1, pos.1);
                        if in_bounds(&new_pos1, &extents)
                            && (!cache.contains_key(&new_pos1)
                                || !cache.get(&new_pos1).unwrap().contains(&Direction::Up))
                        {
                            queue.push_back((new_pos1, Direction::Up));
                        }
                        let new_pos2 = Position(pos.0 + 1, pos.1);
                        if in_bounds(&new_pos2, &extents)
                            && (!cache.contains_key(&new_pos2)
                                || !cache.get(&new_pos2).unwrap().contains(&Direction::Down))
                        {
                            queue.push_back((new_pos2, Direction::Down));
                        }
                    }
                };
            }
            _ => panic!("unexpected char"),
        }
    }
    cache.keys().count() as u32
}

fn process(contents: &str) -> u32 {
    let grid = build_grid(contents);
    calc_energized(&grid, &Position(0, 0), &Direction::Right)
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
