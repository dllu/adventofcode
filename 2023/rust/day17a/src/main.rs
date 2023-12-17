use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashSet};
use std::env;
use std::fs;
use std::process;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Position(i32, i32);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Direction(i32, i32);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct State {
    loss: u32,
    position: Position,
    direction: Direction,
    steps: u32,
}

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn build_grid(contents: &str) -> Vec<Vec<u32>> {
    let mut grid: Vec<Vec<u32>> = vec![];
    for line in contents.lines() {
        grid.push(
            line.chars()
                .map(|c| c as u32 - '0' as u32)
                .collect::<Vec<u32>>(),
        );
    }
    grid
}

fn in_bounds(pos: &Position, extents: &Position) -> bool {
    pos.0 >= 0 && pos.0 < extents.0 && pos.1 >= 0 && pos.1 < extents.1
}

fn find_min_loss(grid: &Vec<Vec<u32>>) -> u32 {
    let num_rows: i32 = grid.len() as i32;
    let num_cols: i32 = grid[0].len() as i32;
    let extents: Position = Position(num_rows, num_cols);
    let source: Position = Position(0, 0);
    let dest: Position = Position(num_rows - 1, num_cols - 1);
    let min_steps: u32 = 0;
    let max_steps: u32 = 3;
    let mut visited: HashSet<(Position, Direction, u32)> = HashSet::new();
    let mut q: BinaryHeap<Reverse<State>> = BinaryHeap::from([
        Reverse(State {
            loss: grid[source.0 as usize][source.1 as usize + 1],
            position: Position(source.0, source.1 + 1),
            direction: Direction(0, 1),
            steps: 0,
        }),
        Reverse(State {
            loss: grid[source.0 as usize + 1][source.1 as usize],
            position: Position(source.0 + 1, source.1),
            direction: Direction(1, 0),
            steps: 0,
        }),
    ]);
    while !q.is_empty() {
        let state = q.pop().unwrap().0;
        if state.position == dest && min_steps <= state.steps {
            return state.loss;
        }
        if visited.contains(&(state.position, state.direction, state.steps)) {
            continue;
        }
        visited.insert((state.position, state.direction, state.steps));
        let straight_pos: Position = Position(
            state.position.0 + state.direction.0,
            state.position.1 + state.direction.1,
        );
        if state.steps < (max_steps - 1) && in_bounds(&straight_pos, &extents) {
            q.push(Reverse(State {
                loss: state.loss + grid[straight_pos.0 as usize][straight_pos.1 as usize],
                position: straight_pos,
                direction: state.direction,
                steps: state.steps + 1,
            }));
        }
        if min_steps <= state.steps {
            let left_dir: Direction = Direction(state.direction.1, -state.direction.0);
            let left_pos: Position =
                Position(state.position.0 + left_dir.0, state.position.1 + left_dir.1);
            if in_bounds(&left_pos, &extents) {
                q.push(Reverse(State {
                    loss: state.loss + grid[left_pos.0 as usize][left_pos.1 as usize],
                    position: left_pos,
                    direction: left_dir,
                    steps: 0,
                }));
            }
            let right_dir: Direction = Direction(-state.direction.1, state.direction.0);
            let right_pos: Position = Position(
                state.position.0 + right_dir.0,
                state.position.1 + right_dir.1,
            );
            if in_bounds(&right_pos, &extents) {
                q.push(Reverse(State {
                    loss: state.loss + grid[right_pos.0 as usize][right_pos.1 as usize],
                    position: right_pos,
                    direction: right_dir,
                    steps: 0,
                }));
            }
        }
    }
    0
}

fn process(contents: &str) -> u32 {
    let grid = build_grid(contents);
    find_min_loss(&grid)
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
