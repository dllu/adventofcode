use std::collections::VecDeque;
use std::env;
use std::fs;
use std::process;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn build_possibles(springs: &str) -> Vec<String> {
    let mut possibles: Vec<String> = vec![];
    let mut q: VecDeque<String> = VecDeque::new();
    q.push_back(springs.to_string());
    while !q.is_empty() {
        let curr_springs = q.pop_front().unwrap();
        if !curr_springs.chars().any(|c| c == '?') {
            possibles.push(curr_springs);
        } else if let Some(i) = curr_springs.find('?') {
            let mut temp1: Vec<char> = curr_springs.chars().collect();
            if let Some(ch) = temp1.get_mut(i) {
                *ch = '#';
            }
            q.push_back(temp1.into_iter().collect());
            let mut temp2: Vec<char> = curr_springs.chars().collect();
            if let Some(ch) = temp2.get_mut(i) {
                *ch = '.';
            }
            q.push_back(temp2.into_iter().collect());
        }
    }
    possibles
}

fn does_fit(possible: &str, counts: &Vec<u32>) -> bool {
    let working_groups: Vec<&str> = possible
        .split(|c| c != '#')
        .filter(|s| !s.is_empty())
        .collect();
    if working_groups.len() != counts.len() {
        return false;
    }
    for i in 0..working_groups.len() {
        if working_groups[i].len() != counts[i] as usize {
            return false;
        }
    }
    true
}

fn count_fits(line: &str) -> u32 {
    let mut fits: u32 = 0;
    let mut line_split = line.split_whitespace();
    let spring_part: &str = line_split.next().unwrap();
    let count_part: &str = line_split.next().unwrap();
    let counts: Vec<u32> = count_part
        .split(',')
        .map(|s| s.parse::<u32>().unwrap())
        .collect::<Vec<u32>>();
    let possibles = build_possibles(spring_part);
    for possible in possibles.iter() {
        if does_fit(possible, &counts) {
            fits += 1;
        }
    }
    fits
}

fn process(contents: &str) -> u32 {
    let mut sum: u32 = 0;
    for line in contents.lines() {
        sum += count_fits(line);
    }
    sum
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
