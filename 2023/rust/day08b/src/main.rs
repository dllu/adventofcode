use num::integer;
use regex::Regex;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::process;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn process(contents: &str) -> u64 {
    let mut lineptr = contents.lines();
    let mut nodes_map: HashMap<String, (String, String)> = HashMap::new();
    let turns = lineptr.next().unwrap();
    let _ = lineptr.next().unwrap();
    let node_re =
        Regex::new(r"([A-Z][A-Z][A-Z]) = \(([A-Z][A-Z][A-Z]), ([A-Z][A-Z][A-Z])\)").unwrap();
    for line in lineptr {
        let node_caps = node_re.captures(line).unwrap();
        let source = node_caps.get(1).unwrap().as_str().to_string();
        let ldest = node_caps.get(2).unwrap().as_str().to_string();
        let rdest = node_caps.get(3).unwrap().as_str().to_string();
        nodes_map.insert(source, (ldest, rdest));
    }
    let start_nodes: Vec<String> = nodes_map
        .keys()
        .filter(|&k| k.ends_with('A'))
        .cloned()
        .collect();
    let mut all_steps: Vec<u64> = vec![];
    for start_node in start_nodes.iter() {
        let mut curr_node: String = start_node.clone();
        let mut steps: u64 = 0;
        let mut turn_idx: usize = 0;
        while !curr_node.ends_with('Z') {
            let turn: char = turns.chars().nth(turn_idx).unwrap();
            match turn {
                'L' => {
                    curr_node = nodes_map.get(&curr_node).unwrap().0.clone();
                }
                'R' => {
                    curr_node = nodes_map.get(&curr_node).unwrap().1.clone();
                }
                _ => panic!("unexpected char in turns"),
            }
            steps += 1;
            turn_idx = (turn_idx + 1) % turns.len();
        }
        all_steps.push(steps);
    }
    all_steps.iter().fold(1, |acc, x| integer::lcm(acc, *x))
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
