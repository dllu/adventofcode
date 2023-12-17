use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::process;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn process(contents: &str) -> u32 {
    let mut instances: HashMap<u32, u32> = HashMap::new();
    let total_cards = contents.lines().count() as u32;
    for line in contents.lines() {
        if let Some((card_part, rest)) = line.split_once(':') {
            let card = card_part
                .split_whitespace()
                .nth(1)
                .unwrap()
                .parse::<u32>()
                .unwrap();
            if let Some((winning_str, hand_str)) = rest.split_once('|') {
                let winning = winning_str
                    .split_whitespace()
                    .map(|n| n.parse::<u32>().unwrap())
                    .collect::<HashSet<u32>>();
                let hand = hand_str
                    .split_whitespace()
                    .map(|n| n.parse::<u32>().unwrap())
                    .collect::<HashSet<u32>>();
                let count = winning.intersection(&hand).count() as u32;
                for i in (card + 1)..(card + count + 1) {
                    let mut copies = 0;
                    copies += if instances.contains_key(&i) {
                        instances.get(&i).unwrap()
                    } else {
                        &0
                    };
                    copies += 1;
                    copies += if instances.contains_key(&card) {
                        instances.get(&card).unwrap()
                    } else {
                        &0
                    };
                    instances.insert(i, copies);
                }
            }
        }
    }
    instances.values().sum::<u32>() + total_cards
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
