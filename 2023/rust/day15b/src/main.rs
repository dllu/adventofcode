use std::env;
use std::fs;
use std::process;

const NUM_BOXES: usize = 256;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn hash_string(s: &str) -> u32 {
    let mut result: u32 = 0;
    s.chars().for_each(|ch| {
        result += ch as u32;
        result *= 17;
        result %= NUM_BOXES as u32;
    });
    result
}

fn process(contents: &str) -> u32 {
    let mut result: u32 = 0;
    let mut boxes: Vec<Vec<(&str, u32)>> = vec![];
    for _ in 0..NUM_BOXES {
        boxes.push(vec![]);
    }
    let trimmed = contents.trim_end_matches('\n');
    trimmed.split(',').for_each(|s| {
        if s.contains('=') {
            if let Some((label, focal_str)) = s.split_once('=') {
                let focal: u32 = focal_str.parse::<u32>().unwrap();
                let hash = hash_string(label) as usize;
                if boxes[hash].iter().any(|entry| entry.0 == label) {
                    let pos = boxes[hash]
                        .iter()
                        .position(|entry| entry.0 == label)
                        .unwrap();
                    let elem = boxes[hash].get_mut(pos).unwrap();
                    *elem = (label, focal);
                } else {
                    boxes[hash].push((label, focal));
                }
            }
        } else if s.contains('-') {
            let label = &s[..s.len() - 1];
            let hash = hash_string(label) as usize;
            if boxes[hash].iter().any(|entry| entry.0 == label) {
                let pos = boxes[hash]
                    .iter()
                    .position(|entry| entry.0 == label)
                    .unwrap();
                boxes[hash].remove(pos);
            }
        }
    });
    for box_num in 0..NUM_BOXES {
        for slot_num in 0..boxes[box_num].len() {
            result += (box_num as u32 + 1) * (slot_num as u32 + 1) * boxes[box_num][slot_num].1;
        }
    }
    result
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
