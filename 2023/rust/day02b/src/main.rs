use std::env;
use std::fs;
use std::process;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn process(contents: &str) -> u64 {
    let mut powers: Vec<u64> = vec![];
    for line in contents.lines() {
        if let Some((_, reveals_str)) = line.split_once(": ") {
            let mut red_needed: u64 = 0;
            let mut green_needed: u64 = 0;
            let mut blue_needed: u64 = 0;
            for subset_str in reveals_str.split("; ") {
                for cubes_str in subset_str.split(", ") {
                    if let Some((amount_str, color)) = cubes_str.split_once(' ') {
                        let amount: u64 = amount_str.parse::<u64>().unwrap();
                        match color {
                            "red" => {
                                if amount > red_needed {
                                    red_needed = amount;
                                }
                            }
                            "green" => {
                                if amount > green_needed {
                                    green_needed = amount;
                                }
                            }
                            "blue" => {
                                if amount > blue_needed {
                                    blue_needed = amount;
                                }
                            }
                            _ => panic!("unknown color"),
                        }
                    }
                }
            }
            powers.push(red_needed * green_needed * blue_needed);
        }
    }
    powers.iter().sum()
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
