use std::collections::HashMap;
use std::env;
use std::fs;
use std::process;

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn recurse(springs: &String, counts: &Vec<u32>, cache: &mut HashMap<String, u64>) -> u64 {
    let hash_string = format!("{:?}{:?}", &springs, &counts);
    if cache.contains_key(&hash_string) {
        return *cache.get(&hash_string).unwrap();
    }
    if counts.is_empty() {
        if !springs.chars().any(|c| c == '#') {
            return 1;
        }
        return 0;
    }
    let mut counter: u64 = 0;
    let counts_minus_first = Vec::from(&counts[1..]);
    let upper_bound: i64 = springs.len() as i64 - counts_minus_first.iter().sum::<u32>() as i64
        + counts_minus_first.len() as i64
        - counts[0] as i64
        + 1;
    if upper_bound > 0 {
        for position in 0..upper_bound as usize {
            let broken: String = String::from(".").repeat(position);
            let working: String = String::from("#").repeat(counts[0] as usize);
            let mut possible: String = String::from(&broken);
            possible.push_str(&working);
            possible.push('.');
            let mut exited = false;
            for (spring, possible_spring) in springs.chars().zip(possible.chars()) {
                if spring != possible_spring && spring != '?' {
                    exited = true;
                    break;
                }
            }
            if !exited {
                let next_springs_opt = springs.get(possible.len()..);
                match next_springs_opt {
                    Some(next_springs) => {
                        counter += recurse(&next_springs.to_string(), &counts_minus_first, cache);
                    }
                    None => {
                        counter += recurse(&String::new(), &counts_minus_first, cache);
                    }
                }
            }
        }
    }
    cache.insert(hash_string, counter);
    counter
}

fn count_fits(line: &str) -> u64 {
    let mut line_split = line.split_whitespace();
    let spring_part: &str = line_split.next().unwrap();
    let mut new_springs: String = String::from(spring_part);
    new_springs.push('?');
    new_springs = new_springs.repeat(5);
    new_springs.pop(); // remove the trailing '?'
    let count_part: &str = line_split.next().unwrap();
    let counts: Vec<u32> = count_part
        .split(',')
        .map(|s| s.parse::<u32>().unwrap())
        .collect::<Vec<u32>>();
    let new_counts = counts.repeat(5);
    let mut cache: HashMap<String, u64> = HashMap::new();
    recurse(&new_springs, &new_counts, &mut cache)
}

fn process(contents: &str) -> u64 {
    let mut sum: u64 = 0;
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
