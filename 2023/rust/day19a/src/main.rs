use regex::Regex;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::process;

#[derive(Debug)]
enum Predicate {
    LessThan(String, u32),
    GreaterThan(String, u32),
    Unconditional,
}

#[derive(Debug, PartialEq)]
enum Action {
    Accept,
    Reject,
    Goto(String),
}

type Rule = (Predicate, Action);

type Workflow = Vec<Rule>;

#[derive(Debug)]
struct Part {
    x: u32,
    m: u32,
    a: u32,
    s: u32,
}

impl Part {
    fn add_ratings(&self) -> u32 {
        self.x + self.m + self.a + self.s
    }
}

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn parse_input(contents: &str) -> (HashMap<String, Workflow>, Vec<Part>) {
    let mut flows: HashMap<String, Workflow> = HashMap::new();
    let mut parts: Vec<Part> = vec![];
    let mut lineptr = contents.lines();
    let flow_re = Regex::new(r"([a-z]+)\{([A-Za-z0-9:<>,]+)\}").unwrap();
    let cond_re = Regex::new(r"([xmas])(<|>)(\d+):(A|R|[a-z]+)").unwrap();
    loop {
        let line = lineptr.next().unwrap();
        if line.is_empty() {
            break;
        }
        let flow_caps = flow_re.captures(line).unwrap();
        let flow_id = flow_caps.get(1).unwrap().as_str();
        let rules_str = flow_caps.get(2).unwrap().as_str();
        let mut workflow: Workflow = vec![];
        for rule in rules_str.split(',') {
            if cond_re.is_match(rule) {
                let cond_caps = cond_re.captures(rule).unwrap();
                let rating = cond_caps.get(1).unwrap().as_str();
                let test = cond_caps.get(2).unwrap().as_str();
                let value = cond_caps.get(3).unwrap().as_str().parse::<u32>().unwrap();
                let action_str = cond_caps.get(4).unwrap().as_str();
                let predicate = match test {
                    "<" => Predicate::LessThan(rating.to_string(), value),
                    ">" => Predicate::GreaterThan(rating.to_string(), value),
                    _ => unreachable!(),
                };
                let action = match action_str {
                    "A" => Action::Accept,
                    "R" => Action::Reject,
                    _ => Action::Goto(action_str.to_string()),
                };
                let rule = (predicate, action);
                workflow.push(rule);
            } else {
                let predicate = Predicate::Unconditional;
                let action = match rule {
                    "A" => Action::Accept,
                    "R" => Action::Reject,
                    _ => Action::Goto(rule.to_string()),
                };
                let rule = (predicate, action);
                workflow.push(rule);
            }
        }
        flows.insert(flow_id.to_string(), workflow);
    }
    let part_re = Regex::new(r"\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}").unwrap();
    for line in lineptr {
        let part_caps = part_re.captures(line).unwrap();
        let x = part_caps.get(1).unwrap().as_str().parse::<u32>().unwrap();
        let m = part_caps.get(2).unwrap().as_str().parse::<u32>().unwrap();
        let a = part_caps.get(3).unwrap().as_str().parse::<u32>().unwrap();
        let s = part_caps.get(4).unwrap().as_str().parse::<u32>().unwrap();
        parts.push(Part { x, m, a, s });
    }
    (flows, parts)
}

fn accept(part: &Part, flows: &HashMap<String, Workflow>) -> bool {
    let mut curr_label = String::from("in");
    loop {
        let workflow = flows.get(&curr_label).unwrap();
        for rule in workflow.iter() {
            let predicate = &rule.0;
            let action = &rule.1;
            let cond_true = match predicate {
                Predicate::LessThan(rating, value) => match rating.as_str() {
                    "x" => part.x < *value,
                    "m" => part.m < *value,
                    "a" => part.a < *value,
                    "s" => part.s < *value,
                    _ => unreachable!(),
                },
                Predicate::GreaterThan(rating, value) => match rating.as_str() {
                    "x" => part.x > *value,
                    "m" => part.m > *value,
                    "a" => part.a > *value,
                    "s" => part.s > *value,
                    _ => unreachable!(),
                },
                Predicate::Unconditional => true,
            };
            if cond_true {
                match action {
                    Action::Accept => {
                        return true;
                    }
                    Action::Reject => {
                        return false;
                    }
                    Action::Goto(label) => {
                        curr_label = label.clone();
                        break;
                    }
                }
            }
        }
    }
}

fn process(contents: &str) -> u32 {
    let (flows, parts) = parse_input(contents);
    let mut rating_sum: u32 = 0;
    for part in parts.iter() {
        if accept(part, &flows) {
            rating_sum += part.add_ratings();
        }
    }
    rating_sum
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
