use regex::Regex;
use std::collections::{HashMap, VecDeque};
use std::env;
use std::fs;
use std::process;

#[derive(Clone, Debug)]
enum Predicate {
    LessThan(String, u32),
    GreaterThan(String, u32),
    Unconditional,
}

#[derive(Clone, Debug, PartialEq)]
enum Action {
    Accept,
    Reject,
    Goto(String),
}

type Rule = (Predicate, Action);

type Workflow = Vec<Rule>;

#[derive(Clone, Copy, Debug)]
struct Interval {
    from: u32,
    to: u32,
}

#[derive(Clone, Debug)]
struct State {
    label: String,
    ranges: HashMap<String, Interval>,
}

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn parse_input(contents: &str) -> HashMap<String, Workflow> {
    let mut flows: HashMap<String, Workflow> = HashMap::new();
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
    flows
}

fn scan_ranges(flows: &HashMap<String, Workflow>) -> u64 {
    let init_state = State {
        label: String::from("in"),
        ranges: HashMap::from([
            (String::from("x"), Interval { from: 1, to: 4000 }),
            (String::from("m"), Interval { from: 1, to: 4000 }),
            (String::from("a"), Interval { from: 1, to: 4000 }),
            (String::from("s"), Interval { from: 1, to: 4000 }),
        ]),
    };
    let mut q: VecDeque<State> = VecDeque::new();
    let mut accepted: Vec<State> = vec![];
    q.push_back(init_state);
    while !q.is_empty() {
        let state = q.pop_front().unwrap();
        let rules = flows.get(&state.label).unwrap();
        let mut working_state = state.clone();
        for rule in rules.iter() {
            match rule {
                (Predicate::Unconditional, Action::Reject) => {
                    // reject this state, so do nothing
                }
                (Predicate::Unconditional, Action::Accept) => {
                    // add this state to the accepted list
                    accepted.push(working_state.clone());
                }
                (Predicate::Unconditional, Action::Goto(label)) => {
                    // test the current working state on the next node
                    working_state.label = label.clone();
                    q.push_back(working_state.clone());
                }
                (Predicate::LessThan(rating, amount), Action::Reject) => {
                    // split the range and continue with the non-rejected part
                    working_state
                        .ranges
                        .entry(rating.clone())
                        .and_modify(|e| e.from = *amount);
                }
                (Predicate::LessThan(rating, amount), Action::Accept) => {
                    // split the range, accept the less than part, continue
                    let mut accepted_state = working_state.clone();
                    accepted_state
                        .ranges
                        .entry(rating.clone())
                        .and_modify(|e| e.to = *amount - 1);
                    accepted.push(accepted_state);
                    working_state
                        .ranges
                        .entry(rating.clone())
                        .and_modify(|e| e.from = *amount);
                }
                (Predicate::LessThan(rating, amount), Action::Goto(label)) => {
                    //split the range, test on the less than part at the new node, continue
                    let mut goto_state = working_state.clone();
                    goto_state
                        .ranges
                        .entry(rating.clone())
                        .and_modify(|e| e.to = *amount - 1);
                    goto_state.label = label.clone();
                    q.push_back(goto_state);
                    working_state
                        .ranges
                        .entry(rating.clone())
                        .and_modify(|e| e.from = *amount);
                }
                (Predicate::GreaterThan(rating, amount), Action::Reject) => {
                    // split the range and continue with the non-rejected part
                    working_state
                        .ranges
                        .entry(rating.clone())
                        .and_modify(|e| e.to = *amount);
                }
                (Predicate::GreaterThan(rating, amount), Action::Accept) => {
                    // split the range, accept the greater than part, continue
                    let mut accepted_state = working_state.clone();
                    accepted_state
                        .ranges
                        .entry(rating.clone())
                        .and_modify(|e| e.from = *amount + 1);
                    accepted.push(accepted_state);
                    working_state
                        .ranges
                        .entry(rating.clone())
                        .and_modify(|e| e.to = *amount);
                }
                (Predicate::GreaterThan(rating, amount), Action::Goto(label)) => {
                    // split the range, test on the greater than part at the new node, continue
                    let mut goto_state = working_state.clone();
                    goto_state
                        .ranges
                        .entry(rating.clone())
                        .and_modify(|e| e.from = *amount + 1);
                    goto_state.label = label.clone();
                    q.push_back(goto_state);
                    working_state
                        .ranges
                        .entry(rating.clone())
                        .and_modify(|e| e.to = *amount);
                }
            }
        }
    }
    let mut result: u64 = 0;
    for state in accepted.iter() {
        let x_range = state.ranges.get(&String::from("x")).unwrap();
        let m_range = state.ranges.get(&String::from("m")).unwrap();
        let a_range = state.ranges.get(&String::from("a")).unwrap();
        let s_range = state.ranges.get(&String::from("s")).unwrap();
        result += (x_range.to - x_range.from + 1) as u64 *
            (m_range.to - m_range.from + 1) as u64 *
            (a_range.to - a_range.from + 1) as u64 *
            (s_range.to - s_range.from + 1) as u64;
    }
    result
}

fn process(contents: &str) -> u64 {
    let flows = parse_input(contents);
    scan_ranges(&flows)
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
