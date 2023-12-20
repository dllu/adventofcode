use std::cmp;
use std::collections::{HashMap, VecDeque};
use std::env;
use std::fs;
use std::process;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Pulse {
    High,
    Low,
}

#[derive(Clone, Debug, PartialEq)]
struct Message {
    source: String,
    pulse: Pulse,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum ComponentType {
    Broadcast,
    Conjunction,
    FlipFlop,
    Sink,
}

#[derive(Clone, Debug)]
struct Component {
    kind: ComponentType,
    targets: Vec<String>,
    memory: Option<HashMap<String, Pulse>>,
    enabled: Option<bool>,
}

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn gcd(a: u64, b: u64) -> u64 {
    match ((a, b), (a & 1, b & 1)) {
        ((x, y), _) if x == y => y,
        ((0, x), _) | ((x, 0), _) => x,
        ((x, y), (0, 1)) | ((y, x), (1, 0)) => gcd(x >> 1, y),
        ((x, y), (0, 0)) => gcd(x >> 1, y >> 1) << 1,
        ((x, y), (1, 1)) => {
            let (x, y) = (cmp::min(x, y), cmp::max(x, y));
            gcd((y - x) >> 1, x)
        }
        _ => unreachable!(),
    }
}

fn lcm(a: u64, b: u64) -> u64 {
    a * b / gcd(a, b)
}

fn build_components(contents: &str) -> HashMap<String, Component> {
    let mut components: HashMap<String, Component> = HashMap::new();
    for line in contents.lines() {
        let (source_part, dest_part) = line.split_once(" -> ").unwrap();
        match &source_part[0..1] {
            "%" => {
                let name = String::from(&source_part[1..]);
                let component = Component {
                    kind: ComponentType::FlipFlop,
                    targets: dest_part
                        .split(", ")
                        .map(|s| s.to_string())
                        .collect::<Vec<String>>(),
                    memory: None,
                    enabled: Some(false),
                };
                components.insert(name, component);
            }
            "&" => {
                let name = String::from(&source_part[1..]);
                let component = Component {
                    kind: ComponentType::Conjunction,
                    targets: dest_part
                        .split(", ")
                        .map(|s| s.to_string())
                        .collect::<Vec<String>>(),
                    memory: Some(HashMap::new()),
                    enabled: None,
                };
                components.insert(name, component);
            }
            _ => {
                if source_part != "broadcaster" {
                    panic!("unknown source module type!")
                } else {
                    let name = String::from(source_part);
                    let component = Component {
                        kind: ComponentType::Broadcast,
                        targets: dest_part
                            .split(", ")
                            .map(|s| s.to_string())
                            .collect::<Vec<String>>(),
                        memory: None,
                        enabled: None,
                    };
                    components.insert(name, component);
                }
            }
        }
    }
    let conjunctions: Vec<String> = components
        .iter()
        .filter(|(_, v)| v.kind == ComponentType::Conjunction)
        .map(|(k, _)| k)
        .cloned()
        .collect();
    let components_clone = components.clone();
    for conjunction in conjunctions.iter() {
        let sources: Vec<String> = components_clone
            .iter()
            .filter(|(_, v)| v.targets.contains(conjunction))
            .map(|(k, _)| k)
            .cloned()
            .collect();
        for source in sources.iter() {
            components.entry(conjunction.clone()).and_modify(|e| {
                e.memory
                    .as_mut()
                    .unwrap()
                    .insert(source.clone(), Pulse::Low);
            });
        }
    }
    for component in components_clone.values() {
        if component
            .targets
            .iter()
            .any(|t| !components_clone.contains_key(t))
        {
            for sink in component
                .targets
                .iter()
                .filter(|&t| !components_clone.contains_key(t))
            {
                components.insert(
                    sink.clone(),
                    Component {
                        kind: ComponentType::Sink,
                        targets: vec![],
                        memory: None,
                        enabled: None,
                    },
                );
            }
        }
    }
    components
}

fn push_button(components: &mut HashMap<String, Component>) -> u64 {
    let mut queues: HashMap<String, VecDeque<Message>> = HashMap::new();
    for label in components.keys() {
        queues.insert(label.clone(), VecDeque::new());
    }
    let mut loop_count: u64 = 0;
    let mut rx_high_counts: HashMap<String, u64> = HashMap::new();
    let rx_parent = components.iter().filter(|(_, v)| v.targets.contains(&String::from("rx"))).map(|(k, _)| k).next().unwrap().clone();
    let rx_num_inputs = components
        .get(&rx_parent)
        .unwrap()
        .memory
        .as_ref()
        .unwrap()
        .keys()
        .count();
    loop {
        loop_count += 1;
        let current: String = String::from("broadcaster");
        queues.entry(current).and_modify(|e| {
            e.push_back(Message {
                source: String::from("button"),
                pulse: Pulse::Low,
            })
        });
        while queues.values().any(|v| !v.is_empty()) {
            for label in queues
                .clone()
                .iter()
                .filter(|(_, v)| !v.is_empty())
                .map(|(k, _)| k)
            {
                let component = components.get_mut(label).unwrap();
                match component.kind {
                    ComponentType::Broadcast => {
                        while !queues.get(label).unwrap().is_empty() {
                            let message = queues.get_mut(label).unwrap().pop_front().unwrap();
                            for target in component.targets.iter() {
                                queues.get_mut(target).unwrap().push_back(Message {
                                    source: label.clone(),
                                    pulse: message.pulse,
                                });
                            }
                        }
                    }
                    ComponentType::FlipFlop => {
                        while !queues.get(label).unwrap().is_empty() {
                            let message = queues.get_mut(label).unwrap().pop_front().unwrap();
                            match message.pulse {
                                Pulse::Low => {
                                    let enabled = component.enabled.unwrap();
                                    for target in component.targets.iter() {
                                        let target_queue = queues.get_mut(target).unwrap();
                                        if enabled {
                                            target_queue.push_back(Message {
                                                source: label.clone(),
                                                pulse: Pulse::Low,
                                            });
                                        } else {
                                            target_queue.push_back(Message {
                                                source: label.clone(),
                                                pulse: Pulse::High,
                                            });
                                        }
                                    }
                                    component.enabled = Some(!enabled);
                                }
                                Pulse::High => {
                                    // ignored
                                }
                            }
                        }
                    }
                    ComponentType::Conjunction => {
                        while !queues.get(label).unwrap().is_empty() {
                            let message = queues.get_mut(label).unwrap().pop_front().unwrap();
                            let source = message.source.clone();
                            component
                                .memory
                                .as_mut()
                                .unwrap()
                                .entry(message.source)
                                .and_modify(|p| *p = message.pulse);
                            for target in component.targets.iter() {
                                let target_queue = queues.get_mut(target).unwrap();
                                if component
                                    .memory
                                    .as_mut()
                                    .unwrap()
                                    .values()
                                    .all(|p| *p == Pulse::High)
                                {
                                    target_queue.push_back(Message {
                                        source: label.clone(),
                                        pulse: Pulse::Low,
                                    });
                                } else {
                                    target_queue.push_back(Message {
                                        source: label.clone(),
                                        pulse: Pulse::High,
                                    });
                                }
                            }
                            if label.clone() == rx_parent
                                && message.pulse == Pulse::High
                                && !rx_high_counts.contains_key(&source)
                            {
                                rx_high_counts.insert(source, loop_count);
                            }
                        }
                    }
                    ComponentType::Sink => {
                        // does nothing but absorbs messages, used to represent "output" states
                        // clean out any messages that are sitting in the queue
                        while !queues.get(label).unwrap().is_empty() {
                            queues.get_mut(label).unwrap().pop_front().unwrap();
                        }
                    }
                };
            }
        }
        if rx_high_counts.keys().count() == rx_num_inputs {
            break;
        }
    }
    rx_high_counts.values().cloned().reduce(lcm).unwrap()
}

fn process(contents: &str) -> u64 {
    let mut components = build_components(contents);
    push_button(&mut components)
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
