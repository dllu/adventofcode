use std::collections::{HashMap, VecDeque};
use std::env;
use std::fs;
use std::process;

const COUNT: u32 = 1000;

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

fn push_button(components: &mut HashMap<String, Component>, count: u32) -> u32 {
    let mut low_count: u32 = 0;
    let mut high_count: u32 = 0;
    let mut queues: HashMap<String, VecDeque<Message>> = HashMap::new();
    for label in components.keys() {
        queues.insert(label.clone(), VecDeque::new());
    }
    for _ in 0..count {
        let current: String = String::from("broadcaster");
        queues.entry(current).and_modify(|e| {
            e.push_back(Message {
                source: String::from("button"),
                pulse: Pulse::Low,
            })
        });
        low_count += 1;
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
                                match message.pulse {
                                    Pulse::Low => low_count += 1,
                                    Pulse::High => high_count += 1,
                                };
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
                                            low_count += 1;
                                        } else {
                                            target_queue.push_back(Message {
                                                source: label.clone(),
                                                pulse: Pulse::High,
                                            });
                                            high_count += 1;
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
                                    low_count += 1;
                                } else {
                                    target_queue.push_back(Message {
                                        source: label.clone(),
                                        pulse: Pulse::High,
                                    });
                                    high_count += 1;
                                }
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
    }
    low_count * high_count
}

fn process(contents: &str) -> u32 {
    let mut components = build_components(contents);
    push_button(&mut components, COUNT)
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
