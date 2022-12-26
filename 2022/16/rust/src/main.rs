use pathfinding::prelude::dijkstra;
use regex::Regex;
use std::collections::{BTreeSet, HashMap};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct Valve {
    rate: usize,
    name: usize,
    connected: BTreeSet<usize>,
}

// map names into usizes so I can have a type with Copy for easier searching later

fn parse_input(input: String) -> Vec<Valve> {
    let regex_valve =
        Regex::new(r"Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? (.*)")
            .unwrap();

    let mut res: Vec<Valve> = Vec::new();
    let mut name_mapping: HashMap<String, usize> = HashMap::new();
    let mut number: usize = 0;

    for line in input.lines() {
        let captures = regex_valve.captures(line).unwrap();
        let name_str = captures.get(1).map_or("", |m| m.as_str());
        let connected_str: Vec<&str> = captures
            .get(3)
            .map_or(vec![], |m| m.as_str().split(", ").collect());

        if !name_mapping.contains_key(&name_str.to_string()) {
            name_mapping.insert(name_str.to_string(), number);
            number += 1;
        }

        for name in &connected_str {
            if !name_mapping.contains_key(&name.to_string()) {
                name_mapping.insert(name.to_string(), number);
                number += 1;
            }
        }

        let rate: usize = captures.get(2).map_or(0, |m| m.as_str().parse().unwrap());
        let name = *name_mapping.get(&name_str.to_string()).unwrap();
        let connected: BTreeSet<usize> = connected_str
            .iter()
            .map(|s| *name_mapping.get(&s.to_string()).unwrap())
            .collect();

        res.push(Valve {
            rate,
            name,
            connected,
        });
    }
    res
}

// adding extra nodes that represent state of opening
fn transform(map: Vec<Valve>) -> Vec<Valve> {
    let mut res: Vec<Valve> = Vec::new();
    let mut number = map.iter().len();

    for valve in map {
        if valve.rate > 0 {
            let valve = valve.clone();

            // first, a copy with 0 rate and connect to XX*
            let mut connected = valve.connected.clone();
            connected.insert(number);
            res.push(Valve {
                rate: 0,
                connected,
                ..valve
            });

            // then a copy with name XX*
            res.push(Valve {
                name: number,
                ..valve
            });

            number += 1;
        } else {
            res.push(Valve { rate: 0, ..valve });
        }
    }
    res
}

// assumes that start point is first room in list, I changed order of input to match
fn part1(transformed_graph: &Vec<Valve>, min: usize) -> usize {
    let score_offset = 1000;

    // each coordinate is (node,time,open valves)
    let (path, _): (Vec<(Valve, usize, BTreeSet<usize>)>, usize) = dijkstra(
        &(transformed_graph[0].clone(), 0, BTreeSet::new()),
        |(node, time, visited): &(Valve, usize, BTreeSet<usize>)| {
            let mut next_visited = visited.clone();
            if node.rate > 0 {
                next_visited.insert(node.name);
            }

            let next_time = time + 1;

            transformed_graph
                .iter()
                .cloned()
                .filter(|m| node.connected.contains(&m.name) && !visited.contains(&m.name))
                .map(|v| {
                    (
                        (v.clone(), next_time, next_visited.clone()),
                        score_offset - ((30 - next_time) * v.rate),
                    )
                })
                .collect::<Vec<((Valve, usize, BTreeSet<usize>), usize)>>()
        },
        |(_, time, _)| *time == min,
    )
    .unwrap();

    path.iter()
        .fold(0, |acc, (valve, time, _)| acc + (min - time) * valve.rate)
}

fn main() {
    //let input = std::fs::read_to_string("../test.txt").expect("Unable to read file");
    let input = std::fs::read_to_string("../input.txt").expect("Unable to read file");
    let init_graph = parse_input(input);
    let transformed_graph = transform(init_graph);

    let p1_ans = part1(&transformed_graph, 30);
    println!("Part 1 answer: {}", p1_ans);
}
