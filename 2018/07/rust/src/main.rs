use regex::Regex;
use std::collections::{HashMap, HashSet};

fn parse(input: String) -> Vec<(char, HashSet<char>)> {
    let r = Regex::new(r"Step ([A-Z]) must be finished before step ([A-Z]) can begin.").unwrap();

    // keep track of source/destinations so we can calculate the starting points
    let mut sources = HashSet::new();
    let mut destinations = HashSet::new();

    // a map where keys are a node, and values a hashset of required previous nodes
    let mut map = HashMap::<char, HashSet<char>>::new();

    for line in input.lines() {
        let c = r.captures(line).unwrap();
        let l = c[1].chars().next().unwrap();
        let r = c[2].chars().next().unwrap();
        map.entry(r)
            .and_modify(|hs| {
                hs.insert(l);
            })
            .or_insert(HashSet::from([l]));
        sources.insert(l);
        destinations.insert(r);
    }

    for init in sources.difference(&destinations) {
        map.insert(*init, HashSet::from(['@']));
    }

    // convert the hashmap into an ordered vector of tuples
    let mut res: Vec<(char, HashSet<char>)> = map.into_iter().collect();
    res.sort_by_key(|k| k.0);
    res
}

fn p1(mut map: Vec<(char, HashSet<char>)>) -> String {
    let mut res = Vec::from(['@']);
    while !map.is_empty() {
        let idx = map
            .iter()
            .position(|(_, pre)| pre.iter().all(|n| res.contains(n)))
            .unwrap();
        res.push(map[idx].0);
        map.remove(idx);
    }
    res.iter().skip(1).collect()
}

fn p2(mut map: Vec<(char, HashSet<char>)>, offset: u8, n_workers: usize) -> usize {
    let mut res = Vec::from(['@']);
    let mut workers: Vec<Option<(char, u8)>> = vec![None; n_workers];
    let mut time = 0;

    while !map.is_empty() || workers.iter().any(|w| w.is_some()) {
        // first, assign any idle workers to a node
        for w in &mut workers {
            if w.is_none() {
                if let Some(idx) = map
                    .iter()
                    .position(|(_, pre)| pre.iter().all(|n| res.contains(n)))
                {
                    let node = map[idx].0;
                    let ascii: u8 = node.try_into().unwrap();
                    *w = Some((node, ascii - 64 + offset));
                    map.remove(idx);
                }
            }
        }

        // next, perform one unit of work for each worker
        // if we hit zero, push to completed and mark as idle
        for w in &mut workers {
            if let Some((node, seconds)) = w {
                *seconds -= 1;
                if seconds == &0 {
                    res.push(*node);
                    *w = None;
                }
            }
        }

        time += 1;
    }
    time
}

fn main() {
    let input = std::fs::read_to_string("../input.txt").expect("Unable to read file");
    let map = parse(input);
    println!("Part 1 answer: {}", p1(map.clone()));
    println!("Part 2 answer: {}", p2(map, 60, 5));
}
