use regex::Regex;
use std::collections::HashMap;

fn main() {
    let file = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let regex_cd = Regex::new(r"[$] cd (.*)").unwrap();
    let regex_fl = Regex::new(r"(\d+) (.*)").unwrap();

    let lines: Vec<(String, usize)> = file
        .lines()
        .filter_map(|line| {
            if regex_fl.is_match(line) {
                let captures = regex_fl.captures(line).unwrap();
                let size: usize = captures.get(1).map_or(0, |m| m.as_str().parse().unwrap());
                let name = captures.get(2).map_or("", |m| m.as_str());
                Some((name.to_string(), size))
            } else if regex_cd.is_match(line) {
                let captures = regex_cd.captures(line).unwrap();
                let name = captures.get(1).map_or("", |m| m.as_str());
                Some((name.to_string(), 0))
            } else {
                None
            }
        })
        .collect();

    let mut sizes: HashMap<Vec<String>, usize> = HashMap::new();

    let _ = lines
        .iter()
        .scan(Vec::new(), |state, (name, size)| {
            if *size == 0 && name == ".." {
                state.pop();
            }
            if *size == 0 && name != ".." {
                state.push(name.clone());
            }

            let subdirs: Vec<Vec<String>> = state
                .iter()
                .scan(Vec::new(), |state, val| {
                    state.push(val.clone());
                    Some(state.clone())
                })
                .collect();

            for dir in subdirs {
                sizes.entry(dir).and_modify(|s| *s += size).or_insert(*size);
            }

            Some(state.clone())
        })
        .collect::<Vec<Vec<String>>>();

    let p1_ans: usize = sizes.values().cloned().filter(|x| *x <= 100000).sum();

    println!("Part 1 answer: {:?}", p1_ans);

    let total_space = 70000000;
    let required = 30000000;
    let used = sizes.get(&vec!["/".to_string()]).unwrap();
    let must_free = required - (total_space - used);

    let p2_ans: usize = sizes
        .values()
        .cloned()
        .filter(|x| *x >= must_free)
        .min()
        .unwrap();

    println!("Part 2 answer: {:?}", p2_ans);
}
