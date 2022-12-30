use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::cmp::{max,min};

fn main() {
    let input = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let mut cities = HashSet::new();

    let distances: HashMap<(&str, &str), usize> = input
        .lines()
        .map(|line| {
            let s: Vec<&str> = line.split(" ").collect();
            cities.insert(s[0]);
            cities.insert(s[2]);
            ((s[0], s[2]), s[4].parse().unwrap())
        })
        .collect();

    let mut p1_ans = usize::MAX;
    let mut p2_ans = usize::MIN;

    for perm in cities.iter().permutations(cities.iter().len()) {
        let total_dist: usize =
            perm.iter()
                .tuple_windows()
                .map(|(start, end)| match distances.get(&(start, end)) {
                    Some(dist) => dist,
                    None => distances.get(&(end, start)).unwrap(),
                }).sum();

        p1_ans = min(p1_ans, total_dist);
        p2_ans = max(p2_ans, total_dist);
    }

    println!("Part 1 answer: {}", p1_ans);
    println!("Part 2 answer: {}", p2_ans);
}
