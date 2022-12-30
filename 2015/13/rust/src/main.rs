use itertools::Itertools;
use std::cmp::max;
use std::collections::HashMap;
use wrapping::Wrapping;

enum Part {
    Part1,
    Part2,
}

fn optimal_diff(happiness: &HashMap<(&str, &str), isize>, part: Part) -> isize {
    let mut people: Vec<&str> = happiness
        .keys()
        .fold(Vec::new(), |mut vec, c| {
            vec.push(c.0);
            vec.push(c.1);
            vec
        })
        .into_iter()
        .unique()
        .collect();

    match part {
        Part::Part1 => (),
        Part::Part2 => {
            people.push("self");
        }
    };

    let n_people = people.iter().len();
    let mut res = isize::MIN;

    for perm in people.iter().permutations(n_people) {
        let wrapped = Wrapping::from(&perm[..]);

        let happiness_diff: isize = (0..n_people)
            .map(|i| {
                let left = wrapped[i];
                let right = wrapped[i + 1];

                happiness.get(&(left, right)).unwrap_or(&0)
                    + happiness.get(&(right, left)).unwrap_or(&0)
            })
            .sum();

        res = max(res, happiness_diff);
    }

    res
}

fn main() {
    let input = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let happiness: HashMap<(&str, &str), isize> = input
        .lines()
        .map(|line| {
            let s: Vec<&str> = line.split(&[' ', '.']).collect();
            let sign = if s[2] == "gain" { 1 } else { -1 };
            ((s[0], s[10]), sign * s[3].parse::<isize>().unwrap())
        })
        .collect();

    let p1_ans = optimal_diff(&happiness, Part::Part1);
    println!("Part 1 answer: {}", p1_ans);

    let p2_ans = optimal_diff(&happiness, Part::Part2);
    println!("Part 2 answer: {}", p2_ans);
}
