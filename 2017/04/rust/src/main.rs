use std::fs;
use itertools::Itertools;

fn count_unique(input_s: &str, problem: &str) -> usize {
  let input: Vec<Vec<String>> = input_s.lines()
      .map(|s: &str| s.split_whitespace()
                .map(|s: &str|
                  match problem {
                    "p1" => s.chars()         .collect(),
                    "p2" => s.chars().sorted().collect(),
                     _   => panic!("Unexpected argument: {:?}", problem),
                 })
                .collect())
      .collect();

  input
    .iter()
    .map(|v| {
      ( v.len() == 
        v.into_iter().unique().collect::<Vec<&String>>().len() 
      ) as usize 
     })
    .collect::<Vec<usize>>()
    .iter().sum::<usize>()
}

fn main() {
  let input_s = fs::read_to_string("../input.txt").expect("Unable to read file");

  let p1_ans = count_unique(&input_s, "p1");
  println!("Part 1 answer: {:?}", p1_ans);

  let p2_ans = count_unique(&input_s, "p2");
  println!("Part 2 answer: {:?}", p2_ans);
}
