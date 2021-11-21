use std::fs;
use itertools::Itertools;

fn main() {
  let input_s = fs::read_to_string("../input.txt").expect("Unable to read file");

  let input_vv: Vec<Vec<i32>> = input_s.lines()
      .map(|s| s.split_whitespace()
                .filter_map(|word| word.parse().ok())
                .collect())
      .collect();

  
  let checksums: Vec<i32> = input_vv
                              .iter()
                              .map(|x| *x.iter().max().unwrap() - *x.iter().min().unwrap() )
                              .collect();

  let p1_ans: i32 = checksums.iter().sum();
  println!("Part 1 answer: {:?}", p1_ans);

  let divisors: Vec<i32> = input_vv
               .iter()
               .map(|v| v.iter()
                         .tuple_combinations()
                         .filter(|(a,b)| a.rem_euclid(**b) == 0 || b.rem_euclid(**a) == 0)
                         .map(|(a,b)| (a / b).max(b / a))
                         .sum())
               .collect();

  let p2_ans: i32 = divisors.iter().sum();
  println!("Part 2 answer: {:?}", p2_ans);
}
