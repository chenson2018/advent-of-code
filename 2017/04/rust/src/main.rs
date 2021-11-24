use std::fs;
use itertools::Itertools;

fn main() {
  let input_s = fs::read_to_string("../input.txt").expect("Unable to read file");
  
  let input_vv: Vec<Vec<String>> = input_s.lines()
      .map(|s| s.split_whitespace()
                .filter_map(|word| word.parse().ok() )
                .collect())
      .collect();

  let p1_ans = input_vv.iter()
                 .map(|v| {
                   ( v.len() == 
                     v.into_iter().unique().collect::<Vec<&String>>().len() 
                   ) as usize 
                  })
                 .collect::<Vec<usize>>()
                 .iter().sum::<usize>();

  println!("Part 1 answer: {:?}", p1_ans);
}
