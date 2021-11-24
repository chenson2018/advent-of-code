use std::fs;

fn count_steps (input_static: &Vec<i64>, problem: &str) -> i64 {
  // make a clone so we only have to read the file one time
  // could probably be an array since it is fixed length...
  let mut input = input_static.clone();

  let mut idx: i64 = 0;
  let mut idx_prev: i64;
  let mut count: i64 = 0;

  // we stop when our last jump takes us out of the vector
  let max_idx = input.len().try_into().unwrap();

  while idx < max_idx {
    // store current location so we can increment it later
    idx_prev = idx;

    // move pointer forward
    idx += input[idx as usize] as i64;

    // increment the index we just left
    input[idx_prev as usize] += match problem {
      "p1" => 1,
      "p2" => if input[idx_prev as usize] >= 3 { -1 } else { 1 },
       _   => panic!("Unexpected argument: {:?}", problem),    
    };
    count += 1;
  }
  count
}

fn main() {
  let input: Vec<i64> = fs::read_to_string("../input.txt")
                              .expect("Unable to read file")
                              .lines()
                              .filter_map(|s| s.parse().ok() )
                              .collect();

  let p1_ans = count_steps(&input, "p1");
  println!("Part 1 answer: {:?}", p1_ans);

  let p2_ans = count_steps(&input, "p2");
  println!("Part 2 answer: {:?}", p2_ans);

}
