use std::fs;

// compare two characters
// if equal, return the first as an int
fn compare(n1: char, n2:char) -> u64 {
  const RADIX: u32 = 10;
  if n1 == n2 {
    return n1.to_digit(RADIX).unwrap() as u64;
  } else {
   return 0;
  }
}

// takes a vector and an offset
// zips the appropriate slices and sum matches
fn matches (v: &Vec<char>, offset: usize) -> u64 {
  let comp = [&v[offset..], &v[..offset]].concat();
  let comp_zip: Vec<u64> = v[..]
                   .iter()
                   .zip(comp.iter())
                   .map(|w| compare(*w.0, *w.1))
                   .collect();
  comp_zip.iter().sum()
}

fn main() {
  // read input and remove newline
  let mut input_s = fs::read_to_string("../input.txt").expect("Unable to read file");
  input_s = input_s.trim().to_string(); 

  let input_v: Vec<char> = input_s.chars().collect(); 

  let p1_ans: u64 = matches(&input_v, 1);
  let p2_ans: u64 = matches(&input_v, 1095);

  println!("Part 1 answer: {:?}", p1_ans);
  println!("Part 2 answer: {:?}", p2_ans);
}
