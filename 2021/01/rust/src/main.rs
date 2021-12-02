use std::fs;

fn rolling (v: &Vec<i64>, offset: usize) -> Vec<i64> {
  let comp = [&v[offset..], &v[..offset]].concat();
  v[..]
    .iter()
    .zip(comp.iter())
    .map(|w| *w.1 - *w.0)
    .collect()
}

fn main() {
  let input: Vec<i64> = fs::read_to_string("../input.txt")
                              .expect("Unable to read file")
                              .lines()
                              .filter_map(|s| s.parse().ok() )
                              .collect();

  let p1_ans: i64 = rolling(&input, 1)
                      .iter()
                      .map(|x| (x > &0) as i64)
                      .collect::<Vec<i64>>()
                      .iter()
                      .sum();

  let p2_ans: i64 = rolling(&input, 3)
                      .iter()
                      .map(|x| (x > &0) as i64)
                      .collect::<Vec<i64>>()
                      .iter()
                      .sum();

  println!("Part 1 answer: {:?}", p1_ans);
  println!("Part 2 answer: {:?}", p2_ans);

}
