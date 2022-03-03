use std::fs;

fn p1(s: &str) -> u32 {
  let mut iter = s.chars();
  let mut count: u32 = 0;

  loop {
    // stop if we reach end of encoding
    let val = match iter.next() {
      None       => return count,
      Some('\n') => return count,
      Some(c)    => c,
    };
    // increment by one for a single letter
    if val.is_ascii_uppercase() {
      count = count + 1;
    } else {
      // note that these consume one past the actual digits, which in this case is helpful
      let n_char: u32 = iter.by_ref().take_while(|x| x.is_digit(10)).collect::<String>().parse::<u32>().unwrap();
      let rep   : u32 = iter.by_ref().take_while(|x| x.is_digit(10)).collect::<String>().parse::<u32>().unwrap();
      // increment by the repeated sequence, then skip over it
      count = count + (n_char * rep);
      iter.nth((n_char - 1) as usize);
    }
  }
}


fn main() {
  let input = fs::read_to_string("../input.txt").expect("Unable to read file");
  let p1_ans = p1(&input);
  println!("Part 1 answer: {:?}", p1_ans);
}
