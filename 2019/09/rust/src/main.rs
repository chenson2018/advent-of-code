use intcode::Intcode;

fn main() {
    let input_str = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let input: Vec<i64> = input_str
        .trim()
        .split(",")
        .map(|x| x.parse::<i64>().unwrap())
        .collect();

    let mut intcode = Intcode::new(input.clone());
    intcode.silent = true;
    intcode.input = vec![1];
    let p1_ans = intcode.run_until_output().unwrap();

    let mut intcode = Intcode::new(input.clone());
    intcode.silent = true;
    intcode.input = vec![2];
    let p2_ans = intcode.run_until_output().unwrap();

    println!("Part 1 answer: {:?}", p1_ans);
    println!("Part 2 answer: {:?}", p2_ans);
}
