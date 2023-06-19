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
    intcode.run().ok();
    println!("Part 1 answer: {:?}", intcode.output());

    let mut intcode = Intcode::new(input.clone());
    intcode.silent = true;
    intcode.input = vec![5];
    intcode.run().ok();
    println!("Part 2 answer: {:?}", intcode.output());
}
