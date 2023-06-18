use intcode::Intcode;

fn main() {
    let input_str = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let input: Vec<i64> = input_str
        .trim()
        .split(",")
        .map(|x| x.parse::<i64>().unwrap())
        .collect();

    let mut intcode = Intcode::new_simulation(input.clone(), vec![1], true);
    intcode.run().ok();
    println!("Part 1 answer: {:?}", intcode.output());

    let mut intcode = Intcode::new_simulation(input.clone(), vec![5], true);
    intcode.run().ok();
    println!("Part 2 answer: {:?}", intcode.output());
}
