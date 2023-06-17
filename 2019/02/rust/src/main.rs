use intcode::Intcode;

fn main() {
    let input_str = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let input: Vec<i64> = input_str
        .trim()
        .split(",")
        .map(|x| x.parse::<i64>().unwrap())
        .collect();

    let mut intcode = Intcode::new(input.clone());

    intcode.replace(1, 12).ok();
    intcode.replace(2, 2).ok();

    intcode.run().ok();

    println!("Part 1 answer: {:?}", intcode.read_mem(0).unwrap());

    for noun in 0..100 {
        for verb in 0..100 {
            let mut intcode = Intcode::new(input.clone());
            intcode.replace(1, noun).ok();
            intcode.replace(2, verb).ok();
            intcode.run().ok();
            
            if intcode.read_mem(0) == Ok(19690720) {
                println!("Part 2 answer: {}", 100 * noun + verb);
                break;
            }

        }
    }
}
