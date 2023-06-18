use intcode::Intcode;

fn main() {
    //let input : Vec<i64>= vec![109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99];
    //let input : Vec<i64>= vec![1102,34915192,34915192,7,4,7,99,0];
    //let input : Vec<i64>= vec![104,1125899906842624,99];

    let input_str = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let input: Vec<i64> = input_str
        .trim()
        .split(",")
        .map(|x| x.parse::<i64>().unwrap())
        .collect();

    let mut intcode = Intcode::new_simulation(input.clone(), vec![1], true);
    let p1_ans = intcode.run_until_output().unwrap();

    let mut intcode = Intcode::new_simulation(input, vec![2], true);
    let p2_ans = intcode.run_until_output().unwrap();

    println!("Part 1 answer: {:?}", p1_ans);
    println!("Part 2 answer: {:?}", p2_ans);
}
