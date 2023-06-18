use intcode::Intcode;
use itertools::Itertools;

fn main() {
    let input_str = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let input: Vec<i64> = input_str
        .trim()
        .split(",")
        .map(|x| x.parse::<i64>().unwrap())
        .collect();

    let p1_ans: i64 = [0_i64, 1_i64, 2_i64, 3_i64, 4_i64]
        .iter()
        .permutations(5)
        .map(|perm| {
            let mut amp = 0;
            let mut thruster_signals: Vec<i64> = Vec::new();

            for phase in perm {
                let mut intcode = Intcode::new_simulation(input.clone(), vec![amp, *phase], true);
                intcode.run().ok();
                amp = intcode.output().pop().unwrap();
                thruster_signals.push(amp);
            }

            thruster_signals.pop().unwrap()
        })
        .max()
        .unwrap();

    println!("Part 1 answer: {:?}", p1_ans);

    // let input: Vec<i64> = vec![
    //     3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1, 28,
    //     1005, 28, 6, 99, 0, 0, 5,
    // ];

    // let mut intcode = Intcode::new(input);
    // intcode.run().ok();
    // println!("Part 2 answer: {:?}", p2_ans);
}
