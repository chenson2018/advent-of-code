use intcode::Intcode;
use itertools::Itertools;

fn main() {
    let input_str = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let input: Vec<i64> = input_str
        .trim()
        .split(",")
        .map(|x| x.parse::<i64>().unwrap())
        .collect();

    let p1_ans: i64 = (0..=4)
        .map(|x| x as i64)
        .permutations(5)
        .map(|perm| {
            let mut signal = 0;

            for phase in perm {
                let mut intcode = Intcode::new(input.clone());
                intcode.silent = true;
                intcode.input = vec![signal, phase];
                intcode.run().ok();
                signal = intcode.output().pop().unwrap();
            }

            signal
        })
        .max()
        .unwrap();

    println!("Part 1 answer: {:?}", p1_ans);

    let p2_ans = (5..=9)
        .map(|x| x as i64)
        .permutations(5)
        .map(|perm| {
            let mut signal = 0;
            let mut amps = Vec::new();

            // first pass that uses the phases
            for p in perm {
                let mut intcode = Intcode::new(input.clone());
                intcode.silent = true;
                intcode.input = vec![signal, p];
                signal = intcode.run_until_output().unwrap();
                amps.push(intcode);
            }

            // pure signals
            amps.first_mut().unwrap().add_input(signal);

            while !amps.last().unwrap().is_halted() {
                for idx in 0..5 {
                    signal = amps[idx].run_until_output().unwrap();
                    amps[(idx + 1) % 5].add_input(signal);
                }
            }

            signal
        })
        .max()
        .unwrap();

    println!("Part 2 answer: {:?}", p2_ans);
}
