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
            let mut amp = 0;
            let mut thruster_signals: Vec<i64> = Vec::new();

            for phase in perm {
                let mut intcode = Intcode::new_simulation(input.clone(), vec![amp, phase], true);
                intcode.run().ok();
                amp = intcode.output().pop().unwrap();
                thruster_signals.push(amp);
            }

            thruster_signals.pop().unwrap()
        })
        .max()
        .unwrap();

    println!("Part 1 answer: {:?}", p1_ans);

    //    let input: Vec<i64> = vec![
    //        3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54, -5,
    //        54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4, 53,
    //        1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10,
    //    ];

    let p2_ans = (5..=9)
        .map(|x| x as i64)
        .permutations(5)
        .map(|perm| {
            let mut a = Intcode::new_simulation(input.clone(), vec![0, perm[0]], true);
            let a_signal = a.run_until_output().unwrap();

            let mut b = Intcode::new_simulation(input.clone(), vec![a_signal, perm[1]], true);
            let b_signal = b.run_until_output().unwrap();

            let mut c = Intcode::new_simulation(input.clone(), vec![b_signal, perm[2]], true);
            let c_signal = c.run_until_output().unwrap();

            let mut d = Intcode::new_simulation(input.clone(), vec![c_signal, perm[3]], true);
            let d_signal = d.run_until_output().unwrap();

            let mut e = Intcode::new_simulation(input.clone(), vec![d_signal, perm[4]], true);
            let e_signal = e.run_until_output().unwrap();

            a.add_input(e_signal);

            while !e.is_halted() {
                let a_signal = a.run_until_output().unwrap();
                b.add_input(a_signal);

                let b_signal = b.run_until_output().unwrap();
                c.add_input(b_signal);

                let c_signal = c.run_until_output().unwrap();
                d.add_input(c_signal);

                let d_signal = d.run_until_output().unwrap();
                e.add_input(d_signal);

                let e_signal = e.run_until_output().unwrap();
                a.add_input(e_signal);
            }

            e.output().pop().unwrap()
        })
        .max()
        .unwrap();

    println!("Part 2 answer: {:?}", p2_ans);
}
