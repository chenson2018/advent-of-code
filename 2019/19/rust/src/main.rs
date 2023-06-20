use intcode::Intcode;
use itertools::Itertools;

fn is_beam(input: Vec<i64>, x: i64, y: i64) -> bool {
    let mut intcode = Intcode::new(input.clone());
    intcode.input = vec![y, x];
    intcode.silent = true;
    1 == intcode.run_until_output().unwrap()
}

fn allows_square(input: Vec<i64>, x: i64, y: i64, size: i64) -> bool {
    is_beam(input.clone(), x, y)
        && is_beam(input.clone(), x, y + size - 1)
        && is_beam(input, x + size - 1, y)
}

fn main() {
    let input_str = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let input: Vec<i64> = input_str
        .trim()
        .split(",")
        .map(|x| x.parse::<i64>().unwrap())
        .collect();

    let p1_ans: usize = (0..50)
        .cartesian_product(0..50)
        .filter(|(x, y)| is_beam(input.clone(), *x, *y))
        .count();

    println!("Part 1 answer: {:?}", p1_ans);

//    let guess: i64 = {
//        let mut diag: i64 = 0;
//        let mut inc: i64 = 1000000;
//
//        loop {
//            if allows_square(input.clone(), diag, diag, 100) {
//                if inc == 1 {
//                    break;
//                };
////                println!(
////                    "At diag {}, moving to {} changing inc to {}",
////                    diag,
////                    diag - inc,
////                    (inc / 10)
////                );
//                diag -= inc;
//                inc = inc / 10;
//            } else {
//                diag += inc;
//            };
//        }
//
//        diag
//    };
//
//    // check a full grid around the guess
//    let search: Vec<(i64, i64)> = ((guess - 750)..guess)
//        .cartesian_product((guess - 750)..guess)
//        .filter(|(x, y)| allows_square(input.clone(), *x, *y, 100))
//        .collect();
//
//        let (x, y) = search.iter().sorted_by(|(x1, y1), (x2, y2)| Ord::cmp(&(x1 + y1), &(x2 + y2))  ).next().unwrap();
//
//    println!("Initial guess: {:?}", guess);
//    println!("Refine to: {}, {}", x, y);
//    println!("Part 2 answer: {:?}", 10000 * x + y);
}
