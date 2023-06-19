use intcode::Intcode;
use itertools::Itertools;

fn main() {
    let input_str = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let input: Vec<i64> = input_str
        .trim()
        .split(",")
        .map(|x| x.parse::<i64>().unwrap())
        .collect();

    let mut intcode = Intcode::new(input.clone());
    intcode.silent = true;
    intcode.run().ok();

    let out = intcode.output();

    // assume equal widths
    // note that the intcode terminates with two newlines
    let mut lines: Vec<Vec<i64>> = Vec::new();

    let line_width = 1 + out.iter().position(|x| x == &10).unwrap();
    let n_lines = out.iter().filter(|x| x == &&10).count() - 1;

    for chunk in out.chunks(line_width) {
        if chunk.len() > 1 {
            lines.push(chunk.to_vec());
        }
    }

    let p1_ans: i64 = (1..(n_lines - 1))
        .cartesian_product(1..(line_width - 1))
        .map(|(y, x)| {
            if [
                lines[y][x],
                lines[y + 1][x],
                lines[y - 1][x],
                lines[y][x + 1],
                lines[y][x - 1],
            ]
            .iter()
            .all(|x| x == &35)
            {
                (x * y) as i64
            } else {
                0
            }
        })
        .sum();

    println!("Part 1 answer: {:?}", p1_ans);

    // TODO just testing here...

    let ins: Vec<i64> = vec![
        65, 10, //main
        76, 44, 52, 10, // A
        52, 10, // B
        52, 10, // C
    ]
    .iter()
    .map(|x: &i64| *x as i64)
    .rev()
    .collect();

    let mut intcode_p2 = Intcode::new(input.clone());
    intcode_p2.input = ins;
    intcode_p2.ascii_output = true;
    intcode_p2.replace(0, 2).ok();
    intcode_p2.run().ok();
}
