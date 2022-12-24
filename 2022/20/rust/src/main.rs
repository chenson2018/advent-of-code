#[derive(Debug)]
struct Num {
    origin: usize,
    val: isize,
}

fn decode(vec: &Vec<Num>, key: isize, mix: usize) -> isize {
    let mut input: Vec<Num> = vec
        .iter()
        .map(|x| Num {
            val: key * x.val,
            ..*x
        })
        .collect();
    let len = input.iter().len();

    for _ in 0..mix {
        for origin_idx in 0..len {
            let idx_move = input
                .iter()
                .position(|num| num.origin == origin_idx)
                .unwrap();
            let new_index = (idx_move as isize + input[idx_move].val).rem_euclid(len as isize - 1);
            let tmp = input.remove(idx_move);
            input.insert(new_index as usize, tmp);
        }
    }

    let idx_zero = input.iter().position(|x| x.val == 0).unwrap();

    [1000, 2000, 3000]
        .iter()
        .map(|n| input[idx_zero + n % len].val)
        .sum()
}

fn main() {
    let input: Vec<Num> = std::fs::read_to_string("../input.txt")
        .expect("Unable to read file")
        .lines()
        .enumerate()
        .map(|(i, v)| {
            let val = v.parse::<isize>().ok().unwrap();
            Num {
                origin: i,
                val: val,
            }
        })
        .collect();

    println!("Part 1 answer: {}", decode(&input, 1, 1));
    println!("Part 2 answer: {}", decode(&input, 811589153, 10));
}
