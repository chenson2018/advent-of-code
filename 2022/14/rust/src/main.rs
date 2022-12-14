use std::cmp::{max, min};
use std::collections::HashSet;

enum Part {
    Part1,
    Part2,
}

#[derive(Eq, Hash, PartialEq, Debug, Clone, Copy)]
struct Filled {
    x: usize,
    y: usize,
}

fn parse_rocks(input: &str) -> HashSet<Filled> {
    let mut filled: HashSet<Filled> = HashSet::new();

    // this is a little messy, but good enough for AOC
    for line in input.lines() {
        let corners: Vec<(usize, usize)> = line
            .split(" -> ")
            .map(|l| {
                let (x, y) = if let Some((x, y)) = l.split_once(",") {
                    (x, y)
                } else {
                    panic!();
                };
                (x.parse().unwrap(), y.parse().unwrap())
            })
            .collect();

        let pairs = corners.windows(2);

        for pair in pairs {
            match pair {
                [(x1, y1), (x2, y2)] => {
                    let low_x = min(x1, x2);
                    let upp_x = max(x1, x2);

                    let low_y = min(y1, y2);
                    let upp_y = max(y1, y2);

                    for x in *low_x..=*upp_x {
                        for y in *low_y..=*upp_y {
                            filled.insert(Filled { x, y });
                        }
                    }
                }
                _ => (),
            }
        }
    }
    return filled;
}

impl Filled {
    fn priority(&self) -> [Filled; 3] {
        [
            Filled {
                x: self.x,
                y: self.y + 1,
            },
            Filled {
                x: self.x - 1,
                y: self.y + 1,
            },
            Filled {
                x: self.x + 1,
                y: self.y + 1,
            },
        ]
    }

    fn tick(&self, fill: &mut HashSet<Filled>, limit: usize, part: &Part) -> bool {
        let mut pos = self.clone();

        loop {
            // record starting position
            let start = pos;

            // check in order of priority which open spot is empty
            for m in pos.priority() {
                if !fill.contains(&m) {
                    pos = m;
                    break;
                }
            }

            // check if we weren't able to move
            if start == pos {
                break;
            }

            match part {
                Part::Part1 => {
                    // stop for infinite falling, return before inserting
                    if pos.y >= limit {
                        return false;
                    }
                }
                Part::Part2 => {
                    // stop for floor
                    if pos.y == limit + 1 {
                        break;
                    }
                }
            }
        }

        fill.insert(pos);

        match part {
            Part::Part1 => true,            // keep going, no infinite fall yet
            Part::Part2 => pos.y != self.y, // keep inserting until we fill the starting point
        }
    }

    fn tick_all(&self, fill: &HashSet<Filled>, part: &Part) -> HashSet<Filled> {
        let mut hash = fill.clone();
        let limit = hash.iter().fold(0, |acc, sand| max(acc, sand.y));

        while self.tick(&mut hash, limit, part) {}

        return hash;
    }
}

fn main() {
    let file = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let init_state = parse_rocks(&file);
    let rocks = init_state.len();
    let sand = Filled { x: 500, y: 0 };

    let p1_end_state = sand.tick_all(&init_state, &Part::Part1);
    let p2_end_state = sand.tick_all(&init_state, &Part::Part2);

    println!("Part 1 answer: {:?}", p1_end_state.len() - rocks);
    println!("Part 2 answer: {:?}", p2_end_state.len() - rocks);
}
