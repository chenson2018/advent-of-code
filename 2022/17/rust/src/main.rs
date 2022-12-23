use crate::Shape::*;
use std::cmp::max;
use std::collections::HashSet;

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
struct Coor {
    x: usize,
    y: usize,
}

// the coordinate represents the bottom left coordinate
// In the case of the Cross, it is outside the actual shape!

#[derive(Debug)]
enum Shape {
    Horizontal(Coor),
    Cross(Coor),
    Ell(Coor),
    Vertical(Coor),
    Square(Coor),
}

#[derive(Debug)]
enum Movement {
    Left,
    Right,
    Down,
}

impl Shape {
    fn get_offsets(&self, offsets: &Vec<(usize, usize)>) -> Vec<Coor> {
        match self {
            Horizontal(coor) | Cross(coor) | Ell(coor) | Vertical(coor) | Square(coor) => offsets
                .iter()
                .map(|(x, y)| Coor {
                    x: coor.x + x,
                    y: coor.y + y,
                })
                .collect(),
        }
    }

    fn get_coordinates(&self) -> Vec<Coor> {
        let offsets = match self {
            Self::Horizontal(_) => vec![(0, 0), (1, 0), (2, 0), (3, 0)],
            Self::Cross(_) => vec![(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
            Self::Vertical(_) => vec![(0, 0), (0, 1), (0, 2), (0, 3)],
            Self::Ell(_) => vec![(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
            Self::Square(_) => vec![(0, 0), (1, 1), (0, 1), (1, 0)],
        };
        self.get_offsets(&offsets)
    }

    fn spawn(y_max: usize, rock_idx: usize) -> Self {
        let coor = Coor { x: 3, y: y_max + 4 };

        match rock_idx % 5 {
            0 => Self::Horizontal(coor),
            1 => Self::Cross(coor),
            2 => Self::Ell(coor),
            3 => Self::Vertical(coor),
            4 => Self::Square(coor),
            _ => unreachable!("Impossible modulus?"),
        }
    }
}

impl Movement {
    fn from_char(c: char) -> Option<Self> {
        match c {
            '<' => Some(Self::Left),
            '>' => Some(Self::Right),
            _ => None,
        }
    }
}

fn tick(rock: &Vec<Coor>, movement: &Movement) -> Vec<Coor> {
    match movement {
        Movement::Down => rock
            .iter()
            .map(|Coor { x, y }| Coor { x: *x, y: *y - 1 })
            .collect(),
        Movement::Left => rock
            .iter()
            .map(|Coor { x, y }| Coor { x: *x - 1, y: *y })
            .collect(),
        Movement::Right => rock
            .iter()
            .map(|Coor { x, y }| Coor { x: *x + 1, y: *y })
            .collect(),
    }
}

fn valid_move(rock: &Vec<Coor>, locked: &HashSet<Coor>) -> bool {
    rock.iter()
        .all(|c| c.x >= 1 && c.x <= 7 && c.y >= 1 && !locked.contains(c))
}

fn part1(input: &Vec<Movement>, n: usize) -> (HashSet<Coor>, Vec<usize>) {
    let mut jets = input.iter().cycle();
    let mut locked: HashSet<Coor> = HashSet::new();
    let mut y_max: usize = 0;
    let mut cycle_height = Vec::new();

    for rock_idx in 0..n {
        let init_shape = Shape::spawn(y_max, rock_idx);
        let mut rock = init_shape.get_coordinates();

        loop {
            let movement = jets.next().unwrap();
            let proposal = tick(&rock, &movement);

            if valid_move(&proposal, &locked) {
                rock = proposal;
            }

            let downward = tick(&rock, &Movement::Down);

            if valid_move(&downward, &locked) {
                rock = downward;
            } else {
                for coor in rock {
                    locked.insert(coor);
                    y_max = max(y_max, coor.y);
                }
                cycle_height.push(y_max);
                break;
            }
        }
    }
    (locked, cycle_height)
}

// just for fun, since not hard to print nicely
fn _print_state(state: &HashSet<Coor>) {
    let y_max = state.iter().fold(0, |acc, coor| max(acc, coor.y));

    println!("");

    for y_rev in 0..=y_max {
        let y = y_max - y_rev;

        let line = (0..=8).map(|x| {
            if y == 0 && (x == 0 || x == 8) {
                '+'
            } else if y == 0 {
                '_'
            } else if x == 0 || x == 8 {
                '|'
            } else if state.contains(&Coor { x, y }) {
                '#'
            } else {
                '.'
            }
        });

        println!("{}", String::from_iter(line));
    }

    println!("");
}

// A bit hacky but it works...
// I guessed that there is a cycle in the heights
// So what I do here is:
//  1) simulate the initial heights manually (sim_len shapes)
//  2) pick a point somewhere in here as a guess for a middle of a cycle (guess_middle)
//  3) determine how big a window we need for this to be part of a cycle, accepting if there are
//     rep_accept repeats

fn part_2(
    input: &Vec<Movement>,
    n: usize,
    sim_len: usize,
    guess_middle: usize,
    rep_accept: usize,
) -> usize {
    let (_, cycle_heights) = part1(input, sim_len);
    let height_diffs: Vec<usize> = cycle_heights[..]
        .windows(2)
        .map(|slice| slice[1] - slice[0])
        .collect();

    let mut cycle_window: usize = 1;

    loop {
        let potential_cycles: Vec<&[usize]> = (0..rep_accept)
            .map(|i| {
                let start_idx = guess_middle + (cycle_window * i);
                let end_idx = start_idx + cycle_window;
                &height_diffs[start_idx..end_idx]
            })
            .collect();

        let mut iter = potential_cycles.clone().into_iter();
        let cycle = iter.next().unwrap();

        // check if they are all matching, or increase the cycle length
        if iter.all(|x| x == cycle) {
            let pre_cycle_heights: usize = height_diffs[0..guess_middle].into_iter().sum();
            let remaining_n = n - guess_middle;

            let n_cycles = remaining_n / cycle_window;
            let remainder = remaining_n % cycle_window;

            return pre_cycle_heights
                + (n_cycles * cycle.iter().sum::<usize>())
                + cycle[0..remainder].iter().sum::<usize>();
        } else {
            cycle_window += 1;
        }
    }
}

fn main() {
    let input: Vec<Movement> = std::fs::read_to_string("../input.txt")
        .expect("Unable to read file")
        .trim()
        .chars()
        .map(|c| Movement::from_char(c).unwrap())
        .collect();

    let (state_after_2022, _) = part1(&input, 2022);
    let p1_ans = state_after_2022
        .iter()
        .fold(0, |acc, coor| max(acc, coor.y));
    println!("Part 1 answer: {:?}", p1_ans);

    // these values are guesses that correspond to a cycle happening
    // after 1000 shapes, with 5 repeats of the cycle before 20,000 shapes
    let p2_ans = part_2(&input, 1000000000000, 20_000, 1_000, 5);
    println!("Part 2 answer: {}", p2_ans);
}
