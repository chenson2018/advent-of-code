use std::collections::HashSet;

#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
struct Elf {
    x: isize,
    y: isize,
}

enum Dir {
    N,
    NE,
    E,
    SE,
    S,
    SW,
    W,
    NW,
}

impl Elf {
    fn move_dir(&self, dir: &Dir) -> Self {
        match dir {
            Dir::N => Elf {
                y: self.y + 1,
                x: self.x,
            },
            Dir::NE => Elf {
                y: self.y + 1,
                x: self.x + 1,
            },
            Dir::E => Elf {
                y: self.y,
                x: self.x + 1,
            },
            Dir::SE => Elf {
                y: self.y - 1,
                x: self.x + 1,
            },
            Dir::S => Elf {
                y: self.y - 1,
                x: self.x,
            },
            Dir::SW => Elf {
                y: self.y - 1,
                x: self.x - 1,
            },
            Dir::W => Elf {
                y: self.y,
                x: self.x - 1,
            },
            Dir::NW => Elf {
                y: self.y + 1,
                x: self.x - 1,
            },
        }
    }

    fn any_adj(&self, elves: &HashSet<Elf>, dirs: [Dir; 3]) -> bool {
        dirs.iter().fold(false, |acc, dir| {
            acc || elves.contains(&self.move_dir(&dir))
        })
    }

    fn tick(&self, elves: &HashSet<Elf>, n: usize) -> (Self, Self) {
        let checks = [
            self.any_adj(elves, [Dir::N, Dir::NE, Dir::NW]),
            self.any_adj(elves, [Dir::S, Dir::SE, Dir::SW]),
            self.any_adj(elves, [Dir::W, Dir::NW, Dir::SW]),
            self.any_adj(elves, [Dir::E, Dir::NE, Dir::SE]),
        ];

        let dirs = [Dir::N, Dir::S, Dir::W, Dir::E];

        let new = if !checks.iter().any(|x| *x) {
            *self
        } else if !checks[n % 4] {
            self.move_dir(&dirs[n % 4])
        } else if !checks[(n + 1) % 4] {
            self.move_dir(&dirs[(n + 1) % 4])
        } else if !checks[(n + 2) % 4] {
            self.move_dir(&dirs[(n + 2) % 4])
        } else if !checks[(n + 3) % 4] {
            self.move_dir(&dirs[(n + 3) % 4])
        } else {
            *self
        };

        (*self, new)
    }
}

fn tick_all(elves: &HashSet<Elf>, n: usize) -> HashSet<Elf> {
    let tup_prev_next: Vec<(Elf, Elf)> = elves.iter().map(|elf| elf.tick(elves, n)).collect();
    let maybe_next: Vec<Elf> = tup_prev_next
        .clone()
        .iter()
        .map(|(_, nelf)| *nelf)
        .collect();

    tup_prev_next
        .into_iter()
        .map(|(original, new)| {
            if maybe_next.iter().filter(|nelf| **nelf == new).count() == 1 {
                new
            } else {
                original
            }
        })
        .collect()
}

fn input_to_elves(grid: &Vec<Vec<char>>) -> HashSet<Elf> {
    let mut res: HashSet<Elf> = HashSet::new();
    let len = grid.len();

    for (y, row) in grid.iter().enumerate() {
        for (x, c) in row.iter().enumerate() {
            if *c == '#' {
                res.insert(Elf {
                    x: x as isize,
                    y: (len - y) as isize, // doing this to make directions less confusing
                });
            }
        }
    }
    res
}

fn score(elves: &HashSet<Elf>) -> usize {
    let y_coor: Vec<isize> = elves.iter().map(|e| e.y).collect();
    let x_coor: Vec<isize> = elves.iter().map(|e| e.x).collect();

    (((y_coor.iter().max().unwrap() - y_coor.iter().min().unwrap() + 1)
        * (x_coor.iter().max().unwrap() - x_coor.iter().min().unwrap() + 1)) as usize)
        - elves.len()
}

fn main() {
    let input: Vec<Vec<char>> = std::fs::read_to_string("../input.txt")
        .expect("Unable to read file")
        .lines()
        .map(|l| l.chars().collect())
        .collect();

    let mut elves = input_to_elves(&input);
    let mut round = 1;

    while round <= 10 {
        elves = tick_all(&elves, round - 1);
        round += 1;
    }

    let p1_ans = score(&elves);
    println!("Part 1 answer: {:#?}", p1_ans);

    loop {
        let next_iter = tick_all(&elves, round - 1);
        if elves == next_iter {
            break;
        } else {
            elves = next_iter;
            round += 1;
        }
    }

    println!("Part 2 answer: {}", round);
}
