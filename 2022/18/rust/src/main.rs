use std::cmp::{max, min};
use std::collections::HashSet;
use std::collections::VecDeque;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Coor {
    x: isize,
    y: isize,
    z: isize,
}

impl Coor {
    fn from_str(s: &str) -> Self {
        let mut nums = s.split(",");

        Coor {
            x: nums.next().unwrap().parse().unwrap(),
            y: nums.next().unwrap().parse().unwrap(),
            z: nums.next().unwrap().parse().unwrap(),
        }
    }

    fn within_bound(&self, min_idx: isize, max_idx: isize) -> bool {
        self.x >= min_idx
            && self.x <= max_idx
            && self.y >= min_idx
            && self.y <= max_idx
            && self.z >= min_idx
            && self.z <= max_idx
    }

    fn neighbors(&self, min_idx: isize, max_idx: isize) -> Vec<Coor> {
        let res: Vec<Coor> = vec![
            Coor {
                x: self.x + 1,
                ..*self
            },
            Coor {
                x: self.x - 1,
                ..*self
            },
            Coor {
                y: self.y + 1,
                ..*self
            },
            Coor {
                y: self.y - 1,
                ..*self
            },
            Coor {
                z: self.z + 1,
                ..*self
            },
            Coor {
                z: self.z - 1,
                ..*self
            },
        ];

        res.into_iter()
            .filter(|x| x.within_bound(min_idx, max_idx))
            .collect::<Vec<Coor>>()
    }
}

fn find_air(lava: &HashSet<Coor>, min_idx: isize, max_idx: isize) -> HashSet<Coor> {
    let search_start = Coor {
        x: min_idx,
        y: min_idx,
        z: min_idx,
    };

    let mut q: VecDeque<Coor> = VecDeque::from([search_start]);
    let mut air: HashSet<Coor> = HashSet::from([search_start]);
    let mut visited: HashSet<Coor> = HashSet::new();

    while !q.is_empty() {
        let current = q.pop_front().unwrap();

        if !(lava.contains(&current) || visited.contains(&current)) {
            air.insert(current);

            current
                .neighbors(min_idx, max_idx)
                .into_iter()
                .for_each(|x| q.push_back(x));
        }

        visited.insert(current);
    }
    return air;
}

fn main() {
    let lava: HashSet<Coor> = std::fs::read_to_string("../input.txt")
        .expect("Unable to read file")
        .lines()
        .map(|s| Coor::from_str(s))
        .collect();

    let max_idx = lava
        .iter()
        .fold(0, |acc, c| max(acc, *[c.x, c.y, c.z].iter().max().unwrap()))
        + 1;

    let min_idx = lava
        .iter()
        .fold(0, |acc, c| min(acc, *[c.x, c.y, c.z].iter().max().unwrap()))
        - 1;

    let p1_ans: usize = lava
        .iter()
        .map(|c| {
            c.neighbors(min_idx, max_idx)
                .iter()
                .filter(|x| !lava.contains(&x))
                .count()
        })
        .sum();

    println!("Part 1 answer: {:?}", p1_ans);

    let air = find_air(&lava, min_idx, max_idx);

    let p2_ans: usize = lava
        .iter()
        .map(|c| {
            c.neighbors(min_idx, max_idx)
                .iter()
                .filter(|x| !lava.contains(&x) && air.contains(&x))
                .count()
        })
        .sum();

    println!("Part 2 answer: {:?}", p2_ans);
}
