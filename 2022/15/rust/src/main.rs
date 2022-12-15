use itertools::Itertools;
use regex::Regex;
use std::cmp::max;

#[derive(Clone, Copy, PartialEq, Debug)]
struct Coor {
    x: isize,
    y: isize,
}

#[derive(Clone, Copy, PartialEq, Debug)]
struct RotatedCoor {
    x: isize,
    y: isize,
}

// translating back/forth for our coordinate systems
impl From<RotatedCoor> for Coor {
    fn from(rc: RotatedCoor) -> Coor {
        let y = (rc.x + rc.y) / 2;
        Coor { x: rc.y - y, y: y }
    }
}

impl From<Coor> for RotatedCoor {
    fn from(coor: Coor) -> RotatedCoor {
        RotatedCoor {
            x: coor.y - coor.x,
            y: coor.y + coor.x,
        }
    }
}

#[derive(Debug)]
struct Pair {
    sensor: Coor,
    beacon: Coor,
}

impl Coor {
    fn manhattan(&self, other: &Coor) -> isize {
        (self.x - other.x).abs() + (self.y - other.y).abs()
    }
}

impl Pair {
    fn from_str(s: &str) -> Self {
        let re = Regex::new(
            "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)",
        )
        .unwrap();
        let caps = re.captures(s).unwrap();

        // not error handling here
        let sx = caps.get(1).map_or("", |m| m.as_str());
        let sy = caps.get(2).map_or("", |m| m.as_str());
        let bx = caps.get(3).map_or("", |m| m.as_str());
        let by = caps.get(4).map_or("", |m| m.as_str());

        Pair {
            sensor: Coor {
                x: sx.parse().unwrap(),
                y: sy.parse().unwrap(),
            },
            beacon: Coor {
                x: bx.parse().unwrap(),
                y: by.parse().unwrap(),
            },
        }
    }

    fn closest_dist(&self) -> isize {
        self.sensor.manhattan(&self.beacon)
    }

    fn cannot_contain(&self, coor: &Coor) -> bool {
        (self.beacon != *coor) && (coor.manhattan(&self.sensor) <= self.closest_dist())
    }
}

fn part1(pairs: &Vec<Pair>, line: isize) -> usize {
    // these are the furtest left and right a beacon can possibly reach
    let left = pairs
        .iter()
        .map(|p| p.sensor.x - p.closest_dist())
        .min()
        .unwrap();

    let right = pairs
        .iter()
        .map(|p| p.sensor.x + p.closest_dist())
        .max()
        .unwrap();

    // tricky double negative:
    // find all points where every sensor allows a beacon to be placed
    (left..right)
        .map(|x| {
            pairs
                .iter()
                .all(|p| !p.cannot_contain(&Coor { x: x, y: line }))
        })
        .filter(|b| !*b)
        .collect::<Vec<bool>>()
        .len()
}

fn part2(pairs: &Vec<Pair>, limit: isize) -> Option<isize> {
    // two opposite corners of each square in the rotated space
    let rotated_corners: Vec<RotatedCoor> = pairs
        .iter()
        .map(|p| {
            let dist = p.closest_dist();
            let rotate = RotatedCoor::from(p.sensor);

            vec![
                RotatedCoor {
                    x: rotate.x + dist,
                    y: rotate.y + dist,
                },
               RotatedCoor {
                   x: rotate.x - dist,
                   y: rotate.y - dist,
               },
            ]
        })
        .flatten()
        .collect();

    let close_y: Vec<isize> = rotated_corners
        .iter()
        .tuple_combinations()
        .filter(|(p1,p2)| (p1.y - p2.y).abs() == 2)
        .map(|(p1,p2)| max(p1.y, p2.y) - 1)
        .unique()
        .collect();

    let close_x: Vec<isize> = rotated_corners
        .iter()
        .tuple_combinations()
        .filter(|(p1,p2)| (p1.x - p2.x).abs() == 2)
        .map(|(p1,p2)| max(p1.x, p2.x) - 1)
        .unique()
        .collect();

    for x in &close_x {
        for y in &close_y {
            let rot = RotatedCoor { x: *x, y: *y };
            let orig = Coor::from(rot);
            if pairs
                .iter()
                .all(|x| !(x.cannot_contain(&orig) || x.beacon == orig))
                && orig.x <= limit
                && orig.y <= limit
                && orig.x >= 0
                && orig.y >= 0
            {
                return Some(orig.x * 4000000 + orig.y);
            }
        }
    }
    return None;
}

fn main() {
    let pairs: Vec<Pair> = std::fs::read_to_string("../input.txt")
        .expect("Unable to read file")
        .lines()
        .map(|s| Pair::from_str(s))
        .collect();

    let p1_ans = part1(&pairs, 2000000);
    let p2_ans = part2(&pairs, 4000000).unwrap();

    println!("Part 1 answer: {:?}", p1_ans);
    println!("Part 2 answer: {:?}", p2_ans);
}
