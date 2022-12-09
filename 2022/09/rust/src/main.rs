use std::collections::HashSet;

#[derive(Debug)]
enum Instruction {
    Left,
    Right,
    Up,
    Down,
}

impl Instruction {
    fn from_str(s: &str) -> Option<(Instruction, usize)> {
        let (letter, amount) = s.split_once(' ').unwrap();
        match letter {
            "L" => Some((Instruction::Left, amount.parse().unwrap())),
            "R" => Some((Instruction::Right, amount.parse().unwrap())),
            "U" => Some((Instruction::Up, amount.parse().unwrap())),
            "D" => Some((Instruction::Down, amount.parse().unwrap())),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Knot {
    x: isize,
    y: isize,
}

impl Knot {
    fn new() -> Self {
        Knot { x: 0, y: 0 }
    }

    fn adjacent(&self, other: &Knot) -> bool {
        (self.x - other.x).abs() <= 1 && (self.y - other.y).abs() <= 1
    }

    fn move_head(&mut self, ins: &Instruction) {
        match ins {
            Instruction::Left => self.x -= 1,
            Instruction::Right => self.x += 1,
            Instruction::Up => self.y += 1,
            Instruction::Down => self.y -= 1,
        }
    }

    fn follow(&mut self, other: &Knot) {
        if !(self == other || self.adjacent(other)) {
            // this is tricky, the order matters: first horizontal/vertical, then diagonals
            const IDX: [(isize, isize); 8] = [
                (0, -1),
                (0, 1),
                (-1, 0),
                (1, 0),
                (1, 1),
                (-1, -1),
                (1, -1),
                (-1, 1),
            ];

            for (x, y) in IDX {
                let neighbor = Knot {
                    x: other.x + x,
                    y: other.y + y,
                };
                if self.adjacent(&neighbor) {
                    *self = neighbor;
                    break;
                }
            }
        }
    }
}

fn knot_trail(instructions: &Vec<(Instruction, usize)>, length: usize) -> usize {
    let mut knots: Vec<Knot> = std::iter::repeat(Knot::new()).take(length).collect();

    let mut tail_visited = HashSet::new();
    tail_visited.insert(knots[length - 1]);

    for (ins, amount) in instructions {
        // we repeat each line's instruction based on the number given
        for _ in 0..*amount {
            // first, move the head
            knots[0].move_head(&ins);

            // next, each successive knot follows its leader
            for idx in 1..length {
                let leader = knots[idx - 1];
                knots[idx].follow(&leader);
            }
            // we record the very last knot, the tail
            tail_visited.insert(knots[length - 1]);
        }
    }
    return tail_visited.len();
}

fn main() {
    let instructions: Vec<(Instruction, usize)> = std::fs::read_to_string("../input.txt")
        .expect("Unable to read file")
        .lines()
        .map(|s| Instruction::from_str(s).unwrap())
        .collect();

    let p1_ans = knot_trail(&instructions, 2);
    println!("Part 1 answer: {}", p1_ans);

    let p2_ans = knot_trail(&instructions, 10);
    println!("Part 2 answer: {}", p2_ans);
}
