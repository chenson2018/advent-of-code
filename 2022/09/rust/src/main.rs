use std::collections::HashSet;

#[derive(Debug)]
enum Instruction {
    Left,
    Right,
    Up,
    Down,
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

    fn go(&mut self, ins: &Instruction) {
        match ins {
            Instruction::Left => self.x -= 1,
            Instruction::Right => self.x += 1,
            Instruction::Up => self.y += 1,
            Instruction::Down => self.y -= 1,
        }
    }

    fn follow(&mut self, other: &Knot) {
        if !(self == other || self.adjacent(other)) {
            // one of the four options surronding head must be adjacent to tail
            // is there a nice way I could not have to define all four here, only constructing the
            // one that is used????

            let bot = Knot {
                x: other.x,
                y: other.y - 1,
            };

            let top = Knot {
                x: other.x,
                y: other.y + 1,
            };

            let left = Knot {
                x: other.x - 1,
                y: other.y,
            };

            let right = Knot {
                x: other.x + 1,
                y: other.y,
            };

            // this is confusing because of the way it mutates self, then checks again, meaning it
            // always gets past these, even though that's not the original intent. Maybe the issue
            // with part 2????
            if self.adjacent(&bot) {
                *self = bot;
            } else if self.adjacent(&top) {
                *self = top;
            } else if self.adjacent(&left) {
                *self = left;
            } else if self.adjacent(&right) {
                *self = right;
            }
        }
        //println!("Leader is {:?}, follower is {:?}", other, self);
    }
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

fn knot_trail(instructions: &Vec<(Instruction, usize)>, length: usize) -> usize {
    let mut knots: Vec<Knot> = std::iter::repeat(Knot::new()).take(length).collect();

    let mut tail_visited = HashSet::new();
    tail_visited.insert(knots[length - 1]);

    for (ins, amount) in instructions {
        // we repeat each line's instruction based on the number given
        for _ in 0..*amount {
            // first, move the head
            knots[0].go(&ins);

            // next move each item sequentially
            for idx in 1..length {
                let current_head = knots[idx - 1];
                knots[idx].follow(&current_head);
            }
            //            println!("After {:?} at: {:?}", ins, knots);

            // we record the very last tail
            tail_visited.insert(knots[length - 1]);
        }
        //    println!("After {:?} {:?} at: {:?}", ins, amount, knots);
    }
    return tail_visited.len();
}

fn main() {
    //let instructions: Vec<(Instruction, usize)> = std::fs::read_to_string("../test.txt")
    let instructions: Vec<(Instruction, usize)> = std::fs::read_to_string("../input.txt")
        .expect("Unable to read file")
        .lines()
        .map(|s| Instruction::from_str(s).unwrap())
        .collect();

    let p1_ans = knot_trail(&instructions, 2);
    println!("Part 1 answer: {}", p1_ans);

    // not working yet
    //let p2_ans= knot_trail(&instructions, 10);
    //println!("Part 2 answer: {}", p2_ans);
}
