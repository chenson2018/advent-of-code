#[derive(Debug)]
enum Instruction {
    Noop,
    Addx(isize),
}

impl Instruction {
    fn from_str(s: &str) -> Option<Instruction> {
        match s {
            "noop" => Some(Instruction::Noop),
            _ => {
                let (op, amount) = s.split_once(' ').unwrap();
                match op {
                    "addx" => Some(Instruction::Addx(amount.parse().unwrap())),
                    _ => None,
                }
            }
        }
    }
}

#[derive(Debug)]
struct CPU<'a> {
    x: isize,
    cycle: usize,
    signal_strength: isize,
    screen: Vec<&'a str>,
}

impl CPU<'_> {
    fn new() -> Self {
        CPU {
            x: 1,
            cycle: 1,
            signal_strength: 0,
            screen: Vec::with_capacity(240),
        }
    }

    fn cycle_inc(&mut self, n: usize) {
        self.cycle += n;
    }

    fn check_signal(&mut self) {
        if self.cycle % 40 == 20 {
            self.signal_strength += self.cycle as isize * self.x;
        }

        // -1 for indexing, mod 40 for the row
        let draw_position = (self.cycle as isize - 1) % 40;

        if (draw_position - self.x).abs() <= 1 {
            self.screen.push("#");
        } else {
            self.screen.push(".");
        }
    }

    fn process(&mut self, ins: &Instruction) {
        match ins {
            Instruction::Noop => {
                self.check_signal();
                self.cycle_inc(1);
            }
            Instruction::Addx(x) => {
                self.check_signal();
                self.cycle_inc(1);

                self.check_signal();
                self.x += x;
                self.cycle_inc(1);
            }
        }
    }
}

fn main() {
    let instructions: Vec<Instruction> = std::fs::read_to_string("../input.txt")
        .expect("Unable to read file")
        .lines()
        .map(|s| Instruction::from_str(s).unwrap())
        .collect();

    let mut cpu = CPU::new();

    for ins in instructions {
        cpu.process(&ins);
    }

    println!(
        "Part 1 answer: {:?}",
        cpu.signal_strength
    );

    println!("Part 2 answer:\n");

    for i in 0..6 {
        let start = 40 * i;
        println!("{}", &cpu.screen[start..start + 40].join(""));
    }
}
