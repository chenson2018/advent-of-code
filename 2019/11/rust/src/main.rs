use core::fmt;
use intcode::Intcode;
use std::collections::HashMap;

#[derive(Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl TryFrom<i8> for Direction {
    type Error = &'static str;

    fn try_from(value: i8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Direction::Up),
            1 => Ok(Direction::Right),
            2 => Ok(Direction::Down),
            3 => Ok(Direction::Left),
            _ => Err("Invalid direction (outside of range 0..4)."),
        }
    }
}

impl Into<i8> for Direction {
    fn into(self) -> i8 {
        match self {
            Direction::Up => 0,
            Direction::Right => 1,
            Direction::Down => 2,
            Direction::Left => 3,
        }
    }
}

#[derive(Clone, Copy)]
enum Paint {
    Black,
    White,
}

impl fmt::Display for Paint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Paint::White => write!(f, "█"),
            Paint::Black => write!(f, " "),
        }
    }
}

impl Into<i64> for Paint {
    fn into(self) -> i64 {
        match self {
            Paint::Black => 0,
            Paint::White => 1,
        }
    }
}

impl TryFrom<i64> for Paint {
    type Error = &'static str;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Paint::Black),
            1 => Ok(Paint::White),
            _ => Err("Invalid paint (outside of range 0..=1)."),
        }
    }
}

enum Rotation {
    Left,
    Right,
}

impl TryFrom<i64> for Rotation {
    type Error = &'static str;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Rotation::Left),
            1 => Ok(Rotation::Right),
            _ => Err("Invalid rotate (outside of range 0..=1)."),
        }
    }
}

struct Robot {
    x: isize,
    y: isize,
    direction: Direction,
    state: HashMap<(isize, isize), Paint>,
}

impl Robot {
    fn new() -> Self {
        Self {
            x: 0,
            y: 0,
            direction: Direction::Up,
            state: HashMap::new(),
        }
    }

    fn rotate(&mut self, rotate: Rotation) {
        let offset = match rotate {
            Rotation::Left => -1,
            Rotation::Right => 1,
        };

        let current: i8 = self.direction.into();
        let new: i8 = (current + offset).rem_euclid(4);
        self.direction = new.try_into().unwrap();
    }

    fn paint(&mut self, paint: Paint) {
        self.state.insert((self.x, self.y), paint);
    }

    fn current(&self) -> Paint {
        *self.state.get(&(self.x, self.y)).unwrap_or(&Paint::Black)
    }

    fn step(&mut self) {
        match self.direction {
            Direction::Up => {
                self.y += 1;
            }
            Direction::Down => {
                self.y -= 1;
            }
            Direction::Left => {
                self.x -= 1;
            }
            Direction::Right => {
                self.x += 1;
            }
        }
    }
}

fn run_robot(robot: &mut Robot, intcode: &mut Intcode) {
    while !intcode.is_halted() {
        intcode.add_input(robot.current().into());

        let paint = intcode.run_until_output().unwrap();
        let direction = intcode.run_until_output().unwrap();

        robot.paint(paint.try_into().unwrap());
        robot.rotate(direction.try_into().unwrap());
        robot.step();
    }
}

fn main() {
    let input_str = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let input: Vec<i64> = input_str
        .trim()
        .split(",")
        .map(|x| x.parse::<i64>().unwrap())
        .collect();

    let mut intcode_p1 = Intcode::new(input.clone());
    intcode_p1.silent = true;
    let mut robot_p1 = Robot::new();
    run_robot(&mut robot_p1, &mut intcode_p1);
    println!("Part 1 answer: {}", robot_p1.state.len());

    let mut intcode_p2 = Intcode::new(input);
    intcode_p2.silent = true;
    let mut robot_p2 = Robot::new();
    robot_p2.paint(Paint::White);
    run_robot(&mut robot_p2, &mut intcode_p2);

    let x_vals: Vec<isize> = robot_p2.state.iter().map(|((x, _), _)| *x).collect();
    let y_vals: Vec<isize> = robot_p2.state.iter().map(|((_, y), _)| *y).collect();

    let x_min = *x_vals.iter().min().unwrap();
    let x_max = *x_vals.iter().max().unwrap();
    let y_min = *y_vals.iter().min().unwrap();
    let y_max = *y_vals.iter().max().unwrap();

    println!("Part 2 answer:");
    for y in (y_min..=y_max).rev() {
        for x in x_min..=x_max {
            let paint = robot_p2.state.get(&(x, y)).unwrap_or(&Paint::Black);
            print!("{}", paint);
        }
        println!();
    }
}
