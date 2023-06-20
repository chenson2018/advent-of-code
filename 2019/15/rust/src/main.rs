use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode};
use intcode::{Intcode, Message};
use std::{collections::HashMap, fmt::Display};

use crossterm::{cursor, execute, terminal, ExecutableCommand};
use std::io::stdout;

#[derive(Clone, Copy)]
enum Tile {
    Unknown,
    Empty,
    Wall,
    Droid,
    Oxygen,
    Start,
}

impl Display for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tile::Unknown => write!(f, "?"),
            Tile::Empty => write!(f, "."),
            Tile::Wall => write!(f, "█"),
            Tile::Droid => write!(f, "D"),
            Tile::Oxygen => write!(f, "O"),
            Tile::Start => write!(f, "S"),
        }
    }
}

struct DroidMap {
    x: isize,
    y: isize,
    state: HashMap<(isize, isize), Tile>,
}

impl DroidMap {
    fn new() -> Self {
        Self {
            x: 0,
            y: 0,
            state: HashMap::from([((0, 0), Tile::Start)]),
        }
    }

    fn handle_status(&mut self, status: Status, direction: Option<Movement>) {
        let (x_mov, y_mov): (isize, isize) = match direction {
            Some(Movement::North) => (0, 1),
            Some(Movement::South) => (0, -1),
            Some(Movement::West) => (-1, 0),
            Some(Movement::East) => (1, 0),
            None => unreachable!(),
        };

        let new_coor = (self.x + x_mov, self.y + y_mov);

        let tile = match status {
            Status::HitWall => Tile::Wall,
            Status::Success => Tile::Empty,
            Status::SuccessOxygen => Tile::Oxygen,
        };

        // don't wan't to overwrite the starting tile
        if new_coor != (0,0) {
            self.state.insert(new_coor, tile);
        };

        if status != Status::HitWall {
            (self.x, self.y) = new_coor;
        }
    }
}

const MAP_SIZE: isize = 25;

impl Display for DroidMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for y in (-MAP_SIZE..MAP_SIZE).rev() {
            for x in -MAP_SIZE..MAP_SIZE {
                let tile = if self.x == x && self.y == y {
                    Tile::Droid
                } else {
                    *self.state.get(&(x, y)).unwrap_or(&Tile::Unknown)
                };
                write!(f, "{}", tile)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Copy)]
enum Movement {
    North,
    South,
    West,
    East,
}

impl Into<i64> for Movement {
    fn into(self) -> i64 {
        match self {
            Movement::North => 1,
            Movement::South => 2,
            Movement::West => 3,
            Movement::East => 4,
        }
    }
}

#[derive(PartialEq)]
enum Status {
    HitWall,
    Success,
    SuccessOxygen,
}

impl TryFrom<i64> for Status {
    type Error = String;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Status::HitWall),
            1 => Ok(Status::Success),
            2 => Ok(Status::SuccessOxygen),
            _ => Err(format!("Invalid status code {}", value)),
        }
    }
}

// assume IO works becuse I don't feel like handling it
fn run_droid(intcode: &mut Intcode, map: &mut DroidMap) -> Result<(), String> {
    let mut direction: Option<Movement> = None;
    let mut stdout = stdout();
    stdout
        .execute(terminal::Clear(terminal::ClearType::All))
        .ok();
    execute!(stdout, cursor::MoveTo(0, 0)).ok();
    println!("{}", map);

    while !intcode.is_halted() {
        let message = intcode.run_until_io()?;

        match message {
            Message::Input => {
                enable_raw_mode().unwrap();

                while let None = direction {
                    match crossterm::event::read().unwrap() {
                        Event::Key(KeyEvent {
                            code: KeyCode::Up, ..
                        }) => {
                            direction = Some(Movement::North);
                            intcode.add_input(direction.unwrap().into());
                        }
                        Event::Key(KeyEvent {
                            code: KeyCode::Down,
                            ..
                        }) => {
                            direction = Some(Movement::South);
                            intcode.add_input(direction.unwrap().into());
                        }
                        Event::Key(KeyEvent {
                            code: KeyCode::Left,
                            ..
                        }) => {
                            direction = Some(Movement::West);
                            intcode.add_input(direction.unwrap().into());
                        }
                        Event::Key(KeyEvent {
                            code: KeyCode::Right,
                            ..
                        }) => {
                            direction = Some(Movement::East);
                            intcode.add_input(direction.unwrap().into());
                        }
                        Event::Key(KeyEvent {
                            code: KeyCode::Char('d'),
                            modifiers: KeyModifiers::CONTROL,
                            ..
                        }) => {
                            disable_raw_mode().unwrap();
                            return Err("User exit".to_string());
                        }
                        _ => (),
                    }
                }

                disable_raw_mode().unwrap();
                intcode.step()?;
            }
            Message::Output => {
                let status: Status = intcode.flush_output().pop().unwrap().try_into()?;
                map.handle_status(status, direction);
                direction = None;
                execute!(stdout, cursor::MoveTo(0, 0)).ok();
                println!("{}", map);
            }
        }
    }
    Ok(())
}

fn main() -> Result<(), String> {
    let input_str = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let input: Vec<i64> = input_str
        .trim()
        .split(",")
        .map(|x| x.parse::<i64>().unwrap())
        .collect();

    let mut intcode = Intcode::new(input.clone());
    let mut map = DroidMap::new();
    run_droid(&mut intcode, &mut map)?;
    Ok(())
}
