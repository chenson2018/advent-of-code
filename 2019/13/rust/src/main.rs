use intcode::Intcode;
use std::collections::HashMap;
use std::fmt::Display;
use std::{thread, time};

use crossterm::event::{poll, read, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode};
use crossterm::{cursor, execute, terminal, ExecutableCommand};
use std::io::{stdout, Write};

#[derive(PartialEq)]
enum Tile {
    Empty,
    Wall,
    Block,
    HorizontalPaddle,
    Ball,
}

impl TryFrom<i64> for Tile {
    type Error = &'static str;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Tile::Empty),
            1 => Ok(Tile::Wall),
            2 => Ok(Tile::Block),
            3 => Ok(Tile::HorizontalPaddle),
            4 => Ok(Tile::Ball),
            _ => Err("Invalid tile (outside range 0..=4)."),
        }
    }
}

impl Display for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tile::Empty => write!(f, " "),
            Tile::Wall => write!(f, "█"),
            Tile::Block => write!(f, "B"),
            Tile::HorizontalPaddle => write!(f, "-"),
            Tile::Ball => write!(f, "O"),
        }
    }
}

struct Game {
    tiles: HashMap<(i64, i64), Tile>,
}

impl Display for Game {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for y in 0..=24 {
            for x in 0..=39 {
                let tile = self.tiles.get(&(x, y)).unwrap_or(&Tile::Empty);
                write!(f, "{}", tile)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Game {
    fn add_tile(&mut self, x: i64, y: i64, tile_int: i64) -> Result<(), &'static str> {
        let tile: Tile = tile_int.try_into()?;
        self.tiles.insert((x, y), tile);
        Ok(())
    }

    fn new() -> Self {
        Self {
            tiles: HashMap::new(),
        }
    }
}

fn run_game(intcode: &mut Intcode, game: &mut Game, print: bool) -> Result<(), String> {
    let mut stdout = stdout();
    stdout.execute(terminal::Clear(terminal::ClearType::All));

    while !intcode.is_halted() {
        // check if the current instruction is Input
        // if so, pass to the key handler
        if intcode.read_offset(0).unwrap().rem_euclid(10) == 3 {
            enable_raw_mode().unwrap();
            if poll(time::Duration::from_millis(10000)).unwrap() {
                match crossterm::event::read().unwrap() {
                    Event::Key(KeyEvent {
                        code: KeyCode::Left,
                        ..
                    }) => {
                        intcode.input = vec![-1];
                    }
                    Event::Key(KeyEvent {
                        code: KeyCode::Right,
                        ..
                    }) => {
                        intcode.input = vec![1];
                    }
                    _ => (),
                };
            } else {
                intcode.input = vec![0];
            };
            disable_raw_mode().unwrap();
        };

        let x = intcode.run_until_output()?;
        let y = intcode.run_until_output()?;
        let tile_int = intcode.run_until_output()?;

        if x == -1 && y == 0 {
            println!("Score: {}", tile_int);
        } else {
            game.add_tile(x, y, tile_int)?;
        };

        execute!(stdout, cursor::MoveTo(0, 0));

        if print {
            println!("{}", game);
        };
    }
    Ok(())
}

fn main() {
    let input_str = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let input: Vec<i64> = input_str
        .trim()
        .split(",")
        .map(|x| x.parse::<i64>().unwrap())
        .collect();

    //    let mut intcode = Intcode::new(input.clone(), true);
    //    let mut game = Game::new();
    //    run_game(&mut intcode, &mut game, false).ok();
    //
    //    let p1_ans = game
    //        .tiles
    //        .iter()
    //        .filter(|((_, _), tile)| tile == &&Tile::Block)
    //        .count();
    //
    //    println!("Part 1 answer: {}", p1_ans);

    let mut intcode_p2 = Intcode::new(input.clone(), true);
    intcode_p2.replace(0, 2).ok();
    let mut game_p2 = Game::new();
    run_game(&mut intcode_p2, &mut game_p2, true).ok();
}
