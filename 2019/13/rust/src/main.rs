use intcode::Intcode;
use std::collections::HashMap;

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

struct Game {
    tiles: HashMap<(i64, i64), Tile>,
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

fn main() {
    let input_str = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let input: Vec<i64> = input_str
        .trim()
        .split(",")
        .map(|x| x.parse::<i64>().unwrap())
        .collect();

    let mut intcode = Intcode::new(input, true);
    let mut game = Game::new();

    while !intcode.is_halted() {
        let x = intcode.run_until_output().unwrap();
        let y = intcode.run_until_output().unwrap();
        let tile_int = intcode.run_until_output().unwrap();
        game.add_tile(x, y, tile_int).ok();
    }

    let p1_ans = game
        .tiles
        .iter()
        .filter(|((_, _), tile)| tile == &&Tile::Block)
        .count();
    println!("Part 1 answer: {}", p1_ans);
}
