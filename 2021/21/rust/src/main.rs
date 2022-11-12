use itertools::Itertools;
use std::cmp::min;

#[derive(Debug)]
struct Player {
    position: u32,
    score: u32,
}

impl Player {
    pub fn new(position: u32) -> Self {
        Self { position, score: 0 }
    }

    fn advance(&mut self, n: u32) {
        self.position = (self.position + n) % 10;
        self.score = self.score + self.position;
        if self.position == 0 {
            self.score += 10
        };
    }
}

struct Game {
    p1: Player,
    p2: Player,
    rolls: u32,
}

impl Game {
    fn turn(&mut self, n: u32) {
        if self.rolls % 2 == 0 {
            self.p1.advance(n);
        } else {
            self.p2.advance(n);
        };
        self.rolls += 3;
    }

    pub fn new(n1: u32, n2: u32) -> Self {
        Self {
            p1: Player::new(n1),
            p2: Player::new(n2),
            rolls: 0,
        }
    }

    fn playing(&self) -> bool {
        (self.p1.score < 1000) && (self.p2.score < 1000)
    }

    fn game_deterministic(&mut self) {
        let mut deterministic = (1..=100).cycle();

        while self.playing() {
            let roll: u32 = deterministic.by_ref().take(3).sum();
            self.turn(roll);
        }
    }

    fn metric(&self) -> u32 {
        self.rolls * min(self.p1.score, self.p2.score)
    }
}

fn main() {
    let mut game = Game::new(4, 1);
    game.game_deterministic();
    println!("Part 1 answer: {:?}", game.metric());
    //let combo: Vec<_> = (1..=2).map(|_| 3..=9).multi_cartesian_product().collect();
    //println!("{:?}", combo);
}
