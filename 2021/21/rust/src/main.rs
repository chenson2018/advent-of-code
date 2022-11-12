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
}

fn main() {
    let mut game = Game::new(4, 1);
    let mut deterministic = (1..=100).cycle();

    while game.playing() {
        let roll: u32 = deterministic.by_ref().take(3).sum();
        game.turn(roll);
    }

    println!(
        "Part 1 answer: {:?}",
        game.rolls * min(game.p1.score, game.p2.score)
    );
}
