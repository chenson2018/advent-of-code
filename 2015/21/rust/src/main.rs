use itertools::Itertools;

#[derive(Debug, Clone)]
struct Item {
    cost: isize,
    damage: isize,
    armor: isize,
}

impl Item {
    fn new(cost: isize, damage: isize, armor: isize) -> Self {
        Self {
            cost,
            damage,
            armor,
        }
    }
}

#[derive(Debug, Clone)]
struct Fighter {
    hp: isize,
    damage: isize,
    armor: isize,
}

impl Fighter {
    fn from_items<T>(items: T, hp: isize) -> Self
    where
        T: IntoIterator<Item = Item>,
    {
        let (damage, armor) = items.into_iter().fold((0, 0), |(damage, armor), item| {
            (damage + item.damage, armor + item.armor)
        });
        Self { hp, damage, armor }
    }
}

#[derive(Debug)]
struct Game {
    player: Fighter,
    enemy: Fighter,
    turn: usize,
}

impl Game {
    // true if the player wins
    fn fight_result(&mut self) -> bool {
        while self.enemy.hp > 0 && self.player.hp > 0 {
            let attacker = if self.turn % 2 == 0 {
                &self.player
            } else {
                &self.enemy
            };
            let defender = if self.turn % 2 == 1 {
                &self.player
            } else {
                &self.enemy
            };

            let damage = std::cmp::max(1, attacker.damage - defender.armor);

            // extra statement b/c borrow checker can't tell that attacker/defender is mut. exclusive
            if self.turn % 2 == 0 {
                self.enemy.hp -= damage;
            } else {
                self.player.hp -= damage;
            };

            self.turn += 1;
        }
        self.player.hp > 0
    }
}

fn main() {
    let weapons = [
        Some(Item::new(8, 4, 0)),
        Some(Item::new(10, 5, 0)),
        Some(Item::new(25, 6, 0)),
        Some(Item::new(40, 7, 0)),
        Some(Item::new(74, 8, 0)),
    ];

    let armor = [
        None,
        Some(Item::new(13, 0, 1)),
        Some(Item::new(31, 0, 2)),
        Some(Item::new(53, 0, 3)),
        Some(Item::new(75, 0, 4)),
        Some(Item::new(102, 0, 5)),
    ];

    let rings = [
        Some(Item::new(25, 1, 0)),
        Some(Item::new(50, 2, 0)),
        Some(Item::new(100, 3, 0)),
        Some(Item::new(20, 0, 1)),
        Some(Item::new(40, 0, 2)),
        Some(Item::new(80, 0, 3)),
    ];

    let enemy = Fighter {
        hp: 104,
        damage: 8,
        armor: 1,
    };

    let mut p1_ans = isize::MAX;
    let mut p2_ans = isize::MIN;

    for w in &weapons {
        for a in &armor {
            let no_ring = rings.clone().into_iter().combinations(0);
            let one_ring = rings.clone().into_iter().combinations(1);
            let two_ring = rings.clone().into_iter().combinations(2);

            for r in one_ring.chain(two_ring).chain(no_ring) {
                let combo = vec![vec![w.clone()], vec![a.clone()], r];
                let v: Vec<Item> = combo.iter().flatten().filter_map(|x| x.clone()).collect();
                let cost = v.iter().fold(0, |acc, item| acc + item.cost);
                let player = Fighter::from_items(v, 100);
                let mut game = Game {
                    player,
                    enemy: enemy.clone(),
                    turn: 0,
                };
                let win = game.fight_result();

                if win {
                    p1_ans = std::cmp::min(p1_ans, cost);
                } else {
                    p2_ans = std::cmp::max(p2_ans, cost);
                }
            }
        }
    }

    println!("Part 1 answer: {}", p1_ans);
    println!("Part 2 answer: {}", p2_ans);
}
