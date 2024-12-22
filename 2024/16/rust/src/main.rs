use pathfinding::prelude::dijkstra;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum Dir {
    N,
    S,
    E,
    W,
}

impl Dir {
    fn clockwise(&self) -> Dir {
        match self {
            Dir::N => Dir::E,
            Dir::E => Dir::S,
            Dir::S => Dir::W,
            Dir::W => Dir::N,
        }
    }

    fn counterclockwise(&self) -> Dir {
        self.clockwise().clockwise().clockwise()
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct Coor {
    x: usize,
    y: usize,
    dir: Dir,
}

impl Coor {
    fn forward(&self) -> Self {
        match self {
            Coor { x, dir: Dir::N, .. } => Coor { x: x - 1, ..*self },
            Coor { x, dir: Dir::S, .. } => Coor { x: x + 1, ..*self },
            Coor { y, dir: Dir::W, .. } => Coor { y: y - 1, ..*self },
            Coor { y, dir: Dir::E, .. } => Coor { y: y + 1, ..*self },
        }
    }

    fn successors(&self, grid: &Vec<Vec<char>>) -> Vec<(Self, usize)> {
        vec![
            (self.forward(), 1),
            (
                Coor {
                    dir: self.dir.clockwise(),
                    ..*self
                },
                1000,
            ),
            (
                Coor {
                    dir: self.dir.counterclockwise(),
                    ..*self
                },
                1000,
            ),
        ]
        .into_iter()
        .filter(|(Coor { x, y, .. }, _)| grid[*x][*y] != '#')
        .collect()
    }
}

fn main() {
    let grid: Vec<Vec<char>> = std::fs::read_to_string("../input/input.txt")
        .expect("Unable to read file")
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect();

    let mut start = Coor {
        x: 0,
        y: 0,
        dir: Dir::E,
    };
    let mut end = Coor {
        x: 0,
        y: 0,
        dir: Dir::E,
    };

    for x in 0..grid.len() {
        for y in 0..grid[0].len() {
            if grid[x][y] == 'S' {
                start.x = x;
                start.y = y;
            }
            if grid[x][y] == 'E' {
                end.x = x;
                end.y = y;
            }
        }
    }

    let (_, p1_ans) = dijkstra(
        &start,
        |c| c.successors(&grid),
        |Coor { x, y, .. }| end.x == *x && end.y == *y,
    )
    .unwrap();
    println!("Part 1 answer: {p1_ans}");
}
