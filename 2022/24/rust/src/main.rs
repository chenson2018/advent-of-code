use pathfinding::prelude::{bfs, Matrix};

#[derive(Debug)]
enum Blizzard {
    Up(usize, usize),
    Right(usize, usize),
    Down(usize, usize),
    Left(usize, usize),
}

// indicies are (row,col)
// opposite of what I usually do, be careful!

impl Blizzard {
    fn from_char(c: char, x: usize, y: usize) -> Option<Self> {
        match c {
            '<' => Some(Self::Left(x, y)),
            '>' => Some(Self::Right(x, y)),
            '^' => Some(Self::Up(x, y)),
            'v' => Some(Self::Down(x, y)),
            _ => None,
        }
    }

    fn advance(&self, y_max: usize, x_max: usize) -> Self {
        match self {
            &Self::Up(1, x) => Self::Up(y_max - 1, x),
            &Self::Up(y, x) => Self::Up(y - 1, x),
            &Self::Down(y, x) if y == y_max - 1 => Self::Down(1, x),
            &Self::Down(y, x) => Self::Down(y + 1, x),
            &Self::Right(y, x) if x == x_max - 1 => Self::Right(y, 1),
            &Self::Right(y, x) => Self::Right(y, x + 1),
            &Self::Left(y, 1) => Self::Left(y, x_max - 1),
            &Self::Left(y, x) => Self::Left(y, x - 1),
        }
    }

    fn get_idx(&self) -> (usize, usize) {
        match self {
            &Self::Down(y, x) | &Self::Up(y, x) | &Self::Right(y, x) | &Self::Left(y, x) => (y, x),
        }
    }
}

// given a vector of blizzards, indicies corresponding to time, extend to the required time

fn extend_blizz(n: usize, xs: &mut Vec<Vec<Blizzard>>, y_max: usize, x_max: usize) {
    while xs.iter().len() < (n + 1) {
        let last = xs.iter().last().unwrap();
        let next: Vec<Blizzard> = last.iter().map(|b| b.advance(y_max, x_max)).collect();
        xs.push(next);
    }
}

fn make_3d((x, y): (usize, usize), z: usize) -> (usize, usize, usize) {
    (x, y, z)
}

fn make_2d((x, y, _): (usize, usize, usize)) -> (usize, usize) {
    (x, y)
}

// `m` is the matrix of characters
// `blizzards` is a vector of blizzards, the indicies correspond to time
// `visit` is all points we'd like to visit

fn visit_multiple(
    m: &Matrix<char>,
    blizzards: &mut Vec<Vec<Blizzard>>,
    visit: Vec<(usize, usize)>,
) -> Option<usize> {
    let mut visit_iter = visit.iter();
    let mut start = make_3d(*visit_iter.next().unwrap(), 0);
    let mut visited: Vec<usize> = Vec::new();

    // for blizzard warping
    let (y_max, x_max) = m.keys().last().unwrap();

    loop {
        match visit_iter.next() {
            Some(next_end) => {
                let search = bfs(
                    &start,
                    |p| {
                        let (x, y, z) = *p;
                        let coor_2d = (x, y);

                        extend_blizz(z + 1, blizzards, y_max, x_max);

                        let mut neighbours: Vec<(usize, usize)> =
                            m.neighbours(coor_2d, false).collect();

                        // option to wait
                        neighbours.push(coor_2d);

                        // cannot enter wall
                        neighbours = neighbours.into_iter().filter(|x| m[*x] != '#').collect();

                        // cannot match a blizzard coordinate at time z+1
                        neighbours = neighbours
                            .into_iter()
                            .filter(|_| blizzards[z + 1].iter().all(|b| b.get_idx() != coor_2d))
                            .collect();

                        //cast to 3d
                        let neighbours_3d: Vec<(usize, usize, usize)> =
                            neighbours.iter().map(|c| make_3d(*c, z + 1)).collect();

                        neighbours_3d
                    },
                    |p| make_2d(*p) == *next_end,
                );

                match search {
                    Some(path) => {
                        start = *path.iter().last().unwrap();
                        visited.push(path.iter().len());
                    }
                    None => return None,
                }
            }
            None => {
                break;
            }
        }
    }

    // visited is number of nodes, including start/end, so we subtract 1 from all but the first
    Some(1 + visited.iter().fold(0, |acc, x| acc + x - 1))
}

fn main() {
    let input: Vec<Vec<char>> = std::fs::read_to_string("../input.txt")
        .expect("Unable to read file")
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect();

    // I'm only using the matrix for walls/open, blizzards are tracked seperately
    let m = Matrix::from_rows(input).unwrap();

    let blizzards_init: Vec<Blizzard> = m
        .keys()
        .filter_map(|(x, y)| Blizzard::from_char(m[(x, y)], x, y))
        .collect();

    let mut blizzards = vec![blizzards_init];

    // assumption about start/end being the last open
    let mut open_iter = m.keys().filter(|p| m[*p] == '.');
    let start = open_iter.next().unwrap();
    let end = open_iter.last().unwrap();

    let p1_ans = visit_multiple(&m, &mut blizzards, vec![start, end]);
    let p2_ans = visit_multiple(&m, &mut blizzards, vec![start, end, start, end]);

    println!("Part 1 answer: {:?}", p1_ans);
    println!("Part 2 answer: {:?}", p2_ans);
}
