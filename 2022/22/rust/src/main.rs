use std::cmp::max;
use std::iter;

#[derive(Debug, Clone, Copy)]
enum Orientation {
    Up,
    Right,
    Down,
    Left,
}

#[derive(Debug, Clone, Copy)]
enum Action {
    Forward(usize),
    RotateRight,
    RotateLeft,
}

impl Action {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "R" => Some(Self::RotateRight),
            "L" => Some(Self::RotateLeft),
            _ => match s.parse::<usize>() {
                Ok(val) => Some(Self::Forward(val)),
                Err(_) => None,
            },
        }
    }
}

impl Orientation {
    fn rotate(&self, action: &Action) -> Orientation {
        match action {
            Action::Forward(_) => *self,
            Action::RotateRight => match self {
                Orientation::Up => Orientation::Right,
                Orientation::Right => Orientation::Down,
                Orientation::Down => Orientation::Left,
                Orientation::Left => Orientation::Up,
            },
            Action::RotateLeft => match self {
                Orientation::Up => Orientation::Left,
                Orientation::Left => Orientation::Down,
                Orientation::Down => Orientation::Right,
                Orientation::Right => Orientation::Up,
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Location {
    x: usize,
    y: usize,
    orient: Orientation,
}

#[derive(Debug, Clone, Copy)]
enum Grid {
    Wall,
    Open,
    Null,
}

impl Grid {
    fn from_char(c: char) -> Option<Self> {
        match c {
            ' ' => Some(Grid::Null),
            '.' => Some(Grid::Open),
            '#' => Some(Grid::Wall),
            _ => None,
        }
    }
}

fn split_ins(s: &str) -> Vec<&str> {
    let mut result = Vec::new();
    let mut last = 0;
    for (index, matched) in s.match_indices(|c: char| c == 'R' || c == 'L') {
        if last != index {
            result.push(&s[last..index]);
        }
        result.push(matched);
        last = index + matched.len();
    }
    if last < s.len() {
        result.push(&s[last..]);
    }
    return result;
}

// furthest left to jump to
fn left_wrap_idx(line: &Vec<Grid>) -> usize {
    line.iter()
        .position(|g| match g {
            Grid::Null => false,
            _ => true,
        })
        .unwrap()
}

// furtherst right to jump to
fn right_wrap_idx(line: &Vec<Grid>) -> usize {
    line.len()
        - 1
        - line
            .iter()
            .rev()
            .position(|g| match g {
                Grid::Null => false,
                _ => true,
            })
            .unwrap()
}

fn get_col(grid: &Vec<Vec<Grid>>, idx: usize) -> Vec<Grid> {
    grid.iter().map(|row| row[idx]).collect()
}

enum Part {
    Part1,
    Part2,
}

impl Location {
    fn part1_warp_idx(&self, grid: &Vec<Vec<Grid>>) -> Self {
        match self.orient {
            Orientation::Right => {
                let new_x = left_wrap_idx(&grid[self.y]);
                Location { x: new_x, ..*self }
            }
            Orientation::Left => {
                let new_x = right_wrap_idx(&grid[self.y]);
                Location { x: new_x, ..*self }
            }
            Orientation::Up => {
                let col = get_col(&grid, self.x);
                let new_y = right_wrap_idx(&col);
                Location { y: new_y, ..*self }
            }
            Orientation::Down => {
                let col = get_col(&grid, self.x);
                let new_y = left_wrap_idx(&col);
                Location { y: new_y, ..*self }
            }
        }
    }

    fn action(&mut self, action: &Action, grid: &Vec<Vec<Grid>>, part: Part) {
        match action {
            Action::RotateRight | Action::RotateLeft => {
                self.orient = self.orient.rotate(action);
            }
            Action::Forward(num) => {
                for _ in 0..*num {
                    let next_idx = self.next_idx();
                    let next_grid = next_idx.get_grid(grid);

                    match next_grid {
                        Grid::Wall => {
                            break;
                        }
                        Grid::Open => {
                            *self = next_idx;
                        }
                        Grid::Null => {
                            let warp_next_idx = match part {
                                Part::Part1 => self.part1_warp_idx(grid),
                                Part::Part2 => todo!(),
                            };

                            let warp_grid = warp_next_idx.get_grid(grid);

                            match warp_grid {
                                Grid::Null => panic!("Warped into null!"),
                                Grid::Wall => {
                                    break;
                                }
                                Grid::Open => {
                                    *self = warp_next_idx;
                                }
                            };
                        }
                    }
                }
            }
        }
    }

    fn next_idx(&self) -> Location {
        match self.orient {
            Orientation::Up => Location {
                y: self.y - 1,
                ..*self
            },
            Orientation::Right => Location {
                x: self.x + 1,
                ..*self
            },
            Orientation::Down => Location {
                y: self.y + 1,
                ..*self
            },
            Orientation::Left => Location {
                x: self.x - 1,
                ..*self
            },
        }
    }

    fn get_grid(&self, grid: &Vec<Vec<Grid>>) -> Grid {
        grid[self.y][self.x]
    }

    fn score(&self) -> usize {
        let facing = match self.orient {
            Orientation::Up => 3,
            Orientation::Right => 0,
            Orientation::Down => 1,
            Orientation::Left => 2,
        };
        1000 * self.y + 4 * self.x + facing
    }
}

// Steps to pad with Null
// 1: fill end of lines with Null to match length
// 2: append and prepend each line with Null
// 3: add line of full Null to start/end

fn pad_grid(grid: &Vec<Vec<Grid>>) -> Vec<Vec<Grid>> {
    let mut middle = grid.to_vec();
    let max_width = middle.iter().fold(0, |acc, xs| max(acc, xs.len()));

    for line in middle.iter_mut() {
        let start = vec![Grid::Null];
        let end: Vec<Grid> = iter::repeat(Grid::Null)
            .take(max_width - line.len() + 1)
            .collect();
        *line = vec![start, line.to_vec(), end]
            .into_iter()
            .flatten()
            .collect();
    }

    let full_null: Vec<Grid> = iter::repeat(Grid::Null).take(max_width + 2).collect();

    let mut res: Vec<Vec<Grid>> = Vec::new();

    res.extend(vec![full_null.clone()]);
    res.extend(middle);
    res.extend(vec![full_null]);

    res
}

fn main() {
    let input = std::fs::read_to_string("../test.txt").expect("Unable to read file");
    //let input = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let (grid_txt, actions_txt) = input.split_once("\n\n").unwrap();

    let grid: Vec<Vec<Grid>> = grid_txt
        .lines()
        .map(|l| l.chars().map(|c| Grid::from_char(c).unwrap()).collect())
        .collect();

    let actions: Vec<Action> = split_ins(actions_txt.trim())
        .iter()
        .map(|s| Action::from_str(s).unwrap())
        .collect();

    let padded_grid = pad_grid(&grid);

    // first row is index 1 because of padding, then find the first open
    let init_x_idx = left_wrap_idx(&padded_grid[1]);

    let mut p1_loc = Location {
        x: init_x_idx,
        y: 1,
        orient: Orientation::Right,
    };

    for action in &actions {
        println!("");
        println!("Action: {:?}", action);
        p1_loc.action(&action, &padded_grid, Part::Part1);
        println!("New location: {:?}", p1_loc);
    }

    let p1_ans = p1_loc.score();
    println!("Part 1 answer: {}", p1_ans);

}
