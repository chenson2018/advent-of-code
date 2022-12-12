use pathfinding::prelude::bfs;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Pos(usize, usize);

impl Pos {
    fn successors(&self, grid: &Vec<Vec<u32>>) -> Vec<Pos> {
        let mut res: Vec<Pos> = Vec::new();
        let &Pos(x, y) = self;

        let neighbors: Vec<(isize, isize)> = vec![(1, 0), (0, 1), (-1, 0), (0, -1)];

        for (idx, idy) in neighbors {
            if (x == 0 && idx == -1) && (y == 0 && idy == -1) {
                continue;
            }

            let consider_x = ((x as isize) + idx) as usize;
            let consider_y = ((y as isize) + idy) as usize;

            if consider_x < grid.len() && consider_y < grid[0].len() {
                if grid[x][y] + 1 >= grid[consider_x][consider_y] {
                    res.push(Pos(consider_x, consider_y))
                }
            }
        }
        return res;
    }
}

fn main() {
    let grid: Vec<Vec<char>> = std::fs::read_to_string("../input.txt")
        .expect("Unable to read file")
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect();

    let height = grid.len();
    let width = grid[0].len();

    let mut start = Pos(0, 0);
    let mut end = Pos(0, 0);

    for i in 0..height {
        for j in 0..width {
            if grid[i][j] == 'S' {
                start = Pos(i, j);
            }

            if grid[i][j] == 'E' {
                end = Pos(i, j);
            }
        }
    }

    let final_grid: Vec<Vec<u32>> = grid
        .iter()
        .map(|row| {
            row.iter()
                .map(|c| match *c {
                    'S' => 'a' as u32,
                    'E' => 'z' as u32,
                    _ => *c as u32,
                })
                .collect::<Vec<u32>>()
        })
        .collect();

    let result = bfs(&start, |p| p.successors(&final_grid), |p| *p == end);

    // this is the nodes, so subtract one for paths
    println!(
        "Part 1 answer: {:?}",
        result.expect("no path found").len() - 1
    );

    // this relies on noticing that in my input, all the b are located in the second column
    let mut potential_p2_starts: Vec<Pos> = Vec::new();

    for row in 0..height {
        for col in [0, 2] {
            if final_grid[row][col] == 97 {
                potential_p2_starts.push(Pos(row, col))
            }
        }
    }

    let distances: Vec<usize> = potential_p2_starts
        .iter()
        .map(|s| {
            bfs(s, |p| p.successors(&final_grid), |p| *p == end)
                .expect("no path found")
                .len()
                - 1
        })
        .collect();

    println!("Part 2 answer: {:?}", distances.iter().min().unwrap());
}
