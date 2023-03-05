enum Part {
    Part1,
    Part2,
}

fn step(input: Vec<Vec<bool>>, part: Part) -> Vec<Vec<bool>> {
    let grid_size = input.len() as isize;
    let corners = [
        (0, 0),
        (0, grid_size - 1),
        (grid_size - 1, 0),
        (grid_size - 1, grid_size - 1),
    ];

    input
        .iter()
        .enumerate()
        .map(|(gx, line)| {
            line.iter()
                .enumerate()
                .map(|(gy, light)| {
                    let ix = gx as isize;
                    let iy = gy as isize;

                    let x_shift: Vec<usize> = [ix - 1, ix, ix + 1]
                        .iter()
                        .filter(|x| x >= &&0 && x < &&grid_size)
                        .map(|x| *x as usize)
                        .collect();
                    let y_shift: Vec<usize> = [iy - 1, iy, iy + 1]
                        .iter()
                        .filter(|y| y >= &&0 && y < &&grid_size)
                        .map(|y| *y as usize)
                        .collect();

                    let neighbors: usize = x_shift
                        .iter()
                        .map(|x| {
                            y_shift.iter().map(|y| {
                                if gx == *x && gy == *y {
                                    false
                                } else {
                                    input[*x][*y]
                                }
                            })
                        })
                        .flatten()
                        .filter(|x| *x)
                        .count();

                    let new_light = if *light {
                        neighbors == 2 || neighbors == 3
                    } else {
                        neighbors == 3
                    };

                    match part {
                        Part::Part1 => new_light,
                        Part::Part2 => corners.contains(&(ix, iy)) || new_light,
                    }
                })
                .collect::<Vec<bool>>()
        })
        .collect::<Vec<Vec<bool>>>()
}

fn main() {
    let input: Vec<Vec<bool>> = std::fs::read_to_string("../input.txt")
        .expect("Unable to read file")
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| match c {
                    '#' => true,
                    '.' => false,
                    _ => panic!("Invalid input {:?}", c),
                })
                .collect()
        })
        .collect();

    let n_steps = 100;

    let mut p1_state = input.clone();

    for _ in 0..n_steps {
        p1_state = step(p1_state, Part::Part1);
    }

    println!(
        "Part 1 answer: {:?}",
        p1_state.iter().flatten().filter(|x| **x).count()
    );

    let mut p2_state = input.clone();

    // light the initital corners

    let grid_size = p2_state.len();
    let corners = [
        (0, 0),
        (0, grid_size - 1),
        (grid_size - 1, 0),
        (grid_size - 1, grid_size - 1),
    ];

    for (x, y) in corners {
        p2_state[x][y] = true;
    }

    for _ in 0..n_steps {
        p2_state = step(p2_state, Part::Part2);
    }

    println!(
        "Part 2 answer: {:?}",
        p2_state.iter().flatten().filter(|x| **x).count()
    );
}
