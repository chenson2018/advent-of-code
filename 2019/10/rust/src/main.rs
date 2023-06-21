use std::collections::HashSet;

use fraction::Fraction;
use fraction::Sign;

#[derive(Hash, PartialEq, Eq)]
enum Sight {
    Up,
    Down,
    Right(Fraction),
    Left(Fraction),
}

fn main() {
    let input_str = std::fs::read_to_string("../input.txt").expect("Unable to read file");

    let mat: Vec<(isize, isize)> = input_str
        .lines()
        .enumerate()
        .map(|(y, line)| {
            line.chars().enumerate().filter_map(move |(x, c)| {
                if c == '#' {
                    Some((x as isize, y as isize))
                } else {
                    None
                }
            })
        })
        .flatten()
        .collect();

    let slopes: &Vec<usize> = &mat[..]
        .iter()
        .map(|(station_x, station_y)| {
            let slopes: &HashSet<Sight> = &mat[..]
                .iter()
                .filter_map(|(asteroid_x, asteroid_y)| {
                    if station_x == asteroid_x && station_y == asteroid_y {
                        None
                    } else {
                        if let Some(frac) = Fraction::new_generic(
                            Sign::Plus,
                            station_y - asteroid_y,
                            station_x - asteroid_x,
                        ) {
                            if asteroid_x > station_x {
                                Some(Sight::Right(frac))
                            } else {
                                Some(Sight::Left(frac))
                            }
                        } else {
                            if asteroid_x > station_x {
                                Some(Sight::Up)
                            } else {
                                Some(Sight::Down)
                            }
                        }
                    }
                })
                .collect();
            slopes.len()
        })
        .collect();

    println!("Part 1 answer: {}", slopes.iter().max().unwrap());

    // TODO part 2...
    // get the  (x,y) -> slope relative to the station
    // order by angle, then by distance
    // flatten and take requested index
}
