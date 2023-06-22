use std::collections::HashMap;
use std::collections::HashSet;

use fraction::Fraction;
use fraction::Sign;

#[derive(Hash, PartialEq, Eq, Debug)]
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

    let n_visible: &Vec<(isize, isize, HashMap<_, _>)> = &mat[..]
        .iter()
        .map(|(station_x, station_y)| {
            let mut groups: HashMap<Sight, Vec<(isize, isize)>> = HashMap::new();

            for (asteroid_x, asteroid_y) in &mat[..] {
                if station_x == asteroid_x && station_y == asteroid_y {
                    continue;
                };

                let sight = if let Some(frac) = Fraction::new_generic(
                    Sign::Plus,
                    station_y - asteroid_y,
                    station_x - asteroid_x,
                ) {
                    if asteroid_x > station_x {
                        Sight::Right(frac)
                    } else {
                        Sight::Left(frac)
                    }
                } else {
                    if asteroid_x > station_x {
                        Sight::Up
                    } else {
                        Sight::Down
                    }
                };

                groups
                    .entry(sight)
                    .or_default()
                    .push((*asteroid_x, *asteroid_y));
            }
            (*station_x, *station_y, groups)
        })
        .collect();

    let (station_x, station_y, groups) = n_visible
        .iter()
        .max_by_key(|(_, _, group)| group.len())
        .unwrap();
    println!("Part 1 answer: {:#?}", groups.len());

    //    println!("Station: {}, {}", station_x, station_y);
    //    println!("{:#?}", groups);

    // TODO part 2...
    // get the  (x,y) -> slope relative to the station
    // order by angle, then by distance (should be Vec<Vec<_>>)
    // pop one off each is the ordering
}
