use std::iter;

// a struct reresenting a coordinate
#[derive(Clone, Debug, Copy)]
struct Point {
  x: i64,
  y: i64,
}

// every movment along the spiral is proportional to one of these
static UNIT_VECS: &'static [Point] = &[ Point{ x:  1, y:  0},
                                        Point{ x:  0, y:  1},
                                        Point{ x: -1, y:  0},
                                        Point{ x:  0, y: -1}  ];

// scale a point by an integer
// e.g. 3*(4,5) -> (12, 15)
fn scale(p: Point, scale: i64) -> Point {
  Point { x: scale * p.x, y: scale * p.y }
}

// e.g. [(1, 2), (2, 3)] -> (3, 5)
fn sum_points(v: &Vec<Point>) -> Point {
  Point { x : v.iter().map(|coor| coor.x).sum(),
          y : v.iter().map(|coor| coor.y).sum() } 
}

// e.g. [(1, 2), (2, 3)] -> 8
fn sum_points_abs(v: &Vec<Point>) -> i64 {
  v.iter().map(|coor| coor.x.abs() + coor.y.abs() ).sum::<i64>()
}

// e.g. 5 -> [1, 1, 2, 2, 3]
fn get_scales(n: usize) -> Vec<i64> {
  // a little trick to divide and ceiling at the same time
  let div_ceil = n/2 + (n%2 != 0) as usize;

  // e.g. [1, 2, 3] -> [1, 1, 2, 2, 3, 3]  and drop the last
  Vec::from_iter(1..=div_ceil)
    .iter()
    .flat_map(|&x| iter::repeat(x as i64).take(2) )
    .take(n)
    .collect()
}

// get the nth "corner" of the spiral
fn get_corner(n: i64) -> Vec<Point> {
  // cycling through the unit vectors
  let cyc: Vec<&Point> = UNIT_VECS
                           .iter()
                           .cycle()
                           .take(n as usize)
                           .collect();

  // zip together the unit vectors and scales and multiply  
  get_scales(n as usize)
    .iter()
    .zip(cyc.iter())
    .map(|w| scale(**w.1, *w.0))
    .collect()
}

// get the corner past the value that we are seeking
fn get_corner_past(val: i64) -> i64 {
  let mut corner = 0;
  let mut dist   = 0;
  let mut scales: Vec<i64>;

  while dist < val {
    scales = get_scales(corner);
    corner += 1;
    dist = scales.iter().sum::<i64>() + 1 // adding one because of the spiral indexing
  }
  (corner-1) as i64
}

// after moving past the point we are seeking, come back
fn move_back(vp: &Vec<Point>, seek: i64) -> Point {
  let val_ahead  = sum_points_abs(vp) + 1;
  let idx_ahead  = sum_points(vp);
  let diff       = val_ahead - seek;                            // find how far ahead we are
  let ind        = (vp.len().rem_euclid(4) + 1).rem_euclid(4); // get unit vector to take us back
  let offset     = UNIT_VECS[ind as usize];                    
  Point { x: (idx_ahead.x + diff*offset.x), y: (idx_ahead.y + diff*offset.y) }
}

// given a value, find its index
fn get_index(val: i64) -> Point {
  let corner = get_corner_past(val);
  let p = get_corner(corner);
  move_back(&p, val)
}

fn main () {
  // puzzle input, the square we are looking for
  // panic if you provide less than one
  let input = 347991;
  let p1_index = get_index(input);
  println!("Part 1 answer: {:?}", sum_points_abs(&vec![p1_index]));
}
