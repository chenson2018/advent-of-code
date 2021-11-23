use std::iter;

// I refer to the two dimensional coordinates as an index,
// and the one dimensional index as a linear index
#[derive(Clone, Debug, Copy)]
struct Index {
  x: i64,
  y: i64,
}

// every movement along the spiral is proportional to one of these
static UNIT_VECS: &'static [Index] = &[ Index { x:  1, y:  0},
                                        Index { x:  0, y:  1},
                                        Index { x: -1, y:  0},
                                        Index { x:  0, y: -1}, ];

// scale an index by an integer
// e.g. 3*(4,5) -> (12, 15)
fn scale(scale: &i64, i: &Index) -> Index {
  Index { x: scale * i.x, y: scale * i.y }
}

// e.g. [(1, 2), (2, 3)] -> (3, 5)
fn sum_index(v: &Vec<Index>) -> Index {
  Index { x : v.iter().map(|Index{ x  , y:_ }| x).sum(),
          y : v.iter().map(|Index{ x:_, y   }| y).sum() } 
}

// e.g. [(1, -2), (2, -3)] -> 8
fn sum_index_abs(v: &Vec<Index>) -> i64 {
  v.iter().map(|Index{ x, y }| x.abs() + y.abs() ).sum::<i64>()
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

// get the number of the corner past the linear index that we are seeking
fn get_corner_past(lin_idx: &i64) -> i64 {
  let mut corner = 0;
  let mut dist   = 0;
  let mut scales: Vec<i64>;

  while dist < *lin_idx {
    scales = get_scales(corner);
    corner += 1;
    dist = scales.iter().sum::<i64>() + 1 // adding one because of the spiral indexing
  }
  (corner-1) as i64
}

// get the nth corner of the spiral as a vector of the scaled unit vectors used to travel there
fn get_corner(n: &i64) -> Vec<Index> {
  // cycling through the unit vectors
  let cyc: Vec<&Index> = UNIT_VECS
                           .iter()
                           .cycle()
                           .take(*n as usize)
                           .collect();

  // zip together the unit vectors and scales and multiply  
  get_scales(*n as usize)
    .iter()
    .zip(cyc.iter())
    .map(|(index, int)| scale(index, *int))
    .collect()
}

// after moving past the index we are seeking, come back
fn move_back(vp: &Vec<Index>, lin_idx: &i64) -> Index {
  let lin_idx_ahead = sum_index_abs(vp) + 1;                         // linear index of the corner ahead
  let idx_ahead     = sum_index(vp);                                // index of the corner ahead
  let diff          = lin_idx_ahead - lin_idx;                     // find how far ahead we are
  let ind           = (vp.len().rem_euclid(4) + 1).rem_euclid(4); // get unit vector to take us back
  let offset        = UNIT_VECS[ind as usize];                    
  Index { x: (idx_ahead.x + diff*offset.x), y: (idx_ahead.y + diff*offset.y) }
}

// given a linear index, find its index
fn get_index(lin_idx: i64) -> Index {
  let corner   = get_corner_past(&lin_idx); // the number of the corner past our target
  let corner_v = get_corner(&corner);      // vector of indices to get to the corner past our target
  move_back(&corner_v, &lin_idx)          // move back to the target
}

fn main () {
  // puzzle input, the linear index we are looking for
  // panic if you provide less than one
  let input  = 347991;
  let p1_idx = get_index(input);
  println!("Part 1 answer: {:?}", sum_index_abs(&vec![p1_idx]));
}
