use std::iter::zip;

#[derive(Debug, Clone)]
struct Moon {
    x_pos: isize,
    y_pos: isize,
    z_pos: isize,
    x_vel: isize,
    y_vel: isize,
    z_vel: isize,
}

impl Moon {
    fn potential_energy(&self) -> isize {
        self.x_pos.abs() + self.y_pos.abs() + self.z_pos.abs()
    }

    fn kinetic_energy(&self) -> isize {
        self.x_vel.abs() + self.y_vel.abs() + self.z_vel.abs()
    }

    fn total_energy(&self) -> isize {
        self.kinetic_energy() * self.potential_energy()
    }

    fn new(x_pos: isize, y_pos: isize, z_pos: isize) -> Self {
        Moon {
            x_pos,
            y_pos,
            z_pos,
            x_vel: 0,
            y_vel: 0,
            z_vel: 0,
        }
    }

    fn velocity_updates(&self, other: &Moon) -> (isize, isize, isize) {
        let x_update = if self.x_pos == other.x_pos {
            0
        } else if self.x_pos < other.x_pos {
            1
        } else {
            -1
        };
        let y_update = if self.y_pos == other.y_pos {
            0
        } else if self.y_pos < other.y_pos {
            1
        } else {
            -1
        };
        let z_update = if self.z_pos == other.z_pos {
            0
        } else if self.z_pos < other.z_pos {
            1
        } else {
            -1
        };
        (x_update, y_update, z_update)
    }
}

#[derive(Debug, Clone)]
struct Universe {
    moons: Vec<Moon>,
    step: usize,
}

impl Universe {
    fn step(&mut self) {
        let updates: Vec<(isize, isize, isize)> = self
            .moons
            .iter()
            .map(|moon_outer| {
                self.moons
                    .iter()
                    .map(|moon_inner| moon_outer.velocity_updates(moon_inner))
                    .fold((0, 0, 0), |(ax, ay, az), (x, y, z)| {
                        (ax + x, ay + y, az + z)
                    })
            })
            .collect();

        for (moon_ref, (x, y, z)) in zip(&mut self.moons, updates) {
            moon_ref.x_vel += x;
            moon_ref.y_vel += y;
            moon_ref.z_vel += z;

            moon_ref.x_pos += moon_ref.x_vel;
            moon_ref.y_pos += moon_ref.y_vel;
            moon_ref.z_pos += moon_ref.z_vel;
        }

        self.step += 1;
    }

    fn step_n(&mut self, n: usize) {
        for _ in 0..n {
            self.step()
        }
    }

    //    fn rep_x(&mut self) -> (usize, usize) {
    //        let mut state: Vec<Vec<Moon>> = Vec::new();
    //
    //        loop {
    //            match state.iter().position(|s| zip(self.moons.clone(), s).all(|(a, b)| a.x_pos == b.x_pos && a.x_vel == b.x_vel)  ) {
    //                Some(idx) => { return (idx, self.step); },
    //                None => { state.push(self.moons.clone()); }
    //            }
    //            self.step();
    //        }
    //
    //    }
    //
    //    fn rep_y(&mut self) -> (usize, usize) {
    //        let mut state: Vec<Vec<Moon>> = Vec::new();
    //
    //        loop {
    //            match state.iter().position(|s| zip(self.moons.clone(), s).all(|(a, b)| a.y_pos == b.y_pos && a.y_vel == b.y_vel)  ) {
    //                Some(idx) => { return (idx, self.step); },
    //                None => { state.push(self.moons.clone()); }
    //            }
    //            self.step();
    //        }
    //
    //    }
    //
    //    fn rep_z(&mut self) -> (usize, usize) {
    //        let mut state: Vec<Vec<Moon>> = Vec::new();
    //
    //        loop {
    //            match state.iter().position(|s| zip(self.moons.clone(), s).all(|(a, b)| a.z_pos == b.z_pos && a.z_vel == b.z_vel)  ) {
    //                Some(idx) => { return (idx, self.step); },
    //                None => { state.push(self.moons.clone()); }
    //            }
    //            self.step();
    //        }
    //
    //    }

    fn total_energy(&self) -> isize {
        self.moons
            .iter()
            .fold(0, |acc, moon| acc + moon.total_energy())
    }
}

fn main() {
    //    let u = Universe {
    //        step: 0,
    //        moons: vec![
    //            Moon::new(-1, 0, 2),
    //            Moon::new(2, -10, -7),
    //            Moon::new(4, -8, 8),
    //            Moon::new(3, 5, -1),
    //        ],
    //    };

    let u = Universe {
        step: 0,
        moons: vec![
            Moon::new(5, 4, 4),
            Moon::new(-11, -11, -3),
            Moon::new(0, 7, 0),
            Moon::new(-13, 2, 10),
        ],
    };

    let p1_ans = {
        let mut uni = u.clone();
        uni.step_n(1000);
        uni.total_energy()
    };

    println!("Part 1 answer: {}", p1_ans);

    // TODO I see that each axis is independent, but there must be something else...
    //
    //    let x = {
    //        let mut uni = u.clone();
    //        uni.rep_x()
    //    };
    //
    //    let y = {
    //        let mut uni = u.clone();
    //        uni.rep_y()
    //    };
    //
    //    let z = {
    //        let mut uni = u.clone();
    //        uni.rep_z()
    //    };
    //
    //   println!("Repeat x: {:?}", x);
    //   println!("Repeat y: {:?}", y);
    //   println!("Repeat z: {:?}", z);
}
