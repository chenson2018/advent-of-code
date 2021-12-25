use std::cell::Cell;
use std::fs;

#[derive(Debug)]
struct ALU {
  w: Cell<i64>,
  x: Cell<i64>,
  y: Cell<i64>,
  z: Cell<i64>,
  input: Vec<i64>,
  ptr: Cell<usize>,
  ins: Vec<Instruction>,
}

#[derive(Debug, Copy, Clone)]
struct Var(char);

#[derive(Debug, Copy, Clone)]
struct Val(i64);

#[derive(Debug, Copy, Clone)]
enum RightOperand {
  Var(Var),
  Val(Val),
}

#[derive(Debug, Copy, Clone)]
enum Instruction {
  Inp(Var),
  Add(Var, RightOperand),
  Mul(Var, RightOperand),
  Div(Var, RightOperand),
  Mod(Var, RightOperand),
  Eql(Var, RightOperand),
}

impl ALU {

  fn new(input: Vec<i64>, ins: Vec<Instruction>) -> ALU {
    ALU 
      { 
        w: Cell::new(0), 
        x: Cell::new(0),
        y: Cell::new(0), 
        z: Cell::new(0), 
        ptr: Cell::new(0), 
        input: input,
        ins: ins,
      }
  }

  fn get_cell(&self, variable: char) -> &Cell<i64> {
    match variable {
      'w' => &self.w,
      'x' => &self.x,
      'y' => &self.y,
      'z' => &self.z,
      _   => panic!("Invalid varible {}", variable),
    }
  }

  fn get(&self, variable: char) -> i64 {
    self.get_cell(variable).get()
  }

  fn set(&self, variable: char, value: i64) {
    self.get_cell(variable).set(value);
  }

  fn inp(&self, variable: Var) {
    let idx = self.ptr.get();
    self.ptr.set(idx + 1);

    match variable {
      Var(c) => self.set(c, self.input[idx]),
    }
  }

  fn add(&self, variable: Var, right: RightOperand) {
    match (variable, right) {
      (Var(c), RightOperand::Val(Val(x))) => self.set(c, self.get(c) +          x ),
      (Var(c), RightOperand::Var(Var(x))) => self.set(c, self.get(c) + self.get(x)),
    }
  }

  fn mul(&self, variable: Var, right: RightOperand) {
    match (variable, right) {
      (Var(c), RightOperand::Val(Val(x))) => self.set(c, self.get(c) *          x ),
      (Var(c), RightOperand::Var(Var(x))) => self.set(c, self.get(c) * self.get(x)),
    }
  }

  fn div(&self, variable: Var, right: RightOperand) {
    match (variable, right) {
      (Var(c), RightOperand::Val(Val(x))) => self.set(c, self.get(c) /          x ),
      (Var(c), RightOperand::Var(Var(x))) => self.set(c, self.get(c) / self.get(x)),
    }
  }

  fn r#mod(&self, variable: Var, right: RightOperand) {
    match (variable, right) {
      (Var(c), RightOperand::Val(Val(x))) => self.set(c, self.get(c).rem_euclid(         x )),
      (Var(c), RightOperand::Var(Var(x))) => self.set(c, self.get(c).rem_euclid(self.get(x))),
    }
  }

  fn eql(&self, variable: Var, right: RightOperand) {
    match (variable, right) {
      (Var(c), RightOperand::Val(Val(x))) => self.set(c, ( self.get(c) ==          x  )  as i64),
      (Var(c), RightOperand::Var(Var(x))) => self.set(c, ( self.get(c) == self.get(x) )  as i64),
    }
  }

  fn exec(&self, op: Instruction) {
    match op {
      Instruction::Inp(var     ) => self.inp(var,    ),
      Instruction::Add(var, val) => self.add(var, val),
      Instruction::Mul(var, val) => self.mul(var, val),
      Instruction::Div(var, val) => self.div(var, val),
      Instruction::Mod(var, val) => self.div(var, val),
      Instruction::Eql(var, val) => self.eql(var, val),
    }
  }

  fn exec_all(&self) {
    for i in self.ins.iter() {
      self.exec(*i);
    }
  }

}

impl Instruction {

  fn get_op(op: &str) -> Option<fn(Var, RightOperand) -> Instruction> {
    match op {
      "add" => Some(Instruction::Add),
      "mul" => Some(Instruction::Mul),
      "div" => Some(Instruction::Div),
      "mod" => Some(Instruction::Mod),
      "eql" => Some(Instruction::Eql),
      _     => None,
    }
  }

  fn from_str(s: &str) -> Option<Instruction> {
    let tokens: Vec<&str> = s.split(' ').collect();

    // note that for variables that I take just the first character... also dont check the register here

    match tokens[..] {
      [op, var     ] => match op {
                          "inp" => Some(Instruction::Inp(Var(var.chars().nth(0).unwrap()))),
                          _     => None,
                        },
      [op, var, val] => match Instruction::get_op(op) {
                          Some(f) => match val.parse::<i64>() { 
                                       Err(_)  => Some(f(Var(var.chars().nth(0).unwrap()), RightOperand::Var(Var(val.chars().nth(0).unwrap())))),
                                       Ok(num) => Some(f(Var(var.chars().nth(0).unwrap()), RightOperand::Val(Val(num                        )))),
                                     }
                          None    => None
                        }
      _              => None,
    }

  }

}

// See https://stackoverflow.com/a/41536521/11090784
fn digits(num: i64) -> Vec<i64> {
  num.to_string().chars().map(|d| d.to_digit(10).unwrap() as i64).collect()
}

fn main() {
  let ins: Vec<Instruction> = fs::read_to_string("../input.txt")
                                  .expect("Unable to read file")
                                  .lines()
                                  .map(|s| Instruction::from_str(s).unwrap() )
                                  .collect();

  let mut model: i64 = 99999999999999;
  let mut input =  digits(model);

  let mut alu = ALU::new(input, ins);
  alu.exec_all();
  println!("{:?}", alu);


//  alu = ALU::new(input, ins);
//  alu.exec_all();
//  println!("{:?}", alu);
}

