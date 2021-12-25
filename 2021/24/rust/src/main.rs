use std::cell::Cell;

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

fn main() {
  let alu = ALU::new(vec![88], vec![Instruction::Inp(Var('x')), Instruction::Mul(Var('x'), RightOperand::Val(Val(-1)))]);
  alu.exec_all();
  println!("{:?}", alu);

  
}
