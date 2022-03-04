use std::cell::Cell;
use std::fs;

#[derive(Debug)]
struct Assembunny<'a> {
  a: Cell<i64>,
  b: Cell<i64>,
  c: Cell<i64>,
  d: Cell<i64>,
  ptr: Cell<i64>,
  ins: &'a Vec<Instruction>,
}

impl Assembunny<'_> {
  fn new(ins: &Vec<Instruction>) -> Assembunny<'_> {
    Assembunny
      { 
        a: Cell::new(0), 
        b: Cell::new(0),
        c: Cell::new(0), 
        d: Cell::new(0), 
        ptr: Cell::new(0), 
        ins: ins,
      }
  }

  fn get_cell(&self, variable: char) -> &Cell<i64> {
    match variable {
      'a' => &self.a,
      'b' => &self.b,
      'c' => &self.c,
      'd' => &self.d,
      _   => panic!("Invalid varible {}", variable),
    }
  }

  fn get(&self, variable: char) -> i64 {
    self.get_cell(variable).get()
  }

  fn set(&self, variable: char, value: i64) {
    self.get_cell(variable).set(value);
  }

  fn exec(&self, op: Instruction) {
    match op {
      Instruction::Inc(Var(v)) => self.set(v, self.get(v) + 1),
      Instruction::Dec(Var(v)) => self.set(v, self.get(v) - 1),

      Instruction::Cpy(Operand::Var(Var(v1)), Var(v2)) => self.set(v2, self.get(v1)),
      Instruction::Cpy(Operand::Val(Val(v1)), Var(v2)) => self.set(v2,          v1 ),

      Instruction::Jnz(Operand::Var(Var(v1)), Val(v2)) => if self.get(v1) != 0 { self.ptr.set(self.ptr.get() + v2) } else { self.ptr.set(self.ptr.get() + 1) },
      Instruction::Jnz(Operand::Val(Val(v1)), Val(v2)) => if          v1  != 0 { self.ptr.set(self.ptr.get() + v2) } else { self.ptr.set(self.ptr.get() + 1) },
    }

    match op {
      Instruction::Jnz(..) => {},
      _                    => self.ptr.set(self.ptr.get() + 1),
    }

  }

  fn exec_all(&self) {
    while self.ptr.get() < (self.ins.len() as i64) {
      self.exec(self.ins[self.ptr.get() as usize])
    }
  }
}


#[derive(Debug, Copy, Clone)]
struct Var(char);

#[derive(Debug, Copy, Clone)]
struct Val(i64);

#[derive(Debug, Copy, Clone)]
enum Operand {
  Var(Var),
  Val(Val),
}

#[derive(Debug, Copy, Clone)]
enum Instruction {
  Cpy(Operand, Var),
  Inc(Var),
  Dec(Var),
  Jnz(Operand, Val),
}

impl Instruction {
  fn from_str(s: &str) -> Option<Instruction> {
    let tokens: Vec<&str> = s.split(' ').collect();

    match tokens[..] {
      [op, one]      => match op {
                          "inc" => Some(Instruction::Inc(Var(one.chars().nth(0).unwrap()))),
                          "dec" => Some(Instruction::Dec(Var(one.chars().nth(0).unwrap()))),
                          _     => None
                        },
      [op, one, two] => match one.parse::<i64>() {
                          Err(_)  => match op {
                                       "cpy" => Some(Instruction::Cpy(Operand::Var(Var(one.chars().nth(0).unwrap())), Var(two.chars().nth(0).unwrap()))),
                                       "jnz" => Some(Instruction::Jnz(Operand::Var(Var(one.chars().nth(0).unwrap())), Val(two.parse::<i64>().unwrap()))),
                                       _     => None
                                     },
                          Ok(num) => match op {
                                       "cpy" => Some(Instruction::Cpy(Operand::Val(Val(num)), Var(two.chars().nth(0).unwrap()))),
                                       "jnz" => Some(Instruction::Jnz(Operand::Val(Val(num)), Val(two.parse::<i64>().unwrap()))),
                                       _     => None
                                     }
                        }
     _     => None
    }
  }
}

fn main() {
  let ins: Vec<Instruction> = fs::read_to_string("../input.txt")
                                  .expect("Unable to read file")
                                  .lines()
                                  .map(|s| Instruction::from_str(s).unwrap() )
                                  .collect();


  let p1_bunny = Assembunny::new(&ins);
  p1_bunny.exec_all();
  println!("Part 1 answer: {:?}", p1_bunny.get('a'));

  let p2_bunny = Assembunny::new(&ins);
  p2_bunny.set('c', 1);
  p2_bunny.exec_all();
  println!("Part 2 answer: {:?}", p2_bunny.get('a'));
}
