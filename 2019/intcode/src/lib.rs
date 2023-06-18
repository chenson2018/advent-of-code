#[derive(Debug)]
pub struct Intcode {
    idx: usize,
    ins: Vec<i64>,
}

enum Opcode {
    Add,
    Mult,
    Halt,
    Input,
    Output,
    Jt,
    Jf,
    Lt,
    Eq,
}

impl Opcode {
    fn width(&self) -> usize {
        match self {
            Opcode::Add => 4,
            Opcode::Mult => 4,
            Opcode::Halt => 1,
            Opcode::Input => 2,
            Opcode::Output => 2,
            Opcode::Jt => 3,
            Opcode::Jf => 3,
            Opcode::Lt => 4,
            Opcode::Eq => 4,
        }
    }

    fn advance(&self) -> bool {
        match self {
            Opcode::Halt | Opcode::Jt | Opcode::Jf => false,
            _ => true,
        }
    }
}

impl TryFrom<i64> for Opcode {
    type Error = String;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        match value % 100 {
            1 => Ok(Opcode::Add),
            2 => Ok(Opcode::Mult),
            3 => Ok(Opcode::Input),
            4 => Ok(Opcode::Output),
            5 => Ok(Opcode::Jt),
            6 => Ok(Opcode::Jf),
            7 => Ok(Opcode::Lt),
            8 => Ok(Opcode::Eq),
            99 => Ok(Opcode::Halt),
            _ => Err(format!("Opcode {} is invalid", value)),
        }
    }
}

enum Mode {
    Immediate,
    Position,
}

impl TryFrom<char> for Mode {
    type Error = String;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '0' => Ok(Mode::Position),
            '1' => Ok(Mode::Immediate),
            _ => Err(format!("Invalid mode indicator {:?}", value)),
        }
    }
}

impl Intcode {
    fn interpret_mode(&self, mode: Mode, value: i64) -> Result<i64, String> {
        match mode {
            Mode::Position => self.read_mem(value as usize),
            Mode::Immediate => Ok(value),
        }
    }

    pub fn replace(&mut self, idx: usize, value: i64) -> Result<(), String> {
        match self.ins.get_mut(idx) {
            Some(val) => {
                *val = value;
                Ok(())
            }
            None => Err(format!("Attempt to access invalid memory address {}.", idx)),
        }
    }

    pub fn new(ins: Vec<i64>) -> Self {
        Self { idx: 0, ins }
    }

    pub fn read_mem(&self, idx: usize) -> Result<i64, String> {
        match self.ins.get(idx) {
            Some(val) => Ok(*val),
            None => Err(format!("Attempt to access invalid memory address {}.", idx)),
        }
    }

    fn read_offset(&self, offset: usize) -> Result<i64, String> {
        match self.ins.get(self.idx + offset) {
            Some(val) => Ok(*val),
            None => Err(format!(
                "Attempt to access invalid memory address {}.",
                self.idx + offset
            )),
        }
    }

    // TODO fix the width case for better pattern matching

    fn step(&mut self) -> Result<bool, String> {
        let op_raw = self.read_mem(self.idx)?;
        let opcode: Opcode = op_raw.try_into()?;
        let width = opcode.width();

        let filled: Vec<char> = format!("{:0>5}", op_raw).chars().collect();

        let (c_mode, b_mode, a_mode): (Mode, Mode, Mode) = match filled.as_slice() {
            [c, b, a, _, _] => ((*c).try_into()?, (*b).try_into()?, (*a).try_into()?),
            _ => unreachable!(),
        };

        let mut optional_advance = false;

        if width == 1 {
            match opcode {
                Opcode::Halt => (),
                _ => unreachable!(),
            }
        } else if width == 2 {
            let a_raw = self.read_offset(1)?;
            let a = self.interpret_mode(a_mode, a_raw)?;

            match opcode {
                Opcode::Input => {
                    let mut line = String::new();
                    print!("\n> ");
                    std::io::Write::flush(&mut std::io::stdout()).expect("IO failure");
                    let _ = std::io::stdin().read_line(&mut line).expect("IO failure");

                    match line.trim().parse::<i64>() {
                        Ok(val) => {
                            self.replace(a_raw as usize, val)?;
                        }
                        Err(_) => return Err(format!("{} is not a valid integer.", line)),
                    }
                }
                Opcode::Output => {
                    println!("{}", a);
                }
                _ => unreachable!(),
            }
        } else if width == 3 {
            let a_raw = self.read_offset(1)?;
            let b_raw = self.read_offset(2)?;

            let a = self.interpret_mode(a_mode, a_raw)?;
            let b = self.interpret_mode(b_mode, b_raw)?;

            match opcode {
                Opcode::Jt => {
                    if a != 0 {
                        self.idx = b as usize;
                    } else {
                        optional_advance = true;
                    };
                }
                Opcode::Jf => {
                    if a == 0 {
                        self.idx = b as usize;
                    } else {
                        optional_advance = true;
                    };
                }
                _ => unreachable!(),
            }
        } else if width == 4 {
            let a_raw = self.read_offset(1)?;
            let b_raw = self.read_offset(2)?;

            let a = self.interpret_mode(a_mode, a_raw)?;
            let b = self.interpret_mode(b_mode, b_raw)?;

            let write_addr = self.read_offset(3)?;

            match opcode {
                Opcode::Add => {
                    self.replace(write_addr as usize, a + b)?;
                }
                Opcode::Mult => {
                    self.replace(write_addr as usize, a * b)?;
                }
                Opcode::Lt => {
                    let val = if a < b { 1 } else { 0 };
                    self.replace(write_addr as usize, val)?;
                }
                Opcode::Eq => {
                    let val = if a == b { 1 } else { 0 };
                    self.replace(write_addr as usize, val)?;
                }
                _ => unreachable!(),
            }
        } else {
            unreachable!()
        }

        if opcode.advance() || optional_advance {
            self.idx += width;
        }

        match opcode {
            Opcode::Halt => Ok(true),
            _ => Ok(false),
        }
    }

    pub fn run(&mut self) -> Result<bool, String> {
        loop {
            let step = self.step();
            match step {
                Err(_) | Ok(true) => return step,
                Ok(false) => (),
            }
        }
    }
}
