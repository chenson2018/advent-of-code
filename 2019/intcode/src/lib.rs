#[derive(Debug)]
pub struct Intcode {
    idx: usize,
    ins: Vec<i64>,
}

enum Opcode {
    Add,
    Mult,
    Halt,
}

impl Opcode {
    fn width(&self) -> usize {
        match self {
            Opcode::Add => 4,
            Opcode::Mult => 4,
            Opcode::Halt => 1,
        }
    }
}

impl Intcode {
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

    fn step(&mut self) -> Result<bool, String> {
        let op_raw = self.read_mem(self.idx)?;
        let opcode: Opcode = op_raw.try_into()?;
        let width = opcode.width();

        let (ar, br, cr) = (
            self.read_offset(1),
            self.read_offset(2),
            self.read_offset(3),
        );
        let (mut a, mut b, mut c) = (0_i64, 0_i64, 0_i64);

        for (maybe_err, var) in
            [(ar, &mut a), (br, &mut b), (cr, &mut c)][..(width - 1) as usize].iter_mut()
        {
            match maybe_err {
                Err(e) => return Err(e.clone()),
                Ok(val) => {
                    **var = *val;
                }
            }
        }

        match opcode {
            Opcode::Add => {
                let val1 = self.read_mem(a as usize)?;
                let val2 = self.read_mem(b as usize)?;
                self.replace(c as usize, val1 + val2)?;
            }
            Opcode::Mult => {
                let val1 = self.read_mem(a as usize)?;
                let val2 = self.read_mem(b as usize)?;
                self.replace(c as usize, val1 * val2)?;
            }
            Opcode::Halt => (),
        }

        match opcode {
            Opcode::Halt => Ok(true),
            _ => {
                self.idx += width;
                Ok(false)
            }
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

impl TryFrom<i64> for Opcode {
    type Error = String;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Opcode::Add),
            2 => Ok(Opcode::Mult),
            99 => Ok(Opcode::Halt),
            _ => Err(format!("Opcode {} is invalid", value)),
        }
    }
}
