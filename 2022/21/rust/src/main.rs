use regex::Regex;
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Label(String);

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Add,
    Multiply,
    Subtract,
    Divide,
}

#[derive(Debug, Clone)]
pub enum Rhs {
    FreeVariable(Label),
    Number(isize),
    Label(Label),
    Expression(Box<Rhs>, Op, Box<Rhs>),
}

#[derive(Debug, Clone)]
pub struct Equation {
    lhs: Label,
    rhs: Rhs,
}

impl Op {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "+" => Some(Op::Add),
            "*" => Some(Op::Multiply),
            "-" => Some(Op::Subtract),
            "/" => Some(Op::Divide),
            _ => None,
        }
    }
}

impl Equation {
    fn from_str(s: &str) -> Option<Self> {
        let sep = Regex::new(r"[\s:]+").unwrap();
        let splits: Vec<&str> = sep.split(s).collect();

        if splits.len() == 2 {
            Some(Equation {
                lhs: Label(splits[0].to_string()),
                rhs: Rhs::Number(splits[1].parse().unwrap()),
            })
        } else if splits.len() == 4 {
            Some(Equation {
                lhs: Label(splits[0].to_string()),
                rhs: Rhs::Expression(
                    Box::new(Rhs::Label(Label(splits[1].to_string()))),
                    Op::from_str(splits[2]).unwrap(),
                    Box::new(Rhs::Label(Label(splits[3].to_string()))),
                ),
            })
        } else {
            None
        }
    }
}

impl Rhs {
    fn contains_free(&self) -> bool {
        match self {
            Rhs::Number(_) => false,
            Rhs::Label(_) => false,
            Rhs::FreeVariable(_) => true,
            Rhs::Expression(lbox, _, rbox) => lbox.contains_free() || rbox.contains_free(),
        }
    }

    fn evaluate(&self, table: &HashMap<Label, Rhs>) -> Rhs {
        match self {
            Rhs::FreeVariable(_) => panic!("Cannot evaluate a free variable."),
            Rhs::Number(_) => self.clone(),
            Rhs::Label(label) => Rhs::evaluate(&table.get(label).unwrap().clone(), table),
            Rhs::Expression(lbox, op, rbox) => {
                let eval_left = Rhs::evaluate(lbox, table);
                let eval_right = Rhs::evaluate(rbox, table);

                match (eval_left, eval_right) {
                    (Rhs::Number(n1), Rhs::Number(n2)) => match op {
                        Op::Add => Rhs::Number(n1 + n2),
                        Op::Multiply => Rhs::Number(n1 * n2),
                        Op::Subtract => Rhs::Number(n1 - n2),
                        Op::Divide => Rhs::Number(n1 / n2),
                    },
                    _ => panic!("Evaluation did not reach numerical values!"),
                }
            }
        }
    }

    // like the above, but don't evaluate numbers
    fn expand(&self, table: &HashMap<Label, Rhs>) -> Rhs {
        match self {
            Rhs::FreeVariable(_) => self.clone(),
            Rhs::Number(_) => self.clone(),
            Rhs::Label(label) => Rhs::expand(&table.get(label).unwrap().clone(), table),
            Rhs::Expression(lbox, op, rbox) => Rhs::Expression(
                Box::new(Rhs::expand(lbox, table)),
                *op,
                Box::new(Rhs::expand(rbox, table)),
            ),
        }
    }
}

// I make the assumption that only the left has a free variable
fn solve_equality(left: &Rhs, right: &Rhs, table: &HashMap<Label, Rhs>) -> Rhs {
    let expanded_left = left.expand(&table);
    let evaluated_right = right.evaluate(table);

    match expanded_left {
        Rhs::Expression(lbox, op, rbox) => {
            if (*lbox).contains_free() {
                match op {
                    Op::Add => solve_equality(
                        &*lbox,
                        &Rhs::Expression(Box::new(evaluated_right), Op::Subtract, rbox),
                        table,
                    ),
                    Op::Multiply => solve_equality(
                        &*lbox,
                        &Rhs::Expression(Box::new(evaluated_right), Op::Divide, rbox),
                        table,
                    ),
                    Op::Divide => solve_equality(
                        &*lbox,
                        &Rhs::Expression(Box::new(evaluated_right), Op::Multiply, rbox),
                        table,
                    ),
                    Op::Subtract => solve_equality(
                        &*lbox,
                        &Rhs::Expression(Box::new(evaluated_right), Op::Add, rbox),
                        table,
                    ),
                }
            } else {
                match op {
                    Op::Add => solve_equality(
                        &*rbox,
                        &Rhs::Expression(Box::new(evaluated_right), Op::Subtract, lbox),
                        table,
                    ),
                    Op::Multiply => solve_equality(
                        &*rbox,
                        &Rhs::Expression(Box::new(evaluated_right), Op::Divide, lbox),
                        table,
                    ),
                    Op::Divide => solve_equality(
                        &*rbox,
                        &Rhs::Expression(lbox, Op::Divide, Box::new(evaluated_right)),
                        table,
                    ),
                    Op::Subtract => solve_equality(
                        &*rbox,
                        &Rhs::Expression(lbox, Op::Subtract, Box::new(evaluated_right)),
                        table,
                    ),
                }
            }
        }
        Rhs::FreeVariable(_) => evaluated_right,
        Rhs::Number(_) | Rhs::Label(_) => {
            panic!("Left hand side simplified to an unevaluated label/number.")
        }
    }
}

fn main() {
    let equations: Vec<(Label, Rhs)> = std::fs::read_to_string("../input.txt")
        .expect("Unable to read file")
        .lines()
        .map(|s| {
            let eq = Equation::from_str(s).unwrap();
            (eq.lhs, eq.rhs)
        })
        .collect();

    let p1_hash: HashMap<Label, Rhs> = HashMap::from_iter(equations);

    let root = p1_hash.get(&Label("root".to_string())).unwrap();
    let p1_ans = root.evaluate(&p1_hash);
    println!("Part 1 answer: {:?}", p1_ans);

    let (lbox, rbox) = if let Rhs::Expression(lbox, _, rbox) = &root {
        (lbox, rbox)
    } else {
        panic!()
    };

    let mut p2_hash = p1_hash.clone();
    let human = "humn";

    p2_hash
        .entry(Label(human.to_string()))
        .and_modify(|x| *x = Rhs::FreeVariable(Label(human.to_string())));

    let p2_ans = solve_equality(&lbox, &rbox, &p2_hash);

    println!("Part 2 answer: {:?}", p2_ans);
}
