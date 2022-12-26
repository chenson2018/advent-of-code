use std::collections::VecDeque;

#[derive(Debug, Clone)]
struct Monkey {
    items: VecDeque<u64>,
    operation: fn(u64) -> u64,
    test: u64,
    true_monkey: u64,
    false_monkey: u64,
    inspected: u64,
}

fn monkey_rounds(monkeys: &mut Vec<Monkey>, div_three: bool, n: usize) -> u64 {
    // product of moduli will preserve our divisibility test
    // since we only care about counts, this is sufficient
    let prod = monkeys
        .iter()
        .map(|m| m.test)
        .fold(1, |acc, x| acc * x);

    for _ in 0..n {
        for i in 0..monkeys.len() {
            monkeys[i].inspected += monkeys[i].items.len() as u64;
            while monkeys[i].items.len() > 0 {
                let consider = monkeys[i].items.pop_front().unwrap();
                let op_applied = (monkeys[i].operation)(consider);
                let val_pass = if div_three {
                    op_applied / 3
                } else {
                    op_applied
                };
                let pass_monkey = if val_pass % monkeys[i].test == 0 {
                    monkeys[i].true_monkey
                } else {
                    monkeys[i].false_monkey
                };
                monkeys[pass_monkey as usize]
                    .items
                    .push_back(val_pass % prod);
            }
        }
    }
    let mut inspects = monkeys.iter().map(|m| m.inspected).collect::<Vec<u64>>();
    inspects.sort_by(|a, b| b.cmp(a));
    return inspects[0] * inspects[1];
}

fn main() {
    let monkeys = vec![
        Monkey {
            items: VecDeque::from([85, 77, 77]),
            operation: |old| -> u64 { old * 7 },
            test: 19,
            true_monkey: 6,
            false_monkey: 7,
            inspected: 0,
        },
        Monkey {
            items: VecDeque::from([80, 99]),
            operation: |old| -> u64 { old * 11 },
            test: 3,
            true_monkey: 3,
            false_monkey: 5,
            inspected: 0,
        },
        Monkey {
            items: VecDeque::from([74, 60, 74, 63, 86, 92, 80]),
            operation: |old| -> u64 { old + 8 },
            test: 13,
            true_monkey: 0,
            false_monkey: 6,
            inspected: 0,
        },
        Monkey {
            items: VecDeque::from([71, 58, 93, 65, 80, 68, 54, 71]),
            operation: |old| -> u64 { old + 7 },
            test: 7,
            true_monkey: 2,
            false_monkey: 4,
            inspected: 0,
        },
        Monkey {
            items: VecDeque::from([97, 56, 79, 65, 58]),
            operation: |old| -> u64 { old + 5 },
            test: 5,
            true_monkey: 2,
            false_monkey: 0,
            inspected: 0,
        },
        Monkey {
            items: VecDeque::from([77]),
            operation: |old| -> u64 { old + 4 },
            test: 11,
            true_monkey: 4,
            false_monkey: 3,
            inspected: 0,
        },
        Monkey {
            items: VecDeque::from([99, 90, 84, 50]),
            operation: |old| -> u64 { old * old },
            test: 17,
            true_monkey: 7,
            false_monkey: 1,
            inspected: 0,
        },
        Monkey {
            items: VecDeque::from([50, 66, 61, 92, 64, 78]),
            operation: |old| -> u64 { old + 3 },
            test: 2,
            true_monkey: 5,
            false_monkey: 1,
            inspected: 0,
        },
    ];

    let mut p1_monkeys = monkeys.clone();
    let mut p2_monkeys = monkeys.clone();

    let p1_ans = monkey_rounds(&mut p1_monkeys, true, 20);
    let p2_ans = monkey_rounds(&mut p2_monkeys, false, 10000);

    println!("Part 1 answer: {:?}", p1_ans);
    println!("Part 2 answer: {:?}", p2_ans);
}
