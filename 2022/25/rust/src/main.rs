#![allow(non_snake_case)]
use std::cmp::max;

fn valid_SNAFU_char(c: char) -> bool {
    match c {
        '2' | '1' | '0' | '-' | '=' => true,
        _ => false,
    }
}

// I check good SNAFU values passed so that I can do a nice recursion
fn SNAFU_add_single(l: char, r: char) -> [char; 2] {
    if !(valid_SNAFU_char(l) && valid_SNAFU_char(r)) {
        panic!("Invalid SNAFU characters");
    }

    match (l, r) {
        ('=', '=') => ['-', '1'],
        ('-', '=') => ['-', '2'],
        ('0', '=') | ('-', '-') => ['0', '='],
        ('1', '=') | ('0', '-') => ['0', '-'],
        ('2', '=') | ('1', '-') | ('0', '0') => ['0', '0'],
        ('2', '-') | ('1', '0') => ['0', '1'],
        ('2', '0') | ('1', '1') => ['0', '2'],
        ('2', '1') => ['1', '='],
        ('2', '2') => ['1', '-'],
        _ => SNAFU_add_single(r, l),
    }
}

fn add_SNAFU(l: &String, r: &String) -> String {
    let len = max(l.chars().count(), r.chars().count());

    let l_pad = format!("{:0>len$}", l);
    let r_pad = format!("{:0>len$}", r);

    let mut l_iter = l_pad.chars().rev();
    let mut r_iter = r_pad.chars().rev();
    let mut res: Vec<char> = Vec::new();
    let mut carry = '0';

    loop {
        match (l_iter.next(), r_iter.next()) {
            (Some(lc), Some(rc)) => {
                let [carry_1, val_1] = SNAFU_add_single(lc, rc);
                let [carry_2, val_2] = SNAFU_add_single(val_1, carry);
                let [_, val_3] = SNAFU_add_single(carry_1, carry_2);
                carry = val_3;
                res.push(val_2);
            }
            (Some(_), None) | (None, Some(_)) => panic!("SNAFU padding failed."),
            (None, None) => {
                if carry != '0' {
                    res.push(carry);
                }
                return res.into_iter().rev().collect();
            }
        }
    }
}

fn main() {
    //let input: Vec<String> = std::fs::read_to_string("../test.txt")
    let input: Vec<String> = std::fs::read_to_string("../input.txt")
        .expect("Unable to read file")
        .lines()
        .map(|s| s.to_string())
        .collect();

    let p1_ans: String = input
        .iter()
        .fold('0'.to_string(), |acc, x| add_SNAFU(&acc, &x.to_string()));

    println!("Part 1 answer: {}", p1_ans);
}
