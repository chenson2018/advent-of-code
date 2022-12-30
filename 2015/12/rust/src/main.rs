use serde_json;
use serde_json::Value::*;

fn part1(obj: &serde_json::Value) -> i64 {
    match obj {
        Object(obj) => obj.iter().map(|(_, v)| part1(v)).sum(),
        Array(arr) => arr.iter().map(|v| part1(v)).sum(),
        Number(num) => num.as_i64().unwrap(),
        String(_) => 0,
        _ => panic!(),
    }
}

fn part2(obj: &serde_json::Value) -> i64 {
    match obj {
        Object(obj) => {
            if obj.iter().any(|(_, v)| *v == String("red".to_string())) {
                0
            } else {
                obj.iter().map(|(_, v)| part2(v)).sum()
            }
        }
        Array(arr) => arr.iter().map(|v| part2(v)).sum(),
        Number(num) => num.as_i64().unwrap(),
        String(_) => 0,
        _ => panic!(),
    }
}

fn main() {
    let input = std::fs::read_to_string("../input.txt").expect("Unable to read file");
    let json: serde_json::Value = serde_json::from_str(&input).expect("Unable to parse");

    let p1_ans = part1(&json);
    let p2_ans = part2(&json);

    println!("Part 1 answer: {}", p1_ans);
    println!("Part 2 answer: {}", p2_ans);
}
