use std::collections::VecDeque;

#[derive(Debug)]
struct Tree {
    child_quant: u16,
    meta_quant: u16,
    children: Vec<Tree>,
    metadata: Vec<u16>,
}

impl Tree {
    fn metadata_sum(&self) -> u16 {
        self.children
            .iter()
            .fold(0, |acc, child| acc + child.metadata_sum())
            + self.metadata.iter().sum::<u16>()
    }

    fn node_value(&self) -> u16 {
        if self.children.is_empty() {
            self.metadata_sum()
        } else {
            self.metadata
                .iter()
                .map(|idx| match self.children.get((*idx as usize) - 1) {
                    Some(child) => child.node_value(),
                    None => 0,
                })
                .sum()
        }
    }

    fn from_vec(
        values: &mut VecDeque<u16>,
        meta_queue: &mut Vec<u16>,
    ) -> Result<Tree, &'static str> {
        if let (Some(child_quant), Some(meta_quant)) = (values.pop_front(), values.pop_front()) {
            let mut res = Tree {
                child_quant,
                meta_quant,
                children: Vec::new(),
                metadata: Vec::new(),
            };

            meta_queue.push(meta_quant);

            for _ in 0..(child_quant as usize) {
                match Tree::from_vec(values, meta_queue) {
                    Ok(child) => {
                        res.children.push(child);
                    }
                    Err(e) => {
                        return Err(e);
                    }
                }
            }

            if let Some(meta_iter) = meta_queue.pop() {
                for _ in 0..(meta_iter as usize) {
                    if let Some(meta) = values.pop_front() {
                        res.metadata.push(meta);
                    } else {
                        return Err("Invalid format, ran out of metadata.");
                    }
                }
            } else {
                return Err("Invalid format, ran out of metadata.");
            }

            Ok(res)
        } else {
            Err("Invalid format, incomplete header.")
        }
    }
}

fn main() {
    let input_str = std::fs::read_to_string("../input.txt").expect("Unable to read file");
    let mut input: VecDeque<u16> = input_str
        .trim()
        .split(" ")
        .map(|x| x.parse::<u16>().unwrap())
        .collect();

    let tree = Tree::from_vec(&mut input, &mut Vec::new()).unwrap();

    println!("Part 1 answer: {:?}", tree.metadata_sum());
    println!("Part 2 answer: {:?}", tree.node_value());
}
