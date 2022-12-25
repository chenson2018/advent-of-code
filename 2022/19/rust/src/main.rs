use good_lp::*;
use std::iter::zip;

enum Part {
    Part1,
    Part2,
}

// input is the seven numbers that define a blueprint
fn max_geodes(nums: Vec<f64>, time: usize, part: Part) -> f64 {
    let minutes = time + 1;
    let mut it = nums.into_iter();

    let id = it.next().unwrap();
    let ore_cost_ore = it.next().unwrap();
    let clay_cost_ore = it.next().unwrap();
    let obsidian_cost_ore = it.next().unwrap();
    let obsidian_cost_clay = it.next().unwrap();
    let geode_cost_ore = it.next().unwrap();
    let geode_cost_obsidian = it.next().unwrap();

    variables! {
        vars:
          build_ore_robot      [minutes] (binary);
          build_clay_robot     [minutes] (binary);
          build_obsidian_robot [minutes] (binary);
          build_geode_robot    [minutes] (binary);

          ore_robot      [minutes] (integer);
          clay_robot     [minutes] (integer);
          obsidian_robot [minutes] (integer);
          geode_robot    [minutes] (integer);

          ore      [minutes] (integer);
          clay     [minutes] (integer);
          obsidian [minutes] (integer);
          geode    [minutes] (integer);
    }

    let mut problem = vars.maximise(geode[minutes - 1]).using(default_solver);
    problem.set_parameter("log", "0");

    // at every time, we build at most one robot
    for min in 0..minutes {
        problem.add_constraint(constraint!(
            build_ore_robot[min]
                + build_clay_robot[min]
                + build_obsidian_robot[min]
                + build_geode_robot[min]
                <= 1
        ));

        let resources = [&ore, &clay, &obsidian, &geode];
        let robots = [&ore_robot, &clay_robot, &obsidian_robot, &geode_robot];
        let build_decisions = [
            &build_ore_robot,
            &build_clay_robot,
            &build_obsidian_robot,
            &build_geode_robot,
        ];

        let cost_mat = vec![
            vec![
                ore_cost_ore,
                clay_cost_ore,
                obsidian_cost_ore,
                geode_cost_ore,
            ],
            vec![0., 0., obsidian_cost_clay, 0.],
            vec![0., 0., 0., geode_cost_obsidian],
            vec![0., 0., 0., 0.],
        ];

        if min == 0 {
            // start with zero resources
            for resource in resources.iter() {
                problem.add_constraint(constraint!(resource[min] == 0));
            }

            // start with one ore robot
            problem.add_constraint(constraint!(ore_robot[min] == 1));

            for robot in robots.iter().skip(1) {
                problem.add_constraint(constraint!(robot[min] == 0));
            }

            // cannot build on 0th minute
            for build_decision in build_decisions.iter() {
                problem.add_constraint(constraint!(build_decision[min] == 0));
            }
        } else {
            // robots at end of minute are previous minute robots plus build decision
            for (robot, build_decision) in zip(robots, build_decisions) {
                problem.add_constraint(constraint!(
                    robot[min] == robot[min - 1] + build_decision[min]
                ));
            }

            for ((resource, costs), robot) in zip(zip(resources, cost_mat), robots) {
                // current ore + what robots bring
                let mut resource_level = resource[min - 1] + robot[min - 1];

                for (cost, build_decision) in zip(costs, build_decisions) {
                    // do we have enough materials to build?
                    problem.add_constraint(constraint!(
                        resource[min - 1] >= cost * build_decision[min]
                    ));
                    // subtract ore we use to build
                    resource_level -= cost * build_decision[min];
                }
                problem.add_constraint(constraint!(resource[min] == resource_level));
            }
        }
    }

    let solution = problem.solve().unwrap();
    let geodes = solution.value(geode[minutes - 1]);

    match part {
        Part::Part1 => id * geodes,
        Part::Part2 => geodes,
    }
}

fn main() {
    let input: Vec<Vec<f64>> = std::fs::read_to_string("../input.txt")
        .expect("Unable to read file")
        .lines()
        .map(|line| {
            line.split(&[' ', ':'])
                .filter_map(|x| x.parse::<f64>().ok())
                .collect::<Vec<f64>>()
        })
        .collect();

    let p1_ans: f64 = input
        .iter()
        .map(|xs| max_geodes(xs.to_vec(), 24, Part::Part1))
        .sum();
    println!("Part 1 answer: {}", p1_ans);

    let p2_ans = input
        .into_iter()
        .take(3)
        .map(|xs| max_geodes(xs.to_vec(), 32, Part::Part2))
        .fold(1., |acc, x| acc * x);

    println!("Part 2 answer: {}", p2_ans);
}
