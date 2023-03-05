use prime_factorization::Factorization;

fn presents_calc_p1(n: u32) -> u32 {
    let factor_repr = Factorization::run(n);
    let prime_tuples = factor_repr.prime_factor_repr();

    // see https://math.stackexchange.com/questions/163245/finding-sum-of-factors-of-a-number-using-prime-factorization

    let sum_factors = prime_tuples.iter().fold(1, |acc, (factor, exponent)| {
        let powers: u32 = (0..=*exponent).map(|exp| factor.pow(exp)).sum();
        acc * powers
    });

    sum_factors * 10
}

fn all_factors(n: u32) -> Vec<u32> {
    let factor_repr = Factorization::run(n);
    let prime_tuples = factor_repr.prime_factor_repr();

    prime_tuples
        .iter()
        .fold(vec![1], |acc: Vec<u32>, (factor, exponent)| {
            acc.iter()
                .map(|int| (0..=*exponent).map(move |exp| int * factor.pow(exp)))
                .flatten()
                .collect()
        })
}

fn presents_calc_p2(n: u32) -> u32 {
    let factors = all_factors(n);
    11 * factors
        .iter()
        .filter(|x| (n as f64) / (**x as f64) <= 50.0)
        .sum::<u32>()
}

fn main() {
    let input = 36000000;

    let mut n = 1;
    let mut presents = 0;

    while presents < input {
        presents = presents_calc_p1(n);
        n += 1;
    }

    println!("Part 1 answer: {}", n - 1);

    n = 1;
    presents = 0;

    while presents < input {
        presents = presents_calc_p2(n);
        n += 1;
    }

    println!("Part 2 answer: {}", n - 1);
}
