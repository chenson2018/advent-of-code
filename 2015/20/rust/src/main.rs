use prime_factorization::Factorization;

fn presents_calc_p1(n: u32) -> u32 {
    let factor_repr = Factorization::run(n);
    let prime_tuples = factor_repr.prime_factor_repr();

    // see https://math.stackexchange.com/questions/163245/finding-sum-of-factors-of-a-number-using-prime-factorization

    let sum_factors = prime_tuples
        .iter()
        .fold(1, |acc, (factor, exponent)| {
            let powers : u32 = (0..=*exponent).map(|exp| factor.pow(exp) ).sum();
            acc * powers
        });

    sum_factors * 10
}

fn main() {
    let input = 36000000;

    let mut n = 1; 
    let mut p1_presents = 0;

    while p1_presents < input {
        p1_presents = presents_calc_p1(n);
        n += 1;
    }

    println!("Part 1 answer: {}", n-1);
}
