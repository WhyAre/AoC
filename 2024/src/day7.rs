use std::io::{self, BufRead};
use std::iter::zip;

fn is_possible(eqn: &Vec<i64>, index: usize, ans: i64) -> bool {
    if index == 0 {
        return ans == eqn[index];
    }

    // Try all operators
    let num = eqn[index]; // Decide what can be done with this number
    let mut res = false;

    // Multiply
    if ans % num == 0 {
        res |= is_possible(eqn, index - 1, ans / num);
    }

    // Addition
    if ans - num >= 0 {
        res |= is_possible(eqn, index - 1, ans - num);
    }

    res
}

fn is_digits_same(num1: i64, num2: i64, num_digits: u32) -> bool {
    let mask = 10i64.pow(num_digits);

    num1 % mask == num2 % mask
}

fn is_possible_p2(eqn: &Vec<i64>, index: usize, ans: i64) -> bool {
    if index == 0 {
        return ans == eqn[index];
    }

    // Try all operators
    let num = eqn[index]; // Decide what can be done with this number
    let mut res = false;
    if ans % num == 0 {
        res |= is_possible_p2(eqn, index - 1, ans / num);
    }

    if ans - num >= 0 {
        res |= is_possible_p2(eqn, index - 1, ans - num);
    }

    let num_digits = num.checked_ilog10().unwrap_or(0) + 1;
    if is_digits_same(ans, num, num_digits) {
        res |= is_possible_p2(eqn, index - 1, ans / 10i64.pow(num_digits));
    }

    res
}
fn main() {
    let (ans, eqns): (Vec<i64>, Vec<Vec<i64>>) = io::stdin()
        .lock()
        .lines()
        .map(|x| {
            let line = x.unwrap();
            let mut line = line.split_whitespace();

            let ans = line.next().unwrap();
            let ans: i64 = (&ans[..ans.len() - 1]).parse().unwrap();

            (ans, line.map(|x| x.parse().unwrap()).collect::<Vec<_>>())
        })
        .unzip();

    let p1 = zip(ans.clone(), eqns.clone())
        .filter(|(a, e)| is_possible(e, e.len() - 1, *a))
        .map(|(a, _)| a)
        .sum::<i64>();
    println!("Part 1: {}", p1);

    let ans = zip(ans, eqns)
        .filter(|(a, e)| is_possible_p2(e, e.len() - 1, *a))
        .map(|(a, _)| a)
        .sum::<i64>();
    println!("Part 2: {}", ans);
}
