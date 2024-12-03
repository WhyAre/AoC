use regex::Regex;
use std::io::{self, BufRead};
use std::str;

fn part1(str: &str) -> i64 {
    let re = Regex::new(r"mul\(([0-9]{1,3}),([0-9]{1,3})\)").unwrap();

    re.captures_iter(str)
        .map(|m| {
            let [a, b]: [i64; 2] = m.extract().1.map(|x| x.parse().unwrap());
            a * b
        })
        .sum()
}

fn part2(str: &str) -> i64 {
    // This is a bit diff than the above regex cos it starts with '^'
    let re = Regex::new(r"^mul\(([0-9]{1,3}),([0-9]{1,3})\)").unwrap();

    let mut sum = 0;
    let mut enabled = true;

    let mut i = 0usize;
    while i < str.len() {
        let s = str.get(i..str.len()).unwrap();
        // dbg!(s);

        if s.starts_with("do()") {
            enabled = true;
            i += "do()".len();
            continue;
        }

        if s.starts_with("don't()") {
            enabled = false;
            i += "don't()".len();
            continue;
        }

        let matches = re.captures(s);

        if let Some(capture) = matches {
            let (full_str, vals) = capture.extract();
            let [a, b]: [i64; 2] = vals.map(|x| x.parse().unwrap());

            if enabled {
                sum += a * b;
            }

            i += full_str.len();
            continue;
        }

        i += 1;
    }
    sum
}

fn main() {
    let v: Vec<String> = io::stdin().lock().lines().map(|x| x.unwrap()).collect();

    // let str = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";
    let ans = v.iter().map(|x| part1(x)).sum::<i64>();
    println!("Part 1: {}", ans);

    let str = v.join("");
    let ans = part2(&str);
    println!("Part 2: {}", ans);
}
