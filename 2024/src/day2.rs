use std::io::{self, BufRead};

fn is_safe(v: &Vec<i64>) -> bool {
    // Checks that the gap is okay
    let diff_property = v.windows(2).all(|x| {
        let absdiff = (x[0] - x[1]).abs();
        absdiff >= 1 && absdiff <= 3
    });

    if !diff_property {
        return false;
    }

    // Ensure that it's strictly increasing or decreasing by looking at
    // windows of 3 items
    v.windows(3).all(|x| (x[0] < x[1]) == (x[1] < x[2]))
}

fn is_almost_safe(v: &Vec<i64>) -> bool {
    for i in 0..(v.len() - 1) {
        // Look at windows of 2
        let diff = (v[i + 1] - v[i]).abs();

        if diff < 1 || diff > 3 {
            // Something is bad here
            // Either remove v[i] or v[i+1]
            let mut tmp = v.clone();

            for t in i..=i + 1 {
                tmp.remove(t);
                if is_safe(&tmp) {
                    return true;
                }
                tmp.insert(t, v[t]);
            }

            return false;
        }

        // Check window of 3
        if i + 2 < v.len() && (v[i] < v[i + 1]) != (v[i + 1] < v[i + 2]) {
            // Something is bad here
            let mut tmp = v.clone();
            for t in i..=i + 2 {
                tmp.remove(t);
                if is_safe(&tmp) {
                    return true;
                }
                tmp.insert(t, v[t]);
            }
            return false;
        }
    }

    true
}

fn main() {
    let v: Vec<Vec<i64>> = io::stdin()
        .lock()
        .lines()
        .map(|x| {
            x.unwrap()
                .split_whitespace()
                .map(|x| x.parse().unwrap())
                .collect()
        })
        .collect();

    let part1ans = v.clone().into_iter().filter(|x| is_safe(x)).count();
    println!("Part 1: {}", part1ans);

    let part2ans = v
        .into_iter()
        .filter(|x| {
            let res = is_almost_safe(x);
            res
        })
        .count();
    println!("Part 2: {}", part2ans);
}
