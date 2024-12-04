use std::io::{self, BufRead};
use std::iter::zip;

fn part1(v: &Vec<Vec<u8>>) -> usize {
    let dr: Vec<i64> = vec![1, 1, 1, 0, -1, -1, -1, 0];
    let dc: Vec<i64> = vec![-1, 0, 1, 1, 1, 0, -1, -1];

    let word = "XMAS".as_bytes();

    let mut total = 0usize;
    for r in 0..v.len() {
        for c in 0..v[r].len() {
            for (dr, dc) in zip(&dr, &dc) {
                // Check all eight directions.
                let has_xmas = (0..4).all(|i| {
                    let nr = (r as i64 + (dr * i)) as usize;
                    let nc = (c as i64 + (dc * i)) as usize;

                    v.get(nr)
                        .and_then(|x| x.get(nc))
                        .map(|&x| x == word[i as usize])
                        .unwrap_or(false)
                });

                if has_xmas {
                    total += 1;
                }
            }
        }
    }

    total
}

fn is_x_mas(mat: &Vec<Vec<u8>>) -> bool {
    let word = "MAS";
    let word_rev = "SAM";

    // Get primary diagonal word
    let str1 = String::from_utf8((0..3).map(|x| mat[0 + x][0 + x]).collect::<Vec<_>>()).unwrap();

    // Get secondary diagonal word
    let str2 = String::from_utf8((0..3).map(|x| mat[2 - x][0 + x]).collect::<Vec<_>>()).unwrap();

    (str1 == word || str1 == word_rev) && (str2 == word || str2 == word_rev)
}

fn part2(v: &Vec<Vec<u8>>) -> usize {
    let mut total = 0usize;
    for rows in v.windows(3) {
        for (col_ind, _) in rows[0].windows(3).enumerate() {
            let square: Vec<Vec<u8>> = rows
                .iter()
                .map(|row| row[col_ind..col_ind + 3].to_vec())
                .collect();

            if is_x_mas(&square) {
                total += 1
            }
        }
    }
    total
}

fn main() {
    let v: Vec<Vec<_>> = io::stdin()
        .lock()
        .lines()
        .map(|x| x.unwrap().as_bytes().to_vec())
        .collect();

    let count = part1(&v);
    println!("Part 1: {}", count);

    let count = part2(&v);
    println!("Part 2: {}", count);
}
