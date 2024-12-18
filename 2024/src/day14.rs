use core::fmt;
use std::str::FromStr;

use itertools::Itertools;

const ROWS: usize = 103;
const COLS: usize = 101;

const DBG_ROWS: usize = 7;
const DBG_COLS: usize = 11;

fn get_sec_factor(robots: &Vec<Robot>, rows: usize, cols: usize, elapsed_time: usize) -> usize {
    let mid_row = rows / 2;
    let mid_col = cols / 2;

    let mut quadrants: [usize; 4] = [0; 4];

    for r in robots {
        let (nr, nc) = r.mv(rows, cols, elapsed_time);
        // println!("{} {}", nr, nc);

        if nr < mid_row {
            if nc > mid_col {
                // println!("Q1");
                quadrants[0] += 1;
            } else if nc < mid_col {
                // println!("Q2");
                quadrants[1] += 1;
            }
        } else if nr > mid_row {
            if nc < mid_col {
                // println!("Q3");
                quadrants[2] += 1;
            } else if nc > mid_col {
                // println!("Q4");
                quadrants[3] += 1;
            }
        }
    }

    quadrants.into_iter().reduce(|a, b| a * b).unwrap()
}

fn print_map(robots: &Vec<Robot>, rows: usize, cols: usize, elapsed_time: usize) {
    let mut map: Vec<Vec<bool>> = (0..rows)
        .map(|_| (0..cols).map(|_| false).collect())
        .collect();

    for r in robots {
        let (nr, nc) = r.mv(rows, cols, elapsed_time);
        map[nr][nc] = true;
    }

    for row in map {
        for cell in row {
            print!("{}", if cell { '#' } else { '.' });
        }
        println!("");
    }
}

#[derive(Debug)]
struct Robot {
    r: usize,
    c: usize,
    vr: i64,
    vc: i64,
}

impl Robot {
    fn mv(&self, rows: usize, cols: usize, elapsed_time: usize) -> (usize, usize) {
        // This works, we'll figure out something later
        let nr = (self.r as i64 + (self.vr * elapsed_time as i64)).rem_euclid(rows as i64) as usize;
        let nc = (self.c as i64 + (self.vc * elapsed_time as i64)).rem_euclid(cols as i64) as usize;
        (nr, nc)
    }
}

fn extract_numbers<T>(inp: &str) -> (T, T)
where
    T: FromStr,
    <T as FromStr>::Err: fmt::Debug,
{
    inp.split("=")
        .skip(1)
        .flat_map(|part| part.split(",").map(|num| num.parse().unwrap()))
        .collect_tuple()
        .unwrap()
}

fn parse(input: &str) -> Vec<Robot> {
    input
        .lines()
        .map(|x| {
            let (pos, velocity) = x.split_whitespace().collect_tuple().unwrap();

            let pos: (usize, usize) = extract_numbers(pos);
            let velocity: (i64, i64) = extract_numbers(velocity);

            Robot {
                r: pos.1,
                c: pos.0,
                vr: velocity.1,
                vc: velocity.0,
            }
        })
        .collect()
}

fn main() {
    let robots = parse(include_str!("../inputs/day14.txt"));

    let part1 = get_sec_factor(&robots, ROWS, COLS, 100);
    println!("Part 1: {}", part1);

    let min_sec_fact = (0..(ROWS * COLS + 1))
        .map(|time| (get_sec_factor(&robots, ROWS, COLS, time), time))
        .min()
        .map(|(_, v)| v)
        .unwrap();
    print_map(&robots, ROWS, COLS, min_sec_fact);
    println!("Part 2: {}", min_sec_fact);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sample_part_one() {
        let robots = parse(include_str!("../inputs/day14-sample.txt"));
        let ans = get_sec_factor(&robots, DBG_ROWS, DBG_COLS, 100);
        assert_eq!(ans, 12);
    }
}
