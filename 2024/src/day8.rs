use core::fmt;
use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    iter::successors,
    ops::Sub,
};

use itertools::Itertools;

#[derive(fmt::Debug, Hash, PartialEq, Eq, Clone, Copy)]
struct Coord {
    r: isize,
    c: isize,
}

impl Coord {
    fn diff(&self, other: &Self) -> Self {
        let dr = self.r.sub(other.r);
        let dc = self.c.sub(other.c);

        Coord { r: dr, c: dc }
    }

    fn inbound(&self, rows: usize, cols: usize) -> bool {
        self.r >= 0 && self.c >= 0 && self.r < rows as isize && self.c < cols as isize
    }
}

fn part1(board: &BTreeMap<char, Vec<Coord>>, nrows: usize, ncols: usize) -> usize {
    let antinodes = board
        .iter()
        .flat_map(|(_, coords)| {
            coords.iter().combinations(2).flat_map(|a| {
                let c1 = **a.first().unwrap();
                let c2 = **a.last().unwrap();

                let Coord { r: dr, c: dc } = c2.diff(&c1);

                assert!(dr >= 0);
                assert!(c2.r > c1.r);
                assert!(c2.r != c1.r || c2.c > c1.c);

                let out1 = Coord {
                    r: c1.r - dr,
                    c: c1.c - dc,
                };
                let out2 = Coord {
                    r: c2.r + dr,
                    c: c2.c + dc,
                };

                vec![out1, out2]
            })
        })
        .filter(|c| c.inbound(nrows, ncols))
        .fold(HashSet::new(), |mut acc, cur| {
            acc.insert(cur);
            acc
        });

    antinodes.len()
}

fn part2(board: &BTreeMap<char, Vec<Coord>>, nrows: usize, ncols: usize) -> usize {
    let antinodes = board
        .iter()
        .flat_map(|(_, coords)| {
            coords.iter().combinations(2).flat_map(move |a| {
                let c1 = **a.first().unwrap();
                let c2 = **a.last().unwrap();

                let Coord { r: dr, c: dc } = c2.diff(&c1);

                assert!(dr >= 0);
                assert!(c2.r > c1.r);
                assert!(c2.r != c1.r || c2.c > c1.c);

                let iter1 = successors(Some(c1), |c| {
                    Some(Coord {
                        r: c.r - dr,
                        c: c.c - dc,
                    })
                })
                .take_while(|c| c.inbound(nrows, ncols));

                let iter2 = successors(Some(c2), |c| {
                    Some(Coord {
                        r: c.r + dr,
                        c: c.c + dc,
                    })
                })
                .take_while(|c| c.inbound(nrows, ncols));

                // Get all the antinodes
                iter1.chain(iter2).collect::<Vec<_>>()
            })
        })
        .filter(|c| c.inbound(nrows, ncols))
        .fold(HashSet::new(), |mut acc, cur| {
            acc.insert(cur);
            acc
        });

    antinodes.len()
}

fn main() {
    let inp = include_str!("../inputs/day8.txt");
    let split = inp.split("\n");

    let rows = split.clone().count();
    let cols = split.clone().next().map(|line| line.len()).unwrap();

    let map = split
        .enumerate()
        .flat_map(|(row, line)| {
            line.chars().enumerate().filter_map(move |(col, c)| {
                if c == '.' {
                    None
                } else {
                    Some((
                        Coord {
                            r: row as isize,
                            c: col as isize,
                        },
                        c,
                    ))
                }
            })
        })
        .fold(
            BTreeMap::<char, Vec<Coord>>::new(),
            |mut acc, (coord, c)| {
                let entry = acc.entry(c).or_default();
                entry.push(coord);
                acc
            },
        );

    dbg!(&map);
    dbg!(rows);
    dbg!(cols);

    println!("Part 1: {}", part1(&map, rows - 1, cols));
    println!("Part 2: {}", part2(&map, rows - 1, cols));
}
