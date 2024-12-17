use std::{
    collections::{BTreeMap, BTreeSet, BinaryHeap},
    io::{stdin, BufRead},
};

use itertools::repeat_n;

#[derive(PartialEq, Clone, Copy, PartialOrd, Eq, Ord, Debug)]
enum Dir {
    NORTH = 0,
    EAST = 1,
    SOUTH = 2,
    WEST = 3,
}

fn part1(board: &Vec<Vec<u8>>) -> i64 {
    const WALL: u8 = b'#';
    const INF: i64 = i64::MAX;
    const NUM_DIR: i64 = 4;
    const DELTAS: [(isize, isize, Dir); 4] = [
        (-1, 0, Dir::NORTH),
        (0, 1, Dir::EAST),
        (1, 0, Dir::SOUTH),
        (0, -1, Dir::WEST),
    ];

    let start_row = board.len() - 2;
    let start_col = 1usize;
    let start_dir = Dir::EAST;

    let mut pq: BinaryHeap<(usize, usize, i64, Dir)> = BinaryHeap::new();
    let mut dists: Vec<Vec<i64>> =
        repeat_n(repeat_n(INF, board[0].len()).collect(), board.len()).collect();

    dists[start_row][start_col] = 0;
    pq.push((start_row, start_col, 0, start_dir));
    while !pq.is_empty() {
        let (r, c, d, curdir) = pq.pop().unwrap();
        // dbg!(r, c, d);

        if d > dists[r][c] {
            // Lazy deletion
            continue;
        }

        for (dr, dc, dir) in DELTAS {
            let (nr, nc) = (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc));

            if board[nr][nc] == WALL {
                continue;
            }

            let diff = (curdir as i64 - dir as i64).abs();
            // dbg!(diff);
            let dist = 1 + match diff {
                0 => 0,
                1 | 3 => 1000,
                2 => 2000,
                _ => panic!("Noooo"),
            };

            if dists[r][c] + dist >= dists[nr][nc] {
                continue;
            }

            dists[nr][nc] = dists[r][c] + dist;
            pq.push((nr, nc, dists[nr][nc], dir));
        }
    }

    dists[1][board[0].len() - 2]
}

fn part2(board: &Vec<Vec<u8>>) -> i64 {
    const WALL: u8 = b'#';
    const INF: i64 = i64::MAX;
    const NUM_DIR: i64 = 4;
    const DELTAS: [(isize, isize, Dir); 4] = [
        (-1, 0, Dir::NORTH),
        (0, 1, Dir::EAST),
        (1, 0, Dir::SOUTH),
        (0, -1, Dir::WEST),
    ];

    let start_row = board.len() - 2;
    let start_col = 1usize;
    let start_dir = Dir::EAST;

    let mut pq: BinaryHeap<(i64, usize, usize, Dir)> = BinaryHeap::new();
    let mut dists: Vec<Vec<Vec<i64>>> = repeat_n(
        repeat_n(repeat_n(INF, NUM_DIR as usize).collect(), board[0].len()).collect(),
        board.len(),
    )
    .collect();
    let mut parents: BTreeMap<(usize, usize, Dir), BTreeMap<(usize, usize, Dir), i64>> =
        BTreeMap::new();
    let mut visited: BTreeSet<(usize, usize, Dir)> = BTreeSet::new();

    dists[start_row][start_col][start_dir as usize] = 0;
    pq.push((0, start_row, start_col, start_dir));
    while !pq.is_empty() {
        let (d, r, c, curdir) = pq.pop().unwrap();
        // println!("{:?} {:?} {:?} {:?}", r, c, curdir, d);

        if d > dists[r][c][curdir as usize] {
            // Lazy deletion
            continue;
        }

        if d > 73432 {
            continue;
        }

        for (dr, dc, dir) in DELTAS {
            let (nr, nc) = (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc));

            if board[nr][nc] == WALL {
                continue;
            }

            let diff = (curdir as i64 - dir as i64).abs();
            // dbg!(diff);
            let dist = 1 + match diff {
                0 => 0,
                1 | 3 => 1000,
                2 => 2000,
                _ => panic!("Noooo"),
            };

            // dbg!(dists[r][c][curdir as usize] + dist);
            let newdist = dists[r][c][curdir as usize] + dist;
            if newdist > dists[nr][nc][dir as usize] {
                continue;
            }

            // parents[nr][nc].push((r, c));
            parents
                .entry((nr, nc, dir))
                .or_insert_with(|| BTreeMap::new())
                .insert((r, c, curdir), newdist);

            dists[nr][nc][dir as usize] = newdist;
            pq.push((newdist, nr, nc, dir));
        }
    }

    let mut visited: Vec<Vec<bool>> =
        repeat_n(repeat_n(false, board[0].len()).collect(), board.len()).collect();

    let (end_row, end_col) = (1, board[0].len() - 2);

    // dbg!(&parents);
    // dbg!(parents.get(&(1, 13, Dir::NORTH)));
    // dbg!(parents.get(&(1, 13, Dir::SOUTH)));
    // dbg!(parents.get(&(1, 13, Dir::EAST)));
    // dbg!(parents.get(&(1, 13, Dir::WEST)));
    println!("Retracing");
    let set = retrace_steps(end_row, end_col, &parents, &dists);

    for (r, c, _) in set {
        visited[r][c] = true;
    }

    let mut count = 0;
    for (row_ind, row) in visited.into_iter().enumerate() {
        for (col_ind, is_visited) in row.into_iter().enumerate() {
            let char = if board[row_ind][col_ind] == WALL {
                WALL as char
            } else if is_visited {
                'O'
            } else {
                '.'
            };
            print!("{}", char);
            count += if is_visited { 1 } else { 0 };
        }
        println!("");
    }
    count
}

fn retrace_steps(
    end_row: usize,
    end_col: usize,
    parents: &BTreeMap<(usize, usize, Dir), BTreeMap<(usize, usize, Dir), i64>>,
    dists: &Vec<Vec<Vec<i64>>>,
) -> BTreeSet<(usize, usize, Dir)> {
    let mut visited: BTreeSet<(usize, usize, Dir)> = BTreeSet::new();

    let min_val = *dists[end_row][end_col].iter().min().unwrap();

    let dirs: Vec<usize> = dists[end_row][end_col]
        .iter()
        .enumerate()
        .filter(|&(_, val)| *val == min_val)
        .map(|(k, _)| k)
        .collect();

    for d in dirs {
        let d = match d {
            0 => Dir::NORTH,
            1 => Dir::EAST,
            2 => Dir::SOUTH,
            3 => Dir::WEST,
            _ => panic!("Noo"),
        };
        dfs(end_row, end_col, d, parents, &mut visited);
    }

    visited
}
fn dfs(
    r: usize,
    c: usize,
    dir: Dir,
    parents: &BTreeMap<(usize, usize, Dir), BTreeMap<(usize, usize, Dir), i64>>,
    visited: &mut BTreeSet<(usize, usize, Dir)>,
) {
    visited.insert((r, c, dir));

    let parents_list = parents.get(&(r, c, dir));

    if parents_list.is_none() {
        return;
    }

    let min_val = parents_list
        .unwrap()
        .into_iter()
        .min_by(|(_, v1), (_, v2)| v1.cmp(v2))
        .map(|(_, v)| *v)
        .unwrap();

    let least_parents = parents_list
        .unwrap()
        .into_iter()
        .filter(|(_, v)| **v == min_val)
        .map(|(k, _)| *k)
        .collect::<Vec<_>>();

    for (pr, pc, parent_dir) in least_parents {
        if visited.contains(&(pr, pc, parent_dir)) {
            continue;
        }

        dfs(pr, pc, parent_dir, parents, visited);
    }
}

fn main() {
    let lines = stdin().lock().lines();
    let board = lines.map(|x| x.unwrap().into_bytes()).collect::<Vec<_>>();

    let ans = part1(&board);
    println!("Part 1: {}", ans);

    let ans = part2(&board);
    println!("Part 2: {}", ans);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let input = "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############";

        let board = input
            .lines()
            .map(|x| x.as_bytes().to_vec())
            .collect::<Vec<_>>();

        assert_eq!(part1(&board), 7036);
    }
}
