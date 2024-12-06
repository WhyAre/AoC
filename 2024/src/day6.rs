use std::{
    collections::BTreeSet,
    io::{self, BufRead},
};

const ME: u8 = b'^';
const WALL: u8 = b'#';
const EMPTY: u8 = b'.';

fn find_starting_point(board: &Vec<Vec<u8>>) -> Option<(usize, usize)> {
    for (row_ind, row) in board.iter().enumerate() {
        for (col_ind, cell) in row.iter().enumerate() {
            if *cell == ME {
                return Some((row_ind, col_ind));
            }
        }
    }

    None
}

fn has_reached(r: usize, c: usize, rows: usize, cols: usize) -> bool {
    r == 0 || c == 0 || r == rows - 1 || c == cols - 1
}

fn part1(board: &Vec<Vec<u8>>, mut r: usize, mut c: usize) -> BTreeSet<(usize, usize)> {
    const DR: &[isize] = &[-1, 0, 1, 0];
    const DC: &[isize] = &[0, 1, 0, -1];

    let mut curdir = 0usize;

    let mut visited = BTreeSet::new();

    while !has_reached(r, c, board.len(), board[0].len()) {
        visited.insert((r, c));

        let nr = r.wrapping_add_signed(DR[curdir]);
        let nc = c.wrapping_add_signed(DC[curdir]);

        // Check whether it's wall
        if board[nr][nc] == WALL {
            // turn right
            curdir = (curdir + 1) % DR.len();
            continue;
        }

        r = nr;
        c = nc;
    }

    visited.insert((r, c));
    visited
}

fn is_infinite_loop(board: &Vec<Vec<u8>>, mut r: usize, mut c: usize) -> bool {
    const DR: &[isize] = &[-1, 0, 1, 0];
    const DC: &[isize] = &[0, 1, 0, -1];

    let mut curdir = 0usize;

    let mut visited = BTreeSet::new();

    while !has_reached(r, c, board.len(), board[0].len()) {
        if visited.contains(&(r, c, curdir)) {
            return true;
        }

        visited.insert((r, c, curdir));

        let nr = r.wrapping_add_signed(DR[curdir]);
        let nc = c.wrapping_add_signed(DC[curdir]);

        // Check whether it's wall
        if board[nr][nc] == WALL {
            // turn right
            curdir = (curdir + 1) % DR.len();
            continue;
        }

        r = nr;
        c = nc;
    }

    false
}

fn part2(
    board: &Vec<Vec<u8>>,
    start_r: usize,
    start_c: usize,
    visited: BTreeSet<(usize, usize)>,
) -> usize {
    let mut board = board.clone();

    visited
        .iter()
        .filter(|&&(r, c)| {
            board[r][c] = WALL;
            let res = is_infinite_loop(&board, start_r, start_c);
            board[r][c] = EMPTY;
            res
        })
        .count()
}

fn main() {
    let board = io::stdin()
        .lock()
        .lines()
        .map(|x| x.unwrap().into_bytes())
        .collect::<Vec<_>>();

    let (r, c) = find_starting_point(&board).unwrap();

    let visited = part1(&board, r, c);
    println!("Part 1: {}", visited.len());

    let ans = part2(&board, r, c, visited);
    println!("Part 2: {}", ans);
}
