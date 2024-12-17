use std::{
    cmp::Ordering,
    collections::{BTreeMap, BTreeSet, BinaryHeap},
    io::{stdin, BufRead},
};

const WALL: u8 = b'#';
const INF: usize = usize::MAX;
const DIRS: [Dir; 4] = [Dir::North, Dir::South, Dir::East, Dir::West];

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone, Debug)]
enum Dir {
    North = 0,
    East = 1,
    South = 2,
    West = 3,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
struct Pos {
    r: usize,
    c: usize,
    dir: Dir,
}

impl Pos {
    fn new(r: usize, c: usize, dir: Dir) -> Self {
        Self { r, c, dir }
    }
    fn neighbours(&self) -> impl Iterator<Item = (Pos, usize)> + '_ {
        let dirs = [
            (-1, 0, Dir::North),
            (0, 1, Dir::East),
            (1, 0, Dir::South),
            (0, -1, Dir::West),
        ];

        dirs.into_iter().map(|(dr, dc, dir)| {
            let newpos = Self::new(
                self.r.wrapping_add_signed(dr),
                self.c.wrapping_add_signed(dc),
                dir,
            );

            let diff = (self.dir as i64 - dir as i64).abs();
            let dist = 1 + match diff {
                0 => 0,
                1 | 3 => 1000,
                2 => 2000,
                _ => panic!(),
            } as usize;

            (newpos, dist)
        })
    }
}

#[derive(PartialEq, Eq, Ord)]
struct State {
    pos: Pos,
    dist: usize,
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        other.dist.partial_cmp(&self.dist)
    }
}

fn find(board: &Vec<Vec<u8>>, char: u8) -> (usize, usize) {
    for (r, row) in board.iter().enumerate() {
        for (c, cell) in row.iter().enumerate() {
            if *cell == char {
                return (r, c);
            }
        }
    }

    panic!("Cannot find char in cell");
}

fn sssp(board: &Vec<Vec<u8>>) -> (usize, usize) {
    let (start_row, start_col) = find(board, b'S');
    let (end_row, end_col) = find(board, b'E');

    let mut dists: BTreeMap<Pos, usize> = BTreeMap::new();
    let mut parents: BTreeMap<Pos, BTreeMap<Pos, usize>> = BTreeMap::new();
    let mut pq: BinaryHeap<State> = BinaryHeap::new();

    let start_pos = Pos::new(start_row, start_col, Dir::East);
    pq.push(State {
        pos: start_pos.clone(),
        dist: 0,
    });
    dists.insert(start_pos, 0);

    while let Some(State { pos, dist }) = pq.pop() {
        if dist > *dists.get(&pos).unwrap() {
            continue; // Lazy deletion
        }

        // println!("Processing {:?} {:?}", pos, dist);

        for (newpos, cost) in pos.neighbours() {
            let Pos {
                r: nr,
                c: nc,
                dir: _,
            } = newpos;

            if board[nr][nc] == WALL {
                continue;
            }

            let newdist = dists[&pos] + cost;
            if newdist >= *dists.get(&newpos).unwrap_or(&INF) {
                continue;
            }

            // println!("Adding {:?} {:?}", newpos, newdist);

            dists.insert(newpos.clone(), newdist);
            parents
                .entry(newpos.clone())
                .or_insert(BTreeMap::new())
                .insert(pos.clone(), newdist);

            pq.push(State {
                pos: newpos,
                dist: newdist,
            })
        }
    }

    // dbg!(&dists);
    let mut visited: BTreeSet<Pos> = BTreeSet::new();
    let mindist = find_min_dist(end_row, end_col, &dists);
    for d in DIRS {
        let Some(dist) = dists.get(&Pos::new(end_row, end_col, d)) else {
            continue;
        };

        if *dist != mindist {
            continue;
        }

        dfs(end_row, end_col, d, &parents, &mut visited);
    }

    print_map(&board, &visited);

    (
        mindist,
        visited
            .into_iter()
            .map(|Pos { r, c, dir: _ }| (r, c))
            .collect::<BTreeSet<(usize, usize)>>()
            .len(),
    )
}

fn print_map(board: &Vec<Vec<u8>>, visited: &BTreeSet<Pos>) {
    for (r, row) in board.iter().enumerate() {
        for c in 0..row.len() {
            if DIRS
                .iter()
                .any(|&x| visited.get(&Pos::new(r, c, x)).is_some())
            {
                print!("O");
            } else {
                print!("{}", board[r][c] as char);
            }
        }
        println!("");
    }
}

fn find_min_dist(r: usize, c: usize, dists: &BTreeMap<Pos, usize>) -> usize {
    // dbg!(dists, r, c);
    *DIRS
        .into_iter()
        .filter_map(|d| {
            let pos = Pos::new(r, c, d);
            let out = dists.get(&pos);
            // dbg!(out);
            out
        })
        .min()
        .unwrap()
}

fn dfs(
    r: usize,
    c: usize,
    dir: Dir,
    parents_map: &BTreeMap<Pos, BTreeMap<Pos, usize>>,
    visited: &mut BTreeSet<Pos>,
) {
    let pos = Pos::new(r, c, dir);

    visited.insert(pos.clone());
    let Some(parents) = parents_map.get(&pos) else {
        return;
    };

    let mindist = *parents.values().min().unwrap();
    for newpos in parents
        .into_iter()
        .filter(|&(_, v)| *v == mindist)
        .map(|(k, _)| (*k).clone())
    {
        if visited.contains(&newpos) {
            continue;
        }

        let Pos { r, c, dir } = newpos;
        dfs(r, c, dir, parents_map, visited);
    }
}

fn main() {
    let lines = stdin().lock().lines();
    let board = lines.map(|x| x.unwrap().into_bytes()).collect::<Vec<_>>();

    let (mindist, visited) = sssp(&board);
    println!("mindist={:?}", mindist);
    println!("visited={:?}", visited);
}
