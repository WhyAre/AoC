use std::{
    cmp::Ordering,
    collections::{BTreeMap, BTreeSet, BinaryHeap, VecDeque},
    time::Instant,
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

impl Dir {
    fn diff(&self, other: &Self) -> usize {
        let diff = (*self as i64 - *other as i64).abs();
        diff as usize
    }

    fn cost_to(&self, other: &Self) -> usize {
        match self.diff(other) {
            0 => 0,
            1 | 3 => 1000,
            2 => 2000,
            _ => panic!(),
        }
    }
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

            let dist = 1 + self.dir.cost_to(&dir);
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
    panic!();
}

fn sssp(board: &Vec<Vec<u8>>) -> (usize, usize) {
    let (start_row, start_col) = find(board, b'S');
    let (end_row, end_col) = find(board, b'E');

    let mut dists: BTreeMap<Pos, usize> = BTreeMap::new();
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

            dists.insert(newpos.clone(), newdist);
            pq.push(State {
                pos: newpos,
                dist: newdist,
            })
        }
    }

    let (mindist, visited) = retrace_steps(end_row, end_col, &dists);
    print_map(&board, &visited);

    (mindist, visited.len())
}

fn retrace_steps(
    end_row: usize,
    end_col: usize,
    dists: &BTreeMap<Pos, usize>,
) -> (usize, BTreeSet<(usize, usize)>) {
    let mut visited: BTreeSet<(usize, usize)> = BTreeSet::new();

    let mindist = *DIRS
        .iter()
        .filter_map(|x| dists.get(&Pos::new(end_row, end_col, *x)))
        .min()
        .unwrap();

    let mut queue: VecDeque<State> = DIRS
        .iter()
        .filter_map(|x| {
            let key = Pos::new(end_row, end_col, *x);
            dists.get(&key).map(|val| State {
                pos: key,
                dist: *val,
            })
        })
        .filter(|State { pos: _, dist }| *dist == mindist)
        .collect();

    while let Some(State {
        pos: Pos { r, c, dir },
        dist,
    }) = queue.pop_front()
    {
        visited.insert((r, c));
        let (nr, nc) = match dir {
            Dir::North => (r + 1, c),
            Dir::South => (r - 1, c),
            Dir::East => (r, c - 1),
            Dir::West => (r, c + 1),
        }; // The directions are reversed and it's expected, cos I'm going backwards
        for new_dir in DIRS {
            let newpos = Pos::new(nr, nc, new_dir);
            let cost = 1 + dir.cost_to(&new_dir);

            let Some(newdist) = dists.get(&newpos) else {
                continue;
            };

            if newdist + cost != dist {
                continue;
            }

            queue.push_back(State {
                pos: newpos,
                dist: *newdist,
            });
        }
    }

    (mindist, visited)
}

fn print_map(board: &Vec<Vec<u8>>, visited: &BTreeSet<(usize, usize)>) {
    for (r, row) in board.iter().enumerate() {
        for c in 0..row.len() {
            if visited.get(&(r, c)).is_some() {
                print!("O");
            } else {
                print!("{}", board[r][c] as char);
            }
        }
        println!("");
    }
}

fn parse(inp: &str) -> Vec<Vec<u8>> {
    inp.lines().map(|x| x.as_bytes().to_vec()).collect()
}

fn main() {
    let now = Instant::now();

    let input = include_str!("../inputs/day16.txt");
    let board = parse(input);

    let (mindist, visited) = sssp(&board);
    println!("mindist={:?}", mindist);
    println!("visited={:?}", visited);

    println!("Elapsed: {:?}", now.elapsed());

    assert_eq!(mindist, 73432);
    assert_eq!(visited, 496);
}

#[cfg(test)]
mod tests {
    use super::*;

    const INP1: &str = include_str!("../inputs/day16-sample-1.txt");
    const INP2: &str = include_str!("../inputs/day16-sample-2.txt");

    #[test]
    fn test1() {
        let board = parse(INP1);
        let (part1, part2) = sssp(&board);

        assert_eq!(part1, 7036);
        assert_eq!(part2, 45);
    }

    #[test]
    fn test2() {
        let board = parse(INP2);
        let (part1, part2) = sssp(&board);

        assert_eq!(part1, 11048);
        assert_eq!(part2, 64);
    }
}
