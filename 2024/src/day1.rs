use std::{
    collections::BTreeMap,
    fs::File,
    io::{BufRead, BufReader},
    iter::zip,
};

fn part1(mut lst1: Vec<i64>, mut lst2: Vec<i64>) {
    lst1.sort();
    lst2.sort();

    let ans: i64 = zip(lst1, lst2).map(|(a, b)| (a - b).abs()).sum();
    println!("Part 1: {}", ans);
}

fn part2(lst1: Vec<i64>, lst2: Vec<i64>) {
    let occurs = lst2.into_iter().fold(BTreeMap::new(), |mut map, val| {
        map.entry(val).and_modify(|x| *x += 1).or_insert(1i64);
        map
    });

    let res: i64 = lst1
        .into_iter()
        .map(|x| {
            let times = occurs.get(&x).map(|x| *x).unwrap_or(0);
            x * times
        })
        .sum();

    println!("Part 2: {:?}", res);
}

fn main() {
    let file = File::open("inputs/day1-1.txt");
    let Ok(file) = file else {
        return;
    };

    let lines = BufReader::new(file).lines();

    let (lst1, lst2): (Vec<_>, Vec<_>) = lines
        .map(|x| {
            let parts: Vec<i64> = x
                .unwrap()
                .split_whitespace()
                .map(|x| x.parse().unwrap())
                .collect();
            (parts[0], parts[1])
        })
        .unzip();

    part1(lst1.clone(), lst2.clone());
    part2(lst1, lst2);
}
