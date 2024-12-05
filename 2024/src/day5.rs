use std::collections::BTreeMap;
use std::io::{self, BufRead};

fn part1(am: &BTreeMap<usize, Vec<usize>>, queries: &Vec<Vec<usize>>) -> usize {
    queries
        .iter()
        .map(|query| {
            let mut visited = BTreeMap::<usize, bool>::new();

            let is_query_valid = query.iter().all(|number| {
                visited.entry(*number).or_insert(true);

                // Check that all it's children are unvisited
                am.get(number)
                    .map(|children| children.iter().all(|x| !visited.get(x).unwrap_or(&false)))
                    .unwrap_or(true)
            });

            // Return the middle number if valid
            if is_query_valid {
                query[query.len() / 2]
            } else {
                0
            }
        })
        .sum::<usize>()
}

/// Finds the first offending pair, and swap
fn fix_range(am: &BTreeMap<usize, Vec<usize>>, query: &mut Vec<usize>) -> bool {
    let mut visited = BTreeMap::<usize, usize>::new();
    let mut has_changed = false;

    for i in 0..query.len() {
        let cur_node = query[i];
        visited.entry(cur_node).or_insert(i);

        // Find the offending children
        let pair_to_swap = am
            .get(&cur_node)
            .and_then(|children| {
                let parent = children.iter().filter(|x| visited.get(x).is_some()).nth(0);
                parent
            })
            .map(|parent| (i, visited.get(parent).unwrap().to_owned()));

        // Swap the two indices
        pair_to_swap.map(|(src, dst)| {
            has_changed = true;
            query.swap(src, dst);
        });
    }

    has_changed
}

fn part2(am: &BTreeMap<usize, Vec<usize>>, queries: &Vec<Vec<usize>>) -> usize {
    queries
        .iter()
        .map(|query| {
            let mut query = query.clone();

            let mut is_ordered = true;

            // Bruh idk I just loop as many times as I need to fix the array
            loop {
                let has_changed = fix_range(am, &mut query);
                if !has_changed {
                    break;
                }

                is_ordered = false;
            }

            if is_ordered {
                0
            } else {
                query[query.len() / 2]
            }
        })
        .sum::<usize>()
}

fn main() {
    let am = io::stdin()
        .lock()
        .lines()
        .take_while(|x| x.as_ref().unwrap().contains("|"))
        .map(|x| {
            let v: Vec<usize> = x.unwrap().split("|").map(|x| x.parse().unwrap()).collect();
            (v[0], v[1])
        })
        .fold(BTreeMap::<usize, Vec<usize>>::new(), |mut map, (u, v)| {
            map.entry(u)
                .and_modify(|vec| vec.push(v))
                .or_insert_with(|| vec![v]);
            map
        });

    let queries = io::stdin()
        .lock()
        .lines()
        .map(|x| {
            x.unwrap()
                .split(',')
                .map(|x| x.parse().unwrap())
                .collect::<Vec<usize>>()
        })
        .collect::<Vec<_>>();

    let part1ans = part1(&am, &queries);
    println!("Part 1: {}", part1ans);

    let part2ans = part2(&am, &queries);
    println!("Part 2: {}", part2ans);
}
