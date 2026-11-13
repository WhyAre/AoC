use std::fs::read_to_string;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn part1(filename: &str) {
    let mut sum: i32 = 0;
    for line in read_to_string(filename).unwrap().lines() {
        let mut first: i32 = -1;
        let mut last: i32 = -1;

        // println!("{}", line);
        for c in line.chars() {
            if !c.is_digit(10) {
                continue;
            }

            if first == -1 {
                first = c.to_digit(10).unwrap() as i32;
            }
            last = c.to_digit(10).unwrap() as i32;
        }

        let num = first * 10 + last;
        // println!("Num: {}", num);
        sum += num
    }

    println!("Part 1={sum}");
}

fn main() {
    // Part 1
    part1("input.txt");

    // Part 2
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);
    let lut = [
        "one", "1", "two", "2", "three", "3", "four", "4", "five", "5", "six",
        "6", "seven", "7", "eight", "8", "nine", "9",
    ];

    let mut sum: i32 = 0;
    for line in reader.lines() {
        if let Ok(ip) = line {
            let mut number: i32 = 0;
            let mut last: i32 = -1;

            let mut first: bool = false;

            for i in 0..ip.len() {
                if first == false {
                    for (index, num) in lut.iter().enumerate() {
                        if i + num.len() > ip.len() {
                            continue;
                        }

                        // println!("{ip}");
                        // println!("{}, {}, {}", i, num.len(), num);
                        if ip[i..i + num.len()] == **num {
                            number += ((index + 2) / 2) as i32 * 10;
                            first = true;
                            // println!("First: {}", number);
                            break;
                        }
                    }
                }

                for (index, num) in lut.iter().enumerate() {
                    if i + num.len() > ip.len() {
                        continue;
                    }

                    if ip[i..i + num.len()] == **num {
                        last = ((index + 2) / 2) as i32;
                        break;
                    }
                }
            }
            number += last;
            sum += number;

            // println!("{ip}");
            // println!("Number: {number}");
        }
    }
    println!("Part 2={}", sum);
}
