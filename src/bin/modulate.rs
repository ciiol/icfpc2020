extern crate icfpc2020;

use std::env;
use std::io::{self, Error, Read};

use icfpc2020::modulator::{modulate, Modulatable};

fn main() -> Result<(), Error> {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 1 {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        buffer
    } else {
        args[1].clone()
    };
    match pretty(&input) {
        (val, "") => println!("{}", modulate(&val)),
        (_val, tail) => panic!("Unexpected tail {}", tail),
    }
    Ok(())
}

fn pretty(input: &str) -> (Modulatable, &str) {
    let mut chars = input.chars();
    loop {
        match chars.next() {
            Some(c) if c.is_whitespace() => {
                continue;
            }
            Some('n') => {
                let second = chars.next();  // i
                let third = chars.next();  // l
                assert_eq!(second, Some('i'));
                assert_eq!(third, Some('l'));
                return (Modulatable::Nil, chars.as_str())
            }
            Some(c @ '-') => {
                return pretty_number(c, chars.as_str())
            }
            Some(c) if c.is_numeric() => {
                return pretty_number(c, chars.as_str())
            }
            Some('[') => {
                return pretty_pair(chars.as_str())
            },
            other => panic!("Unexpected {:?}", other)
        }
    }
}

fn pretty_number(c: char, input: &str) -> (Modulatable, &str) {
    let sign: i64 = if c == '-' {
        -1
    } else {
       1
    };
    let mut result: i64 = if c == '-' {
        0
    } else {
        c.to_digit(10).unwrap() as i64
    };
    let mut last_num_pos: usize = 0;
    let mut chars = input.chars();
    loop {
        match chars.next() {
            Some(c) if c.is_numeric() => {
                result *= 10;
                result += c.to_digit(10).unwrap() as i64
            }
            _other => break
        }
        last_num_pos += 1;
    }
    (Modulatable::Num(sign * result), &input[last_num_pos..])
}

fn pretty_pair(input: &str) -> (Modulatable, &str) {
    let (left, tail) = pretty(input);
    let mut chars = tail.chars();
    loop {
        match chars.next() {
            Some(c) if c.is_whitespace() => {
                continue;
            }
            Some(',') => {
                let (right, tail) = pretty_pair(chars.as_str());
                return (Modulatable::Pair(Box::new(left), Box::new(right)), tail)
            }
            Some('|') => {
                let (right, tail) = pretty(chars.as_str());
                assert!(tail.starts_with(']'));
                return (Modulatable::Pair(Box::new(left), Box::new(right)), &tail[1..])
            }
            Some(']') => {
                let left = Box::new(left);
                let right = Box::new(Modulatable::Nil);
                return (Modulatable::Pair(left, right), chars.as_str())
            }
            other => panic!("Unexpected {:?}", other)
        }
    }
}