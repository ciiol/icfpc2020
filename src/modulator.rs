use crate::solver;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Modulatable {
    Num(i64),
    Nil,
    Pair(Box<Modulatable>, Box<Modulatable>),
}

pub fn modulate(val: &Modulatable) -> String {
    let mut result = String::new();
    modulate_into(val, &mut result);
    result
}

pub fn modulate_into(op: &Modulatable, buffer: &mut String) -> () {
    match op {
        Modulatable::Num(n) => modulate_num(*n, buffer),
        Modulatable::Nil => modulate_nil(buffer),
        Modulatable::Pair(l, r) => modulate_pair(l, r, buffer),
    }
}

pub fn demodulate(input: &str) -> Modulatable {
    match demodulate_part(input) {
        (val, "") => val,
        (_val, tail) => panic!("Unexpected tail {}", tail),
    }
}

pub fn demodulate_part(input: &str) -> (Modulatable, &str) {
    match &input[0..2] {
        "00" => demodulate_nil(input),
        "11" => demodulate_pair(input),
        "01" => demodulate_num(input),
        "10" => demodulate_num(input),
        tag => panic!("Unexpected tag {}", tag),
    }
}

fn modulate_num(num: i64, buffer: &mut String) -> () {
    // See https://message-from-space.readthedocs.io/en/latest/message13.html for details
    if num < 0i64 {
        buffer.push_str("10");
    } else {
        buffer.push_str("01");
    }
    let num = num.abs() as u64;
    let size_tag_len = (num_size(num) as f32 / 4.0).ceil() as u32;
    for _ in 0..size_tag_len {
        buffer.push('1');
    }
    buffer.push('0');
    let print_size = (size_tag_len * 4) as u64;
    for i in 0..print_size {
        let mask = 1 << (print_size - i - 1);
        match num & mask {
            0 => buffer.push('0'),
            _ => buffer.push('1'),
        }
    }
}

fn demodulate_num(input: &str) -> (Modulatable, &str) {
    let sign = match &input[0..2] {
        "01" => 1,
        "10" => -1,
        tag => panic!("Unexpected tag {}", tag),
    };
    let mut num = input[2..].chars();
    let mut len = 0;
    loop {
        match num.next() {
            Some('0') => break,
            Some('1') => len += 4,
            Some(c) => panic!("Unexpected char {}", c),
            None => panic!("Unexpected end"),
        }
    }
    let mut result: i64 = 0;
    for _ in 0..len {
        result = result << 1;
        result += match num.next() {
            Some('0') => 0,
            Some('1') => 1,
            Some(c) => panic!("Unexpected char {}", c),
            None => panic!("Unexpected end"),
        };
    }
    (Modulatable::Num(sign * result), num.as_str())
}

fn modulate_nil(buffer: &mut String) -> () {
    buffer.push_str("00");
}

fn demodulate_nil(input: &str) -> (Modulatable, &str) {
    assert!(input.starts_with("00"));
    (Modulatable::Nil, &input[2..])
}

fn modulate_pair(left: &Modulatable, right: &Modulatable, buffer: &mut String) -> () {
    buffer.push_str("11");
    modulate_into(left, buffer);
    modulate_into(right, buffer);
}

fn demodulate_pair(input: &str) -> (Modulatable, &str) {
    assert!(input.starts_with("11"));
    let (left, tail) = demodulate_part(&input[2..]);
    let (right, tail) = demodulate_part(tail);
    (Modulatable::Pair(Box::new(left), Box::new(right)), tail)
}

const fn num_bits<T>() -> usize {
    std::mem::size_of::<T>() * 8
}

fn num_size(num: u64) -> u32 {
    num_bits::<u64>() as u32 - num.leading_zeros()
}

impl From<solver::Value> for Modulatable {
    fn from(v: solver::Value) -> Self {
        match v {
            solver::Value::Num(n) => Modulatable::Num(n),
            solver::Value::F(solver::Fun::Nil) => Modulatable::Nil,
            ap @ solver::Value::Ap(_) => {
                let (head, tail) = solver::deconstruct_pair(ap).unwrap();
                Modulatable::Pair(
                    Box::new(Modulatable::from(head)),
                    Box::new(Modulatable::from(tail)),
                )
            }
            other => panic!("Can not convert {}", other),
        }
    }
}

impl fmt::Display for Modulatable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Modulatable::Nil => write!(f, "nil"),
            Modulatable::Num(n) => write!(f, "{}", n),
            Modulatable::Pair(l, r) => {
                write!(f, "[{}", *l)?;
                let mut head = r;
                loop {
                    match head.as_ref() {
                        Modulatable::Nil => break,
                        Modulatable::Pair(l, r) => {
                            write!(f, ", {}", *l)?;
                            head = r;
                        }
                        other => {
                            write!(f, " | {}", *other)?;
                            break;
                        }
                    }
                }
                write!(f, "]")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer_modulating() {
        assert_eq!(modulate(&Modulatable::Num(0)), "010");
        assert_eq!(modulate(&Modulatable::Num(1)), "01100001");
        assert_eq!(modulate(&Modulatable::Num(3)), "01100011");
        assert_eq!(modulate(&Modulatable::Num(-1)), "10100001");
        assert_eq!(modulate(&Modulatable::Num(4)), "01100100");
        assert_eq!(modulate(&Modulatable::Num(-4)), "10100100");
    }

    #[test]
    fn test_integer_demodulating() {
        assert_eq!(Modulatable::Num(0), demodulate("010"));
        assert_eq!(Modulatable::Num(1), demodulate("01100001"));
        assert_eq!(Modulatable::Num(3), demodulate("01100011"));
        assert_eq!(Modulatable::Num(-1), demodulate("10100001"));
        assert_eq!(Modulatable::Num(4), demodulate("01100100"));
        assert_eq!(Modulatable::Num(-4), demodulate("10100100"));
    }

    #[test]
    fn test_pair_modulating() {
        assert_eq!(modulate(&Modulatable::Nil), "00");
        assert_eq!(
            modulate(&Modulatable::Pair(
                Box::new(Modulatable::Nil),
                Box::new(Modulatable::Nil)
            )),
            "110000"
        );
        assert_eq!(
            modulate(&Modulatable::Pair(
                Box::new(Modulatable::Num(0)),
                Box::new(Modulatable::Num(2))
            )),
            "1101001100010"
        );
    }

    #[test]
    fn test_pair_demodulating() {
        assert_eq!(modulate(&Modulatable::Nil), "00");
        assert_eq!(
            Modulatable::Pair(Box::new(Modulatable::Nil), Box::new(Modulatable::Nil)),
            demodulate("110000")
        );
        assert_eq!(
            Modulatable::Pair(Box::new(Modulatable::Num(0)), Box::new(Modulatable::Num(2))),
            demodulate("1101001100010")
        );
    }
}
