extern crate icfpc2020;

use std::env;
use std::io::{self, Error, Read};

use icfpc2020::modulator::{demodulate, Modulatable};

fn main() -> Result<(), Error> {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 1 {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        buffer
    } else {
        args[1].clone()
    };
    println!("{}", pretty(&demodulate(&input.trim())));
    Ok(())
}

fn pretty(op: &Modulatable) -> String {
    match op {
        Modulatable::Nil => String::from("nil"),
        Modulatable::Num(n) => n.to_string(),
        Modulatable::Pair(l, r) => format!("[{}", pretty_pair_int(l, r)),
    }
}

fn pretty_pair_int(l: &Box<Modulatable>, r: &Box<Modulatable>) -> String {
    match (l.as_ref(), r.as_ref()) {
        (lref, Modulatable::Nil) => format!("{}]", pretty(lref)),
        (lref, Modulatable::Pair(internal_l, internal_r)) => format!(
            "{}, {}",
            pretty(lref),
            pretty_pair_int(internal_l, internal_r)
        ),
        (lref, rref) => format!("{} | {}]", pretty(lref), pretty(rref)),
    }
}
