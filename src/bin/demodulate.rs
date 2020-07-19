extern crate icfpc2020;

use std::env;
use std::io::{self, Error, Read};

use icfpc2020::modulator::demodulate;

fn main() -> Result<(), Error> {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 1 {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        buffer
    } else {
        args[1].clone()
    };
    println!("{}", demodulate(&input.trim()));
    Ok(())
}
