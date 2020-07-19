extern crate icfpc2020;

use std::env;
use std::fs::File;
use std::io::{self, BufRead, Error};

use icfpc2020::ast;
use icfpc2020::parser::parse;
use icfpc2020::solver::{self, Solver};

fn main() -> Result<(), Error> {
    let args: Vec<String> = env::args().collect();

    let rules_file_name = &args[1];
    let entry_text = &args[2];

    let rules_file = File::open(rules_file_name)?;
    let rules = io::BufReader::new(rules_file)
        .lines()
        .map(|s| solver::Rule::from(ast::build_define_tree(&parse(&s.unwrap())).unwrap()));

    let mut solver = Solver::new();
    for rule in rules {
        solver.add_rule(rule).unwrap();
    }

    let entry = match ast::build_subtree(&parse(entry_text)).unwrap() {
        (Some(entry), []) => entry,
        (_entry, tail) => panic!("Unexpected tail {:?}", tail),
    };
    let result = solver.deduce(solver::Value::from(entry), 10000);
    println!("{}", result);

    Ok(())
}
