use crate::solver::{self, Solver, Value};
use std::fmt;

pub struct Interactor {
    solver: Solver,
    protocol: solver::Ref,
    state: Value,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Point {
    pub x: i64,
    pub y: i64,
}

impl Point {
    pub fn new(x: i64, y: i64) -> Self {
        Self { x, y }
    }
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Result {
    Draw(Vec<Point>),
    Send(Value),
}

impl Interactor {
    pub fn new(mut solver: Solver, protocol: Value) -> Self {
        let protocol = solver.put_hidden(protocol);
        let state = Value::F(solver::Fun::Nil);
        Self {
            solver,
            protocol,
            state,
        }
    }

    pub fn apply(&mut self, point: Point) -> Result {
        let result = self.call_protocol(self.state.clone(), point);
        let (flag, new_state, points) = self.decode_result(result);
        self.state = new_state;
        if flag == 0 {
            Result::Draw(decode_points(points))
        } else {
            Result::Send(points)
        }
    }

    fn call_protocol(&mut self, state: Value, point: Point) -> Value {
        self.call2(
            solver::Value::Ref(self.protocol),
            state,
            solver::Value::from(point),
        )
    }

    fn call2(&mut self, f: Value, x0: Value, x1: Value) -> Value {
        let ap = solver::ap(solver::ap(f, x0), x1);
        self.solver.deduce(ap, 10000)
    }

    fn decode_result(&mut self, result: Value) -> (i64, Value, Value) {
        let (flag, tail) = solver::deconstruct_pair(result).unwrap();
        let flag = if let Value::Num(n) = flag {
            n
        } else {
            panic!("Flag is not num: {}", flag)
        };
        let (state, tail) = solver::deconstruct_pair(tail).unwrap();
        let (data, _tail) = solver::deconstruct_pair(tail).unwrap();
        let (point_list, _tail) = solver::deconstruct_pair(data).unwrap();
        (flag, state, point_list)
    }
}

impl From<Point> for Value {
    fn from(point: Point) -> Self {
        let x = Value::Num(point.x);
        let y = Value::Num(point.y);
        let ap0 = solver::ap(solver::Value::F(solver::Fun::Cons), x);
        solver::ap(ap0, y)
    }
}

impl From<Value> for Point {
    fn from(value: Value) -> Self {
        match solver::deconstruct_ap2(value) {
            (Some(f), Some(x), Some(y)) => match (*f, *x, *y) {
                (Value::F(solver::Fun::Cons), Value::Num(x), Value::Num(y)) => Point::new(x, y),
                (Value::F(solver::Fun::Cons), x, y) => {
                    panic!("Point isn't fully computed: x = {}, y = {}", x, y)
                }
                (f, x, y) => panic!(
                    "It doesn't look like point: {}",
                    solver::reconstruct_ap2(
                        Some(Box::new(f)),
                        Some(Box::new(x)),
                        Some(Box::new(y))
                    )
                ),
            },
            (f, x0, x1) => panic!(
                "It doesn't look like point: {}",
                solver::reconstruct_ap2(f, x0, x1)
            ),
        }
    }
}

fn decode_points(value: Value) -> Vec<Point> {
    let mut head = value;
    let mut points = Vec::new();
    loop {
        if let Value::F(solver::Fun::Nil) = head {
            break;
        }
        let (point_pair, tail) = solver::deconstruct_pair(head).unwrap();
        points.push(Point::from(point_pair));
        head = tail;
    }
    points
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast;
    use crate::parser::parse;

    fn rules(input: &[&str]) -> Vec<solver::Rule> {
        input
            .iter()
            .map(|s| solver::Rule::from(ast::build_define_tree(&parse(s)).unwrap()))
            .collect()
    }

    fn with_rules(input: &[&str]) -> Interactor {
        let mut solver = Solver::new();
        for r in rules(input).into_iter() {
            solver.add_rule(r).unwrap();
        }
        Interactor::new(solver, Value::F(solver::Fun::Galaxy))
    }

    #[test]
    fn test_stateless() {
        let mut interactor = with_rules(&[
            "galaxy = ap ap c ap ap b b ap ap b ap b ap cons 0 ap ap c ap ap b \
            b cons ap ap c cons nil ap ap c ap ap b cons ap ap c cons nil nil",
        ]);
        assert_eq!(
            interactor.apply(Point::new(1, 0)),
            Result::Draw(vec![Point::new(1, 0)]),
        );
        assert_eq!(
            interactor.apply(Point::new(3, 0)),
            Result::Draw(vec![Point::new(3, 0)]),
        );
    }

    #[test]
    fn test_statefull() {
        let mut interactor = with_rules(&[
            "galaxy = ap ap b ap b ap ap s ap ap b ap b ap cons 0 ap ap c ap ap b b \
            cons ap ap c cons nil ap ap c cons nil ap c cons",
        ]);
        assert_eq!(
            interactor.apply(Point::new(0, 0)),
            Result::Draw(vec![Point::new(0, 0)]),
        );
        assert_eq!(
            interactor.apply(Point::new(2, 3)),
            Result::Draw(vec![Point::new(2, 3), Point::new(0, 0)]),
        );
    }
}
