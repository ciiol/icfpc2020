use crate::ast::{self, Node};
use crate::operand::Address;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
struct Solver {
    memory: HashMap<Address, Value>,
    hidden_memory: Vec<Value>,
    definitions: HashMap<Fun, Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Num(i64),
    Ref(Ref),
    Ap(Ap),
    F(Fun),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Rule {
    left: Value,
    right: Value,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Ref {
    Ref(Address),
    HiddenRef(Address),
}

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub enum Fun {
    Inc,
    Dec,
    Add,
    Mul,
    Div,
    T,
    F,
    Lt,
    Mod,
    Dem,
    Send,
    Neg,
    Pwr2,
    S,
    C,
    B,
    I,
    Cons,
    Car,
    Cdr,
    Nil,
    IsNil,
    Vec,
    Draw,
    Chkb,
    MultipleDraw,
    If0,
    Interact,
    Galaxy,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ap {
    pub f: Option<Box<Value>>,
    pub arg: Option<Box<Value>>,
}

impl Ap {
    pub fn is_fully_defined(&self) -> bool {
        if let Ap {
            f: Some(_),
            arg: Some(_),
        } = self
        {
            return true;
        }
        return false;
    }

    pub fn is_partial(&self) -> bool {
        if let Ap { f: _, arg: None } = self {
            return true;
        }
        return false;
    }
}

impl Value {
    pub fn seek_left_fun(&self, depth: usize) -> Option<Fun> {
        match self {
            Value::Ap(Ap { f: Some(l), arg: _ }) if depth > 0 => l.seek_left_fun(depth - 1),
            Value::F(f) if depth == 0 => Some(*f),
            _ => None,
        }
    }
}

impl Solver {
    pub fn new() -> Self {
        Self {
            memory: HashMap::new(),
            hidden_memory: Vec::new(),
            definitions: HashMap::new(),
        }
    }

    pub fn get(&self, r: &Ref) -> Option<&Value> {
        match r {
            Ref::Ref(addr) => self.memory.get(addr),
            Ref::HiddenRef(addr) => self.hidden_memory.get(*addr),
        }
    }

    pub fn put(&mut self, r: Ref, value: Value) -> Result<(), String> {
        if let Some(exists) = self.get(&r) {
            return Err(format!(
                "Memory {:?} has already been defined as {:?}",
                r, exists
            ));
        }
        match r {
            Ref::Ref(addr) => self.memory.insert(addr, value),
            Ref::HiddenRef(_) => panic!("Do not use hidden memory here"),
        };
        Ok(())
    }

    fn put_hidden(&mut self, value: Value) -> Ref {
        match value {
            Value::Ref(r) => r,
            other => {
                let addr = self.hidden_memory.len();
                self.hidden_memory.push(other);
                Ref::HiddenRef(addr)
            }
        }
    }

    pub fn add_rule(&mut self, rule: Rule) -> Result<(), String> {
        match rule {
            Rule {
                left: Value::Ref(r),
                right: value,
            } => self.put(r, value),
            Rule {
                left: Value::F(f),
                right: value,
            } => {
                self.definitions.insert(f, value);
                Ok(())
            }
            _ => Err(format!("Don't know how to process rule {}", rule)),
        }
    }

    pub fn deduce(&mut self, entry: Value, depth: usize) -> Value {
        if depth == 0 {
            return entry;
        }
        match entry {
            Value::Ap(ap) => self.deduce_ap(ap, depth),
            Value::Ref(r) => {
                let branch = self.get(&r).unwrap().clone();
                self.deduce(branch, depth - 1)
            }
            Value::F(f) => {
                if let Some(exists) = self.definitions.get(&f) {
                    let new_entry = exists.clone();
                    self.deduce(new_entry, depth - 1)
                } else {
                    Value::F(f)
                }
            }
            other => other,
        }
    }

    fn deduce_ap(&mut self, ap: Ap, depth: usize) -> Value {
        let val = match ap {
            Ap { f: None, arg } => Value::Ap(Ap { f: None, arg }),
            Ap { f: Some(f), arg } => {
                let f = self.deduce(*f, depth - 1);
                Value::Ap(Ap {
                    f: Some(Box::new(f)),
                    arg,
                })
            }
        };
        let (changed, new_val) = self.simplify(val);
        if changed {
            self.deduce(new_val, depth - 1)
        } else {
            new_val
        }
    }

    fn simplify(&mut self, entry: Value) -> (bool, Value) {
        let l1 = entry.seek_left_fun(1);
        let l2 = entry.seek_left_fun(2);
        let l3 = entry.seek_left_fun(3);
        match (l1, l2, l3) {
            (Some(Fun::I), _, _) => self.apply_combinator_i(entry),
            (Some(Fun::Nil), _, _) => self.apply_nil(entry),
            (_, Some(Fun::T), _) => self.apply_combinator_k(entry),
            (_, Some(Fun::F), _) => self.apply_false(entry),
            (_, _, Some(Fun::S)) => self.apply_combinator_s(entry),
            (_, _, Some(Fun::C)) => self.apply_combinator_c(entry),
            (_, _, Some(Fun::B)) => self.apply_combinator_b(entry),
            (_, _, Some(Fun::Cons)) => self.apply_cons(entry),
            _ => (false, entry),
        }
    }

    fn apply_combinator_i(&mut self, entry: Value) -> (bool, Value) {
        match entry {
            Value::Ap(Ap {
                f: _,
                arg: Some(arg),
            }) => (true, *arg),
            other => (false, other),
        }
    }

    fn apply_nil(&mut self, entry: Value) -> (bool, Value) {
        match entry {
            Value::Ap(Ap {
                f: _,
                arg: Some(_x0),
            }) => (true, Value::F(Fun::T)),
            other => (false, other),
        }
    }

    fn apply_combinator_k(&mut self, entry: Value) -> (bool, Value) {
        match entry {
            Value::Ap(Ap {
                f: Some(f),
                arg: Some(_x1),
            }) => match *f {
                Value::Ap(Ap {
                    f: _,
                    arg: Some(x0),
                }) => (true, *x0),
                other => (false, other),
            },
            other => (false, other),
        }
    }

    fn apply_false(&mut self, entry: Value) -> (bool, Value) {
        match entry {
            Value::Ap(Ap {
                f: Some(f),
                arg: Some(x1),
            }) => match *f {
                Value::Ap(Ap {
                    f: _,
                    arg: Some(_x0),
                }) => (true, *x1),
                other => (false, other),
            },
            other => (false, other),
        }
    }

    fn apply_combinator_s(&mut self, entry: Value) -> (bool, Value) {
        match entry {
            Value::Ap(Ap {
                f: Some(f),
                arg: Some(x2),
            }) => match *f {
                Value::Ap(Ap {
                    f: Some(f),
                    arg: Some(x1),
                }) => match *f {
                    Value::Ap(Ap {
                        f: _,
                        arg: Some(x0),
                    }) => {
                        let ref_x2 = self.put_hidden(*x2);
                        let ap1 = Value::Ap(Ap {
                            f: Some(x0),
                            arg: Some(Box::new(Value::Ref(ref_x2))),
                        });
                        let ap2 = Value::Ap(Ap {
                            f: Some(x1),
                            arg: Some(Box::new(Value::Ref(ref_x2))),
                        });
                        (
                            true,
                            Value::Ap(Ap {
                                f: Some(Box::new(ap1)),
                                arg: Some(Box::new(ap2)),
                            }),
                        )
                    }
                    other => (false, other),
                },
                other => (false, other),
            },
            other => (false, other),
        }
    }

    fn apply_combinator_c(&mut self, entry: Value) -> (bool, Value) {
        match entry {
            Value::Ap(Ap {
                f: Some(f),
                arg: Some(x2),
            }) => match *f {
                Value::Ap(Ap {
                    f: Some(f),
                    arg: Some(x1),
                }) => match *f {
                    Value::Ap(Ap {
                        f: _,
                        arg: Some(x0),
                    }) => {
                        let ap1 = Value::Ap(Ap {
                            f: Some(x0),
                            arg: Some(x2),
                        });
                        (
                            true,
                            Value::Ap(Ap {
                                f: Some(Box::new(ap1)),
                                arg: Some(x1),
                            }),
                        )
                    }
                    other => (false, other),
                },
                other => (false, other),
            },
            other => (false, other),
        }
    }

    fn apply_combinator_b(&mut self, entry: Value) -> (bool, Value) {
        match entry {
            Value::Ap(Ap {
                f: Some(f),
                arg: Some(x2),
            }) => match *f {
                Value::Ap(Ap {
                    f: Some(f),
                    arg: Some(x1),
                }) => match *f {
                    Value::Ap(Ap {
                        f: _,
                        arg: Some(x0),
                    }) => {
                        let ap1 = Value::Ap(Ap {
                            f: Some(x1),
                            arg: Some(x2),
                        });
                        (
                            true,
                            Value::Ap(Ap {
                                f: Some(x0),
                                arg: Some(Box::new(ap1)),
                            }),
                        )
                    }
                    other => (false, other),
                },
                other => (false, other),
            },
            other => (false, other),
        }
    }

    fn apply_cons(&mut self, entry: Value) -> (bool, Value) {
        match entry {
            Value::Ap(Ap {
                f: Some(f),
                arg: Some(x2),
            }) => match *f {
                Value::Ap(Ap {
                    f: Some(f),
                    arg: Some(x1),
                }) => match *f {
                    Value::Ap(Ap {
                        f: _,
                        arg: Some(x0),
                    }) => {
                        let ap1 = Value::Ap(Ap {
                            f: Some(x2),
                            arg: Some(x0),
                        });
                        (
                            true,
                            Value::Ap(Ap {
                                f: Some(Box::new(ap1)),
                                arg: Some(x1),
                            }),
                        )
                    }
                    other => (false, other),
                },
                other => (false, other),
            },
            other => (false, other),
        }
    }
}

impl From<Node> for Rule {
    fn from(node: Node) -> Self {
        match node {
            Node::Eq(eq) => Self {
                left: Value::from(*eq.left),
                right: Value::from(*eq.right),
            },
            other => panic!("Impossible to convert {:?} into Rule", other),
        }
    }
}

impl From<Node> for Value {
    fn from(node: Node) -> Self {
        match node {
            Node::Num(n) => Value::Num(n),
            Node::Ref(r) => Value::Ref(Ref::from(r)),
            Node::F(ast::Fun::Inc) => Value::F(Fun::Inc),
            Node::F(ast::Fun::Dec) => Value::F(Fun::Dec),
            Node::F(ast::Fun::Add) => Value::F(Fun::Add),
            Node::F(ast::Fun::Mul) => Value::F(Fun::Mul),
            Node::F(ast::Fun::Div) => Value::F(Fun::Div),
            Node::F(ast::Fun::T) => Value::F(Fun::T),
            Node::F(ast::Fun::F) => Value::F(Fun::F),
            Node::F(ast::Fun::Lt) => Value::F(Fun::Lt),
            Node::F(ast::Fun::Mod) => Value::F(Fun::Mod),
            Node::F(ast::Fun::Dem) => Value::F(Fun::Dem),
            Node::F(ast::Fun::Send) => Value::F(Fun::Send),
            Node::F(ast::Fun::Neg) => Value::F(Fun::Neg),
            Node::F(ast::Fun::Pwr2) => Value::F(Fun::Pwr2),
            Node::F(ast::Fun::S) => Value::F(Fun::S),
            Node::F(ast::Fun::C) => Value::F(Fun::C),
            Node::F(ast::Fun::B) => Value::F(Fun::B),
            Node::F(ast::Fun::I) => Value::F(Fun::I),
            Node::F(ast::Fun::Cons) => Value::F(Fun::Cons),
            Node::F(ast::Fun::Car) => Value::F(Fun::Car),
            Node::F(ast::Fun::Cdr) => Value::F(Fun::Cdr),
            Node::F(ast::Fun::Nil) => Value::F(Fun::Nil),
            Node::F(ast::Fun::IsNil) => Value::F(Fun::IsNil),
            Node::F(ast::Fun::Vec) => Value::F(Fun::Vec),
            Node::F(ast::Fun::Draw) => Value::F(Fun::Draw),
            Node::F(ast::Fun::Chkb) => Value::F(Fun::Chkb),
            Node::F(ast::Fun::MultipleDraw) => Value::F(Fun::MultipleDraw),
            Node::F(ast::Fun::If0) => Value::F(Fun::If0),
            Node::F(ast::Fun::Interact) => Value::F(Fun::Interact),
            Node::F(ast::Fun::Galaxy) => Value::F(Fun::Galaxy),
            Node::Ap(ap) => Value::Ap(Ap::from(ap)),
            Node::Eq(_) => panic!("Don't know how to convert eq"),
        }
    }
}

impl From<ast::Ap> for Ap {
    fn from(ap: ast::Ap) -> Self {
        Self {
            f: ap.f.map(|v| Box::new(Value::from(*v))),
            arg: ap.arg.map(|v| Box::new(Value::from(*v))),
        }
    }
}

impl From<ast::Ref> for Ref {
    fn from(r: ast::Ref) -> Self {
        Ref::Ref(r.addr)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Num(n) => write!(f, "{}", n),
            Value::F(fun) => write!(f, "{}", fun),
            Value::Ref(r) => write!(f, "{}", r),
            Value::Ap(ap) => write!(f, "{}", ap),
        }
    }
}

impl fmt::Display for Fun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Fun::Inc => write!(f, "inc"),
            Fun::Dec => write!(f, "dec"),
            Fun::Add => write!(f, "add"),
            Fun::Mul => write!(f, "mul"),
            Fun::Div => write!(f, "div"),
            Fun::T => write!(f, "t"),
            Fun::F => write!(f, "f"),
            Fun::Lt => write!(f, "lt"),
            Fun::Mod => write!(f, "mod"),
            Fun::Dem => write!(f, "dem"),
            Fun::Send => write!(f, "send"),
            Fun::Neg => write!(f, "neg"),
            Fun::Pwr2 => write!(f, "pwr2"),
            Fun::S => write!(f, "s"),
            Fun::C => write!(f, "c"),
            Fun::B => write!(f, "b"),
            Fun::I => write!(f, "i"),
            Fun::Cons => write!(f, "cons"),
            Fun::Cdr => write!(f, "cdr"),
            Fun::Car => write!(f, "car"),
            Fun::Nil => write!(f, "nil"),
            Fun::IsNil => write!(f, "isnil"),
            Fun::Vec => write!(f, "vec"),
            Fun::Draw => write!(f, "draw"),
            Fun::Chkb => write!(f, "chkb"),
            Fun::MultipleDraw => write!(f, "multipledraw"),
            Fun::If0 => write!(f, "if0"),
            Fun::Interact => write!(f, "interact"),
            Fun::Galaxy => write!(f, "galaxy"),
        }
    }
}

impl fmt::Display for Ref {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ref::Ref(addr) => write!(f, ":{}", *addr),
            Ref::HiddenRef(addr) => write!(f, "#{}", *addr),
        }
    }
}

impl fmt::Display for Ap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ap")?;
        if let Some(v) = &self.f {
            write!(f, " {}", *v)?;
        }
        if let Some(v) = &self.arg {
            write!(f, " {}", *v)?;
        }
        write!(f, "")
    }
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.left, self.right)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::build_eq_tree;
    use crate::parser::parse;

    fn rules(input: &[&str]) -> Vec<Rule> {
        input
            .iter()
            .map(|s| Rule::from(build_eq_tree(&parse(s)).unwrap()))
            .collect()
    }

    fn with_rules(input: &[&str]) -> Solver {
        let mut result = Solver::new();
        for r in rules(input).into_iter() {
            result.add_rule(r).unwrap();
        }
        result
    }

    #[test]
    fn test_rules_load() {
        let solver = with_rules(&vec![
            ":1 = ap ap cons 1 nil",
            ":2 = cons",
            "nil = ap ap cons t t",
        ]);
        assert_eq!(
            format!("{}", solver.get(&Ref::Ref(1)).unwrap()),
            "ap ap cons 1 nil"
        );
        assert_eq!(None, solver.get(&Ref::Ref(5)));
    }

    #[test]
    fn test_i() {
        let mut solver = with_rules(&vec![":1 = ap i :2", ":2 = 1", ":3 = ap ap ap i 1 :1 2"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(2)), 100)),
            "1"
        );
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(3)), 100)),
            "ap ap 1 :1 2"
        );
    }
    #[test]
    fn test_nil() {
        let mut solver = with_rules(&vec![":1 = ap nil :2"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 100)),
            "t"
        );
    }

    #[test]
    fn test_k() {
        let mut solver = with_rules(&vec![":1 = ap ap t 1 2", ":2 = ap ap t t ap inc 5"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 100)),
            "1"
        );
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(2)), 100)),
            "t"
        );
    }

    #[test]
    fn test_false() {
        let mut solver = with_rules(&vec![":1 = ap ap f 1 2"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 100)),
            "2"
        );
    }

    #[test]
    fn test_s() {
        let mut solver = with_rules(&vec![":1 = ap ap ap s inc dec :3"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 100)),
            "ap ap inc :3 ap dec :3"
        );
    }

    #[test]
    fn test_b() {
        let mut solver = with_rules(&vec![":1 = ap ap ap b inc dec mul"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 100)),
            "ap inc ap dec mul"
        );
    }

    #[test]
    fn test_c() {
        let mut solver = with_rules(&vec![":1 = ap ap ap c inc dec mul"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 100)),
            "ap ap inc mul dec"
        );
    }

    #[test]
    fn test_cons() {
        let mut solver = with_rules(&vec![":1 = ap ap ap cons inc dec mul"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 100)),
            "ap ap mul inc dec"
        );
    }

    #[test]
    fn test_depth_limit() {
        let mut solver = with_rules(&vec![":1 = ap :1 :1"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 10)),
            "ap ap ap ap ap :1 :1 :1 :1 :1 :1"
        );
    }

    #[test]
    fn test_simple_definiton() {
        let mut solver = with_rules(&vec!["galaxy = ap inc 1"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::F(Fun::Galaxy), 10)),
            "ap inc 1"
        );
    }
}
