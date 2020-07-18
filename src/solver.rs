use crate::ast::{self, Node};
use crate::operand::Address;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
struct Solver {
    memory: HashMap<Address, Value>,
    hidden_memory: HashMap<Address, Value>,
    rules: Vec<Rule>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Num(i64),
    Ref(Ref),
    Ap(Ap),
    F(Fun),
    Pair(Pair),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Rule {
    left: Value,
    right: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ref {
    Ref(Address),
    HiddenRef(Address),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pair {
    Nil,
    Pair(Box<Value>, Box<Value>),
}

#[derive(Debug, Copy, Clone, PartialEq)]
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

impl Solver {
    pub fn new() -> Self {
        Self {
            memory: HashMap::new(),
            hidden_memory: HashMap::new(),
            rules: Vec::new(),
        }
    }

    pub fn get(&self, r: &Ref) -> Option<&Value> {
        match r {
            Ref::Ref(addr) => self.memory.get(addr),
            Ref::HiddenRef(addr) => self.hidden_memory.get(addr),
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
            Ref::HiddenRef(addr) => self.hidden_memory.insert(addr, value),
        };
        Ok(())
    }

    pub fn get_mut(&mut self, r: &Ref) -> Option<&mut Value> {
        match r {
            Ref::Ref(addr) => self.memory.get_mut(addr),
            Ref::HiddenRef(addr) => self.hidden_memory.get_mut(addr),
        }
    }

    pub fn add_rule(&mut self, rule: Rule) -> Result<(), String> {
        match rule {
            Rule {
                left: Value::Ref(r),
                right: value,
            } => self.put(r, value)?,
            rule => self.rules.push(rule),
        }
        Ok(())
    }

    pub fn deduce(&mut self, entry: Value, depth: usize) -> Value {
        // TODO
        entry
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
            Value::Pair(p) => write!(f, "{}", p),
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

impl fmt::Display for Pair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pair::Nil => write!(f, "nil"),
            Pair::Pair(l, r) => {
                write!(f, "[{}", *l)?;
                let mut head = r;
                loop {
                    match head.as_ref() {
                        Value::Pair(Pair::Nil) => break,
                        Value::Pair(Pair::Pair(l, r)) => {
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
}
