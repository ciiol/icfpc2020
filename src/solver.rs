use crate::ast::{self, Node};
use crate::operand::Address;
use std::collections::HashMap;

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

    pub fn add_rule(&mut self, rule: Rule) -> () {
        self.rules.push(rule)
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
