use crate::ast::{self, Node};
use crate::operand::Address;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Solver {
    memory: HashMap<Address, Value>,
    hidden_memory: Vec<Value>,
    definitions: HashMap<Fun, Value>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Num(i64),
    Ref(Ref),
    Ap(Ap),
    F(Fun),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rule {
    left: Value,
    right: Value,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Ref {
    Ref(Address),
    HiddenRef(Address),
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Fun {
    Inc,
    Dec,
    Add,
    Mul,
    Div,
    T,
    F,
    Lt,
    Eq,
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

#[derive(Debug, Clone, PartialEq, Eq)]
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
        match r {
            Ref::Ref(addr) => {
                self.memory.insert(addr, value);
                ()
            }
            Ref::HiddenRef(addr) => self.hidden_memory[addr] = value,
        };
        Ok(())
    }

    pub fn put_hidden(&mut self, value: Value) -> Ref {
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
                let new_value = self.deduce(branch, depth - 1);
                self.put(r, new_value.clone()).unwrap();
                new_value
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
        let (changed, new_val) = self.simplify(val, depth);
        if changed {
            self.deduce(new_val, depth - 1)
        } else {
            new_val
        }
    }

    fn simplify(&mut self, entry: Value, depth: usize) -> (bool, Value) {
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
            (_, _, Some(Fun::Cons)) => self.apply_cons_combinator(entry),
            (_, Some(Fun::Add), _) => self.apply_add(entry, depth),
            (_, Some(Fun::Cons), _) => self.apply_cons(entry, depth),
            (_, Some(Fun::Mul), _) => self.apply_mul(entry, depth),
            (_, Some(Fun::Div), _) => self.apply_div(entry, depth),
            (_, Some(Fun::Lt), _) => self.apply_lt(entry, depth),
            (_, Some(Fun::Eq), _) => self.apply_eq(entry, depth),
            (Some(Fun::Inc), _, _) => self.apply_inc(entry, depth),
            (Some(Fun::Dec), _, _) => self.apply_dec(entry, depth),
            (Some(Fun::Pwr2), _, _) => self.apply_pwr2(entry, depth),
            (Some(Fun::Car), _, _) => self.apply_car(entry),
            (Some(Fun::Cdr), _, _) => self.apply_cdr(entry),
            (Some(Fun::IsNil), _, _) => self.apply_isnil(entry, depth),
            (Some(Fun::If0), _, _) => self.apply_if0(entry, depth),
            (Some(Fun::Neg), _, _) => self.apply_neg(entry, depth),
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
        match deconstruct_ap2(entry) {
            (_f, Some(x0), Some(_)) => (true, *x0),
            (f, x0, x1) => (false, reconstruct_ap2(f, x0, x1)),
        }
    }

    fn apply_false(&mut self, entry: Value) -> (bool, Value) {
        match deconstruct_ap2(entry) {
            (_f, Some(_), Some(x1)) => (true, *x1),
            (f, x0, x1) => (false, reconstruct_ap2(f, x0, x1)),
        }
    }

    fn apply_combinator_s(&mut self, entry: Value) -> (bool, Value) {
        match deconstruct_ap3(entry) {
            (_f, Some(x0), Some(x1), Some(x2)) => {
                let ref_x2 = self.put_hidden(*x2);
                (
                    true,
                    ap(ap(*x0, Value::Ref(ref_x2)), ap(*x1, Value::Ref(ref_x2))),
                )
            }
            (f, x0, x1, x2) => (false, reconstruct_ap3(f, x0, x1, x2)),
        }
    }

    fn apply_combinator_c(&mut self, entry: Value) -> (bool, Value) {
        match deconstruct_ap3(entry) {
            (_f, x0 @ Some(_), x1 @ Some(_), x2 @ Some(_)) => (true, reconstruct_ap2(x0, x2, x1)),
            (f, x0, x1, x2) => (false, reconstruct_ap3(f, x0, x1, x2)),
        }
    }

    fn apply_combinator_b(&mut self, entry: Value) -> (bool, Value) {
        match deconstruct_ap3(entry) {
            (_f, Some(x0), Some(x1), Some(x2)) => (true, ap(*x0, ap_boxed(x1, x2))),
            (f, x0, x1, x2) => (false, reconstruct_ap3(f, x0, x1, x2)),
        }
    }

    fn apply_cons_combinator(&mut self, entry: Value) -> (bool, Value) {
        match deconstruct_ap3(entry) {
            (_f, x0 @ Some(_), x1 @ Some(_), x2 @ Some(_)) => (true, reconstruct_ap2(x2, x0, x1)),
            (f, x0, x1, x2) => (false, reconstruct_ap3(f, x0, x1, x2)),
        }
    }

    fn apply_cons(&mut self, entry: Value, depth: usize) -> (bool, Value) {
        match deconstruct_ap2(entry) {
            (f, Some(x0), Some(x1)) => {
                let x0 = self.deduce(*x0, depth - 1);
                let x1 = self.deduce(*x1, depth - 1);
                (
                    false,
                    reconstruct_ap2(f, Some(Box::new(x0)), Some(Box::new(x1))),
                )
            }
            (f, x0, x1) => (false, reconstruct_ap2(f, x0, x1)),
        }
    }

    fn apply_add(&mut self, entry: Value, depth: usize) -> (bool, Value) {
        match deconstruct_ap2(entry) {
            (f, Some(x0), Some(x1)) => {
                let x0 = self.deduce(*x0, depth - 1);
                let x1 = self.deduce(*x1, depth - 1);
                match (x0, x1) {
                    (Value::Num(n1), Value::Num(n2)) => (true, Value::Num(n1 + n2)),
                    (x0, x1) => (false, ap(ap_with_f(f, x0), x1)),
                }
            }
            (f, x0, x1) => (false, reconstruct_ap2(f, x0, x1)),
        }
    }

    fn apply_mul(&mut self, entry: Value, depth: usize) -> (bool, Value) {
        match deconstruct_ap2(entry) {
            (f, Some(x0), Some(x1)) => {
                let x0 = self.deduce(*x0, depth - 1);
                let x1 = self.deduce(*x1, depth - 1);
                match (x0, x1) {
                    (Value::Num(n1), Value::Num(n2)) => (true, Value::Num(n1 * n2)),
                    (x0, x1) => (false, ap(ap_with_f(f, x0), x1)),
                }
            }
            (f, x0, x1) => (false, reconstruct_ap2(f, x0, x1)),
        }
    }

    fn apply_div(&mut self, entry: Value, depth: usize) -> (bool, Value) {
        match deconstruct_ap2(entry) {
            (f, Some(x0), Some(x1)) => {
                let x0 = self.deduce(*x0, depth - 1);
                let x1 = self.deduce(*x1, depth - 1);
                match (x0, x1) {
                    (Value::Num(n1), Value::Num(n2)) => (true, Value::Num(n1 / n2)),
                    (x0, x1) => (false, ap(ap_with_f(f, x0), x1)),
                }
            }
            (f, x0, x1) => (false, reconstruct_ap2(f, x0, x1)),
        }
    }

    fn apply_lt(&mut self, entry: Value, depth: usize) -> (bool, Value) {
        match deconstruct_ap2(entry) {
            (f, Some(x0), Some(x1)) => {
                let x0 = self.deduce(*x0, depth - 1);
                let x1 = self.deduce(*x1, depth - 1);
                match (x0, x1) {
                    (Value::Num(n1), Value::Num(n2)) => {
                        let res = if n1 < n2 { Fun::T } else { Fun::F };
                        (true, Value::F(res))
                    }
                    (x0, x1) => (false, ap(ap_with_f(f, x0), x1)),
                }
            }
            (f, x0, x1) => (false, reconstruct_ap2(f, x0, x1)),
        }
    }

    fn apply_eq(&mut self, entry: Value, depth: usize) -> (bool, Value) {
        match deconstruct_ap2(entry) {
            (f, Some(x0), Some(x1)) => {
                let x0 = self.deduce(*x0, depth - 1);
                let x1 = self.deduce(*x1, depth - 1);
                match (x0, x1) {
                    (Value::Num(n1), Value::Num(n2)) => {
                        let res = if n1 == n2 { Fun::T } else { Fun::F };
                        (true, Value::F(res))
                    }
                    (x0, x1) => (false, ap(ap_with_f(f, x0), x1)),
                }
            }
            (f, x0, x1) => (false, reconstruct_ap2(f, x0, x1)),
        }
    }

    fn apply_inc(&mut self, entry: Value, depth: usize) -> (bool, Value) {
        match deconstruct_ap1(entry) {
            (f, Some(x0)) => {
                let x0 = self.deduce(*x0, depth - 1);
                match x0 {
                    Value::Num(n) => (true, Value::Num(n + 1)),
                    x0 => (false, ap_with_f(f, x0)),
                }
            }
            (f, x0) => (false, reconstruct_ap1(f, x0)),
        }
    }

    fn apply_dec(&mut self, entry: Value, depth: usize) -> (bool, Value) {
        match deconstruct_ap1(entry) {
            (f, Some(x0)) => {
                let x0 = self.deduce(*x0, depth - 1);
                match x0 {
                    Value::Num(n) => (true, Value::Num(n - 1)),
                    x0 => (false, ap_with_f(f, x0)),
                }
            }
            (f, x0) => (false, reconstruct_ap1(f, x0)),
        }
    }

    fn apply_pwr2(&mut self, entry: Value, depth: usize) -> (bool, Value) {
        match deconstruct_ap1(entry) {
            (f, Some(x0)) => {
                let x0 = self.deduce(*x0, depth - 1);
                match x0 {
                    Value::Num(n) if n >= 0 => (true, Value::Num((2 as i64).pow(n as u32))),
                    x0 => (false, ap_with_f(f, x0)),
                }
            }
            (f, x0) => (false, reconstruct_ap1(f, x0)),
        }
    }

    fn apply_car(&mut self, entry: Value) -> (bool, Value) {
        match deconstruct_ap1(entry) {
            (_f, Some(x0)) => (
                true,
                reconstruct_ap1(Some(x0), Some(Box::new(Value::F(Fun::T)))),
            ),
            _ => panic!("Not implemented: partial car"),
        }
    }

    fn apply_cdr(&mut self, entry: Value) -> (bool, Value) {
        match deconstruct_ap1(entry) {
            (_f, Some(x0)) => (
                true,
                reconstruct_ap1(Some(x0), Some(Box::new(Value::F(Fun::F)))),
            ),
            _ => panic!("Not implemented: partial cdr"),
        }
    }

    fn apply_isnil(&mut self, entry: Value, depth: usize) -> (bool, Value) {
        match deconstruct_ap1(entry) {
            (f, Some(x0)) => {
                let x0 = self.deduce(*x0, depth - 1);
                match x0 {
                    Value::F(Fun::Nil) => (true, Value::F(Fun::T)),
                    x0 => {
                        if let Some(Fun::Cons) = x0.seek_left_fun(2) {
                            (true, Value::F(Fun::F))
                        } else {
                            (false, ap_with_f(f, x0))
                        }
                    }
                }
            }
            (f, x0) => (false, reconstruct_ap1(f, x0)),
        }
    }

    fn apply_if0(&mut self, entry: Value, depth: usize) -> (bool, Value) {
        match deconstruct_ap1(entry) {
            (f, Some(x0)) => {
                let x0 = self.deduce(*x0, depth - 1);
                match x0 {
                    Value::Num(n) => {
                        let res = if n == 0 { Fun::T } else { Fun::F };
                        (true, Value::F(res))
                    }
                    x0 => (false, ap_with_f(f, x0)),
                }
            }
            (f, x0) => (false, reconstruct_ap1(f, x0)),
        }
    }

    fn apply_neg(&mut self, entry: Value, depth: usize) -> (bool, Value) {
        match deconstruct_ap1(entry) {
            (f, Some(x0)) => {
                let x0 = self.deduce(*x0, depth - 1);
                match x0 {
                    Value::Num(n) => (true, Value::Num(-n)),
                    x0 => (false, ap_with_f(f, x0)),
                }
            }
            (f, x0) => (false, reconstruct_ap1(f, x0)),
        }
    }
}

pub fn ap(f: Value, arg: Value) -> Value {
    Value::Ap(Ap {
        f: Some(Box::new(f)),
        arg: Some(Box::new(arg)),
    })
}

fn ap_boxed(f: Box<Value>, arg: Box<Value>) -> Value {
    Value::Ap(Ap {
        f: Some(f),
        arg: Some(arg),
    })
}

fn ap_with_f(f: Option<Box<Value>>, arg: Value) -> Value {
    Value::Ap(Ap {
        f,
        arg: Some(Box::new(arg)),
    })
}

pub fn reconstruct_ap1(f: Option<Box<Value>>, x0: Option<Box<Value>>) -> Value {
    Value::Ap(Ap { f, arg: x0 })
}

pub fn deconstruct_ap1(entry: Value) -> (Option<Box<Value>>, Option<Box<Value>>) {
    match entry {
        Value::Ap(Ap { f, arg: x0 }) => (f, x0),
        _other => panic!("It isn't ap1"),
    }
}

pub fn reconstruct_ap2(
    f: Option<Box<Value>>,
    x0: Option<Box<Value>>,
    x1: Option<Box<Value>>,
) -> Value {
    Value::Ap(Ap {
        f: Some(Box::new(Value::Ap(Ap { f, arg: x0 }))),
        arg: x1,
    })
}

pub fn deconstruct_ap2(
    entry: Value,
) -> (Option<Box<Value>>, Option<Box<Value>>, Option<Box<Value>>) {
    match entry {
        Value::Ap(Ap {
            f: Some(f),
            arg: x1,
        }) => match *f {
            Value::Ap(Ap { f, arg: x0 }) => (f, x0, x1),
            _other => panic!("It isn't ap2"),
        },
        _other => panic!("It isn't ap2"),
    }
}

pub fn deconstruct_ap3(
    entry: Value,
) -> (
    Option<Box<Value>>,
    Option<Box<Value>>,
    Option<Box<Value>>,
    Option<Box<Value>>,
) {
    match entry {
        Value::Ap(Ap {
            f: Some(f),
            arg: x2,
        }) => match *f {
            Value::Ap(Ap {
                f: Some(f),
                arg: x1,
            }) => match *f {
                Value::Ap(Ap { f, arg: x0 }) => (f, x0, x1, x2),
                _other => panic!("It isn't ap3"),
            },
            _other => panic!("It isn't ap3"),
        },
        _other => panic!("It isn't ap3"),
    }
}

pub fn reconstruct_ap3(
    f: Option<Box<Value>>,
    x0: Option<Box<Value>>,
    x1: Option<Box<Value>>,
    x2: Option<Box<Value>>,
) -> Value {
    Value::Ap(Ap {
        f: Some(Box::new(Value::Ap(Ap {
            f: Some(Box::new(Value::Ap(Ap { f, arg: x0 }))),
            arg: x1,
        }))),
        arg: x2,
    })
}

pub fn deconstruct_pair(value: Value) -> Option<(Value, Value)> {
    match deconstruct_ap2(value) {
        (Some(f), Some(x0), Some(x1)) => match (*f, *x0, *x1) {
            (Value::F(Fun::Cons), x0, x1) => Some((x0, x1)),
            (_f, _x0, _x1) => None,
        },
        (_f, _x0, _x1) => None,
    }
}

impl From<Node> for Rule {
    fn from(node: Node) -> Self {
        match node {
            Node::Define(eq) => Self {
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
            Node::F(ast::Fun::Eq) => Value::F(Fun::Eq),
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
            Node::Define(_) => panic!("Don't know how to convert eq"),
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
            Fun::Eq => write!(f, "eq"),
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
    use crate::ast;
    use crate::parser::parse;

    fn rules(input: &[&str]) -> Vec<Rule> {
        input
            .iter()
            .map(|s| Rule::from(ast::build_define_tree(&parse(s)).unwrap()))
            .collect()
    }

    fn with_rules(input: &[&str]) -> Solver {
        let mut result = Solver::new();
        for r in rules(input).into_iter() {
            result.add_rule(r).unwrap();
        }
        result
    }

    fn entry(input: &str) -> Value {
        let entry = match ast::build_subtree(&parse(input)).unwrap() {
            (Some(entry), []) => entry,
            (_entry, tail) => panic!("Unexpected tail {:?}", tail),
        };
        Value::from(entry)
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
        assert_eq!(format!("{}", solver.deduce(Value::F(Fun::Galaxy), 10)), "2");
    }

    #[test]
    fn test_memoization() {
        let mut solver = with_rules(&vec![":1 = ap ap add 1 2", ":2 = ap inc :1"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(2)), 100)),
            "4"
        );
        assert_eq!(format!("{}", solver.get(&Ref::Ref(1)).unwrap()), "3");
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
        let mut solver = with_rules(&vec![
            ":1 = ap ap ap s add inc 1",
            ":2 = ap ap ap s mul ap add 1 6",
        ]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 100)),
            "3"
        );
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(2)), 100)),
            "42"
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
        let mut solver = with_rules(&vec![
            ":1 = ap ap ap ap c inc dec mul inc",
            ":2 = ap ap ap c add 1 2",
        ]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 100)),
            "ap ap ap inc mul dec inc"
        );
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(2)), 100)),
            "3"
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
    fn test_add() {
        let mut solver = with_rules(&vec![":1 = ap ap add 1 2"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 10)),
            "3"
        );
    }

    #[test]
    fn test_inc() {
        let mut solver = with_rules(&vec![":1 = ap inc 2"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 10)),
            "3"
        );
    }

    #[test]
    fn test_dec() {
        let mut solver = with_rules(&vec![":1 = ap dec 2"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 10)),
            "1"
        );
    }

    #[test]
    fn test_mul() {
        let mut solver = with_rules(&vec![":1 = ap ap mul 3 2"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 10)),
            "6"
        );
    }

    #[test]
    fn test_div() {
        let mut solver = with_rules(&vec![":1 = ap ap div 5 2"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 10)),
            "2"
        );
    }

    #[test]
    fn test_pwr2() {
        let mut solver = with_rules(&vec![":1 = ap pwr2 2", ":2 = ap pwr2 0"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 10)),
            "4"
        );
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(2)), 10)),
            "1"
        );
    }

    #[test]
    fn test_car() {
        let mut solver = with_rules(&vec![":1 = ap car ap ap cons 1 2"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 10)),
            "1"
        );
    }

    #[test]
    fn test_cdr() {
        let mut solver = with_rules(&vec![":1 = ap cdr ap ap cons 1 2"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 10)),
            "2"
        );
    }

    #[test]
    fn test_eq() {
        let mut solver = with_rules(&vec![
            ":1 = ap ap eq -19 -20",
            ":2 = ap ap eq -20 -20",
            ":3 = ap ap eq -21 -20",
        ]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 10)),
            "f"
        );
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(2)), 10)),
            "t"
        );
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(3)), 10)),
            "f"
        );
    }

    #[test]
    fn test_lt() {
        let mut solver = with_rules(&vec![
            ":1 = ap ap lt -19 -20",
            ":2 = ap ap lt -20 -20",
            ":3 = ap ap lt -21 -20",
        ]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 10)),
            "f"
        );
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(2)), 10)),
            "f"
        );
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(3)), 10)),
            "t"
        );
    }

    #[test]
    fn test_isnil() {
        let mut solver = with_rules(&vec![":1 = ap isnil nil", ":2 = ap isnil ap ap cons 1 2"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 10)),
            "t"
        );
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(2)), 10)),
            "f"
        );
    }

    #[test]
    fn test_if0() {
        let mut solver = with_rules(&vec![":1 = ap ap ap if0 0 1 2", ":2 = ap ap ap if0 1 1 2"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 10)),
            "1"
        );
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(2)), 10)),
            "2"
        );
    }

    #[test]
    fn test_neg() {
        let mut solver = with_rules(&vec![":1 = ap neg 1", ":2 = ap neg -1"]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 10)),
            "-1"
        );
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(2)), 10)),
            "1"
        );
    }

    #[test]
    fn test_cons_recursive() {
        let mut solver = with_rules(&vec![
            ":1 = ap ap cons ap inc 1 ap dec 1",
            ":2 = ap ap cons 1 ap ap ap i cons ap inc 1 ap ap cons 3 nil",
        ]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 10)),
            "ap ap cons 2 0"
        );
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(2)), 10)),
            "ap ap cons 1 ap ap cons 2 ap ap cons 3 nil"
        );
        assert_eq!(
            format!("{}", solver.deduce(entry("ap ap cons 1 ap inc 1"), 10)),
            "ap ap cons 1 2"
        );
    }

    #[test]
    fn test_cominators() {
        let mut solver = with_rules(&vec![
            ":1 = ap ap ap s ap ap c ap eq 0 1 ap ap b ap mul 2 ap ap b pwr2 ap add -1 0",
        ]);
        assert_eq!(
            format!("{}", solver.deduce(Value::Ref(Ref::Ref(1)), 10)),
            "1"
        );
    }
}
