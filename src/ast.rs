use crate::operand::{Address, Op};

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Ap(Ap),
    Define(Define),
    Ref(Ref),
    Num(i64),
    F(Fun),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ap {
    pub f: Option<Box<Node>>,
    pub arg: Option<Box<Node>>,
}

impl Ap {
    pub fn new(f: Node, arg: Node) -> Ap {
        Ap {
            f: Some(Box::new(f)),
            arg: Some(Box::new(arg)),
        }
    }

    pub fn partial(f: Node) -> Ap {
        Ap {
            f: Some(Box::new(f)),
            arg: None,
        }
    }

    pub fn essence() -> Ap {
        Ap { f: None, arg: None }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Define {
    pub left: Box<Node>,
    pub right: Box<Node>,
}

impl Define {
    pub fn new(left: Node, right: Node) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
        }
    }
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Ref {
    pub addr: Address,
}

impl Ref {
    pub fn new(addr: Address) -> Ref {
        Ref { addr }
    }
}

pub fn build_define_tree(ops: &[Op]) -> Result<Node, String> {
    let (left, tail) = build_subtree(ops)?;
    if !tail.starts_with(&[Op::Define]) {
        return Err(format!(
            "Unexpected {:?} instead of expected =",
            tail.get(0)
        ));
    }
    let (right, tail) = build_subtree(&tail[1..])?;
    match (left, right, tail) {
        (None, _right, _tail) => Err("Unexpected empty left subtree".to_string()),
        (Some(_), None, _tail) => Err("Unexpected empty right subtree".to_string()),
        (Some(left), Some(right), []) => Ok(Node::Define(Define::new(left, right))),
        (Some(_left), Some(_right), tail) => Err(format!("Unexpected tail {:?}", tail)),
    }
}

pub fn build_subtree(ops: &[Op]) -> Result<(Option<Node>, &[Op]), String> {
    if let [] = ops {
        return Ok((None, ops));
    }
    match ops[0] {
        Op::Define => Ok((None, ops)),
        Op::Ap => {
            let (f, tail) = build_subtree(&ops[1..])?;
            let (arg, tail) = build_subtree(tail)?;
            let ap = match (f, arg) {
                (None, None) => Ap::essence(),
                (Some(f), None) => Ap::partial(f),
                (Some(f), Some(arg)) => Ap::new(f, arg),
                res => panic!("O_o, unexpected pair {:?}", res),
            };
            Ok((Some(Node::Ap(ap)), tail))
        }
        op => Ok((Some(build_simple_node(&op)), &ops[1..])),
    }
}

fn build_simple_node(op: &Op) -> Node {
    match op {
        Op::Num(n) => Node::Num(*n),
        Op::Var(a) => Node::Ref(Ref::new(*a)),
        Op::Inc => Node::F(Fun::Inc),
        Op::Dec => Node::F(Fun::Dec),
        Op::Add => Node::F(Fun::Add),
        Op::Mul => Node::F(Fun::Mul),
        Op::Div => Node::F(Fun::Div),
        Op::T => Node::F(Fun::T),
        Op::F => Node::F(Fun::F),
        Op::Lt => Node::F(Fun::Lt),
        Op::Eq => Node::F(Fun::Eq),
        Op::Mod => Node::F(Fun::Mod),
        Op::Dem => Node::F(Fun::Dem),
        Op::Send => Node::F(Fun::Send),
        Op::Neg => Node::F(Fun::Neg),
        Op::Pwr2 => Node::F(Fun::Pwr2),
        Op::S => Node::F(Fun::S),
        Op::C => Node::F(Fun::C),
        Op::B => Node::F(Fun::B),
        Op::I => Node::F(Fun::I),
        Op::Cons => Node::F(Fun::Cons),
        Op::Car => Node::F(Fun::Car),
        Op::Cdr => Node::F(Fun::Cdr),
        Op::Nil => Node::F(Fun::Nil),
        Op::IsNil => Node::F(Fun::IsNil),
        Op::Vec => Node::F(Fun::Vec),
        Op::Draw => Node::F(Fun::Draw),
        Op::Chkb => Node::F(Fun::Chkb),
        Op::MultipleDraw => Node::F(Fun::MultipleDraw),
        Op::If0 => Node::F(Fun::If0),
        Op::Interact => Node::F(Fun::Interact),
        Op::Galaxy => Node::F(Fun::Galaxy),
        Op::Define => panic!("Don't know how to build define"),
        Op::Ap => panic!("Don't know how to build ap"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    fn build_define(input: &str) -> Result<Node, String> {
        build_define_tree(&parse(input))
    }

    fn eq(left: Node, right: Node) -> Node {
        Node::Define(Define::new(left, right))
    }

    fn ap(f: Node, arg: Node) -> Node {
        Node::Ap(Ap::new(f, arg))
    }

    fn ap_p(f: Node) -> Node {
        Node::Ap(Ap::partial(f))
    }

    fn ap_e() -> Node {
        Node::Ap(Ap::essence())
    }

    fn f(f: Fun) -> Node {
        Node::F(f)
    }

    fn r(addr: Address) -> Node {
        Node::Ref(Ref::new(addr))
    }

    fn n(n: i64) -> Node {
        Node::Num(n)
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            Ok(eq(ap_p(ap_p(ap_e())), ap_p(ap_p(ap_e())))),
            build_define("ap ap ap = ap ap ap")
        );
        assert_eq!(
            Ok(eq(ap(f(Fun::Inc), r(0)), n(-2))),
            build_define("ap inc :0 = -2")
        );
        assert_eq!(
            Ok(eq(ap_p(f(Fun::Inc)), n(-2))),
            build_define("ap inc = -2")
        );
        assert_eq!(Ok(eq(ap_e(), n(-2))), build_define("ap = -2"));
        assert_eq!(
            Ok(eq(
                ap(r(200), r(0)),
                ap(
                    ap(ap(f(Fun::S), r(1)), r(2)),
                    ap(ap(f(Fun::Cons), n(1)), f(Fun::Nil))
                )
            )),
            build_define("ap :200 :0 = ap ap ap s :1 :2 ap ap cons 1 nil")
        );
    }
}
