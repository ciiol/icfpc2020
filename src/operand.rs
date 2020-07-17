#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Op {
    Num(i64),     // message #1
    Var(usize),   // message #8
    Ap,           // message #17
    Eq,           // message #4
    Inc,          // message #5
    Dec,          // message #6
    Add,          // message #7
    Mul,          // message #9
    Div,          // message #10
    T,            // message #21, #11
    F,            // message #22, #11
    Lt,           // message #12
    Mod,          // message #13
    Dem,          // message #14
    Send,         // message #15
    Neg,          // message #16
    Pwr2,         // message #23
    S,            // message #18
    C,            // message #19
    B,            // message #20
    I,            // message #24
    Cons,         // message #25
    Car,          // message #26
    Cdr,          // message #27
    Nil,          // message #28
    IsNil,        // message #29
    LBracket,     // message #30
    RBracket,     // message #30
    Comma,        // message #30
    Vec,          // message #31
    Draw,         // message #32
    Chkb,         // message #33
    MultipleDraw, // message #34
    If0,          // message #37
    Interact,     // message #38
}
