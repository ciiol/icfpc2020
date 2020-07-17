use crate::operand::Op;
use regex::Regex;

pub fn parse(text: &str) -> Vec<Op> {
    text.split_whitespace().map(decode).collect()
}

fn decode(token: &str) -> Op {
    lazy_static! {
        static ref NUMBERS_RE: Regex = Regex::new(r"^-?\d+$").unwrap();
        static ref VARIABLES_RE: Regex = Regex::new(r"^:(\d+)$").unwrap();
    }
    if NUMBERS_RE.is_match(token) {
        return Op::Num(token.parse().unwrap());
    }
    if let Some(captured) = VARIABLES_RE.captures_iter(token).next() {
        return Op::Var((&captured[1]).parse().unwrap());
    }
    match token {
        "ap" => Op::Ap,
        "=" => Op::Eq,
        "inc" => Op::Inc,
        "dec" => Op::Dec,
        "add" => Op::Add,
        "mul" => Op::Mul,
        "div" => Op::Div,
        "t" => Op::T,
        "f" => Op::F,
        "lt" => Op::Lt,
        "mod" => Op::Mod,
        "dem" => Op::Dem,
        "send" => Op::Send,
        "neg" => Op::Neg,
        "pwr2" => Op::Pwr2,
        "s" => Op::S,
        "c" => Op::C,
        "b" => Op::B,
        "i" => Op::I,
        "cons" => Op::Cons,
        "cdr" => Op::Cdr,
        "car" => Op::Car,
        "nil" => Op::Nil,
        "isnil" => Op::IsNil,
        "vec" => Op::Vec,
        "draw" => Op::Draw,
        "chkb" => Op::Chkb,
        "multipledraw" => Op::MultipleDraw,
        "if0" => Op::If0,
        "interact" => Op::Interact,
        _ => panic!("Unknown operand {}", token),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(
            parse("ap inc :0 1 = -2"),
            vec![Op::Ap, Op::Inc, Op::Var(0), Op::Num(1), Op::Eq, Op::Num(-2)]
        );
    }

    #[test]
    fn test_number_decoding() {
        assert_eq!(decode("123"), Op::Num(123));
        assert_eq!(decode("-123"), Op::Num(-123));
    }

    #[test]
    fn test_variable_decoding() {
        assert_eq!(decode(":0"), Op::Var(0));
        assert_eq!(decode(":1"), Op::Var(1));
        assert_eq!(decode(":100500"), Op::Var(100500));
    }

    #[test]
    fn test_inc_decode() {
        assert_eq!(decode("inc"), Op::Inc)
    }

    #[test]
    fn test_dec_decode() {
        assert_eq!(decode("dec"), Op::Dec)
    }

    #[test]
    fn test_add_decode() {
        assert_eq!(decode("add"), Op::Add)
    }

    #[test]
    fn test_mul_decode() {
        assert_eq!(decode("mul"), Op::Mul)
    }

    #[test]
    fn test_div_decode() {
        assert_eq!(decode("div"), Op::Div)
    }

    #[test]
    fn test_t_decode() {
        assert_eq!(decode("t"), Op::T)
    }

    #[test]
    fn test_f_decode() {
        assert_eq!(decode("f"), Op::F)
    }

    #[test]
    fn test_lt_decode() {
        assert_eq!(decode("lt"), Op::Lt)
    }

    #[test]
    fn test_mod_decode() {
        assert_eq!(decode("mod"), Op::Mod)
    }

    #[test]
    fn test_dem_decode() {
        assert_eq!(decode("dem"), Op::Dem)
    }

    #[test]
    fn test_send_decode() {
        assert_eq!(decode("send"), Op::Send)
    }

    #[test]
    fn test_neg_decode() {
        assert_eq!(decode("neg"), Op::Neg)
    }

    #[test]
    fn test_pwr2_decode() {
        assert_eq!(decode("pwr2"), Op::Pwr2)
    }

    #[test]
    fn test_s_decode() {
        assert_eq!(decode("s"), Op::S)
    }

    #[test]
    fn test_c_decode() {
        assert_eq!(decode("c"), Op::C)
    }

    #[test]
    fn test_b_decode() {
        assert_eq!(decode("b"), Op::B)
    }

    #[test]
    fn test_i_decode() {
        assert_eq!(decode("i"), Op::I)
    }

    #[test]
    fn test_cons_decode() {
        assert_eq!(decode("cons"), Op::Cons)
    }

    #[test]
    fn test_cdr_decode() {
        assert_eq!(decode("cdr"), Op::Cdr)
    }

    #[test]
    fn test_car_decode() {
        assert_eq!(decode("car"), Op::Car)
    }

    #[test]
    fn test_nil_decode() {
        assert_eq!(decode("nil"), Op::Nil)
    }

    #[test]
    fn test_isnil_decode() {
        assert_eq!(decode("isnil"), Op::IsNil)
    }

    #[test]
    fn test_vec_decode() {
        assert_eq!(decode("vec"), Op::Vec)
    }

    #[test]
    fn test_draw_decode() {
        assert_eq!(decode("draw"), Op::Draw)
    }

    #[test]
    fn test_chkb_decode() {
        assert_eq!(decode("chkb"), Op::Chkb)
    }

    #[test]
    fn test_multipledraw_decode() {
        assert_eq!(decode("multipledraw"), Op::MultipleDraw)
    }

    #[test]
    fn test_if0_decode() {
        assert_eq!(decode("if0"), Op::If0)
    }

    #[test]
    fn test_interact_decode() {
        assert_eq!(decode("interact"), Op::Interact)
    }
}
