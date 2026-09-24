use super::*;

const EDGES: [u64; 12] =
    [0, 1, 2, 3, 0x7f, 0x80, 0xff, 0x7fff, 0x8000, 0x7fff_ffff, 0x8000_0000, u64::MAX];

macro_rules! check_width {
    ($u:ty, $i:ty, $width:expr) => {{
        let width: u8 = $width;
        let canon = |v: $u| v as u64;
        let canon_i = |v: $i| v as $u as u64;
        for &a in &EDGES {
            for &b in &EDGES {
                let (ua, ub) = (a as $u, b as $u);
                let (ia, ib) = (a as $i, b as $i);
                let shift = (b as u32) & (width as u32 - 1);
                assert_eq!(int_op(IntOp::Add, width, a, b), Ok(canon(ua.wrapping_add(ub))));
                assert_eq!(int_op(IntOp::Sub, width, a, b), Ok(canon(ua.wrapping_sub(ub))));
                assert_eq!(int_op(IntOp::Mul, width, a, b), Ok(canon(ua.wrapping_mul(ub))));
                assert_eq!(int_op(IntOp::And, width, a, b), Ok(canon(ua & ub)));
                assert_eq!(int_op(IntOp::Or, width, a, b), Ok(canon(ua | ub)));
                assert_eq!(int_op(IntOp::Xor, width, a, b), Ok(canon(ua ^ ub)));
                assert_eq!(int_op(IntOp::Shl, width, a, b), Ok(canon(ua << shift)));
                assert_eq!(int_op(IntOp::ShrU, width, a, b), Ok(canon(ua >> shift)));
                assert_eq!(int_op(IntOp::ShrS, width, a, b), Ok(canon_i(ia >> shift)));
                match ub {
                    0 => {
                        assert_eq!(
                            int_op(IntOp::DivU, width, a, b),
                            Err(IntOpError::DivisionByZero)
                        );
                        assert_eq!(
                            int_op(IntOp::DivS, width, a, b),
                            Err(IntOpError::DivisionByZero)
                        );
                        assert_eq!(
                            int_op(IntOp::RemS, width, a, b),
                            Err(IntOpError::DivisionByZero)
                        );
                    }
                    _ => {
                        assert_eq!(int_op(IntOp::DivU, width, a, b), Ok(canon(ua / ub)));
                        assert_eq!(int_op(IntOp::RemU, width, a, b), Ok(canon(ua % ub)));
                        match ia.checked_div(ib) {
                            Some(q) => {
                                assert_eq!(int_op(IntOp::DivS, width, a, b), Ok(canon_i(q)));
                                assert_eq!(int_op(IntOp::RemS, width, a, b), Ok(canon_i(ia % ib)));
                            }
                            None => assert_eq!(
                                int_op(IntOp::DivS, width, a, b),
                                Err(IntOpError::SignedDivisionOverflow)
                            ),
                        }
                    }
                }
                assert_eq!(int_cmp(width, IntCmpPred::Ult, a, b), ua < ub);
                assert_eq!(int_cmp(width, IntCmpPred::Slt, a, b), ia < ib);
                assert_eq!(int_cmp(width, IntCmpPred::Eq, a, b), ua == ub);
            }
            assert_eq!(bit_not(width, a), canon(!(a as $u)));
            assert_eq!(int_trunc(width, a), canon(a as $u));
            assert_eq!(int_ext_s(width, 64, a), (a as $i) as i64 as u64);
        }
    }};
}

#[test]
fn int_ops_match_native_typed_arithmetic() {
    check_width!(u8, i8, 8);
    check_width!(u16, i16, 16);
    check_width!(u32, i32, 32);
    check_width!(u64, i64, 64);
}

#[test]
fn results_are_zero_extended() {
    assert_eq!(int_op(IntOp::DivS, 8, 0xff, 1), Ok(0xff));
    assert_eq!(int_op(IntOp::ShrS, 16, 0x8000, 15), Ok(0xffff));
    assert_eq!(bit_not(8, 0), 0xff);
    assert_eq!(int_ext_s(8, 32, 0xfd), 0xffff_fffd);
}
