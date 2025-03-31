use crate::typer::*;

#[test]
fn struct_layout_1() {
    let mut l = Layout::ZERO;
    l.append_to_aggregate(Layout::from_scalar_bits(64));
    assert_eq!(l.size_bits, 64);
    assert_eq!(l.align_bits, 64);
    assert_eq!(l.stride_bits, 64);
    l.append_to_aggregate(Layout::from_scalar_bits(8));
    assert_eq!(l.size_bits, 72);
    assert_eq!(l.align_bits, 64);
    assert_eq!(l.stride_bits, 128);
    l.append_to_aggregate(Layout::from_scalar_bits(32));
    assert_eq!(l.size_bits, 128);
    assert_eq!(l.align_bits, 64);
    assert_eq!(l.stride_bits, 128);
}

#[test]
fn add_zero_no_change() {
    let mut l = Layout::ZERO;
    let o1 = l.append_to_aggregate(Layout::from_scalar_bits(64));
    assert_eq!(o1, 0);
    assert_eq!(l.size_bits, 64);
    assert_eq!(l.align_bits, 64);
    assert_eq!(l.stride_bits, 64);
    let o2 = l.append_to_aggregate(Layout::from_scalar_bits(8));
    assert_eq!(o2, 64);
    assert_eq!(l.size_bits, 72);
    assert_eq!(l.align_bits, 64);
    assert_eq!(l.stride_bits, 128);
    let o3 = l.append_to_aggregate(Layout::ZERO);
    assert_eq!(o3, 72);
    assert_eq!(l.size_bits, 72);
    assert_eq!(l.align_bits, 64);
    assert_eq!(l.stride_bits, 128);
}
