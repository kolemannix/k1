use crate::typer::*;

#[test]
fn struct_layout_1() {
    let mut l = Layout::ZERO;
    l.append_to_aggregate(Layout::from_scalar_bits(64));
    assert_eq!(l.size_bits, 64);
    assert_eq!(l.align_bits, 64);
    assert_eq!(l.stride_bits(), 64);
    assert_eq!(l.size_bytes(), 8);
    assert_eq!(l.align_bytes(), 8);
    assert_eq!(l.stride_bytes(), 8);

    l.append_to_aggregate(Layout::from_scalar_bits(8));
    assert_eq!(l.size_bits, 72);
    assert_eq!(l.align_bits, 64);
    assert_eq!(l.stride_bits(), 128);
    assert_eq!(l.size_bytes(), 9);
    assert_eq!(l.align_bytes(), 8);
    assert_eq!(l.stride_bytes(), 16);

    l.append_to_aggregate(Layout::from_scalar_bits(32));
    assert_eq!(l.size_bits, 128);
    assert_eq!(l.align_bits, 64);
    assert_eq!(l.stride_bits(), 128);
    assert_eq!(l.size_bytes(), 16);
    assert_eq!(l.align_bytes(), 8);
    assert_eq!(l.stride_bytes(), 16);
}

#[test]
fn add_zero_no_change() {
    let mut l = Layout::ZERO;
    let o1 = l.append_to_aggregate(Layout::from_scalar_bits(64));
    assert_eq!(o1, 0);
    assert_eq!(l.size_bits, 64);
    assert_eq!(l.align_bits, 64);
    assert_eq!(l.stride_bits(), 64);
    let o2 = l.append_to_aggregate(Layout::from_scalar_bits(8));
    assert_eq!(o2, 64);
    assert_eq!(l.size_bits, 72);
    assert_eq!(l.align_bits, 64);
    assert_eq!(l.stride_bits(), 128);
    let o3 = l.append_to_aggregate(Layout::ZERO);
    assert_eq!(o3, 72);
    assert_eq!(l.size_bits, 72);
    assert_eq!(l.align_bits, 64);
    assert_eq!(l.stride_bits(), 128);
}
