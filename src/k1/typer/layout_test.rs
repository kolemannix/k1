// Copyright (c) 2026 knix
// All rights reserved.

use crate::typer::*;

#[test]
fn struct_layout_1() {
    let mut l = Layout::ZERO_SIZED;
    l.append_to_aggregate(Layout::from_scalar_bits(64));
    assert_eq!(l.size, 8);
    assert_eq!(l.align, 8);
    assert_eq!(l.stride(), 8);

    assert_eq!(l.size_bits(), 64);
    assert_eq!(l.align_bits(), 64);

    l.append_to_aggregate(Layout::from_scalar_bits(8));
    assert_eq!(l.size, 9);
    assert_eq!(l.align, 8);
    assert_eq!(l.stride(), 16);

    assert_eq!(l.size_bits(), 72);
    assert_eq!(l.align_bits(), 64);

    l.append_to_aggregate(Layout::from_scalar_bits(32));
    assert_eq!(l.size, 16);
    assert_eq!(l.align, 8);
    assert_eq!(l.stride(), 16);
}

#[test]
fn add_zero_no_change() {
    let mut l = Layout::ZERO_SIZED;
    let o1 = l.append_to_aggregate(Layout::from_scalar_bits(64));
    assert_eq!(o1, 0);
    assert_eq!(l.size, 8);
    assert_eq!(l.align, 8);
    assert_eq!(l.stride(), 8);
    let o2 = l.append_to_aggregate(Layout::from_scalar_bits(8));
    assert_eq!(o2, 8);
    assert_eq!(l.size, 9);
    assert_eq!(l.align, 8);
    assert_eq!(l.stride(), 16);
    let o3 = l.append_to_aggregate(Layout::ZERO_SIZED);
    assert_eq!(o3, 9);
    assert_eq!(l.size, 9);
    assert_eq!(l.align, 8);
    assert_eq!(l.stride(), 16);
}

#[test]
fn aggregate_member_tail_padding_not_reused() {
    // inner = { i64, u8 }: size 9, align 8, stride 16
    let mut inner = Layout::ZERO_SIZED;
    inner.append_to_aggregate(Layout::from_scalar_bits(64));
    inner.append_to_aggregate(Layout::from_scalar_bits(8));
    assert_eq!(inner.size, 9);
    assert_eq!(inner.stride(), 16);

    // { inner, u8 }: the u8 goes after inner's alloc size (16), not into its
    // tail padding at 9 — matching C and LLVM struct layout
    let mut outer = Layout::ZERO_SIZED;
    let o1 = outer.append_to_aggregate(inner);
    let o2 = outer.append_to_aggregate(Layout::from_scalar_bits(8));
    assert_eq!(o1, 0);
    assert_eq!(o2, 16);
    assert_eq!(outer.size, 17);
    assert_eq!(outer.align, 8);
    assert_eq!(outer.stride(), 24);
}

#[test]
fn packed_layout() {
    // packed { u8, u64, u16 }: offsets 0/1/9, size 11, align 1
    let mut l = Layout::ZERO_SIZED;
    let o1 = l.append_to_aggregate_packed(Layout::from_scalar_bits(8));
    let o2 = l.append_to_aggregate_packed(Layout::from_scalar_bits(64));
    let o3 = l.append_to_aggregate_packed(Layout::from_scalar_bits(16));
    assert_eq!(o1, 0);
    assert_eq!(o2, 1);
    assert_eq!(o3, 9);
    assert_eq!(l.size, 11);
    assert_eq!(l.align, 1);
    assert_eq!(l.stride(), 11);

    // aggregate members advance by stride here too
    let mut p = Layout::ZERO_SIZED;
    let inner = Layout { size: 9, align: 8 };
    let po1 = p.append_to_aggregate_packed(inner);
    let po2 = p.append_to_aggregate_packed(Layout::from_scalar_bits(8));
    assert_eq!(po1, 0);
    assert_eq!(po2, 16);
    assert_eq!(p.size, 17);
    assert_eq!(p.align, 1);
    assert_eq!(p.stride(), 17);
}

#[test]
fn array_me() {
    let l = Layout::ZERO_SIZED.array_me(10);
    assert_eq!(l.size, 0);
    assert_eq!(l.align, 1);
    assert_eq!(l.stride(), 0);
    let l2 = Layout { size: 5, align: 4 };
    assert_eq!(l2.stride(), 8);
    let l3 = l2.array_me(10);
    assert_eq!(l3.size, 80);
    assert_eq!(l3.align, 4);
    assert_eq!(l3.stride(), 80);
}
