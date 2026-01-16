// Copyright (c) 2025 knix
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
