use super::*;

#[test]
fn test() {
    let mut mem: Mem<()> = Mem::make();
    let handle = mem.push_h(42u32);
    let value = mem.get(handle);
    assert_eq!(*value, 42);
    let handle2 = mem.push_h(43u32);
    let value2 = *mem.get(handle2);
    assert_eq!(value2, 43);

    let h3 = mem.pushn(&[1, 2, 3, 4]);
    assert_eq!(h3.len(), 4);

    assert_eq!(mem.getn(h3), &[1, 2, 3, 4]);
}

#[test]
fn push_slice_iter() {
    let mut arena: Mem<()> = Mem::make();
    let h = arena.pushn_iter((0..10).map(|x| x * 10));
    assert_eq!(h.len(), 10);
    assert_eq!(arena.getn(h), &[0, 10, 20, 30, 40, 50, 60, 70, 80, 90]);

    let empty_h = arena.pushn_iter(std::iter::empty::<u32>());
    assert_eq!(empty_h.len(), 0);
    assert!(arena.getn(empty_h).is_empty());
}

#[test]
fn vec() {
    let mut mem: Mem<()> = Mem::make();
    let mut v = mem.new_list(16);
    for i in 0..16 {
        v.push(i * 10);
    }
    assert_eq!(v.len(), 16);
    for i in 0..16 {
        assert_eq!(v.as_slice()[i], i * 10);
    }
    let err = v.try_push(42);
    assert!(err.is_err());
}

#[test]
fn vec_extend() {
    let mut mem: Mem<()> = Mem::make();
    let mut v = mem.new_list(16);
    v.extend(&[1, 2, 3, 4, 5]);
    assert_eq!(v.len(), 5);
    for i in 0..5 {
        assert_eq!(v.as_slice()[i], i + 1);
    }

    v.extend(&[6]);
    assert_eq!(v.len(), 6);
}

#[test]
#[should_panic(expected = "MList is full")]
fn vec_extend_oob() {
    let mut mem: Mem<()> = Mem::make();
    let mut v = mem.new_list(3);
    v.extend(&[1, 2, 3, 4, 5]);
}

#[test]
#[should_panic(expected = "MList is full")]
fn vec_oob() {
    let mut mem: Mem<()> = Mem::make();
    let mut v = mem.new_list(4);
    for i in 0..5 {
        v.push(i * 10);
    }
}

#[test]
fn dup_slice() {
    let mut mem: Mem<()> = Mem::make();
    let h = mem.pushn(&[1, 2, 3, 4, 5]);
    let h2 = mem.dupn(h);
    assert_eq!(h.len(), h2.len());
    assert_eq!(mem.getn(h), mem.getn(h2));
    assert_ne!(h.offset, h2.offset);
}

#[test]
fn insert() {
    let mut mem: Mem<()> = Mem::make();
    let mut v = mem.new_list(5);
    v.push(1);
    v.push(2);
    v.push(4);
    v.insert(2, 3);
    assert_eq!(v.len(), 4);
    assert_eq!(v.as_slice(), &[1, 2, 3, 4]);
}

#[test]
fn grow() {
    let mut mem: Mem<()> = Mem::make();
    let mut v = mem.new_list(2);
    let initial_base = v.base_ptr();
    v.push_grow(&mut mem, 1);
    v.push_grow(&mut mem, 2);
    v.push_grow(&mut mem, 3);

    // Check that the in-place growth optimization occurs
    assert_eq!(v.base_ptr(), initial_base);
    assert_eq!(v.len(), 3);
    assert_eq!(v.as_slice(), &[1, 2, 3]);
}

#[test]
fn grow_from_zero() {
    let mut mem: Mem<()> = Mem::make();
    let mut v = mem.new_list(0);
    v.push_grow(&mut mem, 1);
    v.push_grow(&mut mem, 2);
    v.push_grow(&mut mem, 3);
    assert_eq!(v.len(), 3);
    assert_eq!(v.as_slice(), &[1, 2, 3]);
}

#[test]
fn spill_list_inline_and_spill() {
    let mut mem: Mem<()> = Mem::make();

    // Inline path (N=2): 0, 1, 2 elements
    let mut a: MSL2<u32> = MSpillList::new();
    assert!(!a.is_spilled() && a.is_empty() && a.as_slice(&mem).is_empty());
    a.push(&mut mem, 10);
    assert!(!a.is_spilled() && a.len() == 1 && a.as_slice(&mem) == [10]);
    a.push(&mut mem, 20);
    assert!(!a.is_spilled() && a.len() == 2 && a.as_slice(&mem) == [10, 20]);

    // First overflow triggers spill; order preserved
    a.push(&mut mem, 30);
    assert!(a.is_spilled() && a.len() == 3 && a.as_slice(&mem) == [10, 20, 30]);

    // Further pushes go to spilled list
    a.push(&mut mem, 40);
    a.push(&mut mem, 50);
    assert!(a.is_spilled() && a.len() == 5 && a.as_slice(&mem) == [10, 20, 30, 40, 50]);
}

#[test]
fn spill_list_into_handle_both_paths() {
    let mut mem: Mem<()> = Mem::make();

    // Inline -> handle allocates exactly and matches contents
    let mut inl: MSL2<u32> = MSpillList::new();
    inl.push(&mut mem, 1);
    inl.push(&mut mem, 2);
    assert!(!inl.is_spilled());
    let h_inl = inl.into_slice(&mut mem);
    assert_eq!(h_inl.len(), 2);
    assert_eq!(mem.getn(h_inl), &[1, 2]);

    // Spilled -> handle should reuse underlying buffer (offset should match list buf)
    let mut sp: MSL2<u32> = MSpillList::new();
    sp.push(&mut mem, 7);
    sp.push(&mut mem, 8);
    sp.push(&mut mem, 9); // spill
    assert!(sp.is_spilled());
    let before_ptr = sp.as_slice(&mem).as_ptr().addr();
    let h_sp = sp.into_slice(&mut mem);
    let after_ptr = mem.getn(h_sp).as_ptr().addr();
    assert_eq!(mem.getn(h_sp), &[7, 8, 9]);
    assert_eq!(before_ptr, after_ptr);
}

#[test]
fn spill_list_many_pushes_growth_smoke() {
    let mut mem: Mem<()> = Mem::make();

    // Ensure spill + repeated pushes stays correct across grows
    let mut v: MSL2<u32> = MSpillList::new();
    for i in 0..10 {
        v.push(&mut mem, i);
    }
    assert!(v.is_spilled() && v.len() == 10);
    let s = v.as_slice(&mem);
    assert_eq!(s.len(), 10);
    assert_eq!(s[0], 0);
    assert_eq!(s[1], 1);
    assert_eq!(s[2], 2);
    assert_eq!(s[5], 5);
    assert_eq!(s[9], 9);

    let h = v.into_slice(&mut mem);
    assert_eq!(h.len(), 10);
    assert_eq!(mem.get_nth(h, 0), &0);
    assert_eq!(mem.get_nth(h, 9), &9);
}

#[allow(clippy::needless_range_loop)]
fn assert_dlist(mem: &Mem<()>, l: Dlist<char, ()>, expected: &[char]) {
    let elems: Vec<_> = mem.dlist_iter(l).map(|x| *x).collect();
    mem.dlist_assert_valid(l);
    assert_eq!(&elems, expected);
    assert_eq!(mem.dlist_compute_len(l), expected.len());
    for i in 0..mem.dlist_compute_len(l) {
        assert_eq!(mem.dlist_nth(l, i).data, expected[i]);
        assert_eq!(*mem.dlist_nth_data(l, i), expected[i]);
    }
}

#[test]
fn mdl_basic() {
    let mut mem: Mem<()> = Mem::make();
    let mut l = mem.dlist_new();
    mem.dlist_push(&mut l, '5');
    mem.dlist_assert_valid(l);
    mem.dlist_push(&mut l, '4');
    mem.dlist_push(&mut l, '3');
    mem.dlist_push(&mut l, '2');
    mem.dlist_push(&mut l, '1');
    assert_dlist(&mem, l, &['5', '4', '3', '2', '1']);

    // Insert begin
    mem.dlist_insert(&mut l, 0, '6');
    assert_dlist(&mem, l, &['6', '5', '4', '3', '2', '1']);

    // Insert end
    mem.dlist_insert(&mut l, 6, '0');
    assert_dlist(&mem, l, &['6', '5', '4', '3', '2', '1', '0']);

    // Insert middle
    mem.dlist_insert(&mut l, 3, 'Y');
    assert_dlist(&mem, l, &['6', '5', '4', 'Y', '3', '2', '1', '0']);
    mem.dlist_insert(&mut l, 3, 'X');
    assert_dlist(&mem, l, &['6', '5', '4', 'X', 'Y', '3', '2', '1', '0']);

    // Remove first, last, and middle
    mem.dlist_remove_index(&mut l, 0);
    mem.dlist_remove_index(&mut l, 7);
    mem.dlist_remove_index(&mut l, 4);
    assert_dlist(&mem, l, &['5', '4', 'X', 'Y', '2', '1']);

    assert_eq!(mem.dlist_pop_last(&mut l).unwrap().data, '1');
    assert_dlist(&mem, l, &['5', '4', 'X', 'Y', '2']);
}

#[test]
fn mdl_pop_last() {
    let mut mem: Mem<()> = Mem::make();
    let mut l = mem.dlist_new();

    assert!(mem.dlist_pop_last(&mut l).is_none());

    mem.dlist_push(&mut l, 'a');
    let Some(popped) = mem.dlist_pop_last(&mut l) else { panic!() };
    assert_eq!(popped.data, 'a');
    assert_dlist(&mem, l, &[]);

    mem.dlist_push(&mut l, 'a');
    let first = l.first;
    mem.dlist_remove(&mut l, first);
    assert_dlist(&mem, l, &[]);
}

#[test]
fn mdl_pop_first() {
    let mut mem: Mem<()> = Mem::make();
    let mut l = mem.dlist_new();

    assert!(mem.dlist_pop_first(&mut l).is_none());

    mem.dlist_push(&mut l, 'a');
    let Some(popped) = mem.dlist_pop_first(&mut l) else { panic!() };
    assert_eq!(popped.data, 'a');
    assert_dlist(&mem, l, &[]);
}

#[test]
fn mdl_split() {
    let mut mem: Mem<()> = Mem::make();
    let mut l = mem.dlist_new();
    for c in ['a', 'b', 'c', 'd', 'e'] {
        mem.dlist_push(&mut l, c);
    }
    assert_dlist(&mem, l, &['a', 'b', 'c', 'd', 'e']);

    let mut l2 = mem.dlist_split_at_index(&mut l, 2);
    assert_dlist(&mem, l, &['a', 'b']);
    assert_dlist(&mem, l2, &['c', 'd', 'e']);

    let l3 = mem.dlist_split_at_index(&mut l2, 0);
    assert_dlist(&mem, l2, &[]);
    assert_dlist(&mem, l3, &['c', 'd', 'e']);

    let l4 = mem.dlist_split_at_index(&mut l, 2);
    assert_dlist(&mem, l, &['a', 'b']);
    assert_dlist(&mem, l4, &[]);

    let last = l.last;
    let l5 = mem.dlist_split_at_node(&mut l, last);
    assert_dlist(&mem, l, &['a']);
    assert_dlist(&mem, l5, &['b']);
}
