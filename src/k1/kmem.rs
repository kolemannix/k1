// Copyright (c) 2025 knix
// All rights reserved.
use crate::static_assert_size;
/// A wildly unsafe memory Arena because I never
/// intend to free any of this except when throwing
/// away the entire compiler and I simply cannot deal
/// with adding lifetime annotations to every single struct
/// and function in the entire codebase
///
/// I am just having fun with this; I may regret it and
/// decide to stick with safer Rust, but I already have
/// the wildly unsafe VM that executes my language raw
/// so who cares.
///
/// My main use case for this Arena is to allocate all the tiny
/// Vecs and EcoVecs that never need to be mutated here and avoid
/// all their issues; we should be able to go from 192 bytes to 64 per collection
/// if our slice handle is 64 bytes, which it should be, if I'm willing to
/// address less than 4GB in here and can use a 32-bit base. Actually I can use
/// 48 bits for the base and 16 for the length
macro_rules! fuckit {
    ($($t:tt)*) => {
        unsafe { $($t)* }
    };
}

use core::mem::{align_of, size_of};
use std::{num::NonZeroU32, ops::Deref};
pub struct Mem {
    mmap: memmap2::MmapMut,
    cursor: *const u8,
}

// We use NonZeroU32 so that the handles are niched, allowing for use
// for no size cost in types like Option and Result
pub struct MHandle<T>(NonZeroU32, std::marker::PhantomData<T>);
static_assert_size!(MHandle<u128>, 4);

impl<T> Copy for MHandle<T> {}
impl<T> Clone for MHandle<T> {
    fn clone(&self) -> Self {
        *self
    }
}

/// A handle to a slice of Ts inside a `Mem` pool
pub struct MSlice<T> {
    offset: NonZeroU32,
    count: u32,
    _marker: std::marker::PhantomData<T>,
    // TODO: Add fingerprints to handles in dbg mode
    // #[cfg(feature = "dbg")]
    // fingerprint: u64,
}
static_assert_size!(MSlice<u128>, 8);
impl<T> Copy for MSlice<T> {}
impl<T> Clone for MSlice<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> MSlice<T> {
    pub fn empty() -> Self {
        const BOGUS_OFFSET: NonZeroU32 = NonZeroU32::new(8).unwrap();
        // `offset` should never be touched when count is 0
        Self { offset: BOGUS_OFFSET, count: 0, _marker: std::marker::PhantomData }
    }
    pub fn len(&self) -> u32 {
        self.count
    }
}

impl Mem {
    pub fn make() -> Self {
        // Note(knix) If we never allow larger than a 4gb allocation, then we are safe to hand out 32-bit
        //            offsets instead of pointers, which could be big for some codebases
        let mmap = memmap2::MmapMut::map_anon(crate::GIGABYTE).unwrap();
        mmap.advise(memmap2::Advice::Sequential).unwrap();

        // We waste the first 8 bytes every time, so that our handles can be niched
        // We could instead store offsets as (true offset) + 1, but I'd rather waste 8 bytes per
        // arena than convert on every single access
        let cursor = unsafe { mmap.as_ptr().byte_add(8) };

        Self { cursor, mmap }
    }
    fn cursor_mut(&self) -> *mut u8 {
        self.cursor.cast_mut()
    }

    fn base_ptr(&self) -> *const u8 {
        self.mmap.as_ptr()
    }

    fn ptr_to_offset<T: ?Sized>(&self, ptr: *const T) -> NonZeroU32 {
        let Some(result) = NonZeroU32::new(ptr.addr() as u32 - self.base_ptr().addr() as u32)
        else {
            panic!("Attempted to create a null handle");
        };
        result
    }

    fn offset_to_ptr<T>(&self, offset: NonZeroU32) -> *const T {
        unsafe { self.base_ptr().add(offset.get() as usize) as *const T }
    }

    fn pack_handle<T>(&self, ptr: *const T) -> MHandle<T> {
        let offset = self.ptr_to_offset(ptr);
        MHandle(offset, std::marker::PhantomData)
    }

    fn unpack_handle<T>(&self, handle: MHandle<T>) -> *const T {
        self.offset_to_ptr::<T>(handle.0)
    }

    pub fn vec_to_mslice<T>(&self, m_vec: &MVec<T>) -> MSlice<T> {
        let offset = self.ptr_to_offset(m_vec.buf.cast_const());
        MSlice { offset, count: m_vec.len() as u32, _marker: std::marker::PhantomData }
    }

    #[track_caller]
    #[inline]
    fn check_in_bounds(&self, ptr: usize) {
        if ptr >= self.mmap.as_ptr_range().end.addr() {
            panic!("Address out of bounds: {}", ptr,);
        }
    }

    #[track_caller]
    #[inline]
    fn check_mine(&self, ptr: usize) {
        if ptr < self.base_ptr().addr() {
            panic!("Address not mine (less than base): {} < {}", ptr, self.base_ptr().addr());
        }
        if ptr > self.cursor.addr() {
            panic!("Address is beyond cursor: {} > {}", ptr, self.cursor.addr());
        }
    }

    #[track_caller]
    #[inline]
    fn set_cursor_checked(&mut self, proposed_cursor: *const u8) {
        self.check_in_bounds(proposed_cursor.addr());
        self.cursor = proposed_cursor;
    }

    pub fn push<T: Copy>(&mut self, t: T) -> &mut T {
        unsafe {
            let dst = self.cursor_mut();
            let dst = dst.byte_add(dst.align_offset(align_of::<T>()));

            let new_cursor = dst.byte_add(size_of::<T>());
            self.set_cursor_checked(new_cursor);

            let dst = dst as *mut T;
            core::ptr::write(dst, t);

            // std::mem::transmute::<&T, &'static T>(&*dst)
            &mut *dst
        }
    }

    pub fn push_h<T: Copy>(&mut self, t: T) -> MHandle<T> {
        let t_ptr = self.push(t) as *const T;
        self.pack_handle(t_ptr)
    }

    pub fn new_vec<T>(&mut self, len: usize) -> MVec<T> {
        unsafe {
            let start_cursor = self.cursor_mut();
            let dst_aligned = start_cursor.byte_add(start_cursor.align_offset(align_of::<T>()));
            let layout = core::alloc::Layout::array::<T>(len).unwrap();

            let new_cursor = dst_aligned.byte_add(layout.size());
            self.set_cursor_checked(new_cursor);

            let raw_slice: *mut [T] =
                core::ptr::slice_from_raw_parts_mut(dst_aligned as *mut T, len);
            MVec { buf: raw_slice, len: 0 }
        }
    }

    pub fn push_slice<T: Copy>(&mut self, ts: &[T]) -> MSlice<T> {
        unsafe {
            let dst = self.cursor_mut();
            let dst = dst.byte_add(dst.align_offset(align_of::<T>()));

            let new_cursor = dst.byte_add(size_of_val(ts));
            self.set_cursor_checked(new_cursor);

            let dst: &mut [T] = std::slice::from_raw_parts_mut(dst as *mut T, ts.len());
            dst.copy_from_slice(ts);
            MSlice {
                offset: self.ptr_to_offset(dst.as_ptr()),
                count: ts.len() as u32,
                _marker: std::marker::PhantomData,
            }
        }
    }

    pub fn get<T>(&self, handle: MHandle<T>) -> &T {
        let ptr = self.unpack_handle(handle);
        if cfg!(feature = "dbg") {
            self.check_mine(ptr.addr())
        }
        unsafe { &*ptr }
    }

    pub fn get_copy<T: Copy>(&self, handle: MHandle<T>) -> T {
        let ptr = self.unpack_handle(handle);
        if cfg!(feature = "dbg") {
            self.check_mine(ptr.addr())
        }
        unsafe { *ptr }
    }

    pub fn get_slice<T>(&self, handle: MSlice<T>) -> &'static [T] {
        fuckit! {
            let ptr: *const T = self.offset_to_ptr(handle.offset);
            if cfg!(debug_assertions) {
                self.check_mine(ptr.addr())
            }
            let src: &[T] = std::slice::from_raw_parts(ptr, handle.count as usize);
            src
        }
    }

    pub fn get_nth<T>(&self, handle: MSlice<T>, n: usize) -> &'static T {
        &self.get_slice(handle)[n]
    }

    pub fn bytes_used(&self) -> usize {
        self.cursor.addr() - self.base_ptr().addr()
    }
}

impl Mem {
    pub fn print_usage(&self, name: &str) {
        let used_kb = self.bytes_used() / crate::KILOBYTE;
        let total = self.mmap.len() / crate::KILOBYTE;
        let percent = (used_kb as f64) / (total as f64) * 100.0;
        println!("{name} usage: {used_kb}/{total}kb ({percent:.2}%)");
    }
}

/// A fixed-size Vec-like collection pointing into a Mem's data
pub struct MVec<T> {
    buf: *mut [T],
    len: usize,
}

impl<T> MVec<T> {
    pub fn len(&self) -> usize {
        self.len
    }
    pub fn push(&mut self, val: T) {
        if self.len == self.buf.len() {
            panic!("AVec is full {}", self.buf.len());
        }
        unsafe {
            (*self.buf)[self.len] = val;
        }
        self.len += 1;
    }

    pub fn try_push(&mut self, val: T) -> Result<(), T> {
        if self.len == self.buf.len() {
            Err(val)
        } else {
            unsafe {
                (*self.buf)[self.len] = val;
            }
            self.len += 1;
            Ok(())
        }
    }

    pub fn as_slice(&self) -> &[T] {
        unsafe { &(*self.buf)[..self.len] }
    }

    pub fn as_slice_mut(&mut self) -> &mut [T] {
        unsafe { &mut (*self.buf)[..self.len] }
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.as_slice().iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.as_slice_mut().iter_mut()
    }
}

impl<T> std::ops::Index<usize> for MVec<T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        &self.as_slice()[index]
    }
}

impl<T> Deref for MVec<T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test() {
        let mut arena = Mem::make();
        let handle = arena.push_h(42u32);
        let value = arena.get(handle);
        assert_eq!(*value, 42);
        let handle2 = arena.push_h(43u32);
        let value2 = *arena.get(handle2);
        assert_eq!(value2, 43);

        let h3 = arena.push_slice(&[1, 2, 3, 4]);
        assert_eq!(h3.len(), 4);

        assert_eq!(arena.get_slice(h3), &[1, 2, 3, 4]);
    }

    #[test]
    fn vec() {
        let mut arena = Mem::make();
        let mut v = arena.new_vec(16);
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
    #[should_panic(expected = "AVec is full")]
    fn vec_oob() {
        let mut arena = Mem::make();
        let mut v = arena.new_vec(4);
        for i in 0..5 {
            v.push(i * 10);
        }
    }
}
