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
pub struct A {
    mmap: memmap2::MmapMut,
    cursor: *const u8,
}

static_assert_size!(AHandle<u128>, 4);
pub struct AHandle<T>(u32, std::marker::PhantomData<T>);

static_assert_size!(ASliceHandle<u128>, 8);
#[derive(Clone, Copy)]
pub struct ASliceHandle<T> {
    offset: u32,
    count: u32,
    _marker: std::marker::PhantomData<T>,
    // TODO: Add fingerprints to handles in dbg mode
    // #[cfg(feature = "dbg")]
    // fingerprint: u64,
}

impl<T> ASliceHandle<T> {
    pub fn empty() -> Self {
        Self { offset: 0, count: 0, _marker: std::marker::PhantomData }
    }
    pub fn len(&self) -> u32 {
        self.count
    }
}

impl A {
    pub fn make() -> Self {
        // Note(knix) If we never allow larger than a 4gb allocation, then we are safe to hand out 32-bit
        //            offsets instead of pointers, which could be big for some codebases
        let mmap = memmap2::MmapMut::map_anon(1 * crate::GIGABYTE).unwrap();
        mmap.advise(memmap2::Advice::Sequential).unwrap();
        Self { cursor: mmap.as_ptr(), mmap }
    }
    fn cursor_mut(&self) -> *mut u8 {
        self.cursor.cast_mut()
    }

    fn base_ptr(&self) -> *const u8 {
        self.mmap.as_ptr()
    }

    fn ptr_to_offset<T>(&self, ptr: *const T) -> u32 {
        (ptr.addr() - self.base_ptr().addr()) as u32
    }

    fn offset_to_ptr<T>(&self, offset: u32) -> *const T {
        unsafe { self.base_ptr().add(offset as usize) as *const T }
    }

    fn pack_handle<T>(&self, ptr: *const T) -> AHandle<T> {
        let offset = self.ptr_to_offset(ptr);
        AHandle(offset, std::marker::PhantomData)
    }

    fn unpack_handle<T>(&self, handle: AHandle<T>) -> *const T {
        self.offset_to_ptr::<T>(handle.0)
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
        if ptr >= self.cursor.addr() {
            panic!("Address is beyond cursor: {} >= {}", ptr, self.cursor.addr());
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

    pub fn push_h<T: Copy>(&mut self, t: T) -> AHandle<T> {
        let t_ptr = self.push(t) as *const T;
        self.pack_handle(t_ptr)
    }

    pub fn new_vec<T>(&mut self, len: usize) -> AVec<T> {
        unsafe {
            let start_cursor = self.cursor_mut();
            let dst_aligned = start_cursor.byte_add(start_cursor.align_offset(align_of::<T>()));
            let layout = core::alloc::Layout::array::<T>(len).unwrap();

            let new_cursor = dst_aligned.byte_add(layout.size());
            self.set_cursor_checked(new_cursor);

            let raw_slice: *mut [T] =
                core::ptr::slice_from_raw_parts_mut(dst_aligned as *mut T, len);
            AVec { buf: raw_slice, len: 0 }
        }
    }

    pub fn push_slice<T: Copy>(&mut self, ts: &[T]) -> ASliceHandle<T> {
        unsafe {
            let dst = self.cursor_mut();
            let dst = dst.byte_add(dst.align_offset(align_of::<T>()));

            let new_cursor = dst.byte_add(size_of_val(ts));
            self.set_cursor_checked(new_cursor);

            let dst: &mut [T] = std::slice::from_raw_parts_mut(dst as *mut T, ts.len());
            dst.copy_from_slice(ts);
            ASliceHandle {
                offset: self.ptr_to_offset(dst.as_ptr()),
                count: ts.len() as u32,
                _marker: std::marker::PhantomData,
            }
        }
    }

    pub fn get<T>(&self, handle: AHandle<T>) -> &T {
        let ptr = self.unpack_handle(handle);
        if cfg!(feature = "dbg") {
            self.check_mine(ptr.addr())
        }
        unsafe { &*ptr }
    }

    pub fn get_copy<T: Copy>(&self, handle: AHandle<T>) -> T {
        let ptr = self.unpack_handle(handle);
        if cfg!(feature = "dbg") {
            self.check_mine(ptr.addr())
        }
        unsafe { *ptr }
    }

    pub fn get_slice<T>(&self, handle: ASliceHandle<T>) -> &'static [T] {
        fuckit! {
            let ptr: *const T = self.offset_to_ptr(handle.offset);
            if cfg!(debug_assertions) {
                self.check_mine(ptr.addr())
            }
            let src: &[T] = std::slice::from_raw_parts(ptr, handle.count as usize);
            src
        }
    }
}

pub struct AVec<T> {
    buf: *mut [T],
    len: usize,
}

impl<T> AVec<T> {
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test() {
        let mut arena = A::make();
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
        let mut arena = A::make();
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
        let mut arena = A::make();
        let mut v = arena.new_vec(4);
        for i in 0..5 {
            v.push(i * 10);
        }
    }
}
