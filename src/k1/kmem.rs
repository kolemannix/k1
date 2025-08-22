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
use core::mem::{align_of, size_of};
pub struct A {
    mmap: memmap2::MmapMut,
    cursor: *const u8,
}

static_assert_size!(AHandle<u128>, 4);
pub struct AHandle<T>(u32, std::marker::PhantomData<T>);

static_assert_size!(ASliceHandle<u128>, 8);
pub struct ASliceHandle<T> {
    base_offset: u32,
    count: u32,
    _marker: std::marker::PhantomData<T>,
}

impl<T> ASliceHandle<T> {
    pub fn len(&self) -> u32 {
        self.count
    }
}

impl A {
    fn make() -> Self {
        // Note(knix) If we never allow larger than a 4gb allocation, then we are safe to hand out 32-bit
        //            offsets instead of pointers, which could be big for some codebases
        let mmap = memmap2::MmapMut::map_anon(1 * crate::GIGABYTE).unwrap();
        mmap.advise(memmap2::Advice::Sequential);
        Self { cursor: mmap.as_ptr(), mmap }
    }
    fn cursor_mut(&mut self) -> *mut u8 {
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
        if ptr <= self.base_ptr().addr() || ptr >= self.cursor.addr() {
            panic!("Address out of bounds: {}", ptr,);
        }
    }

    #[track_caller]
    #[inline]
    fn set_cursor_checked(&mut self, proposed_cursor: *const u8) {
        self.check_in_bounds(proposed_cursor.addr());
        self.cursor = proposed_cursor;
    }

    pub fn push<T: Copy>(&mut self, t: T) -> *mut T {
        unsafe {
            let dst = self.cursor_mut();
            let dst = dst.byte_add(dst.align_offset(align_of::<T>()));

            let new_cursor = dst.byte_add(size_of::<T>());
            self.set_cursor_checked(new_cursor);

            let dst = dst as *mut T;
            core::ptr::write(dst, t);

            // std::mem::transmute::<&T, &'static T>(&*dst)
            dst
        }
    }

    pub fn push_h<T: Copy>(&mut self, t: T) -> AHandle<T> {
        let t_ptr = self.push(t);
        self.pack_handle(t_ptr)
    }

    pub fn push_slice<T: Copy>(&mut self, ts: &[T]) -> ASliceHandle<T> {
        unsafe {
            let dst = self.cursor_mut();
            let dst = dst.byte_add(dst.align_offset(align_of::<T>()));

            let new_cursor = dst.byte_add(size_of_val(ts));
            self.set_cursor_checked(new_cursor);

            let dst = std::slice::from_raw_parts_mut(dst as *mut T, ts.len());
            dst.copy_from_slice(ts);
            ASliceHandle {
                base_offset: self.ptr_to_offset(dst.as_ptr()),
                count: ts.len() as u32,
                _marker: std::marker::PhantomData,
            }
        }
    }

    pub fn get<T>(&self, handle: AHandle<T>) -> &T {
        let ptr = self.unpack_handle(handle);
        if cfg!(debug_assertions) {
            self.check_mine(ptr.addr())
        }
        // nocommit: Should I check if ptr is between our base and cursor?
        unsafe { &*ptr }
    }

    pub fn get_slice<T>(&self, handle: ASliceHandle<T>) -> &[T] {
        todo!()
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
    }
}
