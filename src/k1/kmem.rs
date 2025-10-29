use smallvec::SmallVec;

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
/// address less than 4GB in here and can use a 32-bit offset. Actually I can use
/// 48 bits for the offset and 16 for the length
macro_rules! fuckit {
    ($($t:tt)*) => {
        unsafe { $($t)* }
    };
}

use core::mem::{align_of, size_of};
use std::{marker::PhantomData, num::NonZeroU32, ops::Deref};
/// Use 'Tag' to meaningfully identify the arena to help
/// prevent mixups
pub struct Mem<Tag = ()> {
    mmap: memmap2::MmapMut,
    cursor: *const u8,
    _marker: PhantomData<Tag>,
}
pub type MemNoTag = Mem<()>;

// We use NonZeroU32 so that the handles are niched, allowing for use
// for no size cost in types like Option and Result
pub struct MHandle<T, Tag>(NonZeroU32, PhantomData<T>, PhantomData<Tag>);
static_assert_size!(MHandle<u128, ()>, 4);

impl<T, Tag> Copy for MHandle<T, Tag> {}
impl<T, Tag> Clone for MHandle<T, Tag> {
    fn clone(&self) -> Self {
        *self
    }
}

/// A handle to a slice of Ts inside a `Mem` pool with tag type `Tag`
/// me'slice <tips fedora>
pub struct MSlice<T, Tag = ()> {
    offset: NonZeroU32,
    count: u32,
    _data: std::marker::PhantomData<T>,
    _tag: std::marker::PhantomData<Tag>,
    // TODO: Add fingerprints to handles in dbg mode
    // #[cfg(feature = "dbg")]
    // fingerprint: u64,
}
static_assert_size!(MSlice<u128, ()>, 8);
impl<T, Tag> Copy for MSlice<T, Tag> {}
impl<T, Tag> Clone for MSlice<T, Tag> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T, Tag> MSlice<T, Tag> {
    pub const fn empty() -> Self {
        const BOGUS_OFFSET: NonZeroU32 = NonZeroU32::new(8).unwrap();
        // `offset` should never be touched when count is 0
        Self::make(BOGUS_OFFSET, 0)
    }

    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    pub fn len(&self) -> u32 {
        self.count
    }

    /// Skip this entry, resulting in a handle with a length
    /// decreased by n, and pointing n elements ahead of where it was.
    /// Returns an empty handle on an already empty handle
    pub fn skip(&self, n: usize) -> Self {
        if n == 0 {
            return *self;
        }
        let new_len = (self.count).saturating_sub(n as u32);
        if new_len == 0 {
            Self { offset: self.offset, count: 0, _data: PhantomData, _tag: PhantomData }
        } else {
            let new_offset = self.offset.checked_add(n as u32 * size_of::<T>() as u32).unwrap();
            Self { offset: new_offset, count: new_len, _data: PhantomData, _tag: PhantomData }
        }
    }

    const fn make(offset: NonZeroU32, count: u32) -> Self {
        Self { offset, count, _data: PhantomData, _tag: PhantomData }
    }
}

pub struct MStr<Tag>(pub MSlice<u8, Tag>);
impl<Tag> Copy for MStr<Tag> {}
impl<Tag> Clone for MStr<Tag> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<Tag> MStr<Tag> {
    pub fn empty() -> Self {
        Self(MSlice::empty())
    }

    pub fn len(&self) -> u32 {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

struct MemWriter<'a, Tag> {
    mem: &'a mut Mem<Tag>,
}
impl<'a, Tag> std::fmt::Write for MemWriter<'a, Tag> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        #[cfg(debug_assertions)]
        let start = self.mem.cursor;

        let bytes = self.mem.push_slice_raw(s.as_bytes());
        #[cfg(debug_assertions)]
        {
            let bytes_start = bytes.as_ptr();
            if start != bytes_start {
                // If we pushed any padding, fail. We
                // shouldn't be pushing any padding for a u8 slice
                panic!("Inserted padding in kmem::Mem fmt::Write::write_str")
            }
        };
        Ok(())
    }
}

impl Mem {
    fn make_untagged() -> Mem<()> {
        Mem::make()
    }
}

impl<Tag> Mem<Tag> {
    pub fn make() -> Self {
        // Note(knix) If we never allow larger than a 4gb allocation, then we are safe to hand out 32-bit
        //            offsets instead of pointers, which could be big for some codebases
        let mmap = memmap2::MmapMut::map_anon(crate::GIGABYTE).unwrap();
        mmap.advise(memmap2::Advice::Sequential).unwrap();

        // We waste the first 8 bytes every time, so that our handles can be niched
        // We could instead store offsets as (true offset) + 1, but I'd rather waste 8 bytes per
        // arena than convert on every single access
        let cursor = unsafe { mmap.as_ptr().byte_add(8) };

        Self { cursor, mmap, _marker: PhantomData }
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

    fn pack_handle<T>(&self, ptr: *const T) -> MHandle<T, Tag> {
        let offset = self.ptr_to_offset(ptr);
        MHandle(offset, PhantomData, PhantomData)
    }

    fn unpack_handle<T>(&self, handle: MHandle<T, Tag>) -> *const T {
        self.offset_to_ptr::<T>(handle.0)
    }

    pub fn vec_to_mslice<T>(&self, fix_vec: &FixVec<T, Tag>) -> MSlice<T, Tag> {
        let offset = self.ptr_to_offset(fix_vec.buf.cast_const());
        MSlice::make(offset, fix_vec.len() as u32)
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

    pub fn push<T>(&mut self, t: T) -> &mut T {
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

    pub fn push_h<T>(&mut self, t: T) -> MHandle<T, Tag> {
        let t_ptr = self.push(t) as *const T;
        self.pack_handle(t_ptr)
    }

    fn push_slice_uninit<T>(&mut self, len: usize) -> *mut T {
        unsafe {
            let dst = self.cursor_mut();
            let dst = dst.byte_add(dst.align_offset(align_of::<T>()));

            let array_layout = core::alloc::Layout::array::<T>(len).unwrap();
            let new_cursor = dst.byte_add(array_layout.size());
            self.set_cursor_checked(new_cursor);

            dst as *mut T
        }
    }

    fn push_slice_raw<T: Copy>(&mut self, ts: &[T]) -> &mut [T] {
        unsafe {
            let dst = self.push_slice_uninit::<T>(ts.len());

            let dst: &mut [T] = std::slice::from_raw_parts_mut(dst, ts.len());
            dst.copy_from_slice(ts);
            dst
        }
    }

    pub fn push_slice_iter<T: Clone>(&mut self, iter: impl Iterator<Item = T>) -> MSlice<T, Tag> {
        let mut count = 0;
        let mut first = None;
        for t in iter {
            let p = self.push(t);
            if count == 0 {
                first = Some(p as *const T);
            }
            count += 1;
        }
        if count == 0 {
            MSlice::empty()
        } else {
            MSlice::make(self.ptr_to_offset(first.unwrap()), count)
        }
    }

    pub fn push_slice<T: Copy>(&mut self, ts: &[T]) -> MSlice<T, Tag> {
        let slice = self.push_slice_raw(ts);
        let ptr = slice.as_ptr();
        MSlice::make(self.ptr_to_offset(ptr), ts.len() as u32)
    }

    pub fn dup_slice<T: Copy>(&mut self, h: MSlice<T, Tag>) -> MSlice<T, Tag> {
        let (ptr, count) = self.get_slice_raw(h);
        let slice = unsafe { core::slice::from_raw_parts(ptr, count) };
        self.push_slice(slice)
    }

    /// We know we can't address more than 4GB (to keep handles small), so we accept a u32 len, not a usize
    pub fn new_vec<T>(&mut self, len: u32) -> FixVec<T, Tag> {
        let dst = self.push_slice_uninit(len as usize);

        let raw_slice: *mut [T] = core::ptr::slice_from_raw_parts_mut(dst, len as usize);
        FixVec { buf: raw_slice, len: 0, _tag: PhantomData }
    }

    pub fn get_str(&self, s: MStr<Tag>) -> &str {
        let bytes = self.get_slice(s.0);
        unsafe { str::from_utf8_unchecked(bytes) }
    }

    pub fn push_str(&mut self, s: impl AsRef<str>) -> MStr<Tag> {
        let bytes = self.push_slice_raw(s.as_ref().as_bytes());
        let bytes_ptr = bytes.as_ptr();
        let len = bytes.len() as u32;
        MStr(MSlice::make(self.ptr_to_offset(bytes_ptr), len))
    }

    // Zero-allocation formatted strings.
    // Formats a string directly into our backing buffer, to avoid any extra allocations, then
    // bumps cursor based on final length
    pub fn format_str(&mut self, args: std::fmt::Arguments) -> MStr<Tag> {
        use std::fmt::Write;

        let base: *const u8 = self.cursor;
        let mut writer = MemWriter { mem: self };
        writer.write_fmt(args).unwrap();
        let len = unsafe { self.cursor.offset_from(base) };
        if len < 0 {
            panic!("Cursor moved backwards in kmem::Mem::format_str");
        }
        MStr(MSlice::make(self.ptr_to_offset(base), len as u32))
    }

    pub fn get<T>(&self, handle: MHandle<T, Tag>) -> &T {
        let ptr = self.unpack_handle(handle);
        if cfg!(feature = "dbg") {
            self.check_mine(ptr.addr())
        }
        unsafe { &*ptr }
    }

    pub fn get_copy<T: Copy>(&self, handle: MHandle<T, Tag>) -> T {
        let ptr = self.unpack_handle(handle);
        if cfg!(feature = "dbg") {
            self.check_mine(ptr.addr())
        }
        unsafe { *ptr }
    }

    pub fn get_slice_raw<T>(&self, handle: MSlice<T, Tag>) -> (*const T, usize) {
        let ptr: *const T = self.offset_to_ptr(handle.offset);
        if cfg!(debug_assertions) {
            self.check_mine(ptr.addr())
        }
        (ptr, handle.count as usize)
    }

    fn get_slice_lt<T>(&self, handle: MSlice<T, Tag>) -> &'_ [T] {
        let (ptr, count) = self.get_slice_raw(handle);
        fuckit! {
            let src: &[T] = std::slice::from_raw_parts(ptr, count);
            src
        }
    }

    pub fn get_slice<T>(&self, handle: MSlice<T, Tag>) -> &'static [T] {
        let (ptr, count) = self.get_slice_raw(handle);
        fuckit! {
            let src: &[T] = std::slice::from_raw_parts(ptr, count);
            src
        }
    }

    pub fn get_slice_sv<T: Copy, const N: usize>(&self, handle: MSlice<T, Tag>) -> SmallVec<[T; N]>
    where
        [T; N]: smallvec::Array<Item = T>,
    {
        let (ptr, count) = self.get_slice_raw(handle);
        let slice = unsafe { core::slice::from_raw_parts(ptr, count) };
        SmallVec::from_slice(slice)
    }

    pub fn get_slice_sv4<T: Copy>(&self, handle: MSlice<T, Tag>) -> SmallVec<[T; 4]> {
        self.get_slice_sv(handle)
    }

    pub fn get_slice_sv8<T: Copy>(&self, handle: MSlice<T, Tag>) -> SmallVec<[T; 8]> {
        self.get_slice_sv(handle)
    }

    pub fn get_slice_mut<T>(&self, handle: MSlice<T, Tag>) -> &'static mut [T] {
        let (ptr, count) = self.get_slice_raw(handle);
        fuckit! {
            let src: &mut [T] = std::slice::from_raw_parts_mut(ptr.cast_mut(), count);
            src
        }
    }

    pub fn get_nth<T>(&self, handle: MSlice<T, Tag>, n: usize) -> &'static T {
        &self.get_slice(handle)[n]
    }

    pub fn get_nth_opt<T>(&self, handle: MSlice<T, Tag>, n: usize) -> Option<&'static T> {
        self.get_slice(handle).get(n)
    }

    pub fn slices_equal_copy<T: Copy + PartialEq + Eq>(
        &self,
        h1: MSlice<T, Tag>,
        h2: MSlice<T, Tag>,
    ) -> bool {
        if h1.len() != h2.len() {
            return false;
        }
        let slice1 = self.get_slice_lt(h1);
        let slice2 = self.get_slice_lt(h2);
        slice1 == slice2
    }

    pub fn bytes_used(&self) -> usize {
        self.cursor.addr() - self.base_ptr().addr()
    }
}

#[macro_export]
macro_rules! mformat {
    ($mem:expr, $($arg:tt)*) => {
        $mem.format_str(format_args!($($arg)*))
    };
}

impl<Tag> Mem<Tag> {
    pub fn print_usage(&self, name: &str) {
        let used_kb = self.bytes_used() / crate::KILOBYTE;
        let total = self.mmap.len() / crate::KILOBYTE;
        let percent = (used_kb as f64) / (total as f64) * 100.0;
        println!("{name} usage: {used_kb}/{total}kb ({percent:.2}%)");
    }
}

/// A fixed-size Vec-like collection pointing into a Mem's data
pub struct FixVec<T, Tag = ()> {
    buf: *mut [T],
    len: usize,
    _tag: PhantomData<Tag>,
}

impl<T, Tag> FixVec<T, Tag> {
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn push(&mut self, val: T) {
        if self.len == self.buf.len() {
            panic!("FixVec is full {}", self.buf.len());
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

    // Inserts `val` at index `index`, shifting all elements to the right as needed
    pub fn insert(&mut self, index: usize, val: T)
    where
        T: Copy,
    {
        if self.len == self.buf.len() {
            panic!("FixVec is full {}", self.buf.len());
        }
        if index > self.len {
            panic!("FixVec insert index out of bounds: {} > {}", index, self.len);
        }
        unsafe {
            let slice = &mut *self.buf;
            slice.copy_within(index..self.len, index + 1);
            slice[index] = val;
        }
        self.len += 1;
    }

    pub fn extend(&mut self, vals: &[T])
    where
        T: Copy,
    {
        if self.len + vals.len() > self.buf.len() {
            panic!("FixVec is full {} + {} > {}", self.len, vals.len(), self.buf.len());
        }
        unsafe {
            let dst = &mut (*self.buf)[self.len..self.len + vals.len()];
            dst.copy_from_slice(vals);
        }
        self.len += vals.len();
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

impl<T, Tag> std::ops::Index<usize> for FixVec<T, Tag> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        &self.as_slice()[index]
    }
}

impl<T, Tag> std::ops::IndexMut<usize> for FixVec<T, Tag> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.as_slice_mut()[index]
    }
}

impl<T, Tag> Deref for FixVec<T, Tag> {
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
        let mut arena = Mem::make_untagged();
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
    fn push_slice_iter() {
        let mut arena = Mem::make_untagged();
        let h = arena.push_slice_iter((0..10).map(|x| x * 10));
        assert_eq!(h.len(), 10);
        assert_eq!(arena.get_slice(h), &[0, 10, 20, 30, 40, 50, 60, 70, 80, 90]);

        let empty_h = arena.push_slice_iter(std::iter::empty::<u32>());
        assert_eq!(empty_h.len(), 0);
        assert_eq!(arena.get_slice(empty_h), &[]);
    }

    #[test]
    fn vec() {
        let mut arena = Mem::make_untagged();
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
    fn vec_extend() {
        let mut arena = Mem::make_untagged();
        let mut v = arena.new_vec(16);
        v.extend(&[1, 2, 3, 4, 5]);
        assert_eq!(v.len(), 5);
        for i in 0..5 {
            assert_eq!(v.as_slice()[i], i + 1);
        }

        v.extend(&[6]);
        assert_eq!(v.len(), 6);
    }

    #[test]
    #[should_panic(expected = "FixVec is full")]
    fn vec_extend_oob() {
        let mut arena = Mem::make_untagged();
        let mut v = arena.new_vec(3);
        v.extend(&[1, 2, 3, 4, 5]);
    }

    #[test]
    #[should_panic(expected = "FixVec is full")]
    fn vec_oob() {
        let mut arena = Mem::make_untagged();
        let mut v = arena.new_vec(4);
        for i in 0..5 {
            v.push(i * 10);
        }
    }

    #[test]
    fn dup_slice() {
        let mut arena = Mem::make_untagged();
        let h = arena.push_slice(&[1, 2, 3, 4, 5]);
        let h2 = arena.dup_slice(h);
        assert_eq!(h.len(), h2.len());
        assert_eq!(arena.get_slice(h), arena.get_slice(h2));
        assert_ne!(h.offset, h2.offset);
    }

    #[test]
    fn insert() {
        let mut arena = Mem::make_untagged();
        let mut v = arena.new_vec(5);
        v.push(1);
        v.push(2);
        v.push(4);
        v.insert(2, 3);
        assert_eq!(v.len(), 4);
        assert_eq!(v.as_slice(), &[1, 2, 3, 4]);
    }
}
