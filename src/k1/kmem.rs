// Copyright (c) 2025 knix
// All rights reserved.
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
/// all their issues; we should be able to go from 24 bytes to 8 per collection
/// if our slice handle is 8 bytes, which it should be, if I'm willing to
/// address less than 4GB in here and can use a 32-bit offset. Actually I can use
/// 48 bits for the offset and 16 for the length
use smallvec::SmallVec;

use crate::{static_assert_size, typer::dump::DepDisplay};
macro_rules! fuckit {
    ($($t:tt)*) => {
        unsafe { $($t)* }
    };
}

use core::{
    fmt,
    mem::{MaybeUninit, align_of, size_of},
    slice,
};
use std::{
    marker::PhantomData,
    mem::ManuallyDrop,
    num::NonZeroU32,
    ops::{Deref, DerefMut},
};
/// Use 'Tag' to meaningfully identify the arena to help
/// prevent mixups
pub struct Mem<Tag = ()> {
    mmap: memmap2::MmapMut,
    cursor: *const u8,
    _marker: PhantomData<Tag>,
}

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

    #[cfg(test)]
    pub fn forged(offset: u32, count: u32) -> Self {
        let offset = NonZeroU32::new(offset).expect("Cannot forge MSlice with zero offset");
        Self::make(offset, count)
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
            let byte_advance = (n * size_of::<T>()) as u32;
            let new_offset = self.offset.checked_add(byte_advance).unwrap();
            Self { offset: new_offset, count: new_len, _data: PhantomData, _tag: PhantomData }
        }
    }

    const fn make(offset: NonZeroU32, count: u32) -> Self {
        Self { offset, count, _data: PhantomData, _tag: PhantomData }
    }
}

pub struct MStr<Tag> {
    bytes: *const [u8],
    _tag: PhantomData<Tag>,
}
impl<Tag> Copy for MStr<Tag> {}
impl<Tag> Clone for MStr<Tag> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<Tag> MStr<Tag> {
    pub fn from_static(s: &'static str) -> Self {
        Self { bytes: s.as_bytes(), _tag: PhantomData }
    }

    pub fn empty() -> Self {
        Self { bytes: "".as_bytes(), _tag: PhantomData }
    }

    fn make(bytes: *const [u8]) -> Self {
        Self { bytes, _tag: PhantomData }
    }

    fn from_parts(data: *const u8, len: usize) -> Self {
        let bytes = unsafe { slice::from_raw_parts(data, len) };
        Self { bytes, _tag: PhantomData }
    }

    pub fn len(&self) -> u32 {
        self.bytes.len() as u32
    }

    pub fn as_str(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(&*self.bytes) }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<Tag> AsRef<str> for MStr<Tag> {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl<Tag> std::fmt::Display for MStr<Tag> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = self.as_str();
        write!(f, "{}", s)
    }
}

impl<AnyTag> From<&'static str> for MStr<AnyTag> {
    fn from(s: &'static str) -> Self {
        MStr::<AnyTag>::from_static(s)
    }
}

/// We avoid the borrow checker here in MemWriter
/// since we sometimes need access to the `Mem` and to other data
/// at the same time; see `Mem::format_all`
pub struct MemWriter<Tag> {
    mem: *mut Mem<Tag>,
}
impl<Tag> std::fmt::Write for MemWriter<Tag> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        let mem = unsafe { &mut *self.mem };

        #[cfg(debug_assertions)]
        let start = mem.cursor;

        #[cfg_attr(not(debug_assertions), allow(unused_variables))]
        let bytes = mem.pushn_raw(s.as_bytes());

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

    pub fn reset(&mut self, zero: bool) {
        if zero {
            let used = self.bytes_used();
            unsafe {
                core::ptr::write_bytes(self.mmap.as_mut_ptr(), 0, used);
            }
        }
        self.cursor = unsafe { self.mmap.as_ptr().byte_add(8) };
    }

    /// Sends an advise_range call, effectively faulting in
    /// all the pages you expect to use
    pub fn will_need(&mut self, byte_count: usize) {
        self.mmap.advise_range(memmap2::Advice::WillNeed, 0, byte_count).unwrap();
    }

    pub fn cursor(&self) -> *const u8 {
        self.cursor
    }

    pub fn cursor_mut(&self) -> *mut u8 {
        self.cursor.cast_mut()
    }

    pub fn set_cursor(&mut self, new_cursor: *const u8) {
        self.set_cursor_checked(new_cursor);
    }

    pub fn base_ptr(&self) -> *const u8 {
        self.mmap.as_ptr()
    }

    pub fn end_ptr(&self) -> *const u8 {
        self.mmap.as_ptr_range().end
    }

    fn ptr_to_offset<T: ?Sized>(&self, ptr: *const T) -> NonZeroU32 {
        let base = self.base_ptr().addr();
        let p = ptr.addr();
        let diff = p - base;
        if diff > u32::MAX as usize {
            panic!("Offset exceeds 32-bit handle")
        }
        if diff == 0 {
            panic!("Attempted to use index 0 (we reserve 0..8)")
        }
        NonZeroU32::new(diff as u32).unwrap()
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

    pub fn list_to_handle<T>(&self, list: MList<T, Tag>) -> MSlice<T, Tag> {
        let offset = self.ptr_to_offset(list.buf.cast_const());
        MSlice::make(offset, list.len() as u32)
    }

    #[track_caller]
    #[inline]
    fn check_in_bounds(&self, ptr: usize) {
        if ptr < self.base_ptr().addr() + 8 {
            panic!("Address before start+8: {}", ptr);
        }
        if ptr >= self.end_ptr().addr() {
            panic!("Address beyond end: {}", ptr);
        }
    }

    #[track_caller]
    #[inline]
    fn check_mine(&self, ptr: usize) {
        if ptr < self.base_ptr().addr() {
            panic!("Address not mine (less than base): {} < {}", ptr, self.base_ptr().addr());
        }
        if ptr >= self.cursor.addr() {
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

    pub fn align_to_bytes(&mut self, align: usize) {
        unsafe {
            debug_assert!(
                align.is_power_of_two() && align != 0,
                "Alignment must be a power of two"
            );
            let dst = self.cursor_mut();
            let dst = dst.byte_add(dst.align_offset(align));
            self.set_cursor_checked(dst);
        }
    }

    pub fn push_layout_uninit(&mut self, size: u32, align: u32) -> *mut u8 {
        unsafe {
            debug_assert!(
                align.is_power_of_two() && align != 0,
                "Alignment must be a power of two"
            );
            let dst = self.cursor_mut();
            let dst = dst.byte_add(dst.align_offset(align as usize));

            let new_cursor = dst.byte_add(size as usize);
            self.set_cursor_checked(new_cursor);

            dst
        }
    }

    pub fn push_slice_uninit<T>(&mut self, len: usize) -> *mut T {
        unsafe {
            let dst = self.cursor_mut();
            let dst = dst.byte_add(dst.align_offset(align_of::<T>()));

            let array_layout = core::alloc::Layout::array::<T>(len).unwrap();
            let new_cursor = dst.byte_add(array_layout.size());
            self.set_cursor_checked(new_cursor);

            dst as *mut T
        }
    }

    fn pushn_raw<T: Copy>(&mut self, ts: &[T]) -> &mut [T] {
        unsafe {
            let dst = self.push_slice_uninit::<T>(ts.len());

            let dst: &mut [T] = slice::from_raw_parts_mut(dst, ts.len());
            dst.copy_from_slice(ts);
            dst
        }
    }

    pub fn pushn_iter<T: Clone>(&mut self, iter: impl Iterator<Item = T>) -> MSlice<T, Tag> {
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

    pub fn pushn<T: Copy>(&mut self, ts: &[T]) -> MSlice<T, Tag> {
        let slice = self.pushn_raw(ts);
        let ptr = slice.as_ptr();
        MSlice::make(self.ptr_to_offset(ptr), ts.len() as u32)
    }

    pub fn dupn<T: Copy>(&mut self, h: MSlice<T, Tag>) -> MSlice<T, Tag> {
        let (ptr, count) = self.get_slice_raw(h);
        let slice = unsafe { slice::from_raw_parts(ptr, count) };
        self.pushn(slice)
    }

    /// We know we can't address more than 4GB (to keep handles small), so we accept a u32 len, not a usize
    pub fn new_list<T>(&mut self, len: u32) -> MList<T, Tag> {
        let dst = self.push_slice_uninit(len as usize);

        let raw_slice: *mut [T] = core::ptr::slice_from_raw_parts_mut(dst, len as usize);
        MList { buf: raw_slice, len: 0, _tag: PhantomData }
    }

    pub fn push_str(&mut self, s: impl AsRef<str>) -> MStr<Tag> {
        let bytes = self.pushn_raw(s.as_ref().as_bytes());
        MStr::make(bytes)
    }

    pub fn push_str_raw(&mut self, ptr: *const u8, len: usize) -> MStr<Tag> {
        let s = str::from_utf8(unsafe { slice::from_raw_parts(ptr, len) }).unwrap();
        self.push_str(s)
    }

    // Zero-allocation formatted strings.
    // Formats a string directly into our backing buffer, then
    // bump cursor based on final length
    pub fn format_str(&mut self, args: std::fmt::Arguments) -> MStr<Tag> {
        use std::fmt::Write;

        let base: *const u8 = self.cursor;
        let mut writer = MemWriter { mem: self };
        writer.write_fmt(args).unwrap();
        let len = unsafe { self.cursor.offset_from(base) };
        if len < 0 {
            panic!("Cursor moved backwards in kmem::Mem::format_str");
        }
        MStr::from_parts(base, len as usize)
    }

    pub fn format_all<D, A>(
        &mut self,
        dep: &D,
        args: &A,
        stuff: &[&dyn DepDisplay<D, A>],
    ) -> MStr<Tag> {
        let base: *const u8 = self.cursor;
        let mut writer = MemWriter { mem: self };
        for s in stuff.iter() {
            DepDisplay::fmt(*s, &mut writer, dep, args).unwrap();
        }
        let len = unsafe { self.cursor.offset_from(base) };
        if len < 0 {
            panic!("Cursor moved backwards in kmem::Mem::format_str");
        }
        MStr::from_parts(base, len as usize)
    }

    pub fn format_with<D, A, F>(&mut self, dep: &D, args: &A, f: F) -> MStr<Tag>
    where
        F: FnOnce(&mut MemWriter<Tag>, &D, &A) -> fmt::Result,
    {
        let base: *const u8 = self.cursor;
        let mut writer = MemWriter { mem: self };

        f(&mut writer, dep, args).unwrap();

        let len = unsafe { self.cursor.offset_from(base) };
        if len < 0 {
            panic!("Cursor moved backwards in Mem::format_with");
        }
        MStr::from_parts(base, len as usize)
    }

    pub fn get<T>(&self, handle: MHandle<T, Tag>) -> &T {
        let ptr = self.unpack_handle(handle);
        if cfg!(feature = "dbg") {
            if size_of::<T>() != 0 {
                self.check_mine(ptr.addr())
            }
        }
        unsafe { &*ptr }
    }

    pub fn get_slice_raw<T>(&self, handle: MSlice<T, Tag>) -> (*const T, usize) {
        let ptr: *const T = self.offset_to_ptr(handle.offset);
        if cfg!(debug_assertions) {
            if handle.count != 0 {
                self.check_mine(ptr.addr())
            }
        }
        (ptr, handle.count as usize)
    }

    pub fn getn_lt<T>(&self, handle: MSlice<T, Tag>) -> &[T] {
        let (ptr, count) = self.get_slice_raw(handle);
        fuckit! {
            let src: &[T] = slice::from_raw_parts(ptr, count);
            src
        }
    }

    pub fn getn<T>(&self, handle: MSlice<T, Tag>) -> &'static [T] {
        let (ptr, count) = self.get_slice_raw(handle);
        fuckit! {
            let src: &[T] = slice::from_raw_parts(ptr, count);
            src
        }
    }

    pub fn iter<T>(&self, handle: MSlice<T, Tag>) -> slice::Iter<'_, T> {
        self.getn_lt(handle).iter()
    }

    pub fn iter_with_is_last<'a, T: 'a>(
        &'a self,
        handle: MSlice<T, Tag>,
    ) -> impl Iterator<Item = (bool, &'a T)> + 'a {
        self.getn_lt(handle).iter().enumerate().map(move |(index, i)| {
            let is_last = index + 1 == handle.len() as usize;
            (is_last, i)
        })
    }

    pub fn getn_sv<T: Copy, const N: usize>(&self, handle: MSlice<T, Tag>) -> SmallVec<[T; N]>
    where
        [T; N]: smallvec::Array<Item = T>,
    {
        let (ptr, count) = self.get_slice_raw(handle);
        let slice = unsafe { slice::from_raw_parts(ptr, count) };
        SmallVec::from_slice(slice)
    }

    pub fn getn_sv4<T: Copy>(&self, handle: MSlice<T, Tag>) -> SmallVec<[T; 4]> {
        self.getn_sv(handle)
    }

    pub fn getn_sv8<T: Copy>(&self, handle: MSlice<T, Tag>) -> SmallVec<[T; 8]> {
        self.getn_sv(handle)
    }

    pub fn getn_mut<T>(&mut self, handle: MSlice<T, Tag>) -> &'static mut [T] {
        let (ptr, count) = self.get_slice_raw(handle);
        fuckit! {
            let src: &mut [T] = slice::from_raw_parts_mut(ptr.cast_mut(), count);
            src
        }
    }

    /// `lt` stands for lifetime; this one takes any lifetime vs the one that fixes to static
    pub fn get_nth_lt<T>(&self, handle: MSlice<T, Tag>, n: usize) -> &'_ T {
        &self.getn_lt(handle)[n]
    }

    pub fn get_nth<T>(&self, handle: MSlice<T, Tag>, n: usize) -> &'static T {
        &self.getn(handle)[n]
    }

    pub fn get_nth_opt<T>(&self, handle: MSlice<T, Tag>, n: usize) -> Option<&'static T> {
        self.getn(handle).get(n)
    }

    pub fn get_last_opt<T>(&self, handle: MSlice<T, Tag>) -> Option<&'static T> {
        let slice = self.getn(handle);
        slice.last()
    }

    pub fn slices_equal_copy<T: Copy + PartialEq + Eq>(
        &self,
        h1: MSlice<T, Tag>,
        h2: MSlice<T, Tag>,
    ) -> bool {
        if h1.len() != h2.len() {
            return false;
        }
        let slice1 = self.getn_lt(h1);
        let slice2 = self.getn_lt(h2);
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

#[macro_export]
macro_rules! k1_format_user {
    ($k1:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {{
        k1_format!($k1, &K1DisplayArgs::user_facing(), $fmt, $($arg),*)
    }};
}

#[macro_export]
macro_rules! kerr {
    ($k1:expr, $span:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {{
        let msg: String = k1_format_user!($k1, $fmt, $($arg),*).to_string();
        TyperError {
            span: $span,
            message: msg,
            level: MessageLevel::Error
        }
    }}
}

#[macro_export]
macro_rules! kbail {
    ($k1:expr, $span:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {{
        return Err($crate::kerr!($k1, $span, $fmt, $($arg),*))
    }}
}

#[macro_export]
macro_rules! k1_format {
    ($k1:expr, $args:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {{
        #[allow(unused)]
        $k1.get_tmp_unsafe().format_with($k1, $args, |w, dep, args| {
            use core::fmt::Write as _;
            write!(w, $fmt $(, $crate::typer::dump::depfmt(dep, args, &$arg))* )
        })
    }};
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
/// Can behave like an auto-growing list if the _grow variants are used
pub struct MList<T, Tag = ()> {
    buf: *mut [T],
    len: usize,
    _tag: PhantomData<Tag>,
}

impl<T, Tag> MList<T, Tag> {
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn cap(&self) -> usize {
        self.buf.len()
    }

    fn push_unchecked(&mut self, val: T) {
        unsafe {
            (*self.buf)[self.len] = val;
        }
        self.len += 1;
    }

    pub fn push(&mut self, val: T) {
        if self.len == self.cap() {
            panic!("MList is full {}", self.buf.len());
        }
        self.push_unchecked(val)
    }

    pub fn try_push(&mut self, val: T) -> Result<(), T> {
        if self.len == self.cap() {
            Err(val)
        } else {
            self.push_unchecked(val);
            Ok(())
        }
    }

    fn grow(&mut self, mem: &mut Mem<Tag>) -> Self
    where
        T: Copy,
    {
        let loc = std::panic::Location::caller();
        // Growth doesnt invalidate the old pointers
        let new_cap = if self.len == 0 {
            let size_of_t = size_of::<T>();
            let initial_cap = if size_of_t >= 1024 { 1 } else { 8 };
            initial_cap
        } else {
            let new_cap_usize = self.cap() * 2;
            #[cfg(debug_assertions)]
            if new_cap_usize > u32::MAX as usize {
                panic!("MList capacity exceeds 32-bit handle limit: {}", new_cap_usize);
            }
            new_cap_usize as u32
        };
        if cfg!(debug_assertions) {
            // No need to log grows from 0
            if self.len != 0 {
                eprintln!(
                    "{}:{} Growing from {} -> {}",
                    loc.file(),
                    loc.line(),
                    self.cap(),
                    new_cap
                );
            }
        }
        let mut new_me = mem.new_list(new_cap);
        new_me.extend(self.as_slice());
        new_me
    }

    // This doesn't have to be the same arena, actually.
    // For now we'll constrain the tag, but we should provide
    // a separate method that returns a new list in whatever
    // arena is passed
    #[track_caller]
    pub fn push_grow(&mut self, mem: &mut Mem<Tag>, val: T)
    where
        T: Copy,
    {
        // FIXME: arena, special case for growth when this list is the last thing in the arena,
        // which it quite often will be
        if self.len >= self.cap() {
            *self = self.grow(mem);
        }
        self.push_unchecked(val)
    }

    // Inserts `val` at index `index`, shifting all elements to the right as needed
    pub fn insert(&mut self, index: usize, val: T)
    where
        T: Copy,
    {
        if index > self.len {
            panic!("MList insert index out of bounds: {} > {}", index, self.len);
        }
        if self.len == self.cap() {
            panic!("MList.insert is full {}", self.buf.len());
        }
        unsafe {
            let slice = &mut *self.buf;
            slice.copy_within(index..self.len, index + 1);
            slice[index] = val;
        }
        self.len += 1;
    }

    // Inserts `val` at index `index`, shifting all elements to the right as needed
    // And growing the allocation if needed
    pub fn insert_grow(&mut self, mem: &mut Mem<Tag>, index: usize, val: T)
    where
        T: Copy,
    {
        if index > self.len {
            panic!("MList insert index out of bounds: {} > {}", index, self.len);
        }
        if self.len >= self.cap() {
            *self = self.grow(mem)
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
            panic!("MList is full {} + {} > {}", self.len, vals.len(), self.buf.len());
        }
        unsafe {
            let dst = &mut (&mut (*self.buf))[self.len..self.len + vals.len()];
            dst.copy_from_slice(vals);
        }
        self.len += vals.len();
    }

    pub fn extend_iter<I>(&mut self, vals: I)
    where
        T: Copy,
        I: Iterator<Item = T>,
    {
        for item in vals {
            self.push(item);
        }
    }
    pub fn as_slice(&self) -> &[T] {
        unsafe { &(&(*self.buf))[..self.len] }
    }

    pub fn as_slice_mut(&mut self) -> &mut [T] {
        unsafe { &mut (&mut (*self.buf))[..self.len] }
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.as_slice().iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.as_slice_mut().iter_mut()
    }

    pub fn into_handle(self, mem: &mut Mem<Tag>) -> MSlice<T, Tag> {
        mem.list_to_handle(self)
    }
}

impl<T, Tag> std::ops::Index<usize> for MList<T, Tag> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        &self.as_slice()[index]
    }
}

impl<T, Tag> std::ops::Index<std::ops::Range<usize>> for MList<T, Tag> {
    type Output = [T];
    fn index(&self, index: std::ops::Range<usize>) -> &Self::Output {
        &self.as_slice()[index]
    }
}

impl<T, Tag> std::ops::Index<std::ops::RangeInclusive<usize>> for MList<T, Tag> {
    type Output = [T];
    fn index(&self, index: std::ops::RangeInclusive<usize>) -> &Self::Output {
        &self.as_slice()[index]
    }
}

impl<T, Tag> std::ops::IndexMut<usize> for MList<T, Tag> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.as_slice_mut()[index]
    }
}

impl<T, Tag> std::ops::IndexMut<std::ops::Range<usize>> for MList<T, Tag> {
    fn index_mut(&mut self, index: std::ops::Range<usize>) -> &mut Self::Output {
        &mut self.as_slice_mut()[index]
    }
}

impl<T, Tag> Deref for MList<T, Tag> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<T, Tag> DerefMut for MList<T, Tag> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_slice_mut()
    }
}

////////////////////////////// MSpillList //////////////////////////////
/// Top bit of `len_flags` indicates whether we've spilled to the arena.
/// Low 31 bits are the logical length.
const SPILLED_BIT: u32 = 0x8000_0000;
const LEN_MASK: u32 = 0x7fff_ffff;

/// A SmallVec-like pushable list that stores up to `N` items inline,
/// then spills into `Mem<Tag>` as an `MList<T, Tag>` on overflow.
///
/// - `len_flags`: low 31 bits = len, top bit = spilled
/// - MSpillListStorage is a union of inline buffer vs spilled `MList`.
/// - No Drop glue required; `T: Copy` and `MList` doesn't own heap memory.
///
/// *Clanker helped with this one, hence the SAFETY annotations
pub struct MSpillList<T: Copy, const N: usize, Tag> {
    len_flags: u32,
    storage: MSpillListStorage<T, Tag, N>,
}
pub type MSL2<T, Tag = ()> = MSpillList<T, 2, Tag>;
pub type MSL4<T, Tag = ()> = MSpillList<T, 4, Tag>;
pub type MSL8<T, Tag = ()> = MSpillList<T, 8, Tag>;

static_assert_size!(MSL8<i32>, 4 + 4 + (8 * 4));

union MSpillListStorage<T: Copy, Tag, const N: usize> {
    inline: [MaybeUninit<T>; N],
    spill: ManuallyDrop<MList<T, Tag>>,
}

impl<T: Copy, Tag, const N: usize> MSpillList<T, N, Tag> {
    #[inline]
    pub fn new() -> Self {
        assert!(N > 0);

        // SAFETY: MaybeUninit<T> array does not require initialization.
        let inline = unsafe { MaybeUninit::<[MaybeUninit<T>; N]>::uninit().assume_init() };
        Self { len_flags: 0, storage: MSpillListStorage { inline } }
    }

    #[inline]
    pub fn is_spilled(&self) -> bool {
        (self.len_flags & SPILLED_BIT) != 0
    }

    #[inline]
    pub fn len(&self) -> u32 {
        self.len_flags & LEN_MASK
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    fn set_len(&mut self, new_len: u32) {
        debug_assert_eq!(new_len & SPILLED_BIT, 0);
        self.len_flags = (self.len_flags & SPILLED_BIT) | new_len;
    }

    #[inline]
    fn mark_spilled(&mut self) {
        self.len_flags |= SPILLED_BIT;
    }

    /// Pushes into inline storage if possible, spills into `mem` otherwise.
    #[inline]
    pub fn push(&mut self, mem: &mut Mem<Tag>, val: T) {
        let len = self.len() as usize;
        debug_assert!((len as u32) < LEN_MASK, "MSpillList length exceeds 31-bit limit");

        if !self.is_spilled() {
            if len < N {
                // SAFETY: inline variant is active when not spilled.
                unsafe { self.storage.inline[len].write(val) };
                self.set_len((len as u32) + 1);
                return;
            }
            self.spill(mem);
        }

        debug_assert!(self.is_spilled());

        unsafe {
            let list: &mut MList<T, Tag> = &mut self.storage.spill;
            list.push_grow(mem, val);
        }
        self.set_len((len as u32) + 1);
    }

    #[inline]
    pub fn as_slice<'a>(&'a self, _mem: &'a Mem<Tag>) -> &'a [T] {
        let len = self.len() as usize;

        if self.is_spilled() {
            // SAFETY: spilled variant is active when spilled bit is set.
            unsafe {
                let list: &MList<T, Tag> = &self.storage.spill;
                list.as_slice()
            }
        } else {
            // SAFETY: inline variant active; first `len` elements were written.
            unsafe {
                let ptr = self.storage.inline.as_ptr() as *const T;
                slice::from_raw_parts(ptr, len)
            }
        }
    }

    #[inline]
    pub fn iter<'a>(&'a self, mem: &'a Mem<Tag>) -> slice::Iter<'a, T> {
        self.as_slice(mem).iter()
    }

    #[inline]
    fn spill(&mut self, mem: &mut Mem<Tag>) {
        debug_assert!(!self.is_spilled());
        let len = self.len() as usize;

        let cap = {
            let size = core::mem::size_of::<T>();
            if size >= 1024 { len.max(1) } else { len.max(8) }
        };

        let mut list = mem.new_list::<T>(cap as u32);

        // Move/copy inline data into list.
        for i in 0..len {
            // SAFETY: inline active; 0..len initialized.
            let v = unsafe { self.storage.inline[i].assume_init() };
            list.push(v);
        }

        // Overwrite union with spilled list and set flag.
        self.storage = MSpillListStorage { spill: ManuallyDrop::new(list) };
        self.mark_spilled();
    }

    /// Convert to an arena-backed immutable handle.
    ///
    /// - If spilled: reuses spilled buffer (no copy).
    /// - If inline: allocates exactly `len` elements in the arena and copies.
    #[inline]
    pub fn into_slice(self, mem: &mut Mem<Tag>) -> MSlice<T, Tag> {
        let len = self.len() as usize;

        if self.is_spilled() {
            // SAFETY: spilled variant active. We consume self, so we can move the list out.
            let list: MList<T, Tag> = unsafe { ManuallyDrop::into_inner(self.storage.spill) };
            mem.list_to_handle(list)
        } else {
            if len == 0 {
                return MSlice::empty();
            }
            // SAFETY: inline active; first `len` initialized.
            let slice: &[T] = unsafe {
                let ptr = self.storage.inline.as_ptr() as *const T;
                slice::from_raw_parts(ptr, len)
            };
            mem.pushn(slice)
        }
    }

    pub fn into_spill_slice(self, mem: &mut Mem<Tag>) -> MSpillSlice<T, N, Tag> {
        let len = self.len();
        if self.is_spilled() {
            let mslice =
                unsafe { mem.list_to_handle(ManuallyDrop::into_inner(self.storage.spill)) };
            MSpillSlice::spilled(len, mslice)
        } else {
            let array = unsafe { self.storage.inline };
            MSpillSlice::inline(len, array)
        }
    }
}

impl<T: Copy, Tag, const N: usize> Default for MSpillList<T, N, Tag> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct MSpillSlice<T: Copy, const N: usize, Tag> {
    len_flags: u32,
    storage: MSpillSliceStorage<T, Tag, N>,
}
impl<T: Copy, Tag, const N: usize> Copy for MSpillSlice<T, N, Tag> {}
impl<T: Copy, Tag, const N: usize> Clone for MSpillSlice<T, N, Tag> {
    fn clone(&self) -> Self {
        *self
    }
}
union MSpillSliceStorage<T: Copy, Tag, const N: usize> {
    inline: [MaybeUninit<T>; N],
    spill: MSlice<T, Tag>,
}
impl<T: Copy, Tag, const N: usize> Copy for MSpillSliceStorage<T, Tag, N> {}
impl<T: Copy, Tag, const N: usize> Clone for MSpillSliceStorage<T, Tag, N> {
    fn clone(&self) -> Self {
        *self
    }
}

pub type MSS1<T, Tag = ()> = MSpillSlice<T, 1, Tag>;
pub type MSS2<T, Tag = ()> = MSpillSlice<T, 2, Tag>;
pub type MSS4<T, Tag = ()> = MSpillSlice<T, 4, Tag>;
pub type MSS8<T, Tag = ()> = MSpillSlice<T, 8, Tag>;

impl<T: Copy, Tag, const N: usize> Default for MSpillSlice<T, N, Tag> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Copy, Tag, const N: usize> MSpillSlice<T, N, Tag> {
    pub fn new() -> Self {
        assert!(N > 0);

        // SAFETY: MaybeUninit<T> array does not require initialization.
        let inline = unsafe { MaybeUninit::<[MaybeUninit<T>; N]>::uninit().assume_init() };
        Self { len_flags: 0, storage: MSpillSliceStorage { inline } }
    }

    fn spilled(len: u32, data: MSlice<T, Tag>) -> Self {
        debug_assert!(len <= LEN_MASK);
        Self { len_flags: len | SPILLED_BIT, storage: MSpillSliceStorage { spill: data } }
    }

    fn inline(len: u32, data: [MaybeUninit<T>; N]) -> Self {
        debug_assert!(len <= LEN_MASK);
        Self { len_flags: len, storage: MSpillSliceStorage { inline: data } }
    }

    pub fn is_spilled(&self) -> bool {
        (self.len_flags & SPILLED_BIT) != 0
    }

    pub fn len(&self) -> u32 {
        self.len_flags & LEN_MASK
    }

    pub fn as_slice(&self, mem: &Mem<Tag>) -> &'static [T] {
        let len = self.len();

        if self.is_spilled() {
            // SAFETY: spilled variant active when spilled bit is set.
            unsafe { mem.getn(self.storage.spill) }
        } else {
            // SAFETY: inline variant active; first `len` elements were written.
            unsafe {
                let ptr = self.storage.inline.as_ptr() as *const T;
                slice::from_raw_parts(ptr, len as usize)
            }
        }
    }

    // Similar to MSpillList::into_handle but returns MSlice directly.
    pub fn into_handle(self, mem: &mut Mem<Tag>) -> MSlice<T, Tag> {
        let len = self.len();

        if self.is_spilled() {
            // SAFETY: spilled variant active. We consume self, so we can move the slice out.
            let slice: MSlice<T, Tag> = unsafe { self.storage.spill };
            slice
        } else {
            if len == 0 {
                return MSlice::empty();
            }
            // SAFETY: inline active; first `len` initialized.
            let slice: &[T] = unsafe {
                let ptr = self.storage.inline.as_ptr() as *const T;
                slice::from_raw_parts(ptr, len as usize)
            };
            mem.pushn(slice)
        }
    }

    pub fn one(t: T) -> MSpillSlice<T, N, Tag> {
        Self::from_slice(&[t])
    }

    pub fn from_slice(ts: &[T]) -> MSpillSlice<T, N, Tag> {
        assert!(N >= ts.len());
        let mut inline = [MaybeUninit::<T>::uninit(); N];
        for (i, t) in ts.iter().enumerate() {
            inline[i].write(*t);
        }
        Self::inline(ts.len() as u32, inline)
    }

    pub fn from_array<const M: usize>(ts: [T; M]) -> MSpillSlice<T, N, Tag> {
        debug_assert!(N >= M);
        let mut inline = [MaybeUninit::<T>::uninit(); N];
        for (i, t) in ts.iter().enumerate() {
            inline[i].write(*t);
        }
        Self::inline(ts.len() as u32, inline)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test() {
        let mut arena: Mem<()> = Mem::make();
        let handle = arena.push_h(42u32);
        let value = arena.get(handle);
        assert_eq!(*value, 42);
        let handle2 = arena.push_h(43u32);
        let value2 = *arena.get(handle2);
        assert_eq!(value2, 43);

        let h3 = arena.pushn(&[1, 2, 3, 4]);
        assert_eq!(h3.len(), 4);

        assert_eq!(arena.getn(h3), &[1, 2, 3, 4]);
    }

    #[test]
    fn push_slice_iter() {
        let mut arena: Mem<()> = Mem::make();
        let h = arena.pushn_iter((0..10).map(|x| x * 10));
        assert_eq!(h.len(), 10);
        assert_eq!(arena.getn(h), &[0, 10, 20, 30, 40, 50, 60, 70, 80, 90]);

        let empty_h = arena.pushn_iter(std::iter::empty::<u32>());
        assert_eq!(empty_h.len(), 0);
        assert_eq!(arena.getn(empty_h), &[]);
    }

    #[test]
    fn vec() {
        let mut arena: Mem<()> = Mem::make();
        let mut v = arena.new_list(16);
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
        let mut arena: Mem<()> = Mem::make();
        let mut v = arena.new_list(16);
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
        let mut arena: Mem<()> = Mem::make();
        let mut v = arena.new_list(3);
        v.extend(&[1, 2, 3, 4, 5]);
    }

    #[test]
    #[should_panic(expected = "MList is full")]
    fn vec_oob() {
        let mut arena: Mem<()> = Mem::make();
        let mut v = arena.new_list(4);
        for i in 0..5 {
            v.push(i * 10);
        }
    }

    #[test]
    fn dup_slice() {
        let mut arena: Mem<()> = Mem::make();
        let h = arena.pushn(&[1, 2, 3, 4, 5]);
        let h2 = arena.dupn(h);
        assert_eq!(h.len(), h2.len());
        assert_eq!(arena.getn(h), arena.getn(h2));
        assert_ne!(h.offset, h2.offset);
    }

    #[test]
    fn insert() {
        let mut arena: Mem<()> = Mem::make();
        let mut v = arena.new_list(5);
        v.push(1);
        v.push(2);
        v.push(4);
        v.insert(2, 3);
        assert_eq!(v.len(), 4);
        assert_eq!(v.as_slice(), &[1, 2, 3, 4]);
    }

    #[test]
    fn grow() {
        let mut arena: Mem<()> = Mem::make();
        let mut v = arena.new_list(2);
        v.push_grow(&mut arena, 1);
        v.push_grow(&mut arena, 2);
        v.push_grow(&mut arena, 3);
        assert_eq!(v.len(), 3);
        assert_eq!(v.as_slice(), &[1, 2, 3]);
    }

    #[test]
    fn grow_from_zero() {
        let mut arena: Mem<()> = Mem::make();
        let mut v = arena.new_list(0);
        v.push_grow(&mut arena, 1);
        v.push_grow(&mut arena, 2);
        v.push_grow(&mut arena, 3);
        assert_eq!(v.len(), 3);
        assert_eq!(v.as_slice(), &[1, 2, 3]);
    }

    #[test]
    fn spill_list_inline_and_spill() {
        let mut arena: Mem<()> = Mem::make();

        // Inline path (N=2): 0, 1, 2 elements
        let mut a: MSL2<u32> = MSpillList::new();
        assert!(!a.is_spilled() && a.is_empty() && a.as_slice(&arena).is_empty());
        a.push(&mut arena, 10);
        assert!(!a.is_spilled() && a.len() == 1 && a.as_slice(&arena) == [10]);
        a.push(&mut arena, 20);
        assert!(!a.is_spilled() && a.len() == 2 && a.as_slice(&arena) == [10, 20]);

        // First overflow triggers spill; order preserved
        a.push(&mut arena, 30);
        assert!(a.is_spilled() && a.len() == 3 && a.as_slice(&arena) == [10, 20, 30]);

        // Further pushes go to spilled list
        a.push(&mut arena, 40);
        a.push(&mut arena, 50);
        assert!(a.is_spilled() && a.len() == 5 && a.as_slice(&arena) == [10, 20, 30, 40, 50]);
    }

    #[test]
    fn spill_list_into_handle_both_paths() {
        let mut arena: Mem<()> = Mem::make();

        // Inline -> handle allocates exactly and matches contents
        let mut inl: MSL2<u32> = MSpillList::new();
        inl.push(&mut arena, 1);
        inl.push(&mut arena, 2);
        assert!(!inl.is_spilled());
        let h_inl = inl.into_slice(&mut arena);
        assert_eq!(h_inl.len(), 2);
        assert_eq!(arena.getn(h_inl), &[1, 2]);

        // Spilled -> handle should reuse underlying buffer (offset should match list buf)
        let mut sp: MSL2<u32> = MSpillList::new();
        sp.push(&mut arena, 7);
        sp.push(&mut arena, 8);
        sp.push(&mut arena, 9); // spill
        assert!(sp.is_spilled());
        let before_ptr = sp.as_slice(&arena).as_ptr().addr();
        let h_sp = sp.into_slice(&mut arena);
        let after_ptr = arena.getn(h_sp).as_ptr().addr();
        assert_eq!(arena.getn(h_sp), &[7, 8, 9]);
        assert_eq!(before_ptr, after_ptr);
    }

    #[test]
    fn spill_list_many_pushes_growth_smoke() {
        let mut arena: Mem<()> = Mem::make();

        // Ensure spill + repeated pushes stays correct across grows
        let mut v: MSL2<u32> = MSpillList::new();
        for i in 0..10 {
            v.push(&mut arena, i);
        }
        assert!(v.is_spilled() && v.len() == 10);
        let s = v.as_slice(&arena);
        assert_eq!(s.len(), 10);
        assert_eq!(s[0], 0);
        assert_eq!(s[1], 1);
        assert_eq!(s[2], 2);
        assert_eq!(s[5], 5);
        assert_eq!(s[9], 9);

        let h = v.into_slice(&mut arena);
        assert_eq!(h.len(), 10);
        assert_eq!(arena.get_nth(h, 0), &0);
        assert_eq!(arena.get_nth(h, 9), &9);
    }
}
