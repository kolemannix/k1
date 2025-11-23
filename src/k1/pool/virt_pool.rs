// Copyright (c) 2025 knix
// All rights reserved.

use std::num::NonZeroU32;

use smallvec::SmallVec;

use crate::pool::{PoolIndex, SliceHandle};

pub struct VPool<T, Index: PoolIndex> {
    // It would be a lot more powerful if each entry could point to its 'next', or if each entry
    // were an enum allowing redirects. The issue there is we can't really provide a slice, we can
    // only provide iterators, it's just a lot more to do, and there's overhead per lookup
    //
    // So it makes sense to just have this simple one be simple, and then if I need mutation,
    // such as appending to a list while keeping its Handles valid, that will just be a different
    // type of pool
    mmap: memmap2::MmapMut,
    len: usize,
    max_len: usize,
    expected_hint: Option<usize>,
    #[allow(unused)]
    name: &'static str,
    _index: std::marker::PhantomData<Index>,
    _elem: std::marker::PhantomData<T>,
}

impl<T, Index: PoolIndex> VPool<T, Index> {
    pub fn make_with_hint(name: &'static str, expected_usage: usize) -> Self {
        Self::make_bytes(name, crate::GIGABYTE, Some(expected_usage))
    }
    pub fn make(name: &'static str) -> Self {
        Self::make_bytes(name, crate::GIGABYTE, None)
    }
    pub fn make_bytes(name: &'static str, bytes: usize, expected_usage: Option<usize>) -> Self {
        let mmap = memmap2::MmapMut::map_anon(bytes).unwrap();
        mmap.advise(memmap2::Advice::Sequential).unwrap();
        if let Some(expected_usage) = expected_usage {
            mmap.advise_range(
                memmap2::Advice::WillNeed,
                0,
                expected_usage * std::mem::size_of::<T>(),
            )
            .unwrap();
        }
        let len_in_ts = mmap.len() / std::mem::size_of::<T>();
        VPool {
            name,
            mmap,
            len: 0,
            max_len: len_in_ts,
            expected_hint: expected_usage,
            _index: std::marker::PhantomData,
            _elem: std::marker::PhantomData,
        }
    }

    #[inline]
    fn data(&self) -> &[T] {
        unsafe { core::slice::from_raw_parts(self.mmap.as_ptr() as *const T, self.max_len) }
    }

    #[inline]
    fn data_mut(&mut self) -> &mut [T] {
        unsafe { core::slice::from_raw_parts_mut(self.mmap.as_mut_ptr() as *mut T, self.max_len) }
    }

    pub fn next_id(&self) -> Index {
        let index = crate::nzu32_from_incr(self.len() as u32);
        Index::from(index)
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn set(&mut self, id: Index, t: T) {
        let index = Self::id_to_actual_index(id);
        let ptr = unsafe { self.data_mut().as_mut_ptr().add(index) };
        unsafe { core::ptr::write(ptr, t) }
    }

    pub fn add(&mut self, t: T) -> Index {
        let id = self.next_id();

        self.set(id, t);

        self.set_len_checked(self.len + 1);

        id
    }

    pub fn add_expected_id(&mut self, t: T, expected_id: Index) -> Index {
        let id = self.next_id();

        #[cfg(debug_assertions)]
        if id != expected_id {
            panic!("VPool add_expected_id: expected id {}, got {}", std::convert::Into::<NonZeroU32>::into(expected_id), id.into());
        }

        self.set(id, t);

        self.set_len_checked(self.len + 1);

        id
    }

    pub fn add_slice_from_iter(&mut self, items: impl Iterator<Item = T>) -> SliceHandle<Index> {
        let id = self.next_id();
        let mut count: u32 = 0;
        for t in items {
            let item_id = id + count;
            self.set(item_id, t);
            count += 1;
        }

        self.set_len_checked(self.len + count as usize);

        SliceHandle::make(id, count)
    }

    fn id_to_actual_index(index: Index) -> usize {
        let nz32: NonZeroU32 = index.into();
        nz32.get() as usize - 1
    }

    fn physical_index_to_id(index: usize) -> Index {
        // Safety: Incrementing by 1
        let index_inc = crate::nzu32_from_incr(index as u32);
        Index::from(index_inc)
    }

    fn get_index(&self, index: usize) -> &T {
        self.bounds_check(index);
        let v = &self.data()[index];
        v
    }

    pub fn get(&self, index: Index) -> &T {
        self.get_index(Self::id_to_actual_index(index))
    }

    pub fn get_opt(&self, id: Index) -> Option<&T> {
        let index = Self::id_to_actual_index(id);
        if index >= self.len {
            return None;
        }
        Some(self.get_index(index))
    }

    #[inline]
    #[track_caller]
    fn bounds_check(&self, index: usize) {
        if index >= self.len {
            panic!("Pool Index out of bounds: {} >= {}", index, self.len);
        }
    }

    #[inline]
    #[track_caller]
    fn set_len_checked(&mut self, proposed_len: usize) {
        if proposed_len > self.max_len {
            panic!("VPool out of space: len {} > {}", proposed_len, self.max_len);
        }
        self.len = proposed_len
    }

    pub fn get_mut(&mut self, id: Index) -> &mut T {
        let index = Self::id_to_actual_index(id);
        self.bounds_check(index);
        &mut self.data_mut()[index]
    }

    pub fn get_slice(&self, handle: SliceHandle<Index>) -> &[T] {
        match handle.index() {
            None => &[],
            Some(index) => self.get_n(index, handle.len),
        }
    }

    pub fn get_slice_mut(&mut self, handle: SliceHandle<Index>) -> &mut [T] {
        match handle.index() {
            None => &mut [],
            Some(index) => self.get_n_mut(index, handle.len),
        }
    }

    pub fn get_nth(&self, handle: SliceHandle<Index>, index: usize) -> &T {
        debug_assert!(index < handle.len());
        let Some(id) = handle.index() else {
            panic!("get_nth called on empty handle");
        };
        let slice_start_index = Self::id_to_actual_index(id);
        self.bounds_check(slice_start_index + handle.len as usize - 1);
        let elem_index = slice_start_index + index;
        self.get_index(elem_index)
    }

    pub fn get_n(&self, index: Index, count: u32) -> &[T] {
        let index = Self::id_to_actual_index(index);
        let end = index + count as usize;
        self.bounds_check(end - 1);
        &self.data()[index..end]
    }

    pub fn get_n_mut(&mut self, index: Index, count: u32) -> &mut [T] {
        let index = Self::id_to_actual_index(index);
        let end = index + count as usize;
        self.bounds_check(end - 1);
        &mut self.data_mut()[index..end]
    }

    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.data()[0..self.len].iter()
    }

    pub fn iter_ids(&self) -> impl Iterator<Item = Index> {
        (0..self.len()).map(|i| Self::physical_index_to_id(i))
    }

    pub fn iter_with_ids(&self) -> impl Iterator<Item = (Index, &T)> {
        (0..self.len()).map(|i| (Self::physical_index_to_id(i), self.get_index(i)))
    }

    pub fn get_first(&self, handle: SliceHandle<Index>) -> Option<&T> {
        #[allow(clippy::question_mark)]
        let Some(index) = handle.index() else { return None };
        self.get_opt(index)
    }

    pub fn print_size_info(&self) {
        let percent_used = (self.len as u128) * 100 / self.max_len as u128;
        let mb_used = self.len * std::mem::size_of::<T>() / crate::MEGABYTE;
        let size_in_mb = self.mmap.len() / crate::MEGABYTE;
        let hint = match self.expected_hint {
            None => "".to_string(),
            Some(hint) => format!("[{:2}% of hinted]", self.len as u128 * 100 / (hint as u128)),
        };
        eprintln!(
            "VPool {:16}: {:04} / {:04}mb ({:02}%) used by {} / {} elements {} ({} size {})",
            self.name,
            mb_used,
            size_in_mb,
            percent_used,
            self.len,
            self.max_len,
            hint,
            std::any::type_name::<T>(),
            std::mem::size_of::<T>()
        );
    }
}

#[cfg(feature = "profile")]
impl<T, Index: PoolIndex> Drop for VPool<T, Index> {
    fn drop(&mut self) {
        self.print_size_info()
    }
}

/// WHEN T IS COPY IMPL
impl<T: Copy, Index: PoolIndex> VPool<T, Index> {
    pub fn copy_slice_sv<const N: usize>(&self, handle: SliceHandle<Index>) -> SmallVec<[T; N]>
    where
        [T; N]: smallvec::Array<Item = T>,
    {
        SmallVec::from_slice(self.get_slice(handle))
    }

    pub fn copy_slice_sv4(&self, handle: SliceHandle<Index>) -> SmallVec<[T; 4]>
    where
        [T; 4]: smallvec::Array<Item = T>,
    {
        SmallVec::from_slice(self.get_slice(handle))
    }

    pub fn copy_slice_sv8(&self, handle: SliceHandle<Index>) -> SmallVec<[T; 8]>
    where
        [T; 8]: smallvec::Array<Item = T>,
    {
        SmallVec::from_slice(self.get_slice(handle))
    }

    pub fn add_slice_copy(&mut self, items: &[T]) -> SliceHandle<Index> {
        let items_len = items.len();
        let id = self.next_id();
        let index = Self::id_to_actual_index(id);
        let dst = &mut self.data_mut()[index..index + items_len];
        dst.copy_from_slice(items);
        self.set_len_checked(self.len + items_len);
        SliceHandle::make(id, items_len as u32)
    }
}

/// WHEN T IS COPY AND EQ
impl<T: Copy + PartialEq + Eq, Index: PoolIndex> VPool<T, Index> {
    pub fn slices_equal_copy(
        &self,
        handle1: SliceHandle<Index>,
        handle2: SliceHandle<Index>,
    ) -> bool {
        if handle1.len() != handle2.len() {
            return false;
        }
        let slice1 = self.get_slice(handle1);
        let slice2 = self.get_slice(handle2);
        slice1 == slice2
    }
}

// WHEN T IS EQ
impl<T: PartialEq, Index: PoolIndex> VPool<T, Index> {
    pub fn slice_contains(&self, handle: SliceHandle<Index>, elem: &T) -> bool {
        self.get_slice(handle).contains(elem)
    }
}

#[cfg(test)]
mod test {
    use super::VPool;
    use itertools::Itertools;
    use std::num::NonZeroU32;

    use crate::{nz_u32_id, pool::SliceHandle};

    nz_u32_id!(MyIndex);

    pub struct Foo {
        data: i32,
    }
    impl From<i32> for Foo {
        fn from(v: i32) -> Self {
            Foo { data: v }
        }
    }
    impl Drop for Foo {
        fn drop(&mut self) {
            eprintln!("Dropping Foo {}", self.data)
        }
    }

    #[test]
    fn single() {
        let mut pool: VPool<Foo, MyIndex> = VPool::make("single");
        let handle = pool.add(Foo { data: 42 });
        assert_eq!(pool.get(handle).data, 42);
        *pool.get_mut(handle) = Foo { data: 43 };
        assert_eq!(pool.get(handle).data, 43);
    }

    #[test]
    fn slice() {
        let mut pool: VPool<Foo, MyIndex> = VPool::make("slice");
        let handle = pool
            .add_slice_from_iter([Foo { data: 1 }, Foo { data: 2 }, Foo { data: 3 }].into_iter());
        assert_eq!(pool.get_slice(handle).iter().map(|foo| foo.data).collect_vec(), vec![1, 2, 3]);

        pool.get_slice_mut(handle)[1] = Foo { data: 42 };
        assert_eq!(pool.get_slice(handle).iter().map(|foo| foo.data).collect_vec(), &[1, 42, 3]);

        let skip_1 = handle.skip(1);
        assert_eq!(skip_1.len(), 2);
        assert_eq!(pool.get_slice(skip_1).iter().map(|foo| foo.data).collect_vec(), &[42, 3]);

        let skip_all = handle.skip(3);
        assert!(skip_all.is_empty());
    }

    #[test]
    #[should_panic(expected = "Pool Index out of bounds")]
    fn out_of_bounds_single() {
        let pool: VPool<i32, MyIndex> = VPool::make("bounds_test");
        let invalid_id = MyIndex::from(NonZeroU32::new(1).unwrap());

        pool.get(invalid_id);
    }

    #[test]
    #[should_panic(expected = "Pool Index out of bounds")]
    fn out_of_bounds_slice_start() {
        let mut pool: VPool<i32, MyIndex> = VPool::make("bounds_test");
        pool.add(1);
        pool.add(2);
        pool.add(3);

        // x x x _
        //       [   ]
        let slice_handle = SliceHandle::make(MyIndex::from(NonZeroU32::new(4).unwrap()), 1);
        pool.get_slice(slice_handle);
    }

    #[test]
    #[should_panic(expected = "Pool Index out of bounds")]
    fn out_of_bounds_slice_end() {
        let mut pool: VPool<i32, MyIndex> = VPool::make("bounds_test");
        pool.add(1);
        pool.add(2);
        pool.add(3);

        // x x x _
        //   [   ]
        let slice_handle = SliceHandle::make(MyIndex::from(NonZeroU32::new(2).unwrap()), 3);
        pool.get_slice(slice_handle);
    }
}
