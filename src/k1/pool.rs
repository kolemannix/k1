// Copyright (c) 2025 knix
// All rights reserved.
use std::hash::{Hash, Hasher};
use std::{num::NonZeroU32, ops::Add};

use smallvec::SmallVec;
mod virt_pool;

pub trait PoolIndex:
    Copy
    + Into<NonZeroU32>
    + From<NonZeroU32>
    + PartialEq
    + Eq
    + Add<Self, Output = Self>
    + Add<u32, Output = Self>
    + Hash
{
}
impl<
    T: Copy
        + Into<NonZeroU32>
        + From<NonZeroU32>
        + PartialEq
        + Eq
        + Add<Self, Output = Self>
        + Add<u32, Output = Self>
        + Hash,
> PoolIndex for T
{
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SliceHandle<Index: PoolIndex> {
    index: Option<Index>,
    len: u32,
}

impl<Index: PoolIndex> Hash for SliceHandle<Index> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state);
        self.len.hash(state);
    }
}

impl<Index: PoolIndex> SliceHandle<Index> {
    pub const fn empty() -> Self {
        Self { index: None, len: 0 }
    }
    pub fn make_nz(index: Index, len: NonZeroU32) -> Self {
        Self { index: Some(index), len: len.get() }
    }
    pub fn make(index: Index, len: u32) -> Self {
        if len == 0 { Self { index: None, len: 0 } } else { Self { index: Some(index), len } }
    }

    #[inline]
    pub fn index(&self) -> Option<Index> {
        self.index
    }

    #[inline]
    pub fn end_index(&self) -> Option<Index> {
        match self.index {
            None => None,
            Some(index) => {
                let end_index = index + self.len;
                Some(end_index)
            }
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len as usize
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Skip this entry, resulting in a handle with a length
    /// decreased by n, and pointing n elements ahead of where it was.
    /// Returns an empty handle on an already empty handle
    pub fn skip(&self, n: usize) -> Self {
        if n == 0 {
            return *self;
        }
        match self.index {
            None => *self,
            Some(index) => {
                let new_len = (self.len).saturating_sub(n as u32);
                if new_len == 0 {
                    Self { index: Some(index), len: new_len }
                } else {
                    let new_index = index + (n as u32);
                    Self { index: Some(new_index), len: new_len }
                }
            }
        }
    }
}

#[allow(unused)]
pub struct Pool<T, Index: Into<NonZeroU32> + From<NonZeroU32>> {
    // It would be a lot more powerful if each entry could point to its 'next', or if each entry
    // were an enum allowing redirects. The issue there is we can't really provide a slice, we can
    // only provide iterators, it's just a lot more to do, and there's overhead per lookup
    //
    // So it makes sense to just have this simple one be simple, and then if I need mutation,
    // such as appending to a list while keeping its Handles valid, that will just be a different
    // type of pool
    vec: Vec<T>,
    #[allow(unused)]
    name: &'static str,
    _index: std::marker::PhantomData<Index>,
}

#[allow(unused)]
impl<T, Index: PoolIndex> Pool<T, Index> {
    pub fn with_capacity(name: &'static str, capacity: usize) -> Pool<T, Index> {
        Pool { name, vec: Vec::with_capacity(capacity), _index: std::marker::PhantomData }
    }

    pub fn new(name: &'static str) -> Pool<T, Index> {
        Pool { name, vec: Vec::new(), _index: std::marker::PhantomData }
    }

    pub fn next_id(&self) -> Index {
        let index = crate::nzu32_from_incr(self.vec.len() as u32);
        Index::from(index)
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    pub fn add(&mut self, t: T) -> Index {
        #[cfg(debug_assertions)]
        let cap = self.vec.capacity();

        let index = self.next_id();
        self.vec.push(t);

        #[cfg(debug_assertions)]
        {
            let new_cap = self.vec.capacity();
            if new_cap != cap {
                eprintln!("WARNING: POOL {} RESIZED {cap} -> {new_cap}", self.name);
            }
        }

        index
    }

    pub fn add_slice_from_iter(&mut self, items: impl Iterator<Item = T>) -> SliceHandle<Index> {
        #[cfg(debug_assertions)]
        let cap = self.vec.capacity();

        let index = self.next_id();
        let mut count: u32 = 0;
        for t in items {
            self.vec.push(t);
            count += 1;
        }

        #[cfg(debug_assertions)]
        {
            let new_cap = self.vec.capacity();
            if new_cap != cap {
                eprintln!("POOL {} RESIZED {cap} -> {new_cap}", self.name)
            }
        }

        SliceHandle::make(index, count)
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

    pub fn get(&self, index: Index) -> &T {
        let index = Self::id_to_actual_index(index);
        &self.vec[index]
    }

    pub fn get_opt(&self, index: Index) -> Option<&T> {
        let index = Self::id_to_actual_index(index);
        self.vec.get(index)
    }

    pub fn get_mut(&mut self, index: Index) -> &mut T {
        let index = Self::id_to_actual_index(index);
        &mut self.vec[index]
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
        let Some(handle_index) = handle.index() else {
            panic!("get_nth called on empty handle");
        };
        let slice_start_index = Self::id_to_actual_index(handle_index);
        let elem_index = slice_start_index + index;
        &self.vec[elem_index]
    }

    pub fn get_n(&self, index: Index, count: u32) -> &[T] {
        if count == 0 {
            return &[];
        }
        let index = Self::id_to_actual_index(index);
        let end = index + count as usize;
        &self.vec[index..end]
    }

    pub fn get_n_mut(&mut self, index: Index, count: u32) -> &mut [T] {
        if count == 0 {
            return &mut [];
        }
        let index = Self::id_to_actual_index(index);
        let end = index + count as usize;
        &mut self.vec[index..end]
    }

    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.vec.iter()
    }

    pub fn iter_ids(&self) -> impl Iterator<Item = Index> {
        (0..self.vec.len()).map(|i| Self::physical_index_to_id(i))
    }

    pub fn iter_with_ids(&self) -> impl Iterator<Item = (Index, &T)> {
        (0..self.vec.len()).map(|i| (Self::physical_index_to_id(i), &self.vec[i]))
    }

    pub fn get_first(&self, handle: SliceHandle<Index>) -> Option<&T> {
        #[allow(clippy::question_mark)]
        let Some(index) = handle.index() else { return None };
        let slice_start_index = Self::id_to_actual_index(index);
        self.vec.get(slice_start_index)
    }
}

/// WHEN T IS CLONE IMPL
#[allow(unused)]
impl<T: Clone, Index: PoolIndex> Pool<T, Index> {
    pub fn get_slice_to_smallvec<const N: usize>(
        &self,
        handle: SliceHandle<Index>,
    ) -> SmallVec<[T; N]>
    where
        [T; N]: smallvec::Array<Item = T>,
    {
        smallvec::SmallVec::from(self.get_slice(handle))
    }

    pub fn add_slice_from_slice(&mut self, items: &[T]) -> SliceHandle<Index> {
        // This implementation is specialized for slice iterators, where it uses [`copy_from_slice`] to
        // append the entire slice at once.
        let index = self.next_id();
        self.vec.extend_from_slice(items);
        SliceHandle::make(index, items.len() as u32)
    }
}

/// WHEN T IS COPY IMPL
#[allow(unused)]
impl<T: Copy, Index: PoolIndex> Pool<T, Index> {
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

    pub fn add_slice_from_copy_slice(&mut self, items: &[T]) -> SliceHandle<Index> {
        let index = self.next_id();
        // This implementation is specialized for slice iterators, where it uses [`copy_from_slice`] to
        // append the entire slice at once.
        self.vec.extend(items);
        SliceHandle::make(index, items.len() as u32)
    }
}

/// WHEN T IS COPY AND EQ
#[allow(unused)]
impl<T: Copy + PartialEq + Eq, Index: PoolIndex> Pool<T, Index> {
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
#[allow(unused)]
impl<T: PartialEq, Index: PoolIndex> Pool<T, Index> {
    pub fn slice_contains(&self, handle: SliceHandle<Index>, elem: &T) -> bool {
        self.get_slice(handle).contains(elem)
    }
}

pub use virt_pool::VPool;

#[cfg(test)]
mod test {
    use crate::nz_u32_id;

    nz_u32_id!(MyIndex);

    use super::Pool;
    #[test]
    fn single() {
        let mut pool: Pool<i32, MyIndex> = Pool::new("single");
        let handle = pool.add(42);
        assert_eq!(*pool.get(handle), 42);
    }

    #[test]
    fn slice() {
        let mut pool: Pool<i32, MyIndex> = Pool::new("slice");
        let handle = pool.add_slice_from_iter([1, 2, 3].iter().copied());
        assert_eq!(pool.get_slice(handle), &[1, 2, 3]);
    }

    #[test]
    fn mutate_slice() {
        let mut pool: Pool<i32, MyIndex> = Pool::new("mutate_slice");
        let handle = pool.add_slice_from_iter([1, 2, 3].iter().copied());
        pool.get_slice_mut(handle)[1] = 42;
        assert_eq!(pool.get_slice(handle), &[1, 42, 3]);
    }

    #[test]
    fn skip_slice_handle() {
        let mut pool: Pool<i32, MyIndex> = Pool::new("mutate_slice");
        let handle = pool.add_slice_from_iter([1, 2].iter().copied());
        assert_eq!(handle.len(), 2);
        let handle = handle.skip(1);
        assert_eq!(pool.get_slice(handle), &[2]);
        assert_eq!(handle.len(), 1);

        let handle = handle.skip(1);
        assert!(pool.get_slice(handle).is_empty());
        assert_eq!(handle.len(), 0);

        let handle = handle.skip(1);
        assert!(pool.get_slice(handle).is_empty());
        assert_eq!(handle.len(), 0);
    }
}
