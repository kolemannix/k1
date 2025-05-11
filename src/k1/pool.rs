use std::{num::NonZeroU32, ops::Add};

use smallvec::SmallVec;

pub trait PoolIndex:
    Copy + Into<NonZeroU32> + From<NonZeroU32> + Eq + Add<Self, Output = Self>
{
}
impl<T: Copy + Into<NonZeroU32> + From<NonZeroU32> + Eq + Add<Self, Output = Self>> PoolIndex
    for T
{
}

#[derive(Debug, Clone, Copy)]
pub struct SliceHandleInner<Index: PoolIndex> {
    pub index: Index,
    pub len: NonZeroU32,
}

#[derive(Debug, Clone, Copy)]
pub enum SliceHandle<Index: PoolIndex> {
    Empty,
    NonEmpty(SliceHandleInner<Index>),
}

impl<Index: PoolIndex> SliceHandle<Index> {
    pub fn len(&self) -> usize {
        match self {
            SliceHandle::Empty => 0,
            SliceHandle::NonEmpty(slice) => slice.len.get() as usize,
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            SliceHandle::Empty => true,
            SliceHandle::NonEmpty(_) => false,
        }
    }

    /// Skip this entry, resulting in a handle with a length
    /// decreased by n, and pointing n elements ahead of where it was.
    /// Returns an empty handle on an already empty handle
    pub fn skip(&self, n: usize) -> Self {
        let Some(skip_count_nzu32) = NonZeroU32::new(n as u32) else {
            return *self;
        };
        match self {
            SliceHandle::Empty => SliceHandle::Empty,
            SliceHandle::NonEmpty(inner) => {
                let new_len = (inner.len.get() as usize).saturating_sub(n);
                if new_len == 0 {
                    SliceHandle::Empty
                } else {
                    let new_index = inner.index + Index::from(skip_count_nzu32);
                    SliceHandle::NonEmpty(SliceHandleInner {
                        index: new_index,
                        len: NonZeroU32::new(new_len as u32).unwrap(),
                    })
                }
            }
        }
    }
}

pub struct Pool<T, Index: Into<NonZeroU32> + From<NonZeroU32>> {
    // It would be a lot more powerful if each entry could point to its 'next', or if each entry
    // were an enum allowing redirects. The issue there is we can't really provide a slice, we can
    // only provide iterators, it's just a lot more to do, and there's overhead per lookup
    //
    // So it makes sense to just have this simple one be simple, and then if I need mutation,
    // such as appending to a list while keeping its Handles valid, that will just be a different
    // type of pool
    vec: Vec<T>,
    name: &'static str,
    _index: std::marker::PhantomData<Index>,
}

impl<T, Index: PoolIndex> Pool<T, Index> {
    pub fn with_capacity(name: &'static str, capacity: usize) -> Pool<T, Index> {
        Pool { name, vec: Vec::with_capacity(capacity), _index: std::marker::PhantomData }
    }

    pub fn new(name: &'static str) -> Pool<T, Index> {
        Pool { name, vec: Vec::new(), _index: std::marker::PhantomData }
    }

    pub fn next_id(&self) -> Index {
        let index = crate::nzu32_increment(self.vec.len() as u32);
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

    pub fn add_list(&mut self, items: impl Iterator<Item = T>) -> SliceHandle<Index> {
        #[cfg(debug_assertions)]
        let cap = self.vec.capacity();

        let index = self.next_id();
        let mut count: u32 = 0;
        for t in items {
            self.vec.push(t);
            count += 1;
        }

        let Some(count) = NonZeroU32::new(count) else { return SliceHandle::Empty };

        #[cfg(debug_assertions)]
        {
            let new_cap = self.vec.capacity();
            if new_cap != cap {
                eprintln!("POOL {} RESIZED {cap} -> {new_cap}", self.name)
            }
        }

        SliceHandle::NonEmpty(SliceHandleInner { index, len: count })
    }

    fn id_to_actual_index(index: Index) -> usize {
        let nz32: NonZeroU32 = index.into();
        nz32.get() as usize - 1
    }

    fn physical_index_to_id(index: usize) -> Index {
        // Safety: Incrementing by 1
        let index_inc = crate::nzu32_increment(index as u32);
        Index::from(index_inc)
    }

    pub fn get(&self, index: Index) -> &T {
        let index = Self::id_to_actual_index(index);
        &self.vec[index]
    }

    pub fn get_mut(&mut self, index: Index) -> &mut T {
        let index = Self::id_to_actual_index(index);
        &mut self.vec[index]
    }

    pub fn get_slice(&self, handle: SliceHandle<Index>) -> &[T] {
        match handle {
            SliceHandle::Empty => &[],
            SliceHandle::NonEmpty(handle) => self.get_n(handle.index, handle.len.get()),
        }
    }

    pub fn get_slice_mut(&mut self, handle: SliceHandle<Index>) -> &mut [T] {
        match handle {
            SliceHandle::Empty => &mut [],
            SliceHandle::NonEmpty(handle) => self.get_n_mut(handle.index, handle.len.get()),
        }
    }

    pub fn get_nth(&self, handle: SliceHandle<Index>, index: usize) -> &T {
        debug_assert!(index < handle.len());
        let SliceHandle::NonEmpty(handle) = handle else {
            panic!("get_list_elem called on empty list");
        };
        let list_start_index = Self::id_to_actual_index(handle.index);
        let elem_index = list_start_index + index;
        &self.vec[elem_index]
    }

    pub fn get_n(&self, index: Index, count: u32) -> &[T] {
        let index = Self::id_to_actual_index(index);
        let end = index + count as usize;
        &self.vec[index..end]
    }

    pub fn get_n_mut(&mut self, index: Index, count: u32) -> &mut [T] {
        let index = Self::id_to_actual_index(index);
        let end = index + count as usize;
        &mut self.vec[index..end]
    }

    pub fn iter(&self) -> std::slice::Iter<T> {
        self.vec.iter()
    }

    pub fn iter_ids(&self) -> impl Iterator<Item = Index> {
        (0..self.vec.len()).map(|i| Self::physical_index_to_id(i))
    }

    pub fn iter_with_ids(&self) -> impl Iterator<Item = (Index, &T)> {
        (0..self.vec.len()).map(|i| (Self::physical_index_to_id(i), &self.vec[i]))
    }

    pub fn get_first(&self, handle: SliceHandle<Index>) -> Option<&T> {
        let SliceHandle::NonEmpty(handle) = handle else { return None };
        let list_start_index = Self::id_to_actual_index(handle.index);
        self.vec.get(list_start_index)
    }
}

/// WHEN T IS CLONE IMPL
impl<T: Clone, Index: PoolIndex> Pool<T, Index> {
    pub fn get_list_to_smallvec<const N: usize>(
        &self,
        handle: SliceHandle<Index>,
    ) -> SmallVec<[T; N]>
    where
        [T; N]: smallvec::Array<Item = T>,
    {
        smallvec::SmallVec::from(self.get_slice(handle))
    }

    pub fn add_list_from_slice(&mut self, items: &[T]) -> SliceHandle<Index> {
        if let Some(len) = NonZeroU32::new(items.len() as u32) {
            // This implementation is specialized for slice iterators, where it uses [`copy_from_slice`] to
            // append the entire slice at once.
            let index = self.next_id();
            self.vec.extend_from_slice(items);
            SliceHandle::NonEmpty(SliceHandleInner { index, len })
        } else {
            SliceHandle::Empty
        }
    }
}

/// WHEN T IS COPY IMPL
impl<T: Copy, Index: PoolIndex> Pool<T, Index> {
    pub fn get_list_to_smallvec_copy<const N: usize>(
        &self,
        handle: SliceHandle<Index>,
    ) -> SmallVec<[T; N]>
    where
        [T; N]: smallvec::Array<Item = T>,
    {
        SmallVec::from_slice(self.get_slice(handle))
    }

    pub fn add_list_from_copy_slice(&mut self, items: &[T]) -> SliceHandle<Index> {
        if let Some(len) = NonZeroU32::new(items.len() as u32) {
            // This implementation is specialized for slice iterators, where it uses [`copy_from_slice`] to
            // append the entire slice at once.
            let index = self.next_id();
            self.vec.extend(items);
            SliceHandle::NonEmpty(SliceHandleInner { index, len })
        } else {
            SliceHandle::Empty
        }
    }
}

/// WHEN T IS COPY AND EQ
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
        // nocommit find a way to ensure we get the bitwise comparison without
        // using unsafe ideally and maybe without requiring partialeq+eq
        slice1 == slice2
    }
}

#[cfg(test)]
mod test {
    use std::num::NonZeroU32;

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
    fn list() {
        let mut pool: Pool<i32, MyIndex> = Pool::new("list");
        let handle = pool.add_list([1, 2, 3].iter().copied());
        assert_eq!(pool.get_slice(handle), &[1, 2, 3]);
    }

    #[test]
    fn mutate_list() {
        let mut pool: Pool<i32, MyIndex> = Pool::new("mutate_list");
        let handle = pool.add_list([1, 2, 3].iter().copied());
        pool.get_slice_mut(handle)[1] = 42;
        assert_eq!(pool.get_slice(handle), &[1, 42, 3]);
    }

    #[test]
    fn skip_slice_handle() {
        let mut pool: Pool<i32, MyIndex> = Pool::new("mutate_list");
        let handle = pool.add_list([1, 2].iter().copied());
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
