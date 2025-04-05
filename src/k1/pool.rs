use std::num::NonZeroU32;

use smallvec::{Array, SmallVec};

trait PoolIndex: Copy + Into<NonZeroU32> + From<NonZeroU32> {}
impl<T: Copy + Into<NonZeroU32> + From<NonZeroU32>> PoolIndex for T {}

#[derive(Debug, Clone, Copy)]
pub struct SliceHandleInner<T: Copy + Into<NonZeroU32> + From<NonZeroU32>> {
    pub index: T,
    pub len: NonZeroU32,
}

#[derive(Debug, Clone, Copy)]
pub enum SliceHandle<T: Copy + Into<NonZeroU32> + From<NonZeroU32>> {
    Empty,
    NonEmpty(SliceHandleInner<T>),
}

impl<T: Copy + Into<NonZeroU32> + From<NonZeroU32>> SliceHandle<T> {
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

impl<T, Index: Copy + Into<NonZeroU32> + From<NonZeroU32>> Pool<T, Index> {
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

    pub fn add(&mut self, t: T) -> Index {
        #[cfg(debug_assertions)]
        let cap = self.vec.capacity();

        let index = self.next_id();
        self.vec.push(t);

        #[cfg(debug_assertions)]
        {
            let new_cap = self.vec.capacity();
            if new_cap != cap {
                eprintln!("POOL {} RESIZED {cap} -> {new_cap}", self.name)
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

    pub fn get_list(&self, handle: SliceHandle<Index>) -> &[T] {
        match handle {
            SliceHandle::Empty => &[],
            SliceHandle::NonEmpty(handle) => self.get_many(handle.index, handle.len.get()),
        }
    }

    pub fn get_list_elem(&self, handle: SliceHandle<Index>, index: usize) -> &T {
        debug_assert!(index < handle.len());
        let SliceHandle::NonEmpty(handle) = handle else {
            panic!("get_list_elem called on empty list");
        };
        let list_start_index = Self::id_to_actual_index(handle.index);
        let elem_index = list_start_index + index;
        &self.vec[elem_index]
    }

    pub fn get_many(&self, index: Index, count: u32) -> &[T] {
        let index = Self::id_to_actual_index(index);
        let end = index + count as usize;
        &self.vec[index..end]
    }

    pub fn get_many_mut(&mut self, index: Index, count: u32) -> &mut [T] {
        let index = Self::id_to_actual_index(index);
        let end = index + count as usize;
        &mut self.vec[index..end]
    }

    pub fn get_list_mut(&mut self, handle: SliceHandle<Index>) -> &mut [T] {
        match handle {
            SliceHandle::Empty => &mut [],
            SliceHandle::NonEmpty(handle) => self.get_many_mut(handle.index, handle.len.get()),
        }
    }

    pub fn iter(&self) -> std::slice::Iter<T> {
        self.vec.iter()
    }

    pub fn iter_ids(&self) -> impl Iterator<Item = Index> {
        (0..self.vec.len()).map(|i| Self::physical_index_to_id(i))
    }
}

/// WHEN T IS CLONE IMPL
impl<T: Clone, Index: Copy + Into<NonZeroU32> + From<NonZeroU32>> Pool<T, Index> {
    pub fn get_list_to_smallvec<const N: usize>(
        &self,
        handle: SliceHandle<Index>,
    ) -> SmallVec<[T; N]>
    where
        [T; N]: smallvec::Array<Item = T>,
    {
        smallvec::SmallVec::from(self.get_list(handle))
    }
}

/// WHEN T IS COPY IMPL
impl<T: Copy, Index: Copy + Into<NonZeroU32> + From<NonZeroU32>> Pool<T, Index> {
    pub fn get_list_to_smallvec_copy<const N: usize>(
        &self,
        handle: SliceHandle<Index>,
    ) -> SmallVec<[T; N]>
    where
        [T; N]: smallvec::Array<Item = T>,
    {
        SmallVec::from_slice(self.get_list(handle))
    }

    pub fn add_list_from_copy_slice(&mut self, items: &[T]) -> SliceHandle<Index> {}
}

#[cfg(test)]
mod test {
    use std::num::NonZeroU32;

    use super::Pool;
    #[test]
    fn single() {
        let mut pool: Pool<i32, NonZeroU32> = Pool::new("single");
        let handle: NonZeroU32 = pool.add(42);
        assert_eq!(*pool.get(handle), 42);
    }

    #[test]
    fn list() {
        let mut pool: Pool<i32, NonZeroU32> = Pool::new("list");
        let handle = pool.add_list([1, 2, 3].iter().copied());
        assert_eq!(pool.get_list(handle), &[1, 2, 3]);
    }

    #[test]
    fn mutate_list() {
        let mut pool: Pool<i32, NonZeroU32> = Pool::new("mutate_list");
        let handle = pool.add_list([1, 2, 3].iter().copied());
        pool.get_list_mut(handle)[1] = 42;
        assert_eq!(pool.get_list(handle), &[1, 42, 3]);
    }
}
