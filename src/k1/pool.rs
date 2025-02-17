use std::num::NonZeroU32;

use crate::static_assert_size;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(NonZeroU32);
impl Id {
    pub const PENDING: Id = Id(NonZeroU32::MAX);
}
impl From<usize> for Id {
    fn from(i: usize) -> Id {
        Id(NonZeroU32::new(i as u32 + 1).unwrap())
    }
}

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<Id> for usize {
    fn from(id: Id) -> usize {
        id.0.get() as usize - 1
    }
}

#[derive(Clone, Copy)]
pub struct PoolListHandle {
    pub index: Id,
    pub count: u32,
}

static_assert_size!(PoolListHandle, 8);

pub struct Pool<T> {
    // It would be a lot more powerful if each entry could point to its 'next', or if each entry
    // were an enum allowing redirects. The issue there is we can't really provide a slice, we can
    // only provide iterators, it's just a lot more to do, and there's overhead per lookup
    //
    // So it makes sense to just have this simple one be simple, and then if I need mutation,
    // such as appending to a list while keeping its Handles valid, that will just be a different
    // type of pool
    vec: Vec<T>,
    name: &'static str,
}

impl<T> Pool<T> {
    pub fn with_capacity(name: &'static str, capacity: usize) -> Pool<T> {
        Pool { name, vec: Vec::with_capacity(capacity) }
    }

    pub fn new(name: &'static str) -> Pool<T> {
        Pool { name, vec: Vec::new() }
    }

    pub fn next_id(&self) -> Id {
        Id::from(self.vec.len())
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn add(&mut self, t: T) -> Id {
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

    pub fn add_list(&mut self, items: impl Iterator<Item = T>) -> PoolListHandle {
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

        PoolListHandle { index, count }
    }

    pub fn get(&self, index: Id) -> &T {
        let index: usize = index.into();
        &self.vec[index]
    }

    pub fn get_mut(&mut self, index: Id) -> &mut T {
        let index: usize = index.into();
        &mut self.vec[index]
    }

    pub fn get_list(&self, handle: PoolListHandle) -> &[T] {
        let start: usize = handle.index.into();
        let end = start + handle.count as usize;
        &self.vec[start..end]
    }

    pub fn get_list_mut(&mut self, handle: PoolListHandle) -> &mut [T] {
        let start: usize = handle.index.into();
        let end = start + handle.count as usize;
        &mut self.vec[start..end]
    }
}

#[cfg(test)]
mod test {
    use super::Pool;
    #[test]
    fn single() {
        let mut pool = Pool::new("single");
        let handle = pool.add(42);
        assert_eq!(*pool.get(handle), 42);
    }

    #[test]
    fn list() {
        let mut pool = Pool::new("list");
        let handle = pool.add_list([1, 2, 3].iter().copied());
        assert_eq!(pool.get_list(handle), &[1, 2, 3]);
    }

    #[test]
    fn mutate_list() {
        let mut pool = Pool::new("mutate_list");
        let handle = pool.add_list([1, 2, 3].iter().copied());
        pool.get_list_mut(handle)[1] = 42;
        assert_eq!(pool.get_list(handle), &[1, 42, 3]);
    }
}
