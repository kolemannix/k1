use std::num::NonZeroU32;

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

impl<T, Index: Into<NonZeroU32> + From<NonZeroU32>> Pool<T, Index> {
    pub fn with_capacity(name: &'static str, capacity: usize) -> Pool<T, Index> {
        Pool { name, vec: Vec::with_capacity(capacity), _index: std::marker::PhantomData }
    }

    pub fn new(name: &'static str) -> Pool<T, Index> {
        Pool { name, vec: Vec::new(), _index: std::marker::PhantomData }
    }

    pub fn next_id(&self) -> Index {
        let index = NonZeroU32::new(self.vec.len() as u32 + 1).unwrap();
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

    pub fn add_list(&mut self, items: impl Iterator<Item = T>) -> (Index, u32) {
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

        (index, count)
    }

    fn index_to_actual_index(index: Index) -> usize {
        let nz32: NonZeroU32 = index.into();
        nz32.get() as usize - 1
    }

    pub fn get(&self, index: Index) -> &T {
        let index = Self::index_to_actual_index(index);
        &self.vec[index]
    }

    pub fn get_mut(&mut self, index: Index) -> &mut T {
        let index = Self::index_to_actual_index(index);
        &mut self.vec[index]
    }

    pub fn get_list(&self, index: Index, count: u32) -> &[T] {
        let index = Self::index_to_actual_index(index);
        let end = index + count as usize;
        &self.vec[index..end]
    }

    pub fn get_list_mut(&mut self, index: Index, count: u32) -> &mut [T] {
        let index = Self::index_to_actual_index(index);
        let end = index + count as usize;
        &mut self.vec[index..end]
    }

    pub fn iter(&self) -> std::slice::Iter<T> {
        self.vec.iter()
    }
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
        let (id, count) = pool.add_list([1, 2, 3].iter().copied());
        assert_eq!(pool.get_list(id, count), &[1, 2, 3]);
    }

    #[test]
    fn mutate_list() {
        let mut pool: Pool<i32, NonZeroU32> = Pool::new("mutate_list");
        let (id, count) = pool.add_list([1, 2, 3].iter().copied());
        pool.get_list_mut(id, count)[1] = 42;
        assert_eq!(pool.get_list(id, count), &[1, 42, 3]);
    }
}
