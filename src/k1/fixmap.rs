use ahash::HashMapExt;
use fxhash::FxHashMap;
use std::{collections::hash_map::Entry, hash::Hash};

pub struct FixMap<K, V> {
    inner: FxHashMap<K, V>,
}

impl<K, V> FixMap<K, V> {
    pub fn with_capacity(cap: usize) -> Self {
        Self { inner: FxHashMap::with_capacity(cap) }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn capacity(&self) -> usize {
        self.inner.capacity()
    }
}

impl<K: Eq + Hash, V> FixMap<K, V> {
    pub fn insert(&mut self, key: K, value: V) -> Option<V>
    where
        K: Eq + Hash,
    {
        #[cfg(debug_assertions)]
        let pre_cap = self.capacity();

        let ins = self.inner.insert(key, value);

        #[cfg(debug_assertions)]
        {
            let post_cap = self.capacity();
            if post_cap > pre_cap {
                panic!("FixMap capacity increased from {} to {}", pre_cap, post_cap)
            }
        }

        ins
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.inner.get(key)
    }

    pub fn entry(&mut self, key: K) -> Entry<K, V> {
        self.inner.entry(key)
    }
}
