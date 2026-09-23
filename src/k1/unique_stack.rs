use ahash::HashSetExt;
use fxhash::FxHashSet;
use std::hash::Hash;

/// LIFO work stack whose keys are unique while queued: a repeated push of a
/// queued key is dropped, keeping the first push's value.
pub struct UniqueStack<K, V = ()> {
    entries: Vec<(K, V)>,
    queued: FxHashSet<K>,
}

impl<K: Eq + Hash + Copy, V> UniqueStack<K, V> {
    pub fn new() -> Self {
        Self { entries: Vec::new(), queued: FxHashSet::new() }
    }

    pub fn push(&mut self, key: K, value: V) {
        if self.queued.insert(key) {
            self.entries.push((key, value));
        }
    }

    pub fn pop(&mut self) -> Option<(K, V)> {
        let (key, value) = self.entries.pop()?;
        self.queued.remove(&key);
        Some((key, value))
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}
