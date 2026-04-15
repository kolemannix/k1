use std::ptr::NonNull;

/// A lifetime-erased mutable reference. Semantically equivalent to `&'a mut T`
/// but without the borrow checker tracking the lifetime.
///
/// Safety: caller must ensure the pointee outlives all uses of this handle.
#[derive(Debug, Clone, Copy)]
pub struct RawRef<T>(NonNull<T>);

impl<T> RawRef<T> {
    pub fn from_ref(r: &T) -> Self {
        Self(NonNull::from_ref(r))
    }
    pub fn from_mut(r: &mut T) -> Self {
        Self(NonNull::from_mut(r))
    }

    pub fn from_ptr(r: *mut T) -> Self {
        if r.is_null() {
            panic!()
        };
        Self(unsafe { NonNull::new_unchecked(r) })
    }

    pub fn as_mut<'a>(&mut self) -> &'a mut T {
        unsafe { self.0.as_mut() }
    }

    pub fn as_ref<'a>(&self) -> &'a T {
        unsafe { self.0.as_ref() }
    }

    pub fn as_ptr(&self) -> *mut T {
        self.0.as_ptr()
    }

    pub fn map<U>(&self, f: impl FnOnce(&T) -> &U) -> RawRef<U> {
        RawRef::from_ptr(f(self.as_ref()) as *const U as *mut U)
    }
}

impl<T> std::ops::Deref for RawRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T> std::ops::DerefMut for RawRef<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}

// Safe because we're just holding an address — same as a raw pointer.
unsafe impl<T: Send> Send for RawRef<T> {}
unsafe impl<T: Sync> Sync for RawRef<T> {}
