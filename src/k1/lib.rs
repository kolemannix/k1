// Copyright (c) 2025 knix
// All rights reserved.

use smallvec::SmallVec;

pub mod codegen_llvm;
pub mod compiler;
pub mod lex;
pub mod parse;
mod pool;
mod strings;
pub mod typer;
pub mod vm;

pub const KILOBYTE: usize = 1024;
pub const MEGABYTE: usize = KILOBYTE * 1024;

pub const STACK_SIZE: usize = 10 * MEGABYTE;

pub type SV8<T> = SmallVec<[T; 8]>;
pub type SV4<T> = SmallVec<[T; 4]>;

#[macro_export]
macro_rules! nz_u32_id {
    ($name: ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $name(std::num::NonZeroU32);
        impl From<std::num::NonZeroU32> for $name {
            fn from(value: std::num::NonZeroU32) -> Self {
                Self::from_nzu32(value)
            }
        }
        impl From<$name> for std::num::NonZeroU32 {
            fn from(val: $name) -> Self {
                val.0
            }
        }
        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.0.fmt(f)
            }
        }

        impl $name {
            pub const fn as_u32(self) -> u32 {
                self.0.get()
            }

            pub const fn from_nzu32(value: NonZeroU32) -> Self {
                $name(value)
            }
            pub const fn from_u32(value: u32) -> Option<Self> {
                match NonZeroU32::new(value) {
                    None => None,
                    Some(nz_u32) => Some($name(nz_u32)),
                }
            }
            pub const ONE: Self = $name(NonZeroU32::new(1).unwrap());
            pub const PENDING: Self = $name(NonZeroU32::MAX);
        }

        impl core::ops::Add for $name {
            type Output = $name;

            fn add(self, rhs: Self) -> Self::Output {
                let add_result = self.0.get() + rhs.0.get();
                // Safety: Cannot possibly get 0 when adding two nonzero values together
                let new_result = unsafe { NonZeroU32::new_unchecked(add_result) };
                $name(new_result)
            }
        }
    };
}

fn nzu32_increment(n: u32) -> std::num::NonZeroU32 {
    // Safety: If you add one to a u32 it'll never be zero
    unsafe { std::num::NonZeroU32::new_unchecked(n + 1) }
}

#[macro_export]
macro_rules! impl_copy_if_small {
    ($size:expr, $struct_name:ident) => {
        const _: () = {
            if std::mem::size_of::<$struct_name>() > $size {
                panic!("impl_copy_if_small: Size constraint violated");
            }
        };

        impl std::marker::Copy for $struct_name where Self: Sized {}
    };
}

// A hash function with dependencies on D
pub trait DepHash<D> {
    fn dep_hash<H: std::hash::Hasher>(&self, dep: &D, state: &mut H);
}

// An Eq function with dependencies on D
pub trait DepEq<D> {
    fn dep_eq(&self, other: &Self, dep: &D) -> bool;
}
