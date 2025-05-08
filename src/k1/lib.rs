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
