pub mod codegen_llvm;
pub mod compiler;
pub mod lex;
pub mod parse;
mod pool;
mod strings;
pub mod typer;
pub mod vm;

pub const KILOBYTE: usize = 1024 * 1024;
pub const MEGABYTE: usize = KILOBYTE * 1024;

fn nzu32_increment(n: u32) -> std::num::NonZeroU32 {
    // Safety: If you add one to a u32 it'll never be zero
    unsafe { std::num::NonZeroU32::new_unchecked(n + 1) }
}
