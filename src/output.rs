pub const TRACE: bool = false;

#[macro_export]
macro_rules! trace {
    () => (println!("\n"));
    ($($arg:tt)*) => ({
        if (crate::output::TRACE) {
          println!($($arg)*);
        }
    })
}
