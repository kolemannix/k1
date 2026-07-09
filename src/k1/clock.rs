//! Fast monotonic clock for profiling. `raw()` returns ticks from the
//! cheapest monotonic source on each platform; `elapsed_nanos` converts a
//! tick delta to nanoseconds using the exact scale the OS reports, so no
//! runtime calibration is needed (that's only required for raw rdtsc,
//! which we never read directly).

pub struct Clock {
    /// nanos = ticks * numer / denom
    numer: u64,
    denom: u64,
}

impl Clock {
    #[cfg(target_vendor = "apple")]
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let mut info = MachTimebaseInfo { numer: 0, denom: 0 };
        unsafe { mach_timebase_info(&mut info) };
        Clock { numer: info.numer as u64, denom: info.denom as u64 }
    }

    #[cfg(target_os = "linux")]
    pub fn new() -> Self {
        // CLOCK_MONOTONIC already returns nanoseconds (the kernel scales
        // the TSC in the vDSO).
        Clock { numer: 1, denom: 1 }
    }

    #[cfg(windows)]
    pub fn new() -> Self {
        let mut freq: i64 = 0;
        unsafe { QueryPerformanceFrequency(&mut freq) };
        Clock { numer: 1_000_000_000, denom: freq as u64 }
    }

    #[cfg(target_vendor = "apple")]
    #[inline]
    pub fn raw(&self) -> u64 {
        unsafe { mach_absolute_time() }
    }

    #[cfg(target_os = "linux")]
    #[inline]
    pub fn raw(&self) -> u64 {
        let mut ts = libc::timespec { tv_sec: 0, tv_nsec: 0 };
        unsafe { libc::clock_gettime(libc::CLOCK_MONOTONIC, &mut ts) };
        ts.tv_sec as u64 * 1_000_000_000 + ts.tv_nsec as u64
    }

    #[cfg(windows)]
    #[inline]
    pub fn raw(&self) -> u64 {
        let mut count: i64 = 0;
        unsafe { QueryPerformanceCounter(&mut count) };
        count as u64
    }

    #[inline]
    pub fn elapsed_nanos(&self, since: u64) -> u64 {
        let ticks = self.raw().saturating_sub(since);
        if self.numer == self.denom {
            ticks
        } else {
            (ticks as u128 * self.numer as u128 / self.denom as u128) as u64
        }
    }

    pub fn elapsed_ms(&self, since: u64) -> u64 {
        self.elapsed_nanos(since) / 1_000_000
    }
}

#[cfg(target_vendor = "apple")]
#[repr(C)]
struct MachTimebaseInfo {
    numer: u32,
    denom: u32,
}

#[cfg(target_vendor = "apple")]
unsafe extern "C" {
    fn mach_absolute_time() -> u64;
    fn mach_timebase_info(info: *mut MachTimebaseInfo) -> i32;
}

#[cfg(windows)]
#[link(name = "kernel32")]
unsafe extern "system" {
    fn QueryPerformanceCounter(count: *mut i64) -> i32;
    fn QueryPerformanceFrequency(freq: *mut i64) -> i32;
}

#[cfg(test)]
mod tests {
    use super::Clock;

    #[test]
    fn elapsed_is_sane() {
        let clock = Clock::new();
        let start = clock.raw();
        std::thread::sleep(std::time::Duration::from_millis(5));
        let elapsed = clock.elapsed_nanos(start);
        assert!(elapsed >= 4_000_000, "elapsed {elapsed}ns < 4ms");
        assert!(elapsed < 1_000_000_000, "elapsed {elapsed}ns > 1s");
    }
}
