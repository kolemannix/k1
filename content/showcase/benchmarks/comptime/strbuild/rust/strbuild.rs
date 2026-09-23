#![allow(long_running_const_eval)]

const N: usize = 10_000;
const CAP: usize = N * 8;

struct Summary { len: usize, sum: u32 }

const fn write_decimal(buf: &mut [u8; CAP], at: usize, mut v: usize) -> usize {
    let mut digits = [0u8; 20];
    let mut n = 0;
    loop {
        digits[n] = b'0' + (v % 10) as u8;
        n += 1;
        v /= 10;
        if v == 0 { break; }
    }
    let mut k = 0;
    while k < n {
        buf[at + k] = digits[n - 1 - k];
        k += 1;
    }
    n
}

const fn fnv1a(buf: &[u8; CAP], len: usize) -> u32 {
    let mut h: u32 = 2166136261;
    let mut i = 0;
    while i < len {
        h = (h ^ buf[i] as u32).wrapping_mul(16777619);
        i += 1;
    }
    h
}

const fn build() -> Summary {
    let mut buf = [0u8; CAP];
    let mut len = 0;
    let mut i = 0;
    while i < N {
        if i > 0 { buf[len] = b','; len += 1; }
        len += write_decimal(&mut buf, len, i);
        i += 1;
    }
    Summary { len, sum: fnv1a(&buf, len) }
}

fn main() {
    #[cfg(comptime)]
    let r = { const R: Summary = build(); R };
    #[cfg(not(comptime))]
    let r = build();
    println!("{} {}", r.len, r.sum);
}
