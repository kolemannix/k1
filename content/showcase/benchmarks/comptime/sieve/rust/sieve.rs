#![allow(long_running_const_eval)]

const N: usize = 10_000;
struct Summary { count: usize, last: usize }

const fn sieve() -> Summary {
    let mut composite = [false; N];
    let mut primes = [0usize; N];
    let mut count = 0;
    let mut i = 2;
    while i < N {
        if !composite[i] {
            primes[count] = i;
            count += 1;
            let mut j = i * i;
            while j < N { composite[j] = true; j += i; }
        }
        i += 1;
    }
    Summary { count, last: primes[count - 1] }
}

fn main() {
    #[cfg(comptime)]
    let r = { const R: Summary = sieve(); R };
    #[cfg(not(comptime))]
    let r = sieve();
    println!("{} {}", r.count, r.last);
}
