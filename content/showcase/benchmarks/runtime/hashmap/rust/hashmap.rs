use std::collections::HashMap;

struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.0 = x;
        x
    }
}

fn main() {
    let n: u64 = 5_000_000;
    let mut m: HashMap<u64, u64> = HashMap::new();
    let mut keys = Rng(0x9E3779B97F4A7C15);
    for i in 0..n {
        m.insert(keys.next(), i);
    }
    let mut hits = Rng(0x9E3779B97F4A7C15);
    let mut misses = Rng(0x2545F4914F6CDD1D);
    let mut found = 0u64;
    let mut sum = 0u64;
    for i in 0..n {
        let hit_key = hits.next();
        let miss_key = misses.next();
        let key = if i % 2 == 0 { hit_key } else { miss_key };
        if let Some(&v) = m.get(&key) {
            found += 1;
            sum += v;
        }
    }
    println!("inserted: {} found: {} sum: {}", m.len(), found, sum);
}
