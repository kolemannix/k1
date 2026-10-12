use crate::SV8;

pub const C1: u64 = 0x9E37_79B9_7F4A_7C15;
pub const C2: u64 = 0xC2B2_AE3D_27D4_EB4F;

pub struct PerfectHash {
    pub bucket_bits: u32,
    pub slot_bits: u32,
    pub bucket_seeds: Vec<u16>,
    pub slot_entries: Vec<Option<usize>>,
    pub slot_keys: Vec<u64>,
}

fn bucket_of(key: u64, bucket_bits: u32) -> usize {
    (key.wrapping_mul(C1) >> (64 - bucket_bits)) as usize
}

fn slot_of(key: u64, seed: u16, slot_bits: u32) -> usize {
    ((key ^ (seed as u64).wrapping_mul(C1)).wrapping_mul(C2) >> (64 - slot_bits)) as usize
}

impl PerfectHash {
    pub fn build(keys: &[u64]) -> PerfectHash {
        let n = keys.len();
        let mut slot_bits: u32 = 1;
        while (1usize << slot_bits) * 4 < n * 5 {
            slot_bits += 1;
        }
        let mut bucket_bits: u32 = 1;
        while (1usize << bucket_bits) * 4 < n {
            bucket_bits += 1;
        }
        loop {
            let bucket_count = 1usize << bucket_bits;
            let mut buckets: Vec<SV8<usize>> = vec![SV8::new(); bucket_count];
            for (index, key) in keys.iter().enumerate() {
                buckets[bucket_of(*key, bucket_bits)].push(index);
            }
            let mut order: Vec<usize> = Vec::with_capacity(bucket_count);
            for bucket in 0..bucket_count {
                if !buckets[bucket].is_empty() {
                    order.push(bucket);
                }
            }
            order.sort_by_key(|bucket| std::cmp::Reverse(buckets[*bucket].len()));
            let mut bucket_seeds = vec![0u16; bucket_count];
            let mut slot_entries: Vec<Option<usize>> = vec![None; 1 << slot_bits];
            let mut placed_all = true;
            'bucket: for bucket in order {
                for seed in 0..=u16::MAX {
                    let mut slots: SV8<usize> = SV8::new();
                    for index in &buckets[bucket] {
                        let slot = slot_of(keys[*index], seed, slot_bits);
                        if slot_entries[slot].is_some() || slots.contains(&slot) {
                            break;
                        }
                        slots.push(slot);
                    }
                    if slots.len() == buckets[bucket].len() {
                        for (index, slot) in buckets[bucket].iter().zip(slots.iter()) {
                            slot_entries[*slot] = Some(*index);
                        }
                        bucket_seeds[bucket] = seed;
                        continue 'bucket;
                    }
                }
                placed_all = false;
                break;
            }
            if !placed_all {
                bucket_bits += 1;
                continue;
            }
            let slot_count = slot_entries.len();
            let mut hash = PerfectHash {
                bucket_bits,
                slot_bits,
                bucket_seeds,
                slot_entries,
                slot_keys: Vec::with_capacity(slot_count),
            };
            for slot in 0..slot_count {
                let key = match hash.slot_entries[slot] {
                    Some(index) => keys[index],
                    None => {
                        let mut key: u64 = 0;
                        while hash.slot(key) == slot {
                            key += 1;
                        }
                        key
                    }
                };
                hash.slot_keys.push(key);
            }
            return hash;
        }
    }

    pub fn slot(&self, key: u64) -> usize {
        slot_of(key, self.bucket_seeds[bucket_of(key, self.bucket_bits)], self.slot_bits)
    }
}

#[cfg(test)]
mod perfect_hash_test {
    use super::*;

    #[test]
    fn every_key_hits_only_its_slot() {
        for n in [0u64, 1, 2, 634, 5000] {
            let mut keys: Vec<u64> = Vec::with_capacity(n as usize);
            for i in 0..n {
                keys.push(1 + i * 17 + (i * i) % 13);
            }
            let hash = PerfectHash::build(&keys);
            for (index, key) in keys.iter().enumerate() {
                let slot = hash.slot(*key);
                assert_eq!(hash.slot_entries[slot], Some(index));
                assert_eq!(hash.slot_keys[slot], *key);
            }
            for probe in [0u64, 3, 9999, 1 << 32, u64::MAX] {
                let slot = hash.slot(probe);
                assert_eq!(hash.slot_keys[slot] == probe, keys.contains(&probe));
            }
        }
    }
}
