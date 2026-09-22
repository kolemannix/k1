#![allow(long_running_const_eval)]

const B: usize = 65536;
const fn crc_table() -> [u32; 256] {
    let mut table = [0u32; 256];
    let mut i = 0;
    while i < 256 {
        let mut c = i as u32;
        let mut k = 0;
        while k < 8 {
            c = if c & 1 == 1 { 0xEDB88320 ^ (c >> 1) } else { c >> 1 };
            k += 1;
        }
        table[i] = c;
        i += 1;
    }
    table
}

const fn random_bytes() -> [u8; B] {
    let mut bytes = [0u8; B];
    let mut x: u32 = 2463534242;
    let mut i = 0;
    while i < B {
        x ^= x << 13;
        x ^= x >> 17;
        x ^= x << 5;
        bytes[i] = x as u8;
        i += 1;
    }
    bytes
}

const fn crc32(table: &[u32; 256], bytes: &[u8]) -> u32 {
    let mut c = 0xFFFFFFFFu32;
    let mut i = 0;
    while i < bytes.len() {
        c = table[((c ^ bytes[i] as u32) & 0xFF) as usize] ^ (c >> 8);
        i += 1;
    }
    c ^ 0xFFFFFFFF
}

const fn checksum() -> u32 { crc32(&crc_table(), &random_bytes()) }

fn main() {
    #[cfg(comptime)]
    let sum = { const S: u32 = checksum(); S };
    #[cfg(not(comptime))]
    let sum = checksum();
    println!("{sum}");
}
