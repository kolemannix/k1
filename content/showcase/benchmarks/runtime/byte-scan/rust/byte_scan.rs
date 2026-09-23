const SIZE: usize = 256 * 1024 * 1024;
const REPEAT: usize = 8;
const NEWLINE_STRIDE: usize = 4093;
const DELIM_STRIDE: usize = 1_000_003;

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

fn fill(buf: &mut [u8]) {
    let mut rng = Rng(0x2545F4914F6CDD1D);
    for word in buf.chunks_exact_mut(8) {
        word.copy_from_slice(&((rng.next() & 0x1f1f1f1f1f1f1f1f) | 0x4040404040404040).to_le_bytes());
    }
    let mut i = NEWLINE_STRIDE;
    while i < buf.len() {
        buf[i] = b'\n';
        i += NEWLINE_STRIDE;
    }
    let marks = [b',', b':', b'"'];
    let mut d = DELIM_STRIDE;
    let mut which = 0;
    while d < buf.len() {
        buf[d] = marks[which];
        which = (which + 1) % 3;
        d += DELIM_STRIDE;
    }
}

fn count_newlines(data: &[u8]) -> i64 {
    let mut count = 0;
    let mut from = 0;
    while let Some(p) = data[from..].iter().position(|&b| b == b'\n') {
        count += 1;
        from += p + 1;
    }
    count
}

fn scan_delims(data: &[u8]) -> (i64, i64, i64) {
    let (mut found, mut index_sum, mut which_sum) = (0, 0, 0);
    let mut from = 0;
    while let Some(p) = data[from..].iter().position(|&b| matches!(b, b',' | b':' | b'"')) {
        let index = from + p;
        found += 1;
        index_sum += index as i64;
        which_sum += match data[index] {
            b',' => 0,
            b':' => 1,
            _ => 2,
        };
        from = index + 1;
    }
    (found, index_sum, which_sum)
}

fn main() {
    let mut buf = vec![0u8; SIZE];
    fill(&mut buf);
    let (mut absent, mut newlines, mut delims, mut index_sum, mut which_sum) = (0, 0, 0, 0, 0);
    for _ in 0..REPEAT {
        if buf.contains(&0) {
            absent += 1;
        }
        newlines += count_newlines(&buf);
        let (f, i, w) = scan_delims(&buf);
        delims += f;
        index_sum += i;
        which_sum += w;
    }
    println!("absent: {} newlines: {} delims: {} index-sum: {} which-sum: {}", absent, newlines, delims, index_sum, which_sum);
}
