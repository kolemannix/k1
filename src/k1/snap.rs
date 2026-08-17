// Copyright (c) 2026 knix
// All rights reserved.
//! Snapshotting program memory for incremental and cached compilation

use std::mem::size_of;

pub const SNAP_MAGIC: [u8; 8] = *b"K1SNAP02";

pub struct SnapWriter {
    pub buf: Vec<u8>,
}

impl SnapWriter {
    pub fn new() -> SnapWriter {
        let mut w = SnapWriter { buf: Vec::with_capacity(1 << 25) };
        w.buf.extend_from_slice(&SNAP_MAGIC);
        w.write_str(crate::BUILD_ID);
        w
    }

    #[inline]
    pub fn write_raw(&mut self, bytes: &[u8]) {
        self.buf.extend_from_slice(bytes);
    }

    #[inline]
    pub fn write_u32(&mut self, v: u32) {
        self.write_raw(&v.to_le_bytes());
    }

    #[inline]
    pub fn write_u64(&mut self, v: u64) {
        self.write_raw(&v.to_le_bytes());
    }

    #[inline]
    pub fn write_len(&mut self, v: usize) {
        self.write_u64(v as u64);
    }

    pub fn write_t<T: Copy>(&mut self, t: &T) {
        let bytes =
            unsafe { std::slice::from_raw_parts(t as *const T as *const u8, size_of::<T>()) };
        self.write_raw(bytes);
    }

    pub fn write_slice<T: Copy>(&mut self, ts: &[T]) {
        self.write_len(ts.len());
        let bytes =
            unsafe { std::slice::from_raw_parts(ts.as_ptr() as *const u8, size_of_val(ts)) };
        self.write_raw(bytes);
    }

    pub fn write_str(&mut self, s: &str) {
        self.write_slice(s.as_bytes());
    }

    pub fn write_section(&mut self, name: &str) {
        self.write_u32(0x5EC7_1071);
        self.write_u32(fxhash::hash32(name.as_bytes()));
    }

    pub fn sorted_entries<I, T, F>(&mut self, entries: I, mut enc: F)
    where
        I: Iterator<Item = T>,
        F: FnMut(&mut SnapWriter, T),
    {
        let mut scratch = SnapWriter { buf: Vec::new() };
        let mut ranges: Vec<(usize, usize)> = Vec::new();
        for e in entries {
            let start = scratch.buf.len();
            enc(&mut scratch, e);
            ranges.push((start, scratch.buf.len()));
        }
        ranges.sort_unstable_by(|a, b| scratch.buf[a.0..a.1].cmp(&scratch.buf[b.0..b.1]));
        self.write_len(ranges.len());
        for (start, end) in ranges {
            self.write_raw(&scratch.buf[start..end]);
        }
    }
}

pub fn write_map_snap<K: Copy, V: Copy, S>(
    w: &mut SnapWriter,
    map: &std::collections::HashMap<K, V, S>,
) {
    w.sorted_entries(map.iter(), |w, (k, v)| {
        w.write_t(k);
        w.write_t(v);
    });
}

pub fn restore_map_snap<K, V, S>(r: &mut SnapReader) -> std::collections::HashMap<K, V, S>
where
    K: Copy + Eq + std::hash::Hash,
    V: Copy,
    S: std::hash::BuildHasher + Default,
{
    restore_map_with(r, |r| r.read_t())
}

pub fn snap_map_with<K: Copy, V, S>(
    w: &mut SnapWriter,
    map: &std::collections::HashMap<K, V, S>,
    mut enc: impl FnMut(&mut SnapWriter, &V),
) {
    w.sorted_entries(map.iter(), |w, (k, v)| {
        w.write_t(k);
        enc(w, v);
    });
}

pub fn restore_map_with<K, V, S>(
    r: &mut SnapReader,
    mut dec: impl FnMut(&mut SnapReader) -> V,
) -> std::collections::HashMap<K, V, S>
where
    K: Copy + Eq + std::hash::Hash,
    S: std::hash::BuildHasher + Default,
{
    let n = r.read_len();
    let mut map = std::collections::HashMap::with_capacity_and_hasher(n, S::default());
    for _ in 0..n {
        let k: K = r.read_t();
        let v = dec(r);
        map.insert(k, v);
    }
    map
}

pub struct SnapReader<'a> {
    buf: &'a [u8],
    pos: usize,
}

impl<'a> SnapReader<'a> {
    pub fn new(buf: &'a [u8]) -> Result<SnapReader<'a>, String> {
        let mut r = SnapReader { buf, pos: 0 };
        let magic = r.take(SNAP_MAGIC.len());
        if magic != SNAP_MAGIC {
            return Err("snapshot magic mismatch".to_string());
        }
        let build_id = r.str();
        if build_id != crate::BUILD_ID {
            return Err(format!(
                "snapshot from build {build_id}, this is {}: invalid",
                crate::BUILD_ID
            ));
        }
        Ok(r)
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn is_done(&self) -> bool {
        self.pos == self.buf.len()
    }

    #[track_caller]
    pub fn take(&mut self, n: usize) -> &'a [u8] {
        let end = self.pos.checked_add(n).unwrap();
        assert!(end <= self.buf.len(), "snapshot truncated: need {n} bytes at {}", self.pos);
        let bytes = &self.buf[self.pos..end];
        self.pos = end;
        bytes
    }

    pub fn read_u32(&mut self) -> u32 {
        u32::from_le_bytes(self.take(4).try_into().unwrap())
    }

    pub fn read_u64(&mut self) -> u64 {
        u64::from_le_bytes(self.take(8).try_into().unwrap())
    }

    #[track_caller]
    pub fn read_len(&mut self) -> usize {
        self.read_u64() as usize
    }

    pub fn read_t<T: Copy>(&mut self) -> T {
        let bytes = self.take(size_of::<T>());
        unsafe { std::ptr::read_unaligned(bytes.as_ptr() as *const T) }
    }

    /// Borrow of the raw element bytes; caller copies into its pool/arena.
    pub fn read_slice_bytes<T: Copy>(&mut self) -> (&'a [u8], usize) {
        let count = self.read_len();
        let bytes = self.take(count * size_of::<T>());
        (bytes, count)
    }

    pub fn read_vec<T: Copy>(&mut self) -> Vec<T> {
        let (bytes, count) = self.read_slice_bytes::<T>();
        let mut v: Vec<T> = Vec::with_capacity(count);
        unsafe {
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), v.as_mut_ptr() as *mut u8, bytes.len());
            v.set_len(count);
        }
        v
    }

    pub fn str(&mut self) -> &'a str {
        let (bytes, _) = self.read_slice_bytes::<u8>();
        std::str::from_utf8(bytes).expect("snapshot string not utf-8")
    }

    pub fn string(&mut self) -> String {
        self.str().to_string()
    }

    #[track_caller]
    pub fn section(&mut self, name: &str) {
        let tag = self.read_u32();
        assert_eq!(tag, 0x5EC7_1071, "snapshot out of sync before section '{name}'");
        let h = self.read_u32();
        assert_eq!(
            h,
            fxhash::hash32(name.as_bytes()),
            "snapshot section mismatch: expected '{name}'"
        );
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InputsHash(pub u128);

impl InputsHash {
    /// Two independently-salted SipHashes give 128 bits; a false cache hit
    /// would need a double collision
    pub fn add(self, parts: &[&[u8]]) -> InputsHash {
        use std::hash::Hasher;
        let mut halves = [0u64; 2];
        for (salt, half) in halves.iter_mut().enumerate() {
            let mut h = std::collections::hash_map::DefaultHasher::new();
            h.write_u64(salt as u64);
            h.write_u128(self.0);
            for p in parts {
                h.write(p);
            }
            *half = h.finish();
        }
        InputsHash((halves[0] as u128) << 64 | halves[1] as u128)
    }

    pub fn add_module_header(
        self,
        name: &str,
        root_dir: &str,
        root_filename: &str,
        root_hash: u64,
    ) -> InputsHash {
        self.add(&[
            name.as_bytes(),
            root_dir.as_bytes(),
            root_filename.as_bytes(),
            &root_hash.to_le_bytes(),
        ])
    }

    pub fn add_module_sources<S: AsRef<str>>(
        self,
        name: &str,
        sources: impl Iterator<Item = (S, u64)>,
    ) -> InputsHash {
        let mut hash = self.add(&[name.as_bytes()]);
        for (path, content_hash) in sources {
            hash = hash.add(&[path.as_ref().as_bytes(), &content_hash.to_le_bytes()]);
        }
        hash
    }

    fn filename(&self) -> String {
        format!("{:032x}.snap", self.0)
    }
}

pub const CACHE_DIR_NAME: &str = "cache";
const CACHE_MAX_ENTRIES: usize = 64;

pub fn cache_load(cache_dir: &std::path::Path, hash: InputsHash) -> Option<memmap2::Mmap> {
    let path = cache_dir.join(hash.filename());
    let file = std::fs::File::open(&path).ok()?;
    let mmap = unsafe { memmap2::Mmap::map(&file) }.ok()?;
    let _ = mmap.advise(memmap2::Advice::Sequential);
    touch(&path);
    Some(mmap)
}

fn touch(path: &std::path::Path) {
    if let Ok(f) = std::fs::OpenOptions::new().append(true).open(path) {
        let _ = f.set_modified(std::time::SystemTime::now());
    }
}

pub fn cache_load_text(cache_dir: &std::path::Path, filename: &str) -> Option<String> {
    let path = cache_dir.join(filename);
    let text = std::fs::read_to_string(&path).ok()?;
    touch(&path);
    Some(text)
}

pub fn cache_store_text(cache_dir: &std::path::Path, filename: &str, text: &str) {
    if std::fs::create_dir_all(cache_dir).is_err() {
        return;
    }
    let tmp = cache_dir.join(format!("{filename}.tmp.{}", std::process::id()));
    if std::fs::write(&tmp, text).is_err() {
        let _ = std::fs::remove_file(&tmp);
        return;
    }
    let _ = std::fs::rename(&tmp, cache_dir.join(filename));
}

pub fn cache_exists(cache_dir: &std::path::Path, hash: InputsHash) -> bool {
    cache_dir.join(hash.filename()).exists()
}

pub fn cache_store(cache_dir: &std::path::Path, hash: InputsHash, bytes: &[u8]) {
    if std::fs::create_dir_all(cache_dir).is_err() {
        return;
    }
    let tmp = cache_dir.join(format!("{:032x}.tmp.{}", hash.0, std::process::id()));
    if std::fs::write(&tmp, bytes).is_err() {
        let _ = std::fs::remove_file(&tmp);
        return;
    }
    let _ = std::fs::rename(&tmp, cache_dir.join(hash.filename()));
    evict(cache_dir);
}

fn evict(cache_dir: &std::path::Path) {
    let Ok(entries) = std::fs::read_dir(cache_dir) else { return };
    let mut snaps: Vec<(std::time::SystemTime, std::path::PathBuf)> = vec![];
    for entry in entries {
        let Ok(e) = entry else { continue };
        #[allow(clippy::nonminimal_bool)]
        if !e.path().extension().is_some_and(|x| x == "snap") {
            continue;
        }
        let Ok(metadata) = e.metadata() else { continue };
        let Ok(mtime) = metadata.modified() else { continue };
        snaps.push((mtime, e.path()));
    }
    if snaps.len() <= CACHE_MAX_ENTRIES {
        return;
    }
    snaps.sort_by_key(|(mtime, _)| *mtime);
    for (_, path) in &snaps[..snaps.len() - CACHE_MAX_ENTRIES] {
        let _ = std::fs::remove_file(path);
    }
}

pub fn assert_identical(first: &[u8], second: &[u8], what: &str) {
    if first == second {
        return;
    }
    let n = first.len().min(second.len());
    let diff_at = (0..n).find(|&i| first[i] != second[i]).unwrap_or(n);
    panic!(
        "{what}: snapshot roundtrip mismatch at byte {diff_at} (lens {} vs {})",
        first.len(),
        second.len()
    );
}
