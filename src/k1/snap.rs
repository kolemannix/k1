// Copyright (c) 2026 knix
// All rights reserved.
//! Snapshotting program memory for incremental and cached compilation

use std::mem::size_of;

pub const SNAP_MAGIC: [u8; 8] = *b"K1SNAP15";

const BLOB_COMPRESS_MIN: usize = 1 << 16;
const BLOB_CHUNK: usize = 1 << 20;
const SNAP_RESERVE_BYTES: usize = 16 << 30;

pub struct BlobHeader {
    pub raw_len: usize,
    compressed_len: usize,
}

fn blob_worker_count(chunks: usize) -> usize {
    let cpus = std::thread::available_parallelism().map_or(1, |n| n.get());
    chunks.clamp(1, cpus)
}

pub struct SnapWriter {
    mmap: memmap2::MmapMut,
    len: usize,
    file: Option<std::fs::File>,
}

pub struct SnapBytes {
    mmap: memmap2::MmapMut,
    len: usize,
}

impl std::ops::Deref for SnapBytes {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        &self.mmap[..self.len]
    }
}

impl SnapWriter {
    pub fn new() -> SnapWriter {
        let mut w = SnapWriter::reserve(SNAP_RESERVE_BYTES);
        w.write_raw(&SNAP_MAGIC);
        w.write_str(crate::BUILD_ID);
        w
    }

    fn reserve(bytes: usize) -> SnapWriter {
        let mmap = memmap2::MmapMut::map_anon(bytes).expect("failed to reserve snapshot buffer");
        SnapWriter { mmap, len: 0, file: None }
    }

    /// Serializes straight into the destination mmapped file
    fn new_file(path: &std::path::Path) -> std::io::Result<SnapWriter> {
        let file =
            std::fs::OpenOptions::new().read(true).write(true).create_new(true).open(path)?;
        let mapped = (|| {
            file.set_len(SNAP_RESERVE_BYTES as u64)?;
            unsafe { memmap2::MmapMut::map_mut(&file) }
        })();
        let mmap = match mapped {
            Ok(mmap) => mmap,
            Err(e) => {
                let _ = std::fs::remove_file(path);
                return Err(e);
            }
        };
        let mut w = SnapWriter { mmap, len: 0, file: Some(file) };
        // The magic bytes stay zeroed until `finish_file` to protect from torn writes
        w.write_raw(&[0u8; SNAP_MAGIC.len()]);
        w.write_str(crate::BUILD_ID);
        Ok(w)
    }

    fn finish_file(self) -> std::io::Result<()> {
        let SnapWriter { mmap, len, file } = self;
        let file = file.expect("finish_file on an anonymous SnapWriter");
        drop(mmap);
        file.set_len(len as u64)?;
        std::os::unix::fs::FileExt::write_at(&file, &SNAP_MAGIC, 0)?;
        Ok(())
    }

    pub fn finish(self) -> SnapBytes {
        SnapBytes { mmap: self.mmap, len: self.len }
    }

    #[inline]
    pub fn write_raw(&mut self, bytes: &[u8]) {
        self.mmap[self.len..self.len + bytes.len()].copy_from_slice(bytes);
        self.len += bytes.len();
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

    /// Blob = (raw_len, compressed_len, payload).
    /// compressed_len 0 = raw payload (small blobs)
    /// Otherwise the payload is 1MB chunks
    /// each lz4-compressed independently and prefixed by its own compressed_len
    /// (0 = that chunk stored raw)
    pub fn write_blob(&mut self, bytes: &[u8]) {
        let header_pos = self.len;
        self.write_len(bytes.len());
        self.write_len(0);
        if bytes.len() < BLOB_COMPRESS_MIN {
            self.write_raw(bytes);
            return;
        }
        let nchunks = bytes.len().div_ceil(BLOB_CHUNK);
        let max_out = lz4_flex::block::get_maximum_output_size(BLOB_CHUNK);
        let body = self.len;
        // Compressed sizes aren't known until compression runs, so workers
        // compress at a fixed stride directly in the output, and a serial pass
        // packs the chunks down in place. A packed chunk takes at most
        // 8 + BLOB_CHUNK bytes against a stride of max_out (> BLOB_CHUNK + 8),
        // so packing never overwrites a slot that hasn't been read yet; the +8
        // keeps chunk 0's header off its own slot
        let stride_base = body + 8;
        let mut compressed_lens = vec![0usize; nchunks];
        let workers = blob_worker_count(nchunks);
        let per_worker = nchunks.div_ceil(workers);
        std::thread::scope(|s| {
            let mut out_rest: &mut [u8] =
                &mut self.mmap[stride_base..stride_base + nchunks * max_out];
            let mut lens_rest: &mut [usize] = &mut compressed_lens;
            for w in 0..workers {
                let start = w * per_worker;
                if start >= nchunks {
                    break;
                }
                let count = per_worker.min(nchunks - start);
                let (out_part, rest) = std::mem::take(&mut out_rest).split_at_mut(count * max_out);
                out_rest = rest;
                let (lens_part, rest) = std::mem::take(&mut lens_rest).split_at_mut(count);
                lens_rest = rest;
                s.spawn(move || {
                    for i in 0..count {
                        let chunk_start = (start + i) * BLOB_CHUNK;
                        let chunk_end = (chunk_start + BLOB_CHUNK).min(bytes.len());
                        let out = &mut out_part[i * max_out..(i + 1) * max_out];
                        lens_part[i] =
                            lz4_flex::block::compress_into(&bytes[chunk_start..chunk_end], out)
                                .expect("lz4 compress failed");
                    }
                });
            }
        });
        for chunk_index in 0..nchunks {
            let chunk_start = chunk_index * BLOB_CHUNK;
            let chunk_end = (chunk_start + BLOB_CHUNK).min(bytes.len());
            let compressed_len = compressed_lens[chunk_index];
            if compressed_len < chunk_end - chunk_start {
                self.write_len(compressed_len);
                let src = stride_base + chunk_index * max_out;
                self.mmap.copy_within(src..src + compressed_len, self.len);
                self.len += compressed_len;
            } else {
                self.write_len(0);
                self.write_raw(&bytes[chunk_start..chunk_end]);
            }
        }
        let payload_len = self.len - body;
        self.mmap[header_pos + 8..header_pos + 16]
            .copy_from_slice(&(payload_len as u64).to_le_bytes());
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
        let mut scratch = SnapWriter::reserve(1 << 30);
        let mut ranges: Vec<(usize, usize)> = Vec::new();
        for e in entries {
            let start = scratch.len;
            enc(&mut scratch, e);
            ranges.push((start, scratch.len));
        }
        let bytes = &scratch.mmap[..scratch.len];
        ranges.sort_unstable_by(|a, b| bytes[a.0..a.1].cmp(&bytes[b.0..b.1]));
        self.write_len(ranges.len());
        for (start, end) in ranges {
            self.write_raw(&scratch.mmap[start..end]);
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
    mut decode: impl FnMut(&mut SnapReader) -> V,
) -> std::collections::HashMap<K, V, S>
where
    K: Copy + Eq + std::hash::Hash,
    S: std::hash::BuildHasher + Default,
{
    let n = r.read_len();
    let mut map = std::collections::HashMap::with_capacity_and_hasher(n, S::default());
    for _ in 0..n {
        let k: K = r.read_t();
        let v = decode(r);
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

    pub fn read_blob_header(&mut self) -> BlobHeader {
        BlobHeader { raw_len: self.read_len(), compressed_len: self.read_len() }
    }

    #[track_caller]
    pub fn read_blob_body(&mut self, header: BlobHeader, dst: &mut [u8]) {
        assert_eq!(dst.len(), header.raw_len, "snapshot blob destination size mismatch");
        if header.compressed_len == 0 {
            dst.copy_from_slice(self.take(header.raw_len));
            return;
        }
        let payload = self.take(header.compressed_len);
        let nchunks = header.raw_len.div_ceil(BLOB_CHUNK);
        let mut chunks: Vec<(&[u8], bool)> = Vec::with_capacity(nchunks);
        let mut pos = 0usize;
        for chunk_index in 0..nchunks {
            let raw_chunk_len = BLOB_CHUNK.min(header.raw_len - chunk_index * BLOB_CHUNK);
            let compressed_len =
                u64::from_le_bytes(payload[pos..pos + 8].try_into().unwrap()) as usize;
            pos += 8;
            if compressed_len == 0 {
                chunks.push((&payload[pos..pos + raw_chunk_len], false));
                pos += raw_chunk_len;
            } else {
                chunks.push((&payload[pos..pos + compressed_len], true));
                pos += compressed_len;
            }
        }
        assert_eq!(pos, payload.len(), "snapshot blob payload out of sync");
        let workers = blob_worker_count(nchunks);
        let per_worker = nchunks.div_ceil(workers);
        std::thread::scope(|s| {
            let mut dst_rest: &mut [u8] = dst;
            let mut chunks_rest: &[(&[u8], bool)] = &chunks;
            for w in 0..workers {
                let start = w * per_worker;
                if start >= nchunks {
                    break;
                }
                let count = per_worker.min(nchunks - start);
                let (chunk_part, rest) = chunks_rest.split_at(count);
                chunks_rest = rest;
                let dst_take = (count * BLOB_CHUNK).min(dst_rest.len());
                let (dst_part, rest) = std::mem::take(&mut dst_rest).split_at_mut(dst_take);
                dst_rest = rest;
                s.spawn(move || {
                    let mut d = dst_part;
                    for (src, compressed) in chunk_part {
                        let chunk_take = BLOB_CHUNK.min(d.len());
                        let (chunk_dst, rest) = std::mem::take(&mut d).split_at_mut(chunk_take);
                        d = rest;
                        if *compressed {
                            let n = lz4_flex::block::decompress_into(src, chunk_dst)
                                .expect("snapshot blob failed to decompress");
                            assert_eq!(
                                n,
                                chunk_dst.len(),
                                "snapshot blob decompressed to wrong size"
                            );
                        } else {
                            chunk_dst.copy_from_slice(src);
                        }
                    }
                });
            }
        });
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

    pub fn add_module_header(self, name: &str, root_path: &str, root_hash: u64) -> InputsHash {
        self.add(&[name.as_bytes(), root_path.as_bytes(), &root_hash.to_le_bytes()])
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
const CACHE_MAX_BYTES: u64 = 8 << 30;
const TMP_ORPHAN_MAX_AGE: std::time::Duration = std::time::Duration::from_secs(60 * 60);

pub fn cache_load(cache_dir: &std::path::Path, hash: InputsHash) -> Option<memmap2::Mmap> {
    let path = cache_dir.join(hash.filename());
    let file = std::fs::File::open(&path).ok()?;
    let mmap = unsafe { memmap2::Mmap::map(&file) }.ok()?;
    if mmap.len() >= SNAP_RESERVE_BYTES {
        return None;
    }
    let _ = mmap.advise(memmap2::Advice::Sequential);
    let _ = mmap.advise(memmap2::Advice::WillNeed);
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

pub fn cache_exists_entry(cache_dir: &std::path::Path, hash: InputsHash) -> bool {
    cache_dir.join(hash.filename()).exists()
}

pub fn cache_store_begin(
    cache_dir: &std::path::Path,
    hash: InputsHash,
) -> std::io::Result<SnapWriter> {
    std::fs::create_dir_all(cache_dir)?;
    SnapWriter::new_file(&cache_dir.join(hash.filename()))
}

pub fn cache_store_finish(
    cache_dir: &std::path::Path,
    hash: InputsHash,
    w: SnapWriter,
) -> std::io::Result<()> {
    let result = w.finish_file();
    if result.is_err() {
        let _ = std::fs::remove_file(cache_dir.join(hash.filename()));
    }
    cache_run_eviction(cache_dir, CACHE_MAX_BYTES);
    result
}

fn snap_file_has_magic(path: &std::path::Path) -> bool {
    let Ok(mut f) = std::fs::File::open(path) else { return false };
    let mut magic = [0u8; SNAP_MAGIC.len()];
    std::io::Read::read_exact(&mut f, &mut magic).is_ok() && magic == SNAP_MAGIC
}

/// Newest-first by mtime within a byte budget (the newest entry always
/// survives); also sweeps `.tmp.<pid>` files and magic-less partial `.snap`
/// entries orphaned by a crashed writer. Sizes count allocated blocks capped
/// at the apparent length, so an in-progress or torn sparse entry weighs only
/// its written bytes
fn cache_run_eviction(cache_dir: &std::path::Path, max_bytes: u64) {
    let Ok(entries) = std::fs::read_dir(cache_dir) else { return };
    let now = std::time::SystemTime::now();
    let mut snaps: Vec<(std::time::SystemTime, u64, std::path::PathBuf)> = vec![];
    let mut total: u64 = 0;
    for entry in entries {
        let Ok(e) = entry else { continue };
        let path = e.path();
        let Ok(metadata) = e.metadata() else { continue };
        let Ok(mtime) = metadata.modified() else { continue };
        let is_old = now.duration_since(mtime).is_ok_and(|age| age > TMP_ORPHAN_MAX_AGE);
        let name = e.file_name();
        if name.to_string_lossy().contains(".tmp.") && is_old {
            let _ = std::fs::remove_file(&path);
            continue;
        }
        if path.extension().is_some_and(|x| x == "snap") {
            if is_old && !snap_file_has_magic(&path) {
                let _ = std::fs::remove_file(&path);
                continue;
            }
            let size = metadata.len().min(std::os::unix::fs::MetadataExt::blocks(&metadata) * 512);
            total += size;
            snaps.push((mtime, size, path));
        }
    }
    if total <= max_bytes {
        return;
    }
    snaps.sort_by_key(|(mtime, _, _)| std::cmp::Reverse(*mtime));
    let mut kept: u64 = 0;
    for (i, (_, size, path)) in snaps.iter().enumerate() {
        kept += size;
        if i > 0 && kept > max_bytes {
            let _ = std::fs::remove_file(path);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn blob_roundtrip() {
        let mut compressible: Vec<u8> = Vec::with_capacity(5 << 20);
        for i in 0..((5 << 20) / 4) as u32 {
            compressible.extend_from_slice(&[(i % 7) as u8, 0, 0, 0]);
        }
        let mut incompressible: Vec<u8> = Vec::with_capacity(2 << 20);
        let mut x: u64 = 0x243f6a8885a308d3;
        for _ in 0..(2 << 20) {
            x = x.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            incompressible.push((x >> 56) as u8);
        }
        let mut mixed = incompressible.clone();
        mixed.extend_from_slice(&compressible[..2 << 20]);
        let sub_chunk = compressible[..100_000].to_vec();
        let small = b"below the compression threshold".to_vec();
        let empty: Vec<u8> = vec![];

        let blobs = [&compressible, &incompressible, &mixed, &sub_chunk, &small, &empty];
        let mut w = SnapWriter::new();
        for blob in blobs {
            w.write_blob(blob);
        }
        let bytes = w.finish();
        let raw_total: usize = blobs.iter().map(|b| b.len()).sum();
        assert!(bytes.len() < raw_total);

        let mut r = SnapReader::new(&bytes).unwrap();
        for blob in blobs {
            let header = r.read_blob_header();
            let mut dst = vec![0u8; header.raw_len];
            r.read_blob_body(header, &mut dst);
            assert_eq!(&dst, blob);
        }
        assert!(r.is_done());
    }

    #[test]
    fn eviction_byte_budget_and_tmp_orphans() {
        let dir = std::env::temp_dir().join(format!("k1_evict_test_{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let write = |name: &str, size: usize, age_secs: u64| {
            let path = dir.join(name);
            std::fs::write(&path, vec![0u8; size]).unwrap();
            let f = std::fs::OpenOptions::new().append(true).open(&path).unwrap();
            f.set_modified(std::time::SystemTime::now() - std::time::Duration::from_secs(age_secs))
                .unwrap();
        };
        write("newest.snap", 60, 0);
        write("mid.snap", 30, 10);
        write("oldest.snap", 30, 20);
        write("fresh.tmp.123", 10, 0);
        write("orphan.tmp.456", 10, 2 * 60 * 60);
        std::fs::write(dir.join("modules"), "x").unwrap();

        cache_run_eviction(&dir, 100);
        assert!(dir.join("newest.snap").exists());
        assert!(dir.join("mid.snap").exists());
        assert!(!dir.join("oldest.snap").exists());
        assert!(dir.join("fresh.tmp.123").exists());
        assert!(!dir.join("orphan.tmp.456").exists());
        assert!(dir.join("modules").exists());

        cache_run_eviction(&dir, 10);
        assert!(dir.join("newest.snap").exists());
        assert!(!dir.join("mid.snap").exists());

        std::fs::remove_dir_all(&dir).unwrap();
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
