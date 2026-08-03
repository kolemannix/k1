// Copyright (c) 2026 knix
// All rights reserved.

//! Compiler-internal paths are canonical, absolute, UTF-8 strings using the
//! host's separator; [canonicalize] is the only intake and the only place
//! non-UTF-8 is rejected.

use std::path::{MAIN_SEPARATOR, Path, is_separator};

pub fn canonicalize(path: impl AsRef<Path>) -> anyhow::Result<String> {
    let path = path.as_ref();
    let canonical = path
        .canonicalize()
        .map_err(|e| anyhow::anyhow!("Failed to resolve path '{}': {e}", path.display()))?;
    canonical
        .into_os_string()
        .into_string()
        .map_err(|s| anyhow::anyhow!("Path is not valid UTF-8: {}", s.to_string_lossy()))
}

pub fn join(base: &str, seg: &str) -> String {
    let mut s = String::with_capacity(base.len() + 1 + seg.len());
    s.push_str(base);
    if !base.ends_with(is_separator) {
        s.push(MAIN_SEPARATOR);
    }
    s.push_str(seg);
    s
}

/// Path separators are ASCII on every supported host, so byte offsets around
/// them are char boundaries
pub fn file_name(path: &str) -> &str {
    match path.rfind(is_separator) {
        Some(i) => &path[i + 1..],
        None => path,
    }
}

pub fn file_stem(path: &str) -> &str {
    let name = file_name(path);
    match name.rfind('.') {
        Some(0) | None => name,
        Some(i) => &name[..i],
    }
}

pub fn parent(path: &str) -> &str {
    match path.rfind(is_separator) {
        Some(0) => &path[..1],
        Some(i) => &path[..i],
        None => "",
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn slicing() {
        let sep = MAIN_SEPARATOR;
        let p = format!("{sep}a{sep}b{sep}c.k1");
        assert_eq!(file_name(&p), "c.k1");
        assert_eq!(file_stem(&p), "c");
        assert_eq!(parent(&p), format!("{sep}a{sep}b"));
        assert_eq!(parent(&format!("{sep}a")), format!("{sep}"));
        assert_eq!(file_stem(".hidden"), ".hidden");
        assert_eq!(join(&p, "d"), format!("{sep}a{sep}b{sep}c.k1{sep}d"));
    }
}
