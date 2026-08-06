// Copyright (c) 2026 knix
// All rights reserved.

//! Compiler-internal paths are canonical, absolute, UTF-8 strings using the
//! host's separator; [canonicalize] is the only intake and the only place
//! non-UTF-8 is rejected. Joins are named by destination: [join_buf] for OS
//! sinks, [join_tmp] for scratch-arena strings, [join_id] for interned paths.

use std::fmt;
use std::path::{MAIN_SEPARATOR, Path, PathBuf, is_separator};

use crate::kmem::{MStr, Mem};
use crate::parse::{IdentPool, StringId};

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

/// A join base: something that already is a whole path
pub trait Base {
    fn as_str<'a>(&'a self, idents: &'a IdentPool) -> &'a str;
}
impl Base for &str {
    fn as_str<'a>(&'a self, _: &'a IdentPool) -> &'a str {
        self
    }
}
impl Base for StringId {
    fn as_str<'a>(&'a self, idents: &'a IdentPool) -> &'a str {
        idents.get_string(*self)
    }
}

/// A path segment; never begins or ends with a separator. The joiner inserts
/// MAIN_SEPARATOR between base and segment and between tuple elements.
pub trait Seg {
    fn write(&self, idents: &IdentPool, out: &mut dyn fmt::Write) -> fmt::Result;
    fn len_hint(&self, idents: &IdentPool) -> usize;
}
impl Seg for &str {
    fn write(&self, _: &IdentPool, out: &mut dyn fmt::Write) -> fmt::Result {
        out.write_str(self)
    }
    fn len_hint(&self, _: &IdentPool) -> usize {
        self.len()
    }
}
impl Seg for StringId {
    fn write(&self, idents: &IdentPool, out: &mut dyn fmt::Write) -> fmt::Result {
        out.write_str(idents.get_string(*self))
    }
    fn len_hint(&self, idents: &IdentPool) -> usize {
        idents.get_string(*self).len()
    }
}
impl Seg for fmt::Arguments<'_> {
    fn write(&self, _: &IdentPool, out: &mut dyn fmt::Write) -> fmt::Result {
        out.write_fmt(*self)
    }
    fn len_hint(&self, _: &IdentPool) -> usize {
        self.as_str().map_or(24, str::len)
    }
}
impl<A: Seg, B: Seg> Seg for (A, B) {
    fn write(&self, idents: &IdentPool, out: &mut dyn fmt::Write) -> fmt::Result {
        self.0.write(idents, out)?;
        out.write_char(MAIN_SEPARATOR)?;
        self.1.write(idents, out)
    }
    fn len_hint(&self, idents: &IdentPool) -> usize {
        self.0.len_hint(idents) + 1 + self.1.len_hint(idents)
    }
}

fn write_join(
    idents: &IdentPool,
    base: &str,
    seg: &impl Seg,
    out: &mut dyn fmt::Write,
) -> fmt::Result {
    out.write_str(base)?;
    if !base.ends_with(is_separator) {
        out.write_char(MAIN_SEPARATOR)?;
    }
    seg.write(idents, out)
}

/// Owned join for OS sinks (Command args, paths handed across thread or
/// process boundaries); the only join visible outside the crate
pub fn join_buf(idents: &IdentPool, base: impl Base, seg: impl Seg) -> PathBuf {
    let base = base.as_str(idents);
    let mut s = String::with_capacity(base.len() + 1 + seg.len_hint(idents));
    write_join(idents, base, &seg, &mut s).unwrap();
    PathBuf::from(s)
}

/// Scratch join; lives until the arena's next reset
pub(crate) fn join_tmp<Tag>(
    mem: &mut Mem<Tag>,
    idents: &IdentPool,
    base: impl Base,
    seg: impl Seg,
) -> MStr<Tag> {
    mem.format_with(&(), &(), |w, _, _| write_join(idents, base.as_str(idents), &seg, w))
}

/// Interned join; the scratch bytes are reclaimed before returning
pub(crate) fn join_id<Tag>(
    idents: &IdentPool,
    tmp: &mut Mem<Tag>,
    base: impl Base,
    seg: impl Seg,
) -> StringId {
    let mark = tmp.mark();
    let s = join_tmp(tmp, idents, base, seg);
    let id = idents.intern(s.as_str());
    tmp.reset_to(mark);
    id
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
    use crate::parse::ParsedProgram;

    #[test]
    fn slicing() {
        let sep = MAIN_SEPARATOR;
        let p = format!("{sep}a{sep}b{sep}c.k1");
        assert_eq!(file_name(&p), "c.k1");
        assert_eq!(file_stem(&p), "c");
        assert_eq!(parent(&p), format!("{sep}a{sep}b"));
        assert_eq!(parent(&format!("{sep}a")), format!("{sep}"));
        assert_eq!(file_stem(".hidden"), ".hidden");
    }

    #[test]
    fn joins() {
        let sep = MAIN_SEPARATOR;
        let mut mem: Mem<ParsedProgram> = Mem::make();
        let idents = IdentPool::make(&mut mem);
        let mut tmp: Mem<()> = Mem::make();

        let base = format!("{sep}a{sep}b");
        let base_id = idents.intern(&base);
        let seg_id = idents.intern("c.k1");
        let joined = format!("{sep}a{sep}b{sep}c.k1");

        assert_eq!(join_buf(&idents, base.as_str(), "c.k1"), Path::new(&joined));
        assert_eq!(join_buf(&idents, base_id, seg_id), Path::new(&joined));
        // A base ending in a separator gets no second one
        let root = format!("{sep}");
        assert_eq!(join_buf(&idents, root.as_str(), "x"), Path::new(&format!("{sep}x")));
        // format_args and tuple segments
        assert_eq!(
            join_buf(&idents, base_id, ("sub", format_args!("mod{}", 7))),
            Path::new(&format!("{sep}a{sep}b{sep}sub{sep}mod7"))
        );

        let t = join_tmp(&mut tmp, &idents, base.as_str(), seg_id);
        assert_eq!(t.as_str(), joined);

        let cursor_before = tmp.cursor();
        let id1 = join_id(&idents, &mut tmp, base_id, "c.k1");
        let id2 = join_id(&idents, &mut tmp, base.as_str(), seg_id);
        assert_eq!(id1, id2);
        assert_eq!(idents.get_string(id1), joined);
        assert_eq!(tmp.cursor(), cursor_before, "join_id must reclaim its scratch bytes");
    }
}
