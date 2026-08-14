use crate::kmem::MSlice;
use crate::kmem::Mem;
use crate::nz_u32_id;
use crate::parse::AstSlice;
use crate::parse::ParsedProgram;
use crate::vpool::VPool;
use crate::{impl_copy_if_small, lex::SpanId};

nz_u32_id!(StringId);

impl Ord for StringId {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_u32().cmp(&other.as_u32())
    }
}

impl PartialOrd for StringId {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub type IdentSlice = AstSlice<StringId>;

#[derive(Clone, Copy)]
pub struct IdentSpanned {
    pub name: StringId,
    pub span: SpanId,
}

impl IdentSpanned {
    pub fn make(ident: StringId, span: SpanId) -> Self {
        IdentSpanned { name: ident, span }
    }
    pub fn make_anon(ident: StringId) -> Self {
        IdentSpanned { name: ident, span: SpanId::NONE }
    }
}

#[derive(Clone)]
///```txt
///A [q]ualified identifier; as in, foo/bar/baz/thing
///                                 ^^^^^^^^^^^ ^^^^^
///                                 path[3]     name
///```
pub struct QIdent {
    pub name: StringId,
    pub name_span: SpanId,
    pub path: AstSlice<IdentSpanned>,
}
impl_copy_if_small!(16, QIdent);
impl QIdent {
    pub fn naked(name: StringId, span: SpanId) -> QIdent {
        QIdent { name, name_span: span, path: MSlice::empty() }
    }
    pub fn with_span(&self, span: SpanId) -> QIdent {
        QIdent { name_span: span, ..*self }
    }
}

#[allow(non_snake_case)]
#[derive(Clone, Copy)]
pub(crate) struct BuiltinIdents {
    pub null: StringId,
    pub main: StringId,
    pub next: StringId,
    pub _self: StringId,
    pub self_: StringId,
    pub it: StringId,
    pub string: StringId,
    pub len: StringId,
    pub get: StringId,
    pub iter: StringId,
    pub it_index: StringId,
    pub as_: StringId,
    pub list: StringId,
    pub with_capacity: StringId,
    pub dest: StringId,
    pub optelse_lhs: StringId,
    pub lambda_env_var_name: StringId,
    pub env: StringId,
    pub fn_ptr: StringId,
    pub env_ptr: StringId,
    pub state: StringId,
    pub asterisk: StringId,
    pub bang: StringId,
    pub amp: StringId,
    pub payload: StringId,
    pub load: StringId,
    pub load_async: StringId,
    pub loaded_version: StringId,
    pub watch: StringId,
    pub reload: StringId,
    pub load_ns: StringId,
    pub load_ns_async: StringId,
    pub ns_version: StringId,
    pub watch_ns: StringId,
    pub try_: StringId,
    pub try_value: StringId,
    pub defer_value: StringId,
    pub if_target: StringId,
    pub to_dyn: StringId,
    pub to_static: StringId,
    pub from_static: StringId,
    pub filename: StringId,
    pub line: StringId,
    pub equals: StringId,
    pub tag: StringId,
    pub value: StringId,
    pub module: StringId,
    pub module_params: StringId,
    pub dep: StringId,
    pub add_dep: StringId,
    pub setup: StringId,
    pub setup_ctx: StringId,
    pub root_module_name: StringId,
    pub core: StringId,
    pub std: StringId,
    pub k1: StringId,
    pub types: StringId,
    pub type_schema: StringId,
    pub int_kind: StringId,
    pub int_value: StringId,
    pub float_kind: StringId,
    pub float_value: StringId,
    pub layout: StringId,
    pub source_location: StringId,
    pub array: StringId,
    pub vector: StringId,
    pub pre: StringId,
    pub iterator: StringId,
    pub iterable: StringId,
    pub opt: StringId,
    pub ordering: StringId,
    pub buffer: StringId,
    pub set: StringId,
    pub mem: StringId,
    pub sys: StringId,
    pub libc: StringId,
    pub span: StringId,
    pub add: StringId,
    pub sub: StringId,
    pub mul: StringId,
    pub div: StringId,
    pub rem: StringId,
    pub scalar_cmp: StringId,
    pub invoke: StringId,
    pub lt: StringId,
    pub le: StringId,
    pub gt: StringId,
    pub ge: StringId,

    pub param_0: StringId,
    pub param_1: StringId,
    pub param_2: StringId,
    pub param_3: StringId,
    pub param_4: StringId,
    pub param_5: StringId,
    pub param_6: StringId,
    pub param_7: StringId,
    pub param_8: StringId,
    pub string_builder: StringId,
    pub code: StringId,
    pub code_chunk: StringId,
    pub code_builder: StringId,
    pub builder: StringId,
    pub bitwise: StringId,
    pub arena_tmp: StringId,
    pub t: StringId,
    pub phony: StringId,
    pub none: StringId,
    pub some: StringId,
    pub with: StringId,
    pub return_: StringId,
    pub break_: StringId,
    pub continue_: StringId,
    pub test_compile: StringId,
    pub write: StringId,
    pub writeln: StringId,
    pub fmt: StringId,
    pub subject: StringId,
    pub fmtargs: StringId,
    pub e: StringId,
    pub comparable: StringId,
    // type-schema variant names; must match types/type-schema in types.k1
    pub char: StringId,
    pub bool: StringId,
    pub ptr: StringId,
    pub int: StringId,
    pub float: StringId,
    pub enum_: StringId,
    pub struct_: StringId,
    pub union: StringId,
    pub reference: StringId,
    pub either: StringId,
    pub other: StringId,
    pub never: StringId,
    pub function: StringId,
    pub function_pointer: StringId,
    pub base_struct: StringId,
    pub newline: StringId,
    // messages for synthesized crash calls
    pub crash_msg_no_cases: StringId,
    pub crash_msg_no_cases_exhaustive: StringId,
    pub crash_msg_array_oob: StringId,
    pub for_each: StringId,
}

#[allow(non_snake_case)]
#[derive(Clone, Copy)]
pub(crate) struct BuiltinFunctions {
    pub List_with_capacity: QIdent,
    pub List_push: QIdent,
    pub Iterator_next: QIdent,
    pub Iterable_iterator: QIdent,
    pub try__is_ok: QIdent,
    pub try__get_value: QIdent,
    pub try__get_error: QIdent,
    pub bool__negated: QIdent,
    pub core_crash: QIdent,
    pub core_crash_bounds: QIdent,
    pub core_discard: QIdent,
    pub core_print_print_to: QIdent,
    pub buffer_allocate: QIdent,
    pub buffer_set: QIdent,
    pub Array_set: QIdent,
    pub mem_zeroed: QIdent,
    pub mem_new: QIdent,
    pub span_wrapBuffer: QIdent,
    pub equals__equals: QIdent,
    pub add__add: QIdent,
    pub sub__sub: QIdent,
    pub mul__mul: QIdent,
    pub div__div: QIdent,
    pub rem__rem: QIdent,
    pub ScalarCmp_lt: QIdent,
    pub ScalarCmp_le: QIdent,
    pub ScalarCmp_gt: QIdent,
    pub ScalarCmp_ge: QIdent,
    pub StringBuilder_new: QIdent,
    pub StringBuilder_build_tmp: QIdent,
    pub CodeBuilder_new: QIdent,
    pub CodeBuilder_build: QIdent,
    pub CodeBuilder_code: QIdent,
    pub bitwise_and: QIdent,
    pub bitwise_or: QIdent,
    pub bitwise_xor: QIdent,
    pub bitwise_shl: QIdent,
    pub bitwise_shr: QIdent,
}

/// Long strings get a sampled hash: O(1) instead of hashing megabytes of
/// metaprogram output. Dedup stays exact — a hash hit is always confirmed by a
/// full content compare in `Interner::intern`/`lookup`, which must both use
/// this same function so probes and inserts agree.
fn content_hash(b: &[u8]) -> u64 {
    use std::hash::Hasher;
    let mut h = fxhash::FxHasher::default();
    if b.len() <= 256 {
        h.write(b);
    } else {
        let mid = b.len() / 2;
        h.write_usize(b.len());
        h.write(&b[..64]);
        h.write(&b[mid..mid + 64]);
        h.write(&b[b.len() - 64..]);
    }
    h.finish()
}

struct Interner {
    bytes: Mem<IdentPool>,
    entries: VPool<MSlice<u8, IdentPool>, StringId>,
    dedup: hashbrown::HashTable<StringId>,
}

impl Interner {
    fn make() -> Interner {
        Interner {
            bytes: Mem::make(),
            entries: VPool::make_with_hint("idents", 65536),
            dedup: hashbrown::HashTable::with_capacity(65536),
        }
    }

    fn get_str(
        bytes: &Mem<IdentPool>,
        entries: &VPool<MSlice<u8, IdentPool>, StringId>,
        id: StringId,
    ) -> &'static str {
        unsafe { std::str::from_utf8_unchecked(bytes.getn(*entries.get(id))) }
    }

    fn get(&self, id: StringId) -> &'static str {
        Self::get_str(&self.bytes, &self.entries, id)
    }

    fn intern(&mut self, s: &str) -> StringId {
        let hash = content_hash(s.as_bytes());
        let Interner { bytes, entries, dedup } = self;
        if let Some(id) = dedup.find(hash, |&id| Self::get_str(bytes, entries, id) == s) {
            return *id;
        }
        let id = entries.add(bytes.pushn(s.as_bytes()));
        dedup.insert_unique(hash, id, |&id| {
            content_hash(Self::get_str(bytes, entries, id).as_bytes())
        });
        id
    }

    fn lookup(&self, s: &str) -> Option<StringId> {
        let hash = content_hash(s.as_bytes());
        self.dedup.find(hash, |&id| self.get(id) == s).copied()
    }

    fn snap(&self, w: &mut crate::snap::SnapWriter) {
        self.bytes.snap(w);
        self.entries.snap(w);
    }

    fn restore(&mut self, r: &mut crate::snap::SnapReader) {
        self.bytes.restore(r);
        self.entries.restore(r);
        self.dedup.clear();
        let Interner { bytes, entries, dedup } = self;
        for (id, slice) in entries.iter_with_ids() {
            let hash = content_hash(bytes.getn(*slice));
            dedup.insert_unique(hash, id, |&id| {
                content_hash(Self::get_str(bytes, entries, id).as_bytes())
            });
        }
    }
}

pub struct IdentPool {
    pool: std::cell::RefCell<Interner>,
    /// b for builtins
    pub(crate) b: BuiltinIdents,
    /// f for functions
    pub(crate) f: BuiltinFunctions,
}
impl IdentPool {
    pub fn intern(&self, s: impl AsRef<str>) -> StringId {
        self.pool.borrow_mut().intern(s.as_ref())
    }
    pub fn lookup(&self, s: impl AsRef<str>) -> Option<StringId> {
        self.pool.borrow().lookup(s.as_ref())
    }
    pub fn get_string(&self, id: StringId) -> &'static str {
        self.pool.borrow().get(id)
    }

    pub fn snap(&self, w: &mut crate::snap::SnapWriter) {
        w.write_section("idents");
        self.pool.borrow().snap(w);
        w.write_t(&self.b);
        w.write_t(&self.f);
    }

    pub fn restore(&mut self, r: &mut crate::snap::SnapReader) {
        r.section("idents");
        self.pool.get_mut().restore(r);
        self.b = r.read_t();
        self.f = r.read_t();
    }

    pub fn len(&self) -> usize {
        self.pool.borrow().entries.len()
    }

    /// All interned strings in insertion order.
    pub fn iter(&self) -> impl Iterator<Item = (StringId, &'static str)> {
        let pool = self.pool.borrow();
        pool.entries
            .iter_with_ids()
            .map(|(id, slice)| {
                (id, unsafe { std::str::from_utf8_unchecked(pool.bytes.getn(*slice)) })
            })
            .collect::<Vec<_>>()
            .into_iter()
    }

    /// Total bytes of string content in the pool.
    pub fn content_bytes(&self) -> usize {
        self.pool.borrow().entries.iter().map(|s| s.len() as usize).sum()
    }

    #[allow(non_snake_case)]
    pub fn make(mem: &mut Mem<ParsedProgram>) -> Self {
        let mut pool = Interner::make();

        macro_rules! intern {
            ($name: expr) => {
                pool.intern($name)
            };
        }

        macro_rules! intern_path {
            ($($name: expr),*) => {
                mem.pushn(&[$(IdentSpanned::make_anon($name)),*])
            }
        }

        let b = BuiltinIdents {
            null: intern!("null"),
            main: intern!("main"),
            next: intern!("next"),
            _self: intern!("_self"),
            self_: intern!("self"),
            it: intern!("it"),
            string: intern!("string"),
            len: intern!("len"),
            get: intern!("get"),
            iter: intern!("iter"),
            it_index: intern!("it-index"),
            as_: intern!("as"),
            list: intern!("list"),
            with_capacity: intern!("with-capacity"),
            dest: intern!("dest"),
            optelse_lhs: intern!("optelse_lhs"),
            lambda_env_var_name: intern!("__lambda_env"),
            env: intern!("env"),
            fn_ptr: intern!("fn_ptr"),
            env_ptr: intern!("env_ptr"),
            state: intern!("state"),
            asterisk: intern!("*"),
            bang: intern!("!"),
            amp: intern!("&"),
            payload: intern!("payload"),
            load: intern!("load"),
            load_async: intern!("load-async"),
            loaded_version: intern!("loaded-version"),
            watch: intern!("watch"),
            reload: intern!("reload"),
            load_ns: intern!("load-ns"),
            load_ns_async: intern!("load-ns-async"),
            ns_version: intern!("ns-version"),
            watch_ns: intern!("watch-ns"),
            try_: intern!("try"),
            try_value: intern!("try_value"),
            defer_value: intern!("defer_value"),
            if_target: intern!("if_target"),
            to_dyn: intern!("to-dyn"),
            to_static: intern!("to-static"),
            from_static: intern!("from-static"),
            filename: intern!("filename"),
            line: intern!("line"),
            equals: intern!("equals"),
            tag: intern!("tag"),
            value: intern!("value"),
            module: intern!("module"),
            module_params: intern!("module-params"),
            dep: intern!("dep"),
            add_dep: intern!("add-dep-impl"),
            setup: intern!("setup"),
            setup_ctx: intern!("setup-ctx"),
            root_module_name: intern!("_root"),
            core: intern!("core"),
            std: intern!("std"),
            k1: intern!("k1"),
            types: intern!("types"),
            type_schema: intern!("type-schema"),
            int_kind: intern!("int-kind"),
            int_value: intern!("int-value"),
            float_kind: intern!("float-kind"),
            float_value: intern!("float-value"),
            layout: intern!("layout"),
            source_location: intern!("source-location"),
            array: intern!("array"),
            vector: intern!("vector"),
            pre: intern!("pre"),
            iterator: intern!("iterator"),
            iterable: intern!("iterable"),
            opt: intern!("opt"),
            ordering: intern!("ordering"),
            buffer: intern!("buffer"),
            set: intern!("set"),
            mem: intern!("mem"),
            sys: intern!("sys"),
            libc: intern!("libc"),
            span: intern!("span"),
            add: intern!("add"),
            sub: intern!("sub"),
            mul: intern!("mul"),
            div: intern!("div"),
            rem: intern!("rem"),
            scalar_cmp: intern!("scalar-cmp"),
            invoke: intern!("invoke"),
            lt: intern!("lt"),
            le: intern!("le"),
            gt: intern!("gt"),
            ge: intern!("ge"),
            param_0: intern!("param_0"),
            param_1: intern!("param_1"),
            param_2: intern!("param_2"),
            param_3: intern!("param_3"),
            param_4: intern!("param_4"),
            param_5: intern!("param_5"),
            param_6: intern!("param_6"),
            param_7: intern!("param_7"),
            param_8: intern!("param_8"),
            string_builder: intern!("string-builder"),
            code: intern!("code"),
            code_chunk: intern!("code-chunk"),
            code_builder: intern!("code-builder"),
            builder: intern!("builder"),
            bitwise: intern!("bitwise"),
            arena_tmp: intern!("arena-tmp"),
            t: intern!("t"),
            phony: intern!("phony"),
            none: intern!("none"),
            some: intern!("some"),
            with: intern!("with"),
            return_: intern!("return"),
            break_: intern!("break"),
            continue_: intern!("continue"),
            test_compile: intern!("test-compile"),
            write: intern!("write"),
            writeln: intern!("writeln"),
            fmt: intern!("fmt"),
            subject: intern!("subject"),
            fmtargs: intern!("fmtargs"),
            e: intern!("e"),
            comparable: intern!("comparable"),
            char: intern!("char"),
            bool: intern!("bool"),
            ptr: intern!("ptr"),
            int: intern!("int"),
            float: intern!("float"),
            enum_: intern!("enum"),
            struct_: intern!("struct"),
            union: intern!("union"),
            reference: intern!("reference"),
            either: intern!("either"),
            other: intern!("other"),
            never: intern!("never"),
            function: intern!("function"),
            function_pointer: intern!("function-pointer"),
            base_struct: intern!("base_struct"),
            newline: intern!("\n"),
            crash_msg_no_cases: intern!("No cases matched"),
            crash_msg_no_cases_exhaustive: intern!(
                "No cases matched but match was meant to be exhaustive. \
                Either the match subject is corrupt, or there is a compiler bug."
            ),
            crash_msg_array_oob: intern!("Array index out of bounds"),
            for_each: intern!("for-each"),
        };

        macro_rules! make_fn {
            ($path: expr, $name: expr) => {{ QIdent { path: $path, name: $name, name_span: SpanId::NONE } }};
        }

        let path_core_list = intern_path!(b.core, b.list);
        let List_with_capacity = make_fn!(path_core_list, b.with_capacity);
        let List_push = make_fn!(path_core_list, intern!("push"));

        let path_core_iterator = intern_path!(b.core, b.iterator);
        let Iterator_next = make_fn!(path_core_iterator, b.next);

        let path_core_iterable = intern_path!(b.core, b.iterable);
        let Iterable_iterator = make_fn!(path_core_iterable, intern!("iterator"));

        let path_core_bool = intern_path!(b.core, intern!("bool"));
        let bool_negated = make_fn!(path_core_bool, intern!("negated"));

        let path_core = intern_path!(b.core);
        let core_crash = make_fn!(path_core, intern!("crash"));
        let core_crashBounds = make_fn!(path_core, intern!("crash-bounds"));
        let core_discard = make_fn!(path_core, intern!("discard"));

        let path_core_print = intern_path!(b.core, intern!("print"));
        let core_print_print_to = make_fn!(path_core_print, intern!("print-to"));

        let path_try = intern_path!(b.try_);
        let try__is_ok: QIdent = make_fn!(path_try, intern!("is-ok"));
        let try__get_value: QIdent = make_fn!(path_try, intern!("get-value"));
        let try__get_error: QIdent = make_fn!(path_try, intern!("get-error"));

        let path_core_buffer = intern_path!(b.core, b.buffer);
        let buffer_allocate: QIdent = make_fn!(path_core_buffer, intern!("allocate"));
        let buffer_set: QIdent = make_fn!(path_core_buffer, intern!("set"));

        let path_array = intern_path!(b.core, b.array);
        let array__set: QIdent = make_fn!(path_array, intern!("set"));

        let path_mem = intern_path!(b.mem);
        let mem_zeroed: QIdent = make_fn!(path_mem, intern!("zeroed"));
        let mem_new: QIdent = make_fn!(path_mem, intern!("new"));

        let path_core_span = intern_path!(b.core, b.span);
        let span_wrapBuffer: QIdent = make_fn!(path_core_span, intern!("wrap-buffer"));

        let path_core_equals = intern_path!(b.core, b.equals);
        let equals__equals: QIdent = make_fn!(path_core_equals, b.equals);

        let add__add: QIdent = make_fn!(intern_path!(b.add), b.add);
        let sub__sub: QIdent = make_fn!(intern_path!(b.sub), b.sub);
        let mul__mul: QIdent = make_fn!(intern_path!(b.mul), b.mul);
        let div__div: QIdent = make_fn!(intern_path!(b.div), b.div);
        let rem__rem: QIdent = make_fn!(intern_path!(b.rem), b.rem);

        let path_scalar_cmp = intern_path!(b.scalar_cmp);
        let ScalarCmp_lt: QIdent = make_fn!(path_scalar_cmp, b.lt);
        let ScalarCmp_le: QIdent = make_fn!(path_scalar_cmp, b.le);
        let ScalarCmp_gt: QIdent = make_fn!(path_scalar_cmp, b.gt);
        let ScalarCmp_ge: QIdent = make_fn!(path_scalar_cmp, b.ge);

        let path_stringbuilder = intern_path!(b.core, b.string_builder);
        let StringBuilder_new: QIdent = make_fn!(path_stringbuilder, intern!("new"));
        let StringBuilder_build_tmp: QIdent = make_fn!(path_stringbuilder, intern!("build-tmp"));

        let path_codebuilder = intern_path!(b.core, b.code_builder);
        let CodeBuilder_new: QIdent = make_fn!(path_codebuilder, intern!("new"));
        let CodeBuilder_build: QIdent = make_fn!(path_codebuilder, intern!("build"));
        let CodeBuilder_code: QIdent = make_fn!(path_codebuilder, b.code);

        let path_core_bitwise = intern_path!(b.core, b.bitwise);
        let bitwise_and = make_fn!(path_core_bitwise, intern!("bit-and"));
        let bitwise_or = make_fn!(path_core_bitwise, intern!("bit-or"));
        let bitwise_xor = make_fn!(path_core_bitwise, intern!("xor"));
        let bitwise_shl = make_fn!(path_core_bitwise, intern!("shift-left"));
        let bitwise_shr = make_fn!(path_core_bitwise, intern!("shift-right"));

        let f = BuiltinFunctions {
            List_with_capacity,
            List_push,
            Iterator_next,
            Iterable_iterator,
            try__is_ok,
            try__get_value,
            try__get_error,
            bool__negated: bool_negated,
            core_crash,
            core_crash_bounds: core_crashBounds,
            core_discard,
            core_print_print_to,
            buffer_allocate,
            buffer_set,
            Array_set: array__set,
            mem_zeroed,
            mem_new,
            span_wrapBuffer,
            equals__equals,
            add__add,
            sub__sub,
            mul__mul,
            div__div,
            rem__rem,
            ScalarCmp_lt,
            ScalarCmp_le,
            ScalarCmp_gt,
            ScalarCmp_ge,
            StringBuilder_new,
            StringBuilder_build_tmp,
            CodeBuilder_new,
            CodeBuilder_build,
            CodeBuilder_code,
            bitwise_and,
            bitwise_or,
            bitwise_xor,
            bitwise_shl,
            bitwise_shr,
        };

        Self { pool: std::cell::RefCell::new(pool), b, f }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn make_pool() -> IdentPool {
        let mut mem = Mem::make();
        IdentPool::make(&mut mem)
    }

    #[test]
    fn intern_dedup_and_roundtrip() {
        let p = make_pool();
        let a = p.intern("hello");
        let b = p.intern("hello");
        let c = p.intern("world");
        assert_eq!(a, b);
        assert_ne!(a, c);
        assert_eq!(p.get_string(a), "hello");
        assert_eq!(p.get_string(c), "world");
        assert_eq!(p.lookup("hello"), Some(a));
        assert_eq!(p.lookup("nope"), None);
        assert_eq!(p.intern(""), p.intern(""));
    }

    #[test]
    fn builtins_resolve() {
        let p = make_pool();
        assert_eq!(p.lookup("null"), Some(p.b.null));
        assert_eq!(p.intern("for-each"), p.b.for_each);
        assert_eq!(p.get_string(p.b.self_), "self");
    }

    #[test]
    fn big_strings_use_sampled_hash_and_still_dedup() {
        let p = make_pool();
        let big_a: String = "a".repeat(100_000);
        let mut big_b = big_a.clone();
        // Differs only outside the sampled windows (head/mid/tail 64 bytes)
        big_b.replace_range(200..201, "b");

        let a1 = p.intern(&big_a);
        let a2 = p.intern(&big_a);
        let b1 = p.intern(&big_b);
        assert_eq!(a1, a2);
        assert_ne!(a1, b1);
        assert_eq!(p.get_string(a1), big_a);
        assert_eq!(p.get_string(b1), big_b);
        assert_eq!(p.lookup(&big_a), Some(a1));
    }

    #[test]
    fn pointers_stable_across_growth() {
        let p = make_pool();
        let id = p.intern("stable");
        let ptr_before = p.get_string(id).as_ptr();
        for i in 0..100_000 {
            p.intern(format!("filler_{i}"));
        }
        assert_eq!(p.get_string(id).as_ptr(), ptr_before);
    }
}
