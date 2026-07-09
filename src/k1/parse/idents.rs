use std::fmt::{Display, Formatter};

use string_interner::{Symbol, backend::StringBackend};

use crate::kmem::MSlice;
use crate::kmem::Mem;
use crate::parse::AstSlice;
use crate::parse::ParsedProgram;
use crate::{impl_copy_if_small, lex::SpanId};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct StringId(string_interner::symbol::SymbolU32);

impl StringId {
    pub fn forged() -> StringId {
        StringId(string_interner::symbol::SymbolU32::try_from_usize(1).unwrap())
    }
    pub fn as_usize(&self) -> usize {
        self.0.to_usize()
    }
    /// Inverse of `as_usize`
    pub fn from_usize(v: usize) -> StringId {
        StringId(string_interner::symbol::SymbolU32::try_from_usize(v).unwrap())
    }
}

impl Ord for StringId {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl PartialOrd for StringId {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl From<StringId> for usize {
    fn from(value: StringId) -> Self {
        value.0.to_usize()
    }
}

impl Display for StringId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.to_usize())
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
    pub get_ref: StringId,
    pub iter: StringId,
    pub it_index: StringId,
    pub as_: StringId,
    pub list: StringId,
    pub withCapacity: StringId,
    pub dest: StringId,
    pub block_expr_val: StringId,
    pub optelse_lhs: StringId,
    pub lambda_env_var_name: StringId,
    pub env: StringId,
    pub fn_ptr: StringId,
    pub env_ptr: StringId,
    pub asterisk: StringId,
    pub bang: StringId,
    pub payload: StringId,
    pub try_: StringId,
    pub try_value: StringId,
    pub if_target: StringId,
    pub to_dyn: StringId,
    pub to_static: StringId,
    pub from_static: StringId,
    pub filename: StringId,
    pub line: StringId,
    pub equals: StringId,
    pub tag: StringId,
    pub value: StringId,
    pub MODULE_INFO: StringId,
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
    pub StringBuilder: StringId,
    pub builder: StringId,
    pub bitwise: StringId,
    pub arena_tmp: StringId,
    pub t: StringId,
    pub phony: StringId,
    pub some: StringId,
    pub with: StringId,
    pub return_: StringId,
    pub break_: StringId,
    pub continue_: StringId,
    pub testCompile: StringId,
    pub writef: StringId,
    pub writelnf: StringId,
    pub stringf: StringId,
    pub v: StringId,
    pub subject: StringId,
    pub fmtargs: StringId,
}

#[allow(non_snake_case)]
pub(crate) struct BuiltinFunctions {
    pub List_withCapacity: QIdent,
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
    pub bitwise_and: QIdent,
    pub bitwise_or: QIdent,
    pub bitwise_xor: QIdent,
    pub bitwise_shl: QIdent,
    pub bitwise_shr: QIdent,
}

// We use the default StringInterner, which uses a contiguous string as its backend
// and u32 symbols
pub struct IdentPool {
    intern_pool: string_interner::StringInterner<StringBackend, fxhash::FxBuildHasher>,
    /// b for builtins
    pub(crate) b: BuiltinIdents,
    /// f for functions
    pub(crate) f: BuiltinFunctions,
}
impl IdentPool {
    pub fn intern(&mut self, s: impl AsRef<str>) -> StringId {
        let s = self.intern_pool.get_or_intern(&s);
        StringId(s)
    }
    pub fn lookup(&self, s: impl AsRef<str>) -> Option<StringId> {
        self.intern_pool.get(&s).map(StringId)
    }
    pub fn get_string(&self, id: StringId) -> &str {
        self.intern_pool.resolve(id.0).expect("failed to resolve identifier")
    }

    pub fn len(&self) -> usize {
        self.intern_pool.len()
    }

    #[allow(non_snake_case)]
    pub fn make(mem: &mut Mem<ParsedProgram>) -> Self {
        let mut pool = string_interner::StringInterner::with_capacity_and_hasher(
            65536,
            fxhash::FxBuildHasher::default(),
        );

        macro_rules! intern {
            ($name: expr) => {
                StringId(pool.get_or_intern_static($name))
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
            _self: intern!("_self"),
            self_: intern!("self"),
            it: intern!("it"),
            string: intern!("string"),
            len: intern!("len"),
            get: intern!("get"),
            get_ref: intern!("get-ref"),
            iter: intern!("iter"),
            it_index: intern!("it-index"),
            as_: intern!("as"),
            list: intern!("list"),
            withCapacity: intern!("withCapacity"),
            dest: intern!("dest"),
            block_expr_val: intern!("block-expr-val"),
            optelse_lhs: intern!("optelse_lhs"),
            lambda_env_var_name: intern!("__lambda_env"),
            env: intern!("env"),
            fn_ptr: intern!("fn_ptr"),
            env_ptr: intern!("env_ptr"),
            asterisk: intern!("*"),
            bang: intern!("!"),
            payload: intern!("payload"),
            try_: intern!("try"),
            try_value: intern!("try_value"),
            if_target: intern!("if_target"),
            to_dyn: intern!("to-dyn"),
            to_static: intern!("to-static"),
            from_static: intern!("from-static"),
            filename: intern!("filename"),
            line: intern!("line"),
            equals: intern!("equals"),
            tag: intern!("tag"),
            value: intern!("value"),
            MODULE_INFO: intern!("MODULE_INFO"),
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
            pre: intern!("pre"),
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
            iterator: intern!("iterator"),
            iterable: intern!("iterable"),
            next: intern!("next"),
            param_0: intern!("param_0"),
            param_1: intern!("param_1"),
            param_2: intern!("param_2"),
            param_3: intern!("param_3"),
            param_4: intern!("param_4"),
            param_5: intern!("param_5"),
            param_6: intern!("param_6"),
            param_7: intern!("param_7"),
            param_8: intern!("param_8"),
            StringBuilder: intern!("StringBuilder"),
            builder: intern!("builder"),
            bitwise: intern!("bitwise"),
            arena_tmp: intern!("arena-tmp"),
            t: intern!("t"),
            phony: intern!("phony"),
            some: intern!("some"),
            with: intern!("with"),
            return_: intern!("return"),
            break_: intern!("break"),
            continue_: intern!("continue"),
            testCompile: intern!("testCompile"),
            writef: intern!("writef"),
            writelnf: intern!("writelnf"),
            stringf: intern!("stringf"),
            v: intern!("v"),
            subject: intern!("subject"),
            fmtargs: intern!("fmtargs"),
        };

        macro_rules! make_fn {
            ($path: expr, $name: expr) => {{ QIdent { path: $path, name: $name, name_span: SpanId::NONE } }};
        }

        let path_core_list = intern_path!(b.core, b.list);
        let List_withCapacity = make_fn!(path_core_list, b.withCapacity);
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

        let path_stringbuilder = intern_path!(b.core, b.StringBuilder);
        let StringBuilder_new: QIdent = make_fn!(path_stringbuilder, intern!("new"));
        let StringBuilder_build_tmp: QIdent = make_fn!(path_stringbuilder, intern!("build-tmp"));

        let path_core_bitwise = intern_path!(b.core, b.bitwise);
        let bitwise_and = make_fn!(path_core_bitwise, intern!("bit-and"));
        let bitwise_or = make_fn!(path_core_bitwise, intern!("bit-or"));
        let bitwise_xor = make_fn!(path_core_bitwise, intern!("xor"));
        let bitwise_shl = make_fn!(path_core_bitwise, intern!("shift-left"));
        let bitwise_shr = make_fn!(path_core_bitwise, intern!("shift-right"));

        let f = BuiltinFunctions {
            List_withCapacity,
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
            bitwise_and,
            bitwise_or,
            bitwise_xor,
            bitwise_shl,
            bitwise_shr,
        };

        Self { intern_pool: pool, b, f }
    }
}
