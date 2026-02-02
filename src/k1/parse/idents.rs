use std::fmt::{Display, Formatter};

use string_interner::{Symbol, backend::StringBackend};

use crate::{
    impl_copy_if_small,
    lex::SpanId,
    nz_u32_id,
    pool::{SliceHandle, VPool},
};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Ident(string_interner::symbol::SymbolU32);

impl Ident {
    pub fn forged() -> Ident {
        Ident(string_interner::symbol::SymbolU32::try_from_usize(1).unwrap())
    }
}

impl Ord for Ident {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl From<Ident> for usize {
    fn from(value: Ident) -> Self {
        value.0.to_usize()
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.to_usize())
    }
}

nz_u32_id!(IdentSliceId);
pub type IdentSlice = SliceHandle<IdentSliceId>;

#[derive(Debug, Clone)]
pub struct QIdent {
    pub path: IdentSlice,
    pub name: Ident,
    pub span: SpanId,
}
impl_copy_if_small!(16, QIdent);
impl QIdent {
    pub fn naked(name: Ident, span: SpanId) -> QIdent {
        QIdent { path: IdentSlice::empty(), name, span }
    }
    pub fn with_span(&self, span: SpanId) -> QIdent {
        QIdent { span, ..*self }
    }
}

#[allow(non_snake_case)]
pub(crate) struct BuiltinIdents {
    pub main: Ident,
    pub next: Ident,
    pub _self: Ident,
    pub self_: Ident,
    pub it: Ident,
    pub string: Ident,
    pub len: Ident,
    pub get: Ident,
    pub get_ref: Ident,
    pub iter: Ident,
    pub itIndex: Ident,
    pub as_: Ident,
    pub list: Ident,
    pub withCapacity: Ident,
    pub dest: Ident,
    pub block_expr_val: Ident,
    pub optelse_lhs: Ident,
    pub lambda_env_var_name: Ident,
    pub env: Ident,
    pub fn_ptr: Ident,
    pub env_ptr: Ident,
    pub asterisk: Ident,
    pub bang: Ident,
    pub payload: Ident,
    pub try_: Ident,
    pub try_value: Ident,
    pub if_target: Ident,
    pub toRef: Ident,
    pub toDyn: Ident,
    pub to_static: Ident,
    pub from_static: Ident,
    pub filename: Ident,
    pub line: Ident,
    pub equals: Ident,
    pub tag: Ident,
    pub variantName: Ident,
    pub MODULE_INFO: Ident,
    pub root_module_name: Ident,
    pub core: Ident,
    pub std: Ident,
    pub k1: Ident,
    pub types: Ident,
    pub type_schema: Ident,
    pub int_kind: Ident,
    pub int_value: Ident,
    pub layout: Ident,
    pub array: Ident,
    pub to_mut: Ident,
    pub un_mut: Ident,
    pub pre: Ident,
    pub iterator: Ident,
    pub iterable: Ident,
    pub opt: Ident,
    pub buffer: Ident,
    pub set: Ident,
    pub mem: Ident,
    pub sys: Ident,
    pub libc: Ident,
    pub view: Ident,
    pub Equals: Ident,
    pub add: Ident,
    pub sub: Ident,
    pub mul: Ident,
    pub div: Ident,
    pub rem: Ident,
    scalar_cmp: Ident,
    lt: Ident,
    le: Ident,
    gt: Ident,
    ge: Ident,

    pub param_0: Ident,
    pub param_1: Ident,
    pub param_2: Ident,
    pub param_3: Ident,
    pub param_4: Ident,
    pub param_5: Ident,
    pub param_6: Ident,
    pub param_7: Ident,
    pub param_8: Ident,
    pub StringBuilder: Ident,
    pub bitwise: Ident,
    pub arena: Ident,
    pub t: Ident,
    pub phony: Ident,
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
    pub core_Print_printTo: QIdent,
    pub buffer__allocate: QIdent,
    pub buffer_set: QIdent,
    pub Array_set: QIdent,
    pub mem_zeroed: QIdent,
    pub view_wrapBuffer: QIdent,
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
    pub StringBuilder_buildTmp: QIdent,
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
    pub slices: VPool<Ident, IdentSliceId>,
    /// b for builtins
    pub(crate) b: BuiltinIdents,
    /// f for functions
    pub(crate) f: BuiltinFunctions,
}
impl IdentPool {
    pub fn intern(&mut self, s: impl AsRef<str>) -> Ident {
        let s = self.intern_pool.get_or_intern(&s);
        Ident(s)
    }
    pub fn get(&self, s: impl AsRef<str>) -> Option<Ident> {
        self.intern_pool.get(&s).map(Ident)
    }
    pub fn get_name(&self, id: Ident) -> &str {
        self.intern_pool.resolve(id.0).expect("failed to resolve identifier")
    }

    pub fn len(&self) -> usize {
        self.intern_pool.len()
    }

    #[allow(non_snake_case)]
    pub fn make() -> Self {
        let mut slices: VPool<Ident, IdentSliceId> = VPool::make_with_hint("ident_slices", 8192);
        let mut pool = string_interner::StringInterner::with_capacity_and_hasher(
            65536,
            fxhash::FxBuildHasher::default(),
        );

        macro_rules! intern {
            ($name: expr) => {
                Ident(pool.get_or_intern_static($name))
            };
        }

        let b = BuiltinIdents {
            main: intern!("main"),
            _self: intern!("_self"),
            self_: intern!("self"),
            it: intern!("it"),
            string: intern!("string"),
            len: intern!("len"),
            get: intern!("get"),
            get_ref: intern!("get-ref"),
            iter: intern!("iter"),
            itIndex: intern!("itIndex"),
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
            toRef: intern!("toRef"),
            toDyn: intern!("toDyn"),
            to_static: intern!("to-static"),
            from_static: intern!("from-static"),
            filename: intern!("filename"),
            line: intern!("line"),
            equals: intern!("equals"),
            tag: intern!("tag"),
            variantName: intern!("variantName"),
            MODULE_INFO: intern!("MODULE_INFO"),
            root_module_name: intern!("_root"),
            core: intern!("core"),
            std: intern!("std"),
            k1: intern!("k1"),
            types: intern!("types"),
            type_schema: intern!("type-schema"),
            int_kind: intern!("int-kind"),
            int_value: intern!("int-value"),
            layout: intern!("layout"),
            array: intern!("array"),
            to_mut: intern!("to-mut"),
            un_mut: intern!("un-mut"),
            pre: intern!("pre"),
            opt: intern!("opt"),
            buffer: intern!("buffer"),
            set: intern!("set"),
            mem: intern!("mem"),
            sys: intern!("sys"),
            libc: intern!("libc"),
            view: intern!("view"),
            Equals: intern!("equals"),
            add: intern!("add"),
            sub: intern!("sub"),
            mul: intern!("mul"),
            div: intern!("div"),
            rem: intern!("rem"),
            scalar_cmp: intern!("scalar-cmp"),
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
            bitwise: intern!("bitwise"),
            arena: intern!("arena"),
            t: intern!("t"),
            phony: intern!("phony"),
        };

        macro_rules! make_fn {
            ($path: expr, $name: expr) => {
                QIdent { path: slices.add_slice_copy($path), name: $name, span: SpanId::NONE }
            };
        }

        let List_withCapacity = make_fn!(&[b.core, b.list], b.withCapacity);
        let List_push = make_fn!(&[b.core, b.list], intern!("push"));
        let Iterator_next = make_fn!(&[b.iterator], b.next);

        let Iterable_iterator = make_fn!(&[b.iterable], intern!("iterator"));


        let bool_negated = make_fn!(&[intern!("bool")], intern!("negated"));

        let core_crash = make_fn!(&[b.core], intern!("crash"));
        let core_crashBounds = make_fn!(&[b.core], intern!("crash-bounds"));
        let core_discard = make_fn!(&[b.core], intern!("discard"));

        let core_Print_printTo = make_fn!(&[b.core, intern!("print")], intern!("printTo"));

        let try__is_ok: QIdent = make_fn!(&[b.try_], intern!("is-ok"));
        let try__get_value: QIdent = make_fn!(&[b.try_], intern!("get-value"));
        let try__get_error: QIdent = make_fn!(&[b.try_], intern!("get-error"));

        let buffer__allocate: QIdent = make_fn!(&[b.core, b.buffer], intern!("_allocate"));
        let buffer_set: QIdent = make_fn!(&[b.core, b.buffer], intern!("set"));

        let array__set: QIdent = make_fn!(&[b.array], intern!("set"));

        let mem_zeroed: QIdent = make_fn!(&[b.mem], intern!("zeroed"));

        let View_wrapBuffer: QIdent = make_fn!(&[b.core, b.view], intern!("wrapBuffer"));

        let equals__equals: QIdent = make_fn!(&[b.core, b.Equals], b.equals);

        let add__add: QIdent = make_fn!(&[b.add], b.add);
        let sub__sub: QIdent = make_fn!(&[b.sub], b.sub);
        let mul__mul: QIdent = make_fn!(&[b.mul], b.mul);
        let div__div: QIdent = make_fn!(&[b.div], b.div);
        let rem__rem: QIdent = make_fn!(&[b.rem], b.rem);

        let ScalarCmp_lt: QIdent = make_fn!(&[b.scalar_cmp], b.lt);
        let ScalarCmp_le: QIdent = make_fn!(&[b.scalar_cmp], b.le);
        let ScalarCmp_gt: QIdent = make_fn!(&[b.scalar_cmp], b.gt);
        let ScalarCmp_ge: QIdent = make_fn!(&[b.scalar_cmp], b.ge);

        let StringBuilder_new: QIdent = make_fn!(&[b.core, b.StringBuilder], intern!("new"));
        let StringBuilder_buildTmp: QIdent =
            make_fn!(&[b.core, b.StringBuilder], intern!("buildTmp"));

        let bitwise_and = make_fn!(&[b.core, b.bitwise], intern!("bit-and"));
        let bitwise_or = make_fn!(&[b.core, b.bitwise], intern!("bit-or"));
        let bitwise_xor = make_fn!(&[b.core, b.bitwise], intern!("xor"));
        let bitwise_shl = make_fn!(&[b.core, b.bitwise], intern!("shift-left"));
        let bitwise_shr = make_fn!(&[b.core, b.bitwise], intern!("shift-right"));

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
            core_Print_printTo,
            buffer__allocate,
            buffer_set,
            Array_set: array__set,
            mem_zeroed,
            view_wrapBuffer: View_wrapBuffer,
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
            StringBuilder_buildTmp,
            bitwise_and,
            bitwise_or,
            bitwise_xor,
            bitwise_shl,
            bitwise_shr,
        };

        Self { intern_pool: pool, slices, b, f }
    }
}
