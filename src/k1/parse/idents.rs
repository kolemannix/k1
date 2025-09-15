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

#[cfg(test)]
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
pub struct BuiltinIdents {
    pub main: Ident,
    pub next: Ident,
    pub self_: Ident,
    pub Self_: Ident,
    pub it: Ident,
    pub unit: Ident,
    pub char: Ident,
    pub string: Ident,
    pub length: Ident,
    pub len: Ident,
    pub hasValue: Ident,
    pub get: Ident,
    pub getRef: Ident,
    pub not: Ident,
    pub iter: Ident,
    pub iteree: Ident,
    pub itIndex: Ident,
    pub as_: Ident,
    pub List: Ident,
    pub withCapacity: Ident,
    pub dest: Ident,
    pub yieldDest: Ident,
    pub iteree_length: Ident,
    pub block_expr_val: Ident,
    pub optelse_lhs: Ident,
    pub list_literal: Ident,
    pub source_location_typename: Ident,
    pub lambda_env_var_name: Ident,
    pub env: Ident,
    pub fn_ptr: Ident,
    pub env_ptr: Ident,
    pub amp: Ident,
    pub asterisk: Ident,
    pub bang: Ident,
    pub sb: Ident,
    pub payload: Ident,
    pub try_: Ident,
    pub try_value: Ident,
    pub if_target: Ident,
    pub crash: Ident,
    pub toRef: Ident,
    pub toDyn: Ident,
    pub toStatic: Ident,
    pub fromStatic: Ident,
    pub filename: Ident,
    pub line: Ident,
    pub equals: Ident,
    pub tag: Ident,
    pub MODULE_INFO: Ident,
    pub root_module_name: Ident,
    pub core: Ident,
    pub std: Ident,
    pub k1: Ident,
    pub types: Ident,
    pub TypeSchema: Ident,
    pub IntKind: Ident,
    pub IntValue: Ident,
    pub Layout: Ident,
    pub Array: Ident,
    pub toMut: Ident,
    pub unMut: Ident,
    pub data: Ident,
    pub meta: Ident,
    pub pre: Ident,
    pub Iterator: Ident,
    pub Iterable: Ident,
    pub Opt: Ident,
    pub Unwrap: Ident,
    pub Try: Ident,
    pub Buffer: Ident,
    pub mem: Ident,
    pub View: Ident,
    pub Equals: Ident,
    pub Add: Ident,
    pub add: Ident,

    pub param_0: Ident,
    pub param_1: Ident,
    pub param_2: Ident,
    pub param_3: Ident,
    pub param_4: Ident,
    pub param_5: Ident,
    pub param_6: Ident,
    pub param_7: Ident,
    pub param_8: Ident,
    pub Some: Ident,
    pub None: Ident,
}

#[allow(non_snake_case)]
pub struct BuiltinFunctions {
    pub List_withCapacity: QIdent,
    pub List_push: QIdent,
    pub Iterator_next: QIdent,
    pub Iterator_sizeHint: QIdent,
    pub Iterable_iterator: QIdent,
    pub Unwrap_hasValue: QIdent,
    pub Unwrap_unwrap: QIdent,
    pub Opt_isSome: QIdent,
    pub Opt_get: QIdent,
    pub Try_isOk: QIdent,
    pub Try_getOk: QIdent,
    pub Try_getError: QIdent,
    pub bool_negated: QIdent,
    pub bool_and: QIdent,
    pub bool_or: QIdent,
    pub core_crash: QIdent,
    pub core_crashBounds: QIdent,
    pub core_discard: QIdent,
    pub core_Print_printTo: QIdent,
    pub core_string_wrapList: QIdent,
    pub Buffer__allocate: QIdent,
    pub Buffer_set: QIdent,
    pub Array_set: QIdent,
    pub mem_zeroed: QIdent,
    pub View_wrapBuffer: QIdent,
    pub Equals_equals: QIdent,
    pub Add_add: QIdent,
}

// We use the default StringInterner, which uses a contiguous string as its backend
// and u32 symbols
pub struct IdentPool {
    intern_pool: string_interner::StringInterner<StringBackend>,
    pub slices: VPool<Ident, IdentSliceId>,
    /// b for builtins
    pub b: BuiltinIdents,
    /// f for functions
    pub f: BuiltinFunctions,
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

    #[allow(non_snake_case)]
    pub fn make() -> Self {
        let mut slices: VPool<Ident, IdentSliceId> = VPool::make_with_hint("ident_slices", 8192);
        let mut pool = string_interner::StringInterner::with_capacity(65536);

        macro_rules! intern {
            ($name: expr) => {
                Ident(pool.get_or_intern_static($name))
            };
        }

        let b = BuiltinIdents {
            main: intern!("main"),
            self_: intern!("self"),
            Self_: intern!("Self"),
            it: intern!("it"),
            unit: intern!("unit"),
            char: intern!("char"),
            string: intern!("string"),
            length: intern!("length"),
            len: intern!("len"),
            hasValue: intern!("hasValue"),
            get: intern!("get"),
            getRef: intern!("getRef"),
            not: intern!("not"),
            iter: intern!("iter"),
            iteree: intern!("iteree"),
            itIndex: intern!("itIndex"),
            as_: intern!("as"),
            List: intern!("List"),
            withCapacity: intern!("withCapacity"),
            dest: intern!("dest"),
            yieldDest: intern!("yieldDest"),
            iteree_length: intern!("iteree_length"),
            block_expr_val: intern!("block_expr_val"),
            optelse_lhs: intern!("optelse_lhs"),
            list_literal: intern!("list_literal"),
            source_location_typename: intern!("SourceLocation"),
            lambda_env_var_name: intern!("__lambda_env"),
            env: intern!("env"),
            fn_ptr: intern!("fn_ptr"),
            env_ptr: intern!("env_ptr"),
            amp: intern!("&"),
            asterisk: intern!("*"),
            bang: intern!("!"),
            sb: intern!("sb"),
            payload: intern!("payload"),
            try_: intern!("try"),
            try_value: intern!("try_value"),
            if_target: intern!("if_target"),
            crash: intern!("crash"),
            toRef: intern!("toRef"),
            toDyn: intern!("toDyn"),
            toStatic: intern!("toStatic"),
            fromStatic: intern!("fromStatic"),
            filename: intern!("filename"),
            line: intern!("line"),
            equals: intern!("equals"),
            tag: intern!("tag"),
            MODULE_INFO: intern!("MODULE_INFO"),
            root_module_name: intern!("_root"),
            core: intern!("core"),
            std: intern!("std"),
            k1: intern!("k1"),
            types: intern!("types"),
            TypeSchema: intern!("TypeSchema"),
            IntKind: intern!("IntKind"),
            IntValue: intern!("IntValue"),
            Layout: intern!("Layout"),
            Array: intern!("Array"),
            toMut: intern!("toMut"),
            unMut: intern!("unMut"),
            data: intern!("data"),
            meta: intern!("meta"),
            pre: intern!("pre"),
            Opt: intern!("Opt"),
            Unwrap: intern!("Unwrap"),
            Try: intern!("Try"),
            Buffer: intern!("Buffer"),
            mem: intern!("mem"),
            View: intern!("View"),
            Equals: intern!("Equals"),
            Add: intern!("Add"),
            add: intern!("add"),
            Iterator: intern!("Iterator"),
            Iterable: intern!("Iterable"),
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
            Some: intern!("Some"),
            None: intern!("None"),
        };

        macro_rules! make_fn {
            ($path: expr, $name: expr) => {
                QIdent { path: slices.add_slice_copy($path), name: $name, span: SpanId::NONE }
            };
        }

        let List_withCapacity = make_fn!(&[b.List], b.withCapacity);
        let List_push = make_fn!(&[b.List], intern!("push"));
        let Iterator_next = make_fn!(&[b.Iterator], b.next);
        let Iterator_sizeHint = make_fn!(&[b.Iterator], intern!("sizeHint"));

        let Iterable_iterator = make_fn!(&[b.Iterable], intern!("iterator"));

        let Unwrap_hasValue = make_fn!(&[b.Unwrap], b.hasValue);
        let Unwrap_unwrap = make_fn!(&[b.Unwrap], intern!("unwrap"));

        let Opt_isSome = make_fn!(&[b.Opt], intern!("isSome"));
        let Opt_get = make_fn!(&[b.Opt], b.get);

        let bool_negated = make_fn!(&[intern!("bool")], intern!("negated"));
        let bool_and = make_fn!(&[intern!("bool")], intern!("_and"));
        let bool_or = make_fn!(&[intern!("bool")], intern!("_or"));

        let core_crash = make_fn!(&[b.core], intern!("crash"));
        let core_crashBounds = make_fn!(&[b.core], intern!("crashBounds"));
        let core_discard = make_fn!(&[b.core], intern!("discard"));

        let core_Print_printTo = make_fn!(&[b.core, intern!("Print")], intern!("printTo"));

        let core_string_wrapList = make_fn!(&[b.core, intern!("string")], intern!("wrapList"));

        let Try_isOk: QIdent = make_fn!(&[b.Try], intern!("isOk"));
        let Try_getOk: QIdent = make_fn!(&[b.Try], intern!("getOk"));
        let Try_getError: QIdent = make_fn!(&[b.Try], intern!("getError"));

        let Buffer__allocate: QIdent = make_fn!(&[b.Buffer], intern!("_allocate"));
        let Buffer_set: QIdent = make_fn!(&[b.Buffer], intern!("set"));

        let Array_set: QIdent = make_fn!(&[b.Array], intern!("set"));

        let mem_zeroed: QIdent = make_fn!(&[b.mem], intern!("zeroed"));

        let View_wrapBuffer: QIdent = make_fn!(&[b.View], intern!("wrapBuffer"));

        let Equals_equals: QIdent = make_fn!(&[b.Equals], b.equals);

        let Add_add: QIdent = make_fn!(&[b.Add], b.add);

        let f = BuiltinFunctions {
            List_withCapacity,
            List_push,
            Iterator_next,
            Iterator_sizeHint,
            Iterable_iterator,
            Unwrap_hasValue,
            Unwrap_unwrap,
            Opt_isSome,
            Opt_get,
            Try_isOk,
            Try_getOk,
            Try_getError,
            bool_negated,
            bool_and,
            bool_or,
            core_crash,
            core_crashBounds,
            core_discard,
            core_Print_printTo,
            core_string_wrapList,
            Buffer__allocate,
            Buffer_set,
            Array_set,
            mem_zeroed,
            View_wrapBuffer,
            Equals_equals,
            Add_add,
        };

        Self { intern_pool: pool, slices, b, f }
    }
}
