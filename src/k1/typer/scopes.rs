// Copyright (c) 2026 knix
// All rights reserved.

use fxhash::FxHashMap;

use std::{fmt::Display, num::NonZeroU32};

use crate::{
    SV4, kbail, kerr,
    kmem::{Dlist, List, Mem},
    nz_u32_id,
    parse::{ParsedAbilityId, ParsedExprId, ParsedGlobalId, ParsedTypeDefnId, QIdent},
    static_assert_niched, static_assert_size,
    typer::{
        AbilityId, FunctionId, K1Result, LoopType, LsEntityKind, MemTmp, NamespaceId, StringId,
        TypeId, TypedProgram, VariableId,
    },
    vpool::VPool,
};

nz_u32_id!(ScopeId);
static_assert_niched!(ScopeId);
nz_u32_id!(ScopeEntryId);
nz_u32_id!(NsMapId);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ScopeType {
    FunctionScope,
    LambdaScope,
    LexicalBlock,
    Namespace,
    WhileLoopBody,
    LoopExprBody,
    IfBody,
    ElseBody,
    ForExpr,
    MatchArm,
    TypeDefn,
    AbilityDefn,
    AbilityImpl,
}

impl ScopeType {
    pub fn short_name(&self) -> &'static str {
        match self {
            ScopeType::FunctionScope => "fn",
            ScopeType::LambdaScope => "clos",
            ScopeType::LexicalBlock => "block",
            ScopeType::Namespace => "ns",
            ScopeType::WhileLoopBody => "while",
            ScopeType::LoopExprBody => "loop",
            ScopeType::IfBody => "if",
            ScopeType::ElseBody => "else",
            ScopeType::ForExpr => "for",
            ScopeType::MatchArm => "match_arm",
            ScopeType::TypeDefn => "type_defn",
            ScopeType::AbilityDefn => "ability_defn",
            ScopeType::AbilityImpl => "ability_impl",
        }
    }

    pub fn is_top_of_function(&self) -> bool {
        matches!(self, ScopeType::FunctionScope | ScopeType::LambdaScope)
    }

    pub fn loop_type(&self) -> Option<LoopType> {
        match self {
            ScopeType::FunctionScope => None,
            ScopeType::LambdaScope => None,
            ScopeType::LexicalBlock => None,
            ScopeType::Namespace => None,
            ScopeType::WhileLoopBody => Some(LoopType::While),
            ScopeType::LoopExprBody => Some(LoopType::Loop),
            ScopeType::IfBody => None,
            ScopeType::ElseBody => None,
            ScopeType::ForExpr => None,
            ScopeType::MatchArm => None,
            ScopeType::TypeDefn => None,
            ScopeType::AbilityDefn => None,
            ScopeType::AbilityImpl => None,
        }
    }
}

impl Display for ScopeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.short_name())
    }
}

#[derive(Clone, Copy)]
pub struct ScopeEnclosingFunctions {
    pub lambda_scope: Option<ScopeId>,
    pub function: Option<FunctionId>,
}

#[derive(Clone, Copy)]
pub struct ScopeLambdaInfo {
    pub expected_return_type: Option<TypeId>,
    // We have to store this here, instead of on the function, since no function
    // declaration exists while we're evaluating a lambda body
    pub returned_variable: Option<VariableId>,
}

#[derive(Clone, Copy)]
pub struct ScopeLoopInfo {
    pub break_type: Option<TypeId>,
    pub label: Option<StringId>,
    pub has_break: bool,
}

pub struct ScopeDefers {
    pub deferred_exprs: SV4<ParsedExprId>,
}

/// A context variable registered under an ability key. `Ambiguous` marks a key
/// claimed by more than one context param of the same function (legal to declare;
/// an implicit lookup that lands on it must fail and ask for explicit passing).
#[derive(Debug, Clone, Copy)]
pub enum ContextAbilityEntry {
    Unique(VariableId),
    Ambiguous,
}

pub mod kinds {
    pub const CONTEXT_VARIABLES: u16 = 1 << 0;
    pub const FUNCTIONS: u16 = 1 << 1;
    pub const NAMESPACES: u16 = 1 << 2;
    pub const TYPES: u16 = 1 << 3;
    pub const ABILITIES: u16 = 1 << 4;
    pub const PENDING_TYPES: u16 = 1 << 5;
    pub const PENDING_ABILITIES: u16 = 1 << 6;
    pub const TYPE_PARAM_SUBSTS: u16 = 1 << 7;
    pub const PENDING_GLOBALS: u16 = 1 << 8;
    pub const VARIABLES: u16 = 1 << 9;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Provenance {
    Defined,
    Use,
    UseExposed,
}

impl Provenance {
    pub fn is_exposed(self) -> bool {
        !matches!(self, Provenance::Use)
    }
}

fn exposed<V>(found: Option<(V, Provenance)>) -> Option<V> {
    match found {
        Some((v, p)) if p.is_exposed() => Some(v),
        _ => None,
    }
}

#[derive(Clone, Copy)]
pub struct ScopeEntry {
    /// the name for name-keyed kinds and the type/ability id for id-keyed kinds
    key: u32,
    prev_entry: Option<ScopeEntryId>,
    pub provenance: Provenance,
    payload: EntryPayload,
}
static_assert_size!(ScopeEntry, 20);

#[derive(Clone, Copy)]
enum EntryPayload {
    Variable(VariableInScope),
    Function(FunctionId),
    Type(TypeId),
    Namespace(NamespaceId),
    Ability(AbilityId),
    PendingType(ParsedTypeDefnId),
    PendingAbility(ParsedAbilityId),
    PendingGlobal(ParsedGlobalId),
    ContextByType(VariableId),
    ContextByAbility(ContextAbilityEntry),
    TypeSubst(TypeId),
}

#[derive(Clone, Copy, Default)]
pub struct NsEntry {
    function: Option<(FunctionId, Provenance)>,
    namespace: Option<(NamespaceId, Provenance)>,
    ty: NsEntryTypeValue,
    ability: NsEntryAbilityVAlue,
    global: NsEntryGlobalValue,
}
static_assert_size!(NsEntry, 40);

#[derive(Clone, Copy, Default)]
enum NsEntryTypeValue {
    #[default]
    None,
    Pending(ParsedTypeDefnId),
    Defined(TypeId, Provenance),
}

#[derive(Clone, Copy, Default)]
enum NsEntryAbilityVAlue {
    #[default]
    None,
    Pending(ParsedAbilityId),
    Defined(AbilityId, Provenance),
}

#[derive(Clone, Copy, Default)]
enum NsEntryGlobalValue {
    #[default]
    None,
    Pending(ParsedGlobalId),
    Bound(VariableInScope, Provenance),
}

pub struct Scopes {
    pub scopes: VPool<Scope, ScopeId>,
    entries: VPool<ScopeEntry, ScopeEntryId>,
    ns_maps: VPool<FxHashMap<StringId, NsEntry>, NsMapId>,
    pub lambda_info: FxHashMap<ScopeId, ScopeLambdaInfo>,
    pub loop_info: FxHashMap<ScopeId, ScopeLoopInfo>,
    pub block_defers: FxHashMap<ScopeId, ScopeDefers>,
    pub core_scope_id: ScopeId,
    pub k1_scope_id: ScopeId,
    pub mem_scope_id: ScopeId,
    pub types_scope_id: ScopeId,
    pub array_scope_id: ScopeId,
    pub vector_scope_id: ScopeId,
}

impl Scopes {
    pub fn snap(&self, w: &mut crate::snap::SnapWriter) {
        use crate::snap::{snap_map_with, write_map_snap};
        let Scopes {
            scopes,
            entries,
            ns_maps,
            lambda_info,
            loop_info,
            block_defers,
            core_scope_id,
            k1_scope_id,
            mem_scope_id,
            types_scope_id,
            array_scope_id,
            vector_scope_id,
        } = self;
        w.write_section("scopes");
        scopes.snap(w);
        entries.snap(w);
        w.write_len(ns_maps.len());
        for map in ns_maps.iter() {
            write_map_snap(w, map);
        }
        write_map_snap(w, lambda_info);
        write_map_snap(w, loop_info);
        snap_map_with(w, block_defers, |w, defers| w.write_slice(&defers.deferred_exprs));
        for id in [
            core_scope_id,
            k1_scope_id,
            mem_scope_id,
            types_scope_id,
            array_scope_id,
            vector_scope_id,
        ] {
            w.write_t(id);
        }
    }

    pub fn restore(r: &mut crate::snap::SnapReader) -> Scopes {
        use crate::snap::{restore_map_snap, restore_map_with};
        r.section("scopes");
        let mut scopes = VPool::make("scopes");
        scopes.restore(r);
        let mut entries = VPool::make("scope_entries");
        entries.restore(r);
        let mut ns_maps = VPool::make("scope_ns_maps");
        for _ in 0..r.read_len() {
            ns_maps.add(restore_map_snap(r));
        }
        Scopes {
            scopes,
            entries,
            ns_maps,
            lambda_info: restore_map_snap(r),
            loop_info: restore_map_snap(r),
            block_defers: restore_map_with(r, |r| ScopeDefers {
                deferred_exprs: SV4::from_slice(&r.read_vec::<ParsedExprId>()),
            }),
            core_scope_id: r.read_t(),
            k1_scope_id: r.read_t(),
            mem_scope_id: r.read_t(),
            types_scope_id: r.read_t(),
            array_scope_id: r.read_t(),
            vector_scope_id: r.read_t(),
        }
    }

    pub const ROOT_SCOPE_ID: ScopeId = ScopeId(NonZeroU32::new(1).unwrap());
    pub fn make() -> Self {
        let mut scopes = Scopes {
            scopes: VPool::make("scopes"),
            entries: VPool::make("scope_entries"),
            ns_maps: VPool::make("scope_ns_maps"),
            lambda_info: FxHashMap::default(),
            loop_info: FxHashMap::default(),
            block_defers: FxHashMap::default(),
            core_scope_id: ScopeId::PENDING,
            k1_scope_id: ScopeId::PENDING,
            mem_scope_id: ScopeId::PENDING,
            types_scope_id: ScopeId::PENDING,
            array_scope_id: ScopeId::PENDING,
            vector_scope_id: ScopeId::PENDING,
        };
        let id = scopes.add_scope(None, ScopeType::Namespace, ScopeOwnerId::None);
        debug_assert_eq!(id, Self::ROOT_SCOPE_ID);
        scopes
    }

    pub fn iter(&self) -> impl Iterator<Item = (ScopeId, &Scope)> {
        self.scopes.iter_ids().zip(self.scopes.iter())
    }

    #[inline]
    pub fn root_scope_id(&self) -> ScopeId {
        Self::ROOT_SCOPE_ID
    }

    fn add_scope(
        &mut self,
        parent: Option<ScopeId>,
        scope_type: ScopeType,
        owner_id: ScopeOwnerId,
    ) -> ScopeId {
        let nearest_lambda = match scope_type {
            ScopeType::LambdaScope => None, // set to its own id below
            // A lambda never appears above an ability, namespace, or type defn scope,
            // nor outside a function (no locally defined named functions)
            ScopeType::AbilityDefn
            | ScopeType::AbilityImpl
            | ScopeType::TypeDefn
            | ScopeType::Namespace
            | ScopeType::FunctionScope => None,
            _ => parent.and_then(|p| self.get_scope(p).nearest_lambda),
        };
        let ns_map = if scope_type == ScopeType::Namespace {
            Some(self.ns_maps.add(FxHashMap::default()))
        } else {
            None
        };
        let id = self.scopes.add(Scope {
            parent,
            scope_type,
            kinds: 0,
            nearest_lambda,
            owner_id,
            entries: None,
            ns_map,
        });
        if scope_type == ScopeType::LambdaScope {
            self.scopes.get_mut(id).nearest_lambda = Some(id);
        }
        id
    }

    pub fn add_sibling_scope(
        &mut self,
        sibling_scope_id: ScopeId,
        scope_type: ScopeType,
        scope_owner_id: ScopeOwnerId,
    ) -> ScopeId {
        let parent = self.get_scope(sibling_scope_id).parent.unwrap();
        self.add_child_scope(parent, scope_type, scope_owner_id)
    }

    pub fn add_child_scope(
        &mut self,
        parent_scope_id: ScopeId,
        scope_type: ScopeType,
        scope_owner_id: ScopeOwnerId,
    ) -> ScopeId {
        self.add_scope(Some(parent_scope_id), scope_type, scope_owner_id)
    }

    pub fn get_scope(&self, id: ScopeId) -> &Scope {
        self.scopes.get(id)
    }

    pub fn get_root_scope(&self) -> &Scope {
        self.get_scope(self.root_scope_id())
    }

    pub fn get_scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        self.scopes.get_mut(id)
    }

    pub fn set_scope_owner_id(&mut self, id: ScopeId, owner_id: ScopeOwnerId) {
        self.get_scope_mut(id).owner_id = owner_id;
    }

    pub fn get_scope_owner(&self, scope_id: ScopeId) -> ScopeOwnerId {
        self.get_scope(scope_id).owner_id
    }

    fn entry_find<V>(
        &self,
        head: Option<ScopeEntryId>,
        key: u32,
        sel: impl Fn(&ScopeEntry) -> Option<V>,
    ) -> Option<V> {
        let mut cur = head;
        while let Some(id) = cur {
            let entry = self.entries.get(id);
            if entry.key == key
                && let Some(v) = sel(entry)
            {
                return Some(v);
            }
            cur = entry.prev_entry;
        }
        None
    }

    fn entry_push(
        &mut self,
        scope_id: ScopeId,
        key: u32,
        payload: EntryPayload,
        provenance: Provenance,
        kind: u16,
    ) {
        let head = self.get_scope(scope_id).entries;
        let id = self.entries.add(ScopeEntry { key, prev_entry: head, provenance, payload });
        let scope = self.get_scope_mut(scope_id);
        scope.entries = Some(id);
        scope.kinds |= kind;
    }

    fn scope_entries(&self, head: Option<ScopeEntryId>) -> impl Iterator<Item = ScopeEntry> + '_ {
        std::iter::successors(head, move |id| self.entries.get(*id).prev_entry)
            .map(move |id| *self.entries.get(id))
    }

    fn ns_entry(&self, map_id: NsMapId, name: StringId) -> Option<&NsEntry> {
        self.ns_maps.get(map_id).get(&name)
    }

    fn ns_entry_mut(&mut self, map_id: NsMapId, name: StringId) -> &mut NsEntry {
        self.ns_maps.get_mut(map_id).entry(name).or_default()
    }

    /// Walk the scope chain from `scope_id` to the root, probing scopes whose
    /// `kinds` contains `kind` with `f`. Returns the first hit and its scope.
    #[inline]
    fn walk_chain<V>(
        &self,
        scope_id: ScopeId,
        kind: u16,
        f: impl Fn(ScopeId) -> Option<V>,
    ) -> Option<(V, ScopeId)> {
        let mut scope_id = scope_id;
        loop {
            let scope = self.get_scope(scope_id);
            if scope.kinds & kind != 0
                && let Some(v) = f(scope_id)
            {
                return Some((v, scope_id));
            }
            scope_id = scope.parent?;
        }
    }

    pub fn find_namespace(&self, scope: ScopeId, ident: StringId) -> Option<NamespaceId> {
        self.walk_chain(scope, kinds::NAMESPACES, |sid| self.find_namespace_local(sid, ident))
            .map(|(v, _)| v)
    }

    pub fn find_namespace_local(&self, scope_id: ScopeId, ident: StringId) -> Option<NamespaceId> {
        self.find_namespace_entry(scope_id, ident).map(|(ns, _)| ns)
    }

    pub fn find_namespace_exposed(
        &self,
        scope_id: ScopeId,
        ident: StringId,
    ) -> Option<NamespaceId> {
        exposed(self.find_namespace_entry(scope_id, ident))
    }

    fn find_namespace_entry(
        &self,
        scope_id: ScopeId,
        ident: StringId,
    ) -> Option<(NamespaceId, Provenance)> {
        let scope = self.get_scope(scope_id);
        match scope.ns_map {
            Some(m) => self.ns_entry(m, ident)?.namespace,
            None => self.entry_find(scope.entries, ident.as_u32(), |e| match e.payload {
                EntryPayload::Namespace(ns) => Some((ns, e.provenance)),
                _ => None,
            }),
        }
    }

    #[must_use]
    pub fn add_namespace(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        namespace_id: NamespaceId,
    ) -> bool {
        self.bind_namespace(scope_id, ident, namespace_id, Provenance::Defined)
    }

    fn bind_namespace(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        namespace_id: NamespaceId,
        provenance: Provenance,
    ) -> bool {
        let added = match self.get_scope(scope_id).ns_map {
            Some(m) => {
                let entry = self.ns_entry_mut(m, ident);
                if entry.namespace.is_some() {
                    false
                } else {
                    entry.namespace = Some((namespace_id, provenance));
                    true
                }
            }
            None => {
                if self.find_namespace_local(scope_id, ident).is_some() {
                    false
                } else {
                    self.entry_push(
                        scope_id,
                        ident.as_u32(),
                        EntryPayload::Namespace(namespace_id),
                        provenance,
                        kinds::NAMESPACES,
                    );
                    true
                }
            }
        };
        if added {
            self.get_scope_mut(scope_id).kinds |= kinds::NAMESPACES;
        }
        added
    }

    pub fn replace_namespace(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        namespace_id: NamespaceId,
    ) {
        match self.get_scope(scope_id).ns_map {
            Some(m) => {
                self.ns_entry_mut(m, ident).namespace = Some((namespace_id, Provenance::Defined));
            }
            None => self.entry_push(
                scope_id,
                ident.as_u32(),
                EntryPayload::Namespace(namespace_id),
                Provenance::Defined,
                kinds::NAMESPACES,
            ),
        }
        self.get_scope_mut(scope_id).kinds |= kinds::NAMESPACES;
    }

    pub fn find_function(&self, scope: ScopeId, ident: StringId) -> Option<FunctionId> {
        self.walk_chain(scope, kinds::FUNCTIONS, |sid| self.find_function_local(sid, ident))
            .map(|(v, _)| v)
    }

    pub fn find_function_local(&self, scope_id: ScopeId, ident: StringId) -> Option<FunctionId> {
        self.find_function_entry(scope_id, ident).map(|(f, _)| f)
    }

    pub fn find_function_exposed(&self, scope_id: ScopeId, ident: StringId) -> Option<FunctionId> {
        exposed(self.find_function_entry(scope_id, ident))
    }

    fn find_function_entry(
        &self,
        scope_id: ScopeId,
        ident: StringId,
    ) -> Option<(FunctionId, Provenance)> {
        let scope = self.get_scope(scope_id);
        match scope.ns_map {
            Some(m) => self.ns_entry(m, ident)?.function,
            None => self.entry_find(scope.entries, ident.as_u32(), |e| match e.payload {
                EntryPayload::Function(f) => Some((f, e.provenance)),
                _ => None,
            }),
        }
    }

    #[must_use]
    pub fn add_function(
        &mut self,
        scope_id: ScopeId,
        identifier: StringId,
        function_id: FunctionId,
    ) -> bool {
        self.bind_function(scope_id, identifier, function_id, Provenance::Defined)
    }

    fn bind_function(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        function_id: FunctionId,
        provenance: Provenance,
    ) -> bool {
        let added = match self.get_scope(scope_id).ns_map {
            Some(m) => {
                let entry = self.ns_entry_mut(m, ident);
                if entry.function.is_some() {
                    false
                } else {
                    entry.function = Some((function_id, provenance));
                    true
                }
            }
            None => {
                if self.find_function_local(scope_id, ident).is_some() {
                    false
                } else {
                    self.entry_push(
                        scope_id,
                        ident.as_u32(),
                        EntryPayload::Function(function_id),
                        provenance,
                        kinds::FUNCTIONS,
                    );
                    true
                }
            }
        };
        if added {
            self.get_scope_mut(scope_id).kinds |= kinds::FUNCTIONS;
        }
        added
    }

    pub fn find_type(&self, scope_id: ScopeId, ident: StringId) -> Option<(TypeId, ScopeId)> {
        self.walk_chain(scope_id, kinds::TYPES, |sid| self.find_type_local(sid, ident))
    }

    pub fn find_type_local(&self, scope_id: ScopeId, ident: StringId) -> Option<TypeId> {
        self.find_type_entry(scope_id, ident).map(|(t, _)| t)
    }

    pub fn find_type_exposed(&self, scope_id: ScopeId, ident: StringId) -> Option<TypeId> {
        exposed(self.find_type_entry(scope_id, ident))
    }

    fn find_type_entry(&self, scope_id: ScopeId, ident: StringId) -> Option<(TypeId, Provenance)> {
        let scope = self.get_scope(scope_id);
        match scope.ns_map {
            Some(m) => match self.ns_entry(m, ident)?.ty {
                NsEntryTypeValue::Defined(t, p) => Some((t, p)),
                NsEntryTypeValue::None | NsEntryTypeValue::Pending(_) => None,
            },
            None => self.entry_find(scope.entries, ident.as_u32(), |e| match e.payload {
                EntryPayload::Type(t) => Some((t, e.provenance)),
                _ => None,
            }),
        }
    }

    #[must_use]
    pub fn add_type(&mut self, scope_id: ScopeId, ident: StringId, ty: TypeId) -> bool {
        self.bind_type(scope_id, ident, ty, Provenance::Defined)
    }

    fn bind_type(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        ty: TypeId,
        provenance: Provenance,
    ) -> bool {
        let added = match self.get_scope(scope_id).ns_map {
            Some(m) => {
                let entry = self.ns_entry_mut(m, ident);
                match entry.ty {
                    NsEntryTypeValue::Defined(..) => false,
                    NsEntryTypeValue::None | NsEntryTypeValue::Pending(_) => {
                        entry.ty = NsEntryTypeValue::Defined(ty, provenance);
                        true
                    }
                }
            }
            None => {
                if self.find_type_local(scope_id, ident).is_some() {
                    false
                } else {
                    self.entry_push(
                        scope_id,
                        ident.as_u32(),
                        EntryPayload::Type(ty),
                        provenance,
                        kinds::TYPES,
                    );
                    true
                }
            }
        };
        if added {
            self.get_scope_mut(scope_id).kinds |= kinds::TYPES;
        }
        added
    }

    #[must_use]
    pub fn add_pending_type(
        &mut self,
        scope_id: ScopeId,
        name: StringId,
        parsed_id: ParsedTypeDefnId,
    ) -> bool {
        let added = match self.get_scope(scope_id).ns_map {
            Some(m) => {
                let entry = self.ns_entry_mut(m, name);
                match entry.ty {
                    NsEntryTypeValue::None => {
                        entry.ty = NsEntryTypeValue::Pending(parsed_id);
                        true
                    }
                    NsEntryTypeValue::Pending(_) => false,
                    NsEntryTypeValue::Defined(..) => true,
                }
            }
            None => {
                if self.find_pending_type_local(scope_id, name).is_some() {
                    false
                } else {
                    self.entry_push(
                        scope_id,
                        name.as_u32(),
                        EntryPayload::PendingType(parsed_id),
                        Provenance::Defined,
                        kinds::PENDING_TYPES,
                    );
                    true
                }
            }
        };
        if added {
            self.get_scope_mut(scope_id).kinds |= kinds::PENDING_TYPES;
        }
        added
    }

    pub fn find_pending_type(
        &self,
        scope_id: ScopeId,
        name: StringId,
    ) -> Option<(ParsedTypeDefnId, ScopeId)> {
        self.walk_chain(scope_id, kinds::PENDING_TYPES, |sid| {
            self.find_pending_type_local(sid, name)
        })
    }

    pub fn find_pending_type_local(
        &self,
        scope_id: ScopeId,
        name: StringId,
    ) -> Option<ParsedTypeDefnId> {
        let scope = self.get_scope(scope_id);
        match scope.ns_map {
            Some(m) => match self.ns_entry(m, name)?.ty {
                NsEntryTypeValue::Pending(parsed_id) => Some(parsed_id),
                NsEntryTypeValue::None | NsEntryTypeValue::Defined(..) => None,
            },
            None => self.entry_find(scope.entries, name.as_u32(), |e| match e.payload {
                EntryPayload::PendingType(parsed_id) => Some(parsed_id),
                _ => None,
            }),
        }
    }

    pub fn find_ability(&self, scope_id: ScopeId, name: StringId) -> Option<AbilityId> {
        self.walk_chain(scope_id, kinds::ABILITIES, |sid| self.find_ability_local(sid, name))
            .map(|(v, _)| v)
    }

    pub fn find_ability_local(&self, scope_id: ScopeId, name: StringId) -> Option<AbilityId> {
        self.find_ability_entry(scope_id, name).map(|(a, _)| a)
    }

    pub fn find_ability_exposed(&self, scope_id: ScopeId, name: StringId) -> Option<AbilityId> {
        exposed(self.find_ability_entry(scope_id, name))
    }

    fn find_ability_entry(
        &self,
        scope_id: ScopeId,
        name: StringId,
    ) -> Option<(AbilityId, Provenance)> {
        let scope = self.get_scope(scope_id);
        match scope.ns_map {
            Some(m) => match self.ns_entry(m, name)?.ability {
                NsEntryAbilityVAlue::Defined(a, p) => Some((a, p)),
                NsEntryAbilityVAlue::None | NsEntryAbilityVAlue::Pending(_) => None,
            },
            None => self.entry_find(scope.entries, name.as_u32(), |e| match e.payload {
                EntryPayload::Ability(a) => Some((a, e.provenance)),
                _ => None,
            }),
        }
    }

    #[must_use]
    pub fn add_ability(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        ability_id: AbilityId,
    ) -> bool {
        self.bind_ability(scope_id, ident, ability_id, Provenance::Defined)
    }

    fn bind_ability(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        ability_id: AbilityId,
        provenance: Provenance,
    ) -> bool {
        let added = match self.get_scope(scope_id).ns_map {
            Some(m) => {
                let entry = self.ns_entry_mut(m, ident);
                match entry.ability {
                    NsEntryAbilityVAlue::Defined(..) => false,
                    NsEntryAbilityVAlue::None | NsEntryAbilityVAlue::Pending(_) => {
                        entry.ability = NsEntryAbilityVAlue::Defined(ability_id, provenance);
                        true
                    }
                }
            }
            None => {
                if self.find_ability_local(scope_id, ident).is_some() {
                    false
                } else {
                    self.entry_push(
                        scope_id,
                        ident.as_u32(),
                        EntryPayload::Ability(ability_id),
                        provenance,
                        kinds::ABILITIES,
                    );
                    true
                }
            }
        };
        if added {
            self.get_scope_mut(scope_id).kinds |= kinds::ABILITIES;
        }
        added
    }

    #[must_use]
    pub fn add_pending_ability_defn(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        parsed_defn_id: ParsedAbilityId,
    ) -> bool {
        let added = match self.get_scope(scope_id).ns_map {
            Some(m) => {
                let entry = self.ns_entry_mut(m, ident);
                match entry.ability {
                    NsEntryAbilityVAlue::None => {
                        entry.ability = NsEntryAbilityVAlue::Pending(parsed_defn_id);
                        true
                    }
                    NsEntryAbilityVAlue::Pending(_) => false,
                    NsEntryAbilityVAlue::Defined(..) => true,
                }
            }
            None => {
                let existing =
                    self.entry_find(self.get_scope(scope_id).entries, ident.as_u32(), |e| match e
                        .payload
                    {
                        EntryPayload::PendingAbility(id) => Some(id),
                        _ => None,
                    });
                if existing.is_some() {
                    false
                } else {
                    self.entry_push(
                        scope_id,
                        ident.as_u32(),
                        EntryPayload::PendingAbility(parsed_defn_id),
                        Provenance::Defined,
                        kinds::PENDING_ABILITIES,
                    );
                    true
                }
            }
        };
        if added {
            self.get_scope_mut(scope_id).kinds |= kinds::PENDING_ABILITIES;
        }
        added
    }

    pub fn find_pending_ability(
        &self,
        scope_id: ScopeId,
        ident: StringId,
    ) -> Option<(ParsedAbilityId, ScopeId)> {
        self.walk_chain(scope_id, kinds::PENDING_ABILITIES, |sid| {
            let scope = self.get_scope(sid);
            match scope.ns_map {
                Some(m) => match self.ns_entry(m, ident)?.ability {
                    NsEntryAbilityVAlue::Pending(parsed_id) => Some(parsed_id),
                    NsEntryAbilityVAlue::None | NsEntryAbilityVAlue::Defined(..) => None,
                },
                None => self.entry_find(scope.entries, ident.as_u32(), |e| match e.payload {
                    EntryPayload::PendingAbility(id) => Some(id),
                    _ => None,
                }),
            }
        })
    }

    pub fn collect_ability_ids_bound_to_names(
        &self,
        scope_id: ScopeId,
        names: &[StringId],
        ability_ids: &mut List<AbilityId, MemTmp>,
        mem: &mut Mem<MemTmp>,
    ) {
        let mut scope_id = scope_id;
        loop {
            let scope = self.get_scope(scope_id);
            if scope.kinds & kinds::ABILITIES != 0 {
                for name in names {
                    if let Some(found) = self.find_ability_local(scope_id, *name)
                        && !ability_ids.contains(&found)
                    {
                        ability_ids.push_grow(mem, found);
                    }
                }
            }
            match scope.parent {
                Some(parent) => scope_id = parent,
                None => return,
            }
        }
    }

    pub fn add_pending_global(
        &mut self,
        scope_id: ScopeId,
        name: StringId,
        parsed_id: ParsedGlobalId,
    ) {
        match self.get_scope(scope_id).ns_map {
            Some(m) => {
                let entry = self.ns_entry_mut(m, name);
                if matches!(entry.global, NsEntryGlobalValue::None) {
                    entry.global = NsEntryGlobalValue::Pending(parsed_id);
                }
            }
            None => {
                if self.find_pending_global_local(scope_id, name).is_none() {
                    self.entry_push(
                        scope_id,
                        name.as_u32(),
                        EntryPayload::PendingGlobal(parsed_id),
                        Provenance::Defined,
                        kinds::PENDING_GLOBALS,
                    );
                }
            }
        }
        self.get_scope_mut(scope_id).kinds |= kinds::PENDING_GLOBALS;
    }

    pub fn find_pending_global(
        &self,
        scope_id: ScopeId,
        name: StringId,
    ) -> Option<(ParsedGlobalId, ScopeId)> {
        self.walk_chain(scope_id, kinds::PENDING_GLOBALS, |sid| {
            self.find_pending_global_local(sid, name)
        })
    }

    pub fn find_pending_global_local(
        &self,
        scope_id: ScopeId,
        name: StringId,
    ) -> Option<ParsedGlobalId> {
        let scope = self.get_scope(scope_id);
        match scope.ns_map {
            Some(m) => match self.ns_entry(m, name)?.global {
                NsEntryGlobalValue::Pending(parsed_id) => Some(parsed_id),
                NsEntryGlobalValue::None | NsEntryGlobalValue::Bound(..) => None,
            },
            None => self.entry_find(scope.entries, name.as_u32(), |e| match e.payload {
                EntryPayload::PendingGlobal(id) => Some(id),
                _ => None,
            }),
        }
    }

    pub fn find_variable(
        &self,
        scope_id: ScopeId,
        ident: StringId,
    ) -> Option<(VariableId, ScopeId)> {
        let (in_scope, found_scope) = self
            .walk_chain(scope_id, kinds::VARIABLES, |sid| self.find_variable_local(sid, ident))?;
        match in_scope {
            VariableInScope::Defined(id) => Some((id, found_scope)),
            VariableInScope::Masked => None,
        }
    }

    pub fn find_variable_local(
        &self,
        scope_id: ScopeId,
        ident: StringId,
    ) -> Option<VariableInScope> {
        self.find_variable_entry(scope_id, ident).map(|(vis, _)| vis)
    }

    pub fn find_variable_exposed(
        &self,
        scope_id: ScopeId,
        ident: StringId,
    ) -> Option<VariableInScope> {
        exposed(self.find_variable_entry(scope_id, ident))
    }

    fn find_variable_entry(
        &self,
        scope_id: ScopeId,
        ident: StringId,
    ) -> Option<(VariableInScope, Provenance)> {
        let scope = self.get_scope(scope_id);
        match scope.ns_map {
            Some(m) => match self.ns_entry(m, ident)?.global {
                NsEntryGlobalValue::Bound(vis, p) => Some((vis, p)),
                NsEntryGlobalValue::None | NsEntryGlobalValue::Pending(_) => None,
            },
            None => self.entry_find(scope.entries, ident.as_u32(), |e| match e.payload {
                EntryPayload::Variable(vis) => Some((vis, e.provenance)),
                _ => None,
            }),
        }
    }

    pub fn add_variable(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        variable_id: VariableId,
    ) -> bool {
        self.bind_variable(
            scope_id,
            ident,
            VariableInScope::Defined(variable_id),
            Provenance::Defined,
        )
    }

    pub fn mask_variable(&mut self, scope_id: ScopeId, ident: StringId) {
        let _ = self.bind_variable(scope_id, ident, VariableInScope::Masked, Provenance::Defined);
    }

    /// Always binds (shadowing is a new entry over the old); returns whether
    /// the name was previously unbound in this scope
    fn bind_variable(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        vis: VariableInScope,
        provenance: Provenance,
    ) -> bool {
        let was_unbound = match self.get_scope(scope_id).ns_map {
            Some(m) => {
                let entry = self.ns_entry_mut(m, ident);
                let unbound = !matches!(entry.global, NsEntryGlobalValue::Bound(..));
                entry.global = NsEntryGlobalValue::Bound(vis, provenance);
                unbound
            }
            None => {
                let unbound = self.find_variable_local(scope_id, ident).is_none();
                self.entry_push(
                    scope_id,
                    ident.as_u32(),
                    EntryPayload::Variable(vis),
                    provenance,
                    kinds::VARIABLES,
                );
                unbound
            }
        };
        self.get_scope_mut(scope_id).kinds |= kinds::VARIABLES;
        was_unbound
    }

    pub fn add_context_variable(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        variable_id: VariableId,
        type_id: TypeId,
    ) -> bool {
        let occupied = self
            .entry_find(self.get_scope(scope_id).entries, type_id.as_u32(), |e| match e.payload {
                EntryPayload::ContextByType(v) => Some(v),
                _ => None,
            })
            .is_some();
        if occupied {
            return false;
        }
        self.entry_push(
            scope_id,
            type_id.as_u32(),
            EntryPayload::ContextByType(variable_id),
            Provenance::Defined,
            kinds::CONTEXT_VARIABLES,
        );
        self.add_variable(scope_id, ident, variable_id);
        true
    }

    pub fn find_context_variable_by_type(
        &self,
        scope: ScopeId,
        type_id: TypeId,
    ) -> Option<VariableId> {
        let mut scope_id = scope;
        loop {
            let scope = self.get_scope(scope_id);
            if scope.kinds & kinds::CONTEXT_VARIABLES != 0
                && let Some(v) =
                    self.entry_find(scope.entries, type_id.as_u32(), |e| match e.payload {
                        EntryPayload::ContextByType(v) => Some(v),
                        _ => None,
                    })
            {
                return Some(v);
            }
            // Context variables are only ever function params or `let(context)`s
            // in function bodies -- never globals -- so once the walk climbs
            // out of function-land into a namespace, there's nothing left to
            // find. (Note some paths still evaluate a function body in its own
            // FunctionScope *below* the declaration scope holding the params,
            // so we cannot stop at the first FunctionScope yet; tighten this
            // to `== FunctionScope` once every body is one scope.)
            if scope.scope_type == ScopeType::Namespace {
                return None;
            }
            scope_id = scope.parent?;
        }
    }

    pub fn add_context_variable_by_ability(
        &mut self,
        scope_id: ScopeId,
        ability_id: AbilityId,
        variable_id: VariableId,
    ) -> bool {
        let occupied = self
            .entry_find(self.get_scope(scope_id).entries, ability_id.as_u32(), |e| {
                match e.payload {
                    EntryPayload::ContextByAbility(e) => Some(e),
                    _ => None,
                }
            })
            .is_some();
        if occupied {
            return false;
        }
        self.entry_push(
            scope_id,
            ability_id.as_u32(),
            EntryPayload::ContextByAbility(ContextAbilityEntry::Unique(variable_id)),
            Provenance::Defined,
            kinds::CONTEXT_VARIABLES,
        );
        true
    }

    pub fn poison_context_ability_key(&mut self, scope_id: ScopeId, ability_id: AbilityId) {
        self.entry_push(
            scope_id,
            ability_id.as_u32(),
            EntryPayload::ContextByAbility(ContextAbilityEntry::Ambiguous),
            Provenance::Defined,
            kinds::CONTEXT_VARIABLES,
        );
    }

    pub fn find_context_variable_by_ability(
        &self,
        scope: ScopeId,
        ability_id: AbilityId,
    ) -> Option<ContextAbilityEntry> {
        let mut scope_id = scope;
        loop {
            let scope = self.get_scope(scope_id);
            if scope.kinds & kinds::CONTEXT_VARIABLES != 0
                && let Some(entry) =
                    self.entry_find(scope.entries, ability_id.as_u32(), |e| match e.payload {
                        EntryPayload::ContextByAbility(e) => Some(e),
                        _ => None,
                    })
            {
                return Some(entry);
            }
            // Same walk and namespace stop as find_context_variable_by_type
            if scope.scope_type == ScopeType::Namespace {
                return None;
            }
            scope_id = scope.parent?;
        }
    }

    pub fn add_type_substitution(&mut self, scope_id: ScopeId, from: TypeId, to: TypeId) -> bool {
        let occupied = self
            .entry_find(self.get_scope(scope_id).entries, from.as_u32(), |e| match e.payload {
                EntryPayload::TypeSubst(t) => Some(t),
                _ => None,
            })
            .is_some();
        if occupied {
            return false;
        }
        self.entry_push(
            scope_id,
            from.as_u32(),
            EntryPayload::TypeSubst(to),
            Provenance::Defined,
            kinds::TYPE_PARAM_SUBSTS,
        );
        true
    }

    pub fn find_type_substitution(
        &self,
        scope_id: ScopeId,
        from: TypeId,
    ) -> Option<(TypeId, ScopeId)> {
        self.walk_chain(scope_id, kinds::TYPE_PARAM_SUBSTS, |sid| {
            self.entry_find(self.get_scope(sid).entries, from.as_u32(), |e| match e.payload {
                EntryPayload::TypeSubst(t) => Some(t),
                _ => None,
            })
        })
    }

    pub fn add_use_binding(
        &mut self,
        scope_id: ScopeId,
        useable_symbol: &UseableSymbol,
        name_to_use: StringId,
        provenance: Provenance,
    ) {
        match useable_symbol.id {
            UseableSymbolId::Function(function_id) => {
                let _ = self.bind_function(scope_id, name_to_use, function_id, provenance);
            }
            UseableSymbolId::Global(variable_id) => {
                let _ = self.bind_variable(
                    scope_id,
                    name_to_use,
                    VariableInScope::Defined(variable_id),
                    provenance,
                );
            }
            UseableSymbolId::Type { type_id, companion_namespace } => {
                let _ = self.bind_type(scope_id, name_to_use, type_id, provenance);
                if let Some(companion_namespace) = companion_namespace {
                    let _ =
                        self.bind_namespace(scope_id, name_to_use, companion_namespace, provenance);
                }
            }
            UseableSymbolId::Namespace(ns_id) => {
                let _ = self.bind_namespace(scope_id, name_to_use, ns_id, provenance);
            }
            UseableSymbolId::Ability(ability_id, namespace_id) => {
                let _ = self.bind_ability(scope_id, name_to_use, ability_id, provenance);
                let _ = self.bind_namespace(scope_id, name_to_use, namespace_id, provenance);
            }
        }
    }

    pub fn iter_scope_variables(
        &self,
        scope_id: ScopeId,
    ) -> impl Iterator<Item = (StringId, VariableInScope, Provenance)> + '_ {
        let scope = self.get_scope(scope_id);
        let map = scope.ns_map.map(|m| {
            self.ns_maps.get(m).iter().filter_map(|(name, e)| match e.global {
                NsEntryGlobalValue::Bound(vis, p) => Some((*name, vis, p)),
                NsEntryGlobalValue::None | NsEntryGlobalValue::Pending(_) => None,
            })
        });
        let list = self.scope_entries(scope.entries).filter_map(|e| match e.payload {
            EntryPayload::Variable(vis) => {
                Some((StringId::from_u32(e.key).unwrap(), vis, e.provenance))
            }
            _ => None,
        });
        map.into_iter().flatten().chain(list)
    }

    pub fn iter_scope_functions(
        &self,
        scope_id: ScopeId,
    ) -> impl Iterator<Item = (StringId, FunctionId, Provenance)> + '_ {
        let scope = self.get_scope(scope_id);
        let map = scope.ns_map.map(|m| {
            self.ns_maps
                .get(m)
                .iter()
                .filter_map(|(name, e)| e.function.map(|(f, p)| (*name, f, p)))
        });
        let list = self.scope_entries(scope.entries).filter_map(|e| match e.payload {
            EntryPayload::Function(f) => {
                Some((StringId::from_u32(e.key).unwrap(), f, e.provenance))
            }
            _ => None,
        });
        map.into_iter().flatten().chain(list)
    }

    pub fn iter_scope_types(
        &self,
        scope_id: ScopeId,
    ) -> impl Iterator<Item = (StringId, TypeId, Provenance)> + '_ {
        let scope = self.get_scope(scope_id);
        let map = scope.ns_map.map(|m| {
            self.ns_maps.get(m).iter().filter_map(|(name, e)| match e.ty {
                NsEntryTypeValue::Defined(t, p) => Some((*name, t, p)),
                NsEntryTypeValue::None | NsEntryTypeValue::Pending(_) => None,
            })
        });
        let list = self.scope_entries(scope.entries).filter_map(|e| match e.payload {
            EntryPayload::Type(t) => Some((StringId::from_u32(e.key).unwrap(), t, e.provenance)),
            _ => None,
        });
        map.into_iter().flatten().chain(list)
    }

    pub fn iter_scope_namespaces(
        &self,
        scope_id: ScopeId,
    ) -> impl Iterator<Item = (StringId, NamespaceId, Provenance)> + '_ {
        let scope = self.get_scope(scope_id);
        let map = scope.ns_map.map(|m| {
            self.ns_maps
                .get(m)
                .iter()
                .filter_map(|(name, e)| e.namespace.map(|(ns, p)| (*name, ns, p)))
        });
        let list = self.scope_entries(scope.entries).filter_map(|e| match e.payload {
            EntryPayload::Namespace(ns) => {
                Some((StringId::from_u32(e.key).unwrap(), ns, e.provenance))
            }
            _ => None,
        });
        map.into_iter().flatten().chain(list)
    }

    pub fn iter_scope_abilities(
        &self,
        scope_id: ScopeId,
    ) -> impl Iterator<Item = (StringId, AbilityId, Provenance)> + '_ {
        let scope = self.get_scope(scope_id);
        let map = scope.ns_map.map(|m| {
            self.ns_maps.get(m).iter().filter_map(|(name, e)| match e.ability {
                NsEntryAbilityVAlue::Defined(a, p) => Some((*name, a, p)),
                NsEntryAbilityVAlue::None | NsEntryAbilityVAlue::Pending(_) => None,
            })
        });
        let list = self.scope_entries(scope.entries).filter_map(|e| match e.payload {
            EntryPayload::Ability(a) => Some((StringId::from_u32(e.key).unwrap(), a, e.provenance)),
            _ => None,
        });
        map.into_iter().flatten().chain(list)
    }

    pub fn scope_has_ancestor(&self, scope_id: ScopeId, ancestor: ScopeId) -> bool {
        let scope = self.get_scope(scope_id);
        match scope.parent {
            Some(parent) => {
                if parent == ancestor {
                    true
                } else {
                    self.scope_has_ancestor(parent, ancestor)
                }
            }
            None => false,
        }
    }

    pub fn nearest_parent_namespace(&self, scope_id: ScopeId) -> NamespaceId {
        let scope = self.get_scope(scope_id);
        match scope.owner_id {
            ScopeOwnerId::Namespace(ns) => ns,
            _ => match scope.parent {
                Some(parent) => self.nearest_parent_namespace(parent),
                None => panic!("No parent namespace found"),
            },
        }
    }

    pub fn nearest_parent_function(&self, calling_scope: ScopeId) -> Option<FunctionId> {
        let mut scope_id = calling_scope;
        loop {
            let scope = self.get_scope(scope_id);
            match scope.owner_id {
                // We can stop searching once we find an ability or namespace scope; a function won't ever appear above them
                ScopeOwnerId::Ability(_) | ScopeOwnerId::Namespace(_) => return None,
                ScopeOwnerId::Function(fn_id) => return Some(fn_id),
                _ => match scope.parent {
                    Some(parent) => scope_id = parent,
                    None => return None,
                },
            }
        }
    }

    pub fn nearest_parent_lambda(&self, scope_id: ScopeId) -> Option<ScopeId> {
        self.get_scope(scope_id).nearest_lambda
    }

    /// Both enclosing-function facts at once, for consumers that give lambdas
    /// priority over the containing function (e.g. `return` handling)
    pub fn enclosing_function_info(&self, scope_id: ScopeId) -> ScopeEnclosingFunctions {
        ScopeEnclosingFunctions {
            lambda_scope: self.nearest_parent_lambda(scope_id),
            function: self.nearest_parent_function(scope_id),
        }
    }

    pub fn add_lambda_info(&mut self, lambda_scope_id: ScopeId, info: ScopeLambdaInfo) {
        self.lambda_info.insert(lambda_scope_id, info);
    }

    pub fn get_lambda_info(&self, lambda_scope_id: ScopeId) -> &ScopeLambdaInfo {
        self.lambda_info.get(&lambda_scope_id).unwrap()
    }

    pub fn add_loop_info(&mut self, loop_scope_id: ScopeId, info: ScopeLoopInfo) {
        self.loop_info.insert(loop_scope_id, info);
    }

    pub fn get_loop_info(&self, loop_scope_id: ScopeId) -> Option<&ScopeLoopInfo> {
        self.loop_info.get(&loop_scope_id)
    }

    pub fn find_loop(
        &self,
        from_scope: ScopeId,
        label: Option<StringId>,
    ) -> Option<(ScopeId, LoopType)> {
        let mut scope_id = from_scope;
        loop {
            let scope = self.get_scope(scope_id);
            if let Some(loop_type) = scope.scope_type.loop_type() {
                let matches = match label {
                    None => true,
                    Some(label) => self.loop_info[&scope_id].label == Some(label),
                };
                if matches {
                    return Some((scope_id, loop_type));
                }
            }
            if scope.scope_type.is_top_of_function() {
                return None;
            }
            scope_id = scope.parent?;
        }
    }
}

impl TypedProgram {
    pub fn find_variable_namespaced(
        &self,
        scope: ScopeId,
        name: &QIdent,
    ) -> K1Result<Option<(VariableId, ScopeId)>> {
        // Unqualified mentions are implicitly recursive searches
        // But qualified mentions imply that the targeted symbol lives directly at the given path!
        if name.path.is_empty() {
            Ok(self.scopes.find_variable(scope, name.name))
        } else {
            let scope_to_search = self.resolve_qident(scope, name)?;
            match self.scopes.find_variable_exposed(scope_to_search, name.name) {
                None => Ok(None),
                Some(VariableInScope::Defined(id)) => Ok(Some((id, scope_to_search))),
                Some(VariableInScope::Masked) => Ok(None),
            }
        }
    }

    pub fn find_function_namespaced(
        &self,
        scope: ScopeId,
        name: &QIdent,
    ) -> K1Result<Option<FunctionId>> {
        // Unqualified mentions are implicitly recursive searches
        // But qualified mentions imply that the targeted symbol lives directly at the given path!
        if name.path.is_empty() {
            Ok(self.scopes.find_function(scope, name.name))
        } else {
            let scope_to_search = self.resolve_qident(scope, name)?;
            Ok(self.scopes.find_function_exposed(scope_to_search, name.name))
        }
    }

    pub fn find_type_namespaced(
        &self,
        scope_id: ScopeId,
        type_name: &QIdent,
    ) -> K1Result<Option<(TypeId, ScopeId)>> {
        // Unqualified mentions are implicitly recursive searches
        // But qualified mentions imply that the targeted symbol lives directly at the given path!
        if type_name.path.is_empty() {
            Ok(self.scopes.find_type(scope_id, type_name.name))
        } else {
            let scope_to_search = self.resolve_qident(scope_id, type_name)?;
            let found_type = self.scopes.find_type_exposed(scope_to_search, type_name.name);
            match found_type {
                None => Ok(None),
                Some(type_id) => Ok(Some((type_id, scope_to_search))),
            }
        }
    }

    /// Scopes don't store names; derive one from the owner, if any.
    /// For pretty-printing and debugging only.
    pub fn scope_owner_name(&self, scope_id: ScopeId) -> Option<StringId> {
        match self.scopes.get_scope_owner(scope_id) {
            ScopeOwnerId::None => None,
            ScopeOwnerId::Namespace(ns_id) => Some(self.namespaces.get(ns_id).name),
            ScopeOwnerId::Ability(ability_id) => self.abilities.get_opt(ability_id).map(|a| a.name),
            // get_opt because owners can be assigned ids before the function
            // itself is added to the pool
            ScopeOwnerId::Function(function_id) | ScopeOwnerId::Lambda(_, function_id, _) => {
                self.functions.get_opt(function_id).map(|f| f.name)
            }
        }
    }

    pub fn scope_name_to_string(&self, scope_id: ScopeId) -> String {
        let mut name = String::new();
        self.display_scope_name(&mut name, scope_id).unwrap();
        name
    }

    pub fn display_scope_name<W: std::fmt::Write + ?Sized>(
        &self,
        name_buf: &mut W,
        scope_id: ScopeId,
    ) -> std::fmt::Result {
        self.write_scope_path(name_buf, scope_id, "/", true);
        Ok(())
    }

    pub fn resolve_qident(&self, scope_id: ScopeId, qident: &QIdent) -> K1Result<ScopeId> {
        let mut ns_iter = self.ast.mem.getn(qident.path).iter();

        let mut cur_scope_id = scope_id;
        let Some(first) = ns_iter.next() else {
            return Ok(cur_scope_id);
        };
        // First lookup is special and recursive because it's in the current scope
        let Some(first_ns) = self.scopes.find_namespace(cur_scope_id, first.name) else {
            kbail!(
                self,
                first.span,
                "Namespace not found: {} from scope: {}",
                first.name,
                self.scope_name_to_string(cur_scope_id,)
            );
        };
        self.emit_ls_entity(first.span, LsEntityKind::Namespace(first_ns));
        cur_scope_id = self.namespaces.get(first_ns).scope_id;

        for ident in ns_iter {
            let namespace_id =
                self.scopes.find_namespace_exposed(cur_scope_id, ident.name).ok_or_else(|| {
                    kerr!(
                        self,
                        ident.span,
                        "Namespace not found: {} in scope: {}",
                        ident.name,
                        format!(
                            "{:?}",
                            self.scope_owner_name(cur_scope_id).map(|n| self.ident_str(n))
                        )
                    )
                })?;
            let namespace = self.namespaces.get(namespace_id);
            self.emit_ls_entity(ident.span, LsEntityKind::Namespace(namespace_id));
            cur_scope_id = namespace.scope_id;
        }
        Ok(cur_scope_id)
    }

    pub fn find_ability_namespaced(
        &self,
        scope_id: ScopeId,
        ability_name: &QIdent,
    ) -> K1Result<Option<AbilityId>> {
        // Unqualified mentions are implicitly recursive searches
        // But qualified mentions imply that the targeted symbol lives directly at the given path!
        if ability_name.path.is_empty() {
            Ok(self.scopes.find_ability(scope_id, ability_name.name))
        } else {
            let scope_to_search = self.resolve_qident(scope_id, ability_name)?;
            Ok(self.scopes.find_ability_exposed(scope_to_search, ability_name.name))
        }
    }

    pub fn find_pending_type_namespaced(
        &self,
        scope_id: ScopeId,
        type_name: &QIdent,
    ) -> K1Result<Option<(ParsedTypeDefnId, ScopeId)>> {
        // Unqualified mentions are implicitly recursive searches
        // But qualified mentions imply that the targeted symbol lives directly at the given path!
        if type_name.path.is_empty() {
            Ok(self.scopes.find_pending_type(scope_id, type_name.name))
        } else {
            let scope_to_search = self.resolve_qident(scope_id, type_name)?;
            Ok(self
                .scopes
                .find_pending_type_local(scope_to_search, type_name.name)
                .map(|parsed_id| (parsed_id, scope_to_search)))
        }
    }

    pub fn find_pending_global_namespaced(
        &self,
        scope_id: ScopeId,
        name: &QIdent,
    ) -> K1Result<Option<(ParsedGlobalId, ScopeId)>> {
        if name.path.is_empty() {
            Ok(self.scopes.find_pending_global(scope_id, name.name))
        } else {
            let scope_to_search = self.resolve_qident(scope_id, name)?;
            Ok(self
                .scopes
                .find_pending_global_local(scope_to_search, name.name)
                .map(|id| (id, scope_to_search)))
        }
    }

    pub fn name_chain(&self, id: NamespaceId) -> Dlist<StringId, MemTmp> {
        let mut chain = Dlist::empty();
        let mut id = id;
        loop {
            let namespace = self.namespaces.get(id);
            self.get_tmp_unsafe().dlist_push_front(&mut chain, namespace.name);
            if let Some(parent_id) = namespace.parent_id {
                id = parent_id;
            } else {
                break;
            }
        }
        chain
    }
}

/// Useful for going from scope to 'thing that owns the scope', like to a scope's ability or namespace or function
#[derive(Debug, Clone, Copy)]
pub enum ScopeOwnerId {
    None,
    Ability(AbilityId),
    Function(FunctionId),
    Namespace(NamespaceId),
    Lambda(TypeId, FunctionId, ScopeId),
}
impl ScopeOwnerId {
    pub fn expect_ability(&self) -> AbilityId {
        match self {
            ScopeOwnerId::Ability(a) => *a,
            _ => panic!("Expected ability scope owner"),
        }
    }

    pub fn as_namespace(&self) -> Option<NamespaceId> {
        match self {
            ScopeOwnerId::Namespace(ns_id) => Some(*ns_id),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct UseableSymbol {
    pub id: UseableSymbolId,
    #[allow(unused)]
    pub source_scope: ScopeId,
}

#[derive(Debug, Clone, Copy)]
pub enum UseableSymbolId {
    Function(FunctionId),
    Global(VariableId),
    Type { type_id: TypeId, companion_namespace: Option<NamespaceId> },
    Namespace(NamespaceId),
    Ability(AbilityId, NamespaceId),
}

impl UseableSymbolId {
    pub fn namespace_id(&self) -> Option<NamespaceId> {
        match self {
            UseableSymbolId::Type { companion_namespace, .. } => *companion_namespace,
            UseableSymbolId::Namespace(ns_id) => Some(*ns_id),
            UseableSymbolId::Ability(_, ns_id) => Some(*ns_id),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum VariableInScope {
    Masked,
    Defined(VariableId),
}

impl VariableInScope {
    pub fn variable_id(&self) -> Option<VariableId> {
        match self {
            VariableInScope::Masked => None,
            VariableInScope::Defined(v) => Some(*v),
        }
    }
}

#[derive(Clone, Copy)]
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub scope_type: ScopeType,
    /// Bitset of `kinds::*`
    kinds: u16,
    /// The enclosing lambda scope (or self, for a lambda scope), fixed at creation.
    /// Queried on every variable mention, for capture checking
    nearest_lambda: Option<ScopeId>,
    pub owner_id: ScopeOwnerId,
    entries: Option<ScopeEntryId>,
    ns_map: Option<NsMapId>,
}
static_assert_size!(Scope, 36);

impl Scope {
    pub fn clear_nearest_lambda(&mut self) {
        self.nearest_lambda = None;
    }
}
