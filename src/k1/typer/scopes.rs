// Copyright (c) 2026 knix
// All rights reserved.

use fxhash::FxHashMap;

use std::{cell::Cell, fmt::Display, num::NonZeroU32};

use crate::{
    kbail, kerr,
    kmem::{List, Mem},
    nz_u32_id,
    parse::{IdentSpanned, ParsedAbilityId, ParsedGlobalId, ParsedTypeDefnId, ParsedUseId, QIdent},
    static_assert_niched, static_assert_size,
    typer::{
        AbilityId, FunctionId, K1Message, K1Result, LoopType, LsEntityKind, MemTmp, NamespaceId,
        StringId, TypeId, TypedProgram, VariableId,
    },
    vpool::{SliceHandle, VPool},
};

nz_u32_id!(ScopeId);
static_assert_niched!(ScopeId);
nz_u32_id!(ScopeEntryId);
nz_u32_id!(NsMapId);
nz_u32_id!(UseTargetId);
nz_u32_id!(UseIdentId);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ScopeType {
    FunctionScope,
    LambdaScope,
    StaticBody,
    DeferBody,
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
            ScopeType::StaticBody => "static",
            ScopeType::DeferBody => "defer",
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
        matches!(self, ScopeType::FunctionScope | ScopeType::LambdaScope | ScopeType::StaticBody)
    }

    pub fn loop_type(&self) -> Option<LoopType> {
        match self {
            ScopeType::FunctionScope => None,
            ScopeType::LambdaScope => None,
            ScopeType::StaticBody => None,
            ScopeType::DeferBody => None,
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
    pub const USES: u16 = 1 << 10;
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
    Namespace(ScopeId),
    Ability(AbilityId),
    PendingType(ParsedTypeDefnId),
    PendingAbility(ParsedAbilityId),
    PendingGlobal(ParsedGlobalId),
    ContextByType(VariableId),
    ContextByAbility(ContextAbilityEntry),
    TypeSubst(TypeId),
    Use(UseTargetId),
}

impl EntryPayload {
    fn kind(self) -> u16 {
        match self {
            EntryPayload::Variable(_) => kinds::VARIABLES,
            EntryPayload::Function(_) => kinds::FUNCTIONS,
            EntryPayload::Type(_) => kinds::TYPES,
            EntryPayload::Namespace(_) => kinds::NAMESPACES,
            EntryPayload::Ability(_) => kinds::ABILITIES,
            EntryPayload::PendingType(_) => kinds::PENDING_TYPES,
            EntryPayload::PendingAbility(_) => kinds::PENDING_ABILITIES,
            EntryPayload::PendingGlobal(_) => kinds::PENDING_GLOBALS,
            EntryPayload::ContextByType(_) | EntryPayload::ContextByAbility(_) => {
                kinds::CONTEXT_VARIABLES
            }
            EntryPayload::TypeSubst(_) => kinds::TYPE_PARAM_SUBSTS,
            EntryPayload::Use(_) => kinds::USES,
        }
    }
}

#[derive(Clone, Copy, Default)]
pub struct NsEntry {
    function: Option<FunctionId>,
    namespace: Option<(ScopeId, Provenance)>,
    ty: NsEntryTypeValue,
    ability: NsEntryAbilityValue,
    global: NsEntryGlobalValue,
    uses: Option<ScopeEntryId>,
}
static_assert_size!(NsEntry, 40);

#[derive(Clone, Copy)]
struct UseTarget {
    use_id: ParsedUseId,
    path: SliceHandle<UseIdentId>,
    name: StringId,
}

#[derive(Clone, Copy, Default)]
struct UseCache {
    epoch: u32,
    version: u32,
    scope_id: Option<ScopeId>,
    entry: Option<NsEntry>,
}

#[derive(Clone, Copy)]
struct Following<'a> {
    target: UseTargetId,
    outer: Option<&'a Following<'a>>,
}

impl Following<'_> {
    fn contains(&self, target: UseTargetId) -> bool {
        self.target == target || self.outer.is_some_and(|outer| outer.contains(target))
    }
}

#[derive(Clone, Copy, Default)]
enum NsEntryTypeValue {
    #[default]
    None,
    Pending(ParsedTypeDefnId),
    Defined(TypeId),
}

#[derive(Clone, Copy, Default)]
enum NsEntryAbilityValue {
    #[default]
    None,
    Pending(ParsedAbilityId),
    Defined(AbilityId),
}

#[derive(Clone, Copy, Default)]
enum NsEntryGlobalValue {
    #[default]
    None,
    Pending(ParsedGlobalId),
    Bound(VariableInScope),
}

pub struct Scopes {
    pub scopes: VPool<Scope, ScopeId>,
    entries: VPool<ScopeEntry, ScopeEntryId>,
    ns_maps: VPool<FxHashMap<StringId, NsEntry>, NsMapId>,
    use_targets: VPool<UseTarget, UseTargetId>,
    use_idents: VPool<StringId, UseIdentId>,
    use_caches: VPool<Cell<UseCache>, UseTargetId>,
    namespace_epoch: u32,
    pub lambda_info: FxHashMap<ScopeId, ScopeLambdaInfo>,
    pub loop_info: FxHashMap<ScopeId, ScopeLoopInfo>,
    pub core_scope_id: ScopeId,
    pub k1_scope_id: ScopeId,
    pub mem_scope_id: ScopeId,
    pub types_scope_id: ScopeId,
    pub test_scope_id: ScopeId,
    pub array_scope_id: ScopeId,
    pub vector_scope_id: ScopeId,
}

impl Scopes {
    pub fn bytes_used(&self) -> usize {
        self.scopes.bytes_used()
            + self.entries.bytes_used()
            + self.ns_maps.bytes_used()
            + self.use_targets.bytes_used()
            + self.use_idents.bytes_used()
    }

    pub fn snap(&self, w: &mut crate::snap::SnapWriter) {
        use crate::snap::write_map_snap;
        let Scopes {
            scopes,
            entries,
            ns_maps,
            use_targets,
            use_idents,
            use_caches: _,
            namespace_epoch: _,
            lambda_info,
            loop_info,
            core_scope_id,
            k1_scope_id,
            mem_scope_id,
            types_scope_id,
            test_scope_id,
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
        use_targets.snap(w);
        use_idents.snap(w);
        write_map_snap(w, lambda_info);
        write_map_snap(w, loop_info);
        for id in [
            core_scope_id,
            k1_scope_id,
            mem_scope_id,
            types_scope_id,
            test_scope_id,
            array_scope_id,
            vector_scope_id,
        ] {
            w.write_t(id);
        }
    }

    pub fn restore(r: &mut crate::snap::SnapReader) -> Scopes {
        use crate::snap::restore_map_snap;
        r.section("scopes");
        let mut scopes = VPool::make("scopes");
        scopes.restore(r);
        let mut entries = VPool::make("scope_entries");
        entries.restore(r);
        let mut ns_maps = VPool::make("scope_ns_maps");
        for _ in 0..r.read_len() {
            ns_maps.add(restore_map_snap(r));
        }
        let mut use_targets = VPool::make("scope_use_targets");
        use_targets.restore(r);
        let mut use_idents = VPool::make("scope_use_idents");
        use_idents.restore(r);
        let mut use_caches = VPool::make("scope_use_caches");
        for _ in 0..use_targets.len() {
            use_caches.add(Cell::default());
        }
        Scopes {
            scopes,
            entries,
            ns_maps,
            use_targets,
            use_idents,
            use_caches,
            namespace_epoch: 1,
            lambda_info: restore_map_snap(r),
            loop_info: restore_map_snap(r),
            core_scope_id: r.read_t(),
            k1_scope_id: r.read_t(),
            mem_scope_id: r.read_t(),
            types_scope_id: r.read_t(),
            test_scope_id: r.read_t(),
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
            use_targets: VPool::make("scope_use_targets"),
            use_idents: VPool::make("scope_use_idents"),
            use_caches: VPool::make("scope_use_caches"),
            namespace_epoch: 1,
            lambda_info: FxHashMap::default(),
            loop_info: FxHashMap::default(),
            core_scope_id: ScopeId::PENDING,
            k1_scope_id: ScopeId::PENDING,
            mem_scope_id: ScopeId::PENDING,
            types_scope_id: ScopeId::PENDING,
            test_scope_id: ScopeId::PENDING,
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
            version: 0,
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
    ) {
        let head = self.get_scope(scope_id).entries;
        let id = self.entries.add(ScopeEntry { key, prev_entry: head, provenance, payload });
        let scope = self.get_scope_mut(scope_id);
        scope.entries = Some(id);
        scope.kinds |= payload.kind();
    }

    fn entry_bind(
        &mut self,
        scope_id: ScopeId,
        key: u32,
        payload: EntryPayload,
        provenance: Provenance,
    ) -> bool {
        let kind = std::mem::discriminant(&payload);
        let head = self.get_scope(scope_id).entries;
        let free = self
            .entry_find(head, key, |e| {
                if std::mem::discriminant(&e.payload) == kind { Some(()) } else { None }
            })
            .is_none();
        if free || matches!(payload, EntryPayload::Variable(_)) {
            self.entry_push(scope_id, key, payload, provenance);
        }
        free
    }

    fn bind(
        &mut self,
        scope_id: ScopeId,
        name: StringId,
        payload: EntryPayload,
        provenance: Provenance,
    ) -> bool {
        let bound = match self.get_scope(scope_id).ns_map {
            Some(_) => {
                let entry = self.ns_entry_mut(scope_id, name);
                match payload {
                    EntryPayload::Variable(vis) => {
                        let free = !matches!(entry.global, NsEntryGlobalValue::Bound(_));
                        entry.global = NsEntryGlobalValue::Bound(vis);
                        free
                    }
                    EntryPayload::Function(f) if entry.function.is_none() => {
                        entry.function = Some(f);
                        true
                    }
                    EntryPayload::Namespace(ns_scope_id) if entry.namespace.is_none() => {
                        entry.namespace = Some((ns_scope_id, provenance));
                        true
                    }
                    EntryPayload::Type(t) if !matches!(entry.ty, NsEntryTypeValue::Defined(_)) => {
                        entry.ty = NsEntryTypeValue::Defined(t);
                        true
                    }
                    EntryPayload::PendingType(p) if matches!(entry.ty, NsEntryTypeValue::None) => {
                        entry.ty = NsEntryTypeValue::Pending(p);
                        true
                    }
                    EntryPayload::PendingType(_) => {
                        matches!(entry.ty, NsEntryTypeValue::Defined(_))
                    }
                    EntryPayload::Ability(a)
                        if !matches!(entry.ability, NsEntryAbilityValue::Defined(_)) =>
                    {
                        entry.ability = NsEntryAbilityValue::Defined(a);
                        true
                    }
                    EntryPayload::PendingAbility(p)
                        if matches!(entry.ability, NsEntryAbilityValue::None) =>
                    {
                        entry.ability = NsEntryAbilityValue::Pending(p);
                        true
                    }
                    EntryPayload::PendingAbility(_) => {
                        matches!(entry.ability, NsEntryAbilityValue::Defined(_))
                    }
                    EntryPayload::PendingGlobal(p)
                        if matches!(entry.global, NsEntryGlobalValue::None) =>
                    {
                        entry.global = NsEntryGlobalValue::Pending(p);
                        true
                    }
                    _ => false,
                }
            }
            None => self.entry_bind(scope_id, name.as_u32(), payload, provenance),
        };
        self.get_scope_mut(scope_id).kinds |= payload.kind();
        bound
    }

    fn scope_entries(&self, head: Option<ScopeEntryId>) -> impl Iterator<Item = ScopeEntry> + '_ {
        std::iter::successors(head, move |id| self.entries.get(*id).prev_entry)
            .map(move |id| *self.entries.get(id))
    }

    fn ns_entry(&self, map_id: NsMapId, name: StringId) -> Option<&NsEntry> {
        self.ns_maps.get(map_id).get(&name)
    }

    fn ns_entry_mut(&mut self, scope_id: ScopeId, name: StringId) -> &mut NsEntry {
        let scope = self.scopes.get_mut(scope_id);
        scope.version += 1;
        let map_id = scope.ns_map.unwrap();
        self.ns_maps.get_mut(map_id).entry(name).or_default()
    }

    /// Walk the scope chain from `scope_id` to the root, probing scopes whose
    /// `kinds` contains `kind` with `f`. Returns the first hit and its scope.
    #[inline]
    fn walk_chain<V>(
        &self,
        scope_id: ScopeId,
        kind: u16,
        mut f: impl FnMut(ScopeId) -> Option<V>,
    ) -> Option<(V, ScopeId)> {
        let kind = kind | kinds::USES;
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

    fn find_entry<V>(
        &self,
        scope_id: ScopeId,
        name: StringId,
        following: Option<&Following>,
        slot: fn(&NsEntry) -> Option<(V, Provenance)>,
        payload: fn(&ScopeEntry) -> Option<(V, Provenance)>,
    ) -> Option<(V, Provenance, ScopeId)> {
        let scope = self.get_scope(scope_id);
        let head = match scope.ns_map {
            Some(m) => {
                let entry = self.ns_entry(m, name)?;
                if let Some((v, p)) = slot(entry) {
                    return Some((v, p, scope_id));
                }
                entry.uses
            }
            None => scope.entries,
        };
        self.find_chain_entry(scope_id, name, head, following, slot, payload)
    }

    #[inline]
    fn find_chain_entry<V>(
        &self,
        scope_id: ScopeId,
        name: StringId,
        head: Option<ScopeEntryId>,
        following: Option<&Following>,
        slot: fn(&NsEntry) -> Option<(V, Provenance)>,
        payload: fn(&ScopeEntry) -> Option<(V, Provenance)>,
    ) -> Option<(V, Provenance, ScopeId)> {
        self.entry_find(head, name.as_u32(), |e| match e.payload {
            EntryPayload::Use(target_id) => self
                .follow_use(scope_id, target_id, following, slot, payload)
                .map(|(v, _, found)| (v, e.provenance, found)),
            _ => payload(e).map(|(v, p)| (v, p, scope_id)),
        })
    }

    fn follow_use<V>(
        &self,
        scope_id: ScopeId,
        target_id: UseTargetId,
        following: Option<&Following>,
        slot: fn(&NsEntry) -> Option<(V, Provenance)>,
        payload: fn(&ScopeEntry) -> Option<(V, Provenance)>,
    ) -> Option<(V, Provenance, ScopeId)> {
        if following.is_some_and(|f| f.contains(target_id)) {
            return None;
        }
        let target = self.use_targets.get(target_id);
        let top_level = following.is_none();
        let following = Following { target: target_id, outer: following };
        let cell = self.use_caches.get(target_id);
        let mut cached = cell.get();
        let current = top_level
            && cached.epoch == self.namespace_epoch
            && cached.scope_id.is_none_or(|t| self.get_scope(t).version == cached.version);
        if !current {
            let path = self.use_idents.get_slice(target.path).iter().copied();
            let target_scope =
                self.find_path_scope(scope_id, path, Some(&following), |_, _| {}).ok();
            cached = UseCache { epoch: self.namespace_epoch, ..UseCache::default() };
            if let Some(target_scope) = target_scope {
                let scope = self.get_scope(target_scope);
                cached.scope_id = Some(target_scope);
                cached.version = scope.version;
                cached.entry = scope
                    .ns_map
                    .map(|m| self.ns_entry(m, target.name).copied().unwrap_or_default());
            }
            if top_level {
                cell.set(cached);
            }
        }
        let target_scope = cached.scope_id?;
        let found = match cached.entry {
            Some(ns_entry) => match slot(&ns_entry) {
                Some((v, p)) => Some((v, p, target_scope)),
                None => self.find_chain_entry(
                    target_scope,
                    target.name,
                    ns_entry.uses,
                    Some(&following),
                    slot,
                    payload,
                ),
            },
            None => self.find_entry(target_scope, target.name, Some(&following), slot, payload),
        };
        found.filter(|(_, p, _)| p.is_exposed())
    }

    fn find_path_scope(
        &self,
        scope_id: ScopeId,
        path: impl Iterator<Item = StringId>,
        following: Option<&Following>,
        mut on_segment: impl FnMut(usize, ScopeId),
    ) -> Result<ScopeId, (usize, ScopeId)> {
        let mut cur_scope_id = scope_id;
        for (index, segment) in path.enumerate() {
            let found = if index == 0 {
                self.walk_chain(cur_scope_id, kinds::NAMESPACES, |sid| {
                    self.find_namespace_scope_entry(sid, segment, following)
                })
                .map(|((ns_scope_id, _), _)| ns_scope_id)
            } else {
                exposed(self.find_namespace_scope_entry(cur_scope_id, segment, following))
            };
            let Some(found) = found else { return Err((index, cur_scope_id)) };
            on_segment(index, found);
            cur_scope_id = found;
        }
        Ok(cur_scope_id)
    }

    pub fn add_use(
        &mut self,
        scope_id: ScopeId,
        name: StringId,
        use_id: ParsedUseId,
        path: impl Iterator<Item = StringId>,
        target_name: StringId,
        provenance: Provenance,
    ) {
        let path = self.use_idents.add_slice_from_iter(path);
        let target_id = self.use_targets.add(UseTarget { use_id, path, name: target_name });
        self.use_caches.add(Cell::default());
        self.namespace_epoch += 1;
        let payload = EntryPayload::Use(target_id);
        match self.get_scope(scope_id).ns_map {
            Some(_) => {
                let prev_entry = self.ns_entry_mut(scope_id, name).uses;
                let entry = ScopeEntry { key: name.as_u32(), prev_entry, provenance, payload };
                let id = self.entries.add(entry);
                self.ns_entry_mut(scope_id, name).uses = Some(id);
                self.get_scope_mut(scope_id).kinds |= kinds::USES;
            }
            None => self.entry_push(scope_id, name.as_u32(), payload, provenance),
        }
    }

    pub fn find_namespace_local(&self, scope_id: ScopeId, ident: StringId) -> Option<NamespaceId> {
        self.find_namespace_entry(scope_id, ident).map(|(ns, _)| ns)
    }

    pub fn find_namespace_entry(
        &self,
        scope_id: ScopeId,
        ident: StringId,
    ) -> Option<(NamespaceId, Provenance)> {
        self.find_namespace_scope_entry(scope_id, ident, None)
            .map(|(ns_scope_id, p)| (self.nearest_parent_namespace(ns_scope_id), p))
    }

    fn find_namespace_scope_entry(
        &self,
        scope_id: ScopeId,
        ident: StringId,
        following: Option<&Following>,
    ) -> Option<(ScopeId, Provenance)> {
        self.find_entry(
            scope_id,
            ident,
            following,
            |e| e.namespace,
            |e| match e.payload {
                EntryPayload::Namespace(ns_scope_id) => Some((ns_scope_id, e.provenance)),
                _ => None,
            },
        )
        .map(|(ns_scope_id, p, _)| (ns_scope_id, p))
    }

    #[must_use]
    pub fn add_namespace(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        ns_scope_id: ScopeId,
        provenance: Provenance,
    ) -> bool {
        self.namespace_epoch += 1;
        self.bind(scope_id, ident, EntryPayload::Namespace(ns_scope_id), provenance)
    }

    pub fn replace_namespace(&mut self, scope_id: ScopeId, ident: StringId, ns_scope_id: ScopeId) {
        self.namespace_epoch += 1;
        match self.get_scope(scope_id).ns_map {
            Some(_) => {
                self.ns_entry_mut(scope_id, ident).namespace =
                    Some((ns_scope_id, Provenance::Defined))
            }
            None => self.entry_push(
                scope_id,
                ident.as_u32(),
                EntryPayload::Namespace(ns_scope_id),
                Provenance::Defined,
            ),
        }
    }

    pub fn find_function(&self, scope: ScopeId, ident: StringId) -> Option<FunctionId> {
        self.walk_chain(scope, kinds::FUNCTIONS, |sid| self.find_function_local(sid, ident))
            .map(|(v, _)| v)
    }

    pub fn find_call_binding(&self, scope_id: ScopeId, ident: StringId) -> Option<CallBinding> {
        let kind = kinds::FUNCTIONS | kinds::VARIABLES | kinds::PENDING_GLOBALS;
        self.walk_chain(scope_id, kind, |sid| {
            let (binding, _, _) = self.find_entry(
                sid,
                ident,
                None,
                |e| match (e.function, e.global) {
                    (Some(f), _) => Some((Some(CallBinding::Function(f)), Provenance::Defined)),
                    (None, NsEntryGlobalValue::Bound(VariableInScope::Masked)) => {
                        Some((None, Provenance::Defined))
                    }
                    (None, NsEntryGlobalValue::Bound(_) | NsEntryGlobalValue::Pending(_)) => {
                        Some((Some(CallBinding::Value), Provenance::Defined))
                    }
                    (None, NsEntryGlobalValue::None) => None,
                },
                |e| match e.payload {
                    EntryPayload::Function(f) => {
                        Some((Some(CallBinding::Function(f)), e.provenance))
                    }
                    EntryPayload::Variable(VariableInScope::Masked) => Some((None, e.provenance)),
                    EntryPayload::Variable(_) | EntryPayload::PendingGlobal(_) => {
                        Some((Some(CallBinding::Value), e.provenance))
                    }
                    _ => None,
                },
            )?;
            Some(binding.unwrap_or_else(|| {
                self.find_function(sid, ident).map_or(CallBinding::Value, CallBinding::Function)
            }))
        })
        .map(|(b, _)| b)
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
        self.find_entry(
            scope_id,
            ident,
            None,
            |e| e.function.map(|f| (f, Provenance::Defined)),
            |e| match e.payload {
                EntryPayload::Function(f) => Some((f, e.provenance)),
                _ => None,
            },
        )
        .map(|(f, p, _)| (f, p))
    }

    #[must_use]
    pub fn add_function(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        function_id: FunctionId,
    ) -> bool {
        self.bind(scope_id, ident, EntryPayload::Function(function_id), Provenance::Defined)
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
        self.find_entry(
            scope_id,
            ident,
            None,
            |e| match e.ty {
                NsEntryTypeValue::Defined(t) => Some((t, Provenance::Defined)),
                NsEntryTypeValue::None | NsEntryTypeValue::Pending(_) => None,
            },
            |e| match e.payload {
                EntryPayload::Type(t) => Some((t, e.provenance)),
                _ => None,
            },
        )
        .map(|(t, p, _)| (t, p))
    }

    #[must_use]
    pub fn add_type(&mut self, scope_id: ScopeId, ident: StringId, ty: TypeId) -> bool {
        self.bind(scope_id, ident, EntryPayload::Type(ty), Provenance::Defined)
    }

    #[must_use]
    pub fn add_pending_type(
        &mut self,
        scope_id: ScopeId,
        name: StringId,
        parsed_id: ParsedTypeDefnId,
    ) -> bool {
        self.bind(scope_id, name, EntryPayload::PendingType(parsed_id), Provenance::Defined)
    }

    pub fn find_pending_type(
        &self,
        scope_id: ScopeId,
        name: StringId,
    ) -> Option<(ParsedTypeDefnId, ScopeId)> {
        self.walk_chain(scope_id, kinds::PENDING_TYPES, |sid| {
            self.find_pending_type_local(sid, name)
        })
        .map(|((pending, _), _)| pending)
    }

    pub fn find_pending_type_local(
        &self,
        scope_id: ScopeId,
        name: StringId,
    ) -> Option<((ParsedTypeDefnId, ScopeId), Provenance)> {
        self.find_entry(
            scope_id,
            name,
            None,
            |e| match e.ty {
                NsEntryTypeValue::Pending(parsed_id) => Some((parsed_id, Provenance::Defined)),
                NsEntryTypeValue::None | NsEntryTypeValue::Defined(..) => None,
            },
            |e| match e.payload {
                EntryPayload::PendingType(parsed_id) => Some((parsed_id, e.provenance)),
                _ => None,
            },
        )
        .map(|(parsed_id, p, defn_scope_id)| ((parsed_id, defn_scope_id), p))
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
        self.find_entry(
            scope_id,
            name,
            None,
            |e| match e.ability {
                NsEntryAbilityValue::Defined(a) => Some((a, Provenance::Defined)),
                NsEntryAbilityValue::None | NsEntryAbilityValue::Pending(_) => None,
            },
            |e| match e.payload {
                EntryPayload::Ability(a) => Some((a, e.provenance)),
                _ => None,
            },
        )
        .map(|(a, p, _)| (a, p))
    }

    #[must_use]
    pub fn add_ability(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        ability_id: AbilityId,
    ) -> bool {
        self.bind(scope_id, ident, EntryPayload::Ability(ability_id), Provenance::Defined)
    }

    #[must_use]
    pub fn add_pending_ability_defn(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        parsed_defn_id: ParsedAbilityId,
    ) -> bool {
        self.bind(
            scope_id,
            ident,
            EntryPayload::PendingAbility(parsed_defn_id),
            Provenance::Defined,
        )
    }

    pub fn find_pending_ability(
        &self,
        scope_id: ScopeId,
        ident: StringId,
    ) -> Option<(ParsedAbilityId, ScopeId)> {
        self.walk_chain(scope_id, kinds::PENDING_ABILITIES, |sid| {
            self.find_pending_ability_local(sid, ident)
        })
        .map(|((pending, _), _)| pending)
    }

    pub fn find_pending_ability_local(
        &self,
        scope_id: ScopeId,
        ident: StringId,
    ) -> Option<((ParsedAbilityId, ScopeId), Provenance)> {
        self.find_entry(
            scope_id,
            ident,
            None,
            |e| match e.ability {
                NsEntryAbilityValue::Pending(parsed_id) => Some((parsed_id, Provenance::Defined)),
                NsEntryAbilityValue::None | NsEntryAbilityValue::Defined(..) => None,
            },
            |e| match e.payload {
                EntryPayload::PendingAbility(id) => Some((id, e.provenance)),
                _ => None,
            },
        )
        .map(|(parsed_id, p, defn_scope_id)| ((parsed_id, defn_scope_id), p))
    }

    pub fn collect_ability_ids_bound_to_names(
        &self,
        scope_id: ScopeId,
        names: &[StringId],
        ability_ids: &mut List<AbilityId, MemTmp>,
        mem: &mut Mem<MemTmp>,
    ) {
        self.walk_chain(scope_id, kinds::ABILITIES, |sid| -> Option<()> {
            for name in names {
                if let Some(found) = self.find_ability_local(sid, *name)
                    && !ability_ids.contains(&found)
                {
                    ability_ids.push_grow(mem, found);
                }
            }
            None
        });
    }

    pub fn add_pending_global(
        &mut self,
        scope_id: ScopeId,
        name: StringId,
        parsed_id: ParsedGlobalId,
    ) {
        self.bind(scope_id, name, EntryPayload::PendingGlobal(parsed_id), Provenance::Defined);
    }

    pub fn find_pending_global(
        &self,
        scope_id: ScopeId,
        name: StringId,
    ) -> Option<(ParsedGlobalId, ScopeId)> {
        self.walk_chain(scope_id, kinds::PENDING_GLOBALS, |sid| {
            self.find_pending_global_local(sid, name)
        })
        .map(|((pending, _), _)| pending)
    }

    pub fn find_pending_global_local(
        &self,
        scope_id: ScopeId,
        name: StringId,
    ) -> Option<((ParsedGlobalId, ScopeId), Provenance)> {
        self.find_entry(
            scope_id,
            name,
            None,
            |e| match e.global {
                NsEntryGlobalValue::Pending(parsed_id) => Some((parsed_id, Provenance::Defined)),
                NsEntryGlobalValue::None | NsEntryGlobalValue::Bound(..) => None,
            },
            |e| match e.payload {
                EntryPayload::PendingGlobal(id) => Some((id, e.provenance)),
                _ => None,
            },
        )
        .map(|(parsed_id, p, defn_scope_id)| ((parsed_id, defn_scope_id), p))
    }

    pub fn find_variable_binding(
        &self,
        scope_id: ScopeId,
        ident: StringId,
    ) -> Option<(VariableInScope, ScopeId)> {
        self.walk_chain(scope_id, kinds::VARIABLES, |sid| self.find_variable_local(sid, ident))
    }

    pub fn find_variable(
        &self,
        scope_id: ScopeId,
        ident: StringId,
    ) -> Option<(VariableId, ScopeId)> {
        let (in_scope, found_scope) = self.find_variable_binding(scope_id, ident)?;
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
        self.find_entry(
            scope_id,
            ident,
            None,
            |e| match e.global {
                NsEntryGlobalValue::Bound(vis) => Some((vis, Provenance::Defined)),
                NsEntryGlobalValue::None | NsEntryGlobalValue::Pending(_) => None,
            },
            |e| match e.payload {
                EntryPayload::Variable(vis) => Some((vis, e.provenance)),
                _ => None,
            },
        )
        .map(|(vis, p, _)| (vis, p))
    }

    pub fn add_variable(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        variable_id: VariableId,
    ) -> bool {
        let payload = EntryPayload::Variable(VariableInScope::Defined(variable_id));
        self.bind(scope_id, ident, payload, Provenance::Defined)
    }

    pub fn mask_variable(&mut self, scope_id: ScopeId, ident: StringId) {
        let payload = EntryPayload::Variable(VariableInScope::Masked);
        self.bind(scope_id, ident, payload, Provenance::Defined);
    }

    pub fn add_context_variable(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        variable_id: VariableId,
        type_id: TypeId,
    ) -> bool {
        let payload = EntryPayload::ContextByType(variable_id);
        if !self.entry_bind(scope_id, type_id.as_u32(), payload, Provenance::Defined) {
            return false;
        }
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
        let payload = EntryPayload::ContextByAbility(ContextAbilityEntry::Unique(variable_id));
        self.entry_bind(scope_id, ability_id.as_u32(), payload, Provenance::Defined)
    }

    pub fn poison_context_ability_key(&mut self, scope_id: ScopeId, ability_id: AbilityId) {
        self.entry_push(
            scope_id,
            ability_id.as_u32(),
            EntryPayload::ContextByAbility(ContextAbilityEntry::Ambiguous),
            Provenance::Defined,
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

    pub fn add_context_capture_abilities(
        &mut self,
        capture_site: ScopeId,
        source: VariableId,
        lambda_scope: ScopeId,
        binding: VariableId,
    ) {
        let mut scope_id = capture_site;
        loop {
            let scope = *self.get_scope(scope_id);
            let mut cur =
                if scope.kinds & kinds::CONTEXT_VARIABLES != 0 { scope.entries } else { None };
            while let Some(id) = cur {
                let entry = *self.entries.get(id);
                cur = entry.prev_entry;
                if let EntryPayload::ContextByAbility(ContextAbilityEntry::Unique(v)) =
                    entry.payload
                    && v == source
                    && let Some(ability_id) = AbilityId::from_u32(entry.key)
                    && let Some(ContextAbilityEntry::Unique(nearest)) =
                        self.find_context_variable_by_ability(capture_site, ability_id)
                    && nearest == source
                {
                    self.add_context_variable_by_ability(lambda_scope, ability_id, binding);
                }
            }
            if scope.scope_type == ScopeType::Namespace {
                return;
            }
            let Some(parent) = scope.parent else { return };
            scope_id = parent;
        }
    }

    pub fn add_type_substitution(&mut self, scope_id: ScopeId, from: TypeId, to: TypeId) -> bool {
        self.entry_bind(scope_id, from.as_u32(), EntryPayload::TypeSubst(to), Provenance::Defined)
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

    pub fn iter_scope_members(
        &self,
        scope_id: ScopeId,
    ) -> impl Iterator<Item = (StringId, ScopeMember, Provenance)> + '_ {
        let scope = self.get_scope(scope_id);
        let map = scope.ns_map.map(|m| {
            self.ns_maps.get(m).keys().flat_map(move |name| {
                self.find_members(scope_id, *name)
                    .into_iter()
                    .flatten()
                    .map(move |(member, p)| (*name, member, p))
            })
        });
        let list = self.scope_entries(scope.entries).flat_map(move |e| {
            let name = StringId::from_u32(e.key).unwrap();
            let member = match e.payload {
                EntryPayload::Variable(v) => Some(ScopeMember::Variable(v)),
                EntryPayload::Function(f) => Some(ScopeMember::Function(f)),
                EntryPayload::Type(t) => Some(ScopeMember::Type(t)),
                EntryPayload::Namespace(ns_scope_id) => {
                    Some(ScopeMember::Namespace(self.nearest_parent_namespace(ns_scope_id)))
                }
                EntryPayload::Ability(a) => Some(ScopeMember::Ability(a)),
                _ => None,
            };
            let used = match e.payload {
                EntryPayload::Use(_) => self.find_members(scope_id, name),
                _ => [None; 5],
            };
            let used = used.into_iter().flatten().filter(|(_, p)| *p != Provenance::Defined);
            member
                .map(|member| (member, e.provenance))
                .into_iter()
                .chain(used)
                .map(move |(member, p)| (name, member, p))
        });
        map.into_iter().flatten().chain(list)
    }

    fn find_members(
        &self,
        scope_id: ScopeId,
        name: StringId,
    ) -> [Option<(ScopeMember, Provenance)>; 5] {
        [
            self.find_variable_entry(scope_id, name).map(|(v, p)| (ScopeMember::Variable(v), p)),
            self.find_function_entry(scope_id, name).map(|(f, p)| (ScopeMember::Function(f), p)),
            self.find_type_entry(scope_id, name).map(|(t, p)| (ScopeMember::Type(t), p)),
            self.find_namespace_entry(scope_id, name)
                .map(|(ns, p)| (ScopeMember::Namespace(ns), p)),
            self.find_ability_entry(scope_id, name).map(|(a, p)| (ScopeMember::Ability(a), p)),
        ]
    }

    pub fn iter_scope_variables(
        &self,
        scope_id: ScopeId,
    ) -> impl Iterator<Item = (StringId, VariableInScope, Provenance)> + '_ {
        self.iter_scope_members(scope_id).filter_map(|(name, member, p)| match member {
            ScopeMember::Variable(v) => Some((name, v, p)),
            _ => None,
        })
    }

    pub fn iter_scope_functions(
        &self,
        scope_id: ScopeId,
    ) -> impl Iterator<Item = (StringId, FunctionId, Provenance)> + '_ {
        self.iter_scope_members(scope_id).filter_map(|(name, member, p)| match member {
            ScopeMember::Function(f) => Some((name, f, p)),
            _ => None,
        })
    }

    pub fn iter_scope_types(
        &self,
        scope_id: ScopeId,
    ) -> impl Iterator<Item = (StringId, TypeId, Provenance)> + '_ {
        self.iter_scope_members(scope_id).filter_map(|(name, member, p)| match member {
            ScopeMember::Type(t) => Some((name, t, p)),
            _ => None,
        })
    }

    pub fn iter_scope_namespaces(
        &self,
        scope_id: ScopeId,
    ) -> impl Iterator<Item = (StringId, NamespaceId, Provenance)> + '_ {
        self.iter_scope_members(scope_id).filter_map(|(name, member, p)| match member {
            ScopeMember::Namespace(ns) => Some((name, ns, p)),
            _ => None,
        })
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
        if !name.has_path() {
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
        if !name.has_path() {
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
        if !type_name.has_path() {
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

    pub fn resolve_qident(&self, scope_id: ScopeId, qident: &QIdent) -> K1Result<ScopeId> {
        let path = self.ast.mem.getn(qident.path(&self.ast.mem));
        self.scopes
            .find_path_scope(scope_id, path.iter().map(|ident| ident.name), None, |index, found| {
                self.emit_path_segment(path[index], found)
            })
            .map_err(|miss| self.make_path_miss_error(path, miss))
    }

    pub fn bind_use(&mut self, scope_id: ScopeId, use_id: ParsedUseId) {
        let parsed_use = *self.ast.uses.get(use_id);
        let provenance = if parsed_use.exposed { Provenance::UseExposed } else { Provenance::Use };
        let path = self.ast.mem.getn(parsed_use.target.path(&self.ast.mem));
        self.scopes.add_use(
            scope_id,
            parsed_use.alias.unwrap_or(parsed_use.target.name),
            use_id,
            path.iter().map(|ident| ident.name),
            parsed_use.target.name,
            provenance,
        );
    }

    pub fn check_use(&mut self, scope_id: ScopeId, use_id: ParsedUseId) -> K1Result<()> {
        let parsed_use = *self.ast.uses.get(use_id);
        let target = parsed_use.target;
        if self.string_is_completion_marker(target.name, false) {
            self.record_qident_completion_site(scope_id, &target);
            return Ok(());
        }
        let path = self.ast.mem.getn(target.path(&self.ast.mem));
        let name = parsed_use.alias.unwrap_or(target.name);
        // Resolve the use's path without following the use itself
        let scopes = &self.scopes;
        let scope = scopes.get_scope(scope_id);
        let head = match scope.ns_map {
            Some(m) => scopes.ns_entry(m, name).and_then(|e| e.uses),
            None => scope.entries,
        };
        let target_id = scopes
            .entry_find(head, name.as_u32(), |e| match e.payload {
                EntryPayload::Use(t) if scopes.use_targets.get(t).use_id == use_id => Some(t),
                _ => None,
            })
            .unwrap();
        let following = Following { target: target_id, outer: None };
        let segments = path.iter().map(|ident| ident.name);
        let target_scope_id = scopes
            .find_path_scope(scope_id, segments, Some(&following), |index, found| {
                self.emit_path_segment(path[index], found)
            })
            .map_err(|miss| self.make_path_miss_error(path, miss))?;
        let names_something = scopes
            .find_entry(
                target_scope_id,
                target.name,
                Some(&following),
                |e| {
                    let defined = e.function.is_some()
                        || !matches!(e.ty, NsEntryTypeValue::None)
                        || !matches!(e.ability, NsEntryAbilityValue::None)
                        || !matches!(e.global, NsEntryGlobalValue::None);
                    if defined {
                        Some(((), Provenance::Defined))
                    } else {
                        e.namespace.filter(|(_, p)| p.is_exposed()).map(|(_, p)| ((), p))
                    }
                },
                |e| match e.payload {
                    EntryPayload::Variable(_)
                    | EntryPayload::Function(_)
                    | EntryPayload::Type(_)
                    | EntryPayload::Namespace(_)
                    | EntryPayload::Ability(_)
                    | EntryPayload::PendingType(_)
                    | EntryPayload::PendingAbility(_)
                    | EntryPayload::PendingGlobal(_) => Some(((), e.provenance)),
                    _ => None,
                },
            )
            .is_some_and(|(_, p, _)| p.is_exposed());
        if !names_something {
            kbail!(self, target.name_span, "Unresolved use of {}", target.name);
        }
        Ok(())
    }

    fn emit_path_segment(&self, segment: IdentSpanned, ns_scope_id: ScopeId) {
        let namespace_id = self.scopes.nearest_parent_namespace(ns_scope_id);
        self.emit_ls_entity(segment.span, LsEntityKind::Namespace(namespace_id));
    }

    fn make_path_miss_error(
        &self,
        path: &[IdentSpanned],
        (index, scope_id): (usize, ScopeId),
    ) -> K1Message {
        let from_or_in = if index == 0 { "from" } else { "in" };
        kerr!(
            self,
            path[index].span,
            "Namespace not found: {} {} namespace: {}",
            path[index].name,
            from_or_in,
            self.namespace_id_to_string(self.scopes.nearest_parent_namespace(scope_id))
        )
    }

    pub fn find_ability_namespaced(
        &self,
        scope_id: ScopeId,
        ability_name: &QIdent,
    ) -> K1Result<Option<AbilityId>> {
        // Unqualified mentions are implicitly recursive searches
        // But qualified mentions imply that the targeted symbol lives directly at the given path!
        if !ability_name.has_path() {
            Ok(self.scopes.find_ability(scope_id, ability_name.name))
        } else {
            let scope_to_search = self.resolve_qident(scope_id, ability_name)?;
            Ok(self.scopes.find_ability_exposed(scope_to_search, ability_name.name))
        }
    }

    pub fn find_pending_ability_namespaced(
        &self,
        scope_id: ScopeId,
        ability_name: &QIdent,
    ) -> K1Result<Option<(ParsedAbilityId, ScopeId)>> {
        if !ability_name.has_path() {
            Ok(self.scopes.find_pending_ability(scope_id, ability_name.name))
        } else {
            let scope_to_search = self.resolve_qident(scope_id, ability_name)?;
            Ok(exposed(self.scopes.find_pending_ability_local(scope_to_search, ability_name.name)))
        }
    }

    pub fn find_pending_type_namespaced(
        &self,
        scope_id: ScopeId,
        type_name: &QIdent,
    ) -> K1Result<Option<(ParsedTypeDefnId, ScopeId)>> {
        // Unqualified mentions are implicitly recursive searches
        // But qualified mentions imply that the targeted symbol lives directly at the given path!
        if !type_name.has_path() {
            Ok(self.scopes.find_pending_type(scope_id, type_name.name))
        } else {
            let scope_to_search = self.resolve_qident(scope_id, type_name)?;
            Ok(exposed(self.scopes.find_pending_type_local(scope_to_search, type_name.name)))
        }
    }

    pub fn find_pending_global_namespaced(
        &self,
        scope_id: ScopeId,
        name: &QIdent,
    ) -> K1Result<Option<(ParsedGlobalId, ScopeId)>> {
        if !name.has_path() {
            Ok(self.scopes.find_pending_global(scope_id, name.name))
        } else {
            let scope_to_search = self.resolve_qident(scope_id, name)?;
            Ok(exposed(self.scopes.find_pending_global_local(scope_to_search, name.name)))
        }
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
    pub fn as_namespace(&self) -> Option<NamespaceId> {
        match self {
            ScopeOwnerId::Namespace(ns_id) => Some(*ns_id),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ScopeMember {
    Variable(VariableInScope),
    Function(FunctionId),
    Type(TypeId),
    Namespace(NamespaceId),
    Ability(AbilityId),
}

#[derive(Debug, Clone, Copy)]
pub enum CallBinding {
    Function(FunctionId),
    Value,
}

#[derive(Debug, Clone, Copy)]
pub enum VariableInScope {
    Masked,
    Defined(VariableId),
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
    version: u32,
}
static_assert_size!(Scope, 40);

impl Scope {
    pub fn clear_nearest_lambda(&mut self) {
        self.nearest_lambda = None;
    }
}
