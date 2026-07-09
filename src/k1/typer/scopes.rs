// Copyright (c) 2026 knix
// All rights reserved.

use ahash::HashMapExt;
use fxhash::FxHashMap;
use smallvec::SmallVec;

use std::{collections::hash_map::Entry, fmt::Display, num::NonZeroU32};

use crate::{
    SV4, errf,
    kmem::Dlist,
    nz_u32_id,
    parse::{ParsedAbilityId, ParsedExprId, QIdent},
    static_assert_niched, static_assert_size,
    typer::{
        AbilityId, FunctionId, K1Result, LoopType, LsEntityKind, MemTmp, NamespaceId, StringId,
        TypeId, TypePendingDefinition, TypedExprId, TypedProgram, VariableId,
    },
    vpool::VPool,
};

nz_u32_id!(ScopeId);
static_assert_niched!(ScopeId);

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

impl ScopeEnclosingFunctions {
    pub fn empty() -> Self {
        ScopeEnclosingFunctions { lambda_scope: None, function: None }
    }
}

pub struct ScopeLambdaInfo {
    pub expected_return_type: Option<TypeId>,
    pub capture_exprs_for_fixup: SmallVec<[TypedExprId; 8]>,
    pub captured_variables: SmallVec<[VariableId; 8]>,
    // We have to store this here, instead of on the function, since no function
    // declaration exists while we're evaluating a lambda body
    pub returned_variable: Option<VariableId>,
}

pub struct ScopeLoopInfo {
    pub break_type: Option<TypeId>,
}

pub struct ScopeDefers {
    pub deferred_exprs: SV4<ParsedExprId>,
}

/// A packed (scope, symbol) key for the global per-kind symbol maps
type ScopeKey = u64;

#[inline]
fn skey(scope: ScopeId, sym: u32) -> ScopeKey {
    ((scope.as_u32() as u64) << 32) | sym as u64
}

#[inline]
fn skey_name(scope: ScopeId, name: StringId) -> ScopeKey {
    skey(scope, name.as_usize() as u32)
}

#[inline]
fn skey_scope_part(key: ScopeKey) -> u32 {
    (key >> 32) as u32
}

#[inline]
fn skey_name_part(key: ScopeKey) -> StringId {
    StringId::from_usize((key & 0xFFFF_FFFF) as usize)
}

/// Bits for `Scope::kinds`: which symbol kinds a scope has at least one entry
/// of. Chain walks test the bit before probing the global maps, so scopes that
/// contain nothing of a kind (most of them) cost no hashing at all.
pub mod kinds {
    pub const CONTEXT_VARIABLES: u8 = 1 << 0;
    pub const FUNCTIONS: u8 = 1 << 1;
    pub const NAMESPACES: u8 = 1 << 2;
    pub const TYPES: u8 = 1 << 3;
    pub const ABILITIES: u8 = 1 << 4;
    pub const PENDING_TYPES: u8 = 1 << 5;
    pub const PENDING_ABILITIES: u8 = 1 << 6;
}

pub struct Scopes {
    /// SCOPES SoA POOLS BEGIN
    pub scopes: VPool<Scope, ScopeId>,
    /// The actual function that a scope appears within is a very important thing to know
    pub enclosing_functions: VPool<ScopeEnclosingFunctions, ScopeId>,
    /// SCOPES SoA POOLS END
    // Scope contents (except variables, which are hot enough that per-scope
    // locality wins) live here, keyed by (scope, symbol), so that its cheap
    // to create a scope; most scopes don't contain most kinds; they are sparse.
    // `Scope::kinds` records which maps can hit for a given scope, saving
    // hashmap lookups; since the maps are now bigger
    context_variables_by_type: FxHashMap<ScopeKey, VariableId>,
    functions: FxHashMap<ScopeKey, FunctionId>,
    namespaces: FxHashMap<ScopeKey, NamespaceId>,
    types: FxHashMap<ScopeKey, TypeId>,
    abilities: FxHashMap<ScopeKey, AbilityId>,
    pending_type_defns: FxHashMap<ScopeKey, TypePendingDefinition>,
    pending_ability_defns: FxHashMap<ScopeKey, ParsedAbilityId>,
    pub lambda_info: FxHashMap<ScopeId, ScopeLambdaInfo>,
    pub loop_info: FxHashMap<ScopeId, ScopeLoopInfo>,
    pub block_defers: FxHashMap<ScopeId, ScopeDefers>,
    pub core_scope_id: ScopeId,
    pub k1_scope_id: ScopeId,
    pub mem_scope_id: ScopeId,
    pub sys_scope_id: ScopeId,
    pub libc_scope_id: ScopeId,
    pub types_scope_id: ScopeId,
    pub array_scope_id: ScopeId,
}

impl Scopes {
    pub const ROOT_SCOPE_ID: ScopeId = ScopeId(NonZeroU32::new(1).unwrap());
    pub fn make() -> Self {
        let root_scope = Scope::make(ScopeType::Namespace, ScopeOwnerId::None);
        let mut scopes = Scopes {
            scopes: VPool::make("scopes"),
            enclosing_functions: VPool::make("scope_enclosing_functions"),
            context_variables_by_type: FxHashMap::new(),
            functions: FxHashMap::new(),
            namespaces: FxHashMap::new(),
            types: FxHashMap::new(),
            abilities: FxHashMap::new(),
            pending_type_defns: FxHashMap::new(),
            pending_ability_defns: FxHashMap::new(),
            lambda_info: FxHashMap::new(),
            loop_info: FxHashMap::new(),
            block_defers: FxHashMap::new(),
            core_scope_id: ScopeId::PENDING,
            k1_scope_id: ScopeId::PENDING,
            mem_scope_id: ScopeId::PENDING,
            sys_scope_id: ScopeId::PENDING,
            libc_scope_id: ScopeId::PENDING,
            types_scope_id: ScopeId::PENDING,
            array_scope_id: ScopeId::PENDING,
        };
        let id =
            scopes.add(root_scope, ScopeEnclosingFunctions { lambda_scope: None, function: None });
        debug_assert_eq!(id, Self::ROOT_SCOPE_ID);
        scopes
    }

    fn add(&mut self, scope: Scope, enclosing: ScopeEnclosingFunctions) -> ScopeId {
        let id = self.scopes.add(scope);
        let id2 = self.enclosing_functions.add(enclosing);
        debug_assert_eq!(id, id2);
        id
    }

    pub fn iter(&self) -> impl Iterator<Item = (ScopeId, &Scope)> {
        self.scopes.iter_ids().zip(self.scopes.iter())
    }

    #[inline]
    pub fn root_scope_id(&self) -> ScopeId {
        Self::ROOT_SCOPE_ID
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
        let mut scope = Scope::make(scope_type, scope_owner_id);
        scope.parent = Some(parent_scope_id);

        let id = self.add(scope, ScopeEnclosingFunctions::empty());

        // nocommit: I wonder if this is wasted work; we do it for _every single scope_.
        // Yes we want it cached, but this is probably bad, and its extra bookkeeping = bug surface
        // area
        let enclosing_lambda_scope = self.nearest_parent_lambda(id);
        let enclosing_function = self.nearest_parent_function(id);

        *self.enclosing_functions.get_mut(id) = ScopeEnclosingFunctions {
            lambda_scope: enclosing_lambda_scope,
            function: enclosing_function,
        };

        id
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
        // The enclosing-function info is derived from owners at scope
        // creation, but function scopes get their owner assigned only after
        // creation, so refresh it. This matters since function bodies are
        // evaluated directly in the function's scope (no separate body scope).
        *self.enclosing_functions.get_mut(id) = ScopeEnclosingFunctions {
            lambda_scope: self.nearest_parent_lambda(id),
            function: self.nearest_parent_function(id),
        };
    }

    pub fn get_scope_owner(&self, scope_id: ScopeId) -> ScopeOwnerId {
        self.get_scope(scope_id).owner_id
    }

    pub fn find_namespace(&self, scope: ScopeId, ident: StringId) -> Option<NamespaceId> {
        self.walk_chain(scope, kinds::NAMESPACES, |sid| {
            self.namespaces.get(&skey_name(sid, ident)).copied()
        })
        .map(|(v, _)| v)
    }

    pub fn find_namespace_local(&self, scope: ScopeId, ident: StringId) -> Option<NamespaceId> {
        self.namespaces.get(&skey_name(scope, ident)).copied()
    }

    pub fn find_ability(&self, scope_id: ScopeId, name: StringId) -> Option<AbilityId> {
        self.walk_chain(scope_id, kinds::ABILITIES, |sid| {
            self.abilities.get(&skey_name(sid, name)).copied()
        })
        .map(|(v, _)| v)
    }

    pub fn find_ability_local(&self, scope_id: ScopeId, name: StringId) -> Option<AbilityId> {
        self.abilities.get(&skey_name(scope_id, name)).copied()
    }

    /// Note: name-based, so an ability brought into scope only under a `use`
    /// alias won't be found by its canonical name
    pub fn is_ability_id_in_scope(
        &self,
        scope_id: ScopeId,
        name: StringId,
        target_ability_id: AbilityId,
    ) -> bool {
        self.walk_chain(scope_id, kinds::ABILITIES, |sid| {
            match self.abilities.get(&skey_name(sid, name)) {
                Some(found) if *found == target_ability_id => Some(()),
                _ => None,
            }
        })
        .is_some()
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
                    self.context_variables_by_type.get(&skey(scope_id, type_id.as_u32()))
            {
                return Some(*v);
            }
            // Context variables are only ever function params or `let context`s
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

    pub fn find_variable(
        &self,
        scope_id: ScopeId,
        ident: StringId,
    ) -> Option<(VariableId, ScopeId)> {
        let mut scope_id = scope_id;
        loop {
            let scope = self.get_scope(scope_id);
            match scope.variables.get(&ident) {
                Some(VariableInScope::Defined(id)) => return Some((*id, scope_id)),
                Some(VariableInScope::Masked) => return None,
                None => {}
            }
            scope_id = scope.parent?;
        }
    }

    pub fn find_variable_local(
        &self,
        scope_id: ScopeId,
        ident: StringId,
    ) -> Option<VariableInScope> {
        self.get_scope(scope_id).variables.get(&ident).copied()
    }

    pub fn find_function(&self, scope: ScopeId, ident: StringId) -> Option<FunctionId> {
        self.walk_chain(scope, kinds::FUNCTIONS, |sid| {
            self.functions.get(&skey_name(sid, ident)).copied()
        })
        .map(|(v, _)| v)
    }

    pub fn find_function_local(&self, scope: ScopeId, ident: StringId) -> Option<FunctionId> {
        self.functions.get(&skey_name(scope, ident)).copied()
    }

    /// Walk the scope chain from `scope_id` to the root, probing scopes whose
    /// `kinds` contains `kind` with `f`. Returns the first hit and its scope.
    #[inline]
    fn walk_chain<V>(
        &self,
        scope_id: ScopeId,
        kind: u8,
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

    #[must_use]
    pub fn add_function(
        &mut self,
        scope_id: ScopeId,
        identifier: StringId,
        function_id: FunctionId,
    ) -> bool {
        let added = match self.functions.entry(skey_name(scope_id, identifier)) {
            Entry::Occupied(_) => false,
            Entry::Vacant(e) => {
                e.insert(function_id);
                true
            }
        };
        if added {
            self.get_scope_mut(scope_id).kinds |= kinds::FUNCTIONS;
        }
        added
    }

    #[must_use]
    pub fn add_type(&mut self, scope_id: ScopeId, ident: StringId, ty: TypeId) -> bool {
        let added = match self.types.entry(skey_name(scope_id, ident)) {
            Entry::Occupied(_) => false,
            Entry::Vacant(e) => {
                e.insert(ty);
                true
            }
        };
        if added {
            self.get_scope_mut(scope_id).kinds |= kinds::TYPES;
        }
        added
    }

    pub fn overwrite_type(&mut self, scope_id: ScopeId, ident: StringId, ty: TypeId) -> bool {
        self.get_scope_mut(scope_id).kinds |= kinds::TYPES;
        self.types.insert(skey_name(scope_id, ident), ty).is_some()
    }

    pub fn find_type(&self, scope_id: ScopeId, ident: StringId) -> Option<(TypeId, ScopeId)> {
        self.walk_chain(scope_id, kinds::TYPES, |sid| {
            self.types.get(&skey_name(sid, ident)).copied()
        })
    }

    pub fn find_type_local(&self, scope_id: ScopeId, ident: StringId) -> Option<TypeId> {
        self.types.get(&skey_name(scope_id, ident)).copied()
    }

    #[must_use]
    pub fn add_namespace(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        namespace_id: NamespaceId,
    ) -> bool {
        let added = match self.namespaces.entry(skey_name(scope_id, ident)) {
            Entry::Occupied(_) => false,
            Entry::Vacant(e) => {
                e.insert(namespace_id);
                true
            }
        };
        if added {
            self.get_scope_mut(scope_id).kinds |= kinds::NAMESPACES;
        }
        added
    }

    #[must_use]
    pub fn add_ability(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        ability_id: AbilityId,
    ) -> bool {
        let added = match self.abilities.entry(skey_name(scope_id, ident)) {
            Entry::Occupied(_) => false,
            Entry::Vacant(e) => {
                e.insert(ability_id);
                true
            }
        };
        if added {
            self.get_scope_mut(scope_id).kinds |= kinds::ABILITIES;
        }
        added
    }

    #[must_use]
    pub fn add_pending_type(
        &mut self,
        scope_id: ScopeId,
        name: StringId,
        pending_type: TypePendingDefinition,
    ) -> bool {
        let added = match self.pending_type_defns.entry(skey_name(scope_id, name)) {
            Entry::Occupied(_) => false,
            Entry::Vacant(e) => {
                e.insert(pending_type);
                true
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
    ) -> Option<(TypePendingDefinition, ScopeId)> {
        self.walk_chain(scope_id, kinds::PENDING_TYPES, |sid| {
            self.pending_type_defns.get(&skey_name(sid, name)).copied()
        })
    }

    pub fn find_pending_type_local(
        &self,
        scope_id: ScopeId,
        name: StringId,
    ) -> Option<TypePendingDefinition> {
        self.pending_type_defns.get(&skey_name(scope_id, name)).copied()
    }

    #[must_use]
    pub fn add_pending_ability_defn(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        parsed_defn_id: ParsedAbilityId,
    ) -> bool {
        let added = match self.pending_ability_defns.entry(skey_name(scope_id, ident)) {
            Entry::Occupied(_) => false,
            Entry::Vacant(e) => {
                e.insert(parsed_defn_id);
                true
            }
        };
        if added {
            self.get_scope_mut(scope_id).kinds |= kinds::PENDING_ABILITIES;
        }
        added
    }

    pub fn remove_pending_ability_defn(&mut self, scope_id: ScopeId, ident: StringId) -> bool {
        // Note: we leave the PENDING_ABILITIES kind bit set; it's a hint that
        // the map *may* hit, not a guarantee
        self.pending_ability_defns.remove(&skey_name(scope_id, ident)).is_some()
    }

    pub fn find_pending_ability(
        &self,
        scope_id: ScopeId,
        ident: StringId,
    ) -> Option<(ParsedAbilityId, ScopeId)> {
        self.walk_chain(scope_id, kinds::PENDING_ABILITIES, |sid| {
            self.pending_ability_defns.get(&skey_name(sid, ident)).copied()
        })
    }

    /// Iterate the symbols of one kind defined directly in `scope_id`.
    /// A filtered scan of the whole map: for debugging/dumps and cold paths only.
    fn iter_scope_map<V: Copy>(
        map: &FxHashMap<ScopeKey, V>,
        scope_id: ScopeId,
    ) -> impl Iterator<Item = (StringId, V)> + '_ {
        map.iter().filter_map(move |(k, v)| {
            if skey_scope_part(*k) == scope_id.as_u32() {
                Some((skey_name_part(*k), *v))
            } else {
                None
            }
        })
    }

    pub fn iter_scope_variables(
        &self,
        scope_id: ScopeId,
    ) -> impl Iterator<Item = (StringId, VariableInScope)> + '_ {
        self.get_scope(scope_id).variables.iter().map(|(k, v)| (*k, *v))
    }

    pub fn iter_scope_functions(
        &self,
        scope_id: ScopeId,
    ) -> impl Iterator<Item = (StringId, FunctionId)> + '_ {
        Self::iter_scope_map(&self.functions, scope_id)
    }

    pub fn iter_scope_types(
        &self,
        scope_id: ScopeId,
    ) -> impl Iterator<Item = (StringId, TypeId)> + '_ {
        Self::iter_scope_map(&self.types, scope_id)
    }

    pub fn iter_scope_namespaces(
        &self,
        scope_id: ScopeId,
    ) -> impl Iterator<Item = (StringId, NamespaceId)> + '_ {
        Self::iter_scope_map(&self.namespaces, scope_id)
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
        let scope = self.get_scope(calling_scope);
        match scope.owner_id {
            // We can stop searching once we find an ability or namespace scope; a function won't ever appear above them
            ScopeOwnerId::Ability(_) | ScopeOwnerId::Namespace(_) => None,
            ScopeOwnerId::Function(fn_id) => Some(fn_id),
            _ => match scope.parent {
                Some(parent) => self.nearest_parent_function(parent),
                None => None,
            },
        }
    }

    fn nearest_parent_lambda(&self, scope_id: ScopeId) -> Option<ScopeId> {
        let scope = self.get_scope(scope_id);
        match scope.scope_type {
            // We can stop searching once we find an ability or namespace scope; a lambda won't ever appear above them
            ScopeType::AbilityDefn
            | ScopeType::AbilityImpl
            | ScopeType::TypeDefn
            | ScopeType::Namespace => None,
            ScopeType::LambdaScope => Some(scope_id),
            // We can stop searching once we find a function scope; a lambda won't ever appear
            // outside of a function! (And we don't do locally defined named functions)
            ScopeType::FunctionScope => None,
            _ => match scope.parent {
                Some(parent) => self.nearest_parent_lambda(parent),
                None => None,
            },
        }
    }

    pub fn add_variable(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        variable_id: VariableId,
    ) -> bool {
        // This accomplishes shadowing by overwriting the name in the scope.
        // I think this is ok because the variable itself (by variable id)
        // is not lost, in case we wanted to do some analysis.
        // Still, might need to mark it shadowed explicitly?
        self.get_scope_mut(scope_id)
            .variables
            .insert(ident, VariableInScope::Defined(variable_id))
            .is_none()
    }

    pub fn mask_variable(&mut self, scope_id: ScopeId, ident: StringId) {
        self.get_scope_mut(scope_id).variables.insert(ident, VariableInScope::Masked);
    }

    pub fn add_context_variable(
        &mut self,
        scope_id: ScopeId,
        ident: StringId,
        variable_id: VariableId,
        type_id: TypeId,
    ) -> bool {
        match self.context_variables_by_type.entry(skey(scope_id, type_id.as_u32())) {
            Entry::Occupied(_) => return false,
            Entry::Vacant(e) => e.insert(variable_id),
        };
        self.get_scope_mut(scope_id).kinds |= kinds::CONTEXT_VARIABLES;
        self.add_variable(scope_id, ident, variable_id);
        true
    }

    pub fn add_use_binding(
        &mut self,
        scope_id: ScopeId,
        useable_symbol: &UseableSymbol,
        name_to_use: StringId,
    ) {
        match useable_symbol.id {
            UseableSymbolId::Function(function_id) => {
                // Discard because 'use's should shadow
                let _ = self.add_function(scope_id, name_to_use, function_id);
            }
            UseableSymbolId::Global(variable_id) => {
                self.add_variable(scope_id, name_to_use, variable_id);
            }
            UseableSymbolId::Type { type_id, companion_namespace } => {
                // Discard because 'use's should shadow
                let _ = self.add_type(scope_id, name_to_use, type_id);
                if let Some(companion_namespace) = companion_namespace {
                    let _ = self.add_namespace(scope_id, name_to_use, companion_namespace);
                }
            }
            UseableSymbolId::Namespace(ns_id) => {
                // Discard because 'use's should shadow
                let _ = self.add_namespace(scope_id, name_to_use, ns_id);
            }
            UseableSymbolId::Ability(ability_id, namespace_id) => {
                // Discard because 'use's should shadow
                let _ = self.add_ability(scope_id, name_to_use, ability_id);
                let _ = self.add_namespace(scope_id, name_to_use, namespace_id);
            }
        }
    }

    pub fn add_capture(
        &mut self,
        scope_id: ScopeId,
        variable_id: VariableId,
        fixup_expr_id: TypedExprId,
    ) {
        match self.lambda_info.entry(scope_id) {
            Entry::Occupied(mut lambda_info) => {
                let info = lambda_info.get_mut();
                if !info.captured_variables.contains(&variable_id) {
                    info.captured_variables.push(variable_id);
                };
                // Even if we mention the captured variable multiple times,
                // every occurrence needs to be patched later
                info.capture_exprs_for_fixup.push(fixup_expr_id)
            }
            Entry::Vacant(_entry) => {
                unreachable!("All lambda scopes should have info by block eval")
            }
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

    pub fn nearest_parent_loop(&self, scope_id: ScopeId) -> Option<(ScopeId, LoopType)> {
        let scope = self.get_scope(scope_id);
        match scope.scope_type.loop_type() {
            Some(loop_type) => Some((scope_id, loop_type)),
            None => match scope.parent {
                Some(parent) => self.nearest_parent_loop(parent),
                None => None,
            },
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
            match self.scopes.find_variable_local(scope_to_search, name.name) {
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
            Ok(self.scopes.find_function_local(scope_to_search, name.name))
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
            let found_type = self.scopes.find_type_local(scope_to_search, type_name.name);
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
            return Err(errf!(
                first.span,
                "Namespace not found: {} from scope: {}",
                self.ident_str(first.name),
                self.scope_name_to_string(cur_scope_id,)
            ));
        };
        self.emit_ls_entity(first.span, LsEntityKind::Namespace(first_ns));
        cur_scope_id = self.namespaces.get(first_ns).scope_id;

        for ident in ns_iter {
            let namespace_id =
                self.scopes.find_namespace_local(cur_scope_id, ident.name).ok_or_else(|| {
                    errf!(
                        ident.span,
                        "Namespace not found: {} in scope: {:?}",
                        self.ident_str(ident.name),
                        self.scope_owner_name(cur_scope_id).map(|n| self.ident_str(n))
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
            Ok(self.scopes.find_ability_local(scope_to_search, ability_name.name))
        }
    }

    pub fn find_pending_type_namespaced(
        &self,
        scope_id: ScopeId,
        type_name: &QIdent,
    ) -> K1Result<Option<(TypePendingDefinition, ScopeId)>> {
        // Unqualified mentions are implicitly recursive searches
        // But qualified mentions imply that the targeted symbol lives directly at the given path!
        if type_name.path.is_empty() {
            Ok(self.scopes.find_pending_type(scope_id, type_name.name))
        } else {
            let scope_to_search = self.resolve_qident(scope_id, type_name)?;
            Ok(self
                .scopes
                .find_pending_type_local(scope_to_search, type_name.name)
                .map(|defn| (defn, scope_to_search)))
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

/// Except for variables, a scope's symbols live in `Scopes`' global maps,
/// keyed by (scope, name); `kinds` records which of those maps can hit for
/// this scope. Variables stay inline because identifier resolution probes
/// them constantly and the freshly-written per-scope maps stay cache-hot.
///
/// Scopes don't have names; for pretty-printing, derive one from `owner_id`
/// via `TypedProgram::scope_owner_name`.
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub scope_type: ScopeType,
    /// Bitset of `kinds::*`
    kinds: u8,
    pub owner_id: ScopeOwnerId,
    pub variables: FxHashMap<StringId, VariableInScope>,
}
static_assert_size!(Scope, 56);

impl Scope {
    pub fn make(scope_type: ScopeType, owner_id: ScopeOwnerId) -> Scope {
        Scope { parent: None, scope_type, kinds: 0, owner_id, variables: FxHashMap::new() }
    }
}
