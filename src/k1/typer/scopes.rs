use ahash::HashMapExt;
use fxhash::FxHashMap;
use smallvec::SmallVec;

use std::{
    collections::{HashSet, hash_map::Entry},
    fmt::Display,
    num::NonZeroU32,
};

use crate::{
    errf,
    lex::SpanId,
    nz_u32_id,
    parse::{Identifiers, NamespacedIdentifier, ParsedAbilityId},
    pool::Pool,
    typer::{
        AbilityId, FunctionId, Identifier, LoopType, NamespaceId, Namespaces, TypeId, TypedExprId,
        TyperResult, VariableId, make_error,
    },
};

nz_u32_id!(ScopeId);

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

pub struct ScopeLambdaInfo {
    pub expected_return_type: Option<TypeId>,
    pub capture_exprs_for_fixup: SmallVec<[TypedExprId; 8]>,
    pub captured_variables: SmallVec<[VariableId; 8]>,
}

pub struct ScopeLoopInfo {
    pub break_type: Option<TypeId>,
}

pub struct Scopes {
    scopes: Pool<Scope, ScopeId>,
    lambda_info: FxHashMap<ScopeId, ScopeLambdaInfo>,
    loop_info: FxHashMap<ScopeId, ScopeLoopInfo>,
    pub core_scope_id: ScopeId,
    pub k1_scope_id: ScopeId,
    pub types_scope_id: ScopeId,
}

impl Scopes {
    pub const ROOT_SCOPE_ID: ScopeId = ScopeId(NonZeroU32::new(1).unwrap());
    pub fn make(root_ident: Identifier) -> Self {
        let root_scope = Scope::make(ScopeType::Namespace, None, Some(root_ident), 0);
        let mut scopes = Scopes {
            scopes: Pool::with_capacity("scopes", 8192),
            lambda_info: FxHashMap::new(),
            loop_info: FxHashMap::new(),
            core_scope_id: ScopeId::PENDING,
            k1_scope_id: ScopeId::PENDING,
            types_scope_id: ScopeId::PENDING,
        };
        let id = scopes.scopes.add(root_scope);
        debug_assert_eq!(id, Self::ROOT_SCOPE_ID);
        scopes
    }

    pub fn iter(&self) -> impl Iterator<Item = (ScopeId, &Scope)> {
        self.scopes.iter_ids().zip(self.scopes.iter())
    }

    #[inline]
    pub fn get_root_scope_id(&self) -> ScopeId {
        Self::ROOT_SCOPE_ID
    }

    pub fn add_sibling_scope(
        &mut self,
        sibling_scope_id: ScopeId,
        scope_type: ScopeType,
        scope_owner_id: Option<ScopeOwnerId>,
        name: Option<Identifier>,
    ) -> ScopeId {
        let parent = self.get_scope(sibling_scope_id).parent.unwrap();
        self.add_child_scope(parent, scope_type, scope_owner_id, name)
    }

    pub fn add_child_scope(
        &mut self,
        parent_scope_id: ScopeId,
        scope_type: ScopeType,
        scope_owner_id: Option<ScopeOwnerId>,
        name: Option<Identifier>,
    ) -> ScopeId {
        let id = self.scopes.next_id();
        let parent_scope = self.get_scope_mut(parent_scope_id);
        parent_scope.children.push(id);
        let depth = parent_scope.depth + 1;
        let scope = Scope {
            parent: Some(parent_scope_id),
            ..Scope::make(scope_type, scope_owner_id, name, depth)
        };
        self.scopes.add(scope);
        id
    }

    pub fn get_scope(&self, id: ScopeId) -> &Scope {
        self.scopes.get(id)
    }

    pub fn get_root_scope(&self) -> &Scope {
        self.get_scope(self.get_root_scope_id())
    }

    pub fn get_scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        self.scopes.get_mut(id)
    }

    pub fn set_scope_owner_id(&mut self, id: ScopeId, owner_id: ScopeOwnerId) {
        self.get_scope_mut(id).owner_id = Some(owner_id);
    }

    pub fn get_scope_owner(&self, scope_id: ScopeId) -> Option<ScopeOwnerId> {
        self.get_scope(scope_id).owner_id
    }

    pub fn find_namespace(&self, scope: ScopeId, ident: Identifier) -> Option<NamespaceId> {
        let scope = self.get_scope(scope);
        if let ns @ Some(_r) = scope.find_namespace(ident) {
            return ns;
        }
        match scope.parent {
            Some(parent) => self.find_namespace(parent, ident),
            None => None,
        }
    }

    pub fn find_variable_namespaced(
        &self,
        scope: ScopeId,
        name: &NamespacedIdentifier,
        namespaces: &Namespaces,
        identifiers: &Identifiers,
    ) -> TyperResult<Option<(VariableId, ScopeId)>> {
        if name.namespaces.is_empty() {
            Ok(self.find_variable(scope, name.name))
        } else {
            let scope_to_search = self.traverse_namespace_chain(
                scope,
                &name.namespaces,
                namespaces,
                identifiers,
                name.span,
            )?;
            match self.get_scope(scope_to_search).find_variable(name.name) {
                None => Ok(None),
                Some(VariableInScope::Defined(id)) => Ok(Some((id, scope_to_search))),
                Some(VariableInScope::Masked) => Ok(None),
            }
        }
    }

    pub fn find_context_variable_by_type(
        &self,
        scope: ScopeId,
        type_id: TypeId,
    ) -> Option<VariableId> {
        let scope = self.get_scope(scope);
        if let Some(v) = scope.find_context_variable_by_type(type_id) {
            return Some(v);
        }
        match scope.parent {
            Some(parent) => self.find_context_variable_by_type(parent, type_id),
            None => None,
        }
    }

    pub fn find_variable(
        &self,
        scope_id: ScopeId,
        ident: Identifier,
    ) -> Option<(VariableId, ScopeId)> {
        let scope = self.get_scope(scope_id);
        match scope.find_variable(ident) {
            Some(VariableInScope::Defined(id)) => Some((id, scope_id)),
            Some(VariableInScope::Masked) => None,
            None => match scope.parent {
                Some(parent) => self.find_variable(parent, ident),
                None => None,
            },
        }
    }

    pub fn add_variable(
        &mut self,
        scope_id: ScopeId,
        ident: Identifier,
        variable_id: VariableId,
    ) -> bool {
        let scope = self.get_scope_mut(scope_id);
        scope.add_variable(ident, variable_id)
    }

    pub fn add_context_variable(
        &mut self,
        scope: ScopeId,
        ident: Identifier,
        variable_id: VariableId,
        type_id: TypeId,
    ) -> bool {
        let scope = self.get_scope_mut(scope);
        scope.add_context_variable(ident, variable_id, type_id)
    }

    pub fn find_function_namespaced(
        &self,
        scope: ScopeId,
        name: &NamespacedIdentifier,
        namespaces: &Namespaces,
        identifiers: &Identifiers,
    ) -> TyperResult<Option<FunctionId>> {
        if name.namespaces.is_empty() {
            Ok(self.find_function(scope, name.name))
        } else {
            let scope_to_search = self.traverse_namespace_chain(
                scope,
                &name.namespaces,
                namespaces,
                identifiers,
                name.span,
            )?;
            Ok(self.get_scope(scope_to_search).find_function(name.name))
        }
    }

    pub fn find_function(&self, scope: ScopeId, ident: Identifier) -> Option<FunctionId> {
        let scope = self.get_scope(scope);
        if let Some(function_id) = scope.find_function(ident) {
            return Some(function_id);
        }
        match scope.parent {
            Some(parent) => self.find_function(parent, ident),
            None => None,
        }
    }

    #[must_use]
    pub fn add_function(
        &mut self,
        scope_id: ScopeId,
        identifier: Identifier,
        function_id: FunctionId,
    ) -> bool {
        self.get_scope_mut(scope_id).add_function(identifier, function_id)
    }

    #[must_use]
    pub fn add_type(&mut self, scope_id: ScopeId, ident: Identifier, ty: TypeId) -> bool {
        self.get_scope_mut(scope_id).add_type(ident, ty)
    }

    pub fn find_type(&self, scope_id: ScopeId, ident: Identifier) -> Option<(TypeId, ScopeId)> {
        let scope = self.get_scope(scope_id);
        if let v @ Some(_r) = scope.find_type(ident) {
            return v.map(|v| (v, scope_id));
        }
        match scope.parent {
            Some(parent) => self.find_type(parent, ident),
            None => None,
        }
    }

    pub fn find_type_namespaced(
        &self,
        scope_id: ScopeId,
        type_name: &NamespacedIdentifier,
        namespaces: &Namespaces,
        identifiers: &Identifiers,
    ) -> TyperResult<Option<(TypeId, ScopeId)>> {
        if type_name.namespaces.is_empty() {
            Ok(self.find_type(scope_id, type_name.name))
        } else {
            let scope_to_search = self.traverse_namespace_chain(
                scope_id,
                &type_name.namespaces,
                namespaces,
                identifiers,
                type_name.span,
            )?;
            Ok(self
                .get_scope(scope_to_search)
                .find_type(type_name.name)
                .map(|t| (t, scope_to_search)))
        }
    }

    pub fn find_pending_ability(
        &self,
        scope_id: ScopeId,
        ident: Identifier,
    ) -> Option<(ParsedAbilityId, ScopeId)> {
        let scope = self.get_scope(scope_id);
        if let Some(defn) = scope.find_pending_ability(ident) {
            return Some((defn, scope_id));
        }
        match scope.parent {
            Some(parent) => self.find_pending_ability(parent, ident),
            None => None,
        }
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
            Some(ScopeOwnerId::Namespace(ns)) => ns,
            _ => match scope.parent {
                Some(parent) => self.nearest_parent_namespace(parent),
                None => panic!("No parent namespace found"),
            },
        }
    }

    pub fn nearest_parent_function(&self, calling_scope: ScopeId) -> Option<FunctionId> {
        let scope = self.get_scope(calling_scope);
        match scope.owner_id {
            Some(ScopeOwnerId::Function(fn_id)) => Some(fn_id),
            _ => match scope.parent {
                Some(parent) => self.nearest_parent_function(parent),
                None => None,
            },
        }
    }

    pub fn nearest_parent_lambda(&self, scope_id: ScopeId) -> Option<ScopeId> {
        let scope = self.get_scope(scope_id);
        match scope.scope_type {
            ScopeType::LambdaScope => Some(scope_id),
            _ => match scope.parent {
                Some(parent) => self.nearest_parent_lambda(parent),
                None => None,
            },
        }
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

    pub fn make_scope_name(&self, scope: &Scope, identifiers: &Identifiers) -> String {
        let mut name = match scope.name {
            Some(_) if scope.parent.is_none() => "",
            Some(name) => identifiers.get_name(name),
            None => scope.scope_type.short_name(),
        }
        .to_string();
        if let Some(p) = scope.parent {
            let parent_scope = self.get_scope(p);
            let parent_name = self.make_scope_name(parent_scope, identifiers);
            if !parent_name.is_empty() {
                name = format!("{}.{}", parent_name, name);
            }
        }
        name
    }

    pub fn traverse_namespace_chain(
        &self,
        scope_id: ScopeId,
        namespace_chain: &[Identifier],
        namespaces: &Namespaces,
        identifiers: &Identifiers,
        span: SpanId,
    ) -> TyperResult<ScopeId> {
        let mut ns_iter = namespace_chain.iter();
        let mut cur_scope_id = scope_id;
        let Some(first) = ns_iter.next() else {
            return Ok(cur_scope_id);
        };
        // First lookup is special and recursive because it's in the current scope
        let Some(first_ns) = self.find_namespace(cur_scope_id, *first) else {
            return Err(errf!(
                span,
                "Namespace not found: {} from scope: {:?}",
                identifiers.get_name(*first),
                self.make_scope_name(self.get_scope(cur_scope_id), identifiers)
            ));
        };
        cur_scope_id = namespaces.get(first_ns).scope_id;

        for ns in ns_iter {
            let cur_scope = self.get_scope(cur_scope_id);
            let namespace_id = cur_scope.find_namespace(*ns).ok_or_else(|| {
                errf!(
                    span,
                    "Namespace not found: {} in scope: {:?}",
                    identifiers.get_name(*ns),
                    self.get_scope(cur_scope_id).name.map(|n| identifiers.get_name(n))
                )
            })?;
            let namespace = namespaces.get(namespace_id);
            cur_scope_id = namespace.scope_id;
        }
        Ok(cur_scope_id)
    }

    pub fn find_ability(&self, scope_id: ScopeId, name: Identifier) -> Option<AbilityId> {
        let scope = self.get_scope(scope_id);
        if let Some(ability_id) = scope.find_ability(name) {
            return Some(ability_id);
        }
        match scope.parent {
            Some(parent_scope_id) => self.find_ability(parent_scope_id, name),
            None => None,
        }
    }

    pub fn find_abilities_in_scope(&self, scope_id: ScopeId) -> HashSet<AbilityId> {
        let scope = self.get_scope(scope_id);
        let mut abilities = HashSet::new();
        abilities.extend(scope.abilities.values());
        match scope.parent {
            Some(parent_scope_id) => {
                let from_parent = self.find_abilities_in_scope(parent_scope_id);
                abilities.extend(from_parent);
                abilities
            }
            None => abilities,
        }
    }

    pub fn add_use_binding(
        &mut self,
        scope_id: ScopeId,
        useable_symbol: UseableSymbol,
        name_to_use: Identifier,
    ) {
        match useable_symbol.id {
            UseableSymbolId::Function(function_id) => {
                // Discard because 'use's should shadow
                let _ = self.add_function(scope_id, name_to_use, function_id);
            }
            UseableSymbolId::Constant(variable_id) => {
                self.add_variable(scope_id, name_to_use, variable_id);
            }
            UseableSymbolId::Type { type_id, companion_namespace } => {
                // Discard because 'use's should shadow
                let _ = self.add_type(scope_id, name_to_use, type_id);
                if let Some(companion_namespace) = companion_namespace {
                    let _ = self
                        .get_scope_mut(scope_id)
                        .add_namespace(name_to_use, companion_namespace);
                }
            }
            UseableSymbolId::Namespace(ns_id) => {
                let s = self.get_scope_mut(scope_id);
                // Discard because 'use's should shadow
                let _ = s.add_namespace(name_to_use, ns_id);
            }
            UseableSymbolId::Ability(ability_id, namespace_id) => {
                let s = self.get_scope_mut(scope_id);
                // Discard because 'use's should shadow
                let _ = s.add_ability(name_to_use, ability_id);
                let _ = s.add_namespace(name_to_use, namespace_id);
            }
        }
    }

    pub fn find_ability_namespaced(
        &self,
        scope_id: ScopeId,
        ability_name: &NamespacedIdentifier,
        namespaces: &Namespaces,
        identifiers: &Identifiers,
    ) -> TyperResult<Option<AbilityId>> {
        if ability_name.namespaces.is_empty() {
            Ok(self.find_ability(scope_id, ability_name.name))
        } else {
            let scope_to_search = self.traverse_namespace_chain(
                scope_id,
                &ability_name.namespaces,
                namespaces,
                identifiers,
                ability_name.span,
            )?;
            Ok(self.get_scope(scope_to_search).find_ability(ability_name.name))
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
}

/// Useful for going from scope to 'thing that owns the scope', like to a scope's ability or namespace or function
#[derive(Debug, Clone, Copy)]
pub enum ScopeOwnerId {
    Ability(AbilityId),
    Function(FunctionId),
    Namespace(NamespaceId),
    Lambda(TypeId),
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
    Constant(VariableId),
    Type { type_id: TypeId, companion_namespace: Option<NamespaceId> },
    Namespace(NamespaceId),
    Ability(AbilityId, NamespaceId),
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

pub struct Scope {
    pub variables: FxHashMap<Identifier, VariableInScope>,
    pub context_variables_by_type: FxHashMap<TypeId, VariableId>,
    pub functions: FxHashMap<Identifier, FunctionId>,
    pub namespaces: FxHashMap<Identifier, NamespaceId>,
    pub types: FxHashMap<Identifier, TypeId>,
    pub abilities: FxHashMap<Identifier, AbilityId>,
    pub pending_ability_defns: FxHashMap<Identifier, ParsedAbilityId>,
    pub parent: Option<ScopeId>,
    pub children: SmallVec<[ScopeId; 4]>,
    pub scope_type: ScopeType,
    pub owner_id: Option<ScopeOwnerId>,
    /// Name is just used for pretty-printing and debugging; scopes don't really have names
    pub name: Option<Identifier>,
    pub depth: usize,
}

impl Scope {
    pub fn make(
        scope_type: ScopeType,
        owner_id: Option<ScopeOwnerId>,
        name: Option<Identifier>,
        depth: usize,
    ) -> Scope {
        Scope {
            variables: FxHashMap::new(),
            context_variables_by_type: FxHashMap::new(),
            functions: FxHashMap::new(),
            namespaces: FxHashMap::new(),
            types: FxHashMap::new(),
            abilities: FxHashMap::new(),
            pending_ability_defns: FxHashMap::new(),
            parent: None,
            children: SmallVec::new(),
            scope_type,
            owner_id,
            name,
            depth,
        }
    }

    pub fn add_variable(&mut self, ident: Identifier, value: VariableId) -> bool {
        // This accomplishes shadowing by overwriting the name in the scope.
        // I think this is ok because the variable itself (by variable id)
        // is not lost, in case we wanted to do some analysis.
        // Still, might need to mark it shadowed explicitly?
        self.variables.insert(ident, VariableInScope::Defined(value)).is_none()
    }

    pub fn mask_variable(&mut self, ident: Identifier) {
        self.variables.insert(ident, VariableInScope::Masked);
    }

    #[must_use]
    pub fn add_context_variable(
        &mut self,
        ident: Identifier,
        value: VariableId,
        type_id: TypeId,
    ) -> bool {
        if let Entry::Vacant(e) = self.context_variables_by_type.entry(type_id) {
            e.insert(value);
            // This accomplishes shadowing by overwriting the name in the scope.
            // I think this is ok because the variable itself (by variable id)
            // is not lost, in case we wanted to do some analysis.
            // Still, might need to mark it shadowed explicitly?
            self.variables.insert(ident, VariableInScope::Defined(value));
            true
        } else {
            false
        }
    }

    pub fn find_variable(&self, ident: Identifier) -> Option<VariableInScope> {
        match self.variables.get(&ident) {
            Some(vis) => Some(*vis),
            None => None,
        }
    }

    pub fn find_context_variable_by_type(&self, type_id: TypeId) -> Option<VariableId> {
        self.context_variables_by_type.get(&type_id).copied()
    }

    pub fn overwrite_type(&mut self, ident: Identifier, ty: TypeId) -> bool {
        self.types.insert(ident, ty).is_some()
    }

    #[must_use]
    pub fn add_type(&mut self, ident: Identifier, ty: TypeId) -> bool {
        if let Entry::Vacant(e) = self.types.entry(ident) {
            e.insert(ty);
            true
        } else {
            false
        }
    }

    pub fn find_type(&self, ident: Identifier) -> Option<TypeId> {
        self.types.get(&ident).copied()
    }

    #[must_use]
    pub fn add_function(&mut self, ident: Identifier, function_id: FunctionId) -> bool {
        if let Entry::Vacant(e) = self.functions.entry(ident) {
            e.insert(function_id);
            true
        } else {
            false
        }
    }

    pub fn find_function(&self, ident: Identifier) -> Option<FunctionId> {
        self.functions.get(&ident).copied()
    }

    #[must_use]
    pub fn add_namespace(&mut self, ident: Identifier, namespace_id: NamespaceId) -> bool {
        if let std::collections::hash_map::Entry::Vacant(e) = self.namespaces.entry(ident) {
            e.insert(namespace_id);
            true
        } else {
            false
        }
    }

    pub fn find_namespace(&self, ident: Identifier) -> Option<NamespaceId> {
        self.namespaces.get(&ident).copied()
    }

    #[must_use]
    pub fn add_ability(&mut self, ident: Identifier, ability_id: AbilityId) -> bool {
        if let std::collections::hash_map::Entry::Vacant(e) = self.abilities.entry(ident) {
            e.insert(ability_id);
            true
        } else {
            false
        }
    }

    pub fn find_ability(&self, ident: Identifier) -> Option<AbilityId> {
        self.abilities.get(&ident).copied()
    }

    #[must_use]
    pub fn add_pending_ability_defn(
        &mut self,
        ident: Identifier,
        parsed_defn_id: ParsedAbilityId,
    ) -> bool {
        if let std::collections::hash_map::Entry::Vacant(e) =
            self.pending_ability_defns.entry(ident)
        {
            e.insert(parsed_defn_id);
            true
        } else {
            false
        }
    }

    pub fn find_pending_ability(&self, ident: Identifier) -> Option<ParsedAbilityId> {
        self.pending_ability_defns.get(&ident).copied()
    }

    pub fn remove_pending_ability_defn(&mut self, ident: Identifier) -> bool {
        self.pending_ability_defns.remove(&ident).is_some()
    }
}
