use ahash::HashMapExt;
use fxhash::FxHashMap;
use log::{debug, trace};

use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    fmt::Display,
};

use crate::{
    errf,
    lex::SpanId,
    parse::{Identifiers, NamespacedIdentifier, ParsedAbilityId, ParsedTypeDefnId},
    typer::{
        make_error, AbilityId, FunctionId, Identifier, LoopType, NamespaceId, Namespaces, TypeId,
        TyperResult, VariableId,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(u32);

impl Display for ScopeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ScopeType {
    FunctionScope,
    ClosureScope,
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
            ScopeType::ClosureScope => "clos",
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
            ScopeType::ClosureScope => None,
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

pub struct ScopeClosureInfo {
    pub expected_return_type: Option<TypeId>,
}

pub struct ScopeLoopInfo {
    pub break_type: Option<TypeId>,
}

pub struct Scopes {
    scopes: Vec<Scope>,
    captures: HashMap<ScopeId, Vec<VariableId>>,
    closure_info: HashMap<ScopeId, ScopeClosureInfo>,
    loop_info: HashMap<ScopeId, ScopeLoopInfo>,
}

impl Scopes {
    pub fn make() -> Self {
        Scopes {
            scopes: Vec::new(),
            captures: HashMap::new(),
            closure_info: HashMap::new(),
            loop_info: HashMap::new(),
        }
    }

    pub fn add_root_scope(&mut self, name: Option<Identifier>) -> ScopeId {
        debug_assert!(self.scopes.is_empty());
        self.scopes.push(Scope::make(ScopeType::Namespace, None, name, 0));
        ScopeId(0)
    }

    pub fn iter(&self) -> impl Iterator<Item = (ScopeId, &Scope)> {
        self.scopes.iter().enumerate().map(|(idx, s)| (ScopeId(idx as u32), s))
    }

    pub fn get_root_scope_id(&self) -> ScopeId {
        ScopeId(0)
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
        let id = ScopeId(self.scopes.len() as u32);
        let parent_scope = self.get_scope_mut(parent_scope_id);
        parent_scope.children.push(id);
        let depth = parent_scope.depth + 1;
        let scope = Scope {
            parent: Some(parent_scope_id),
            ..Scope::make(scope_type, scope_owner_id, name, depth)
        };
        self.scopes.push(scope);
        id
    }

    pub fn get_scope(&self, id: ScopeId) -> &Scope {
        &self.scopes[id.0 as usize]
    }

    pub fn get_root_scope(&self) -> &Scope {
        self.get_scope(self.get_root_scope_id())
    }

    pub fn get_scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        &mut self.scopes[id.0 as usize]
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
                Some(v) => Ok(Some((v, scope_to_search))),
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
        if let Some(v) = scope.find_variable(ident) {
            return Some((v, scope_id));
        }
        match scope.parent {
            Some(parent) => self.find_variable(parent, ident),
            None => None,
        }
    }

    pub fn add_variable(&mut self, scope_id: ScopeId, ident: Identifier, variable_id: VariableId) {
        let scope = self.get_scope_mut(scope_id);
        scope.add_variable(ident, variable_id);
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
        trace!("Find type {} in {:?}", ident, scope.types);
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

    pub fn find_pending_type_defn(
        &self,
        scope_id: ScopeId,
        ident: Identifier,
    ) -> Option<(ParsedTypeDefnId, ScopeId)> {
        let scope = self.get_scope(scope_id);
        if let Some(defn) = scope.find_pending_type_defn(ident) {
            return Some((defn, scope_id));
        }
        match scope.parent {
            Some(parent) => self.find_pending_type_defn(parent, ident),
            None => None,
        }
    }

    pub fn remove_pending_type_defn(&mut self, scope_id: ScopeId, ident: Identifier) -> bool {
        let scope = self.get_scope_mut(scope_id);
        if scope.remove_pending_type_defn(ident) {
            true
        } else {
            match scope.parent {
                Some(parent) => self.remove_pending_type_defn(parent, ident),
                None => false,
            }
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

    pub fn all_pending_type_defns_below(&self, scope_id: ScopeId) -> Vec<ParsedTypeDefnId> {
        let scope = self.get_scope(scope_id);
        let mut pendings: Vec<ParsedTypeDefnId> =
            scope.pending_type_defns.values().copied().collect();
        for child in &scope.children {
            let child_pendings = self.all_pending_type_defns_below(*child);
            pendings.extend(child_pendings);
        }
        pendings
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

    pub fn nearest_parent_closure(&self, scope_id: ScopeId) -> Option<ScopeId> {
        let scope = self.get_scope(scope_id);
        match scope.scope_type {
            ScopeType::ClosureScope => Some(scope_id),
            _ => match scope.parent {
                Some(parent) => self.nearest_parent_closure(parent),
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
            let namespace_id = cur_scope.find_namespace(*ns).ok_or(errf!(
                span,
                "Namespace not found: {} in scope: {:?}",
                identifiers.get_name(*ns),
                self.get_scope(cur_scope_id).name.map(|n| identifiers.get_name(n))
            ))?;
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

    pub fn find_useable_symbol(
        &self,
        scope_id: ScopeId,
        name: &NamespacedIdentifier,
        namespaces: &Namespaces,
        identifiers: &Identifiers,
    ) -> TyperResult<Option<UseableSymbol>> {
        let scope_id_to_search = self.traverse_namespace_chain(
            scope_id,
            &name.namespaces,
            namespaces,
            identifiers,
            name.span,
        )?;
        let scope_to_search = self.get_scope(scope_id_to_search);

        debug!(
            "Searching scope for useable symbol: {}, Functions:\n{:?}",
            self.make_scope_name(scope_to_search, identifiers),
            scope_to_search.functions.iter().collect::<Vec<_>>()
        );

        if let Some(function_id) = scope_to_search.find_function(name.name) {
            Ok(Some(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Function(function_id),
            }))
        } else if let Some(type_id) = scope_to_search.find_type(name.name) {
            Ok(Some(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Type(type_id),
            }))
        } else if let Some(variable_id) = scope_to_search.find_variable(name.name) {
            Ok(Some(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Constant(variable_id),
            }))
        } else if let Some(ns_id) = scope_to_search.find_namespace(name.name) {
            Ok(Some(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Namespace(ns_id),
            }))
        } else {
            Ok(None)
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
            UseableSymbolId::Type(type_id) => {
                // Discard because 'use's should shadow
                let _ = self.add_type(scope_id, name_to_use, type_id);
            }
            UseableSymbolId::Namespace(ns_id) => {
                // Discard because 'use's should shadow
                let s = self.get_scope_mut(scope_id);
                let _ = s.add_namespace(name_to_use, ns_id);
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

    pub fn add_capture(&mut self, scope_id: ScopeId, variable_id: VariableId) {
        match self.captures.entry(scope_id) {
            Entry::Occupied(mut captures) => {
                if !captures.get().contains(&variable_id) {
                    captures.get_mut().push(variable_id)
                };
            }
            Entry::Vacant(entry) => {
                entry.insert(vec![variable_id]);
            }
        }
    }

    pub fn get_captures(&self, scope_id: ScopeId) -> &[VariableId] {
        self.captures.get(&scope_id).map(|v| v.as_slice()).unwrap_or(&[])
    }

    pub fn add_closure_info(&mut self, closure_scope_id: ScopeId, info: ScopeClosureInfo) {
        self.closure_info.insert(closure_scope_id, info);
    }

    pub fn get_closure_info(&self, closure_scope_id: ScopeId) -> Option<&ScopeClosureInfo> {
        self.closure_info.get(&closure_scope_id)
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
    Closure(TypeId),
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
    id: UseableSymbolId,
    #[allow(unused)]
    source_scope: ScopeId,
}

#[derive(Debug, Clone, Copy)]
pub enum UseableSymbolId {
    Function(FunctionId),
    Constant(VariableId),
    Type(TypeId),
    Namespace(NamespaceId),
}

#[derive(Debug)]
pub struct Scope {
    pub variables: FxHashMap<Identifier, VariableId>,
    pub context_variables_by_type: FxHashMap<TypeId, VariableId>,
    pub functions: FxHashMap<Identifier, FunctionId>,
    pub namespaces: FxHashMap<Identifier, NamespaceId>,
    pub types: FxHashMap<Identifier, TypeId>,
    pub abilities: FxHashMap<Identifier, AbilityId>,
    pub pending_type_defns: FxHashMap<Identifier, ParsedTypeDefnId>,
    pub pending_ability_defns: FxHashMap<Identifier, ParsedAbilityId>,
    pub parent: Option<ScopeId>,
    pub children: Vec<ScopeId>,
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
            pending_type_defns: FxHashMap::new(),
            pending_ability_defns: FxHashMap::new(),
            parent: None,
            children: Vec::new(),
            scope_type,
            owner_id,
            name,
            depth,
        }
    }

    pub fn add_variable(&mut self, ident: Identifier, value: VariableId) {
        // This accomplishes shadowing by overwriting the name in the scope.
        // I think this is ok because the variable itself (by variable id)
        // is not lost, in case we wanted to do some analysis.
        // Still, might need to mark it shadowed explicitly?
        self.variables.insert(ident, value);
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
            self.variables.insert(ident, value);
            true
        } else {
            false
        }
    }

    pub fn find_variable(&self, ident: Identifier) -> Option<VariableId> {
        self.variables.get(&ident).copied()
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
    pub fn add_pending_type_defn(
        &mut self,
        ident: Identifier,
        parsed_defn_id: ParsedTypeDefnId,
    ) -> bool {
        if let std::collections::hash_map::Entry::Vacant(e) = self.pending_type_defns.entry(ident) {
            e.insert(parsed_defn_id);
            true
        } else {
            false
        }
    }

    pub fn find_pending_type_defn(&self, ident: Identifier) -> Option<ParsedTypeDefnId> {
        self.pending_type_defns.get(&ident).copied()
    }

    pub fn remove_pending_type_defn(&mut self, ident: Identifier) -> bool {
        self.pending_type_defns.remove(&ident).is_some()
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
