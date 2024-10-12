use log::trace;

use std::{collections::HashMap, fmt::Display};

use crate::{
    errf,
    lex::SpanId,
    parse::{Identifiers, NamespacedIdentifier, ParsedTypeDefnId},
    typer::{
        make_error, AbilityId, FunctionId, Identifier, NamespaceId, Namespaces, TypeId,
        TyperResult, VariableId,
    },
};

pub type ScopeId = u32;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ScopeType {
    FunctionScope,
    LexicalBlock,
    Namespace,
    WhileBody,
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
            ScopeType::LexicalBlock => "block",
            ScopeType::Namespace => "ns",
            ScopeType::WhileBody => "while",
            ScopeType::IfBody => "if",
            ScopeType::ElseBody => "else",
            ScopeType::ForExpr => "for",
            ScopeType::MatchArm => "match_arm",
            ScopeType::TypeDefn => "type_defn",
            ScopeType::AbilityDefn => "ability_defn",
            ScopeType::AbilityImpl => "ability_impl",
        }
    }
}

impl Display for ScopeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.short_name())
    }
}

pub struct Scopes {
    scopes: Vec<Scope>,
}

impl Scopes {
    pub fn make() -> Self {
        Scopes { scopes: Vec::new() }
    }

    pub fn add_root_scope(&mut self, name: Option<Identifier>) -> ScopeId {
        debug_assert!(self.scopes.is_empty());
        self.scopes.push(Scope::make(ScopeType::Namespace, None, name));
        0 as ScopeId
    }

    pub fn iter(&self) -> impl Iterator<Item = (ScopeId, &Scope)> {
        self.scopes.iter().enumerate().map(|(idx, s)| (idx as ScopeId, s))
    }

    pub fn get_root_scope_id(&self) -> ScopeId {
        0 as ScopeId
    }

    pub fn add_child_scope(
        &mut self,
        parent_scope_id: ScopeId,
        scope_type: ScopeType,
        scope_owner_id: Option<ScopeOwnerId>,
        name: Option<Identifier>,
    ) -> ScopeId {
        let scope = Scope {
            parent: Some(parent_scope_id),
            ..Scope::make(scope_type, scope_owner_id, name)
        };
        let id = self.scopes.len() as ScopeId;
        self.scopes.push(scope);
        let parent_scope = self.get_scope_mut(parent_scope_id);
        parent_scope.children.push(id);
        id
    }

    pub fn get_scope(&self, id: ScopeId) -> &Scope {
        &self.scopes[id as usize]
    }

    pub fn get_root_scope(&self) -> &Scope {
        &self.get_scope(self.get_root_scope_id())
    }

    pub fn get_scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        &mut self.scopes[id as usize]
    }

    pub fn set_scope_owner_id(&mut self, id: ScopeId, owner_id: ScopeOwnerId) {
        self.get_scope_mut(id).owner_id = Some(owner_id);
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
    ) -> TyperResult<Option<VariableId>> {
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
            Ok(self.get_scope(scope_to_search).find_variable(name.name))
        }
    }

    pub fn find_variable(&self, scope: ScopeId, ident: Identifier) -> Option<VariableId> {
        let scope = self.get_scope(scope);
        if let v @ Some(_r) = scope.find_variable(ident) {
            return v;
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
        if let f @ Some(_r) = scope.find_function(ident) {
            return f;
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

    pub fn find_type(&self, scope_id: ScopeId, ident: Identifier) -> Option<TypeId> {
        let scope = self.get_scope(scope_id);
        trace!("Find type {} in {:?}", ident, scope.types);
        if let v @ Some(_r) = scope.find_type(ident) {
            return v;
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
    ) -> TyperResult<Option<TypeId>> {
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
            Ok(self.get_scope(scope_to_search).find_type(type_name.name))
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

    pub fn nearest_parent_function(&self, calling_scope: ScopeId) -> FunctionId {
        let scope = self.get_scope(calling_scope);
        match scope.owner_id {
            Some(ScopeOwnerId::Function(fn_id)) => fn_id,
            _ => match scope.parent {
                Some(parent) => self.nearest_parent_function(parent),
                None => panic!("No parent function found"),
            },
        }
    }

    pub fn make_scope_name(&self, scope: &Scope, identifiers: &Identifiers) -> String {
        let mut name = match scope.name {
            Some(name) => (*identifiers.get_name(name)).to_string(),
            None => scope.scope_type.short_name().to_string(),
        };
        if let Some(p) = scope.parent {
            let parent_scope = self.get_scope(p);
            name = format!("{}.{}", self.make_scope_name(parent_scope, identifiers), name);
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

    fn find_ability(&self, scope_id: ScopeId, name: Identifier) -> Option<AbilityId> {
        let scope = self.get_scope(scope_id);
        if let Some(ability_id) = scope.find_ability(name) {
            return Some(ability_id);
        }
        match scope.parent {
            Some(parent_scope_id) => self.find_ability(parent_scope_id, name),
            None => None,
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
}

/// Useful for going from scope to 'thing that owns the scope', like to a scope's ability or namespace or function
#[derive(Debug, Clone, Copy)]
pub enum ScopeOwnerId {
    Ability(AbilityId),
    Function(FunctionId),
    Namespace(NamespaceId),
}
impl ScopeOwnerId {
    pub fn expect_ability(&self) -> AbilityId {
        match self {
            ScopeOwnerId::Ability(a) => *a,
            _ => panic!("Expected ability scope owner"),
        }
    }
}

#[derive(Debug)]
pub struct Scope {
    pub variables: HashMap<Identifier, VariableId>,
    pub functions: HashMap<Identifier, FunctionId>,
    pub namespaces: HashMap<Identifier, NamespaceId>,
    pub types: HashMap<Identifier, TypeId>,
    pub abilities: HashMap<Identifier, AbilityId>,
    // FIXME: Add abilities here so they participate in the type defn phase
    pub pending_type_defns: HashMap<Identifier, ParsedTypeDefnId>,
    pub parent: Option<ScopeId>,
    pub children: Vec<ScopeId>,
    pub scope_type: ScopeType,
    pub owner_id: Option<ScopeOwnerId>,
    /// Name is just used for pretty-printing and debugging scopes don't really have names
    pub name: Option<Identifier>,
}

impl Scope {
    pub fn make(
        scope_type: ScopeType,
        owner_id: Option<ScopeOwnerId>,
        name: Option<Identifier>,
    ) -> Scope {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
            namespaces: HashMap::new(),
            types: HashMap::new(),
            abilities: HashMap::new(),
            pending_type_defns: HashMap::new(),
            parent: None,
            children: Vec::new(),
            scope_type,
            owner_id,
            name,
        }
    }

    pub fn add_variable(&mut self, ident: Identifier, value: VariableId) {
        // This accomplishes shadowing by overwriting the name in the scope.
        // I think this is ok because ther variable itself by variable id
        // is not lost, in case we wanted to do some analysis.
        // Still might need to mark it shadowed
        self.variables.insert(ident, value);
    }

    pub fn find_variable(&self, ident: Identifier) -> Option<VariableId> {
        self.variables.get(&ident).copied()
    }

    #[must_use]
    pub fn add_type(&mut self, ident: Identifier, ty: TypeId) -> bool {
        if self.types.contains_key(&ident) {
            false
        } else {
            self.types.insert(ident, ty);
            true
        }
    }

    pub fn find_type(&self, ident: Identifier) -> Option<TypeId> {
        self.types.get(&ident).copied()
    }

    #[must_use]
    pub fn add_function(&mut self, ident: Identifier, function_id: FunctionId) -> bool {
        if self.functions.contains_key(&ident) {
            false
        } else {
            self.functions.insert(ident, function_id);
            true
        }
    }

    pub fn find_function(&self, ident: Identifier) -> Option<FunctionId> {
        self.functions.get(&ident).copied()
    }

    #[must_use]
    pub fn add_namespace(&mut self, ident: Identifier, namespace_id: NamespaceId) -> bool {
        if self.namespaces.contains_key(&ident) {
            false
        } else {
            self.namespaces.insert(ident, namespace_id);
            true
        }
    }

    pub fn find_namespace(&self, ident: Identifier) -> Option<NamespaceId> {
        self.namespaces.get(&ident).copied()
    }

    #[must_use]
    pub fn add_ability(&mut self, ident: Identifier, ability_id: AbilityId) -> bool {
        if self.abilities.contains_key(&ident) {
            false
        } else {
            self.abilities.insert(ident, ability_id);
            true
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
        if self.pending_type_defns.contains_key(&ident) {
            false
        } else {
            self.pending_type_defns.insert(ident, parsed_defn_id);
            true
        }
    }

    pub fn find_pending_type_defn(&self, ident: Identifier) -> Option<ParsedTypeDefnId> {
        self.pending_type_defns.get(&ident).copied()
    }

    pub fn remove_pending_type_defn(&mut self, ident: Identifier) -> bool {
        self.pending_type_defns.remove(&ident).is_some()
    }
}
