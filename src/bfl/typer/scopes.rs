use log::trace;
use parse_display::Display;

use std::collections::HashMap;

use crate::{
    parse::ParsedTypeDefnId,
    typer::{AbilityId, FunctionId, IdentifierId, NamespaceId, TypeId, VariableId},
};

pub type ScopeId = u32;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Display)]
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
        }
    }
}

pub struct Scopes {
    scopes: Vec<Scope>,
}

impl Scopes {
    pub fn make() -> Self {
        Scopes { scopes: Vec::new() }
    }

    pub fn add_root_scope(&mut self, name: Option<IdentifierId>) -> ScopeId {
        debug_assert!(self.scopes.is_empty());
        self.scopes.push(Scope::make(ScopeType::Namespace, name));
        0 as ScopeId
    }

    pub fn iter(&self) -> impl Iterator<Item = (ScopeId, &Scope)> {
        self.scopes.iter().enumerate().map(|(idx, s)| (idx as ScopeId, s))
    }

    pub fn get_root_scope_id(&self) -> ScopeId {
        0 as ScopeId
    }

    pub fn add_scope_to_root(
        &mut self,
        scope_type: ScopeType,
        name: Option<IdentifierId>,
    ) -> ScopeId {
        self.add_child_scope(0, scope_type, name)
    }

    pub fn add_child_scope(
        &mut self,
        parent_scope_id: ScopeId,
        scope_type: ScopeType,
        name: Option<IdentifierId>,
    ) -> ScopeId {
        let scope = Scope { parent: Some(parent_scope_id), ..Scope::make(scope_type, name) };
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

    pub fn find_namespace(&self, scope: ScopeId, ident: IdentifierId) -> Option<NamespaceId> {
        let scope = self.get_scope(scope);
        if let ns @ Some(_r) = scope.find_namespace(ident) {
            return ns;
        }
        match scope.parent {
            Some(parent) => self.find_namespace(parent, ident),
            None => None,
        }
    }

    pub fn find_variable(&self, scope: ScopeId, ident: IdentifierId) -> Option<VariableId> {
        let scope = self.get_scope(scope);
        if let v @ Some(_r) = scope.find_variable(ident) {
            return v;
        }
        match scope.parent {
            Some(parent) => self.find_variable(parent, ident),
            None => None,
        }
    }

    pub fn add_variable(
        &mut self,
        scope_id: ScopeId,
        ident: IdentifierId,
        variable_id: VariableId,
    ) {
        let scope = self.get_scope_mut(scope_id);
        scope.add_variable(ident, variable_id);
    }

    pub fn find_function(
        &self,
        scope: ScopeId,
        ident: IdentifierId,
        recurse: bool,
    ) -> Option<FunctionId> {
        let scope = self.get_scope(scope);
        if let f @ Some(_r) = scope.find_function(ident) {
            return f;
        }
        if recurse {
            match scope.parent {
                Some(parent) => self.find_function(parent, ident, true),
                None => None,
            }
        } else {
            return None;
        }
    }

    #[must_use]
    pub fn add_function(
        &mut self,
        scope_id: ScopeId,
        identifier: IdentifierId,
        function_id: FunctionId,
    ) -> bool {
        self.get_scope_mut(scope_id).add_function(identifier, function_id)
    }

    #[must_use]
    pub fn add_type(&mut self, scope_id: ScopeId, ident: IdentifierId, ty: TypeId) -> bool {
        self.get_scope_mut(scope_id).add_type(ident, ty)
    }

    pub fn find_type(&self, scope_id: ScopeId, ident: IdentifierId) -> Option<TypeId> {
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

    pub fn find_pending_type_defn(
        &self,
        scope_id: ScopeId,
        ident: IdentifierId,
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

    pub fn remove_pending_type_defn(&mut self, scope_id: ScopeId, ident: IdentifierId) -> bool {
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
}

#[derive(Debug)]
pub struct Scope {
    pub variables: HashMap<IdentifierId, VariableId>,
    pub functions: HashMap<IdentifierId, FunctionId>,
    pub namespaces: HashMap<IdentifierId, NamespaceId>,
    pub types: HashMap<IdentifierId, TypeId>,
    pub abilities: HashMap<IdentifierId, AbilityId>,
    // FIXME(padding): Add abilities here so they participate in the type defn phase
    pub pending_type_defns: HashMap<IdentifierId, ParsedTypeDefnId>,
    pub parent: Option<ScopeId>,
    pub children: Vec<ScopeId>,
    pub scope_type: ScopeType,
    /// Name is just used for pretty-printing and debugging scopes don't really have names
    pub name: Option<IdentifierId>,
}

impl Scope {
    pub fn make(typ: ScopeType, name: Option<IdentifierId>) -> Scope {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
            namespaces: HashMap::new(),
            types: HashMap::new(),
            abilities: HashMap::new(),
            pending_type_defns: HashMap::new(),
            parent: None,
            children: Vec::new(),
            scope_type: typ,
            name,
        }
    }

    pub fn add_variable(&mut self, ident: IdentifierId, value: VariableId) {
        // This accomplishes shadowing by overwriting the name in the scope.
        // I think this is ok because ther variable itself by variable id
        // is not lost, in case we wanted to do some analysis.
        // Still might need to mark it shadowed
        self.variables.insert(ident, value);
    }

    pub fn find_variable(&self, ident: IdentifierId) -> Option<VariableId> {
        self.variables.get(&ident).copied()
    }

    #[must_use]
    pub fn add_type(&mut self, ident: IdentifierId, ty: TypeId) -> bool {
        if self.types.contains_key(&ident) {
            false
        } else {
            self.types.insert(ident, ty);
            true
        }
    }

    pub fn find_type(&self, ident: IdentifierId) -> Option<TypeId> {
        self.types.get(&ident).copied()
    }

    #[must_use]
    pub fn add_function(&mut self, ident: IdentifierId, function_id: FunctionId) -> bool {
        if self.functions.contains_key(&ident) {
            false
        } else {
            self.functions.insert(ident, function_id);
            true
        }
    }

    pub fn find_function(&self, ident: IdentifierId) -> Option<FunctionId> {
        self.functions.get(&ident).copied()
    }

    #[must_use]
    pub fn add_namespace(&mut self, ident: IdentifierId, namespace_id: NamespaceId) -> bool {
        if self.namespaces.contains_key(&ident) {
            false
        } else {
            self.namespaces.insert(ident, namespace_id);
            true
        }
    }

    pub fn find_namespace(&self, ident: IdentifierId) -> Option<NamespaceId> {
        self.namespaces.get(&ident).copied()
    }

    #[must_use]
    pub fn add_ability(&mut self, ident: IdentifierId, ability_id: AbilityId) -> bool {
        if self.abilities.contains_key(&ident) {
            false
        } else {
            self.abilities.insert(ident, ability_id);
            true
        }
    }

    pub fn find_ability(&self, ident: IdentifierId) -> Option<AbilityId> {
        self.abilities.get(&ident).copied()
    }

    #[must_use]
    pub fn add_pending_type_defn(
        &mut self,
        ident: IdentifierId,
        parsed_defn_id: ParsedTypeDefnId,
    ) -> bool {
        if self.pending_type_defns.contains_key(&ident) {
            false
        } else {
            self.pending_type_defns.insert(ident, parsed_defn_id);
            true
        }
    }

    pub fn find_pending_type_defn(&self, ident: IdentifierId) -> Option<ParsedTypeDefnId> {
        self.pending_type_defns.get(&ident).copied()
    }

    pub fn remove_pending_type_defn(&mut self, ident: IdentifierId) -> bool {
        self.pending_type_defns.remove(&ident).is_some()
    }
}
