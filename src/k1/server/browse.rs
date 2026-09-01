//! Read-only browser over the compiled program: a namespace tree rail beside
//! per-namespace member listings.

use fxhash::FxHashMap;
use maud::{Markup, html};

use crate::lex::SpanId;
use crate::parse::StringId;
use crate::typer::scopes::Provenance;
use crate::typer::types::TypeId;
use crate::typer::{
    AbilityId, FunctionId, GlobalInitialValue, NamespaceId, NamespaceKind, TypedProgram, VariableId,
};

struct NsTree {
    root: NamespaceId,
    /// Children of each namespace, sorted by name
    children: FxHashMap<NamespaceId, Vec<NamespaceId>>,
}

fn build_tree(k1: &TypedProgram) -> NsTree {
    let mut children: FxHashMap<NamespaceId, Vec<NamespaceId>> = FxHashMap::default();
    let mut root = None;
    for (id, ns) in k1.namespaces.namespaces.iter_with_ids() {
        match ns.parent_id {
            Some(parent) => children.entry(parent).or_default().push(id),
            None => root = Some(id),
        }
    }
    let root = root.expect("a root namespace");
    for kids in children.values_mut() {
        kids.sort_by(|a, b| {
            let name = |id: &NamespaceId| k1.ident_str(k1.namespaces.get(*id).name);
            name(a).cmp(name(b))
        });
    }
    NsTree { root, children }
}

fn find_child(k1: &TypedProgram, parent: NamespaceId, name: StringId) -> Option<NamespaceId> {
    k1.namespaces
        .namespaces
        .iter_with_ids()
        .find(|(_, ns)| ns.parent_id == Some(parent) && ns.name == name)
        .map(|(id, _)| id)
}

fn resolve_path(k1: &TypedProgram, root: NamespaceId, path: &[&str]) -> Option<NamespaceId> {
    let mut current = root;
    for segment in path {
        let name = k1.ast.idents.lookup(segment)?;
        current = find_child(k1, current, name)?;
    }
    Some(current)
}

fn ns_href(path: &[&str]) -> String {
    let mut href = String::from("/ns");
    for segment in path {
        href.push('/');
        href.push_str(segment);
    }
    href
}

fn ns_canonical_href(k1: &TypedProgram, ns_id: NamespaceId) -> String {
    let mut names: Vec<&str> = Vec::new();
    let mut current = ns_id;
    while let Some(parent) = k1.namespaces.get(current).parent_id {
        names.push(k1.ident_str(k1.namespaces.get(current).name));
        current = parent;
    }
    names.reverse();
    ns_href(&names)
}

/// "file.k1:line" for definitions; None for synthesized spans
fn span_location(k1: &TypedProgram, span_id: SpanId) -> Option<String> {
    if span_id == SpanId::NONE {
        return None;
    }
    let span = k1.ast.spans.get(span_id);
    let source = k1.ast.sources.get(span.file_id);
    let (line, _) = k1.ast.get_lines_for_span_id(span_id)?;
    Some(format!("{}:{}", source.filename_str(&k1.ast.idents), line.line_number()))
}

pub fn render_page(k1: &TypedProgram, path: &[&str]) -> Option<Markup> {
    let tree = build_tree(k1);
    let selected = resolve_path(k1, tree.root, path)?;
    Some(html! {
        (maud::DOCTYPE)
        html {
            (super::render_head())
            body data-init=(super::events_init_action()) {
                (super::render_site_header("browse"))
                main .browse-workspace {
                    (render_tree_rail(k1, &tree, selected))
                    (render_ns_main(k1, &tree, selected, path))
                }
            }
        }
    })
}

fn render_tree_rail(k1: &TypedProgram, tree: &NsTree, selected: NamespaceId) -> Markup {
    html! {
        nav .ns-rail {
            ul .ns-tree {
                (render_tree_node(k1, tree, tree.root, selected, &mut Vec::new()))
            }
        }
    }
}

fn render_tree_node(
    k1: &TypedProgram,
    tree: &NsTree,
    ns_id: NamespaceId,
    selected: NamespaceId,
    path: &mut Vec<String>,
) -> Markup {
    let ns = k1.namespaces.get(ns_id);
    let name = k1.ident_str(ns.name);
    let kids = tree.children.get(&ns_id);
    let path_strs: Vec<&str> = path.iter().map(String::as_str).collect();
    html! {
        li {
            a .tree-node .selected[ns_id == selected] href=(ns_href(&path_strs)) { (name) }
            @if let Some(kids) = kids {
                ul .tree-kids {
                    @for kid in kids {
                        ({
                            path.push(k1.ident_str(k1.namespaces.get(*kid).name).to_string());
                            let markup = render_tree_node(k1, tree, *kid, selected, path);
                            path.pop();
                            markup
                        })
                    }
                }
            }
        }
    }
}

fn ns_kind_label(k1: &TypedProgram, ns_id: NamespaceId) -> String {
    let ns = k1.namespaces.get(ns_id);
    let kind = match ns.namespace_type {
        NamespaceKind::User => "user ns",
        NamespaceKind::TypeCompanion => "companion ns",
        NamespaceKind::Ability => "ability ns",
        NamespaceKind::Root => "root ns",
    };
    match ns.companion_type_id {
        Some(type_id) => format!("{kind} of {}", k1.type_id_to_string(type_id)),
        None => kind.to_string(),
    }
}

fn render_ns_main(k1: &TypedProgram, tree: &NsTree, ns_id: NamespaceId, path: &[&str]) -> Markup {
    let ns = k1.namespaces.get(ns_id);
    let scope_id = ns.scope_id;

    let mut functions: Vec<(StringId, FunctionId)> = Vec::new();
    for (name, function_id, prov) in k1.scopes.iter_scope_functions(scope_id) {
        if prov.is_exposed() {
            functions.push((name, function_id));
        }
    }
    functions.sort_by(|a, b| k1.ident_str(a.0).cmp(k1.ident_str(b.0)));

    let mut types: Vec<(StringId, TypeId)> = Vec::new();
    for (name, type_id, prov) in k1.scopes.iter_scope_types(scope_id) {
        if prov.is_exposed() {
            types.push((name, type_id));
        }
    }
    types.sort_by(|a, b| k1.ident_str(a.0).cmp(k1.ident_str(b.0)));

    let mut abilities: Vec<(StringId, AbilityId)> = Vec::new();
    for (name, ability_id, prov) in k1.scopes.iter_scope_abilities(scope_id) {
        if prov.is_exposed() {
            abilities.push((name, ability_id));
        }
    }
    abilities.sort_by(|a, b| k1.ident_str(a.0).cmp(k1.ident_str(b.0)));

    let mut globals: Vec<(StringId, VariableId)> = Vec::new();
    for (name, vis, prov) in k1.scopes.iter_scope_variables(scope_id) {
        if let Some(vid) = vis.variable_id()
            && prov.is_exposed()
        {
            globals.push((name, vid));
        }
    }
    globals.sort_by(|a, b| k1.ident_str(a.0).cmp(k1.ident_str(b.0)));

    let mut used_namespaces: Vec<(StringId, NamespaceId)> = Vec::new();
    for (name, used_ns_id, prov) in k1.scopes.iter_scope_namespaces(scope_id) {
        if prov == Provenance::UseExposed {
            used_namespaces.push((name, used_ns_id));
        }
    }
    used_namespaces.sort_by(|a, b| k1.ident_str(a.0).cmp(k1.ident_str(b.0)));

    let child_namespaces = tree.children.get(&ns_id);

    html! {
        section .ns-main {
            div .ns-crumbs {
                a href=(ns_href(&[])) { (k1.ident_str(k1.namespaces.get(tree.root).name)) }
                @for (i, segment) in path.iter().enumerate() {
                    span .crumb-sep { "/" }
                    @if i + 1 == path.len() {
                        span .crumb-here { (segment) }
                    } @else {
                        a href=(ns_href(&path[..i + 1])) { (segment) }
                    }
                }
                span .ns-kind-chip { (ns_kind_label(k1, ns_id)) }
            }
            @if child_namespaces.is_some_and(|kids| !kids.is_empty()) || !used_namespaces.is_empty() {
                article .card {
                    div .card-label {
                        span { "namespaces" }
                        span { (child_namespaces.map_or(0, Vec::len) + used_namespaces.len()) }
                    }
                    @for kid in child_namespaces.into_iter().flatten() {
                        ({
                            let kid_name = k1.ident_str(k1.namespaces.get(*kid).name);
                            let mut kid_path: Vec<&str> = path.to_vec();
                            kid_path.push(kid_name);
                            html! {
                                a .member-row href=(ns_href(&kid_path)) {
                                    span .member-kw { "ns" }
                                    span .member-name { (kid_name) }
                                }
                            }
                        })
                    }
                    @for (name, used_ns_id) in &used_namespaces {
                        a .member-row href=(ns_canonical_href(k1, *used_ns_id)) {
                            span .member-kw { "use ns" }
                            span .member-name { (k1.ident_str(*name)) }
                        }
                    }
                }
            }
            @if !functions.is_empty() {
                article .card {
                    div .card-label { span { "functions" } span { (functions.len()) } }
                    @for (_, function_id) in &functions {
                        div .member-row {
                            span .member-kw { "fn" }
                            span .member-sig {
                                (k1.function_signature_to_string(&k1.functions.get(*function_id).signature()))
                            }
                            @if let Some(loc) = span_location(k1, k1.get_function_span(*function_id)) {
                                span .member-meta { (loc) }
                            }
                        }
                    }
                }
            }
            @if !types.is_empty() {
                article .card {
                    div .card-label { span { "types" } span { (types.len()) } }
                    @for (name, type_id) in &types {
                        div .member-row {
                            span .member-kw { "type" }
                            span .member-name { (k1.ident_str(*name)) }
                            span .member-sig { (k1.types.get(*type_id).kind_name()) }
                            @if let Some(layout) = k1.get_layout_nonmut(*type_id) {
                                span .member-meta { "size=" (layout.size) " align=" (layout.align) }
                            }
                        }
                    }
                }
            }
            @if !abilities.is_empty() {
                article .card {
                    div .card-label { span { "abilities" } span { (abilities.len()) } }
                    @for (name, ability_id) in &abilities {
                        div .member-row {
                            span .member-kw { "ability" }
                            span .member-name { (k1.ident_str(*name)) }
                            span .member-sig {
                                (k1.abilities.get(*ability_id).functions.len()) " functions"
                            }
                        }
                    }
                }
            }
            @if !globals.is_empty() {
                article .card {
                    div .card-label { span { "globals" } span { (globals.len()) } }
                    @for (name, variable_id) in &globals {
                        @let variable = k1.variables.get(*variable_id);
                        @let global_id = variable.global_id().unwrap();
                        @let global = k1.globals.get(global_id);
                        div .member-row {
                            span { "constant: " (global.is_constant) }
                            span .member-kw { "let" }
                            span .member-name { (k1.ident_str(*name)) }
                            span .member-sig {
                                ": " (k1.type_id_to_string(k1.variables.get(*variable_id).type_id))
                            }
                            @match global.initial_value {
                                GlobalInitialValue::Value(value_id) => span { " = " (k1.megarepl_static_value_to_string(value_id)) }
                                GlobalInitialValue::Pending => span { "pending" }
                                GlobalInitialValue::Failed(_) => span { "failed" }
                                GlobalInitialValue::Uninit if global.is_external => span { "externally linked" }
                                GlobalInitialValue::Uninit => span { "repl-defined" }
                            }
                        }
                    }
                }
            }
        }
    }
}
