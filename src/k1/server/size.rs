//! Code-size treemap: the functions codegen would emit, sized by IR
//! instruction count, grouped by namespace, each generic's specializations
//! nested under it.

use fxhash::FxHashMap;
use maud::{Markup, PreEscaped, html};
use serde_json::{Value, json};

use crate::codegen_llvm::Cg;
use crate::lex::SpanId;
use crate::typer::{AbilityId, FunctionId, NamespaceId, TypedFunctionKind, TypedProgram};

struct Node {
    name: String,
    kind: &'static str,
    insts: u32,
    location: Option<String>,
    children: Vec<Node>,
}

impl Node {
    fn to_json(&self) -> Value {
        let mut children: Vec<Value> = Vec::with_capacity(self.children.len());
        for child in &self.children {
            children.push(child.to_json());
        }
        let mut object = json!({ "n": self.name, "k": self.kind });
        if self.children.is_empty() {
            object["v"] = json!(self.insts);
        } else {
            object["c"] = Value::Array(children);
        }
        if let Some(location) = &self.location {
            object["l"] = json!(location);
        }
        object
    }
}

/// Ability impl functions live in the ability's namespace; the treemap files
/// them under the module that declares the impl, grouped by ability
fn placement(k1: &TypedProgram, function_id: FunctionId) -> (NamespaceId, Option<AbilityId>) {
    let function = k1.get_function(function_id);
    let TypedFunctionKind::AbilityImpl(ability_id, _) = function.kind else {
        return (function.namespace_id, None);
    };
    let span = k1.get_function_span(function_id);
    if span == SpanId::NONE {
        return (function.namespace_id, Some(ability_id));
    }
    let module = k1.modules.get(k1.module_of_span(span));
    (module.namespace_id, Some(ability_id))
}

fn leaf(k1: &TypedProgram, function_id: FunctionId, name: String, kind: &'static str) -> Node {
    let insts = k1.ir.functions.get(&function_id).map_or(0, |unit| unit.inst_count);
    let location = k1.span_location(k1.get_function_span(function_id));
    Node { name, kind, insts, location, children: Vec::new() }
}

fn namespace_node(
    k1: &TypedProgram,
    tree: &super::browse::NsTree,
    members: &mut FxHashMap<NamespaceId, Vec<Node>>,
    ns_id: NamespaceId,
) -> Option<Node> {
    let mut children: Vec<Node> = Vec::new();
    if let Some(kids) = tree.children.get(&ns_id) {
        for kid in kids {
            if let Some(node) = namespace_node(k1, tree, members, *kid) {
                children.push(node);
            }
        }
    }
    if let Some(functions) = members.remove(&ns_id) {
        children.extend(functions);
    }
    if children.is_empty() {
        return None;
    }
    let name = k1.ident_str(k1.namespaces.get(ns_id).name).to_string();
    Some(Node { name, kind: "ns", insts: 0, location: None, children })
}

fn build_tree(k1: &mut TypedProgram) -> Result<Node, String> {
    let reachable = match Cg::prepare_host(k1) {
        Ok(roots) => roots.reachable,
        Err(e) => return Err(k1.ident_str(e.message).to_string()),
    };
    let mut members: FxHashMap<NamespaceId, Vec<Node>> = FxHashMap::default();
    let mut ability_members: FxHashMap<(NamespaceId, AbilityId), Vec<Node>> = FxHashMap::default();
    let mut ability_order: Vec<(NamespaceId, AbilityId)> = Vec::new();
    let mut specializations: FxHashMap<FunctionId, Vec<Node>> = FxHashMap::default();
    let mut generic_order: Vec<FunctionId> = Vec::new();
    let mut place = |k1: &TypedProgram, function_id: FunctionId, node: Node| {
        let (ns_id, ability_id) = placement(k1, function_id);
        match ability_id {
            None => members.entry(ns_id).or_default().push(node),
            Some(ability_id) => {
                let entry = ability_members.entry((ns_id, ability_id)).or_default();
                if entry.is_empty() {
                    ability_order.push((ns_id, ability_id));
                }
                entry.push(node);
            }
        }
    };
    for function_id in reachable {
        let function = k1.get_function(function_id);
        if let Some(info) = &function.specialization_info {
            let parent = info.parent_function;
            let label = k1.specialization_label(function_id);
            let entry = specializations.entry(parent).or_default();
            if entry.is_empty() {
                generic_order.push(parent);
            }
            entry.push(leaf(k1, function_id, label, "specialization"));
        } else {
            let kind = match function.kind {
                TypedFunctionKind::Lambda => "lambda",
                _ => "fn",
            };
            let label = k1.function_label(function_id);
            place(k1, function_id, leaf(k1, function_id, label, kind));
        }
    }
    for parent in generic_order {
        let children = specializations.remove(&parent).unwrap();
        let location = k1.span_location(k1.get_function_span(parent));
        let generic =
            Node { name: k1.function_label(parent), kind: "generic", insts: 0, location, children };
        place(k1, parent, generic);
    }
    for (ns_id, ability_id) in ability_order {
        let children = ability_members.remove(&(ns_id, ability_id)).unwrap();
        members.entry(ns_id).or_default().push(Node {
            name: format!("impl {}", k1.ident_str(k1.abilities.get(ability_id).name)),
            kind: "ability",
            insts: 0,
            location: None,
            children,
        });
    }
    let tree = super::browse::build_tree(k1);
    let root = namespace_node(k1, &tree, &mut members, tree.root);
    Ok(root.unwrap_or(Node {
        name: k1.program_name().to_string(),
        kind: "ns",
        insts: 0,
        location: None,
        children: Vec::new(),
    }))
}

pub fn render_page(k1: &mut TypedProgram) -> Markup {
    let tree = build_tree(k1);
    html! {
        (maud::DOCTYPE)
        html {
            (super::render_head())
            body data-init=(super::events_init_action()) {
                (super::render_site_header("size"))
                @match tree {
                    Err(message) => main .size-workspace { p .error { (message) } }
                    Ok(root) => {
                        main .size-workspace {
                            div .size-bar {
                                nav .size-crumbs #size-crumbs {}
                                span .size-stats #size-stats {}
                            }
                            div .treemap #treemap {}
                            div .size-tip #size-tip hidden {}
                        }
                        script {
                            "const SIZE_TREE = "
                            (PreEscaped(root.to_json().to_string().replace("</", "<\\/")))
                            ";"
                        }
                        script { (PreEscaped(include_str!("size.js"))) }
                    }
                }
            }
        }
    }
}
