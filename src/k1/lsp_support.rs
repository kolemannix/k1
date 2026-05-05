use crate::lex::Span;
use crate::parse::{FileId, ParsedId, Sources};
use crate::{SV8, typer::*};
use smallvec::smallvec;

pub enum LangItem {
    Expr(TypedExprId),
    Defn(ParsedId),
}

pub fn is_point_in_span(sources: &Sources, line: u32, col: u32, span: Span) -> bool {
    if let Some((entity_start_line, _entity_end_line)) = sources.get_lines_for_span(span) {
        // Only works for single-line spans
        if entity_start_line.line_index == line {
            let char_index_abs = entity_start_line.start_char + col;
            if span.start <= char_index_abs && char_index_abs < span.end() {
                return true;
            }
        }
    }
    false
}

pub fn find_entity_at_point(
    k1: &TypedProgram,
    file_id: FileId,
    // 0-based
    line: u32,
    // 0-based
    col: u32,
) -> Option<LsEntity> {
    let ls_entities = k1.ls_entities.borrow();
    if let Some(entities) = ls_entities.get(&file_id) {
        for entity in entities {
            if is_point_in_span(&k1.ast.sources, line, col, entity.span) {
                return Some(*entity);
            }
        }
    }
    None
}

pub fn get_hover_message_for_entity(k1: &TypedProgram, entity: LsEntity) -> String {
    match entity.kind {
        LsEntityKind::Namespace(ns_id) => {
            let ns = k1.namespaces.get(ns_id);
            let companion_type = match ns.companion_type_id {
                None => "None".to_string(),
                Some(type_id) => format!("Companion Type: {}", k1.type_id_to_string(type_id)),
            };
            let ns_name_qualified = k1.scope_id_to_string(ns.scope_id);
            format!("ns {ns_name_qualified}. {companion_type}")
        }
        LsEntityKind::FunctionCall { function_id } => {
            format!("Call\n{}", k1.function_id_to_string(function_id, false))
        }
    }
}

pub fn get_entity_definition_span(k1: &TypedProgram, entity_kind: LsEntityKind) -> Span {
    match entity_kind {
        LsEntityKind::Namespace(ns_id) => {
            let ns = k1.namespaces.get(ns_id);
            eprintln!("span for ns: {}", k1.ident_str(ns.name));
            let span_id = k1.ast.get_span_for_id(ns.parsed_id);
            eprintln!("span id: {}", span_id);
            k1.ast.spans.get(span_id)
        }
        LsEntityKind::FunctionCall { function_id } => {
            let function = k1.functions.get(function_id);
            let span_id = match function.parsed_id {
                ParsedId::Function(parsed_function_id) => {
                    k1.ast.functions.get(parsed_function_id).name_span
                }
                _ => k1.ast.get_span_for_id(function.parsed_id),
            };
            k1.ast.spans.get(span_id)
        }
    }
}

pub fn get_expr_at_point(
    k1: &mut TypedProgram,
    file: FileId,
    line_index: u32,
    char_index: u32,
) -> Option<String> {
    let mut matching_exprs: SV8<_> = smallvec![];
    for (expr_id, span_id) in k1.exprs.spans.iter_with_ids() {
        let span = k1.ast.spans.get(*span_id);
        if span.file_id == file {
            if is_point_in_span(&k1.ast.sources, line_index, char_index, span) {
                matching_exprs.push((expr_id, span.len));
            }
        }
    }
    matching_exprs.sort_by_key(|(_, len)| *len);
    if let Some((expr_id, _)) = matching_exprs.first() {
        let type_id = k1.exprs.get_type(*expr_id);
        let type_string = k1.type_id_to_string(type_id);
        let layout_string = match k1.get_layout(type_id) {
            None => "No layout".to_string(),
            Some(layout) => format!("Size: {}, Align: {}", layout.size, layout.align),
        };
        let expr_string = k1.expr_to_string(*expr_id);
        let kind_name = k1.exprs.get(*expr_id).kind_name();
        let msg = format!("Kind: {kind_name}\n`{expr_string}`\n`{type_string}`\n{layout_string}",);
        return Some(msg);
    }
    None
}
