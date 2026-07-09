use crate::lex::{Span, SpanId};
use crate::parse::{FileId, ParsedId, Sources};
use crate::typer::types::Type;
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

pub fn get_hover_message_for_entity(k1: &mut TypedProgram, entity: LsEntity) -> String {
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
        LsEntityKind::Function { function_id, is_defn } => {
            format!(
                "{}\n{}",
                if is_defn { "Function" } else { "Call" },
                k1.function_id_to_string(function_id, false)
            )
        }
        LsEntityKind::Variable { variable_id } => {
            let v = k1.variables.get(variable_id);
            let kind_str = match v.kind {
                VariableKind::FnParam(_) => "Param".to_string(),
                VariableKind::Stack(_) => "Local".to_string(),
                VariableKind::StackSynthetic(_) => "Compiler-generated".to_string(),
                VariableKind::Global(global_id) => {
                    let global = k1.globals.get(global_id);
                    format!("Global const={}, export={}", global.is_constant, global.is_exported)
                }
            };
            let type_str = k1.type_id_to_string(v.type_id);
            format!("{}\n{}", type_str, kind_str)
        }
        LsEntityKind::Type { type_id, applied_type_id } => {
            let layout_string = match k1.get_layout(type_id) {
                None => "No layout".to_string(),
                Some(layout) => format!("Size: {}, Align: {}", layout.size, layout.align),
            };
            let type_string = k1.type_id_to_string(type_id);
            let applied_type_string = match applied_type_id {
                None => "".to_string(),
                Some(type_id) => format!("Applied: {}", k1.type_id_to_string(type_id)),
            };
            format!("{type_string}\n{layout_string}\n{applied_type_string}")
        }
        LsEntityKind::Variant { type_id, .. } => {
            let layout_string = match k1.get_layout(type_id) {
                None => "No layout".to_string(),
                Some(layout) => format!("Size: {}, Align: {}", layout.size, layout.align),
            };
            let type_string = k1.type_id_to_string(type_id);
            format!("{type_string}\n{layout_string}")
        }
        LsEntityKind::StructField { type_id, field_index, access_kind } => {
            let struct_type = k1.types.get(type_id).as_struct().unwrap();
            let field = k1.types.mem.get_nth(struct_type.fields, field_index as usize);
            let field_type_string = k1.type_id_to_string(field.type_id);
            let access_kind_string = match access_kind {
                None => "",
                Some(ak) => match ak {
                    FieldAccessKind::ValueToValue => "Value Access",
                    FieldAccessKind::Dereference => "Dereferencing Access",
                    FieldAccessKind::ReferenceThrough => "Referencing Access (pointer to member)",
                },
            };
            format!("{}: {}\n{}", k1.ident_str(field.name), field_type_string, access_kind_string)
        }
    }
}

pub fn get_entity_definition_span(k1: &TypedProgram, entity_kind: LsEntityKind) -> SpanId {
    let span_id = match entity_kind {
        LsEntityKind::Namespace(ns_id) => {
            let ns = k1.namespaces.get(ns_id);
            eprintln!("span for ns: {}", k1.ident_str(ns.name));
            let span_id = k1.ast.get_span_for_id(ns.parsed_id);
            eprintln!("span id: {}", span_id);
            span_id
        }
        LsEntityKind::Function { function_id, .. } => {
            let function = k1.functions.get(function_id);
            let span_id = match function.parsed_id {
                ParsedId::Function(parsed_function_id) => {
                    k1.ast.functions.get(parsed_function_id).name_span
                }
                _ => k1.ast.get_span_for_id(function.parsed_id),
            };
            span_id
        }
        LsEntityKind::Variable { variable_id } => {
            let span_id = k1.variables.get(variable_id).defn_span;
            span_id
        }
        LsEntityKind::Type { type_id, .. } => {
            let defn_info = k1.types.defn_info.get(&type_id);
            match defn_info {
                Some(d) => {
                    let span_id = k1.ast.get_span_for_id(d.ast_id);
                    span_id
                }
                None => SpanId::NONE,
            }
        }
        LsEntityKind::Variant { type_id, variant_index } => match k1.types.get(type_id) {
            Type::Sum(sum) => {
                let variant = k1.types.sum_variant_by_index(sum.variants, variant_index);
                variant.name_span
            }
            Type::Enum(enum_type) => {
                let member = k1.types.mem.get_nth(enum_type.member_values, variant_index as usize);
                member.name_span
            }
            _ => {
                eprintln!("Invalid Variant entity; type is not a sum or enum!");
                SpanId::NONE
            }
        },
        LsEntityKind::StructField { type_id, field_index, .. } => match k1.types.get(type_id) {
            Type::Struct(struct_type) => {
                let field = k1.types.mem.get_nth(struct_type.fields, field_index as usize);
                field.span
            }
            _ => {
                eprintln!("Invalid StructField entity; type is not a struct!");
                SpanId::NONE
            }
        },
    };
    span_id
}

pub fn get_function_generic_id(k1: &TypedProgram, function_id: FunctionId) -> FunctionId {
    let function = k1.functions.get(function_id);
    match function.specialization_info {
        Some(info) => info.parent_function,
        None => function_id,
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
