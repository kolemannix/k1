use crate::parse::{FileId, ParsedId};
use crate::typer::types::Layout;
use crate::{SV8, typer::*};
use smallvec::smallvec;

pub enum LangItem {
    Expr(TypedExprId),
    Defn(ParsedId),
}

pub fn get_expr_at_point(
    k1: &TypedProgram,
    file: FileId,
    line_index: u32,
    char_index: u32,
) -> Option<String> {
    let mut matching_exprs: SV8<_> = smallvec![];
    for (expr_id, span_id) in k1.exprs.spans.iter_with_ids() {
        let span = k1.ast.spans.get(*span_id);
        if span.file_id == file {
            if let Some((start, _end)) = k1.ast.sources.get_lines_for_span(span) {
                if start.line_index == line_index {
                    let char_index_abs = start.start_char + char_index;
                    if span.start <= char_index_abs && char_index_abs < span.end() {
                        matching_exprs.push((expr_id, span.len));
                    }
                }
            }
        }
    }
    matching_exprs.sort_by_key(|(_, len)| *len);
    if let Some((expr_id, _)) = matching_exprs.first() {
        let type_id = k1.exprs.get_type(*expr_id);
        let type_string = k1.type_id_to_string(type_id);
        let layout = k1.types.get_layout_nonmut(type_id).unwrap_or(Layout::ZERO_SIZED);
        let expr_string = k1.expr_to_string(*expr_id);
        let kind_name = k1.exprs.get(*expr_id).kind_str();
        let msg = format!(
            "Kind: {kind_name}\n`{expr_string}`\n`{type_string}`\nSize: {}, Align: {}",
            layout.size, layout.align
        );
        return Some(msg);
    }
    None
}
