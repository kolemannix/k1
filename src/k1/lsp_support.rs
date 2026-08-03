use crate::lex::{Span, SpanId, is_ident_char};
use crate::parse::{FileId, ParsedId, SourceFiles, StringId};
use crate::typer::scopes::{ScopeId, VariableInScope};
use crate::typer::types::{Type, TypeId};
use crate::{SV8, typer::*};
use fxhash::FxHashSet;
use smallvec::smallvec;

pub enum LangItem {
    Expr(TypedExprId),
    Defn(ParsedId),
}

pub fn is_point_in_span(sources: &SourceFiles, line: u32, col: u32, span: Span) -> bool {
    let source = sources.get(span.file_id);
    let Some(line_info) = source.get_line(line as usize) else {
        return false;
    };
    let char_index_abs = line_info.start_char + col;
    span.start <= char_index_abs && char_index_abs < span.end()
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
        LsEntityKind::StructField { type_id, field_index } => {
            let struct_type = k1.types.get(type_id).as_struct().unwrap();
            let field = k1.mem.get_nth(struct_type.fields, field_index as usize);
            let field_type_string = k1.type_id_to_string(field.type_id);
            format!("{}: {}", k1.ident_str(field.name), field_type_string)
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
            let defn_info = k1.type_defn_info.get(&type_id);
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
                let variant = k1.sum_variant_by_index(sum.variants, variant_index);
                variant.name_span
            }
            Type::Enum(enum_type) => {
                let member = k1.mem.get_nth(enum_type.member_values, variant_index as usize);
                member.name_span
            }
            _ => {
                eprintln!("Invalid Variant entity; type is not a sum or enum!");
                SpanId::NONE
            }
        },
        LsEntityKind::StructField { type_id, field_index, .. } => match k1.types.get(type_id) {
            Type::Struct(struct_type) => {
                let field = k1.mem.get_nth(struct_type.fields, field_index as usize);
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

/// Replace the identifier word containing `offset` with the completion marker
/// (pure insertion when there is no word), so the buffer parses and the typer
/// records the CompletionSite at the cursor
pub fn splice_completion_marker(content: &str, mut offset: usize) -> String {
    offset = offset.min(content.len());
    while offset > 0 && !content.is_char_boundary(offset) {
        offset -= 1;
    }
    let bytes = content.as_bytes();
    let mut wstart = offset;
    while wstart > 0 && bytes[wstart - 1].is_ascii() && is_ident_char(bytes[wstart - 1] as char) {
        wstart -= 1;
    }
    let mut wend = offset;
    while wend < bytes.len() && bytes[wend].is_ascii() && is_ident_char(bytes[wend] as char) {
        wend += 1;
    }
    format!("{}{}{}", &content[..wstart], COMPLETION_MARKER, &content[wend..])
}

/// Innermost typed block containing the point; the completion fallback when no
/// CompletionSite was recorded
pub fn scope_at_point(k1: &TypedProgram, file_id: FileId, line: u32, col: u32) -> Option<ScopeId> {
    let mut best: Option<(ScopeId, u32)> = None;
    for (expr_id, span_id) in k1.exprs.spans.iter_with_ids() {
        let span = k1.ast.spans.get(*span_id);
        if span.file_id != file_id || !is_point_in_span(&k1.ast.sources, line, col, span) {
            continue;
        }
        if let TypedExpr::Block(block) = k1.exprs.get(expr_id)
            && best.is_none_or(|(_, len)| span.len < len)
        {
            best = Some((block.scope_id, span.len));
        }
    }
    best.map(|(scope_id, _)| scope_id)
}

pub struct CompletionCandidate {
    pub label: String,
    pub kind: CompletionCandidateKind,
    pub detail: String,
    pub sort_group: u8,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CompletionCandidateKind {
    Field,
    Method,
    Variable,
    Function,
    Type,
    Namespace,
    Ability,
    Keyword,
}

const KEYWORDS: &[&str] = &[
    "fn", "let", "mut", "and", "or", "if", "else", "while", "loop", "ns", "intern", "for", "in",
    "ability", "impl", "is", "not", "builtin", "where", "context", "use", "require", "defer",
];

pub fn collect_completions(k1: &TypedProgram, site: CompletionSite) -> Vec<CompletionCandidate> {
    let mut items: Vec<CompletionCandidate> = vec![];
    match site {
        CompletionSite::Member { base_type_id, .. } => {
            let base_type_id = k1.get_static_family_id_if_static(base_type_id);
            if let Type::Struct(st) = k1.types.get(base_type_id) {
                for field in k1.mem.getn(st.fields) {
                    items.push(CompletionCandidate {
                        label: k1.ident_str(field.name).to_string(),
                        kind: CompletionCandidateKind::Field,
                        detail: k1.type_id_to_string(field.type_id),
                        sort_group: 0,
                    });
                }
            }
            if let Some(companion_ns) = k1.get_companion_namespace(base_type_id) {
                let companion_scope = k1.namespaces.get(companion_ns).scope_id;
                for (name, function_id) in k1.scopes.iter_scope_functions(companion_scope) {
                    if is_method_shaped(k1, function_id, base_type_id) {
                        items.push(CompletionCandidate {
                            label: k1.ident_str(name).to_string(),
                            kind: CompletionCandidateKind::Method,
                            detail: k1.function_id_to_string(function_id, false),
                            sort_group: 1,
                        });
                    }
                }
            }
        }
        CompletionSite::Scope { scope_id } => {
            collect_scope_chain(k1, scope_id, &mut items);
            for kw in KEYWORDS {
                items.push(CompletionCandidate {
                    label: kw.to_string(),
                    kind: CompletionCandidateKind::Keyword,
                    detail: String::new(),
                    sort_group: 9,
                });
            }
        }
        CompletionSite::Path { path_scope_id } => {
            collect_one_scope(k1, path_scope_id, &mut Seen::default(), &mut items);
        }
    }
    items.sort_by(|a, b| (a.sort_group, &a.label).cmp(&(b.sort_group, &b.label)));
    items
}

fn is_method_shaped(k1: &TypedProgram, function_id: FunctionId, base_type_id: TypeId) -> bool {
    let function = k1.get_function(function_id);
    if function.is_generic() {
        return true;
    }
    let Type::Function(ft) = k1.types.get(function.type_id) else { return false };
    let Some(first) = k1.mem.getn(ft.physical_params).iter().find(|p| !p.is_context) else {
        return false;
    };
    first.type_id == base_type_id || k1.get_base_for_method(first.type_id) == base_type_id
}

#[derive(Default)]
struct Seen {
    variables: FxHashSet<StringId>,
    functions: FxHashSet<StringId>,
    types: FxHashSet<StringId>,
    namespaces: FxHashSet<StringId>,
    abilities: FxHashSet<StringId>,
}

fn collect_scope_chain(k1: &TypedProgram, scope_id: ScopeId, items: &mut Vec<CompletionCandidate>) {
    let mut seen = Seen::default();
    let mut current = Some(scope_id);
    while let Some(scope_id) = current {
        collect_one_scope(k1, scope_id, &mut seen, items);
        current = k1.scopes.get_scope(scope_id).parent;
    }
}

fn collect_one_scope(
    k1: &TypedProgram,
    scope_id: ScopeId,
    seen: &mut Seen,
    items: &mut Vec<CompletionCandidate>,
) {
    for (name, in_scope) in k1.scopes.iter_scope_variables(scope_id) {
        // A masked name still hides outer definitions, so it claims the seen-slot
        if !seen.variables.insert(name) {
            continue;
        }
        let VariableInScope::Defined(variable_id) = in_scope else { continue };
        items.push(CompletionCandidate {
            label: k1.ident_str(name).to_string(),
            kind: CompletionCandidateKind::Variable,
            detail: k1.type_id_to_string(k1.variables.get(variable_id).type_id),
            sort_group: 2,
        });
    }
    for (name, function_id) in k1.scopes.iter_scope_functions(scope_id) {
        if seen.functions.insert(name) {
            items.push(CompletionCandidate {
                label: k1.ident_str(name).to_string(),
                kind: CompletionCandidateKind::Function,
                detail: k1.function_id_to_string(function_id, false),
                sort_group: 3,
            });
        }
    }
    for (name, type_id) in k1.scopes.iter_scope_types(scope_id) {
        if seen.types.insert(name) {
            items.push(CompletionCandidate {
                label: k1.ident_str(name).to_string(),
                kind: CompletionCandidateKind::Type,
                detail: k1.type_id_to_string(type_id),
                sort_group: 4,
            });
        }
    }
    for (name, _ns_id) in k1.scopes.iter_scope_namespaces(scope_id) {
        if seen.namespaces.insert(name) {
            items.push(CompletionCandidate {
                label: k1.ident_str(name).to_string(),
                kind: CompletionCandidateKind::Namespace,
                detail: "ns".to_string(),
                sort_group: 5,
            });
        }
    }
    for (name, _ability_id) in k1.scopes.abilities_in_scope(scope_id) {
        if seen.abilities.insert(name) {
            items.push(CompletionCandidate {
                label: k1.ident_str(name).to_string(),
                kind: CompletionCandidateKind::Ability,
                detail: "ability".to_string(),
                sort_group: 6,
            });
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

#[cfg(test)]
mod completion_tests {
    use super::*;
    use crate::compiler::{
        Args, Command, CompileProgramError, LspCompileOptions, compile_program_ext,
    };

    const M: &str = COMPLETION_MARKER;

    #[test]
    fn splice_marker() {
        assert_eq!(splice_completion_marker("foo.", 4), format!("foo.{M}"));
        assert_eq!(splice_completion_marker("foo.ba", 6), format!("foo.{M}"));
        // Mid-word: the whole word is replaced
        assert_eq!(splice_completion_marker("let ab = cd", 5), format!("let {M} = cd"));
        assert_eq!(splice_completion_marker("x = ", 4), format!("x = {M}"));
        assert_eq!(splice_completion_marker("ns/", 3), format!("ns/{M}"));
        // Kebab idents are one word
        assert_eq!(splice_completion_marker("a-long-name", 6), M.to_string());
    }

    /// Compiles `src` (cursor sigil `@@`) in completion mode: the file goes to
    /// disk verbatim, the marker-spliced version is the source override
    fn compile_with_cursor(test_name: &str, src: &str) -> Box<TypedProgram> {
        static SET_HOME: std::sync::Once = std::sync::Once::new();
        SET_HOME.call_once(|| unsafe {
            std::env::set_var("K1_HOME", env!("CARGO_MANIFEST_DIR"));
        });
        let cursor = src.find("@@").expect("fixture needs an @@ cursor");
        let content = src.replace("@@", "");
        let spliced = splice_completion_marker(&content, cursor);
        let dir = std::env::temp_dir().join("k1_completion_tests");
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join(format!("{test_name}.k1"));
        std::fs::write(&path, &content).unwrap();
        let mut source_overrides = fxhash::FxHashMap::default();
        source_overrides.insert(crate::kpath::canonicalize(&path).unwrap(), spliced);
        let args = Args {
            no_std: false,
            emit_llvm: false,
            optimize: false,
            dump_module: false,
            debug: false,
            sanitize: false,
            profile: false,
            chatty: false,
            optimize_ir: true,
            target: None,
            filc: false,
            command: Command::Check { file: path },
            dump_idents: false,
        };
        match compile_program_ext(&args, LspCompileOptions { source_overrides, completion: true }) {
            Ok(program) => Box::new(program),
            Err(CompileProgramError::TyperFailure(program)) => program,
        }
    }

    fn labels(k1: &TypedProgram) -> Vec<String> {
        let site =
            k1.completion.as_ref().and_then(|cs| cs.site).expect("expected a completion site");
        collect_completions(k1, site).into_iter().map(|c| c.label).collect()
    }

    #[test]
    fn member_completion_on_struct() {
        let k1 = compile_with_cursor(
            "member",
            r#"
ns completion-member

type point = { x: int, y: int }

ns for point {
  fn magnitude(self: point): int {
    self.x * self.x + self.y * self.y
  }
}

fn use-it(): int {
  let p: point = .{ x = 1, y = 2 }
  p.@@
  0
}
"#,
        );
        assert!(matches!(
            k1.completion.as_ref().unwrap().site,
            Some(CompletionSite::Member { .. })
        ));
        let labels = labels(&k1);
        assert!(labels.contains(&"x".to_string()));
        assert!(labels.contains(&"y".to_string()));
        assert!(labels.contains(&"magnitude".to_string()));
    }

    #[test]
    fn scope_completion() {
        let k1 = compile_with_cursor(
            "scope",
            r#"
ns completion-scope

fn helper(): int { 3 }

fn use-it(): int {
  let alpha = 1
  let beta = 2
  let gamma = @@
  gamma
}
"#,
        );
        assert!(matches!(k1.completion.as_ref().unwrap().site, Some(CompletionSite::Scope { .. })));
        let labels = labels(&k1);
        assert!(labels.contains(&"alpha".to_string()));
        assert!(labels.contains(&"beta".to_string()));
        assert!(labels.contains(&"helper".to_string()));
        assert!(labels.contains(&"fn".to_string()));
        // Not yet defined at the cursor
        assert!(!labels.contains(&"gamma".to_string()));
    }

    #[test]
    fn path_completion() {
        let k1 = compile_with_cursor(
            "path",
            r#"
ns completion-path

ns util {
  fn helper(): int { 3 }
  fn helper-two(): int { 4 }
}

fn use-it(): int {
  util/@@
  0
}
"#,
        );
        assert!(matches!(k1.completion.as_ref().unwrap().site, Some(CompletionSite::Path { .. })));
        let labels = labels(&k1);
        assert!(labels.contains(&"helper".to_string()));
        assert!(labels.contains(&"helper-two".to_string()));
        // Path enumeration is local to the namespace: no keywords, no outer names
        assert!(!labels.contains(&"use-it".to_string()));
        assert!(!labels.contains(&"fn".to_string()));
    }

    #[test]
    fn earlier_error_yields_no_site() {
        let k1 = compile_with_cursor(
            "earlier_error",
            r#"
ns completion-fallback

fn use-it(): int {
  let bad: int = "nope"
  let x = @@
  0
}
"#,
        );
        assert!(k1.completion.as_ref().unwrap().site.is_none());
    }
}
