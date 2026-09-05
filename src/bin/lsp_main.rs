// Copyright (c) 2026 knix
// All rights reserved.

use k1::debug;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, Mutex, RwLock};

use k1::compiler::{CompileProgramError, LspCompileOptions};
use k1::lex::{self, Span, SpanId};
use k1::lsp_support::CompletionCandidateKind;
use k1::parse;
use k1::parse::{ParsedProgram, SourceFile};
use k1::typer::*;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing::{error, info, warn};

const TOKEN_TYPES: [SemanticTokenType; 22] = [
    SemanticTokenType::NAMESPACE,
    SemanticTokenType::TYPE,
    SemanticTokenType::CLASS,
    SemanticTokenType::ENUM,
    SemanticTokenType::INTERFACE,
    SemanticTokenType::STRUCT,
    SemanticTokenType::TYPE_PARAMETER,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::ENUM_MEMBER,
    SemanticTokenType::EVENT,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::METHOD,
    SemanticTokenType::MACRO,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::MODIFIER,
    SemanticTokenType::COMMENT,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::REGEXP,
    SemanticTokenType::OPERATOR,
];
#[repr(u32)]
#[allow(unused)]
enum TokenTypes {
    Namespace = 0,
    Type = 1,
    Class = 2,
    Enum = 3,
    Interface = 4,
    Struct = 5,
    TypeParameter = 6,
    Parameter = 7,
    Variable = 8,
    Property = 9,
    EnumMember = 10,
    Event = 11,
    Function = 12,
    Method = 13,
    Macro = 14,
    Keyword = 15,
    Modifier = 16,
    Comment = 17,
    String = 18,
    Number = 19,
    Regexp = 20,
    Operator = 21,
}

const TOKEN_MODIFIERS: [SemanticTokenModifier; 10] = [
    SemanticTokenModifier::DECLARATION,
    SemanticTokenModifier::DEFINITION,
    SemanticTokenModifier::READONLY,
    SemanticTokenModifier::STATIC,
    SemanticTokenModifier::DEPRECATED,
    SemanticTokenModifier::ABSTRACT,
    SemanticTokenModifier::ASYNC,
    SemanticTokenModifier::MODIFICATION,
    SemanticTokenModifier::DOCUMENTATION,
    SemanticTokenModifier::DEFAULT_LIBRARY,
];
#[allow(unused)]
#[repr(u32)]
enum TokenModifiers {
    Declaration = 1 << 0, // nothing
    Definition = 1 << 1,  // nothing
    Readonly = 1 << 2,    // orange
    Static = 1 << 3,      // nothing
    Deprecated = 1 << 4,  // strikethrough
    Abstract = 1 << 5,    // nothing
    Async = 1 << 6,
    Modification = 1 << 7,
    Documentation = 1 << 8,
    DefaultLibrary = 1 << 9,
}

fn span_to_range_in_ast(ast: &ParsedProgram, span: Span) -> Option<Range> {
    let source = ast.sources.source_by_span(span);
    let (start_line, end_line) = source.get_lines_for_span(&ast.mem, span)?;
    Some(Range {
        start: Position {
            line: start_line.line_index,
            character: span.start - start_line.start_char,
        },
        end: Position { line: end_line.line_index, character: span.end() - end_line.start_char },
    })
}

fn span_id_to_range(k1: &TypedProgram, span_id: SpanId) -> Option<Range> {
    let span = k1.ast.spans.get(span_id);
    span_to_range(k1, span)
}

fn span_to_range(k1: &TypedProgram, span: Span) -> Option<Range> {
    span_to_range_in_ast(&k1.ast, span)
}

fn error_to_diagnostic(
    k1: &TypedProgram,
    message: String,
    level: MessageLevel,
    span_id: SpanId,
) -> Option<(Url, Diagnostic)> {
    if span_id == SpanId::NONE {
        return None;
    }
    let url = uri_from_span(k1, span_id);
    let severity = match level {
        MessageLevel::Error => DiagnosticSeverity::ERROR,
        MessageLevel::Warn => DiagnosticSeverity::WARNING,
        MessageLevel::Info => DiagnosticSeverity::INFORMATION,
        MessageLevel::Hint => DiagnosticSeverity::HINT,
    };
    match span_id_to_range(k1, span_id) {
        None => {
            error!("Failed span lookup for diagnostic: {}", &message);
            None
        }
        Some(range) => {
            let diagnostic = Diagnostic {
                range,
                severity: Some(severity),
                code: None,
                code_description: None,
                source: None,
                message,
                related_information: None,
                tags: None,
                data: None,
            };
            Some((url, diagnostic))
        }
    }
}

fn source_to_uri(path: impl AsRef<Path>) -> Url {
    debug!("source_to_uri on {:?}", path.as_ref());
    Url::from_file_path(path.as_ref()).unwrap()
}

fn uri_from_span(k1: &TypedProgram, span_id: SpanId) -> Url {
    let span = k1.ast.spans.get(span_id);
    let source = k1.ast.sources.get(span.file_id);
    source_to_uri(k1.ast.idents.get_string(source.file_path))
}

fn uri_to_source<'ast>(ast: &'ast ParsedProgram, url: &Url) -> Option<&'ast SourceFile> {
    let path = Path::new(url.path());
    debug!("uri_to_source: {}", path.display());
    let source = ast.sources.iter().find(|s| {
        let source_path = ast.idents.get_string(s.1.file_path);
        debug!("    source_path: {}", source_path);
        path == Path::new(source_path)
    });
    source.map(|s| s.1)
}

fn uri_to_edited_source(backend: &Backend, url: &Url) -> Option<(SourceFile, bool)> {
    match backend.edited_sources.lock().unwrap().get(url) {
        None => backend
            .with_k1(|k1| uri_to_source(&k1.ast, url).map(|source| (source.clone(), false)))
            .unwrap_or(None),
        Some(ast) => Some((ast.sources.get_main().clone(), true)),
    }
}

fn candidate_to_item(candidate: k1::lsp_support::CompletionCandidate) -> CompletionItem {
    let kind = match candidate.kind {
        CompletionCandidateKind::Field => CompletionItemKind::FIELD,
        CompletionCandidateKind::Variant => CompletionItemKind::ENUM_MEMBER,
        CompletionCandidateKind::Method => CompletionItemKind::METHOD,
        CompletionCandidateKind::Variable => CompletionItemKind::VARIABLE,
        CompletionCandidateKind::Function => CompletionItemKind::FUNCTION,
        CompletionCandidateKind::Type => CompletionItemKind::STRUCT,
        CompletionCandidateKind::Namespace => CompletionItemKind::MODULE,
        CompletionCandidateKind::Ability => CompletionItemKind::INTERFACE,
        CompletionCandidateKind::Keyword => CompletionItemKind::KEYWORD,
    };
    CompletionItem {
        kind: Some(kind),
        sort_text: Some(format!("{:02}_{}", candidate.sort_group, candidate.label)),
        detail: if candidate.detail.is_empty() { None } else { Some(candidate.detail) },
        insert_text_format: candidate.snippet.as_ref().map(|_| InsertTextFormat::SNIPPET),
        insert_text: candidate.snippet,
        label: candidate.label,
        ..CompletionItem::default()
    }
}

struct Backend {
    client: Client,
    module: k1::server::SharedProgram,
    edited_sources: Mutex<HashMap<Url, ParsedProgram>>,
    /// Canonical src_path of the program we compile (a module dir or single
    /// file), derived from the files the client opens and saves
    src_path: RwLock<Option<PathBuf>>,
    published_diagnostic_urls: Mutex<HashSet<Url>>,
    compile_iteration: AtomicU32,
    retarget_generation: AtomicU32,
    retarget_lock: tokio::sync::Mutex<()>,
    completion_generation: AtomicU32,
    completion_compile_lock: tokio::sync::Mutex<()>,
}

impl Backend {
    fn new(client: Client) -> Backend {
        let module: k1::server::SharedProgram = Arc::new(Mutex::new(None));
        let server_module = module.clone();
        std::thread::spawn(move || k1::server::serve(server_module));
        Backend {
            client,
            module,
            edited_sources: Mutex::new(HashMap::new()),
            src_path: RwLock::new(None),
            published_diagnostic_urls: Mutex::new(HashSet::new()),
            compile_iteration: AtomicU32::new(0),
            retarget_generation: AtomicU32::new(0),
            retarget_lock: tokio::sync::Mutex::new(()),
            completion_generation: AtomicU32::new(0),
            completion_compile_lock: tokio::sync::Mutex::new(()),
        }
    }

    /// Insert the completion marker at the cursor, replacing whatever token is there, and run a check compile;
    async fn compile_with_marker(
        &self,
        file_url: &Url,
        line: u32,
        col: u32,
    ) -> Result<Option<TypedProgram>> {
        info!("compile_with_marker {}", file_url.path());
        let content_at_line = |ast: &ParsedProgram, source: &SourceFile| {
            source
                .get_line(&ast.mem, line as usize)
                .map(|line_info| (source.content(&ast.mem).to_string(), line_info))
        };
        let spliced = {
            let content_and_line = {
                let edited_sources = self.edited_sources.lock().unwrap();
                edited_sources
                    .get(file_url)
                    .and_then(|ast| content_at_line(ast, ast.sources.get_main()))
            };
            let content_and_line = match content_and_line {
                Some(found) => Some(found),
                None => self
                    .with_k1(|k1| {
                        uri_to_source(&k1.ast, file_url)
                            .and_then(|source| content_at_line(&k1.ast, source))
                    })
                    .flatten(),
            };
            let Some((content, line_info)) = content_and_line else {
                info!("compile_with_marker: no source or line for {}", file_url.path());
                return Ok(None);
            };
            let offset = (line_info.start_char + col.min(line_info.len)) as usize;
            k1::lsp_support::splice_completion_marker(&content, offset)
        };

        let Ok(canonical_path) = k1::kpath::canonicalize_owned(file_url.path()) else {
            return Ok(None);
        };
        let Some(root_path) = self.target_file_for_url(file_url) else { return Ok(None) };

        let mut source_overrides = fxhash::FxHashMap::default();
        source_overrides.insert(canonical_path, spliced);
        let lsp_options = LspCompileOptions { source_overrides, completion: true };
        let args = k1::compiler::Args {
            no_std: false,
            emit_llvm: false,
            optimize: false,
            dump_module: false,
            debug: true,
            sanitize: false,
            profile: false,
            chatty: false,
            optimize_ir: true,
            target: None,
            cache: true,
            filc: false,
            k1_home_override: None,
            command: k1::compiler::Command::Check { file: Some(root_path) },
            dump_idents: false,
            dump_trace: false,
        };

        let my_generation = self.completion_generation.fetch_add(1, Ordering::SeqCst) + 1;
        let _compile_guard = self.completion_compile_lock.lock().await;
        if self.completion_generation.load(Ordering::SeqCst) != my_generation {
            return Err(Error::request_cancelled());
        }
        let program =
            tokio::task::spawn_blocking(move || {
                match k1::compiler::compile_program_ext(&args, lsp_options) {
                    Ok(program) => program,
                    Err(CompileProgramError::TyperFailure(program)) => *program,
                }
            })
            .await
            .map_err(|_| Error::internal_error())?;
        Ok(Some(program))
    }

    fn with_k1<T>(&self, f: impl Fn(&TypedProgram) -> T) -> Option<T> {
        let m_lock = self.module.lock().unwrap();
        match &*m_lock {
            None => None,
            Some(k1) => Some(f(k1)),
        }
    }

    fn all_file_urls(&self) -> Vec<Url> {
        self.with_k1(|k1| {
            k1.ast
                .sources
                .iter()
                .map(|s| source_to_uri(k1.ast.idents.get_string(s.1.file_path)))
                .collect()
        })
        .unwrap_or_default()
    }

    fn list_all_errors(&self) -> Vec<(Url, Diagnostic)> {
        let errors: Vec<(Url, Diagnostic)> = self
            .with_k1(|k1| {
                k1.ast
                    .errors
                    .iter()
                    .filter_map(|e| {
                        error_to_diagnostic(
                            k1,
                            format!("Parse Error: {}", e.message()),
                            MessageLevel::Error,
                            e.span(),
                        )
                    })
                    .chain(k1.messages.borrow().iter().filter_map(|e| {
                        error_to_diagnostic(
                            k1,
                            k1.ident_str(e.message).to_string(),
                            e.level,
                            e.span,
                        )
                    }))
                    .collect()
            })
            .unwrap_or_default();
        errors
    }

    fn build_all_files_and_errors_map(&self) -> HashMap<Url, Vec<Diagnostic>> {
        let all_files = self.all_file_urls();
        let all_errors = self.list_all_errors();
        let mut map: HashMap<Url, Vec<Diagnostic>> = HashMap::new();
        for file in all_files {
            map.insert(file, vec![]);
        }

        for (url, diagnostic) in all_errors {
            map.entry(url).or_default().push(diagnostic);
        }

        map
    }

    fn target_file_for_url(&self, file_url: &Url) -> Option<PathBuf> {
        let file_path = file_url.to_file_path().ok()?;
        let in_program =
            self.with_k1(|k1| uri_to_source(&k1.ast, file_url).is_some()).unwrap_or(false);
        if in_program {
            if let Some(stored) = self.src_path.read().unwrap().clone() {
                return Some(stored);
            }
        }
        Some(k1::compiler::find_check_target_for_file(&file_path))
    }

    /// Point the program at `file_url`'s module if it isn't already covered,
    /// then compile when the target changed, no program exists yet, or
    /// `force_compile` (saves). Publishes diagnostics after compiling.
    /// Returns whether a compile ran.
    async fn ensure_target_and_compile(&self, file_url: &Url, force_compile: bool) -> bool {
        let my_generation = self.retarget_generation.fetch_add(1, Ordering::SeqCst) + 1;
        let _guard = self.retarget_lock.lock().await;
        if self.retarget_generation.load(Ordering::SeqCst) != my_generation {
            // A newer open/save superseded this one while we waited
            return false;
        }
        let Some(target) = self.target_file_for_url(file_url) else {
            return false;
        };
        let changed = {
            let mut src_path = self.src_path.write().unwrap();
            let changed = src_path.as_deref() != Some(target.as_path());
            *src_path = Some(target);
            changed
        };
        let no_program = self.with_k1(|_| ()).is_none();
        if !(changed || no_program || force_compile) {
            return false;
        }
        self.compile();
        self.send_diagnostics().await;
        true
    }

    fn compile(&self) -> u32 {
        let iteration_number = self.compile_iteration.load(Ordering::Relaxed);
        let src_path = self.src_path.read().unwrap().clone();
        let Some(src_path) = src_path else {
            info!("compile {}: no target yet", iteration_number);
            return iteration_number;
        };
        info!("compiling version {} target {}", iteration_number, src_path.display());
        let compile_start = std::time::Instant::now();
        let args = k1::compiler::Args {
            no_std: false,
            emit_llvm: false,
            optimize: false,
            dump_module: false,
            debug: true,
            sanitize: false,
            profile: false,
            chatty: false,
            optimize_ir: true,
            target: None,
            cache: true,
            filc: false,
            k1_home_override: None,
            command: k1::compiler::Command::Check { file: Some(src_path) },
            dump_idents: false,
            dump_trace: false,
        };
        let compile_result = k1::compiler::compile_program(&args);
        let compiled_module = match compile_result {
            Ok(module) => {
                info!(
                    "compile {} succeeded in {}ms",
                    iteration_number,
                    compile_start.elapsed().as_millis()
                );
                Some(Box::new(module))
            }
            Err(CompileProgramError::TyperFailure(module)) => {
                info!("compile {} typing failed", iteration_number);
                Some(module)
            }
        };

        let mut module_lock = self.module.lock().unwrap();
        *module_lock = compiled_module;
        let prev_iteration = self.compile_iteration.fetch_add(1, Ordering::Relaxed);
        prev_iteration + 1
    }

    async fn send_diagnostics(&self) {
        let version = self.compile_iteration.load(Ordering::Relaxed);
        let errors_by_file = self.build_all_files_and_errors_map();
        // Clear diagnostics for files that left the program (target switch)
        let stale = {
            let mut published = self.published_diagnostic_urls.lock().unwrap();
            let mut stale = vec![];
            for url in published.iter() {
                if !errors_by_file.contains_key(url) {
                    stale.push(url.clone());
                }
            }
            published.clear();
            published.extend(errors_by_file.keys().cloned());
            stale
        };
        for url in stale {
            self.client.publish_diagnostics(url, vec![], Some(version as i32)).await;
        }
        for (file_url, errors) in errors_by_file.into_iter() {
            if !errors.is_empty() {
                info!("Sending {} diagnostics for {file_url} with version {version}", errors.len());
            }
            self.client.publish_diagnostics(file_url, errors, Some(version as i32)).await;
        }
        // Errors with no source position (module resolution failures) can't
        // be diagnostics; show them so a dead compile is never silent
        let spanless = self
            .with_k1(|k1| {
                let mut messages = vec![];
                for m in k1.messages.borrow().iter() {
                    if m.level == MessageLevel::Error && m.span == SpanId::NONE {
                        messages.push(k1.ident_str(m.message).to_string());
                    }
                }
                messages
            })
            .unwrap_or_default();
        for message in spanless {
            self.client.show_message(MessageType::ERROR, message).await;
        }
    }

    fn get_typer_errors(&self, file_url: &Url) -> Vec<K1Message> {
        let module_lock = self.module.lock().unwrap();
        let Some(k1) = &*module_lock else {
            return vec![];
        };
        let Some(source) = uri_to_source(&k1.ast, file_url) else {
            info!("Could not get source for {}", file_url.path());
            return vec![];
        };
        let file_id = source.file_id;
        k1.messages
            .borrow()
            .iter()
            .filter(|m| {
                let span = k1.ast.spans.get(m.span);
                span.file_id == file_id
            })
            .cloned()
            .collect()
    }

    fn messages_to_diagnostics(&self, messages: &[K1Message]) -> Vec<Diagnostic> {
        self.with_k1(|k1| {
            messages
                .iter()
                .filter_map(|k1_message| {
                    error_to_diagnostic(
                        k1,
                        k1.ident_str(k1_message.message).to_string(),
                        k1_message.level,
                        k1_message.span,
                    )
                    .map(|p| p.1)
                })
                .collect()
        })
        .unwrap_or_default()
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let mut res = InitializeResult::default();
        res.capabilities.text_document_sync =
            Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                will_save: Some(false),
                will_save_wait_until: Some(false),
                save: Some(TextDocumentSyncSaveOptions::SaveOptions({
                    SaveOptions { include_text: None }
                })),
            }));
        res.capabilities.definition_provider = Some(OneOf::Left(true));
        res.capabilities.references_provider = Some(OneOf::Left(true));
        res.capabilities.hover_provider = Some(HoverProviderCapability::Simple(true));
        res.capabilities.diagnostic_provider =
            Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
                inter_file_dependencies: true,
                identifier: None,
                workspace_diagnostics: true,
                work_done_progress_options: WorkDoneProgressOptions { work_done_progress: None },
            }));
        res.capabilities.semantic_tokens_provider =
            Some(SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                work_done_progress_options: WorkDoneProgressOptions { work_done_progress: None },
                legend: SemanticTokensLegend {
                    token_types: Vec::from(TOKEN_TYPES),
                    token_modifiers: Vec::from(TOKEN_MODIFIERS),
                },
                range: Some(false),
                full: Some(SemanticTokensFullOptions::Delta { delta: Some(false) }),
            }));
        // res.capabilities.semantic_tokens_provider = None;
        res.capabilities.completion_provider = Some(CompletionOptions {
            resolve_provider: Some(false),
            trigger_characters: Some(vec![".".to_string(), ":".to_string(), "/".to_string()]),
            all_commit_characters: Some(vec!["\n".to_string()]),
            work_done_progress_options: WorkDoneProgressOptions::default(),
            completion_item: Some(CompletionOptionsCompletionItem {
                label_details_support: Some(false),
            }),
        });
        res.capabilities.signature_help_provider = Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
            retrigger_characters: Some(vec![",".to_string()]),
            work_done_progress_options: WorkDoneProgressOptions::default(),
        });
        res.server_info =
            Some(ServerInfo { name: "k1lsp".to_string(), version: Some("ALPHA".to_string()) });
        info!("Got initialize params: {params:#?}");
        Ok(res)
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("K1 LSP initialized");

        self.send_diagnostics().await;
        self.client.show_message(MessageType::INFO, "K1 lsp initialized!").await;
        self.client.log_message(MessageType::INFO, "K1 server initialized!").await;
    }

    async fn shutdown(&self) -> Result<()> {
        info!("K1 LSP shutting down");
        Ok(())
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let file_url = params.text_document.uri;
        info!("textDocument/did_change: {}", &file_url);
        let Some(change) = params.content_changes.into_iter().next() else {
            error!("expect a change");
            return;
        };
        if change.range.is_some() || change.range_length.is_some() {
            error!("expect full content");
            return;
        }
        let new_content = change.text;
        info!("textDocument/did_change: parsing file {}", &file_url);
        let ast = parse::parse_standalone(file_url.path().to_string(), &new_content);
        let mut parse_diagnostics = vec![];
        info!(
            "textDocument/did_change: parsed file {} with {} errors",
            &file_url,
            ast.errors.len()
        );
        for error in &ast.errors {
            if let Some(range) = span_to_range_in_ast(&ast, ast.spans.get(error.span())) {
                let diagnostic = Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some(ast.name_str().to_string()),
                    message: error.message().to_string(),
                    related_information: None,
                    tags: None,
                    data: None,
                };
                parse_diagnostics.push(diagnostic)
            }
        }

        {
            let mut edited_sources = self.edited_sources.lock().unwrap();
            edited_sources.insert(file_url.clone(), ast);
        }
        let version = self.compile_iteration.load(Ordering::Relaxed);

        let typer_errors = self.get_typer_errors(&file_url);
        let mut all_file_diagnostics = self.messages_to_diagnostics(&typer_errors);
        all_file_diagnostics.extend(parse_diagnostics);
        self.client.publish_diagnostics(file_url, all_file_diagnostics, Some(version as i32)).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let start = std::time::Instant::now();
        let position = params.text_document_position_params;
        let file_url = position.text_document.uri;
        let hover_line_index = position.position.line;
        let hover_col = position.position.character;
        let mut module = self.module.lock().unwrap();
        let Some(k1) = &mut *module else {
            warn!("Parsed but not typed");
            return Ok(None);
        };

        info!("hover: {}:{}:{}", file_url.path(), hover_line_index, hover_col);
        let Some(source) = uri_to_source(&k1.ast, &file_url) else {
            info!("Could not get source for {}", file_url.path());
            return Ok(None);
        };
        let file_id = source.file_id;
        if let Some(entity) =
            k1::lsp_support::find_entity_at_point(k1, file_id, hover_line_index, hover_col)
        {
            let hover_msg = k1::lsp_support::get_hover_message_for_entity(k1, entity);
            return Ok(Some(Hover {
                contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
                    language: "txt".to_string(),
                    value: hover_msg,
                })),
                range: None,
            }));
        }

        let expr = k1::lsp_support::get_expr_at_point(k1, file_id, hover_line_index, hover_col);
        let elapsed = start.elapsed();
        info!("hover computed in {:.2?}", elapsed);
        match expr {
            None => Ok(None),
            Some(hover_msg) => Ok(Some(Hover {
                contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
                    language: "txt".to_string(),
                    value: hover_msg,
                })),
                range: None,
            })),
        }
    }

    async fn workspace_diagnostic(
        &self,
        params: WorkspaceDiagnosticParams,
    ) -> Result<WorkspaceDiagnosticReportResult> {
        let _ = params;
        info!("Got a workspace/diagnostic request");
        let errors_by_file = self.build_all_files_and_errors_map();

        let items = errors_by_file
            .into_iter()
            .map(|(file_url, diags)| {
                WorkspaceDocumentDiagnosticReport::Full(WorkspaceFullDocumentDiagnosticReport {
                    uri: file_url,
                    version: None,
                    full_document_diagnostic_report: FullDocumentDiagnosticReport {
                        result_id: None,
                        items: diags,
                    },
                })
            })
            .collect();

        Ok(WorkspaceDiagnosticReportResult::Report(WorkspaceDiagnosticReport { items }))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let file_url = params.text_document.uri;
        let Some((source, is_edited)) = uri_to_edited_source(self, &file_url) else {
            info!("Could not get source for {}", file_url.path());
            return Ok(None);
        };
        info!(
            "semantic_tokens {}. tokens={} is_edited={is_edited}",
            file_url.path(),
            source.tokens.len()
        );
        self.with_k1(|k1| {
            let mut tokens: Vec<SemanticToken> = vec![];
            let mut prev_line = 1;
            let mut prev_start_col = 0;

            let edited_sources = self.edited_sources.lock().unwrap();
            let ast_for_file: &ParsedProgram = match is_edited {
                false => &k1.ast,
                true => edited_sources.get(&file_url).unwrap(),
            };
            // The goal is to use only 'atoms' to avoid overlaps and backwards movement
            let mut spans_and_kinds = vec![];
            for semantic_token in ast_for_file.semantic_tokens.iter() {
                if semantic_token.span.file_id == source.file_id {
                    let token_type = match semantic_token.kind {
                        parse::SemanticTokenKind::Type => TokenTypes::Type,
                        parse::SemanticTokenKind::Variable => TokenTypes::Variable,
                        parse::SemanticTokenKind::String => TokenTypes::String,
                        parse::SemanticTokenKind::Keyword => TokenTypes::Keyword,
                        parse::SemanticTokenKind::Function => TokenTypes::Function,
                        parse::SemanticTokenKind::Namespace => TokenTypes::Namespace,
                        parse::SemanticTokenKind::Operator => TokenTypes::Operator,
                    };
                    spans_and_kinds.push((semantic_token.span, token_type as u32, 0))
                }
            }
            for entry in ast_for_file.mem.getn_lt(source.trivia) {
                match entry.trivia.kind {
                    lex::TokenTriviaKind::LineComment => {
                        let span = ast_for_file.spans.get(entry.trivia.span);
                        spans_and_kinds.push((span, TokenTypes::Comment as u32, 0));
                    }
                    _ => {}
                }
            }
            spans_and_kinds.sort_by_key(|(span, _, _)| span.start);
            for (span, token_type, bitflags) in spans_and_kinds {
                // info!("spans_and_kinds sorted {} {}", span.start, span.len);
                let length = span.len;
                let Some(line) = source.get_line_for_span_start(&ast_for_file.mem, span) else {
                    continue;
                };
                let line_number = line.line_number();
                let start_col = span.start - line.start_char;
                let delta_line = line_number - prev_line;
                let delta_start =
                    if delta_line == 0 { start_col - prev_start_col } else { start_col };

                prev_line = line_number;
                prev_start_col = start_col;
                let token = SemanticToken {
                    delta_line,
                    delta_start,
                    length,
                    token_type,
                    token_modifiers_bitset: bitflags,
                };
                // info!("pushing token {:?}", token);
                tokens.push(token);
            }
            info!(
                "semantic_tokens: iterated {} tokens, returning {}",
                ast_for_file.semantic_tokens.len(),
                tokens.len()
            );
            Ok(Some(SemanticTokensResult::Tokens(SemanticTokens { result_id: None, data: tokens })))
        })
        .unwrap_or(Ok(None))
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Result<DocumentDiagnosticReportResult> {
        info!("textDocument/diagnostic: returning unchanged for unsaved single-file diagnostics");
        let _ = params;
        Ok(DocumentDiagnosticReportResult::Report(DocumentDiagnosticReport::Unchanged(
            RelatedUnchangedDocumentDiagnosticReport {
                related_documents: None,
                unchanged_document_diagnostic_report: UnchangedDocumentDiagnosticReport {
                    result_id: "na".to_string(),
                },
            },
        )))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let file_url = params.text_document.uri;
        info!("handling did_open for document: {}", file_url.path());
        self.ensure_target_and_compile(&file_url, false).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let mut es = self.edited_sources.lock().unwrap();
        es.remove(&params.text_document.uri);
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        info!("handling did_save for document: {}", params.text_document.uri.path());
        {
            let mut es = self.edited_sources.lock().unwrap();
            es.remove(&params.text_document.uri);
        }
        let start = std::time::Instant::now();
        let compiled = self.ensure_target_and_compile(&params.text_document.uri, true).await;
        if !compiled {
            return;
        }
        let elapsed_ms = start.elapsed().as_millis();
        self.client.semantic_tokens_refresh().await.unwrap();
        self.client
            .show_message(
                MessageType::INFO,
                format!("recompiled {} in {}ms", params.text_document.uri.path(), elapsed_ms),
            )
            .await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let start = std::time::Instant::now();
        let position = params.text_document_position;
        let file_url = position.text_document.uri;
        let line = position.position.line;
        let col = position.position.character;
        info!("completion: {}:{}:{}", file_url.path(), line, col);

        let Some(program) = self.compile_with_marker(&file_url, line, col).await? else {
            return Ok(None);
        };

        let site = program.completion.as_ref().and_then(|cs| cs.site);
        let (candidates, is_incomplete) = match site {
            Some(site) => (k1::lsp_support::collect_completions(&program, site), false),
            None => {
                info!("completion: no site recorded, falling back to enclosing scope");
                let fallback = self
                    .with_k1(|k1| {
                        let source = uri_to_source(&k1.ast, &file_url)?;
                        let scope_id =
                            k1::lsp_support::scope_at_point(k1, source.file_id, line, col)?;
                        Some(k1::lsp_support::collect_completions(
                            k1,
                            CompletionSite::Scope { scope_id },
                        ))
                    })
                    .flatten();
                (fallback.unwrap_or_default(), true)
            }
        };

        let items: Vec<CompletionItem> = candidates.into_iter().map(candidate_to_item).collect();
        info!("completion: {} items in {}ms", items.len(), start.elapsed().as_millis());
        Ok(Some(CompletionResponse::List(CompletionList { is_incomplete, items })))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let position = params.text_document_position_params;
        let file_url = position.text_document.uri;
        let line = position.position.line;
        let col = position.position.character;
        info!("signature_help: {}:{}:{}", file_url.path(), line, col);

        let Some(program) = self.compile_with_marker(&file_url, line, col).await? else {
            return Ok(None);
        };
        let Some(site) = program.completion.as_ref().and_then(|cs| cs.site) else {
            return Ok(None);
        };
        let Some(info) = k1::lsp_support::signature_help_info(&program, site) else {
            return Ok(None);
        };
        let parameters = info
            .params
            .iter()
            .map(|p| ParameterInformation {
                label: ParameterLabel::Simple(p.clone()),
                documentation: None,
            })
            .collect();
        Ok(Some(SignatureHelp {
            signatures: vec![SignatureInformation {
                label: info.label,
                documentation: None,
                parameters: Some(parameters),
                active_parameter: Some(info.active_param),
            }],
            active_signature: Some(0),
            active_parameter: Some(info.active_param),
        }))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let line = position.line;
        let char = position.character;

        let mut module = self.module.lock().unwrap();
        let Some(k1) = &mut *module else {
            error!("Parsed but not typed");
            return Ok(None);
        };

        info!("goto_definition: {}:{}:{}", uri.path(), line + 1, char + 1);
        let Some(requested_source) = uri_to_source(&k1.ast, &uri) else {
            error!("Could not get source for {}", uri.path());
            return Ok(None);
        };
        let file_id = requested_source.file_id;
        let Some(entity) = k1::lsp_support::find_entity_at_point(k1, file_id, line, char) else {
            info!("No entity at point");
            return Ok(None);
        };

        let definition_span = k1::lsp_support::get_entity_definition_span(k1, entity.kind);
        if definition_span == Span::NONE {
            error!("definition span is nil");
            return Ok(None);
        }
        let definition_source = k1.ast.sources.get(definition_span.file_id);
        let Some(range) = span_to_range(k1, definition_span) else {
            error!("Failed to convert span to range for goto_definition");
            return Ok(None);
        };
        let definition_uri = source_to_uri(k1.ast.idents.get_string(definition_source.file_path));
        info!("goto_definition response: {}, {:?}", definition_uri, range);
        Ok(Some(GotoDefinitionResponse::Scalar(Location { uri: definition_uri, range })))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let text_document_position = params.text_document_position;
        let position = text_document_position.position;
        let uri = text_document_position.text_document.uri;
        let line = position.line;
        let char = position.character;
        let include_declaration = params.context.include_declaration;

        self.with_k1(|k1| find_references(k1, &uri, line, char, include_declaration))
            .unwrap_or(Ok(None))
    }
}

fn find_references(
    k1: &TypedProgram,
    request_uri: &Url,
    line: u32,
    char: u32,
    include_declaration: bool,
) -> Result<Option<Vec<Location>>> {
    info!("references: {}:{}:{}", request_uri.path(), line + 1, char + 1);
    let Some((_source, ls_entity)) = find_entity_and_source(k1, request_uri, line, char) else {
        return Ok(None);
    };

    // References are served by scanning ls_entities across all files: every
    // usage the typer sees emits an entity at its span; re-evaluated bodies
    // (generic specializations, macro arguments used twice) repeat spans
    let mut spans: Vec<Span> = Vec::new();
    match ls_entity.kind {
        LsEntityKind::Function { function_id, .. } => {
            let target = k1::lsp_support::get_function_generic_id(k1, function_id);
            for entities in k1.ls_entities.borrow().values() {
                for entity in entities {
                    let LsEntityKind::Function { function_id: fid, is_defn } = entity.kind else {
                        continue;
                    };
                    if is_defn && !include_declaration {
                        continue;
                    }
                    if k1::lsp_support::get_function_generic_id(k1, fid) != target {
                        continue;
                    }
                    spans.push(entity.span);
                }
            }
        }
        LsEntityKind::Variable { variable_id } => {
            let defn_span =
                k1.remap_span(k1.ast.spans.get(k1.variables.get(variable_id).defn_span));
            for entities in k1.ls_entities.borrow().values() {
                for entity in entities {
                    let LsEntityKind::Variable { variable_id: vid } = entity.kind else {
                        continue;
                    };
                    if vid != variable_id {
                        continue;
                    }
                    if entity.span == defn_span && !include_declaration {
                        continue;
                    }
                    spans.push(entity.span);
                }
            }
        }
        LsEntityKind::Namespace(_) | LsEntityKind::Type { .. } => return Ok(None),
        LsEntityKind::Variant { .. } => {
            // This will be really useful; a variant that is never constructed...
            return Ok(None);
        }
        LsEntityKind::StructField { .. } => {
            // This will be really useful; a field that is never accessed...
            return Ok(None);
        }
    }
    spans.sort_by_key(|span| (span.file_id, span.start, span.len));
    spans.dedup();
    let mut locations = Vec::new();
    for span in spans {
        if let Some(location) = location_from_resolved_span(k1, span) {
            locations.push(location);
        }
    }
    Ok(Some(locations))
}

fn location_from_resolved_span(k1: &TypedProgram, span: Span) -> Option<Location> {
    let range = span_to_range(k1, span)?;
    let source = k1.ast.sources.get(span.file_id);
    let uri = source_to_uri(k1.ast.idents.get_string(source.file_path));
    Some(Location { uri, range })
}

fn find_entity_and_source<'k1>(
    k1: &'k1 TypedProgram,
    uri: &Url,
    line: u32,
    char: u32,
) -> Option<(&'k1 SourceFile, LsEntity)> {
    let Some(source) = uri_to_source(&k1.ast, uri) else {
        error!("Could not get source for {}", uri.path());
        return None;
    };
    let file_id = source.file_id;
    let Some(entity) = k1::lsp_support::find_entity_at_point(k1, file_id, line, char) else {
        info!("No entity at point");
        return None;
    };
    Some((source, entity))
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let file_appender = tracing_appender::rolling::daily(".", "k1_lsp.log");
    tracing_subscriber::fmt().compact().with_ansi(false).with_writer(file_appender).init();

    let cwd = std::env::current_dir().unwrap();
    info!("K1 LSP. CWD: {}", cwd.to_string_lossy());

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
