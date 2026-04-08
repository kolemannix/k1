// Copyright (c) 2025 knix
// All rights reserved.

use itertools::Itertools;
use log::debug;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Mutex, RwLock};

use k1::compiler::CompileProgramError;
use k1::lex::{self, SpanId, Spans};
use k1::parse;
use k1::parse::{ParsedProgram, Source};
use k1::typer::*;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing::{error, info};

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
enum TokenModifiers {
    Declaration = 0,
    Definition = 1,
    Readonly = 2,
    Static = 3,
    Deprecated = 4,
    Abstract = 5,
    Async = 6,
    Modification = 7,
    Documentation = 8,
    DefaultLibrary = 9,
}

fn span_to_range(source: &Source, spans: &Spans, span_id: SpanId) -> Option<Range> {
    let span = spans.get(span_id);
    let (start_line, end_line) = source.get_lines_for_span(span)?;
    Some(Range {
        start: Position {
            line: start_line.line_index,
            character: span.start - start_line.start_char,
        },
        end: Position { line: end_line.line_index, character: span.end() - end_line.start_char },
    })
}

fn error_to_diagnostic(
    ast: &ParsedProgram,
    message: String,
    level: MessageLevel,
    span_id: SpanId,
) -> Option<(Url, Diagnostic)> {
    let span = ast.spans.get(span_id);
    let source = ast.sources.get(span.file_id);
    let url = source_to_uri(&source.directory, &source.filename);
    let severity = match level {
        MessageLevel::Error => DiagnosticSeverity::ERROR,
        MessageLevel::Warn => DiagnosticSeverity::WARNING,
        MessageLevel::Info => DiagnosticSeverity::INFORMATION,
        MessageLevel::Hint => DiagnosticSeverity::HINT,
    };
    // let mut escaped_message = String::with_capacity(message.len() * 2);
    // escaped_message.push_str("```txt\n");
    // escaped_message.push_str(&message);
    // escaped_message.push_str("\n```");
    match span_to_range(source, &ast.spans, span_id) {
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
                source: Some(ast.name.clone()),
                message,
                related_information: None,
                tags: None,
                data: None,
            };
            Some((url, diagnostic))
        }
    }
}

fn source_to_uri(directory: impl AsRef<Path>, file: impl AsRef<str>) -> Url {
    debug!("source_to_uri on {:?} {:?}", directory.as_ref(), file.as_ref());
    Url::from_directory_path(directory.as_ref()).unwrap().join(file.as_ref()).unwrap()
}

fn uri_to_source<'ast>(ast: &'ast ParsedProgram, url: &Url) -> Option<&'ast Source> {
    let path = url.path();
    debug!("uri_to_source: {}", path);
    let source = ast.sources.iter().find(|s| {
        let source_path = format!("{}/{}", s.1.directory, s.1.filename);
        // info!("    source_path: {}", source_path);
        path == source_path
    });
    source.map(|s| s.1)
}

fn uri_to_edited_source(backend: &Backend, url: &Url) -> Option<(Source, bool)> {
    match backend.edited_sources.lock().unwrap().get(url) {
        None => backend
            .with_ast(|ast| uri_to_source(ast, url).map(|source| (source.clone(), false)))
            .unwrap_or(None),
        Some(ast) => Some((ast.sources.get_main().clone(), true)),
    }
}

enum CompiledProgram {
    Empty,
    Parsed(Box<ParsedProgram>),
    Typed(Box<TypedProgram>),
}

struct Backend {
    client: Client,
    module: Mutex<CompiledProgram>,
    edited_sources: Mutex<HashMap<Url, ParsedProgram>>,
    workspace_uri: RwLock<Option<Url>>,
    compile_iteration: AtomicU32,
}

impl Backend {
    fn new(client: Client) -> Backend {
        Backend {
            client,
            module: Mutex::new(CompiledProgram::Empty),
            edited_sources: Mutex::new(HashMap::new()),
            workspace_uri: RwLock::new(None),
            compile_iteration: AtomicU32::new(0),
        }
    }

    fn with_ast<T>(&self, f: impl Fn(&ParsedProgram) -> T) -> Option<T> {
        let m_lock = self.module.lock().unwrap();
        match &*m_lock {
            CompiledProgram::Empty => None,
            CompiledProgram::Parsed(pm) => Some(f(pm)),
            CompiledProgram::Typed(tm) => Some(f(&tm.ast)),
        }
    }

    fn all_file_urls(&self) -> Vec<Url> {
        self.with_ast(|ast| {
            ast.sources.iter().map(|s| source_to_uri(&s.1.directory, &s.1.filename)).collect()
        })
        .unwrap_or_default()
    }

    fn list_all_errors(&self) -> Vec<(Url, Diagnostic)> {
        let parse_errors: Vec<(Url, Diagnostic)> = self
            .with_ast(|parsed_module| {
                parsed_module
                    .errors
                    .iter()
                    .filter_map(|e| {
                        error_to_diagnostic(
                            parsed_module,
                            format!("Parse Error: {}", e.message()),
                            MessageLevel::Error,
                            e.span(),
                        )
                    })
                    .collect()
            })
            .unwrap_or_default();
        if !parse_errors.is_empty() {
            return parse_errors;
        }

        let mut all_errors = parse_errors;

        if let CompiledProgram::Typed(module) = &*self.module.lock().unwrap() {
            all_errors.extend(module.messages.iter().filter_map(|e| {
                error_to_diagnostic(&module.ast, e.message.clone(), e.level, e.span)
            }));
        };
        all_errors
    }

    fn compile(&self) -> u32 {
        let out_dir: PathBuf = ".k1-out/lsp".into();
        std::fs::create_dir_all(&out_dir).unwrap();
        let iteration_number = self.compile_iteration.load(Ordering::Relaxed);
        info!("compiling version {}", iteration_number);
        let root_uri = self.workspace_uri.read().unwrap();
        let args = k1::compiler::Args {
            no_std: false,
            write_llvm: false,
            optimize: false,
            dump_module: false,
            debug: true,
            profile: false,
            llvm_counts: false,
            target: None,
            command: k1::compiler::Command::Check {
                file: root_uri.as_ref().unwrap().path().into(),
            },
            clang_options: vec![],
        };
        let compile_result = k1::compiler::compile_program(&args, &out_dir);
        let compiled_module = match compile_result {
            Ok(module) => {
                info!("compile {} succeeded", iteration_number);
                CompiledProgram::Typed(Box::new(module))
            }
            Err(CompileProgramError::TyperFailure(module)) => {
                info!("compile {} typing failed", iteration_number);
                CompiledProgram::Typed(module)
            }
            Err(CompileProgramError::ParseFailure(parsed_module)) => {
                info!("compile {}, parse failed", iteration_number);
                CompiledProgram::Parsed(parsed_module)
            }
        };

        let mut module_lock = self.module.lock().unwrap();
        *module_lock = compiled_module;
        let prev_iteration = self.compile_iteration.fetch_add(1, Ordering::Relaxed);
        prev_iteration + 1
    }

    async fn send_diagnostics(&self) {
        let errors = self.list_all_errors();
        let version = self.compile_iteration.load(Ordering::Relaxed);
        let mut errors_by_file = errors.into_iter().into_group_map();
        for e in &errors_by_file {
            info!("Got {} messages for {}", e.1.len(), e.0);
        }

        // Ensure we clear existing diagnostics by always publishing for every file
        let all_files = self.all_file_urls();
        for url in all_files.into_iter() {
            errors_by_file.entry(url).or_insert(vec![]);
        }
        for (file_url, errors) in errors_by_file.into_iter() {
            if !errors.is_empty() {
                info!("Sending {} diagnostics for {file_url} with version {version}", errors.len());
            }
            self.client.publish_diagnostics(file_url, errors, Some(version as i32)).await;
        }
    }

    fn get_typer_errors(&self, file_url: &Url) -> Vec<K1Message> {
        let module_lock = self.module.lock().unwrap();
        let CompiledProgram::Typed(k1) = &*module_lock else {
            return vec![];
        };
        let Some(source) = uri_to_source(&k1.ast, file_url) else {
            info!("Could not get source for {}", file_url.path());
            return vec![];
        };
        let file_id = source.file_id;
        k1.messages
            .iter()
            .filter(|m| {
                let span = k1.ast.spans.get(m.span);
                span.file_id == file_id
            })
            .cloned()
            .collect()
    }

    fn messages_to_diagnostics(&self, messages: &[K1Message]) -> Vec<Diagnostic> {
        self.with_ast(|k1| {
            messages
                .iter()
                .filter_map(|k1_message| {
                    error_to_diagnostic(
                        k1,
                        k1_message.message.clone(),
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

unsafe impl Sync for Backend {}
unsafe impl Send for Backend {}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let mut res = InitializeResult::default();
        res.capabilities.text_document_sync =
            Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
                open_close: Some(false),
                change: Some(TextDocumentSyncKind::FULL),
                will_save: Some(false),
                will_save_wait_until: Some(false),
                save: Some(TextDocumentSyncSaveOptions::SaveOptions({
                    SaveOptions { include_text: None }
                })),
            }));
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
            trigger_characters: Some(vec![".".to_string()]),
            all_commit_characters: Some(vec!["\n".to_string()]),
            work_done_progress_options: WorkDoneProgressOptions::default(),
            completion_item: Some(CompletionOptionsCompletionItem {
                label_details_support: Some(false),
            }),
        });
        res.server_info =
            Some(ServerInfo { name: "k1lsp".to_string(), version: Some("ALPHA".to_string()) });
        info!("Got initialize params: {params:#?}");
        let root_uri = params.root_uri.ok_or(Error::invalid_params("Need root_uri"))?;
        assert!(root_uri.scheme() == "file");
        self.workspace_uri.write().unwrap().replace(root_uri.clone());
        info!("Set root uri: {}", root_uri.path());

        self.compile();
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
        let ast = parse::parse_standalone(file_url.path().to_string(), new_content);
        let new_source = ast.sources.get_main();
        // let (spans, tokens, lex_error) = lex::lex_standalone(&new_source.content);
        let mut parse_diagnostics = vec![];
        for error in &ast.errors {
            if let Some(range) = span_to_range(new_source, &ast.spans, error.span()) {
                let diagnostic = Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some(ast.name.clone()),
                    message: error.message().to_string(),
                    related_information: None,
                    tags: None,
                    data: None,
                };
                parse_diagnostics.push(diagnostic)
            }
        }

        {
            // Scoping hacks for async bullshit
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
        let line = position.position.line;
        let col = position.position.character;
        let module = self.module.lock().unwrap();
        let CompiledProgram::Typed(k1) = &*module else {
            info!("Parsed but not typed (when does this happen?)");
            return Ok(None);
        };
        info!("hover: {}:{}:{}", file_url.path(), line, col);
        let Some(source) = uri_to_source(&k1.ast, &file_url) else {
            info!("Could not get source for {}", file_url.path());
            return Ok(None);
        };

        let expr = k1::lsp_support::get_expr_at_point(k1, source.file_id, line, col);
        let elapsed = start.elapsed();
        info!("hover computed in {:.2?}", elapsed);
        match expr {
            None => Ok(None),
            Some(expr) => Ok(Some(Hover {
                contents: HoverContents::Scalar(MarkedString::String(expr)),
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
        let diagnostics: Vec<(Url, Diagnostic)> = self.list_all_errors();
        Ok(WorkspaceDiagnosticReportResult::Report(WorkspaceDiagnosticReport {
            items: vec![WorkspaceDocumentDiagnosticReport::Full(
                WorkspaceFullDocumentDiagnosticReport {
                    uri: self.workspace_uri.read().unwrap().clone().unwrap(),
                    version: None,
                    full_document_diagnostic_report: FullDocumentDiagnosticReport {
                        result_id: None,
                        items: diagnostics.into_iter().map(|p| p.1).collect(),
                    },
                },
            )],
        }))
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
        self.with_ast(|ast| {
            let mut tokens: Vec<SemanticToken> = vec![];
            let mut prev_line = 1;
            let mut prev_start_col = 0;

            let edited_sources = self.edited_sources.lock().unwrap();
            let ast_for_file: &ParsedProgram = match is_edited {
                false => ast,
                true => edited_sources.get(&file_url).unwrap(),
            };
            for token in &source.tokens {
                // Hack to retrieve the span from the edited ParsedProgram rather than the primary one
                let span = ast_for_file.spans.get(token.span);
                let length = span.len;
                let Some(line) = source.get_line_for_span_start(span) else {
                    continue;
                };
                let line_number = line.line_number();
                let start_col = span.start - line.start_char;
                let delta_line = line_number - prev_line;
                let delta_start =
                    if delta_line == 0 { start_col - prev_start_col } else { start_col };
                let token_type = match token.kind {
                    lex::TokenKind::Numeric => Some(TokenTypes::Number as u32),
                    k1::lex::TokenKind::Ident => Some(TokenTypes::Variable as u32),
                    k1::lex::TokenKind::String { .. }
                    | k1::lex::TokenKind::StringUnterminated { .. }
                    | k1::lex::TokenKind::Char => Some(TokenTypes::String as u32),
                    k1::lex::TokenKind::KeywordFn
                    | k1::lex::TokenKind::KeywordLet
                    | k1::lex::TokenKind::KeywordMut
                    | k1::lex::TokenKind::KeywordAnd
                    | k1::lex::TokenKind::KeywordOr
                    | k1::lex::TokenKind::KeywordIf
                    | k1::lex::TokenKind::KeywordElse
                    | k1::lex::TokenKind::KeywordDefType
                    | k1::lex::TokenKind::KeywordWhile
                    | k1::lex::TokenKind::KeywordLoop
                    | k1::lex::TokenKind::KeywordNamespace
                    | k1::lex::TokenKind::KeywordIntern
                    | k1::lex::TokenKind::KeywordFor
                    | k1::lex::TokenKind::KeywordIn
                    | k1::lex::TokenKind::KeywordAbility
                    | k1::lex::TokenKind::KeywordImpl
                    | k1::lex::TokenKind::KeywordIs
                    | k1::lex::TokenKind::KeywordSwitch
                    | k1::lex::TokenKind::KeywordNot
                    | k1::lex::TokenKind::KeywordBuiltin
                    | k1::lex::TokenKind::KeywordWhere
                    | k1::lex::TokenKind::KeywordContext
                    | k1::lex::TokenKind::KeywordUse
                    | k1::lex::TokenKind::KeywordRequire
                    | k1::lex::TokenKind::KeywordDefer => Some(TokenTypes::Keyword as u32),
                    k1::lex::TokenKind::Slash => Some(TokenTypes::Operator as u32),
                    k1::lex::TokenKind::LineComment => Some(TokenTypes::Comment as u32),
                    k1::lex::TokenKind::OpenParen
                    | k1::lex::TokenKind::CloseParen
                    | k1::lex::TokenKind::OpenBracket
                    | k1::lex::TokenKind::CloseBracket
                    | k1::lex::TokenKind::OpenBrace
                    | k1::lex::TokenKind::CloseBrace
                    | k1::lex::TokenKind::LAngle
                    | k1::lex::TokenKind::LAngleLAngle
                    | k1::lex::TokenKind::RAngle
                    | k1::lex::TokenKind::RAngleRAngle
                    | k1::lex::TokenKind::Colon
                    | k1::lex::TokenKind::ColonEquals
                    | k1::lex::TokenKind::Semicolon
                    | k1::lex::TokenKind::Equals
                    | k1::lex::TokenKind::EqualsEquals
                    | k1::lex::TokenKind::BangEquals
                    | k1::lex::TokenKind::Dot
                    | k1::lex::TokenKind::Comma
                    | k1::lex::TokenKind::Bang
                    | k1::lex::TokenKind::QuestionMark
                    | k1::lex::TokenKind::Pipe
                    | k1::lex::TokenKind::PipePipe
                    | k1::lex::TokenKind::Amp
                    | k1::lex::TokenKind::AmpAmp
                    | k1::lex::TokenKind::Percent
                    | k1::lex::TokenKind::BackSlash
                    | k1::lex::TokenKind::Hash
                    | k1::lex::TokenKind::At
                    | k1::lex::TokenKind::Caret
                    | k1::lex::TokenKind::DoubleQuote
                    | k1::lex::TokenKind::SingleQuote
                    | k1::lex::TokenKind::Plus
                    | k1::lex::TokenKind::Minus
                    | k1::lex::TokenKind::Asterisk
                    | k1::lex::TokenKind::LessEqual
                    | k1::lex::TokenKind::GreaterEqual
                    | k1::lex::TokenKind::LThinArrow
                    | k1::lex::TokenKind::RThinArrow => Some(TokenTypes::Operator as u32),
                    k1::lex::TokenKind::Eof => None,
                };
                if let Some(token_type) = token_type {
                    prev_line = line_number;
                    prev_start_col = start_col;
                    let token = SemanticToken {
                        delta_line,
                        delta_start,
                        length,
                        token_type,
                        token_modifiers_bitset: 0,
                    };
                    // info!("pushing token {:?}", token);
                    tokens.push(token);
                }
            }
            info!(
                "semantic_tokens: iterated {} tokens, returning {}",
                source.tokens.len(),
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

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        info!("handling did_save for document: {}", params.text_document.uri.path());
        {
            let mut es = self.edited_sources.lock().unwrap();
            es.remove(&params.text_document.uri);
        }
        //info!("did_save file {:?}", params.text);
        let start = std::time::Instant::now();
        self.compile();
        let elapsed_ms = start.elapsed().as_millis();
        self.send_diagnostics().await;
        self.client.semantic_tokens_refresh().await.unwrap();
        self.client
            .show_message(
                MessageType::INFO,
                format!("recompiled {} in {}ms", params.text_document.uri.path(), elapsed_ms),
            )
            .await;
    }

    async fn completion(&self, _params: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::List(CompletionList {
            is_incomplete: false,
            items: vec![CompletionItem {
                label: "foo".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some("Example completion item".to_string()),
                label_details: None,
                documentation: None,
                deprecated: None,
                preselect: None,
                sort_text: None,
                filter_text: None,
                insert_text: None,
                insert_text_format: None,
                insert_text_mode: None,
                text_edit: None,
                additional_text_edits: None,
                command: None,
                commit_characters: None,
                data: None,
                tags: None,
            }],
        })))
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let file_appender = tracing_appender::rolling::daily(".", "k1_lsp.log");
    tracing_subscriber::fmt().with_writer(file_appender).init();

    let cwd = std::env::current_dir().unwrap();
    info!("K1 LSP. CWD: {}", cwd.to_string_lossy());

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
