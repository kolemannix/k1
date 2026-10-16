// Copyright (c) 2026 knix
// All rights reserved.

use k1::debug;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::{Arc, Mutex, RwLock};

use k1::compiler::{
    Command, CompileProgramError, CompileRequest, HostProgramMode, LspCompileOptions,
    compile_program,
};
use k1::lex::{self, Span};
use k1::lsp_support::NameKind;
use k1::parse;
use k1::parse::{ParsedProgram, SourceFile};
use k1::typer::trace::LiveProgressSink;
use k1::typer::*;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing::{error, info};

/// Order must match parse::SemanticTokenKind, which indexes this legend
/// directly (`kind as u32`).
const TOKEN_TYPES: [SemanticTokenType; 10] = [
    SemanticTokenType::TYPE,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::ENUM_MEMBER,
    SemanticTokenType::STRING,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::NAMESPACE,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::COMMENT,
];

/// Order must match parse::SemanticToken's bit assignments.
const TOKEN_MODIFIERS: [SemanticTokenModifier; 6] = [
    SemanticTokenModifier::DECLARATION,
    SemanticTokenModifier::DEFINITION,
    SemanticTokenModifier::READONLY,
    SemanticTokenModifier::STATIC,
    SemanticTokenModifier::MODIFICATION,
    // Custom: code that runs at compile time ($, #meta, #static exprs)
    SemanticTokenModifier::new("comptime"),
];

fn span_to_range_in_ast(ast: &ParsedProgram, span: Span) -> Option<Range> {
    let source = ast.sources.source_by_span(span);
    let (start_line, end_line) = source.get_lines_for_span(&ast.mem, span)?;
    Some(Range {
        start: Position {
            line: start_line.line_index,
            character: source.local_from_global_offset(span.global_start) - start_line.start_char,
        },
        end: Position {
            line: end_line.line_index,
            character: source.local_from_global_offset(span.end()) - end_line.start_char,
        },
    })
}

fn span_to_range(k1: &TypedProgram, span: Span) -> Option<Range> {
    span_to_range_in_ast(&k1.ast, span)
}

fn error_to_diagnostic(
    k1: &TypedProgram,
    message: String,
    level: MessageLevel,
    span_id: Span,
) -> Option<(Url, Diagnostic)> {
    if span_id == Span::NONE {
        return None;
    }
    let url = uri_from_span(k1, span_id);
    let severity = match level {
        MessageLevel::Error => DiagnosticSeverity::ERROR,
        MessageLevel::Warn => DiagnosticSeverity::WARNING,
        MessageLevel::Info => DiagnosticSeverity::INFORMATION,
        MessageLevel::Hint => DiagnosticSeverity::HINT,
    };
    let Some(range) = span_to_range(k1, span_id) else {
        error!("Failed span lookup for diagnostic: {}", &message);
        return None;
    };
    Some((url, Diagnostic { range, severity: Some(severity), message, ..Diagnostic::default() }))
}

fn source_to_uri(path: impl AsRef<Path>) -> Url {
    debug!("source_to_uri on {:?}", path.as_ref());
    Url::from_file_path(path.as_ref()).unwrap()
}

fn uri_from_span(k1: &TypedProgram, span: Span) -> Url {
    let source = k1.ast.sources.source_by_span(span);
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

fn get_program_for_url<'k1>(k1: &'k1 mut TypedProgram, url: &Url) -> &'k1 mut TypedProgram {
    let is_build_file =
        Path::new(url.path()).file_name().is_some_and(|name| name == k1::plan::BUILD_FILE_NAME);
    if !is_build_file || !matches!(k1.host_program, HostProgram::Built(_)) {
        return k1;
    }
    let HostProgram::Built(host) = &mut k1.host_program else { unreachable!() };
    host
}

fn get_host_program_inputs_hash(k1: &TypedProgram) -> Option<k1::snap::InputsHash> {
    let HostProgram::Built(host) = &k1.host_program else { return None };
    if host.error_count() == 0 { Some(k1.plan.get_inputs_hash()) } else { None }
}

fn candidate_to_item(candidate: k1::lsp_support::CompletionCandidate) -> CompletionItem {
    let kind = match candidate.kind {
        NameKind::Field => CompletionItemKind::FIELD,
        NameKind::Variant => CompletionItemKind::ENUM_MEMBER,
        NameKind::Method => CompletionItemKind::METHOD,
        NameKind::Variable => CompletionItemKind::VARIABLE,
        NameKind::Function => CompletionItemKind::FUNCTION,
        NameKind::Type => CompletionItemKind::STRUCT,
        NameKind::Namespace => CompletionItemKind::MODULE,
        NameKind::Ability => CompletionItemKind::INTERFACE,
        NameKind::Keyword => CompletionItemKind::KEYWORD,
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

struct OpenSource {
    ast: ParsedProgram,
    dirty: bool,
}

struct Backend {
    client: Client,
    module: k1::server::SharedProgram,
    /// Url of the megarepl http server, reported to the client on `initialized`
    server_url: Option<String>,
    open_sources: Mutex<HashMap<Url, OpenSource>>,
    /// Canonical src_path of the program we compile (a module dir or single
    /// file), derived from the files the client opens and saves
    src_path: RwLock<Option<PathBuf>>,
    published_diagnostic_urls: Mutex<HashSet<Url>>,
    compile_iteration: AtomicU32,
    retarget_generation: AtomicU32,
    retarget_lock: tokio::sync::Mutex<()>,
    completion_generation: AtomicU32,
    completion_compile_lock: tokio::sync::Mutex<()>,
    /// The client accepts `$/progress` (window.workDoneProgress)
    progress_supported: AtomicBool,
}

const PROGRESS_TICK: std::time::Duration = std::time::Duration::from_millis(100);

async fn send_progress_task(
    client: Client,
    token: NumberOrString,
    title: String,
    mut rx: tokio::sync::mpsc::UnboundedReceiver<(String, Option<u32>)>,
) {
    let started = std::time::Instant::now();
    let mut tick = tokio::time::interval(PROGRESS_TICK);
    let mut latest: Option<(String, Option<u32>)> = None;
    let mut begun = false;
    let mut shown_percent: Option<u32> = None;
    loop {
        tokio::select! {
            line = rx.recv() => match line {
                Some(line) => latest = Some(line),
                None => break,
            },
            _ = tick.tick() => {
                let Some((line, percent)) = latest.take() else { continue };
                if let Some(percent) = percent {
                    shown_percent = Some(shown_percent.unwrap_or(0).max(percent));
                }
                let value = if begun {
                    WorkDoneProgress::Report(WorkDoneProgressReport {
                        cancellable: Some(false),
                        message: Some(line),
                        percentage: shown_percent,
                    })
                } else {
                    let created = client
                        .send_request::<request::WorkDoneProgressCreate>(
                            WorkDoneProgressCreateParams { token: token.clone() },
                        )
                        .await;
                    if created.is_err() {
                        return;
                    }
                    begun = true;
                    WorkDoneProgress::Begin(WorkDoneProgressBegin {
                        title: title.clone(),
                        cancellable: Some(false),
                        message: Some(line),
                        percentage: shown_percent,
                    })
                };
                client
                    .send_notification::<notification::Progress>(ProgressParams {
                        token: token.clone(),
                        value: ProgressParamsValue::WorkDone(value),
                    })
                    .await;
            }
        }
    }
    if begun {
        let message = format!("done in {}ms", started.elapsed().as_millis());
        client
            .send_notification::<notification::Progress>(ProgressParams {
                token,
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(WorkDoneProgressEnd {
                    message: Some(message),
                })),
            })
            .await;
    }
}

fn test_failure_diagnostic(
    k1: &TypedProgram,
    result: &k1::typer::StaticTestResult,
) -> Option<(Url, Diagnostic)> {
    let failure = result.failure?;
    let message = format!("test {} failed: {}", k1.ident_str(result.name), k1.ident_str(failure));
    error_to_diagnostic(k1, message, MessageLevel::Error, result.span)
}

fn files_and_errors_map(k1: &TypedProgram) -> (HashMap<Url, Vec<Diagnostic>>, Vec<String>) {
    let mut map: HashMap<Url, Vec<Diagnostic>> = HashMap::new();
    let mut spanless_errors = vec![];
    collect_program_files_and_errors(k1, &mut map, &mut spanless_errors);
    if let HostProgram::Built(host) = &k1.host_program {
        collect_program_files_and_errors(host, &mut map, &mut spanless_errors);
    }
    (map, spanless_errors)
}

fn collect_program_files_and_errors(
    k1: &TypedProgram,
    map: &mut HashMap<Url, Vec<Diagnostic>>,
    spanless_errors: &mut Vec<String>,
) {
    for source in k1.ast.sources.iter() {
        let path = k1.ast.idents.get_string(source.1.file_path);
        map.entry(source_to_uri(path)).or_default();
    }
    for e in k1.ast.errors.iter() {
        let message = format!("Parse Error: {}", e.message());
        if let Some((url, diagnostic)) =
            error_to_diagnostic(k1, message, MessageLevel::Error, e.span())
        {
            map.entry(url).or_default().push(diagnostic);
        }
    }
    for m in k1.messages.borrow().iter() {
        let message = k1.ident_str(m.message).to_string();
        if m.span == Span::NONE {
            if m.level == MessageLevel::Error {
                spanless_errors.push(message);
            }
        } else if let Some((url, diagnostic)) = error_to_diagnostic(k1, message, m.level, m.span) {
            map.entry(url).or_default().push(diagnostic);
        }
    }
    for result in &k1.test_results {
        if let Some((url, diagnostic)) = test_failure_diagnostic(k1, result) {
            map.entry(url).or_default().push(diagnostic);
        }
    }
}

impl Backend {
    fn new(client: Client) -> Backend {
        let module: k1::server::SharedProgram = Arc::new(Mutex::new(None));
        let server_url = k1::server::start(module.clone());
        Backend {
            client,
            module,
            server_url,
            open_sources: Mutex::new(HashMap::new()),
            src_path: RwLock::new(None),
            published_diagnostic_urls: Mutex::new(HashSet::new()),
            compile_iteration: AtomicU32::new(0),
            retarget_generation: AtomicU32::new(0),
            retarget_lock: tokio::sync::Mutex::new(()),
            completion_generation: AtomicU32::new(0),
            completion_compile_lock: tokio::sync::Mutex::new(()),
            progress_supported: AtomicBool::new(false),
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
                let open_sources = self.open_sources.lock().unwrap();
                open_sources
                    .get(file_url)
                    .and_then(|open| content_at_line(&open.ast, open.ast.sources.get_main()))
            };
            let content_and_line = match content_and_line {
                Some(found) => Some(found),
                None => self
                    .with_k1_for_url(file_url, |k1| {
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
        let mut source_overlay = self.construct_source_overlay();
        source_overlay.insert(canonical_path, spliced);
        let Some(root_path) = self.target_file_for_url(file_url, &source_overlay) else {
            return Ok(None);
        };
        let host_inputs_hash = self.with_k1(|k1| get_host_program_inputs_hash(k1)).flatten();
        let lsp_options = LspCompileOptions {
            source_overlay,
            completion: true,
            progress_sink: None,
            host_mode: HostProgramMode::Keep { inputs_hash: host_inputs_hash },
        };
        let my_generation = self.completion_generation.fetch_add(1, Ordering::SeqCst) + 1;
        let _compile_guard = self.completion_compile_lock.lock().await;
        if self.completion_generation.load(Ordering::SeqCst) != my_generation {
            return Err(Error::request_cancelled());
        }
        let program =
            tokio::task::spawn_blocking(move || match compile_check(root_path, lsp_options) {
                Ok(program) => Some(program),
                Err(CompileProgramError::TyperFailure(program)) => Some(*program),
                Err(CompileProgramError::Build(message)) => {
                    error!("{message}");
                    None
                }
            })
            .await
            .map_err(|_| Error::internal_error())?;
        Ok(program)
    }

    fn with_k1<T>(&self, f: impl FnOnce(&mut TypedProgram) -> T) -> Option<T> {
        self.module.lock().unwrap().as_deref_mut().map(f)
    }

    fn with_k1_for_url<T>(&self, url: &Url, f: impl FnOnce(&mut TypedProgram) -> T) -> Option<T> {
        self.with_k1(|k1| f(get_program_for_url(k1, url)))
    }

    fn make_host_program_mode(&self) -> HostProgramMode {
        let inputs_hash = self.with_k1(|k1| get_host_program_inputs_hash(k1)).flatten();
        HostProgramMode::Keep { inputs_hash }
    }

    fn build_all_files_and_errors_map(&self) -> (HashMap<Url, Vec<Diagnostic>>, Vec<String>) {
        self.with_k1(|k1| files_and_errors_map(k1)).unwrap_or_default()
    }

    fn construct_source_overlay(&self) -> fxhash::FxHashMap<String, String> {
        let open_sources = self.open_sources.lock().unwrap();
        let mut overlay = fxhash::FxHashMap::default();
        for (url, open) in open_sources.iter() {
            // keep clean buffers out of the source overlay, because
            // only compiles with an empty overlay are eligible for caching
            if !open.dirty {
                continue;
            }
            let Ok(file_path) = url.to_file_path() else { continue };
            let Ok(path) = k1::kpath::canonicalize_owned(&file_path) else { continue };
            let content = open.ast.sources.get_main().content(&open.ast.mem);
            overlay.insert(path, content.to_string());
        }
        overlay
    }

    fn target_file_for_url(
        &self,
        file_url: &Url,
        overlay: &fxhash::FxHashMap<String, String>,
    ) -> Option<PathBuf> {
        let file_path = k1::kpath::canonicalize_owned(file_url.to_file_path().ok()?).ok()?;
        let in_program = self
            .with_k1_for_url(file_url, |k1| uri_to_source(&k1.ast, file_url).is_some())
            .unwrap_or(false);
        if in_program {
            if let Some(stored) = self.src_path.read().unwrap().clone() {
                return Some(stored);
            }
        }
        Some(k1::compiler::find_check_target_for_file(&file_path, overlay))
    }

    /// Point the program at `file_url`'s module if it isn't already covered,
    /// then compile when the target changed, the file isn't in the program, or
    /// `force_compile` (saves). Publishes diagnostics after compiling.
    async fn ensure_target_and_compile(&self, file_url: &Url, force_compile: bool) {
        let my_generation = self.retarget_generation.fetch_add(1, Ordering::SeqCst) + 1;
        let _guard = self.retarget_lock.lock().await;
        if self.retarget_generation.load(Ordering::SeqCst) != my_generation {
            // A newer open/save superseded this one while we waited
            return;
        }
        let source_overlay = self.construct_source_overlay();
        let Some(target) = self.target_file_for_url(file_url, &source_overlay) else {
            return;
        };
        let changed = {
            let mut src_path = self.src_path.write().unwrap();
            let changed = src_path.as_deref() != Some(target.as_path());
            *src_path = Some(target);
            changed
        };
        let in_program = self
            .with_k1_for_url(file_url, |k1| uri_to_source(&k1.ast, file_url).is_some())
            .unwrap_or(false);
        if !changed && in_program && !force_compile {
            return;
        }
        self.compile(source_overlay).await;
        self.send_diagnostics().await;
    }

    async fn compile(&self, source_overlay: fxhash::FxHashMap<String, String>) {
        let iteration_number = self.compile_iteration.load(Ordering::Relaxed);
        let src_path = self.src_path.read().unwrap().clone();
        let Some(src_path) = src_path else {
            info!("compile {}: no target yet", iteration_number);
            return;
        };
        info!("compiling version {} target {}", iteration_number, src_path.display());
        let compile_start = std::time::Instant::now();
        let progress_sink = if self.progress_supported.load(Ordering::Relaxed) {
            let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
            let title = format!(
                "k1 check {}",
                src_path.file_stem().map(|s| s.to_string_lossy().into_owned()).unwrap_or_default()
            );
            let token = NumberOrString::String(format!("k1-check-{iteration_number}"));
            tokio::spawn(send_progress_task(self.client.clone(), token, title, rx));
            Some(LiveProgressSink::Fn(Arc::new(move |line: &str, percent| {
                let _ = tx.send((line.to_string(), percent));
            })))
        } else {
            None
        };
        let host_program_mode = self.make_host_program_mode();
        let lsp_options = LspCompileOptions {
            source_overlay,
            progress_sink,
            completion: false,
            host_mode: host_program_mode,
        };
        let compile_result =
            match tokio::task::spawn_blocking(move || compile_check(src_path, lsp_options)).await {
                Ok(result) => result,
                Err(join_error) => Err(CompileProgramError::Build(format!(
                    "k1 compiler panicked: {}",
                    panic_message(join_error)
                ))),
            };
        let mut compiled_module = match compile_result {
            Ok(module) => {
                info!(
                    "compile {} succeeded in {}ms",
                    iteration_number,
                    compile_start.elapsed().as_millis()
                );
                Box::new(module)
            }
            Err(CompileProgramError::TyperFailure(module)) => {
                info!(
                    "compile {} typing failed in {}ms",
                    iteration_number,
                    compile_start.elapsed().as_millis()
                );
                module
            }
            Err(CompileProgramError::Build(message)) => {
                self.client.show_message(MessageType::ERROR, message).await;
                return;
            }
        };
        compiled_module.trace.progress_sink = None;

        if let HostProgram::Current = compiled_module.host_program {
            compiled_module.host_program = HostProgram::Absent;
            if let Some(previous) = self.module.lock().unwrap().as_deref_mut()
                && (HostProgramMode::Keep { inputs_hash: get_host_program_inputs_hash(previous) })
                    == host_program_mode
            {
                compiled_module.host_program =
                    std::mem::replace(&mut previous.host_program, HostProgram::Absent);
            }
        }
        if compiled_module.plan.editor.test_on_save && compiled_module.error_count() == 0 {
            let (errors_by_file, spanless_errors) = files_and_errors_map(&compiled_module);
            self.publish_files_and_errors(errors_by_file, spanless_errors).await;
            let tested = tokio::task::spawn_blocking(move || {
                let primary_id = compiled_module.primary_module().id;
                match compiled_module.collect_selected_tests_for_module(primary_id, &[]) {
                    Ok(tests) => compiled_module.run_static_tests(&tests),
                    Err(e) => compiled_module.report(e),
                }
                compiled_module
            })
            .await;
            compiled_module = match tested {
                Ok(tested) => tested,
                Err(join_error) => {
                    let message =
                        format!("k1 static tests panicked: {}", panic_message(join_error));
                    self.client.show_message(MessageType::ERROR, message).await;
                    return;
                }
            };
        }
        *self.module.lock().unwrap() = Some(compiled_module);
        self.compile_iteration.fetch_add(1, Ordering::Relaxed);
    }

    async fn send_diagnostics(&self) {
        let (errors_by_file, spanless_errors) = self.build_all_files_and_errors_map();
        self.publish_files_and_errors(errors_by_file, spanless_errors).await;
    }

    async fn publish_files_and_errors(
        &self,
        errors_by_file: HashMap<Url, Vec<Diagnostic>>,
        spanless_errors: Vec<String>,
    ) {
        let version = self.compile_iteration.load(Ordering::Relaxed);
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
        for message in spanless_errors {
            self.client.show_message(MessageType::ERROR, message).await;
        }
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
        res.capabilities.workspace_symbol_provider = Some(OneOf::Left(true));
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
        let progress_supported =
            params.capabilities.window.as_ref().and_then(|w| w.work_done_progress).unwrap_or(false);
        self.progress_supported.store(progress_supported, Ordering::Relaxed);
        info!("Got initialize params: {params:#?}");
        Ok(res)
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("K1 LSP initialized");

        self.send_diagnostics().await;
        let message = match &self.server_url {
            Some(url) => format!("K1 lsp initialized! megarepl at {url}"),
            None => "K1 lsp initialized!".to_string(),
        };
        self.client.show_message(MessageType::INFO, &message).await;
        self.client.log_message(MessageType::INFO, &message).await;
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
        let (ast, _) = parse::parse_standalone(file_url.path().to_string(), &new_content);
        let mut parse_diagnostics = vec![];
        info!(
            "textDocument/did_change: parsed file {} with {} errors",
            &file_url,
            ast.errors.len()
        );
        for error in &ast.errors {
            if let Some(range) = span_to_range_in_ast(&ast, error.span()) {
                parse_diagnostics.push(Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some(ast.name_str().to_string()),
                    message: error.message().to_string(),
                    ..Diagnostic::default()
                })
            }
        }

        {
            let mut open_sources = self.open_sources.lock().unwrap();
            open_sources.insert(file_url.clone(), OpenSource { ast, dirty: true });
        }
        let version = self.compile_iteration.load(Ordering::Relaxed);

        let mut all_file_diagnostics = self
            .with_k1_for_url(&file_url, |k1| {
                let mut diagnostics = vec![];
                let Some(source) = uri_to_source(&k1.ast, &file_url) else {
                    return diagnostics;
                };
                let file_id = source.file_id;
                for m in k1.messages.borrow().iter() {
                    if m.span.is_none() || k1.ast.sources.file_of(m.span.global_start) != file_id {
                        continue;
                    }
                    let message = k1.ident_str(m.message).to_string();
                    if let Some((_, diagnostic)) = error_to_diagnostic(k1, message, m.level, m.span)
                    {
                        diagnostics.push(diagnostic);
                    }
                }
                for result in &k1.test_results {
                    if k1.ast.sources.file_of(result.span.global_start) == file_id
                        && let Some((_, diagnostic)) = test_failure_diagnostic(k1, result)
                    {
                        diagnostics.push(diagnostic);
                    }
                }
                diagnostics
            })
            .unwrap_or_default();
        all_file_diagnostics.extend(parse_diagnostics);
        self.client.publish_diagnostics(file_url, all_file_diagnostics, Some(version as i32)).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let start = std::time::Instant::now();
        let position = params.text_document_position_params;
        let file_url = position.text_document.uri;
        let line = position.position.line;
        let col = position.position.character;
        info!("hover: {}:{}:{}", file_url.path(), line, col);
        let hover = self
            .with_k1_for_url(&file_url, |k1| {
                let source = uri_to_source(&k1.ast, &file_url)?;
                let file_id = source.file_id;
                let message = match k1::lsp_support::find_entity_at_point(k1, file_id, line, col) {
                    Some(entity) => Some(k1::lsp_support::get_hover_message_for_entity(k1, entity)),
                    None => k1::lsp_support::get_expr_at_point(k1, file_id, line, col),
                }?;
                Some(Hover {
                    contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
                        language: "txt".to_string(),
                        value: message,
                    })),
                    range: None,
                })
            })
            .flatten();
        info!("hover computed in {:.2?}", start.elapsed());
        Ok(hover)
    }

    async fn workspace_diagnostic(
        &self,
        _params: WorkspaceDiagnosticParams,
    ) -> Result<WorkspaceDiagnosticReportResult> {
        info!("Got a workspace/diagnostic request");
        // Spanless errors are surfaced by send_diagnostics as show_message
        let (errors_by_file, _) = self.build_all_files_and_errors_map();

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
        info!("semantic_tokens {}", file_url.path());
        let open_sources = self.open_sources.lock().unwrap();
        let Some(open) = open_sources.get(&file_url) else {
            info!("semantic_tokens: {} is not open", file_url.path());
            return Ok(None);
        };
        let ast_for_file = &open.ast;
        let source = ast_for_file.sources.get_main();
        let file_tokens = source.semantic_tokens.as_slice(&ast_for_file.mem);
        let capacity = file_tokens.len() + source.trivia.len() as usize;
        let mut tokens: Vec<SemanticToken> = Vec::with_capacity(capacity);
        let mut spans_and_kinds = Vec::with_capacity(capacity);
        const COMMENT: u32 = parse::SemanticTokenKind::Comment as u32;
        for semantic_token in file_tokens {
            spans_and_kinds.push((
                semantic_token.span,
                semantic_token.kind as u32,
                semantic_token.mods as u32,
            ))
        }
        for entry in ast_for_file.mem.getn_lt(source.trivia) {
            if let lex::TokenTriviaKind::LineComment = entry.trivia.kind {
                let span = lex::Span {
                    global_start: source.base + entry.trivia.start,
                    len: entry.trivia.len,
                };
                spans_and_kinds.push((span, COMMENT, 0));
            }
        }
        spans_and_kinds.sort_unstable_by_key(|(span, token_type, _)| {
            (span.global_start, *token_type != COMMENT)
        });
        let mut comment_end = 0;
        let mut prev_line = 1;
        let mut prev_start_col = 0;
        for (span, token_type, bitflags) in spans_and_kinds {
            if span.global_start < comment_end {
                continue;
            }
            if token_type == COMMENT {
                comment_end = span.end()
            }
            let length = span.len;
            let Some(line) = source.get_line_for_span_start(&ast_for_file.mem, span) else {
                continue;
            };
            let line_number = line.line_number();
            let start_col = source.local_from_global_offset(span.global_start) - line.start_char;
            let delta_line = line_number - prev_line;
            let delta_start = if delta_line == 0 { start_col - prev_start_col } else { start_col };

            prev_line = line_number;
            prev_start_col = start_col;
            let token = SemanticToken {
                delta_line,
                delta_start,
                length,
                token_type,
                token_modifiers_bitset: bitflags,
            };
            tokens.push(token);
        }
        info!("semantic_tokens: iterated {} tokens, returning {}", capacity, tokens.len());
        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens { result_id: None, data: tokens })))
    }

    async fn diagnostic(
        &self,
        _params: DocumentDiagnosticParams,
    ) -> Result<DocumentDiagnosticReportResult> {
        info!("textDocument/diagnostic: returning unchanged for unsaved single-file diagnostics");
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
        let (ast, _) =
            parse::parse_standalone(file_url.path().to_string(), &params.text_document.text);
        // didOpen content is what the client read from disk, so not dirty
        let open = OpenSource { ast, dirty: false };
        self.open_sources.lock().unwrap().insert(file_url.clone(), open);
        self.ensure_target_and_compile(&file_url, false).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let mut es = self.open_sources.lock().unwrap();
        es.remove(&params.text_document.uri);
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        info!("handling did_save for document: {}", params.text_document.uri.path());
        if let Some(open) = self.open_sources.lock().unwrap().get_mut(&params.text_document.uri) {
            open.dirty = false;
        }
        self.ensure_target_and_compile(&params.text_document.uri, true).await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let start = std::time::Instant::now();
        let position = params.text_document_position;
        let file_url = position.text_document.uri;
        let line = position.position.line;
        let col = position.position.character;
        info!("completion: {}:{}:{}", file_url.path(), line, col);

        let Some(mut program) = self.compile_with_marker(&file_url, line, col).await? else {
            return Ok(None);
        };

        let k1 = get_program_for_url(&mut program, &file_url);
        let site = k1.completion.as_ref().and_then(|cs| cs.site);
        let (candidates, is_incomplete) = match site {
            Some(site) => (k1::lsp_support::collect_completions(k1, site), false),
            None => {
                info!("completion: no site recorded, falling back to enclosing scope");
                // `k1` is the marker compile's program, which supersedes the
                // shared one here
                let fallback =
                    uri_to_source(&k1.ast, &file_url).map(|s| s.file_id).and_then(|file_id| {
                        let scope_id = k1::lsp_support::scope_at_point(k1, file_id, line, col)?;
                        Some(k1::lsp_support::collect_completions(
                            k1,
                            CompletionSite::Scope { scope_id },
                        ))
                    });
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

        let Some(mut root_program) = self.compile_with_marker(&file_url, line, col).await? else {
            return Ok(None);
        };
        let k1 = get_program_for_url(&mut root_program, &file_url);
        let Some(site) = k1.completion.as_ref().and_then(|cs| cs.site) else {
            return Ok(None);
        };
        let Some(info) = k1::lsp_support::signature_help_info(k1, site) else {
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
        info!("goto_definition: {}:{}:{}", uri.path(), position.line + 1, position.character + 1);
        let location = self
            .with_k1_for_url(&uri, |k1| {
                let entity =
                    find_entity_at_point_for_url(k1, &uri, position.line, position.character)?;
                let definition_span = k1::lsp_support::get_entity_definition_span(k1, entity.kind);
                if definition_span == Span::NONE {
                    error!("definition span is nil");
                    return None;
                }
                location_from_resolved_span(k1, definition_span)
            })
            .flatten();
        Ok(location.map(GotoDefinitionResponse::Scalar))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let text_document_position = params.text_document_position;
        let position = text_document_position.position;
        let uri = text_document_position.text_document.uri;
        let line = position.line;
        let char = position.character;
        let include_declaration = params.context.include_declaration;

        self.with_k1_for_url(&uri, |k1| find_references(k1, &uri, line, char, include_declaration))
            .unwrap_or(Ok(None))
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        info!("workspace/symbol: {:?}", params.query);
        Ok(self.with_k1(|k1| {
            let mut infos = Vec::new();
            for symbol in k1::lsp_support::collect_workspace_symbols(k1, &params.query) {
                let Some(location) = location_from_resolved_span(k1, symbol.span) else {
                    continue;
                };
                #[allow(deprecated)]
                infos.push(SymbolInformation {
                    name: symbol.name,
                    kind: name_kind_to_symbol_kind(symbol.kind),
                    tags: None,
                    deprecated: None,
                    location,
                    container_name: Some(symbol.container).filter(|c| !c.is_empty()),
                });
            }
            infos
        }))
    }
}

fn name_kind_to_symbol_kind(kind: NameKind) -> SymbolKind {
    match kind {
        NameKind::Field => SymbolKind::FIELD,
        NameKind::Variant => SymbolKind::ENUM_MEMBER,
        NameKind::Method => SymbolKind::METHOD,
        NameKind::Variable => SymbolKind::VARIABLE,
        NameKind::Function => SymbolKind::FUNCTION,
        NameKind::Type => SymbolKind::STRUCT,
        NameKind::Namespace => SymbolKind::NAMESPACE,
        NameKind::Ability => SymbolKind::INTERFACE,
        NameKind::Keyword => SymbolKind::KEY,
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
    let Some(ls_entity) = find_entity_at_point_for_url(k1, request_uri, line, char) else {
        return Ok(None);
    };

    match ls_entity.kind {
        LsEntityKind::Function { .. }
        | LsEntityKind::Ability(_)
        | LsEntityKind::Variable { .. } => {}
        LsEntityKind::Namespace(_) | LsEntityKind::Type { .. } => return Ok(None),
        // These will be really useful: a variant that is never constructed,
        // a field that is never accessed...
        LsEntityKind::Variant { .. } | LsEntityKind::StructField { .. } => return Ok(None),
    }
    let defn_span = k1::lsp_support::get_entity_definition_span(k1, ls_entity.kind);

    // References are served by scanning ls_entities across all files: every
    // usage the typer sees emits an entity at its span; re-evaluated bodies
    // (generic specializations, macro arguments used twice) repeat spans
    let mut spans: Vec<Span> = Vec::new();
    for entities in k1.ls_entities.borrow().values() {
        for entity in entities {
            let is_defn = match (ls_entity.kind, entity.kind) {
                (
                    LsEntityKind::Function { function_id: target, .. },
                    LsEntityKind::Function { function_id, is_defn },
                ) if k1::lsp_support::get_function_generic_id(k1, function_id)
                    == k1::lsp_support::get_function_generic_id(k1, target) =>
                {
                    is_defn
                }
                (LsEntityKind::Ability(target), LsEntityKind::Ability(ability_id))
                    if k1.abilities.get(ability_id).base_ability_id
                        == k1.abilities.get(target).base_ability_id =>
                {
                    entity.span == defn_span
                }
                (
                    LsEntityKind::Variable { variable_id: target },
                    LsEntityKind::Variable { variable_id },
                ) if variable_id == target => entity.span == defn_span,
                _ => continue,
            };
            if include_declaration || !is_defn {
                spans.push(entity.span);
            }
        }
    }
    spans.sort_by_key(|span| (span.global_start, span.len));
    spans.dedup();
    let locations =
        spans.into_iter().filter_map(|span| location_from_resolved_span(k1, span)).collect();
    Ok(Some(locations))
}

fn location_from_resolved_span(k1: &TypedProgram, span: Span) -> Option<Location> {
    let range = span_to_range(k1, span)?;
    let source = k1.ast.sources.source_by_span(span);
    let uri = source_to_uri(k1.ast.idents.get_string(source.file_path));
    Some(Location { uri, range })
}

fn find_entity_at_point_for_url(
    k1: &TypedProgram,
    uri: &Url,
    line: u32,
    char: u32,
) -> Option<LsEntity> {
    let Some(source) = uri_to_source(&k1.ast, uri) else {
        error!("Could not get source for {}", uri.path());
        return None;
    };
    let entity = k1::lsp_support::find_entity_at_point(k1, source.file_id, line, char);
    if entity.is_none() {
        info!("No entity at point");
    }
    entity
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

fn compile_check(
    file: PathBuf,
    lsp: LspCompileOptions,
) -> std::result::Result<TypedProgram, CompileProgramError> {
    let mut request = CompileRequest::new(file, Command::Check, None)
        .map_err(|e| CompileProgramError::Build(e.to_string()))?;
    request.build.default.debug = true;
    request.build.editor = true;
    request.lsp = lsp;
    compile_program(request)
}

fn panic_message(join_error: tokio::task::JoinError) -> String {
    match join_error.try_into_panic() {
        Ok(payload) => match payload.downcast::<String>() {
            Ok(message) => *message,
            Err(payload) => payload.downcast_ref::<&str>().copied().unwrap_or("").to_string(),
        },
        Err(join_error) => join_error.to_string(),
    }
}
