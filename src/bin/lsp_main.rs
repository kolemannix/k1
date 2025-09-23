// Copyright (c) 2025 knix
// All rights reserved.

use itertools::Itertools;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Mutex, RwLock};

use k1::compiler::CompileProgramError;
use k1::lex::SpanId;
use k1::parse::ParsedProgram;
use k1::typer::*;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing::info;

//trait LspError {
//    fn span() -> SpanId;
//    fn message() -> String;
//}
//
//impl LspError for TyperError {}

fn error_to_diagnostic(ast: &ParsedProgram, span_id: SpanId, message: String) -> (Url, Diagnostic) {
    let span = ast.spans.get(span_id);
    let (start_line, end_line) = ast.get_lines_for_span_id(span_id).unwrap();
    let source = ast.sources.get_source(span.file_id);
    let url = source_to_uri(&source.directory, &source.filename);
    let diagnostic = Diagnostic {
        range: Range {
            start: Position {
                line: start_line.line_index,
                character: span.start - start_line.start_char,
            },
            end: Position {
                line: end_line.line_index,
                character: span.end() - end_line.start_char,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: Some("k1".to_string()),
        message,
        related_information: None,
        tags: None,
        data: None,
    };
    info!("{:?}, {:?}", &diagnostic.range, &diagnostic.message);
    (url, diagnostic)
}

fn source_to_uri(directory: impl AsRef<Path>, file: impl AsRef<str>) -> Url {
    Url::from_directory_path(directory.as_ref()).unwrap().join(file.as_ref()).unwrap()
}

enum CompiledProgram {
    Empty,
    Parsed(Box<ParsedProgram>),
    Typed(Box<TypedProgram>),
}

struct Backend {
    client: Client,
    module: Mutex<CompiledProgram>,
    workspace_uri: RwLock<Option<Url>>,
    compile_iteration: AtomicU32,
}

impl Backend {
    fn new(client: Client) -> Backend {
        Backend {
            client,
            module: Mutex::new(CompiledProgram::Empty),
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

    fn all_files(&self) -> Vec<Url> {
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
                    .map(|e| error_to_diagnostic(parsed_module, e.span(), e.to_string()))
                    .collect()
            })
            .unwrap_or_default();
        if !parse_errors.is_empty() {
            return parse_errors;
        }

        let mut all_errors = parse_errors;

        if let CompiledProgram::Typed(module) = &*self.module.lock().unwrap() {
            all_errors.extend(
                module
                    .errors
                    .iter()
                    .map(|e| error_to_diagnostic(&module.ast, e.span, e.message.clone())),
            )
        };
        all_errors
    }

    fn compile(&self) -> u32 {
        let out_dir: PathBuf = ".k1-out/lsp".into();
        info!("compiling version {}", self.compile_iteration.load(Ordering::Relaxed));
        let root_uri = self.workspace_uri.read().unwrap();
        let args = k1::compiler::Args {
            no_std: false,
            write_llvm: false,
            no_llvm_opt: false,
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
            Ok(module) => CompiledProgram::Typed(Box::new(module)),
            Err(CompileProgramError::TyperFailure(module)) => CompiledProgram::Typed(module),
            Err(CompileProgramError::ParseFailure(parsed_module)) => {
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
            info!("Got {} errors for {}", e.1.len(), e.0);
        }

        // Ensure we clear existing diagnostics by always publishing for every file we compiled
        // TODO(lsp): inefficient
        let all_files = self.all_files();
        for url in all_files.into_iter() {
            errors_by_file.entry(url).or_insert(vec![]);
        }
        for (file_url, errors) in errors_by_file.into_iter() {
            info!("Sending {} diagnostics for {file_url} with version {version}", errors.len());
            self.client.publish_diagnostics(file_url, errors, Some(version as i32)).await;
        }
    }
}

unsafe impl Sync for Backend {}
unsafe impl Send for Backend {}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let mut res = InitializeResult::default();
        res.capabilities.hover_provider = Some(HoverProviderCapability::Simple(true));
        res.capabilities.diagnostic_provider =
            Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
                inter_file_dependencies: true,
                identifier: None,
                workspace_diagnostics: true,
                work_done_progress_options: WorkDoneProgressOptions { work_done_progress: None },
            }));
        res.server_info =
            Some(ServerInfo { name: "k1lsp".to_string(), version: Some("ALPHA".to_string()) });
        info!("Got initialize params: {params:#?}");
        let root_uri = params.root_uri.ok_or(Error::invalid_params("Need root_uri"))?;
        assert!(root_uri.scheme() == "file");
        self.workspace_uri.write().unwrap().replace(root_uri.clone());
        info!("Set root uri: {root_uri:?}");

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

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let _ = params;
        log::info!("Got hover");
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("Hello from the LSP!".into())),
            range: None,
        }))
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

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        info!("handling did_save for document: {}", params.text_document.uri.path());
        info!("did_save file {:?}", params.text);
        self.compile();
        self.send_diagnostics().await;
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
